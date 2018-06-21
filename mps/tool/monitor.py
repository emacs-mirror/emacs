#!/usr/bin/env python
#
# $Id$
# Copyright (c) 2018 Ravenbrook Limited. See end of file for license.
#
# This program reads a telemetry stream from a program using the MPS,
# and constructs a model of the MPS data structures in the progam.


import argparse
from collections import defaultdict, namedtuple
from functools import partial
from struct import Struct
import sys

import matplotlib.pyplot

import mpsevent


# Mapping from event code to a namedtuple for that event.
EVENT_NAMEDTUPLE = {
    code: namedtuple(desc.name, ['header'] + [p.name for p in desc.params])
    for code, desc in mpsevent.EVENT.items()
}

# Mapping from event code to event name.
EVENT_NAME = {code:desc.name for code, desc in mpsevent.EVENT.items()}


def decode_events(read):
    """Decode the events in an I/O stream and generate them as tuples.

    The argument must be a function implementing the io.RawIOBase.read
    specification (that is, it takes a size and returns up to size
    bytes from the I/O stream).

    """
    # Cache frequently-used values in local variables.
    header_desc = mpsevent.HeaderDesc
    header_size = mpsevent.HEADER_SIZE
    event_dict = mpsevent.EVENT
    event_namedtuple = EVENT_NAMEDTUPLE

    # Special handling for Intern.
    Intern_desc = mpsevent.Event.Intern
    Intern_code = Intern_desc.code
    Intern_struct = Struct(Intern_desc.format)
    Intern_size = Intern_struct.size
    Intern_unpack = Intern_struct.unpack
    Intern_namedtuple = event_namedtuple[Intern_code]

    # Build unpacker functions for each type of event.
    header_struct = Struct(mpsevent.HEADER_FORMAT)
    assert header_struct.size == header_size
    header_unpack = header_struct.unpack
    event_unpack = {}
    for code, desc in event_dict.items():
        assert code == desc.code
        s = Struct(desc.format)
        assert code == Intern_code or s.size == desc.maxsize
        event_unpack[code] = s.unpack

    while True:
        header_data = read(header_size)
        if not header_data:
            break
        header = header_desc(*header_unpack(header_data))
        code = header.code
        event_desc = event_dict[code]
        size = header.size - header_size
        if code == Intern_code:
            assert size <= event_desc.maxsize
            event = Intern_namedtuple(header,
                                      *Intern_unpack(read(Intern_size)),
                                      read(size - Intern_size).rstrip(b'\0'))
        else:
            assert size == event_desc.maxsize
            event = event_namedtuple[code](header,
                                           *event_unpack[code](read(size)))
        yield event


class TimeSeries:
    "Series of data points in time order."
    def __init__(self, model, name):
        self.t = []
        self.y = []
        model.add_time_series(self)

    def append(self, t, y):
        "Append data y at time t."
        assert not self.t or t >= self.t[-1]
        self.t.append(t)
        self.y.append(y)


class Accumulator(TimeSeries):
    "Time series that is always non-negative and updates by accumulation."
    def __init__(self, model, name, initial=0):
        super().__init__(model, name)
        self.value = initial

    def add(self, t, delta):
        "Add delta to the accumulator at time t."
        self.value += delta
        self.append(t, self.value)

    def sub(self, t, delta):
        "Subtract delta from the accumulator at time t."
        assert self.value >= delta
        self.value -= delta
        self.append(t, self.value)


class EventHandler:
    """Object that handles a telemetry event by dispatching to the method
    with the same name as the event.

    """
    def ignore(self, event):
        "Handle a telemetry event by doing nothing."

    def handle(self, event):
        "Handle a telemetry event by dispatching."
        getattr(self, EVENT_NAME[event.header.code], self.ignore)(event)


class Pool(EventHandler):
    "Model of an MPS pool."
    def __init__(self, model):
        self.alloc = Accumulator(model, "Bytes allocated from the arena.")

    def ArenaAlloc(self, event):
        self.alloc.add(event.header.clock, event.size)

    def ArenaFree(self, event):
        self.alloc.sub(event.header.clock, event.size)


class Arena(EventHandler):
    "Model of an MPS arena."
    def __init__(self, model):
        self.pool = defaultdict(partial(Pool, model)) # address -> Pool

    def delegate_to_pool(self, event):
        "Handle a telemetry event by delegating to the pool model."
        self.pool[event.pool].handle(event)

    ArenaAlloc = ArenaFree = PoolInit = delegate_to_pool


class Model(EventHandler):
    "Model of an application using the MPS."
    def __init__(self):
        self.intern = {}        # stringId -> string
        self.label = {}         # address -> stringId
        self.arena = defaultdict(partial(Arena, self)) # address -> Arena
        self.time_series = []   # List of TimeSeries

    def add_time_series(self, series):
        "Add a time series to the model."
        self.time_series.append(series)

    def plot(self):
        "Plot all the model's time series."
        plt = matplotlib.pyplot
        figure = plt.figure()
        for series in self.time_series:
            plt.plot(series.t, series.y)
        plt.show()

    def delegate_to_arena(self, event):
        "Handle a telemetry event by delegating to the arena model."
        self.arena[event.arena].handle(event)

    ArenaCreateVM = ArenaCreateCL = ArenaAlloc = ArenaFree = PoolInit = delegate_to_arena

    def Intern(self, event):
        self.intern[event.stringId] = event.string

    def Label(self, event):
        self.label[event.address] = event.stringId


def main():
    parser = argparse.ArgumentParser(description="Memory Pool System Monitor.")
    parser.add_argument('telemetry', metavar='FILENAME', nargs='?',
                        type=argparse.FileType('rb'), default=sys.stdin,
                        help="telemetry output from the MPS instance")
    args = parser.parse_args()

    model = Model()
    for event in decode_events(args.telemetry.read):
        model.handle(event)
    model.plot()


if __name__ == '__main__':
    main()


# C. COPYRIGHT AND LICENCE
#
# Copyright (c) 2018 Ravenbrook Ltd.  All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the
#    distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#
# $Id: //info.ravenbrook.com/project/mps/branch/2018-06-20/monitor/tool/branch#1 $

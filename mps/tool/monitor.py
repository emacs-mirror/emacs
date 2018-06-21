

import argparse
from collections import namedtuple
from struct import Struct
import sys

import mpsevent


# Mapping from event code to a namedtuple for that event.
EVENT_NAMEDTUPLE = {
    code: namedtuple(desc.name, [param.name for param in desc.params])
    for code, desc in mpsevent.EVENT.items()
}


def decode_events(file):
    """Generate the events in file as pairs header, event."""

    # Cache frequently-used values in local variables.
    read = file.read
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
            event = Intern_namedtuple(*Intern_unpack(read(Intern_size)),
                                      (read(size - Intern_size).rstrip(b'\0')))
        else:
            assert size == event_desc.maxsize
            event = event_namedtuple[code](*event_unpack[code](read(size)))
        yield header, event


def main():
    parser = argparse.ArgumentParser(description="Memory Pool System Monitor.")
    parser.add_argument('telemetry', metavar='FILENAME', nargs='?',
                        type=argparse.FileType('rb'), default=sys.stdin,
                        help="telemetry output from the MPS instance")
    args = parser.parse_args()
    for header, event in decode_events(args.telemetry):
        print(header, event)


if __name__ == '__main__':
    main()

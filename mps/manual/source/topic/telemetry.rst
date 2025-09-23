.. highlight:: none

.. _topic-telemetry:

Telemetry
=========

In its :term:`cool` and :term:`hot` :term:`varieties`, the MPS is
capable of outputting a configurable stream of events (the
:term:`telemetry stream`) to assist with debugging and profiling.

The selection of events that appear in the stream is controlled by the
environment variable :envvar:`MPS_TELEMETRY_CONTROL` (by default
none), and the stream is written to the file named by the environment
variable :envvar:`MPS_TELEMETRY_FILENAME` (by default ``mpsio.log``).

The :term:`telemetry system` writes blocks of binary output, and is
fast enough to be left turned on in production code (the :term:`hot`
variety avoids emitting events on the :term:`critical path`), which
can be useful for diagnosing memory management problems in production
environments.

The reporting of garbage collection statistics hasn't always been
suitable for deployment. John McCarthy described the first on-line
demonstration of :term:`Lisp` in an appendix to his paper
":ref:`History of Lisp <MCCARTHY79>`":

    Everything was going well, if slowly, when suddenly the
    Flexowriter began to type (at ten characters per second) ::

        THE GARBAGE COLLECTOR HAS BEEN CALLED.
        SOME INTERESTING STATISTICS ARE AS FOLLOWS:

    and on and on and on. The garbage collector was quite new at the
    time, we were rather proud of it and curious about it, and our
    normal output was on a line printer, so it printed a full page
    every time it was called giving how many words were marked and how
    many were collected and the size of list space, etc. [...]

    Nothing had ever been said about the garbage collector, and I
    could only imagine the reaction of the audience. We were already
    behind time on a tight schedule, it was clear that typing out the
    garbage collector message would take all the remaining time
    allocated to the demonstration, and both the lecturer and the
    audience were incapacitated with laughter. I think some of them
    thought we were victims of a practical joker.


.. index::
   single: telemetry; utilities

.. _topic-telemetry-utilities:

Telemetry utilities
-------------------

There are four programs that help process telemetry streams:

* :ref:`mpseventcnv <telemetry-mpseventcnv>` decodes the
  machine-dependent binary event stream into a portable text format.
  It must be compiled for the same architecture as the MPS-linked
  program whose event stream it decodes.

* :ref:`mpseventtxt <telemetry-mpseventtxt>` takes the output of
  :ref:`mpseventcnv <telemetry-mpseventcnv>` and outputs it in a more
  human-readable form.

* :ref:`mpseventsql <telemetry-mpseventsql>` takes the output of
  :ref:`mpseventcnv <telemetry-mpseventcnv>` and loads it into a
  SQLite database for further analysis.

* :ref:`mpseventpy <telemetry-mpseventpy>` emits Python data
  structures and constants for decoding a telemetry stream.

You must build and install these programs as described in
:ref:`guide-build`. These programs are described in more detail below.


.. index::
   single: telemetry; example
   single: Scheme; telemetry

Example
-------

Here's an example of turning on telemetry in the debugger and then
encountering a corrupted object::

    $ gdb ./scheme
    GNU gdb 6.3.50-20050815 (Apple version gdb-1820) (Sat Jun 16 02:40:11 UTC 2012)
    [...]
    (gdb) set environment MPS_TELEMETRY_CONTROL=all
    (gdb) run
    Starting program: example/scheme/scheme 
    Reading symbols for shared libraries +............................. done
    MPS Toy Scheme Example
    [...]
    7944, 0> (gc)
    [...]
    7968, 1> foo
    Assertion failed: (TYPE(frame) == TYPE_PAIR), function lookup_in_frame, file scheme.c, line 1066.

    Program received signal SIGABRT, Aborted.
    0x00007fff91aeed46 in __kill ()

At this point there's still output in the MPS's internal event
buffers, which needs to be flushed. It would be a good idea to add a
call to :c:func:`mps_telemetry_flush` to the error handler, but for
now we can just call it directly from the debugger::

    (gdb) print mps_telemetry_flush()
    $1 = void

The MPS writes the telemetry to the log in an encoded form for speed.
It can be decoded using the :ref:`mpseventcnv <telemetry-mpseventcnv>`
and :ref:`mpseventtxt <telemetry-mpseventtxt>` programs::

    (gdb) shell mpseventcnv | sort | mpseventtxt > mpsio.txt

The ``sort`` is useful because the events are not necessarily written
to the telemetry file in time order, but each event starts with a
timestamp so sorting makes a time series. The decoded events look like
this, with the timestamp in the first column, the event type in the
second column, and then addresses or other data related to the event
in the remaining columns. The source of the timestamp depends on the
platform; it may be a low-cost high-resolution processor timer, such
as the `Time Stamp Counter
<https://en.wikipedia.org/wiki/Time_Stamp_Counter>`_ on IA-32 and
x86-64, if one is available. All numbers are given in hexadecimal. ::

    000050C3BA05F734 0074 EventInit           major:2 median:3 minor:0 maxCode:143 maxNameLen:19 wordWidth:64 clocksPerSec:00000000000F4240 
    000050C3BA09FC24 0075 EventClockSync      clock:000000000000086C 
    000050C3BA0F22B7 002B VMInit              vm:00007FFEECCCB660 base:0000000103062000 limit:0000000103063000 
    000050C3BA0FA02F 008D GenInit             arena:0000000103062000 gen:00000001030624D8 serial:0 capacity:0000000000000400 mortality:   0.500 
    000050C3BA168B85 0044 PoolInitMFS         pool:0000000103062360 arena:0000000103062000 extendBy:0000000000001000 extendSelf:False unitSize:0000000000000030 
    000050C3BA168C3F 0015 PoolInit            pool:0000000103062360 arena:0000000103062000 poolClass:000000010301CD10 serial:0 
    000050C3BA16BB6F 002B VMInit              vm:00007FFEECCCB520 base:00000001032B3000 limit:000000010369B000 
    000050C3BA1787FC 0005 ArenaCreateVM       arena:0000000103062000 userSize:00000000003E8000 chunkSize:00000000003E8000 grainSize:0000000000001000 arenaClass:0000000103014DA8 serial:0 

You can search through the telemetry for events related to particular
addresses of interest.

In the example, we might look for events related to the address of the
corrupted ``frame`` object::

    (gdb) frame 3
    #3  0x0000000100003f55 in lookup_in_frame (frame=0x1003fa7d0, symbol=0x1003faf20) at scheme.c:1066
    1066            assert(TYPE(frame) == TYPE_PAIR);
    (gdb) print frame
    $2 = (obj_t) 0x1003fa7d0
    (gdb) shell grep -i 1003fa7d0 mpsio.txt || echo not found
    not found

There are no events related to this address, so in particular this
address was never fixed (no ``TraceFix`` event).

.. note::

    You may find it useful to add the command::

        set environment MPS_TELEMETRY_CONTROL=all

    to your ``.gdbinit``.


.. index::
   single: telemetry; event categories
   single: event category

.. _topic-telemetry-categories:

Event categories
----------------

The "bit" column gives the bit number in the :term:`telemetry filter`.
These numbers are liable to change, but the current meanings (zero
being the least significant bit) are:

===  ==========  ========================================================
Bit  Name        Description
===  ==========  ========================================================
0    ``Arena``   Per space or :term:`arena`.
1    ``Pool``    Per :term:`pool`.
2    ``Trace``   Per :term:`trace` or scan.
3    ``Seg``     Per :term:`page` (segment).
4    ``Ref``     Per :term:`reference` or :term:`fix`.
5    ``Object``  Per allocation, :term:`block`, or :term:`object`.
6    ``User``    User-invoked events: see :c:func:`mps_telemetry_intern`.
===  ==========  ========================================================


.. index::
   single: telemetry; environment variables

Environment variables
---------------------

In the ANSI :term:`plinth` (the plinth that comes as default with the
MPS), these two environment variables control the behaviour of the
telemetry feature.

.. envvar:: MPS_TELEMETRY_CONTROL

    The event categories which should be included in the telemetry
    stream.

    If its value can be interpreted as a number, then this number
    represents the set of event categories as a :term:`bitmap`. For
    example, this turns on the ``Pool`` and ``Seg`` event categories::

        MPS_TELEMETRY_CONTROL=6

    Otherwise, the value is split into words at spaces, and any word
    that names an event category turns it on. For example::

        MPS_TELEMETRY_CONTROL="arena pool trace"

    The special event category ``all`` turns on all events.

.. envvar:: MPS_TELEMETRY_FILENAME

    The name of the file to which the telemetry stream should be
    written. Defaults to ``mpsio.log``. For example::

        MPS_TELEMETRY_FILENAME=$(mktemp -t mps)

In addition, the following environment variable controls the behaviour
of the :ref:`mpseventsql <telemetry-mpseventsql>` program.

.. envvar:: MPS_TELEMETRY_DATABASE

    The name of a SQLite database file that will be updated with the
    events from the decoded telemetry stream, if it is not specified
    with the ``-d`` option. If this variable is not assigned,
    ``mpsevent.db`` is used.


.. index::
   single: telemetry; decoding event stream

.. _telemetry-mpseventcnv:

Decoding the telemetry stream
-----------------------------

The MPS writes the telemetry stream in a binary encoded format for
speed. The encoding is specific to the platform the program was
running on, and so the output needs to be decoded before it can be
processed.

The decoding takes place in two stages. First, the program
:program:`mpseventcnv` converts the binary encoded format into a
portable text format suitable for input to one of the second-stage
tools (:ref:`mpseventtxt <telemetry-mpseventtxt>` and
:ref:`mpseventsql <telemetry-mpseventsql>`).

.. program:: mpseventcnv

.. option:: -f <filename>

    The name of the file containing the telemetry stream to decode.
    Defaults to ``mpsio.log``.
    
.. option:: -h

    Help: print a usage message to standard output.

.. note::

    :program:`mpseventcnv` can only read telemetry streams that were
    written by an MPS compiled on the same platform.

Here's some example output. The first column contains the timestamp of
the event, the second column contains the event type, and remaining
columns contain parameters related to the event. ::

    000050C3BA05F734   74 2 3 0 8F 13 40 F4240
    000050C3BA09FC24   75 86C
    000050C3BA0F22B7   2B 7FFEECCCB660 103062000 103063000
    000050C3BA0FA02F   8D 103062000 1030624D8 0 400 0.5
    000050C3BA168B85   44 103062360 103062000 1000 0 30
    000050C3BA168C3F   15 103062360 103062000 10301CD10 0
    000050C3BA16BB6F   2B 7FFEECCCB520 1032B3000 10369B000
    000050C3BA1787FC    5 103062000 3E8000 3E8000 1000 103014DA8 0


.. index::
   single: telemetry; making event stream readable

.. _telemetry-mpseventtxt:

Making the telemetry stream readable
------------------------------------

The output of :ref:`mpseventcnv <telemetry-mpseventcnv>` can be made
more readable by passing it through :program:`mpseventtxt`, which
takes the following options:

.. program:: mpseventtxt

.. option:: -l <filename>

    The name of a file containing telemetry events that have been
    decoded by :ref:`mpseventcnv <telemetry-mpseventcnv>`. Defaults to
    standard input.

.. option:: -h

    Help: print a usage message to standard output.

For example, here's the result of passing the output shown above
through :program:`mpseventtxt`::

    000050C3BA05F734 0074 EventInit           major:2 median:3 minor:0 maxCode:143 maxNameLen:19 wordWidth:64 clocksPerSec:00000000000F4240 
    000050C3BA09FC24 0075 EventClockSync      clock:000000000000086C 
    000050C3BA0F22B7 002B VMInit              vm:00007FFEECCCB660 base:0000000103062000 limit:0000000103063000 
    000050C3BA0FA02F 008D GenInit             arena:0000000103062000 gen:00000001030624D8 serial:0 capacity:0000000000000400 mortality:   0.500 
    000050C3BA168B85 0044 PoolInitMFS         pool:0000000103062360 arena:0000000103062000 extendBy:0000000000001000 extendSelf:False unitSize:0000000000000030 
    000050C3BA168C3F 0015 PoolInit            pool:0000000103062360 arena:0000000103062000 poolClass:000000010301CD10 serial:0 
    000050C3BA16BB6F 002B VMInit              vm:00007FFEECCCB520 base:00000001032B3000 limit:000000010369B000 
    000050C3BA1787FC 0005 ArenaCreateVM       arena:0000000103062000 userSize:00000000003E8000 chunkSize:00000000003E8000 grainSize:0000000000001000 arenaClass:0000000103014DA8 serial:0 


.. index::
   single: telemetry; loading into SQLite

.. _telemetry-mpseventsql:

Loading the telemetry stream into SQLite
----------------------------------------

The decoded telemetry stream (as output by :ref:`mpseventcnv
<telemetry-mpseventcnv>`) can be loaded into a SQLite database for
further analysis by running :program:`mpseventsql`.

:program:`mpseventsql` takes the following options:

.. program:: mpseventsql

.. option:: -i <filename>

    The name of a file containing a decoded telemetry stream. Defaults
    to standard input.

.. option:: -o <filename>

    The name of a SQLite database file that will be updated with the
    events from the decoded telemetry stream specified by the ``-l``
    option. The database will be created if it does not exist. If not
    specified, the file named by the environment variable
    :envvar:`MPS_TELEMETRY_DATABASE` is used; if this variable is not
    assigned, ``mpsevent.db`` is used.

    Updating a database with events from a file is idempotent unless
    the ``-f`` option is specified.

.. option:: -d

    Delete the database before importing.

.. option:: -f

    Forces the database to be updated with events from the decoded
    telemetry stream specified by the ``-i`` option, even if those
    events have previously been added.

.. option:: -v

    Increase the verbosity. With one or more ``-v`` options,
    :program:`mpseventsql` prints informative messages to standard
    error. Verbosity levels up to 3 (``-vvv``) produce successively
    more detailed information.

    This option implies ``-p``.

.. option:: -p

    Show progress by printing a dot to standard output for every
    100,000 events processed.

.. option:: -t

    Run internal tests.

.. option:: -r

    Rebuild the tables ``event_kind``, ``event_type``, and
    ``event_param``. (This is necessary if you changed the event
    descriptions in ``eventdef.h``.)


.. index::
   single: telemetry; decoding in Python

.. _telemetry-mpseventpy:

Decoding the telemetry stream in Python
---------------------------------------

.. program:: mpseventpy

:program:`mpseventpy` takes no options, and emits Python code
containing constants and data structures for decoding a telemetry
stream generated by an application on the same platform and using the
same version of the MPS.

To decode an event from a telemetry stream, start by reading and
decoding the header.

.. py:data:: HEADER_SIZE

    Number of bytes in an event header. The event header consists of
    data that is common to all events, and precedes the event-specific
    data.

.. py:data:: HEADER_FORMAT

    Format string to pass to |unpack|_ to decode an event header.

    .. |unpack| replace:: :py:func:`struct.unpack`
    .. _unpack: https://docs.python.org/3/library/struct.html#struct.unpack

.. py:class:: HeaderDesc

    Named tuple describing an event header. It has the following
    attributes:

    :py:attr:`code` is the code (an integer) for the event type.

    :py:attr:`size` is the size of the remainder of event (in bytes).

    :py:attr:`clock` is when the event occurred (in arbitrary time units).

Using these data structures, you might read an event from a file
:py:obj:`f` like this::

    header_data = f.read(HEADER_SIZE)
    if not header_data:
        # No more telemetry.
    header = HeaderDesc(*struct.unpack(HEADER_FORMAT, header_data))
    event_data = f.read(header.size)
    if not event_data:
        # Telemetry was truncated.

To decode the individual events, you'll need the following data structures:

.. py:data:: EVENT

    Mapping from event code to :py:class:`EventDesc`.

.. py:class:: EventDesc

    Named tuple describing an event type. It has the following attributes:

    :py:attr:`name` is the name of the event type.

    :py:attr:`code` is the code (an integer) for the event type.

    :py:attr:`used` is :py:obj:`True` if the event is used by the MPS,
    :py:obj:`False` if it is obsolete.

    :py:attr:`kind` is the event category (see
    :py::ref:`topic-telemetry-categories`), an instance of the
    :py:class:`KindDesc` class.

    :py:attr:`params` is a list of parameters of the event, each being
    an instance of the :py:class:`EventParam` class.

    :py:attr:`maxsize` is the maximum size of events of this type (in
    bytes).

    :py:attr:`format` is a format string to pass to |unpack|_ to
    decode an event of this type.

.. py:class:: EventParam

    Named tuple describing a parameter to an event type. It has
    the following attributes:

    :py:attr:`sort` is a letter indicating the type of the parameter:
    ``P`` for a pointer to an internal MPS data structures, ``A`` for
    an address in the client program, ``W`` for a word, ``U`` for an
    unsigned integer, ``B`` for a Boolean, ``D`` for a
    double-precision floating-point number, and ``S`` for a string.

    :py:attr:`name` is the name of the parameter.

    :py:attr:`doc` is brief documentation for the parameter.

Using these data structures, you might decode an event like this::

    event_desc = EVENT[header.code]
    event_namedtuple = namedtuple(event_desc.name, [p.name for p in event_desc.params])
    event = event_namedtuple(*struct.unpack(event_desc.format, event_data))

(In practice you'd want to cache the named tuple and reuse it for
future events belonging to the same event type.)


.. index::
   single: telemetry; events

Telemetry events
----------------

The set of telemetry events is not documented, and varies from version
to version as we discover new requirements. You can see the current
set of events by looking in the header ``eventdef.h``.

If you have developed a tool that uses MPS telemetry, and would like
to depend on particular telemetry events, :ref:`contact us <contact>`.


.. index::
   single: telemetry; interface

Telemetry interface
-------------------

.. c:function:: void mps_telemetry_flush(void)

    Flush the internal event buffers into the :term:`telemetry stream`.

    This function also calls :c:func:`mps_io_flush` on the event
    stream itself. This ensures that even the latest events are now
    properly recorded, should the :term:`client program` terminate
    (uncontrollably as a result of a bug, for example) or some
    interactive tool require access to the telemetry stream.

    .. note::

        Unless all :term:`arenas` are properly destroyed (by calling
        :c:func:`mps_arena_destroy`), there are likely to be unflushed
        telemetry events when the program finishes. So in the case of
        abnormal program termination such as a fatal exception, you
        may want to call :c:func:`mps_telemetry_flush` explicitly.


.. c:function:: mps_word_t mps_telemetry_get(void)

    Return the :term:`telemetry filter`.


.. c:function:: void mps_telemetry_set(mps_word_t set_mask)

    Set bits in the :term:`telemetry filter`.

    ``set_mask`` is a :term:`bitmask` indicating the bits in the
    telemetry filter that should be set.


.. c:function:: void mps_telemetry_reset(mps_word_t reset_mask)

    Reset bits in the :term:`telemetry filter`.

    ``reset_mask`` is a :term:`bitmask` indicating the bits in the
    telemetry filter that should be reset.


.. index::
   pair: telemetry; labels

Telemetry labels
----------------

Telemetry labels allow the :term:`client program` to associate strings
with addresses in the telemetry stream. The string must first be
*interned* by calling :c:func:`mps_telemetry_intern`, returning a
label, and then the address can be associated with the label by
calling :c:func:`mps_telemetry_label`.

Typical uses of telemetry labels include:

* labelling pools with a human-meaningful name;

* labelling allocated objects with their type, class, or other description.

It is necessary to enable ``User`` events in the :term:`telemetry
filter` in order for telemetry labels to work. For example::

    mps_label_t label;
    mps_telemetry_set(1 << 6);
    label = mps_telemetry_intern("symbol pool");
    mps_telemetry_label(symbol_pool, label);

Labels are represented by the type :c:type:`mps_label_t`. These are
unsigned integers. After processing by :ref:`mpseventsql
<telemetry-mpseventsql>`, the association of addresses with labels
appears in the ``EVENT_Label`` table, and the association of labels
with strings appears in the ``EVENT_Intern`` table. These can then be
used in queries, for example:

.. code-block:: sql

    /* Pool name and creation time */
    SELECT I.string, P.time
    FROM EVENT_PoolInit AS P,
         EVENT_Label AS L,
         EVENT_Intern AS I
    WHERE I.stringId = L.stringId AND L.address = P.pool;


.. c:function:: mps_label_t mps_telemetry_intern(const char *label)

    Registers a string with the MPS, and receives a :term:`telemetry
    label`, suitable for passing to :c:func:`mps_telemetry_label`.

    ``label`` is a NUL-terminated string. Its length should not exceed
    256 characters, including the terminating NUL.

    Returns a telemetry label: a unique identifier that may be used to
    represent the string in future.

    The intention of this function is to provide an identifier that
    can be used to concisely represent a string for the purposes of
    :c:func:`mps_telemetry_label`. 

    .. note::

        If the ``User`` event category is not turned on in the
        :term:`telemetry filter` (via :c:func:`mps_telemetry_set` or
        :envvar:`MPS_TELEMETRY_CONTROL`) then the string is not sent
        to the :term:`telemetry stream`. A label is still returned in
        this case, but it is useless.


.. c:function:: void mps_telemetry_label(mps_addr_t addr, mps_label_t label)

    Associate a telemetry label returned from
    :c:func:`mps_telemetry_intern` with an address.

    ``addr`` is an address.

    ``label`` is a :term:`telemetry label` returned from
    :c:func:`mps_telemetry_intern`.

    The label will be associated with the address when it appears in
    the :term:`telemetry stream`.

    .. note::

        If the ``User`` event category is not turned on in the
        :term:`telemetry filter` (via :c:func:`mps_telemetry_set` or
        :envvar:`MPS_TELEMETRY_CONTROL`) then calling this function
        has no effect.


.. index::
   pair: telemetry; customizing

Customizing the telemetry system
--------------------------------

If you need the telemetry system to support features not described
here (for example, you need to transmit telemetry data over a network
rather than writing it to a file on the local filesystem) then you may
be able to do so by providing your own implementation of the
:ref:`topic-plinth-io`.

When it first needs to output the :term:`telemetry stream`, the MPS
calls the plinth function :c:func:`mps_io_create` to create an I/O
stream. It then calls :c:func:`mps_io_write` to write binary data to
the stream and :c:func:`mps_io_flush` to flush the stream in response
to :c:func:`mps_telemetry_flush`. By providing your own
implementations of these functions, you can direct the telemetry
stream wherever you like.

See :ref:`topic-plinth` for details.

.. _topic-telemetry:

Telemetry
=========

Typical uses of telemetry labels include:

- Label pools with a human-meaningful name;

- Label allocated objects with their type or class.


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
    example, this turns on event categories numbered 0 to 15::

        MPS_TELEMETRY_CONTROL=65535

    Otherwise, the value is split into words at spaces, and any word
    that names an event category turns it on. For example::

        MPS_TELEMETRY_CONTROL="Arena Pool Trace"

    .. note::

        The names of the event categories are case sensitive.

.. envvar:: MPS_TELEMETRY_FILENAME

    The name of the file to which the telemetry stream should be
    written. Defaults to ``mpsio.log``. For example::

        MPS_TELEMETRY_FILENAME=$(mktemp -t mps)


.. _telemetry-eventcnv:

Decoding the telemetry stream
-----------------------------

The MPS writes the telemetry stream in an encoded form for speed. It
can be decoded using the ``eventcnv`` program, which prints (to
standard output) a representation of each event in the stream.

``eventcnv`` takes the following arguments:

.. program:: eventcnv

.. option:: -f <filename>

    The name of the file containing the telemetry stream to decode.
    Defaults to ``mpsio.log``.

.. option:: -S

    Format output human-readably. This is the default output style.
    For example::

        000007DC7DC1655516E TraceFix 7FFF583001D0 7FFF583000D8 107AFAB20 1

.. option:: -SL

    Format output as S-expressions for consumption by :term:`Lisp`.
    For example::

        (000007DC7DC1655516E TraceFix 7FFF583001D0 7FFF583000D8 107AFAB20 1)

.. option:: -SC

    Format output as CSV (comma-separated values). For example::

        000007DC7DC1655516E, 38, 140734672929232, 140734672928984, 4423920416, 1
    
.. option:: -h

    Help: print a usage message to standard output.

.. note::

    ``eventcnv`` can only read telemetry streams that were written by
    an MPS compiled on the same platform.

    The events are printed in the order that they were written by the
    MPS, which is not the same as the order that they
    occurred. However, each event is prefixed by a timestamp, so that
    a time series of events can be obtained by sorting the output:
    ``eventcnv | sort``.



Interface
---------

.. c:function:: mps_word_t mps_telemetry_control(mps_word_t reset_mask, mps_word_t flip_mask)

    Update and return the :term:`telemetry filter`.

    ``reset_mask`` is a :term:`bitmask` indicating the bits in the
    telemetry filter that should be reset.

    ``flip_mask`` is a bitmask indicating the bits in the telemetry
    filter whose value should be flipped after the resetting.

    Returns the previous value of the telemetry filter, prior to the
    reset and the flip.

    The parameters ``reset_mask`` and ``flip_mask`` allow the
    specification of any binary operation on the filter control. For
    typical operations, the parameters should be set as follows:

    ============  ==============  =============
    Operation     ``reset_mask``  ``flip_mask``
    ============  ==============  =============
    ``set(M)``    ``M``           ``M``        
    ------------  --------------  -------------
    ``reset(M)``  ``M``           ``0``        
    ------------  --------------  -------------
    ``flip(M)``   ``0``           ``M``        
    ------------  --------------  -------------
    ``read()``    ``0``           ``0``        
    ============  ==============  =============


.. c:function:: void mps_telemetry_flush(void)

    Flush the internal event buffers into the :term:`telemetry stream`.

    This function also calls :c:func:`mps_io_flush` on the event
    stream itself. This ensures that even the latest events are now
    properly recorded, should the :term:`client program` terminate
    (uncontrollably as a result of a bug, for example) or some
    interactive tool require access to the telemetry stream. You could
    even try calling this from a debugger after a problem.


.. c:function:: mps_word_t mps_telemetry_intern(char *label)

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

        "User" events must be turned on in the :term:`telemetry
        filter` (via :c:func:`mps_telemetry_control`) before this
        function is invoked.


.. c:function:: void mps_telemetry_label(mps_addr_t addr, mps_word_t label)

    Associate a telemetry label returned from
    :c:func:`mps_telemetry_intern` with an address.

    ``addr`` is an address.

    ``label`` is a telemetry label returned from
    :c:func:`mps_telemetry_intern`.

    The label will be associated with the address when it appears in
    the :term:`telemetry stream`.

    .. note::

       "User" events must be selected in the :term:`telemetry filter`
       via :c:func:`mps_telemetry_control`.

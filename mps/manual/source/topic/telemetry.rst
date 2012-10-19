.. _topic-telemetry:

Telemetry
=========

Typical uses of telemetry labels include:

- Label pools with a human-meaningful name;

- Label allocated objects with their type or class.



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

    The significance of the bits is liable to change, but the current
    meanings (zero being the least significant bit) are:

    0. per space or :term:`arena`;

    1. per :term:`pool`;

    2. per :term:`trace` or scan;

    3. per :term:`page` (segment);

    4. per :term:`reference` or :term:`fix`;

    5. per allocation, :term:`block`, or :term:`object`;

    6. "user" events: see :c:func:`mps_telemetry_intern`.


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

    ``label`` is a NUL-terminated string way. Its length should not
    exceed 256 characters, including the terminating NUL.

    Returns a telemetry label: a unique identifier that may be used to
    represent the string in future.

    The intention of this function is to provide an identifier that
    can be used to concisely represent a string for the purposes of
    :c:func:`mps_telemetry_label`. 

    .. note::

        The appropriate setting must be turned on in the
        :term:`telemetry filter` (via :c:func:`mps_telemetry_control`)
        before this function is invoked; the associated event is of
        the "user" kind.


.. c:function:: void mps_telemetry_label(mps_addr_t addr, mps_word_t label)

    Associate a telemetry label returned from
    :c:func:`mps_telemetry_intern` with an address.

    ``addr`` is an address.

    ``label`` is a telemetry label returned from
    :c:func:`mps_telemetry_intern`.

    The label will be associated with the address when it appears in
    the :term:`telemetry stream`.

    .. note::

       The "user" kind must be set in the :term:`telemetry filter`
       via :c:func:`mps_telemetry_control`.



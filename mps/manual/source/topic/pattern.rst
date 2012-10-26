.. Sources:

     `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/alloc-pattern-ramp/>`_

.. _topic-pattern:

Allocation patterns
===================

An :dfn:`allocation pattern` is a hint to the MPS to expect a
particular pattern of allocation on an :term:`allocation point`. The
MPS may use this hint to schedule its decisions as to when and what to
collect.


.. c:type:: mps_alloc_pattern_t

    The type of :term:`allocation patterns <allocation pattern>`.

    An allocation pattern is a hint to the MPS to expect a particular
    pattern of allocation on an :term:`allocation point`. The MPS may
    use this hint to schedule its decisions as to when and what to
    collect.

    There are two allocation patterns,
    :c:func:`mps_alloc_pattern_ramp` and
    :c:func:`mps_alloc_pattern_ramp_collect_all`.


.. c:function:: mps_res_t mps_ap_alloc_pattern_begin(mps_ap_t ap, mps_alloc_pattern_t alloc_pattern)

    Start a period of allocation that behaves according to an
    :term:`allocation pattern`. The period persists until a
    corresponding call to :c:func:`mps_ap_alloc_pattern_end`.

    ``ap`` is the :term:`allocation point` in which the patterned
    allocation will occur.

    ``alloc_pattern`` is the allocation pattern.

    Returns :c:macro:`MPS_RES_OK` if the allocation pattern is
    supported by this allocation point. At present this is always the
    case, but in future this function may return another :term:`result
    code` if the allocation pattern is not supported by the allocation
    point.

    If :c:func:`mps_ap_alloc_pattern_begin` is used multiple times on
    the same allocation point without intervening calls to
    :c:func:`mps_ap_alloc_pattern_end`, the calls match in a
    stack-like way, outermost and innermost: that is, allocation
    patterns may nest, but not otherwise overlap.

    Some allocation patterns may additionally support overlap: if so,
    the documentation for the individual pattern types will specify
    this.


.. c:function:: mps_res_t mps_ap_alloc_pattern_end(mps_ap_t ap, mps_alloc_pattern_t alloc_pattern)

    End a period of allocation on an :term:`allocation point` that
    behaves according to an :term:`allocation pattern`.

    ``ap`` is the allocation point in which the patterned allocation
    occurred.

    ``alloc_pattern`` is the allocation pattern.

    Returns :c:macro:`MPS_RES_OK` if the period of allocation was
    successfully ended, or :c:macro:`MPS_RES_FAIL` if there was no
    corresponding call to :c:func:`mps_ap_alloc_pattern_begin`.


.. c:function:: mps_res_t mps_ap_alloc_pattern_reset(mps_ap_t ap)

    End all :term:`patterned allocation <allocation pattern>` on an
    :term:`allocation point`.

    ``ap`` is the allocation point on which to end all patterned
    allocation.

    Returns :c:macro:`MPS_RES_OK`. It may fail in future if certain
    allocation patterns cannot be ended for that allocation point at
    that point in time.

    This function may be used to recover from error conditions.


.. _topic-pattern-ramp:

Ramp allocation
---------------

:dfn:`Ramp allocation` a pattern of allocation whereby the
:term:`client program` builds up an increasingly large data structure,
the live size of which increases until a particular time, at which
time most of the data structure is discarded, resulting in sharp
cutoff and decline in the live size.

This pattern is useful if you are building a structure that involves
temporarily allocating much more memory than will fit into your
:term:`nursery generation`. The ramp allocation pattern causes the
collection of the nursery generation to be deferred until the ramp
allocation is over.

.. note::

    Ramp allocation is only supported by :ref:`pool-amc`.


.. c:function:: mps_alloc_pattern_t mps_alloc_pattern_ramp(void)

    Return an :term:`allocation pattern` indicating that allocation
    will follow a :term:`ramp pattern`.

    This indicates to the MPS that most of the blocks allocated after
    the call to :c:func:`mps_ap_alloc_pattern_begin` are likely to be
    :term:`dead` by the time of the corresponding call to
    :c:func:`mps_ap_alloc_pattern_end`.


.. c:function:: mps_alloc_pattern_t mps_alloc_pattern_ramp_collect_all(void)

    Return an :term:`allocation pattern` indicating that allocation
    will follow a :term:`ramp pattern`, and that the next
    :term:`garbage collection` following the ramp should be a full
    collection.

    This indicates to the MPS that most of the blocks allocated after
    the call to :c:func:`mps_ap_alloc_pattern_begin` are likely to be
    :term:`dead` by the time of the corresponding call to
    :c:func:`mps_ap_alloc_pattern_end`.

    This allocation pattern may nest with, but should not otherwise
    overlap with, allocation patterns of type
    :c:func:`mps_alloc_pattern_ramp`. In this case, the MPS may defer
    the full collection until after all ramp allocation patterns have
    ended.

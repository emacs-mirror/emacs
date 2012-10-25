.. Sources:

     `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/alloc-pattern-ramp/>`_

.. _topic-pattern:

Allocation patterns
===================

::

    mps_ap_alloc_pattern_begin(ap, mps_alloc_pattern_ramp_collect_all());
    do_lots_of_work();
    mps_ap_alloc_pattern_end(ap, mps_alloc_pattern_ramp_collect_all());
    wait_for_collection_statistics_while_doing_other_allocation();

::

    res = mps_ap_alloc_pattern_begin(ap, mps_alloc_pattern_ramp());
    assert(res == mps_res_ok);

    do_some_work(); /* Leaves stuff lying around */

    res = mps_ap_alloc_pattern_begin(ap, mps_alloc_pattern_ramp());
    assert(res == mps_res_ok);

    res = do_some_more_work(); /* Tidies up after itself */
    if (res != mps_res_ok) {
      res = mps_ap_alloc_pattern_reset(ap);
      assert(res == mps_res_ok);
      return;
    }

    res = mps_ap_alloc_pattern_end(ap, mps_alloc_pattern_ramp());
    assert(res == mps_res_ok);

    tidy_up_first_work();

    res = mps_ap_alloc_pattern_end(ap, mps_alloc_pattern_ramp());
    assert(res == mps_res_ok);


Interface
---------

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



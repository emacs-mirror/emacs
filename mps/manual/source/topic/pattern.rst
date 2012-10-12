.. _topic-pattern:

===================
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

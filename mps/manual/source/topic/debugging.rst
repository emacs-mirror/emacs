.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/design/object-debug/>`_

.. index::
   single: debugging; pool
   single: pool; debugging

.. _topic-debugging:

Debugging pools
===============

Several :term:`pool classes` have debugging counterparts:

=================  ==============================
Pool class         Debugging counterpart
=================  ==============================
:ref:`pool-ams`    :c:func:`mps_class_ams_debug`
:ref:`pool-mv`     :c:func:`mps_class_mv_debug`
:ref:`pool-mvff`   :c:func:`mps_class_mvff_debug`
=================  ==============================

These debugging pool classes provide two features that are useful for
debugging:

* .. index::
     single: debugging; fencepost
     single: fencepost

  :dfn:`fenceposts` are patterns of data that are written before and
  after each allocated block. In :term:`manually managed <manual
  memory management>` pools, fenceposts are checked when the block is
  deallocated, to see that they are unchanged. This helps detect
  underwriting and :term:`overwriting errors`. Fenceposts for all
  objects in a pool are checked when the pool is destroyed, and can be
  checked at any time by calling :c:func:`mps_pool_check_fenceposts`.

* .. index::
     single: debugging; free space splatting
     single: free space splatting

  :dfn:`free space splatting` overwrites recycled space with a pattern
  of data. If the pattern is designed so that it does not resemble a
  live object (and if code checks the consistency of its data
  structues), then this helps to detect :term:`dangling pointer`
  dereferences. The pattern is checked just before allocation, and
  when a block of memory is released from the pool to the arena, to
  see that it is unchanged. All free space in a pool can be checked
  for the pattern at any time by calling
  :c:func:`mps_pool_check_free_space`.

The :term:`client program` specifies templates for both of these
features via the :c:type:`mps_pool_debug_option_s` structure. This
allows it to specify patterns:

* that mimic illegal data values;

* that cause bus errors if wrongly interpreted as pointers;

* that cause assertions to fire if wrongly interpreted as data values;

* that contain an instruction sequence that wold cause the program to
  signal an error or stop if wrongly interpreted as executable code.

For example::

    mps_pool_debug_option_s debug_options = {
       (const void *)"fencepost", 9,
       (const void *)"free", 4,
    };
    mps_pool_t pool;
    mps_res_t res;
    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_POOL_DEBUG_OPTIONS, &debug_options);
        MPS_ARGS_ADD(args, MPS_KEY_FORMAT, &fmt);
        res = mps_pool_create_k(&pool, arena, mps_class_ams_debug(), args);
    } MPS_ARGS_END(args);
    if (res != MPS_RES_OK) error("can't create debug pool");


.. c:type:: mps_pool_debug_option_s

    The type of the structure passed as the
    :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS` keyword argument to
    :c:func:`mps_pool_create_k` when creating a debugging :term:`pool
    class`. ::

        typedef struct mps_pool_debug_option_s {
            const void *fence_template;
            size_t fence_size;
            const void *free_template;
            size_t free_size;
        } mps_pool_debug_option_s;

    ``fence_template`` points to a template for :term:`fenceposts`.

    ``fence_size`` is the :term:`size` of ``fence_template`` in
    :term:`bytes (1)`, or zero if the debugging pool should not use
    fenceposts.

    ``free_template`` points to a template for splatting free space.

    ``free_size`` is the :term:`size` of ``free_template`` in bytes, or
    zero if the debugging pool should not splat free space.

    The debugging pool will copy the ``fence_size`` bytes pointed to by
    ``fence_template`` in a repeating pattern onto each fencepost during
    allocation, and it will copy the bytes pointed to by
    ``free_template`` in a repeating pattern over free space after the
    space is reclaimed.

    The MPS may not always use the whole of a template: it may use
    pieces smaller than the given size, for example to pad out part of
    a block that was left unused because of alignment requirements.


.. c:function:: void mps_pool_check_fenceposts(mps_pool_t pool)

    Check all the :term:`fenceposts` in a :term:`pool`.

    ``pool`` is the pool whose fenceposts are to be checked.

    If a corrupted fencepost is found, the MPS will :term:`assert
    <assertion>`. It is only useful to call this on a :term:`debugging
    pool` that has fenceposts turned on. It does nothing on
    non-debugging pools.


.. c:function:: void mps_pool_check_free_space(mps_pool_t pool)

    Check all the free space in a :term:`pool` for :term:`overwriting
    errors`.

    ``pool`` is the pool whose free space is to be checked.

    If corrupted free space is found, the MPS will :term:`assert
    <assertion>`. It is only useful to call this on a :term:`debugging
    pool` that has free space splatting turned on. It does nothing on
    non-debugging pools.

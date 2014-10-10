.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/design/poolmvff/>`_

.. index::
   single: MVFF
   single: pool class; MVFF

.. _pool-mvff:

MVFF (Manual Variable First Fit)
================================

**MVFF** :term:`manually manages <manual memory management>`
variable-sized, unformatted objects. It uses the :term:`first fit`
:term:`allocation policy` for blocks allocated via
:c:func:`mps_alloc`.

:ref:`Johnstone (1997) <JOHNSTONE97>` found that in his test cases:

    No version of :term:`best fit` had more than 5% actual
    :term:`fragmentation <external fragmentation>`. This is also true
    for all versions of first fit that used an :term:`address-ordered
    free list <address-ordered first fit>`, and the two versions of
    first fit that used a :term:`FIFO free list <FIFO-ordered first
    fit>`. This strongly suggests that the basic best-fit algorithm
    and the first-fit algorithm with an address-ordered free list are
    very robust algorithms.

The MVFF pool class also supports buffered allocation (that is,
allocation via :term:`allocation points`), and in this case, the
allocation policy is different: the buffers are filled according to
the :term:`worst fit` policy, and allocation always proceeds upwards
from the base.

Buffered and unbuffered allocation can be used at the same time, but
the first allocation point must be created before any call to
:c:func:`mps_alloc`.

It is usually not advisable to use buffered and unbuffered allocation
on the same pool, because the worst-fit policy of buffer filling will
grab all the large blocks, leading to severe fragmentation. If you
need both forms of allocation, use two separate pools.


.. index::
   single: MVFF; properties

MVFF properties
---------------

* Supports allocation via :c:func:`mps_alloc`.

* Supports allocation via :term:`allocation points`. If an allocation
  point is created in an MVFF pool, the call to
  :c:func:`mps_ap_create_k` takes no keyword arguments.

* Supports deallocation via :c:func:`mps_free`.

* Supports :term:`allocation frames` but does not use them to improve
  the efficiency of stack-like allocation.

* Supports :term:`segregated allocation caches`.

* There are no garbage collections in this pool.

* Blocks may not contain :term:`references` to blocks in automatically
  managed pools (unless these are registered as :term:`roots`).

* Allocations may be variable in size.

* The :term:`alignment` of blocks is configurable, but may not be
  smaller than ``sizeof(void *)``.

* Blocks do not have :term:`dependent objects`.

* Blocks are not automatically :term:`reclaimed`.

* Blocks are not :term:`scanned <scan>`.

* Blocks are not protected by :term:`barriers (1)`.

* Blocks do not :term:`move <moving garbage collector>`.

* Blocks may not be registered for :term:`finalization`.

* Blocks must not belong to an :term:`object format`.


.. index::
   single: MVFF; interface

MVFF interface
--------------

::

   #include "mpscmvff.h"

.. c:function:: mps_pool_class_t mps_class_mvff(void)

    Return the :term:`pool class` for an MVFF (Manual Variable First
    Fit) :term:`pool`.

    When creating an MVFF pool, :c:func:`mps_pool_create_k` may take
    the following :term:`keyword arguments`:

    * :c:macro:`MPS_KEY_EXTEND_BY` (type :c:type:`size_t`, default
      65536) is the :term:`size` of block that the pool will request
      from the :term:`arena`.

    * :c:macro:`MPS_KEY_MEAN_SIZE` (type :c:type:`size_t`, default 32)
      is the predicted mean size of blocks that will be allocated from
      the pool. This is a *hint* to the MPS: the pool will be less
      efficient if this is wrong, but nothing will break.

    * :c:macro:`MPS_KEY_ALIGN` (type :c:type:`mps_align_t`, default is
      :c:macro:`MPS_PF_ALIGN`) is the
      :term:`alignment` of addresses for allocation (and freeing) in
      the pool. If an unaligned size is passed to :c:func:`mps_alloc`
      or :c:func:`mps_free`, it will be rounded up to the pool's
      alignment. The minimum alignment supported by pools of this
      class is ``sizeof(void *)``.

    * :c:macro:`MPS_KEY_SPARE` (type :c:type:`double`, default 0.75)
      is the maximum proportion of memory that the pool will keep
      spare for future allocations. If the proportion of memory that's
      free exceeds this, then the pool will return some of it to the
      arena for use by other pools.

    * :c:macro:`MPS_KEY_MVFF_ARENA_HIGH` (type :c:type:`mps_bool_t`,
      default false) determines whether new blocks are acquired at high
      addresses (if true), or at low addresses (if false).

    * :c:macro:`MPS_KEY_MVFF_SLOT_HIGH` [#not-ap]_ (type :c:type:`mps_bool_t`,
      default false) determines whether to search for the highest
      addressed free area (if true) or lowest (if false) when allocating
      using :c:func:`mps_alloc`.

    * :c:macro:`MPS_KEY_MVFF_FIRST_FIT` [#not-ap]_ (type :c:type:`mps_bool_t`, default
      true) determines whether to allocate from the highest address in a
      found free area (if true) or lowest (if false) when allocating
      using :c:func:`mps_alloc`.

    .. [#not-ap]
    
       Allocation points are not affected by
       :c:macro:`MPS_KEY_MVFF_SLOT_HIGH` or
       :c:macro:`MPS_KEY_MVFF_FIRST_FIT`.
       They use a worst-fit policy in order to maximise the number of
       in-line allocations.

    The defaults yield a a simple first-fit allocator.  Specify
    :c:macro:`MPS_KEY_MVFF_ARENA_HIGH` and
    :c:macro:`MPS_KEY_MVFF_SLOT_HIGH` true, and
    :c:macro:`MPS_KEY_MVFF_FIRST_FIT` false to get a first-fit
    allocator that works from the top of memory downwards.
    Other combinations may be useful in special circumstances.
    
    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, 1024 * 1024);
            MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, 32);
            MPS_ARGS_ADD(args, MPS_KEY_ALIGN, 8);
            MPS_ARGS_ADD(args, MPS_KEY_MVFF_ARENA_HIGH, 1);
            MPS_ARGS_ADD(args, MPS_KEY_MVFF_SLOT_HIGH, 1);
            MPS_ARGS_ADD(args, MPS_KEY_MVFF_FIRST_FIT, 0);
            res = mps_pool_create_k(&pool, arena, mps_class_mvff(), args);
        } MPS_ARGS_END(args);

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the arguments like
        this::

            mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                      mps_pool_class_t mps_class_mvff(),
                                      size_t extend_size,
                                      size_t average_size,
                                      mps_align_t alignment,
                                      mps_bool_t slot_high,
                                      mps_bool_t arena_high,
                                      mps_bool_t first_fit)


.. c:function:: mps_pool_class_t mps_class_mvff_debug(void)

    A :ref:`debugging <topic-debugging>` version of the MVFF pool
    class.

    When creating a debugging MVFF pool, :c:func:`mps_pool_create_k`
    takes seven :term:`keyword arguments`.

    * :c:macro:`MPS_KEY_EXTEND_BY`, :c:macro:`MPS_KEY_MEAN_SIZE`,
      :c:macro:`MPS_KEY_ALIGN`, :c:macro:`MPS_KEY_MVFF_ARENA_HIGH`,
      :c:macro:`MPS_KEY_MVFF_SLOT_HIGH`, and
      :c:macro:`MPS_KEY_MVFF_FIRST_FIT` are as described above, and
      :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS` specifies the debugging
      options. See :c:type:`mps_pool_debug_option_s`.

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the arguments like
        this::

            mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                      mps_pool_class_t mps_class_mvff_debug(),
                                      mps_pool_debug_option_s debug_option,
                                      size_t extend_size,
                                      size_t average_size,
                                      mps_align_t alignment,
                                      mps_bool_t slot_high,
                                      mps_bool_t arena_high,
                                      mps_bool_t first_fit)


.. index::
   pair: MVFF; introspection

MVFF introspection
------------------

::

   #include "mpscmvff.h"

.. c:function:: size_t mps_mvff_free_size(mps_pool_t pool)

    Return the total amount of free space in an MVFF pool.

    ``pool`` is the MVFF pool.

    Returns the total free space in the pool, in :term:`bytes (1)`.


.. c:function:: size_t mps_mvff_size(mps_pool_t pool)

    Return the total size of an MVFF pool.

    ``pool`` is the MVFF pool.

    Returns the total size of the pool, in :term:`bytes (1)`. This
    is the sum of allocated space and free space.

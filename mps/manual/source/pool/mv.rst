.. index::
   single: MV
   single: pool class; MV

.. _pool-mv:

MV (Manual Variable)
====================

.. deprecated:: starting with version 1.111.

    :ref:`pool-mvff` or :ref:`pool-mvt` should be used instead.

**MV** is a general-purpose :term:`manually managed <manual memory
management>` :term:`pool class` that manages :term:`blocks` of
variable size.


.. index::
   single: MV; properties

MV properties
-------------

* Supports allocation via :c:func:`mps_alloc` and deallocation via
  :c:func:`mps_free`.

* Does not support allocation via :term:`allocation points`.

* Does not support :term:`allocation frames`.

* Supports :term:`segregated allocation caches`.

* There are no garbage collections in this pool.

* Blocks may not contain :term:`references` to blocks in automatically
  managed pools (unless these are registered as :term:`roots`).

* Allocations may be variable in size.

* The :term:`alignment` of blocks is not configurable: it is the
  :term:`natural alignment` of the platform (see
  :c:macro:`MPS_PF_ALIGN`).

* Blocks do not have :term:`dependent objects`.

* Blocks are not automatically :term:`reclaimed`.

* Blocks are not :term:`scanned <scan>`.

* Blocks are not protected by :term:`barriers (1)`.

* Blocks do not :term:`move <moving garbage collector>`.

* Blocks may not be registered for :term:`finalization`.

* Blocks must not belong to an :term:`object format`.


.. index::
   single: MV; interface

MV interface
------------

::

   #include "mpscmv.h"

.. c:function:: mps_class_t mps_class_mv(void)

    Return the :term:`pool class` for an MV (Manual Variable)
    :term:`pool`.

    When creating an MV pool, :c:func:`mps_pool_create_k` may take
    three :term:`keyword arguments`:

    * :c:macro:`MPS_KEY_EXTEND_BY` (type :c:type:`size_t`,
      default 65536) is the :term:`size` of segment that the pool will
      request from the :term:`arena`.

    * :c:macro:`MPS_KEY_MEAN_SIZE` (type :c:type:`size_t`, default 32)
      is the predicted mean size of blocks that will be allocated from
      the pool. This value must be smaller than, or equal to, the
      value for :c:macro:`MPS_KEY_EXTEND_BY`.

    * :c:macro:`MPS_KEY_MAX_SIZE` (type :c:type:`size_t`,
      default 65536) is the predicted maximum size of blocks that will
      be allocated from the pool. This value must be larger than, or
      equal to, the value for :c:macro:`MPS_KEY_EXTEND_BY`.

    The mean and maximum sizes are *hints* to the MPS: the pool will be
    less efficient if these are wrong, but nothing will break.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, 32);
            MPS_ARGS_ADD(args, MPS_KEY_MAX_SIZE, 1024);
            MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, 1024 * 1024);
            res = mps_pool_create_k(&pool, arena, mps_class_mfs(), args);
        } MPS_ARGS_END(args);

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the segment size,
        mean size, and maximum size like this::

            mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                      mps_class_t mps_class_mv(),
                                      size_t extend_size,
                                      size_t average_size,
                                      mps_size_t maximum_size)


.. c:function:: mps_class_t mps_class_mv_debug(void)

    A :ref:`debugging <topic-debugging>` version of the MV pool
    class.

    When creating a debugging MV pool, :c:func:`mps_pool_create_k`
    takes four keyword arguments: :c:macro:`MPS_KEY_EXTEND_SIZE`,
    :c:macro:`MPS_KEY_MEAN_SIZE`, :c:macro:`MPS_KEY_MAX_SIZE` are as
    described above, and :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS`
    specifies the debugging options. See
    :c:type:`mps_pool_debug_option_s`.

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the arguments like
        this::

            mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                      mps_class_t mps_class_mv_debug(),
                                      mps_pool_debug_option_s debug_option,
                                      mps_size_t extend_size,
                                      mps_size_t average_size,
                                      mps_size_t maximum_size)


.. index::
   pair: MV; introspection

MV introspection
----------------

::

   #include "mpscmv.h"

.. c:function:: size_t mps_mv_free_size(mps_pool_t pool)

    Return the total amount of free space in an MV pool.

    ``pool`` is the MV pool.

    Returns the total free space in the pool, in :term:`bytes (1)`.


.. c:function:: size_t mps_mv_size(mps_pool_t pool)

    Return the total size of an MV pool.

    ``pool`` is the MV pool.

    Returns the total size of the pool, in :term:`bytes (1)`. This
    is the sum of allocated space and free space.

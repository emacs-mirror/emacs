.. index::
   single: MV pool class
   single: pool class; MV

.. _pool-mv:

MV (Manual Variable)
====================

**MV** is a general-purpose :term:`manually managed <manual memory
management>` :term:`pool class` that manages :term:`blocks` of
variable size.


.. index::
   single: MV pool class; properties

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

* The :term:`alignment` of blocks is configurable.

* Blocks do not have :term:`dependent objects`.

* Blocks are not automatically :term:`reclaimed`.

* Blocks are not :term:`scanned <scan>`.

* Blocks are not protected by :term:`barriers (1)`.

* Blocks do not :term:`move <moving garbage collector>`.

* Blocks may not be registered for :term:`finalization`.

* Blocks must not belong to an :term:`object format`.


.. index::
   single: MV pool class; interface

MV interface
------------

::

   #include "mpscmv.h"

.. c:function:: mps_pool_class_t mps_class_mv(void)

    Return the :term:`pool class` for an MV (Manual Variable)
    :term:`pool`.

    When creating an MV pool, :c:func:`mps_pool_create_k` takes four
    optional :term:`keyword arguments`:

    * :c:macro:`MPS_KEY_ALIGN` (type :c:type:`mps_align_t`, default is
      :c:macro:`MPS_PF_ALIGN`) is the :term:`alignment` of the
      addresses allocated (and freed) in the pool. The minimum
      alignment supported by pools of this class is 1 (one)
      and the maximum is the arena grain size
      (see :c:macro:`MPS_KEY_ARENA_GRAIN_SIZE`).

    * :c:macro:`MPS_KEY_EXTEND_BY` (type :c:type:`size_t`,
      default 65536) is the :term:`size` of block that the pool will
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


.. c:function:: mps_pool_class_t mps_class_mv_debug(void)

    A :ref:`debugging <topic-debugging>` version of the MV pool
    class.

    When creating a debugging MV pool, :c:func:`mps_pool_create_k`
    takes five optional keyword arguments: :c:macro:`MPS_KEY_ALIGN`,
    :c:macro:`MPS_KEY_EXTEND_SIZE`, :c:macro:`MPS_KEY_MEAN_SIZE`,
    :c:macro:`MPS_KEY_MAX_SIZE` are as described above, and
    :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS` specifies the debugging
    options. See :c:type:`mps_pool_debug_option_s`.

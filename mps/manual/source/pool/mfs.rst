.. index::
   single: MFS pool class
   single: pool class; MFS

.. _pool-mfs:

MFS (Manual Fixed Small)
========================

**MFS** is an :term:`manually managed <manual memory management>`
:term:`pool class` for small objects of fixed size.

Unlike other manual pool classes, it is not subject to :term:`internal
fragmentation`: if the population remains bounded, the memory usage
remains bounded too. On the other hand, unlike :ref:`pool-mvt` and
:ref:`pool-mvff` it does not return unused memory to the arena for
reuse by other pools.

The implementation is very simple: unlike most other :term:`pool
classes` which store their control structures separately from the
allocated blocks, MFS maintains a stack of free blocks using a pointer
in the free block. :c:func:`mps_alloc` pops this stack and
:c:func:`mps_free` pushes it.


.. index::
   single: MFS pool class; properties

MFS properties
--------------

* Supports allocation via :c:func:`mps_alloc` and deallocation via
  :c:func:`mps_free`.

* Does not support allocation via :term:`allocation points`.

* Does not support :term:`allocation frames`.

* Supports :term:`segregated allocation caches` (but using one would
  be pointless, since all blocks are the same size).

* There are no garbage collections in this pool.

* Blocks may not contain :term:`references` to blocks in automatically
  managed pools (unless these are registered as :term:`roots`).

* Allocations are fixed in size.

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
   single: MFS pool class; interface

MFS interface
-------------

::

   #include "mpscmfs.h"

.. c:function:: mps_pool_class_t mps_class_mfs(void)

    Return the :term:`pool class` for an MFS (Manual Fixed Small)
    :term:`pool`.

    When creating an MFS pool, :c:func:`mps_pool_create_k` requires
    one :term:`keyword argument`:

    * :c:macro:`MPS_KEY_MFS_UNIT_SIZE` (type :c:type:`size_t`) is the
      :term:`size` of blocks that will be allocated from this pool, in
      :term:`bytes (1)`. It must be at least one :term:`word`.

    In addition, :c:func:`mps_pool_create_k` accepts one optional
    keyword argument:

    * :c:macro:`MPS_KEY_EXTEND_BY` (type :c:type:`size_t`,
      default 65536) is the :term:`size` of block that the pool will
      request from the :term:`arena`. It must be at least as big as
      the unit size specified by the :c:macro:`MPS_KEY_MFS_UNIT_SIZE`
      keyword argument. If this is not a multiple of the unit size,
      there will be wasted space in each block.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_MFS_UNIT_SIZE, 1024);
            MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, 1024 * 1024);
            res = mps_pool_create_k(&pool, arena, mps_class_mfs(), args);
        } MPS_ARGS_END(args);

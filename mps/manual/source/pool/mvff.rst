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

It also supports buffered allocation (that is, allocation via
:term:`allocation points`), and in this case, the allocation policy is
different: the buffers are filled according to the :term:`worst fit`
policy, and allocation always proceeds upwards from the base.

Buffered and unbuffered allocation can be used at the same time, but
the first allocation point must be created before any call to
:c:func:`mps_alloc`.

It is usually not advisable to use buffered and unbuffered allocation
on the same pool, because the worst-fit policy of buffer filling will
grab all the large blocks, leading to severe fragmentation. If you
need both forms of allocation, use two separate pools.

Note that using buffered allocation prevents (for obscure technical
reasons) the pool from allocating across segment boundaries. This can
cause added external fragmentation if objects are allocated that are a
significant fraction of the segment size.

.. note::

    If you need to allocate large objects in an MVFF pool,
    :ref:`contact us <contact>`.


.. index::
   single: MVFF; properties

MVFF properties
---------------

* Supports allocation via :c:func:`mps_alloc`.

* Supports allocation via :term:`allocation points`. If an allocation
  point is created in an MVFF pool, the call to
  :c:func:`mps_ap_create` takes no additional parameters.

* Supports deallocation via :c:func:`mps_free`.

* Supports :term:`allocation frames` but does not use them to improve
  the efficiency of stack-like allocation.

* Supports :term:`segregated allocation caches`.

* There are no garbage collections in this pool.

* Allocations may be variable in size.

* The :term:`alignment` of blocks is configurable (but may not be
  smaller than the natural :term:`word` size of the platform).

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

.. c:function:: mps_class_t mps_class_mvff(void)

    Return the :term:`pool class` for an MVFF (Manual Variable First
    Fit) :term:`pool`.

    When creating an MVFF pool, :c:func:`mps_pool_create` takes six
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_mvff(),
                                  mps_size_t extend_size,
                                  mps_size_t average_size,
                                  mps_align_t alignment,
                                  mps_bool_t slot_high,
                                  mps_bool_t arena_high,
                                  mps_bool_t first_fit)

    ``extend_size`` is the :term:`size` of segment that the pool will
    request from the :term:`arena`.

    ``average_size`` is the predicted average size of blocks that will
    be allocated from the pool.

    ``alignment`` is the :term:`alignment` of addresses for allocation
    (and freeing) in the pool. If an unaligned size is passed to
    :c:func:`mps_alloc` or :c:func:`mps_free`, it will be rounded up
    to the pool's alignment. The minimum alignment supported by pools
    of this class is ``sizeof(void *)``.

    ``slot_high`` is undocumented. It must have the same value as
    ``arena_high``.

    If ``arena_high`` is true, new segments for buffered allocation
    are acquired at high addresses; if false, at low addresses.

    ``first_fit`` is undocumented and must be set to true.


.. c:function:: mps_class_t mps_class_mvff_debug(void)

    A :ref:`debugging <topic-debugging>` version of the MVFF pool
    class.

    When creating a debugging MVFF pool, :c:func:`mps_pool_create`
    takes seven extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_mvff_debug(),
                                  mps_debug_option_s debug_option,
                                  mps_size_t extend_size,
                                  mps_size_t average_size,
                                  mps_align_t alignment,
                                  mps_bool_t slot_high,
                                  mps_bool_t arena_high,
                                  mps_bool_t first_fit)

    ``debug_option`` specifies the debugging options. See
    :c:type:`mps_debug_option_s`.

    The other arguments are the same as for :c:func:`mps_class_mvff`.


.. index::
   pair: MVFF; introspection

MVFF introspection
------------------

::

   #include "mpscmvff.h"

.. c:function:: size_t mps_mvff_free_size(mps_pool_t mpspool)

    Return the total amount of free space in an MVFF pool.

    ``pool`` is the MVFF pool.

    Returns the total free space in the pool, in :term:`bytes (1)`.


.. c:function:: size_t mps_mvff_size(mps_pool_t pool)

    Return the total size of an MVFF pool.

    ``pool`` is the MVFF pool.

    Returns the total size of the pool, in :term:`bytes (1)`. This
    is the sum of allocated space and free space.

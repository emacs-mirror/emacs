.. index::
   single: MV; introduction
   single: pool class; MV

.. _pool-mv:

MV (Manual Variable)
====================

**MV** is a general-purpose :term:`manually managed <manual memory
management>` :term:`pool class` that manages :term:`blocks` of
variable size.

.. warning::

    :ref:`pool-mvt` or :ref:`pool-mvff` should be used instead.


.. index::
   single: MV; properties

MV properties
-------------

* Support allocation via :c:func:`mps_alloc` and deallocation via
  :c:func:`mps_free`.

* Does not support allocation via :term:`allocation points`.

* Does not support :term:`allocation frames`.

* Supports :term:`segregated allocation caches`.

* There are no garbage collections in this pool.

* Allocations may be variable in size.

* The :term:`alignment` of blocks is not configurable (it is the
  natural :term:`word` size of the platform).

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

    When creating an MV pool, :c:func:`mps_pool_create` takes three
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_mv(),
                                  mps_size_t extend_size,
                                  mps_size_t average_size,
                                  mps_size_t maximum_size)

    ``extend_size`` is the :term:`size` of segment that the pool will
    request from the :term:`arena`.

    ``average_size`` and ``maximum size`` are the predicted average
    and maximum size of blocks that will be allocated from the pool.
    These are hints to the MPS: the pool will be less efficient if
    these are wrong.


.. c:function:: mps_class_t mps_class_mv_debug(void)

    A :ref:`debugging <topic-debugging>` version of the MV pool
    class.

    When creating a debugging MV pool, :c:func:`mps_pool_create`
    takes four extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_mv_debug(),
                                  mps_debug_option_s debug_option,
                                  mps_size_t extend_size,
                                  mps_size_t average_size,
                                  mps_size_t maximum_size)

    ``debug_option`` specifies the debugging options. See
    :c:type:`mps_debug_option_s`.

    ``extend_szie``, ``average_size`` and ``maximum_size`` are as
    documented in :c:func:`mps_class_mv`.


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

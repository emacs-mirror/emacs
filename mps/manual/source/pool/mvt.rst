.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/design/poolmvt/>`_

.. index::
   single: MVT
   single: pool class; MVT

.. _pool-mvt:

MVT (Manual Variable Temporal)
==============================

**MVT** :term:`manually manages <manual memory management>`
variable-sized, unformatted objects. It uses an :term:`allocation
policy` termed :dfn:`temporal fit`.


.. index::
   pair: MVT; temporal fit
   single: allocation policy; temporal fit

Temporal fit
------------

Temporal fit attempts to place consecutive allocations next to each
other. It relies on delaying reuse as long as possible to permit freed
blocks to :term:`coalesce`, thus maximizing the number of consecutive
allocations that can be co-located. Temporal fit permits a very fast
allocator and a deallocator competitive in speed with all other known
policies.

Temporal fit is intended to take advantage of knowledge of object
:term:`lifetimes`, either *a priori* knowledge, or knowledge acquired
by profiling. The best performance will be achieved by allocating
objects with similar expected death times together.

A simple policy can be implemented to take advantage of MVT. Object
size is typically well-correlated with object life-expectancy, and
birth time plus lifetime gives death time, so allocating objects of
similar size sequentially from the same pool instance should result in
objects allocated close to each other dying at about the same time.

An application that has several classes of objects of widely differing
life expectancy will best be served by creating a different MVT pool
instance for each life-expectancy class. A more sophisticated policy
can use either the programmer's knowledge of the expected lifetime of
an objector any characteristic of objects that correlates with
lifetime to choose an appropriate pool instance to allocate in.

Allocating objects with unknown or very different deathtimes together
will pessimize the space performance of MVT.


.. index::
   single: MVT; properties

MVT properties
--------------

* Does not support allocation via :c:func:`mps_alloc`.

* Supports allocation via :term:`allocation points` only. If an
  allocation point is created in an MVT pool, the call to
  :c:func:`mps_ap_create` takes no additional parameters.

* Supports deallocation via :c:func:`mps_free`.

* Supports :term:`allocation frames` but does not use them to improve
  the efficiency of stack-like allocation.

* Does not support :term:`segregated allocation caches`.

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
   single: MVT; interface

MVT interface
-------------

::

   #include "mpscmv2.h"

.. c:function:: mps_class_t mps_class_mvt(void)

    Return the :term:`pool class` for an MVT (Manual Variable
    Temporal) :term:`pool`.

    When creating an MVT pool, :c:func:`mps_pool_create` takes five
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_mvt(),
                                  size_t minimum_size,
                                  size_t mean_size,
                                  size_t maximum_size,
                                  mps_count_t reserve_depth,
                                  mps_count_t fragmentation_limit)

    ``minimum_size``, ``mean_size``, and ``maximum_size`` are the
    predicted minimum, mean, and maximum :term:`size` of
    :term:`blocks` expected to be allocated in the pool. Blocks
    smaller than ``minimum_size`` and larger than ``maximum_size`` may
    be allocated, but the pool is not guaranteed to manage them
    space-efficiently. Furthermore, partial freeing is not supported
    for blocks larger than ``maximum_size``; doing so will result in
    the storage of the block never being reused. ``mean_size`` need
    not be an accurate mean, although the pool will manage
    ``mean_size`` blocks more efficiently if it is.

    ``reserve_depth`` is the expected hysteresis of the population of
    the pool. When blocks are freed, the pool will retain sufficient
    storage to allocate ``reserve_depth`` blocks of ``mean_size`` for
    near term allocations (rather than immediately making that storage
    available to other pools).

    If a pool has a stable population, or one which only grows over
    the lifetime of the pool, or one which grows steadily and then
    shrinks steadily, use a reserve depth of 0.

    It is always safe to use a reserve depth of 0, but if the
    population typically fluctuates in a range (for example, the
    client program repeatedly creates and destroys a subset of blocks
    in a loop), it is more efficient for the pool to retain enough
    storage to satisfy that fluctuation. For example, if a pool has an
    object population that typically fluctuates between 8,000 and
    10,000, use a reserve depth of 2,000.

    The reserve will not normally be available to other pools for
    allocation, even when it is not used by the pool. If this is
    undesirable, a reserve depth of 0 may be used for a pool whose
    object population does vary, at a slight cost in efficiency. The
    reserve does not guarantee any particular amount of allocation.

    ``fragmentation_limit`` is a percentage in (0, 100] that can be used
    to set an upper limit on the space overhead of MVT in case block
    death times and allocations do not correlate well. If the free
    space managed by the pool as a ratio of all the space managed by
    the pool exceeds ``fragmentation_limit``, the pool falls back to a
    first fit allocation policy, exploiting space more efficiently at
    a cost in time efficiency. A fragmentation limit of 0 would cause
    the pool to operate as a first-fit pool, at a significant cost in
    time efficiency: therefore this is not permitted.

    A fragmentation limit of 100 will cause the pool to use temporal
    fit (unless resources are exhausted). If the objects allocated in
    the pool have similar lifetime expectancies, this mode will have
    the best time- and space-efficiency. If the objects have widely
    varying lifetime expectancies, this mode will be time-efficient,
    but may be space-inefficient. An intermediate setting can be used
    to limit the space-inefficiency of temporal fit due to varying
    object life expectancies.


.. index::
   pair: MVT; introspection

MVT introspection
-----------------

::

   #include "mpscmv2.h"

.. c:function:: size_t mps_mvt_free_size(mps_pool_t pool)

    Return the total amount of free space in an MVT pool.

    ``pool`` is the MVT pool.

    Returns the total free space in the pool, in :term:`bytes (1)`.


.. c:function:: size_t mps_mvt_size(mps_pool_t pool)

    Return the total size of an MVT pool.

    ``pool`` is the MVT pool.

    Returns the total size of the pool, in :term:`bytes (1)`. This
    is the sum of allocated space and free space.

.. index::
   single: allocation; segregated-fit
   single: free list; segregated
   single: segregated allocation cache
   single: segregated free list

.. _topic-cache:

Segregated allocation caches
============================

A :dfn:`segregated allocation cache` is a data structure that can be
attached to any :term:`manually managed <manual memory management>`
:term:`pool`, that maintains a :term:`segregated free list`, that is,
a reserve of free blocks segregated by size.

Create a segregated allocation cache by preparing an array of
structures of type :c:type:`mps_sac_class_s` and passing them to
:c:func:`mps_sac_create`. The values in these structures are hints as
to the size of the blocks, the number of blocks of each size, and the
relative frequency of allocations and deallocations at that size.

For example, suppose we have a pool where we expect to allocate a
small number of relatively long-lived 128-byte objects, and a large
number of relatively short-lived 8-byte objects, we might create a
cache as follows::

    mps_sac_class_s classes[3] = {{8, 100, 10}, {128, 8, 1}};
    mps_sac_t sac;

    res = mps_sac_create(&sac, pool, sizeof classes / sizeof classes[0], classes);
    if (res != MPS_RES_OK)
        error("failed to create allocation cache");

Allocations through the cache (using :c:func:`mps_sac_alloc` or
:c:func:`MPS_SAC_ALLOC_FAST`) are serviced from the cache if possible,
otherwise from the pool. Similarly, deallocations through the cache
(using :c:func:`mps_sac_free` or :c:func:`MPS_SAC_FREE_FAST`) return
the block to the appopriate free list for its size. For example::

    Foo *foo;
    mps_addr_t p;
    mps_res_t res;

    res = mps_sac_alloc(&p, sac, sizeof *foo, false);
    if (res != MPS_RES_OK)
        error("failed to alloc foo");
    foo = p;

    /* use 'foo' */

    mps_sac_free(sac, p, sizeof *foo);

The macros :c:func:`MPS_SAC_ALLOC_FAST` and
:c:func:`MPS_SAC_FREE_FAST` allow allocation and deallocation to be
inlined in the calling functions, in the case where a free block is
found in the cache.

.. note::

    It is recommended that you deallocate a block via the same
    segregated allocation cache that you allocated it from. However,
    the system is more general than that, and in fact a block that was
    allocated from cache A can be deallocated via cache B, provided
    that:

    1. the two caches are attached to the same pool; and

    2. the two caches have the same :dfn:`class structure`, that is,
       they were created by passing identical arrays of :term:`size
       classes`.

.. warning::

    Segregated allocation caches work poorly with debugging pool
    classes: the debugging checks only happen when blocks are moved
    between the cache and the pool.


.. index::
   single: segregated allocation cache; creating

Cache interface
---------------

.. c:type:: mps_sac_t

    The type of :term:`segregated allocation caches`.


.. c:macro:: MPS_SAC_CLASS_LIMIT

    The number of :term:`size classes` that :c:func:`mps_sac_create`
    is guaranteed to accept.

    More might be accepted: in fact, there might not be any limit in
    the implementation on the maximum number of size classes, but if
    you specify more than this many, you should be prepared to handle
    the :term:`result code` :c:macro:`MPS_RES_LIMIT`.


.. c:type:: mps_sac_class_s

    The type of the structure describing a :term:`size class` in a
    :term:`segregated allocation cache`. ::

        typedef struct mps_sac_class_s {
            size_t   mps_block_size;
            size_t   mps_cached_count;
            unsigned mps_frequency;
        } mps_sac_class_s;

    An array of these structures must be passed to
    :c:func:`mps_sac_create` when creating a segregated allocation
    cache.

    ``mps_block_size`` is the maximum :term:`size` of any :term:`block`
    in this size class. It must be a multiple of the alignment of the
    :term:`alignment` of the :term:`pool` to which the cache belongs.

    ``mps_cached_count`` is the number of blocks of this size class to
    cache. It is advice to the MPS on how many blocks to cache, not an
    absolute limit. The cache policy tries to accommodate fluctuations
    in the population and minimize the cost of responding to client
    requests; the purpose of this parameter is to limit how much
    memory the :term:`client program` is willing to set aside for this
    purpose. However, a ``cached_count`` of zero prevents any caching of
    blocks falling into that size class.

    ``mps_frequency`` is a number that describes the frequency of
    requests (allocation and deallocation combined) in this size class
    relative to the other size classes in the cache.


.. c:function:: mps_res_t mps_sac_create(mps_sac_t *sac_o, mps_pool_t pool, size_t classes_count, mps_sac_class_s *classes)

    Create a :term:`segregated allocation cache` for a :term:`pool`.

    ``sac_o`` points to a location that will hold the address of the
    segregated allocation cache.

    ``pool`` is the pool the cache is attached to.

    ``classes_count`` is the number of :term:`size classes` in the
    cache.

    ``classes`` points to an array describing the size classes in the
    cache.

    Returns :c:macro:`MPS_RES_OK` if the segregated allocation cache
    is created successfully. Returns :c:macro:`MPS_RES_MEMORY` or
    :c:macro:`MPS_RES_COMMIT_LIMIT` when it fails to allocate memory
    for the internal cache structure. Returns :c:macro:`MPS_RES_LIMIT`
    if you ask for too many size classes: in this case, combine some
    small adjacent classes. Returns :c:macro:`MPS_RES_PARAM` if the
    pool doesn't support segregated allocation caches.

    After this function returns, the array of size classes pointed to
    be ``classes`` is no longer needed and may be discarded.  The
    segregated allocation cache pointed to by ``sac_o`` persists until
    it is destroyed by calling :c:func:`mps_sac_destroy`.

    This function creates an allocation cache whose :term:`free list`
    is segregated into the given size classes. The cache can get more
    memory from the given pool, or return memory to it.

    Segregated allocation caches can be associated with any pool that
    supports :term:`manual <manual memory management>` allocation with
    the functions :c:func:`mps_alloc` and :c:func:`mps_free`.

    The size classes are described by an array of element type
    :c:type:`mps_sac_class_s`. This array is used to initialize the
    segregated allocation cache, and is not needed
    after:c:func:`mps_sac_create` returns. The following constraints
    apply to the array:

    * You must specify at least one size class. 

    * All size classes must have different sizes.

    * The size classes must be given in the order of increasing size.

    * The smallest size must be at least as large as ``sizeof(void *)``.

    * Each size must be a multiple of the :term:`alignment` of the
      pool.

    * There might be a limit on how many classes can be described, but
      it will be at least :c:macro:`MPS_SAC_CLASS_LIMIT`.

    The MPS automatically provides an "overlarge" size class for
    arbitrarily large allocations above the largest size class
    described. Allocations falling into the overlarge size class are
    not cached.

    Any allocations whose size falls between two size classes are
    allocated from the larger size class.

    .. note::

        Too many size classes will slow down allocation; too few size
        classes waste more space in internal fragmentation. It is
        assumed that overlarge allocations are rare; otherwise, you
        would add another size class for them, or even create separate
        allocation caches or pools for them.


.. c:function:: void mps_sac_destroy(mps_sac_t sac)

    Destroy a :term:`segregated allocation cache`.

    ``sac`` is the segregated allocation cache to destroy.

    Returns all memory in the cache to the associated :term:`pool`.
    The pool might then return some memory to the :term:`arena`, but
    that's up to the pool's usual policy.

    Destroying the cache has no effect on blocks allocated through it.


.. c:function:: void mps_sac_flush(mps_sac_t sac)

    Flush a :term:`segregated allocation cache`, returning all memory
    held in it to the associated :term:`pool`.

    ``sac`` is the segregated allocation cache to flush.

    This is something that you'd typically do when you know you won't
    be using the segregated allocation cache for awhile, but want to
    hold on to the cache itself. Destroying a cache has the effect of
    flushing it.

    Flushing the segregated allocation cache might well cause the pool
    to return some memory to the :term:`arena`, but that's up to the
    pool's usual policy.

    .. note::

        The MPS might also decide to take memory from the segregated
        allocation cache without the :term:`client program` requesting
        a flush.

    .. note::

        The :term:`client program` is responsible for synchronizing
        the access to the cache, but if the cache decides to access
        the pool, the MPS will properly synchronize with any other
        :term:`threads` that might be accessing the same
        pool.


.. index::
   pair: segregated allocation cache; allocation

Allocation interface
--------------------

.. c:function:: mps_res_t mps_sac_alloc(mps_addr_t *p_o, mps_sac_t sac, size_t size, mps_bool_t has_reservoir_permit)

    Allocate a :term:`block` using a :term:`segregated allocation
    cache`. If no suitable block exists in the cache, ask for more
    memory from the associated :term:`pool`.

    ``p_o`` points to a location that will hold the address of the
    allocated block.

    ``sac`` is the segregated allocation cache.

    ``size`` is the :term:`size` of the block to allocate. It does not
    have to be one of the :term:`size classes` of the cache; nor does
    it have to be aligned.

    ``has_reservoir_permit`` should be false.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    address of the allocated block is ``*p_o``. The allocated block
    can be larger than requested. Blocks not matching any size class
    are allocated from the next largest class, and blocks larger than
    the largest size class are simply allocated at the requested size
    (rounded up to alignment, as usual).

    Returns :c:macro:`MPS_RES_MEMORY` if there wasn't enough memory,
    :c:macro:`MPS_RES_COMMIT_LIMIT` if the :term:`commit limit` was
    exceeded, or :c:macro:`MPS_RES_RESOURCE` if it ran out of
    :term:`virtual memory`.

    .. note::

        There's also a macro :c:func:`MPS_SAC_ALLOC_FAST` that does
        the same thing. The macro is faster, but generates more code
        and does less checking.

    .. note::

        The :term:`client program` is responsible for synchronizing
        the access to the cache, but if the cache decides to access
        the pool, the MPS will properly synchronize with any other
        :term:`threads` that might be accessing the same
        pool.

    .. note::

        Blocks allocated through a segregated allocation cache should
        only be freed through a segregated allocation cache with the
        same class structure. Calling :c:func:`mps_free` on them can
        cause :term:`memory leaks`, because the size of
        the block might be larger than you think. Naturally, the cache
        must also be attached to the same pool.


.. c:function:: MPS_SAC_ALLOC_FAST(mps_res_t res_v, mps_addr_t *p_v, mps_sac_t sac, size_t size, mps_bool_t has_reservoir_permit)

    A macro alternative to :c:func:`mps_sac_alloc`. It is faster than
    the function, but generates more code, does less checking.

    It takes an lvalue ``p_v`` which is assigned the address of the
    allocated block (instead of a pointer to a location to store
    it). It takes an additional first argument, the lvalue ``res_v``,
    which is assigned the :term:`result code`.

    .. note::

        :c:func:`MPS_SAC_ALLOC_FAST` may evaluate its arguments
        multiple times, except for ``has_reservoir_permit``, which it
        evaluates at most once, and only if it decides to access the
        pool.


.. c:function:: void mps_sac_free(mps_sac_t sac, mps_addr_t p, size_t size)

    Free a :term:`block` using a :term:`segregated allocation
    cache`. If the cache would become too full, some blocks may be
    returned to the associated :term:`pool`.

    ``sac`` is the segregated allocation cache.

    ``p`` points to the block to be freed. This block must have been
    allocated through a segregated allocation cache with the same
    class structure, attached to the same pool. (Usually, you'd use
    the same cache to allocate and deallocate a block, but the MPS is
    more flexible.)

    ``size`` is the :term:`size` of the block. It should be the size
    that was specified when the block was allocated (the cache knows
    what the real size of the block is).

    .. note::

        The :term:`client program` is responsible for synchronizing
        the access to the cache, but if the cache decides to access
        the pool, the MPS will properly synchronize with any other
        :term:`threads` that might be accessing the same
        pool.

    .. note::

        There's also a macro :c:func:`MPS_SAC_FREE_FAST` that does the
        same thing. The macro is faster, but generates more code and
        does no checking.

    .. note::

        :c:func:`mps_sac_free` does very little checking: it's
        optimized for speed. :term:`Double frees` and
        other mistakes will only be detected when the cache is flushed
        (either by calling :c:func:`mps_sac_flush` or automatically),
        and may not be detected at all, if intervening operations have
        obscured symptoms.


.. c:function:: MPS_SAC_FREE_FAST(mps_sac_t sac, mps_addr_t p, size_t size)

    A macro alternative to :c:func:`mps_sac_free` that is faster than
    the function but does no checking. The arguments are identical to
    the function.

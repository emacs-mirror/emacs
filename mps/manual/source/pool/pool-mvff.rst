.. _pool-mvff:

================================
MVFF (Manual Variable First Fit)
================================


Buffered allocation (:c:func:`mps_reserve` and :c:func:`mps_commit`) is also supported, but in that case, the policy is rather different: buffers are filled worst-fit, and allocation is always upwards from the base. The arenaHigh parameter regulates whether new segments are acquired at high or low addresses;the slotHigh and firstFit parameters do not affect buffered allocation. Buffered and unbuffered allocation can be used at the same time, but in that case, the first allocation point must be created before any call to :c:func:`mps_alloc`.

Cached allocation (:c:macro:`MPS_SAC_ALLOC` and :c:macro:`MPS_SAC_FREE`) is also supported, but in that case,the policy is a little different: allocation from the cache follows its own policy (typicallyfirst-fit), and only when the cache needs to acquire more blocks from the underlying MVFF pool does it use the usual algorithm to choose blocks for the cache.

::

    if(mps_pool_create(&pool, arena, mps_class_mvff(), 8 * 1024, 135, 4, 0, 0, 1)
       != MPS_RES_OK)
    {
        printf("Error creating pool!");
        exit(2);
    }


It is usually not advisable to use buffered and unbuffered allocation at the same time,because the worst-fit policy of buffer filling will grab all the large blocks, leading to severe fragmentation. Use two separate pools instead.

Note that using buffered allocation prevents (for obscure technical reasons) the pool from allocating across segment boundaries. This can cause added external fragmentation if objects are allocated that are a significant fraction of the segment size. (This quirk will disappear in a future version.)


==========================
Declared in ``mpscmvff.h``
==========================

.. c:function:: mps_class_t mps_class_mvff(void)

    Return the :term:`pool class` for an MVFF (Manual Variable-size
    First Fit) :term:`pool`.

    When creating an MVFF pool, :c:func:`mps_pool_create` takes six
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_mvff(),
                                  mps_size_t extendBy,
                                  mps_size_t avgSize,
                                  mps_align_t alignment,
                                  mps_bool_t slotHigh,
                                  mps_bool_t arenaHigh,
                                  mps_bool_t firstFit)

    ``extendBy`` is the :term:`size` of :term:`segment` to allocate by
    default.

    ``avgSize`` is the average size of blocks to be allocated.

    ``alignment`` is the :term:`alignment` of addresses for allocation
    (and freeing) in the pool. If an unaligned size is passed to
    :c:func:`mps_alloc` or :c:func:`mps_free`, it will be rounded up
    to the pool's alignment. The minimum alignment supported by pools
    of this class is ``sizeof(void *)``.

    ``slotHigh``, ``arenaHigh``, and ``firstFit`` are undocumented and may
    be set to (0, 0, 1) or (1, 1, 1). No other setting of these
    parameters is currently recommended.


==============================
Undocumented in ``mpscmvff.h``
==============================

.. c:function:: size_t mps_mvff_free_size(mps_pool_t mpspool)
.. c:function:: size_t mps_mvff_size(mps_pool_t pool)
.. c:function:: mps_class_t mps_class_mvff_debug(void)



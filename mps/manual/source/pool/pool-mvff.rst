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


.. _topic-cache:

=================
Allocation caches
=================


::

    void *p;
    Foo *foo;

    res = mps_sac_alloc(&p, sac, FooSIZE, is_in_panic);
    if (res != MPS_RES_OK) {
        printf("Failed to alloc foo!\n");
        exit(1);
    }
    foo = p;

    /* use foo */

    mps_sac_free(sac, p, FooSIZE);


What does this mean? (from mps_sac_alloc):

    The client is responsible for synchronising the access to the
    cache, but if the cache decides to access the pool, the MPS will
    properly synchronize with any other threads that might be
    accessing the same pool.

::

    void *p;
    Foo *foo;
    mps_res_t res;

    MPS_SAC_ALLOC_FAST(res, p, sac, FooSIZE, is_in_panic);
    if (res != MPS_RES_OK) {
        printf("Failed to alloc foo!\n");
        exit(1);
    }
    foo = p;

    /* use foo */

    MPS_SAC_FREE_FAST(sac, p, FooSIZE);

::

    mps_sac_t sac;
    mps_sac_class_s classes[3] = {{8, 38, 1}, {136, 19, 3}, {512, 4, 1}};

    #if (MPS_SAC_CLASS_LIMIT > 3)
    #  error "Too many classes!"
    #endif

    res = mps_sac_create(&sac, pool, 3, classes);
    if (res != MPS_RES_OK) {
        printf("Failed to create the allocation cache!");
        exit(1);
    }

::

    void *p;
    Foo *foo;

    res = mps_sac_alloc(&p, sac, FooSIZE, is_in_panic);
    if (res != MPS_RES_OK) {
        printf("Failed to alloc foo!\n");
        exit(1);
    }
    foo = p;

    /* use foo */

    mps_sac_free(sac, p, FooSIZE);

::

    void *p;
    Foo *foo;
    mps_res_t res;

    MPS_SAC_ALLOC_FAST(res, p, sac, FooSIZE, is_in_panic);
    if (res != MPS_RES_OK) {
        printf("Failed to alloc foo!\n");
        exit(1);
    }
    foo = p;

    /* use foo */

    MPS_SAC_FREE_FAST(sac, p, FooSIZE);


.. note::

    Some pools will work more efficiently with segregated
    allocation caches than others. [WHICH?] In the future, the MPS might
    offer pools specially optimized for particular types of cache. [WHEN?]


    Segregated allocation caches work poorly with debugging pool
    classes: the debugging checks only happen when blocks are
    moved between the cache and the pool. This will be fixed [WHEN?], but
    the speed of allocation with a debug class will always be
    similar to :c:func:`mps_alloc`, rather than cached speed.

::

    res = mps_sac_create(&sac, pool, 3, classes);
    if (res != MPS_RES_OK) {
        printf("Failed to create the allocation cache!");
        exit(1);
    }

    /* Use sac. */

    mps_sac_destroy(sac);
    mps_pool_destroy(pool);

::

    mps_sac_t sac_small, sac_large;

    res = mps_sac_create(&sac_small, pool, 3, small_classes);
    if (res != MPS_RES_OK) {
        printf("Failed to create the small allocation cache!");
        exit(1);
    }

    res = mps_sac_create(&sac_large, pool, 3, large_classes);
    if (res != MPS_RES_OK) {
        printf("Failed to create the large allocation cache!");
        exit(1);
    }

    /* Use sac_small. */

    mps_sac_flush(sac_small);

    /* Use sac_large. */

    mps_sac_flush(sac_large);

    /* Use sac_small. */

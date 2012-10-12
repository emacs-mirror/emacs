.. _topic-debugging:

===============
Debugging pools
===============

::

    static mps_pool_debug_option_s debugOptions = {
       (void *)"postpost", 8,
       (void *)"freefree", 8,
    };
    if (mps_pool_create(&pool, arena, mps_class_ams_debug(),
                        &debugOptions, 8192, 135, 8)
        != MPS_RES_OK)
    {
        printf("Error creating pool!");
        exit(2);
    }


.. _topic-debugging:

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


Interface
---------

.. c:function:: void mps_pool_check_fenceposts(mps_pool_t pool)

    Check all the :term:`fenceposts <fencepost>` in a :term:`pool`.

    ``pool`` is the pool whose fenceposts are to be checked.

    If a corrupted fencepost is found, the MPS will :term:`assert
    <assertion>`. It is only useful to call this on a :term:`debugging
    pool` that has fenceposts turned on. It does nothing on
    non-debugging pools.


.. c:function:: void mps_pool_check_free_space(mps_pool_t mps_pool)

    Check all the free space in a :term:`pool` for :term:`overwriting
    errors <overwriting error>`

    ``pool`` is the pool whose free space is to be checked.

    If corrupted free space is found, the MPS will :term:`assert
    <assertion>`. It is only useful to call this on a :term:`debugging
    pool` that has free space splatting turned on. It does nothing on
    non-debugging pools.


.. c:type:: mps_pool_debug_option_s

    The type of the structure used to pass options to
    :c:func:`mps_pool_create` for debugging :term:`pool classes <pool
    class>`. ::

        typedef struct mps_pool_debug_option_s {
            void  *fence_template;
            size_t fence_size;
            void  *free_template;
            size_t free_size;
        } mps_pool_debug_option_s;

    ``fence_template`` points to a template for :term:`fenceposts
    <fencepost>`.

    ``fence_size`` is the :term:`size` of ``fence_template`` in
    :term:`bytes <byte (1)>`, or zero if the debugging pool should not
    use fenceposts.

    ``free_template`` points to a template for splatting free space.

    ``free_size`` is the :term:`size` of ``free_template`` in bytes, or
    zero if the debugging pool should not splat free space.

    Both ``fence_size`` and ``free_size`` must be a multiple of the
    :term:`alignment` of the :term:`pool`, and also a multiple of the
    alignment of the pool's :term:`object format` if it has one.

    The debugging pool will copy the ``fence_size`` bytes pointed to by
    ``fence_template`` in a repeating pattern onto each fencepost during
    allocation, and it will copy the bytes pointed to by
    ``free_template`` in a repeating pattern over free space after the
    space is reclaimed.

    The MPS may not always use the whole of a template: it may use
    pieces smaller than the given size, for example to pad out part of
    a block that was left unused because of alignment requirements.

    Fencepost and free space templates allow the :term:`client
    program` to specify patterns:

    * that mimic illegal data values;
  
    * that cause bus errors if wrongly interpreted as pointers;

    * that cause assertions to fire if wrongly interpreted as data values;

    * that contain an instruction sequence that wold cause the program
      to signal an error or stop if wrongly interpreted as executable
      code.



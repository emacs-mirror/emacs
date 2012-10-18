.. _pool-amc:

==============================
AMC (Automatic Mostly-Copying)
==============================

General-purpose automatic (collecting) pool class. This is the most 'advanced' pool class in the MPS, intended for most client objects.

AMC is "Automatic, Mostly Copying": it uses copying collection except when prevented by ambiguous references. It is generational.

Chain: specify capacity and mortality of generations 0..N-1. Survivors from N-1 get promoted into an arena-wide topGen (often anachronistically called the "dynamic" generation).


An AMC pool is both scannable and collectable. Objects may contain exact references to other objects that will preserve such other objects. Objects may be reclaimed if they are not reachable from a root. Objects may move during collection, unless reachable via a (direct) ambiguous reference. Objects in an AMC pool may be registered for finalization. Exact (that is, non-ambiguous)references into an object in an AMC pool must be to the start of the object.

The AMC pool class exploits assumptions about object lifetimes and inter-connection variously referred to as "the generational hypothesis". In particular, the following tendencies will be efficiently exploited by such a pool:

- Most objects die young;

- Objects that don't die young will live a long time;

- Most references are backwards in time.

:c:func:`mps_ap_frame_push` and :c:func:`mps_ap_frame_pop` may be used on an allocation point in an AMC pool.They do not declare the affected objects to be definitely dead (compare with the SNC pool class),but have an undefined effect on the collection strategy.

If an allocation point is created in an AMC pool, the call to :c:func:`mps_ap_create` will take no additional parameters.


--------------------
AMC symbol reference
--------------------

::

   #include "mpscamc.h"


.. c:function:: void mps_amc_apply(mps_pool_t pool, void (*f)(mps_addr_t object, void *p, size_t s), void *p, size_t s)

    Visit all :term:`formatted objects <formatted object>` in an
    :ref:`pool-amc`.

    ``pool`` is the pool whose formatted objects you want to visit.

    ``f`` is a function that will be called for each formatted object in
    the pool. It takes three arguments: ``object`` is the address of the
    object; ``p`` and ``s`` are the corresponding arguments that were
    passed to :c:func:`mps_amc_apply`.

    ``p`` and ``s`` are arguments that will be passed to ``f`` each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    You may only call this function when the :term:`arena` is in the
    :term:`parked state`, for example, after calling
    :c:func:`mps_arena_collect` or :c:func:`mps_arena_park`.

    The function ``f`` will be called on both :term:`client <client
    object>` and :term:`padding objects <padding object>`. It is the
    job of ``f`` to distinguish, if necessary, between the two. It may
    also be called on :term:`dead` objects that the collector has not
    recycled or has been unable to recycle.

    The function ``f`` may not allocate memory or access any
    automatically-managed memory except within ``object``.

    .. topics::

        :ref:`topic-scanning`.

    .. note::

        There is no equivalent function for other pool classes, but
        there is a more general function
        :c:func:`mps_arena_formatted_objects_walk` that visits all
        formatted objects in the arena.


.. c:function:: mps_class_t mps_class_amc(void)

    Return the :term:`pool class` for an AMC (Automatic
    Mostly-Copying) :term:`pool`.

    When creating an AMC pool, :c:func:`mps_pool_create` takes two
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_amc(),
                                  mps_fmt_t fmt,
                                  mps_chain_t chain)

    ``fmt`` specifies the :term:`object format` for the objects
    allocated in the pool.

    ``chain`` specifies the :term:`generation chain` for the pool.

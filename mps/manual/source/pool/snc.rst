.. Sources:

    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/stack-alloc/>`_

.. _pool-snc:

====================
SNC (Stack No Check)
====================

An SNC pool is scannable, in that objects may contain references to objects in other pools that will keep those objects alive (depending on rank). In this sense, an SNC pool is a de-facto root.

Exact references may point to (the start of) objects in an SNC pool, but will have no effect on whether those objects are either scanned or kept alive.

If :c:func:`mps_ap_frame_pop` is used on an allocation point in an SNC pool (after a corresponding call to :c:func:`mps_ap_frame_push`), then the objects affected by the pop are effectively declared dead, and may be reclaimed by the collector. Extant references to such objects from reachable or de facto alive objects are safe, but such other objects should be dead; that is, such references must never be used.

If an allocation point is created in an SNC pool, then the call to :c:func:`mps_ap_create` will take as an additional parameter the rank (of type :c:func:`mps_rank_t`) of references in the objects to be created in that allocation point. Currently, only rank exact (:c:func:`mps_rank_exact`) is supported.

Objects in an SNC pool may not be registered for finalization.

Objects in an SNC pool will not move.



--------------------
SNC symbol reference
--------------------

::

   #include "mpscsnc.h"


.. c:function:: mps_class_t mps_class_snc(void)

    Return the :term:`pool class` for an SNC (Stack No Check)
    :term:`pool`.

    When creating an SNC pool, :c:func:`mps_pool_create` takes one
    extra argument::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_class_t mps_class_snc(),
                                  mps_fmt_t fmt)

    ``fmt`` specifies the :term:`object format` for the objects
    allocated in the pool. The format should provide at least the
    methods scan, skip, and pad.

    .. topics::

        :ref:`pool-snc`.

.. _pool-snc:

=========================
SNC (Stack No Check) pool
=========================

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/stack-alloc/index.html


An SNC pool is scannable, in that objects may contain references to objects in other pools that will keep those objects alive (depending on rank). In this sense, an SNC pool is a de-facto root.

Exact references may point to (the start of) objects in an SNC pool, but will have no effect on whether those objects are either scanned or kept alive.

If :c:func:`mps_ap_frame_pop` is used on an allocation point in an SNC pool (after a corresponding call to :c:func:`mps_ap_frame_push`), then the objects affected by the pop are effectively declared dead, and may be reclaimed by the collector. Extant references to such objects from reachable or de facto alive objects are safe, but such other objects should be dead; that is, such references must never be used.

If an allocation point is created in an SNC pool, then the call to :c:func:`mps_ap_create` will take as an additional parameter the rank (of type :c:func:`mps_rank_t`) of references in the objects to be created in that allocation point. Currently, only rank exact (:c:func:`mps_rank_exact`) is supported.

Objects in an SNC pool may not be registered for finalization.

Objects in an SNC pool will not move.

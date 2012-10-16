.. _pool-amc:

==============================
AMC (Automatic Mostly-Copying)
==============================


An AMC pool is both scannable and collectable. Objects may contain exact references to other objects that will preserve such other objects. Objects may be reclaimed if they are not reachable from a root. Objects may move during collection, unless reachable via a (direct) ambiguous reference. Objects in an AMC pool may be registered for finalization. Exact (that is, non-ambiguous)references into an object in an AMC pool must be to the start of the object.

The AMC pool class exploits assumptions about object lifetimes and inter-connection variously referred to as "the generational hypothesis". In particular, the following tendencies will be efficiently exploited by such a pool:

- Most objects die young;

- Objects that don't die young will live a long time;

- Most references are backwards in time.

:c:func:`mps_ap_frame_push` and :c:func:`mps_ap_frame_pop` may be used on an allocation point in an AMC pool.They do not declare the affected objects to be definitely dead (compare with the SNC pool class),but have an undefined effect on the collection strategy.

If an allocation point is created in an AMC pool, the call to :c:func:`mps_ap_create` will take no additional parameters.

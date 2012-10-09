.. _pool-mvt:

======================================
Manual Variable-size Temporal-fit pool
======================================

The MVT pool class manually manages variable-sized, unformatted objects. The MVT pool uses an allocation policy termed "temporal fit". Temporal fit attempts to place consecutive allocations next to each other. It relies on delaying reuse as long as possible to permit freed blocks to coalesce, thus maximizing the number of consecutive allocations that can be co-located. Temporal fit permits a very fast allocator and a deallocator competitive in speed with all other known policies.


  Temporal fit is intended to take advantage of knowledge of object lifetimes, either
  <cite>
    apriori
  </cite>
  knowledge or knowledge acquired by profiling. The best performance of the MVT pool will be achieved by allocating objects with similar expected deathtimes together.


A simple policy can be implemented to take advantage of MVT: Object size is typically well-correlated with object life-expectancy, and birthtime plus lifetime gives deathtime, so allocating objects of similar size sequentially from the same pool instance should result in objects allocated close to each other dying at about the same time.

An application that has several classes of objects of widely differing life expectancy will best be served by creating a different MVT pool instance for each life-expectancy class. A more sophisticated policy can use either the programmer's knowledge of the expected lifetime of an objector any characteristic of objects that correlates with lifetime to choose an appropriate pool instance to allocate in.

Allocating objects with unknown or very different deathtimes together will pessimize the space performance of MVT.


<h4>Example</h4>

<pre>
  if(mps_pool_create(&amp;pool, arena, mps_class_mvt(), 8, 32, 256, 70, 20)
     != MPS_RES_OK) {
   printf("Error creating pool!");
   exit(2);
 }
</pre>



Reserve depth

    If a pool has a stable population, or one which only grows over
    the lifetime of the pool, or one which grows steadily and then
    shrinks steadily, use a reserve depth of 0.

    It is always safe to use a reserve depth of 0, but if the
    population typically fluctuates in a range (for example, the
    client program repeatedly creates and destroys a subset of blocks
    in a loop), it is more efficient for the pool to retain enough
    storage to satisfy that fluctuation. For example, if a pool has an
    object population that typically fluctuates between 8,000 and
    10,000, use a reserve depth of 2,000.

    The reserve will not normally be available to other pools for
    allocation, even when it is not used by the pool. If this is
    undesirable, a reserve depth of 0 may be used for a pool whose
    object population does vary, at a slight cost in efficiency. The
    reserve does not guarantee any particular amount of allocation.

Fragmentation limit

    A fragmentation limit of 100 will cause the pool to use temporal
    fit (unless resources are exhausted). If the objects allocated in
    the pool have similar lifetime expectancies, this mode will have
    the best time- and space-efficiency. If the objects have widely
    varying lifetime expectancies,this mode will be time-efficient,
    but may be space-inefficient. An intermediate setting can be used
    to limit the space-inefficiency of temporal fit due to varying
    object life expectancies.

Allocation

    The MVT pool class only supports allocation through allocation
    points. See :c:func:`mps_ap_create`.


Deallocation

    The MVT pool class supports explicit freeing. See :c:func:`mps_pool_free`.

<h4>Internal Notes</h4>

Need a life-expectancy parameter! How else will different instances choose their Loci?

Need an alignment parameter. Perhaps this is embedded in a format parameter (when all pools have at least a null format).

It is conceivable that a client would want to mix manual and automatic pools with the manual pool being able to be a root for the automatic. To do so, MVT would need to support formatted objects and scanning. This may be added someday.

Eventually the MM product will include profiling tools that will help determine object characteristics that correlate with object lifetime and suggest how to configure the appropriate number of MVT pool instances and what characteristics to dispatch on when choosing which instance to allocate from.

[From mail.ptw.1998-08-19.02-33(0) ]

Remember Wilson's statement that the goal of a memory manager is to exploit the regularities in allocation patterns? My intent in the interface parameters is to accept measurable regularities in object populations, then the implementation can exploit them.

Perhaps the pool should accept some description of the mean and deviation of the object sizes, object population, and object lifetimes. Is that what you are getting at? [Reserve_depth is in some sense a deviation.]

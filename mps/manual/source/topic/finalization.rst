.. _topic-finalization:

============
Finalization
============

An object becomes finalizable if it is registered for finalization and the collector observes that it would otherwise be reclaimable. Once an object is finalizable the MPS may choose to finalize it (by posting a finalization message, see below) at <em>any</em> future time. Note that the subsequent creation of strong references to the object (from, say, weak references) may cause finalization to occur when an object is not otherwise reclaimable. 

When an object is finalizable, it may be finalized up to N times, where N is the number of times it has been registered for finalization. When an object is finalized, it is also deregistered for finalization (so that it will not be finalized again from the same registration).

Finalization is performed by passing a finalization message to the client, containing an exact reference to the object. See the message protocol, :c:func:`mps_message_type_finalization`, and :c:func:`mps_message_finalization_ref` for details.

If an object is registered for finalization multiple times, then there may be multiple finalization messages on the queue at the same time. On the other hand it may be necessary to discard previous finalization messages for an object before all such messages are posted on the message queue. In other words a finalization message may prevent other finalizations of the same object from occurring until the message is deleted; or, it may not.  We don't provide any guarantees either way. Clients performing multiple registrations must cope with both behaviors. In any case we expect it to be unusual for clients to register the same object multiple times.

Note that there is no guarantee that finalization will be prompt.

<a href="#mps_rank_weak">Weak references</a> do not prevent objects from being finalized.  At the point that an object is finalized, weak references will still validly refer to the object.  The fact that an object is registered for finalization prevents weak references to that object from being deleted.

Note that there will be no attempt to finalize objects in the context of :c:func:`mps_arena_destroy` or :c:func:`mps_pool_destroy`. :c:func:`mps_pool_destroy` should therefore not be invoked on pools containing objects registered for finalization.

Not all pool classes support finalization of objects.  In general only pools that manage objects whose liveness is determined by garbage collection will support finalization of objects.  For more information, see the Pool Class Catalog.



<h4>Example</h4>

<pre>
{
  mps_message_type_t type;

  if(mps_message_queue_type(&amp;type, arena)) {
    if(type == mps_message_type_finalization()) {
      process_finalization_message_from_queue();
    } else {
      unknown_message_type();
    }
  }
}
</pre>

.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/finalize/>`_

.. mps:prefix :: design.mps.finalize


Finalization
============


Overview
--------

Finalization is implemented internally using the Guardian Pool Class
(:mps:ref:`design.mps.poolmrg`). Objects can be registered for
finalization using an interface function (called
:c:func:`mps_finalize`). Notification of finalization is given to the
client via the messaging interface. :c:type:`PoolClassMRG`
(:mps:ref:`design.mps.poolmrg`) implements a Message Class which
implements the finalization messages.


History
-------

:mps:tag:`hist.0` Incomplete design. David Jones, 1997-02-14.

:mps:tag:`hist.1` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.2` Converted to reStructuredText. Gareth Rees,
2013-04-13.


Requirements
------------

:mps:tag:`req` Historically only Dylan had requirements for
finalization, see :mps:ref:`req.dylan.fun.final`. Now (2003-02-19)
Configura have requirements for finalization. Happily they are very
similar.


Architecture
------------

External interface
..................

.. c:function:: mps_res_t mps_finalize(mps_arena_t arena, mps_addr_t obj)

:mps:tag:`if.register` Increases the number of times that the object
located at ``obj`` has been registered for finalization by one. The
object must have been allocated from the arena (space). Any
finalization messages that are created for this object will appear on
the arena's message queue. The MPS will attempt to finalize the object
that number of times.

.. c:function:: void mps_definalize(mps_arena_t arena, mps_addr_t obj)

:mps:tag:`if.deregister` :c:func:`mps_definalize` reduces the number
of times that the object located at ``obj`` has been registered for
finalization by one. It is an error to definalize an object that has
not been registered for finalization.

:mps:tag:`if.deregister.not` At the moment (1997-08-20) :c:func:`mps_definalize` is not implemented.

.. c:function:: void mps_message_finalization_ref(mps_addr_t *mps_addr_return, mps_arena_t mps_arena, mps_message_t mps_message)

:mps:tag:`if.get-ref` :c:func:`mps_message_finalization_ref` returns
the reference to the finalized object stored in the finalization
message.


Implementation
--------------

:mps:tag:`int.over` Registering an object for finalization corresponds
to allocating a reference of rank FINAL to that object. This reference
is allocated in a guardian object in a pool of :c:type:`PoolClassMRG`
(see :mps:ref:`design.mps.poolmrg`).

:mps:tag:`int.arena.struct` The MRG pool used for managing final
references is kept in the Arena (Space), referred to as the "final
pool".

:mps:tag:`int.arena.lazy` The pool is lazily created. It will not be
created until the first object is registered for finalization.

:mps:tag:`int.arena.flag` There is a flag in the Arena that indicates
whether the final pool has been created yet or not.

.. c:function:: Res ArenaFinalize(Arena arena, Ref addr)

:mps:tag:`int.finalize.create` Creates the final pool if it has not
been created yet.

:mps:tag:`int.finalize.alloc` Allocates a guardian in the final pool.

:mps:tag:`int.finalize.write` Writes a reference to the object into
the guardian object.

:mps:tag:`int.finalize.all` That's all.

:mps:tag:`int.finalize.error` If either the 
creation of the pool or the allocation of the object fails then the error will 
be reported back to the caller.

:mps:tag:`int.finalize.error.no-unwind` This function does not need to
do any unwinding in the error cases because the creation of the pool
is not something that needs to be undone.

:mps:tag:`int.arena-destroy.empty` :c:func:`ArenaDestroy` empties the
message queue by calling :c:func:`MessageEmpty`.

:mps:tag:`int.arena-destroy.final-pool` If the final pool has been
created then :c:func:`ArenaDestroy` destroys the final pool.

:mps:tag:`access` :c:func:`mps_message_finalization_ref` needs to
access the finalization message to retrieve the reference and then
write it to where the client asks. This must be done carefully, in
order to avoid breaking the invariants or creating a hidden root.

:mps:tag:`access.invariants` We protect the invariants by using
special routines :c:func:`ArenaRead` and :c:func:`ArenaPoke` to read
and write the reference. This works as long as there's no
write-barrier collection. [Instead of :c:func:`ArenaPoke`, we could
put in an :c:func:`ArenaWrite` that would be identical to
:c:func:`ArenaPoke`, except for AVERring the invariant (or it can
just AVER there are no busy traces unflipped). When we get
write-barrier collection, we could change it to do the real thing, but
in the absence of a write-barrier, it's functionally identical to
:c:func:`ArenaPoke`. Pekka 1997-12-09]

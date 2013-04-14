.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/bt/>`_

.. mps:prefix:: design.mps.class-interface


Pool class interface
====================


Introduction
-------------

This document describes the interface and protocols between the MPM
and the pool class implementations. [This document should be merged
into :mps:ref:`design.mps.pool`. pekka 1999-07-20]


Document history
----------------

:mps:tag:`hist.1` Richard Brooksby started it. David Jones added
comments about how accurate this document is at 1997-08-19.

:mps:tag:`hist.2` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.3` Converted to reStructuredText. Gareth Rees,
2013-03-12.


Architecture
-------------

Methods
-------

:mps:tag:`methods` These methods are provided by pool classes as part
of the :c:type:`PoolClass` object (see :mps:ref:`impl.h.mpmst.class`).
They form the interface which allows the MPM to treat pools in a
uniform manner.

The following description is based on the definition of the
:c:type:`PoolClassStruct` (:mps:ref:`impl.h.mpmst.class`).

If a class is not required to provide a certain method then it should
set the appropriate ``PoolNo*`` method for that method. It is not
acceptable to use ``NULL``. [there are also some ``PoolTriv*`` methods
-- drj 1997-08-19]

:mps:tag:`method.name` The name field should be a short, pithy,
cryptic name for the pool class. Examples are "AMC", "MV".

The ``size`` field is the size of the pool instance structure. For the
``Foo`` :c:type:`PoolClass` this can reasonably be expected to be
``sizeof(FooStruct)``.

The ``offset`` field is the offset into the pool instance structure of
the generic :c:type:`PoolStruct`. Typically this field is called
``poolStruct``, so something like ``offsetof(FooStruct, poolStruct)``
is typical. If possible, arrange for this to be zero.

The ``init`` field is the class's init method. This method is called
via the generic function :c:func:`PoolInit`, which is in turn called
by :c:func:`PoolCreate`. The generic function allocates the pool's
structure (using the size and offset information), initializes the
:c:type:`PoolStruct` (generic part) then calls the ``init`` method to
do any class-specific initialization. Typically this means
initializing the fields in the class instance structure. If ``init``
returns a non-OK result code the instance structure will be
deallocated and the code returned to the caller of :c:func:`PoolInit``
or :c:func:`PoolCreate`. Note that the :c:type:`PoolStruct` isn't made
fully valid until :c:func:`PoolInit` returns.

The ``finish`` field is the class's finish method. This method is
called via the generic function :c:func:`PoolFinish`, which is in turn
called by :c:func:`PoolDestroy`. It is expected to finalise the pool
instance structure and release any resources allocated to the pool, it
is expected to release the memory associated with the pool instance
structure. Note that the pool is valid when it is passed to
``finish``. The :c:type:`PoolStruct` (generic part) is finished off
when the class's ``finish`` method returns.

The ``alloc`` field is the class's allocation method. This method is
called via the generic function :c:func:`PoolAlloc`. It is expected to
return a pointer to a fresh (that is, not overlapping with any other
live object) object of the required size. Failure to allocate should
be indicated by returning an appropriate Error code, and in such a
case, ``*pReturn`` should not be updated. Classes are not required to
provide this method, but they should provide at least one of ``alloc``
and ``bufferCreate``. [There is no ``bufferCreate`` -- gdr 2013-04-14]

The ``free_`` field is the class's free method. This is intended
primarily for manual style pools. this method is called via the
generic function :c:func:`PoolFree`. The parameters to this method are
required to correspond to a previous allocation request (possibly via
a buffer). It is an assertion by the client that the indicated object
is no longer required and the resources associated with it can be
recycled. Pools are not required to provide this method.

The ``bufferInit`` field is the class's buffer initialization method.
It is called by the generic function :c:func:`BufferCreate`, which
allocates the buffer descriptor and initializes the generic fields.
The pool may optionally adjust these fields or fill in extra values
when ``bufferInit`` is called, but often pools set ``bufferInit`` to
:c:func:`PoolTrivBufferInit` because they don't need to do any. If
``bufferInit`` returns a result code other than :c:macro:`ResOK`, the
buffer structure is deallocated and the code is returned to the called
of :c:func:`BufferCreate`. Note that the :c:type:`BufferStruct` isn't
fully valid until :c:func:`BufferCreate` returns.

The ``bufferFinish`` field is the class's buffer finishing method. It
is called by the the generic function :c:func:`BufferDestroy`. The
pool is expected to detach the buffer from any memory and prepare the
buffer for destruction. The class is expected to release the resources
associated with the buffer structure, and any unreserved memory in the
buffer may be recycled. It is illegal for a buffer to be destroyed
when there are pending allocations on it (that is, an allocation has
been reserved, but not committed) and this is checked in the generic
function. This method should be provided if and only if
``bufferCreate`` is provided. [there is no ``bufferCreate`` -- drj
1997-08-19]

The ``condemn`` field is used to condemn a pool. This method is called
via the generic function :c:func:`PoolCondemn`. The class is expected
to condemn a subset (possible the whole set) of objects it manages and
participate in a global trace to determine liveness. The class should
register the refsig of the condemned set with the trace using
:c:func:`TraceCondemn`. The class should expect fix requests (via the
fix method below) during a global trace. Classes are not required to
provide this method, but it is expected that automatic style classes
will. This interface is expected to change in the future. [``condemn``
now takes an action and a segment and should condemn the segment (turn
it white) if it corresponds to the interpretation of the action -- drj
1997-08-19 and is called ``Whiten`` drj 1998-02-02]

The ``mark`` field is used to mark an entire pool. This method is
called via the generic function :c:func:`PoolMark`. The class should
consider all of its objects, except any set that has been condemned in
this trace, to be marked, that is ready for scanning. The class should
arrange that any appropriate invariants are preserved possibly by the
Protection interface. Classes are not required to provide this method,
and not doing so indicates that all instances of this class will have
no fixable or traceable references in them. [no longer present,
``grey`` turns an entire segment grey -- drj 1997-08-19]

The ``scan`` field is used to perform scanning. This method is called
via the generic function :c:func:`PoolScan`. The class should scan the
segment specified. It should scan all the known live (marked, that is,
those objects on which fix has been called) on the segment and
accumulate a summary of *all* the objects on the segment. This means
that mark and sweep pools may have to jump through hoops a little bit
(see :mps:ref:`design.mps.poolasm.summary` for a pedagogical example).
Classes are not required to provide this method, and not doing so
indicates that all instances of this class will have no fixable or
traceable reference in them. [``scan`` method now takes an extra
return parameter which classes should use to indicate whether they
scanned all objects in segment or not. Classes should return summary
only of object they scanned. Caller of this method
(:c:func:`TraceScan`) is responsible for updating summaries correctly
when not a total scan. Hence no jumping through hoops required. drj
1998-01-30]

The ``fix`` field is used to perform fixing. This method is called via
the generic function :c:func:`TraceFix`. It indicates that the
specified reference has been found and the class should consider the
object live. There is provision for adjusting the value of the
reference (to allow for classes that move objects). Classes are not
required to provide this method, and not doing so indicates that the
class is not automatic style (ie it does not use global tracing to
determine liveness).

The ``reclaim`` field is used to reclaim memory. This method is called
via the generic function :c:func:`PoolReclaim`. It indicates that the
trace has fixed all references to reachable objects [actually it
indicates that any remaining white objects have now been proved
unreachable, hence are dead]. The class should consider objects that
have been condemned and not fixed in this trace to be dead and may
reclaim the resources associated with them. Classes are not required
to provide this method. [``reclaim`` is now called on each segment --
drj 1997-08-19]

The ``access`` field is used to indicate client access. This method is
called via the generic functions :c:func:`SpaceAccess` and
:c:func:`PoolAccess`. It indicates that the client has attempted to
access the specified region, but has been denied and the request
trapped due to a protection state. The class should perform any work
necessary to remove the protection whilst still preserving appropriate
invariants (typically this will be scanning work). Classes are not
required to provide this method, and not doing so indicates they never
protect any memory managed by the pool. [``access`` is no longer
present -- drj 1997-08-19]

:mps:tag:`method.act` ``Act`` is called when the MPM has decided to
execute an action that the class declared. The Class should arrange
execution of the associated work (usually by beginning an incremental
trace).

:mps:tag:`method.walk` ``Walk`` is used by the heap walker. ``Walk``
is only required to be implemented by classes which specify the
AttrFMT attribute (formatted pools). The ``Walk`` method should apply
the passed in function (along with its closure variables (which are
also passed in) and the object format) to all *black* objects in the
segment. Padding objects may or may not be included in the walk at the
classes discretion, in any case in will be the responsibility of the
client to do something sensible with padding objects. [what about
broken hearts? drj 1998-01-30]

The ``describe`` field is used to print out a description of a pool.
This method is called via the generic function :c:func:`PoolDescribe`.
The class should emit an textual description of the pool's contents
onto the specified stream. Each line should begin with two spaces.
Classes are not required to provide this method.


Events
------

:mps:tag:`replay` To work with the allocation replayer (see
:mps:ref:`design.mps.telemetry.replayer`), the pool has to emit an
event for each call to an external interface, containing all the
parameters passed by the user. If a new event type is required to
carry this information, the replayer (:mps:ref:`impl.c.eventrep`) must
then be extended to recreate the call.

:mps:tag:`replay.Init` In particular, the ``Init`` method should emit
a ``PoolInit<foo>`` event with all the pool parameters.



Text
-----

:mps:tag:`alloc.size` The pool class implementation defines the
meaning of the "size" parameter to the ``alloc`` and ``free`` methods.
It may not actually correspond to a number of bytes of memory.

:mps:tag:`alloc.size.align` In particular, the class may allow an
unaligned size to be passed.

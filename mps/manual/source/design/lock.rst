.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/lock/>`_

.. mps:prefix:: design.mps.lock


The lock module
===============

History
-------

:mps:tag:`hist.0` Incomplete design. David Moore, 1995-11-21.

:mps:tag:`hist.1` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.2` Converted to reStructuredText. Gareth Rees,
2013-04-14.


Purpose
-------

:mps:tag:`purpose` Support the locking needs of the thread-safe
design. In particular:

- recursive locks;
- binary locks;
- recursive "global" lock that need not be allocated or initialized by
  the client;
- binary "global" lock that need not be allocated or initialized by
  the client.

:mps:tag:`context` The MPS has to be able to operate in a
multi-threaded environment. The thread-safe design
(:mps:ref:`design.mps.thread-safety`) requires client-allocatable
binary locks, a global binary lock and a global recursive lock. An
interface to client-allocatable recursive locks is also present to
support any potential use, because of historic requirements, and
because the implementation will presumably be necessary anyway for the
global recursive lock.


Background
----------

:mps:tag:`need` In an environment where multiple threads are accessing
shared data. The threads which access data which is shared with other
threads need to cooperate with those threads to maintain consistency.
Locks provide a simple mechanism for doing this.

:mps:tag:`ownership` A lock is an object which may be "owned" by a
single thread at a time. By claiming ownership of a lock before
executing some piece of code a thread can guarantee that no other
thread owns the lock during execution of that code. If some other
thread holds a claim on a lock, the thread trying to claim the lock
will suspend until the lock is released by the owning thread.

:mps:tag:`data` A simple way of using this behaviour is to associate a
lock with a shared data structure. By claiming that lock around
accesses to the data, a consistent view of the structure can be seen
by the accessing thread. More generally any set of operations which
are required to be mutually exclusive may be performed so by using
locks.


Overview
--------

:mps:tag:`adt` There is an abstract datatype :c:type:`Lock` which
points to a locking structure :c:type:`LockStruct`. This structure is
opaque to any client, although an interface is provided to supply the
size of the structure for any client wishing to make a new lock. The
lock is not allocated by the module as allocation itself may require
locking. :c:type:`LockStruct` is implementation specific.

:mps:tag:`simple-lock` There are facilities for claiming and releasing
locks. :c:type:`Lock` is used for both binary and recursive locking.

:mps:tag:`global-locks` "Global" locks are so called because they are
used to protect data in a global location (such as a global variable).
The lock module provides two global locks; one recursive and one binary.
There are facilities for claiming and releasing both of these locks.
These global locks have the advantage that they need not be allocated
or atomically initialized by the client, so they may be used for
locking the initialization of the allocator itself. The binary global
lock is intended to protect mutable data, possibly in conjunction with
other local locking strategies. The recursive global lock is intended
to protect static read-only data during one-off initialization. See
:mps:ref:`design.mps.thread-safety`.

:mps:tag:`deadlock` This module does not provide any deadlock
protection. Clients are responsible for avoiding deadlock by using
traditional strategies such as ordering of locks. (See
:mps:ref:`design.mps.thread-safety.deadlock`.)

:mps:tag:`single-thread` In the single-threaded configuration, locks
are not needed and the claim/release interfaces defined to be no-ops.


Detailed design
---------------

:mps:tag:`interface` The interface comprises the following functions:

.. c:function:: size_t LockSize(void)

Return the size of a :c:type:`LockStruct` for allocation purposes.

.. c:function:: void LockInit(Lock lock)

After initialisation the lock is not owned by any thread. 

.. c:function:: void LockFinish(Lock lock)

Before finalisation the lock must not beowned by any thread.

.. c:function:: void LockClaim(Lock lock)

Claims ownership of a lock that was previously not held by current
thread.

.. c:function:: void LockReleaseMPM(Lock lock)

Releases ownership of a lock that is currently owned.

.. c:function:: void LockClaimRecursive(Lock lock)

Remembers the previous state of the lock with respect to the current
thread and claims the lock (if not already held).

.. c:function:: void LockReleaseRecursive(Lock lock)

Testores the previous state of the lock stored by corresponding
:c:func:`LockClaimRecursive` call.

.. c:function:: void LockClaimGlobal(void)

Claims ownership of the binary global lock which was previously not
held by current thread.

.. c:function:: void LockReleaseGlobal(void)

Releases ownership of the binary global lock that is currently owned.

.. c:function:: void LockClaimGlobalRecursive(void)

Remembers the previous state of the recursive global lock with respect
to the current thread and claims the lock (if not already held).

.. c:function:: void LockReleaseGlobalRecursive(void)

Restores the previous state of the recursive global lock stored by
corresponding :c:func:`LockClaimGlobalRecursive` call.

:mps:tag:`impl.recursive` For recursive claims, the list of previous
states can be simply implemented by keeping a count of the number of
claims made by the current thread so far. In multi-threaded
implementation below this is handled by the operating system. A count
is still kept and used to check correctness.

:mps:tag:`impl.global` The binary and recursive global locks may
actually be implemented using the same mechanism as normal locks.

:mps:tag:`impl.ansi` Single-Threaded Generic Implementation:

- single-thread;
- no need for locking;
- locking structure contains count;
- provides checking in debug version;
- otherwise does nothing except keep count of claims.

:mps:tag:`impl.win32` Win32 Implementation:

- supports Win32's threads;
- uses Critical Sections [ref?];
- locking structure contains a Critical Section;
- both recursive and non-recursive calls use same Windows function;
- also performs checking.

:mps:tag:`impl.linux` LinuxThreads Implementation (possibly suitable
for all PThreads implementations):

- supports LinuxThreads threads, which are an implementation of
  PThreads (see `<http://pauillac.inria.fr/~xleroy/linuxthreads/>`_);
- locking structure contains a mutex, initialized to check for
  recursive locking;
- locking structure contains a count of the number of active claims;
- non-recursive locking calls pthread_mutex_lock and expects success;
- recursive locking calls pthread_mutex_lock and expects either
  success or EDEADLK (indicating a recursive claim);
- also performs checking.

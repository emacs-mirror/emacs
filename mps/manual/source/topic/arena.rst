.. sources:

    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/arena/>`_
    `<https://info.ravenbrook.com/project/mps/master/design/arena/>`_

.. index::
   single: arena

.. _topic-arena:

Arenas
======

An arena is an object that encapsulates the state of the Memory Pool
System, and tells it where to get the memory it manages. You typically
start a session with the MPS by creating an arena with
:c:func:`mps_arena_create_k` and end the session by destroying it with
:c:func:`mps_arena_destroy`. The only function you might need to call
before making an arena is :c:func:`mps_telemetry_control`.

Before destroying an arena, you must first destroy all objects and
data in it, as usual for abstract data types in the MPS. If you can't
destroy the arena properly (for example, because your program has
crashed and you are at the debugger prompt), you can still call
:c:func:`mps_telemetry_flush` explicitly.

Other types of objects in the MPS are created "in the arena". They are
part of the world within the arena, and may interact and affect each
other.

.. index::
   single: arena; multiple

.. note::

    The MPS allows creation of multiple arenas, but you would only do
    this in unusual circumstances. It might be useful to have two
    active arenas and to try different things out in them, or you
    might be in the process of integrating two pieces of software that
    each independently uses the MPS.

    Arenas do not normally interact, but they compete with each other
    for resources, and references from one arena to another are not
    traced, though you *can* declare :term:`roots` pointing
    from one arena to another. It is not efficient to have multiple
    arenas containing :term:`automatically managed <automatic memory
    management>` :term:`pools`: if you find yourself in this
    situation it's best to find a way to move all the automatically
    managed pools to one arena.

The open source MPS comes with two classes of arena,
:ref:`topic-arena-client` and :ref:`topic-arena-vm`. These differ in
the way that they acquire the memory to be managed.

.. note::

    The MPS is designed to be extensible with new arena classes. If
    you need features that are not provided by any of the open source
    arena classes, :ref:`contact us <contact>`.


.. c:type:: mps_arena_t

    The type of :term:`arenas`.

    An arena is responsible for requesting :term:`memory (3)` from
    the operating system, making it available to :term:`pools`,
    and for :term:`garbage collection`.


.. c:type:: mps_arena_class_t

    The type of :term:`arena classes`.


.. c:function:: mps_res_t mps_arena_create_k(mps_arena_t *arena_o, mps_arena_class_t arena_class, mps_arg_s args[])

    Create an :term:`arena`.

    ``arena_o`` points to a location that will hold a pointer to the new
    arena.

    ``arena_class`` is the :term:`arena class`.

    ``args`` are :term:`keyword arguments` specific to the arena
    class. See the documentation for the arena class.

    Returns :c:macro:`MPS_RES_OK` if the arena is created
    successfully, or another :term:`result code` otherwise.

    The arena persists until it is destroyed by calling
    :c:func:`mps_arena_destroy`.


.. c:function:: mps_res_t mps_arena_create(mps_arena_t *arena_o, mps_arena_class_t arena_class, ...)

    .. deprecated:: starting with version 1.112.

        Use :c:func:`mps_arena_create_k` instead: the :term:`keyword
        arguments` interface is more reliable and produces better
        error messages.

    An alternative to :c:func:`mps_arena_create_k` that takes its
    extra arguments using the standard :term:`C` variable argument
    list mechanism.


.. c:function:: mps_res_t mps_arena_create_v(mps_arena_t *arena_o, mps_arena_class_t arena_class, va_list args)

    .. deprecated:: starting with version 1.112.

        Use :c:func:`mps_arena_create_k` instead: the :term:`keyword
        arguments` interface is more reliable and produces better
        error messages.

    An alternative to :c:func:`mps_arena_create_k` that takes its
    extra arguments using the standard :term:`C` ``va_list``
    mechanism.


.. c:function:: void mps_arena_destroy(mps_arena_t arena)

    Destroy an :term:`arena`.

    ``arena`` is the arena to destroy.

    This function checks the consistency of the arena, flushes the
    :term:`telemetry stream` and destroys the arena's internal control
    structures. Additionally, :term:`virtual memory arenas` return
    their reserved address space to the operating system if possible.

    It is an error to destroy an arena without first destroying all
    :term:`generation chains`, :term:`object formats`, :term:`pools`
    and :term:`roots` created in the arena, and deregistering all
    :term:`threads` registered with the arena.


.. index::
   single: arena class; client
   single: client arena class

.. _topic-arena-client:

Client arenas
-------------

::

    #include "mpsacl.h"

.. c:function:: mps_arena_class_t mps_arena_class_cl(void)

    Return the :term:`arena class` for a :term:`client arena`.

    A client arena gets its managed memory from the :term:`client
    program`. This memory chunk is passed when the arena is created.

    When creating a client arena, :c:func:`mps_arena_create_k` requires two
    :term:`keyword arguments`:

    * :c:macro:`MPS_KEY_ARENA_CL_BASE` (type :c:type:`mps_addr_t`) is
      the :term:`address` of the chunk of memory that will be managed
      by the arena.

    * :c:macro:`MPS_KEY_ARENA_SIZE` (type :c:type:`size_t`) is its
      size.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_ARENA_CL_BASE, base);
            MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, size);
            res = mps_arena_create_k(&arena, mps_arena_class_cl(), args);
        } MPS_ARGS_END(args);

    If the chunk is too small to hold the internal arena structures,
    :c:func:`mps_arena_create_k` returns :c:macro:`MPS_RES_MEMORY`. In
    this case, you need to use a (much) larger chunk.

    .. note::

        You don't have to provide all the memory up front: you can
        call :c:func:`mps_arena_extend` later on.

        Client arenas have no mechanism for returning unused memory.

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_arena_create`, pass the size and base
        address like this::

            mps_res_t mps_arena_create(mps_arena_t *arena_o,
                                       mps_arena_class_t mps_arena_class_cl,
                                       size_t size, mps_addr_t base)


.. c:function:: mps_res_t mps_arena_extend(mps_arena_t arena, mps_addr_t base, size_t size)

    Extend a :term:`client arena` with another block of memory.

    ``base`` is the :term:`address` of the block of memory that will be
    managed by the arena.

    ``size`` is its :term:`size`.

    Return :c:macro:`MPS_RES_OK` if successful, or another
    :term:`result code` if it fails.


.. index::
   single: arena class; virtual memory
   single: virtual memory arena class

.. _topic-arena-vm:

Virtual memory arenas
---------------------

::

    #include "mpsavm.h"

.. c:function:: mps_arena_class_t mps_arena_class_vm(void)

    Return the :term:`arena class` for a :term:`virtual memory arena`.

    A virtual memory arena uses the operating system's :term:`virtual
    memory` interface to allocate memory. The chief consequence of
    this is that the arena can manage many more virtual addresses than
    it needs to commit memory to. This gives it flexibility as to
    where to place :term:`blocks`, which reduces
    :term:`fragmentation` and helps make :term:`garbage collection`
    more efficient.

    When creating a virtual memory arena, :c:func:`mps_arena_create_k`
    accepts one :term:`keyword argument` on all platforms:

    * :c:macro:`MPS_KEY_ARENA_SIZE` (type :c:type:`size_t`, default
      2\ :superscript:`20`) is the initial amount of virtual address
      space, in :term:`bytes (1)`, that the arena will reserve (this
      space is initially reserved so that the arena can subsequently
      use it without interference from other parts of the program, but
      most of it is not committed, so it doesn't require any RAM or
      backing store). The arena may allocate more virtual address
      space beyond this initial reservation as and when it deems it
      necessary. The MPS is most efficient if you reserve an address
      space that is several times larger than your peak memory usage.

      .. note::

          The MPS asks for more address space if it runs out, but the
          more times it has to extend its address space, the less
          efficient garbage collection will become.

    A second optional :term:`keyword argument` may be passed, but it
    only has any effect on the Windows operating system:

    * :c:macro:`MPS_KEY_VMW3_TOP_DOWN` (type :c:type:`mps_bool_t`). If
      true, the arena will allocate address space starting at the
      highest possible address and working downwards through memory.

      .. note::

          This causes the arena to pass the ``MEM_TOP_DOWN`` flag to
          `VirtualAlloc`_.

          .. _VirtualAlloc: http://msdn.microsoft.com/en-us/library/windows/desktop/aa366887%28v=vs.85%29.aspx

    If the MPS fails to reserve adequate address space to place the
    arena in, :c:func:`mps_arena_create_k` returns
    :c:macro:`MPS_RES_RESOURCE`. Possibly this means that other parts
    of the program are reserving too much virtual memory.

    If the MPS fails to allocate memory for the internal arena
    structures, :c:func:`mps_arena_create_k` returns
    :c:macro:`MPS_RES_MEMORY`. Either :c:macro:`MPS_KEY_ARENA_SIZE`
    was far too small or the operating system refused to provide
    enough memory.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, size);
            res = mps_arena_create_k(&arena, mps_arena_class_cl(), args);
        } MPS_ARGS_END(args);

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_arena_create`, pass the size like
        this::

            mps_res_t mps_arena_create(mps_arena_t *arena_o,
                                       mps_arena_class_t arena_class_vm(),
                                       size_t size)


.. index::
   single: arena; properties

Arena properties
----------------

.. c:function:: mps_word_t mps_collections(mps_arena_t arena)

    Return the number of :term:`flips` that have taken place in an
    :term:`arena` since it was created.

    ``arena`` is the arena.


.. c:function:: size_t mps_arena_commit_limit(mps_arena_t arena)

    Return the current :term:`commit limit` for
    an arena.

    ``arena`` is the arena to return the commit limit for.

    Returns the commit limit in :term:`bytes (1)`. The commit limit
    controls how much memory the MPS can obtain from the operating
    system, and can be changed by calling
    :c:func:`mps_arena_commit_limit_set`.


.. c:function:: mps_res_t mps_arena_commit_limit_set(mps_arena_t arena, size_t limit)

    Change the :term:`commit limit` for an :term:`arena`.

    ``arena`` is the arena to change the commit limit for.

    ``limit`` is the new commit limit in :term:`bytes (1)`.

    Returns :c:macro:`MPS_RES_OK` if successful, or another
    :term:`result code` if not.

    If successful, the commit limit for ``arena`` is set to ``limit``. The
    commit limit controls how much memory the MPS will obtain from the
    operating system. The commit limit cannot be set to a value that
    is lower than the number of bytes that the MPS is using. If an
    attempt is made to set the commit limit to a value greater than or
    equal to that returned by :c:func:`mps_arena_committed` then it
    will succeed. If an attempt is made to set the commit limit to a
    value less than that returned by :c:func:`mps_arena_committed`
    then it will succeed only if the amount committed by the MPS can
    be reduced by reducing the amount of spare committed memory; in
    such a case the spare committed memory will be reduced
    appropriately and the attempt will succeed.

    .. note::

        :c:func:`mps_arena_commit_limit_set` puts a limit on all
        memory committed by the MPS. The :term:`spare committed
        memory` can be limited separately with
        :c:func:`mps_arena_spare_commit_limit_set`. Note that "spare
        committed" memory is subject to both limits; there cannot be
        more spare committed memory than the spare commit limit, and
        there can't be so much spare committed memory that there is
        more committed memory than the commit limit.


.. c:function:: size_t mps_arena_committed(mps_arena_t arena)

    Return the total :term:`committed <mapped>` memory for an
    :term:`arena`.

    ``arena`` is the arena.

    Returns the total amount of memory that has been committed to RAM
    by the MPS, in :term:`bytes (1)`.

    The committed memory is generally larger than the sum of the sizes
    of the allocated :term:`blocks`. The reasons for this are:

    * some memory is used internally by the MPS to manage its own data
      structures and to record information about allocated blocks
      (such as free lists, page tables, colour tables, statistics, and
      so on);

    * operating systems (and hardware) typically restrict programs to
      requesting and releasing memory with a certain granularity (for
      example, :term:`pages`), so extra memory is committed
      when this rounding is necessary;

    * there might also be :term:`spare committed memory`: see
      :c:func:`mps_arena_spare_committed`.

    The amount of committed memory is a good measure of how much
    virtual memory resource ("swap space") the MPS is using from the
    operating system.

    The function :c:func:`mps_arena_committed` may be called whatever
    state the the arena is in (:term:`unclamped <unclamped state>`,
    :term:`clamped <clamped state>`, or :term:`parked <parked
    state>`). If it is called when the arena is in the unclamped state
    then the value may change after this function returns. A possible
    use might be to call it just after :c:func:`mps_arena_collect` to
    (over-)estimate the size of the heap.

    If you want to know how much memory the MPS is using then you're
    probably interested in the value ``mps_arena_committed() -
    mps_arena_spare_committed()``.

    The amount of committed memory can be limited with the function
    :c:func:`mps_arena_commit_limit`.


.. c:function:: size_t mps_arena_reserved(mps_arena_t arena)

    Return the total :term:`address space` reserved by an
    :term:`arena`, in :term:`bytes (1)`.

    ``arena`` is the arena.

    For a :term:`virtual memory arena`, this is the total address space
    reserved via the operating system's virtual memory interface.

    For a :term:`client arena`, this is the sum of the usable portions
    of the chunks of memory passed to the arena by the :term:`client
    program` via :c:func:`mps_arena_create_k` and
    :c:func:`mps_arena_extend`.

    .. note::

        For a client arena, the reserved address may be lower than the
        sum of the :c:macro:`MPS_KEY_ARENA_SIZE` keyword argument
        passed to :c:func:`mps_arena_create_k` and the ``size``
        arguments passed to :c:func:`mps_arena_extend`, because the
        arena may be unable to use the whole of each chunk for reasons
        of alignment.


.. c:function:: size_t mps_arena_spare_commit_limit(mps_arena_t arena)

    Return the current :term:`spare commit limit` for an
    :term:`arena`.

    ``arena`` is the arena to return the spare commit limit for.

    Returns the spare commit limit in :term:`bytes (1)`. The spare
    commit limit can be changed by calling
    :c:func:`mps_arena_spare_commit_limit_set`.


.. c:function:: void mps_arena_spare_commit_limit_set(mps_arena_t arena, size_t limit)

    Change the :term:`spare commit limit` for an :term:`arena`.

    ``arena`` is the arena to change the spare commit limit for.

    ``limit`` is the new spare commit limit in :term:`bytes (1)`.

    The spare commit limit is the maximum amount of :term:`spare
    committed memory` the MPS is allowed to have. Setting it to a
    value lower than the current amount of spare committed memory
    causes spare committed memory to be uncommitted so as to bring the
    value under the limit. In particular, setting it to 0 will mean
    that the MPS will have no spare committed memory.

    Non-virtual-memory arena classes (for example, a :term:`client
    arena`) do not have spare committed memory. For these arenas, this
    function functions sets a value but has no other effect.

    Initially the spare commit limit is a configuration-dependent
    value. The value of the limit can be retrieved by the function
    :c:func:`mps_arena_spare_commit_limit`.


.. c:function:: size_t mps_arena_spare_committed(mps_arena_t arena)

    Return the total :term:`spare committed memory` for an
    :term:`arena`.

    ``arena`` is the arena.

    Returns the number of bytes of spare committed memory.

    Spare committed memory is memory which the arena is managing as
    free memory (not in use by any pool and not otherwise in use for
    internal reasons) but which remains committed (mapped to RAM by
    the operating system). It is used by the arena to (attempt to)
    avoid calling the operating system to repeatedly map and unmap
    areas of :term:`virtual memory` as the amount of memory in use
    goes up and down. Spare committed memory is counted as committed
    memory by :c:func:`mps_arena_committed` and is restricted by
    :c:func:`mps_arena_commit_limit`.

    The amount of "spare committed" memory can be limited by calling
    :c:func:`mps_arena_spare_commit_limit_set`, and the value of that
    limit can be retrieved with
    :c:func:`mps_arena_spare_commit_limit`. This is analogous to the
    functions for limiting the amount of :term:`committed <mapped>`
    memory.


.. index::
   single: arena; states

Arena states
------------

An arena is always in one of three states.

#. .. index::
      single: arena; unclamped state
      single: unclamped state

   In the *unclamped state*, garbage collection may take place,
   objects may move in memory, references may be updated,
   :term:`location dependencies` may become stale, virtual memory may
   be requested from or return to the operating system, and other
   kinds of background activity may occur. This is the normal state.

#. .. index::
      single: arena; clamped state
      single: clamped state

   In the *clamped state*, objects do not move in memory, references
   do not change, the staleness of :term:`location dependencies` does
   not change, and memory occupied by :term:`unreachable` objects is
   not recycled.

   However, a :term:`garbage collection` may be in progress and
   incremental collection may still occur, but it will not be visible
   to the :term:`client program` and no new collections will begin.

#. .. index::
      single: arena; parked state
      single: parked state

   The *parked state* is the same as the clamped state, with the
   additional constraint that no garbage collections are in progress.

Here's a summary:

======================================== ================================== ============================= ===========================
State                                    unclamped                          clamped                       parked
======================================== ================================== ============================= ===========================
Collections may be running?              yes                                yes                           no
New collections may start?               yes                                no                            no
Objects may move?                        yes                                no                            no
Location dependencies may become stale?  yes                                no                            no
Memory may be returned to the OS?        yes                                no                            no
Functions that leave arena in this state :c:func:`mps_arena_create`,        :c:func:`mps_arena_clamp`,    :c:func:`mps_arena_park`,
                                         :c:func:`mps_arena_release`,       :c:func:`mps_arena_step`      :c:func:`mps_arena_collect`
                                         :c:func:`mps_arena_start_collect`, 
                                         :c:func:`mps_arena_step`           
======================================== ================================== ============================= ===========================

The clamped and parked states are important when introspecting and
debugging. If you are examining the contents of the heap, you don't
want data moving under your feet. So for example, if your program is
stopped in GDB you might type::

    (gdb) print mps_arena_clamp(arena)

before inspecting memory, and::

    (gdb) print mps_arena_release(arena)

afterward.

The results of introspection functions like
:c:func:`mps_arena_has_addr` only remain valid while the arena remains
in the parked state, and functions like :c:func:`mps_arena_roots_walk`
can only be called in this state.


.. c:function:: void mps_arena_clamp(mps_arena_t arena)

    Put an :term:`arena` into the :term:`clamped state`.

    ``arena`` is the arena to clamp.

    In the clamped state, no object motion will occur and the
    staleness of :term:`location dependencies` will not change. All
    references to objects loaded while the arena is clamped will keep
    the same binary representation until after it is released by
    calling :c:func:`mps_arena_release`.

    In a clamped arena, incremental collection may still occur, but it
    will not be visible to the mutator and no new collections will
    begin. Space used by unreachable objects will not be recycled
    until the arena is unclamped.


.. c:function:: void mps_arena_park(mps_arena_t arena)

    Put an :term:`arena` into the :term:`parked state`.

    ``arena`` is the arena to park.

    While an arena is parked, no object motion will occur and the
    staleness of :term:`location dependencies` will not change. All
    references to objects loaded while the arena is parked will keep
    the same binary representation until after it is released.

    Any current collection is run to completion before the arena is
    parked, and no new collections will start. When an arena is in the
    parked state, it is necessarily not in the middle of a collection.


.. c:function:: void mps_arena_release(mps_arena_t arena)

    Puts an arena into the :term:`unclamped state`.

    ``arena`` is the arena to unclamp.

    While an arena is unclamped, :term:`garbage collection`, object
    motion, and other background activity can take place.


.. index::
   single: garbage collection; running
   single: collection; running

Running garbage collections
---------------------------

The Memory Pool System's garbage collector runs :term:`asynchronously
<asynchronous garbage collector>` and :term:`incrementally
<incremental garbage collection>`. This means that it is not normally
necessary to tell it when to start garbage collections, or to wait
until it has finished collecting. (But if your program has idle time
that could be productively spent by the MPS, see
:ref:`topic-arena-idle` below.)

However, during development and testing it is useful to be able to
request that MPS run a full :term:`collection cycle`. For example, you
might run frequent collections in an attempt to detect bugs in your
allocation and scanning code.


.. c:function:: void mps_arena_collect(mps_arena_t arena)

    Collect an arena and put it into the :term:`parked state`.

    ``arena`` is the arena to collect.

    The collector attempts to recycle as many unreachable objects as
    possible and reduce the size of the arena as much as possible
    (though in some cases it may increase because it becomes more
    fragmented). Note that the collector may not be able to recycle
    some objects (such as those near the destination of ambiguous
    references) even though they are not reachable.

    If you do not want the arena to remain in the parked state, you
    must explicitly call :c:func:`mps_arena_release` afterwards.

    .. note::

        It is not normally necessary to call this function: in the
        :term:`unclamped state`, collections start automatically.
        However, it may be useful during development and debugging:
        the more frequently the collector runs, the sooner and more
        reliably errors are discovered. See :ref:`guide-debug-advice`.


.. c:function:: mps_res_t mps_arena_start_collect(mps_arena_t arena)

    Request an :term:`arena` to start a full :term:`collection cycle`.

    ``arena`` is the arena.

    Returns :c:macro:`MPS_RES_OK` if a collection is started, or
    another :term:`result code` if not.

    This function puts ``arena`` into the :term:`unclamped state` and
    requests that it start a full collection cycle. The call to
    :c:func:`mps_arena_start_collect` returns quickly, leaving the
    collection to proceed incrementally (as for a collection that is
    scheduled automatically).

    .. note::

        Contrast with :c:func:`mps_arena_collect`, which does not
        return until the collection has completed.


.. index::
   single: garbage collection; limiting pause
   single: garbage collection; using idle time
   single: idle time; using for garbage collection
   single: pause; limiting

.. _topic-arena-idle:

Using idle time for collection
------------------------------

Some types of program have "idle time" in which they are waiting for
an external event such as user input or network activity. The MPS
provides a function, :c:func:`mps_arena_step`, for making use of idle
time to make memory management progress.

Here's an example illustrating the use of this function in a program's
event loop. When the program is idle (there are no client actions to
perform), it requests that the MPS spend up to 10 milliseconds on
incremental work, by calling ``mps_arena_step(arena, 0.010,
0.0)``. When this returns false to indicate that there is no more work
to do, the program blocks on the client for two seconds: if this times
out, it predicts that the user will remain idle for at least a further
second, so it calls ``mps_arena_step(arena, 0.010, 100.0)`` to tell
that it's a good time to start a collection taking up to 10 ms × 100
= 1 second, but not to pause for more than 10 ms.

The program remains responsive: the MPS doesn't take control for more
than a few milliseconds at a time (at most 10). But at the same time,
major collection work can get done at times when the program would
otherwise be idle. Of course the numbers here are only for
illustration and should be chosen based on the requirements of the
application.

::

    for (;;) { /* event loop */
        for (;;) {
            if (client_is_waiting()) {
                perform_client_action();
            } else if (!mps_arena_step(arena, 0.010, 0.0)) {
                /* no incremental MPS work remaining */
                break;
            }
        }

        if (!block_on_client_with_timeout(2.0)) {
            /* Perhaps the user has gone for a cup of coffee? Allow the
             * MPS to start a big piece of work, but don't actually pause
             * for more than 10 ms. */
            mps_arena_step(arena, 0.010, 100.0);
        }
    }


.. c:function:: mps_bool_t mps_arena_step(mps_arena_t arena, double interval, double multiplier)

    Request an :term:`arena` to do some work during a period where the
    :term:`client program` is idle.

    ``arena`` is the arena.

    ``interval`` is the time, in seconds, the MPS is permitted to
    take. It must not be negative, but may be ``0.0``.

    ``multiplier`` is the number of further similar calls that the
    client program expects to make during this idle period.

    Returns true if there was work for the MPS to do in ``arena``
    (regardless of whether or not it did any) or false if there was
    nothing to do.

    :c:func:`mps_arena_step` allows the client program to make use of
    idle time to do some garbage collection, for example when it is
    waiting for interactive input. The MPS makes every effort to
    return from this function within ``interval`` seconds, but cannot
    guarantee to do so, as it may need to call your own scanning
    code. It uses ``multiplier`` to decide whether to commence
    long-duration operations that consume CPU (such as a full
    collection): it will only start such an operation if it is
    expected to be completed within ``multiplier * interval`` seconds.

    If the arena was in the :term:`parked state` or the :term:`clamped
    state` before :c:func:`mps_arena_step` was called, it is in the
    clamped state afterwards. It it was in the :term:`unclamped
    state`, it remains there.


.. index::
   pair: arena; introspection

Arena introspection
-------------------

.. note::

    Introspection functions covered in other chapters are:

    * :c:func:`mps_addr_fmt`: determine the :term:`object format` to
      which an address belongs;
    * :c:func:`mps_arena_formatted_objects_walk`: visit all
      :term:`formatted objects` in an arena;
    * :c:func:`mps_arena_roots_walk`: visit all references in
      :term:`roots` registered with an arena; and
    * :c:func:`mps_addr_pool`: determine the :term:`pool` to which an
      address belongs.


.. c:function:: mps_bool_t mps_arena_has_addr(mps_arena_t arena, mps_addr_t addr)

    Test whether an :term:`address` is managed by an :term:`arena`.

    ``arena`` is an arena.

    ``addr`` is an address.

    Returns true if ``addr`` is managed by ``arena``; false otherwise.

    An arena manages a portion of :term:`address space`. No two arenas
    overlap, so for any particular address this function will return
    true for at most one arena.

    In general, not all addresses are managed by any arena. This is
    what allows the MPS to cooperate with other memory managers,
    shared object loaders, memory mapped file input/ouput, and so on:
    it does not steal the whole address space.

    .. note::

        The result from this function is valid only at the instant at
        which the function returned. In some circumstances the result
        may immediately become invalidated (for example, a
        :term:`garbage collection` may occur, the address in question
        may become free, the arena may choose to unmap the address and
        return storage to the operating system). For reliable results
        call this function and interpret the result while the arena is
        in the :term:`parked state`.


.. index::
   pair: arena; protection

Protection interface
--------------------

.. c:function:: void mps_arena_expose(mps_arena_t arena)

    .. deprecated:: starting with version 1.111.

    Ensure that the MPS is not protecting any :term:`page` in the
    :term:`arena` with a :term:`read barrier` or :term:`write
    barrier`.

    ``mps_arena`` is the arena to expose.

    This is expected to only be useful for debugging. The arena is
    left in the :term:`clamped state`.

    Since barriers are used during a collection, calling this function
    has the same effect as calling :c:func:`mps_arena_park`: all
    collections are run to completion, and the arena is clamped so
    that no new collections begin. The MPS also uses barriers to
    maintain :term:`remembered sets`, so calling this
    function will effectively destroy the remembered sets and any
    optimization gains from them.

    Calling this function is time-consuming: any active collections
    will be run to completion; and the next collection will have to
    recompute all the remembered sets by scanning the entire arena.

    The recomputation of the remembered sets can be avoided by calling
    :c:func:`mps_arena_unsafe_expose_remember_protection` instead of
    :c:func:`mps_arena_expose`, and by calling
    :c:func:`mps_arena_unsafe_restore_protection` before calling
    :c:func:`mps_arena_release`. Those functions have unsafe aspects
    and place restrictions on what the :term:`client program` can do
    (basically no exposed data can be changed).


.. c:function:: void mps_arena_unsafe_expose_remember_protection(mps_arena_t arena)

    .. deprecated:: starting with version 1.111.

    Ensure that the MPS is not protecting any :term:`page` in the
    :term:`arena` with a :term:`read barrier` or :term:`write
    barrier`. In addition, request the MPS to remember some parts of its
    internal state so that they can be restored later.

    ``mps_arena`` is the arena to expose.

    This function is the same as :c:func:`mps_arena_expose`, but
    additionally causes the MPS to remember its protection state. The
    remembered protection state can optionally be restored later by
    calling the :c:func:`mps_arena_unsafe_restore_protection` function.
    This is an optimization that avoids the MPS having to recompute
    all the remembered sets by scanning the entire arena.

    However, restoring the remembered protections is only safe if the
    contents of the exposed pages have not been changed; therefore
    this function should only be used if you do not intend to change
    the pages, and the remembered protection must only be restored if
    the pages have not been changed.

    The MPS will only remember the protection state if resources
    (memory) are available. If memory is low then only some or
    possibly none of the protection state will be remembered, with a
    corresponding necessity to recompute it later. The MPS provides no
    mechanism for the :term:`client program` to determine whether the
    MPS has in fact remembered the protection state.

    The remembered protection state, if any, is discarded after
    calling :c:func:`mps_arena_unsafe_restore_protection`, or as soon
    as the arena leaves the :term:`clamped state` by calling
    :c:func:`mps_arena_release`.


.. c:function:: void mps_arena_unsafe_restore_protection(mps_arena_t arena)

    .. deprecated:: starting with version 1.111.

    Restore the remembered protection state for an :term:`arena`.

    ``mps_arena`` is the arena to restore the protection state for.

    This function restores the protection state that the MPS has
    remembered when the :term:`client program` called
    :c:func:`mps_arena_unsafe_expose_remember_protection`. The purpose
    of remembering and restoring the protection state is to avoid the
    need for the MPS to recompute all the :term:`remembered sets` by scanning the entire arena, that occurs when
    :c:func:`mps_arena_expose` is used, and which causes the next
    :term:`garbage collection` to be slow.

    The client program must not change the exposed data between the
    call to :c:func:`mps_arena_unsafe_expose_remember_protection` and
    :c:func:`mps_arena_unsafe_restore_protection`. If the client
    program has changed the exposed data then
    :c:func:`mps_arena_unsafe_restore_protection` must not be called:
    in this case simply call :c:func:`mps_arena_release`.

    Calling this function does not release the arena from the clamped
    state: :c:func:`mps_arena_release` must be called to continue
    normal collections.

    Calling this function causes the MPS to forget the remember
    protection state; as a consequence the same remembered state
    cannot be restored more than once.



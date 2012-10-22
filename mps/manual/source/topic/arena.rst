.. _topic-arena:

Arenas
======

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/arena/index.html

From //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/ref-man/concepts/index.html

An objects that represents the state of the MPS.

You start a session with the MPS by making an arena and end the session by destroying 
the arena. Even if you do not destroy it, it is guaranteed not to hang on to resources; but it is better to destroy it, to finish off properly. Before destroying the arena, you must first destroy all objects and data in it.

Other types of objects are created "in the arena". They are part of the world within the arena, and may interoperate with each other.

It is possible to create multiple arenas, but you would only do this in unusual circumstances. It might be useful to have two active arenas and to try different things out in them. The maximum number of arenas that the system can support is around 10. Using multiple arenas is an advanced technique. For more information, see the Arena Protocol .

Arenas do not interact. However, they may conflict with each other in terms of resources.

::

    mps_arena_t arena;

    int main(void)
    {
        void *block;
        mps_res_t res;

        block = malloc(ARENA_SIZE);
        if (block == NULL) {
            printf("Not enough memory!");
            exit(1);
        }

        res = mps_arena_create(&arena, mps_arena_class_cl(), ARENA_SIZE, block);
        if (res != MPS_RES_OK) {
            printf("ARENA_SIZE too small");
            exit(2);
        }

        /* rest of program */
    }

::

    mps_arena_t arena;

    int main(void)
    {
        mps_res_t res;

        res = mps_arena_create(&arena, mps_arena_class_vm(), ARENA_SIZE);
        if (res != MPS_RES_OK) {
            printf("Not enough memory!");
            exit(1);
        }

        /* rest of program */
    }

::

    mps_arena_t arena;

    int main(void)
    {
        mps_res_t res;

        res = mps_arena_create(&arena, mps_arena_class_vmnz(), ARENA_SIZE);
        if (res != MPS_RES_OK) {
            printf("Not enough memory!");
            exit(1);
        }

        /* rest of program */
    }

::

    do {
        res = mps_arena_commit_limit_set(arena, limit - 100 * 1024);
        if (res != MPS_RES_OK)
            flush_caches();
    } while(res != MPS_RES_OK);

::

    mps_arena_t arena;

    int main(void)
    {
        mps_res_t res;

        res = mps_arena_create(&arena, mps_arena_class_vm(), ARENA_SIZE);
        if (res != MPS_ RES_OK) {
            printf("Not enough memory!");
            exit(1);
        }

        /* rest of program */
    }



Using idle time for collection
------------------------------

See <http://info.ravenbrook.com/mail/2003/01/03/14-13-25/0.txt>


Interface
---------

.. c:function:: void mps_arena_clamp(mps_arena_t arena)

    Put an :term:`arena` into the :term:`clamped state`.
    
    ``arena`` is the arena to clamp.

    In the clamped state, no object motion will occur and the
    staleness of :term:`location dependencies <location dependency>`
    will not change. All references to objects loaded while the arena
    is clamped will keep the same binary representation until after it
    is released by calling :c:func:`mps_arena_release`.

    In a clamped arena, incremental collection may still occur, but it
    will not be visible to the mutator and no new collections will
    begin. Space used by unreachable objects will not be recycled
    until the arena is unclamped.


.. c:type:: mps_arena_class_t

    The type of :term:`arena classes <arena class>`.


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


.. c:function:: size_t mps_arena_commit_limit(mps_arena_t arena)

    Return the current :term:`commit limit` for
    an arena.

    ``arena`` is the arena to return the commit limit for.

    Returns the commit limit in :term:`bytes <byte (1)>`. The commit
    limit controls how much memory the MPS can obtain from the
    operating system, and can be changed by calling
    :c:func:`mps_arena_commit_limit_set`.


.. c:function:: mps_res_t mps_arena_commit_limit_set(mps_arena_t arena, size_t limit)

    Change the :term:`commit limit` for an :term:`arena`.

    ``arena`` is the arena to change the commit limit for.

    ``limit`` is the new commit limit in :term:`bytes <byte (1)>`.

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
    by the MPS, in :term:`bytes <byte (1)>`.

    The committed memory is generally larger than the sum of the sizes
    of the allocated :term:`blocks <block>`. The reasons for this are:

    * some memory is used internally by the MPS to manage its own data
      structures and to record information about allocated blocks
      (such as free lists, page tables, colour tables, statistics, and
      so on);

    * operating systems (and hardware) typically restrict programs to
      requesting and releasing memory with a certain granularity (for
      example, :term:`pages <page>`), so extra memory is committed
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


.. c:function:: mps_res_t mps_arena_create(mps_arena_t *arena_o, mps_arena_class_t arena_class, ...)

    Create an :term:`arena`.

    ``arena_o`` points to a location that will hold a pointer to the new
    arena.

    ``arena_class`` is the :term:`arena class`.

    Some arena classes require additional arguments to be passed to
    :c:func:`mps_arena_create`. See the documentation for the arena
    class.

    Returns :c:macro:`MPS_RES_OK` if the arena is created
    successfully, or another :term:`result code` otherwise.

    The arena persists until it is destroyed by calling
    :c:func:`mps_arena_destroy`.

    .. note::

        There's an alternative function :c:func:`mps_arena_create_v`
        that takes its extra arguments using the standard :term:`C`
        ``va_list`` mechanism.


.. c:function:: mps_res_t mps_arena_create_v(mps_arena_t *arena_o, mps_arena_class_t arena_class, va_list args)

    An alternative to :c:func:`mps_arena_create` that takes its extra
    arguments using the standard :term:`C` ``va_list`` mechanism.


.. c:function:: void mps_arena_destroy(mps_arena_t arena)

    Destroy an :term:`arena`.

    ``arena`` is the arena to destroy.

    This function checks the consistency of the arena, flushes the
    :term:`telemetry stream` and destroys the arena's internal control
    structures. Additionally, :term:`virtual memory arenas <virtual
    memory arena>` return their reserved address space to the
    operating system if possible.

    It is an error to destroy an arena without first destroying all
    :term:`generation chains <generation chain>`, :term:`object
    formats <object format>`, :term:`pools <pool>` and :term:`roots
    <root>` created in the arena, and deregistering all :term:`threads
    <thread>` registered with the arena.


.. c:function:: void mps_arena_expose(mps_arena_t arena)

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
    maintain :term:`remembered sets <remembered set>`, so calling this
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


.. c:function:: mps_res_t mps_arena_extend(mps_arena_t arena, mps_addr_t base, size_t size)

    Extend a :term:`client arena` with another block of memory.

    ``base`` is the :term:`address` of the block of memory that will be
    managed by the arena.

    ``size`` is its :term:`size`.

    Return :c:macro:`MPS_RES_OK` if successful, or another
    :term:`result code` if it fails.


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

    The result from this function is valid only at the instant at
    which the function returned. In some circumstances the result may
    immediately become invalidated (for example, a :term:`garbage
    collection` may occur, the address in question may become free,
    the arena may choose to unmap the address and return storage to
    the operating system). For reliable results call this function and
    interpret the result while the arena is in the :term:`parked
    state`.


.. c:function:: void mps_arena_park(mps_arena_t arena)

    Put an :term:`arena` into the :term:`parked state`.

    ``arena`` is the arena to park.

    While an arena is parked, no object motion will occur and the
    staleness of :term:`location dependencies <location dependency>`
    will not change. All references to objects loaded while the arena
    is parked will keep the same binary representation until after it
    is released.

    Any current collection is run to completion before the arena is
    parked, and no new collections will start. When an arena is in the
    parked state, it is necessarily not in the middle of a collection.


.. c:function:: void mps_arena_release(mps_arena_t arena)

    Puts an arena into the :term:`unclamped state`.

    ``arena`` is the arena to unclamp.

    While an arena is unclamped, :term:`garbage collection`, object
    motion, and other background activity can take place.

    .. seealso::

        :ref:`topic-collection`.


.. c:function:: size_t mps_arena_reserved(mps_arena_t arena)

    Return the total :term:`address space` reserved by an
    :term:`arena`, in :term:`bytes <byte (1)>`.

    ``arena`` is the arena.

    For a :term:`virtual memory arena`, this is the total address space
    reserved via the operating system's virtual memory interface.

    For a :term:`client arena`, this is the sum of the usable portions
    of the chunks of memory passed to the arena by the :term:`client
    program` via :c:func:`mps_arena_create` and
    :c:func:`mps_arena_extend`.

    .. note::

        For a client arena, the reserved address may be lower than the
        sum of the ``size`` arguments passed to
        :c:func:`mps_arena_create` and :c:func:`mps_arena_extend`,
        because the arena may be unable to use the whole of each chunk
        for reasons of alignment.


.. c:function:: size_t mps_arena_spare_commit_limit(mps_arena_t arena)

    Return the current :term:`spare commit limit` for an
    :term:`arena`.

    ``arena`` is the arena to return the spare commit limit for.

    Returns the spare commit limit in :term:`bytes <byte (1)>`. The
    spare commit limit can be changed by calling
    :c:func:`mps_arena_spare_commit_limit_set`.


.. c:function:: void mps_arena_spare_commit_limit_set(mps_arena_t arena, size_t limit)

    Change the :term:`spare commit limit` for an :term:`arena`.

    ``arena`` is the arena to change the spare commit limit for.

    ``limit`` is the new spare commit limit in :term:`bytes <byte (1)>`.

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


.. c:type:: mps_arena_t

    The type of :term:`arenas <arena>`.

    An arena is responsible for requesting :term:`memory (3)` from
    the operating system, making it available to :term:`pools <pool>`,
    and for :term:`garbage collection`.


.. c:function:: void mps_arena_unsafe_expose_remember_protection(mps_arena_t arena)

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
    calling calling :c:func:`mps_arena_unsafe_restore_protection`, or
    as soon as the arena leaves the :term:`clamped state` by calling
    :c:func:`mps_arena_release`.


.. c:function:: void mps_arena_unsafe_restore_protection(mps_arena_t arena)

    Restore the remembered protection state for an :term:`arena`.

    ``mps_arena`` is the arena to restore the protection state for.

    This function restores the protection state that the MPS has
    remembered when the :term:`client program` called
    :c:func:`mps_arena_unsafe_expose_remember_protection`. The purpose
    of remembering and restoring the protection state is to avoid the
    need for the MPS to recompute all the :term:`remembered sets
    <remembered set>` by scanning the entire arena, that occurs when
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


.. c:function:: mps_word_t mps_collections(mps_arena_t arena)

    Return the number of :term:`collection cycles <collection cycle>`
    that have been completed on an :term:`arena` since it was created.

    ``arena`` is the arena.



Arena classes
-------------

.. c:function:: mps_arena_class_t mps_arena_class_cl(void)

    ::

        #include "mpsacl.h"

    Return the :term:`arena class` for a :term:`client arena`.

    A client arena gets its managed memory from the :term:`client
    program`. This memory chunk is passed when the arena is created.

    When creating a client arena, :c:func:`mps_arena_create` takes two
    extra arguments::

        mps_res_t mps_arena_create(mps_arena_t *arena_o,
                                   mps_arena_class_t mps_arena_class_cl,
                                   size_t size, mps_addr_t base)

    ``base`` is the :term:`address` of the chunk of memory that will
    be managed by the arena.

    ``size`` is its :term:`size`.

    If the chunk is too small to hold the internal arena structures,
    :c:func:`mps_arena_create` returns :c:macro:`MPS_RES_MEMORY`. In
    this case, you need to use a (much) larger chunk.


.. c:function:: mps_arena_class_t mps_arena_class_vm(void)

    ::

        #include "mpsavm.h"

    Return the :term:`arena class` for a :term:`virtual memory arena`.

    A virtual memory arena uses the operating system's :term:`virtual
    memory` interface to allocate memory. The chief consequence of
    this is that the arena can manage many more virtual addresses than
    it needs to commit memory to. This gives it flexibility as to
    where to place :term:`blocks <block>`, which reduces
    :term:`fragmentation` and helps make :term:`garbage collection`
    more efficient.

    When creating a virtual memory arena, :c:func:`mps_arena_create`
    takes one extra argument::

        mps_res_t mps_arena_create(mps_arena_t *arena_o,
                                   mps_arena_class_t arena_class_vm(),
                                   size_t size)

    ``size`` is the initial amount of virtual address space, in
    :term:`bytes <byte (1)>`, that the arena will reserve (this space
    is initially reserved so that the arena can subsequently use it
    without interference from other parts of the program, but most of
    it is not committed, so it don't require any RAM or backing
    store). The arena may allocate more virtual address space beyond
    this initial reservation as and when it deems it necessary. The
    MPS is most efficient if you reserve an address space that is
    several times larger than your peak memory usage.

    If the MPS fails to reserve adequate address space to place the
    arena in, :c:func:`mps_arena_create` returns
    :c:macro:`MPS_RES_RESOURCE`. Possibly this means that other parts
    of the program are reserving too much virtual memory.

    If the MPS fails to allocate memory for the internal arena
    structures, :c:func:`mps_arena_create` returns
    :c:macro:`MPS_RES_MEMORY`. Either ``size`` was far too small or you
    ran out of swap space.

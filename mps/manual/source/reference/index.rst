.. highlight:: c

.. Checklist of things to say about a symbol

    Signature
    Summary
    Arguments
    Result
    Status (deprecated?)
    Topic


****************
Symbol reference
****************


=====================
Declared in ``mps.h``
=====================

.. c:type:: mps_addr_t

    The type of :term:`addresses <address>` managed by the MPS, and
    also the type of :term:`references <reference>`.

    It is used in the MPS interface for any pointer that is under the
    control of the MPS. In accordance with standard :term:`C`
    practice, null pointers of type :c:type:`mps_addr_t` will never be
    used to represent a reference to a block.

    .. topics::

        :ref:`topic-allocation` and :ref:`topic-platform`.


.. c:type:: mps_align_t

    The type of an :term:`alignment`. It is an integral type
    equivalent to ``size_t``. An alignment must be a positive power of
    2.

    .. topics::

        :ref:`topic-allocation`.


.. c:function:: mps_res_t mps_alloc(mps_addr_t *p_o, mps_pool_t pool, size_t size, ...)

    Allocate a :term:`block` of memory in a :term:`pool`.

    *p_o* points to a location that will hold the address of the
    allocated block.

    *pool* the pool to allocate in.

    *size* is the :term:`size` of the block to allocate.

    Some pool classes require additional arguments to be passed to
    :c:func:`mps_alloc`. See the documentation for the pool class.

    .. topics::

        :ref:`topic-allocation`.

    .. note::

        There's an alternative function :c:func:`mps_alloc_v` that
        takes its extra arguments using the standard :term:`C`
        ``va_list`` mechanism.


.. c:function:: mps_res_t mps_alloc_v(mps_addr_t *p_o, mps_pool_t pool, size_t size, va_list args)

    An alternative to :c:func:`mps_alloc` that takes its extra
    arguments using the standard :term:`C` ``va_list`` mechanism.


.. c:function:: mps_alloc_pattern_t mps_alloc_pattern_ramp(void)

    Return an :term:`allocation pattern` indicating that allocation
    will follow a :term:`ramp pattern`.

    This indicates to the MPS that most of the blocks allocated after
    the call to :c:func:`mps_ap_alloc_pattern_begin` are likely to be
    :term:`dead` by the time of the corresponding call to
    :c:func:`mps_ap_alloc_pattern_end`.

    .. topics::

        :ref:`topic-pattern`.


.. c:function:: mps_alloc_pattern_t mps_alloc_pattern_ramp_collect_all(void)

    Return an :term:`allocation pattern` indicating that allocation
    will follow a :term:`ramp pattern`, and that the next
    :term:`garbage collection` following the ramp should be a full
    collection.

    This indicates to the MPS that most of the blocks allocated after
    the call to :c:func:`mps_ap_alloc_pattern_begin` are likely to be
    :term:`dead` by the time of the corresponding call to
    :c:func:`mps_ap_alloc_pattern_end`.

    This allocation pattern may nest with, but should not otherwise
    overlap with, allocation patterns of type
    :c:func:`mps_alloc_pattern_ramp`. In this case, the MPS may defer
    the full collection until after all ramp allocation patterns have
    ended.

    .. topics::

        :ref:`topic-pattern`.


.. c:function:: mps_res_t mps_ap_alloc_pattern_begin(mps_ap_t ap, mps_alloc_pattern_t alloc_pattern)

    Start a period of allocation that behaves according to an
    :term:`allocation pattern`. The period persists until a
    corresponding call to :c:func:`mps_ap_alloc_pattern_end`.

    *ap* is the :term:`allocation point` in which the patterned
    allocation will occur.

    *alloc_pattern* is the allocation pattern.

    Returns :c:macro:`MPS_RES_OK` if the allocation pattern is
    supported by this allocation point. At present this is always the
    case, but in future this function may return another :term:`result
    code` if the allocation pattern is not supported by the allocation
    point.

    If :c:func:`mps_ap_alloc_pattern_begin` is used multiple times on
    the same allocation point without intervening calls to
    :c:func:`mps_ap_alloc_pattern_end`, the calls match in a
    stack-like way, outermost and innermost: that is, allocation
    patterns may nest, but not otherwise overlap.

    Some allocation patterns may additionally support overlap: if so,
    the documentation for the individual pattern types will specify
    this.

    .. topics::

        :ref:`topic-pattern`.


.. c:function:: mps_res_t mps_ap_alloc_pattern_end(mps_ap_t ap, mps_alloc_pattern_t alloc_pattern)

    End a period of allocation on an :term:`allocation point` that
    behaves according to an :term:`allocation pattern`.

    *ap* is the allocation point in which the patterned allocation
    occurred.

    *alloc_pattern* is the allocation pattern.

    Returns :c:macro:`MPS_RES_OK` if the period of allocation was
    successfully ended, or :c:macro:`MPS_RES_FAIL` if there was no
    corresponding call to :c:func:`mps_ap_alloc_pattern_begin`.

    .. topics::

        :ref:`topic-pattern`.


.. c:function:: mps_res_t mps_ap_alloc_pattern_reset(mps_ap_t ap)

    End all :term:`patterned allocation <allocation pattern>` on an
    :term:`allocation point`.

    *ap* is the allocation point on which to end all patterned
    allocation.

    Returns :c:macro:`MPS_RES_OK`. It may fail in future if certain
    allocation patterns cannot be ended for that allocation point at
    that point in time.

    This function may be used to recover from error conditions.

    .. topics::

        :ref:`topic-pattern`.


.. c:function:: mps_res_t mps_ap_frame_pop(mps_ap_t ap, mps_frame_t frame)

    Declare that a set of :term:`blocks <block>` in a
    :term:`allocation frame` are :term:`dead` or likely to be dead,
    and pop the frame from the :term:`allocation point's <allocation
    point>` frame stack.

    *ap* is the allocation point in which *frame* was pushed.

    *frame* is the allocation frame whose blocks are likely to be
    dead.

    Returns a :term:`result code`.

    This function pops *frame*, making its parent the current
    frame. Popping invalidates *frame* and all frames pushed since
    *frame*. Popping *frame* also makes a declaration about the set of
    blocks which were allocated in *frame* and all frames which were
    pushed since *frame*.

    The interpretation of this declaration depends on the :term:`pool`
    that the allocation point belongs to. Typically, :term:`manual
    <manual memory management>` pool classes use this declaration to
    mean that the blocks are dead and their space can be reclaimed
    immediately, whereas :term:`automatic <automatic memory
    management>` pool classes use this declaration to mean that the
    blocks are likely to be mostly dead, and may use this declaration
    to alter its collection decisions. See the documentation for the
    pool class.

    In general a frame other than the current frame can be popped (all
    frames pushed more recently will be invalidated as well, as
    described above), but a pool class may impose the restriction that
    only the current frame may be popped. This restriction means that
    every push must have a corresponding pop. See the documentation
    for the pool class.

    It is illegal to pop frames out of order (so the sequence "A =
    push; B = push; pop A; pop B" is illegal) or to pop the same frame
    twice (so the sequence "A = push, pop A, pop A" is illegal).

    .. topics::

        :ref:`topic-frame`.


.. c:function:: mps_res_t mps_ap_frame_push(mps_frame_t *frame_o, mps_ap_t ap)

    Declare a new :term:`allocation frame` and push it onto an
    :term:`allocation point's <allocation point>` frame stack.

    *frame_o* points to a location that will hold the new frame if the
    function is successful.

    *ap* is the allocation point in which the new frame is declared.

    Returns a :term:`result code`. The creation of new frames (which
    is implicit in the action of this function) can consume resources,
    so this function can fail because there are insufficient
    resources, or if the correct protocol is not followed by the
    :term:`client program`.

    .. topics::

        :ref:`topic-frame`.


.. c:type:: mps_ap_t

    The type of :term:`allocation points <allocation point>`.

    An allocation point is an interface to a :term:`pool` which
    provides very fast allocation, and defers the need for
    synchronization in a multi-threaded environment.

    Create an allocation point for a pool by calling
    :c:func:`mps_ap_create`, and allocate memory via one by calling
    :c:func:`mps_reserve` and :c:func:`mps_commit`.

    .. topics::

        :ref:`topic-allocation`.


.. c:function:: void mps_arena_clamp(mps_arena_t arena)

    Put an :term:`arena` into the :term:`clamped state`.
    
    *arena* is the arena to clamp.

    In the clamped state, no object motion will occur and the
    staleness of :term:`location dependencies <location dependency>`
    will not change. All references to objects loaded while the arena
    is clamped will keep the same binary representation until after it
    is released by calling :c:func:`mps_arena_release`.

    In a clamped arena, incremental collection may still occur, but it
    will not be visible to the mutator and no new collections will
    begin. Space used by unreachable objects will not be recycled
    until the arena is unclamped.

    .. topics::

        :ref:`topic-arena`.


.. c:type:: mps_arena_class_t

    The type of :term:`arena classes <arena class>`.

    .. topics::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_collect(mps_arena_t arena)

    Collect an arena and put it into the :term:`parked state`.

    *arena* is the arena to collect.

    The collector attempts to recycle as many unreachable objects as
    possible and reduce the size of the arena as much as possible
    (though in some cases it may increase because it becomes more
    fragmented). Note that the collector may not be able to recycle
    some objects (such as those near the destination of ambiguous
    references) even though they are not reachable.

    If you do not want the arena to remain in the parked state, you
    must explicitly call :c:func:`mps_arena_release` afterwards.

    .. topics::

        :ref:`topic-arena`.


.. c:function:: size_t mps_arena_commit_limit(mps_arena_t arena)

    Return the current :term:`commit limit` for
    an arena.

    *arena* is the arena to return the commit limit for.

    Returns the commit limit in bytes. The commit limit controls how
    much memory the MPS can obtain from the operating system, and can
    be changed using :c:func:`mps_arena_commit_limit_set`.

    .. topics::

        :ref:`topic-arena`.


.. c:function:: mps_res_t mps_arena_commit_limit_set(mps_arena_t arena, size_t limit)

    Change the :term:`commit limit` for an :term:`arena`.

    *arena* is the arena to change the commit limit for.

    *limit* is the new commit limit in bytes.

    Returns :c:macro:`MPS_RES_OK` if successful, or another
    :term:`result code` if not.

    If successful, the commit limit for *arena* is set to *limit*. The
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

    .. topics::

        :ref:`topic-arena`.

    .. note::

        :c:func:`mps_arena_commit_limit_set` puts a limit on all
        memory committed by the MPS. The :term:`spare committed
        memory` can be limited separately with
        :c:func:`mps_arena_spare_commit_limit_set`. Note that "spare
        committed" memory is subject to both limits; there cannot be
        more spare committed memory than the spare commit limit, and
        there can't be so much spare committed memory that there is
        more committed memory than the commit limit.


.. c:function:: extern size_t mps_arena_committed(mps_arena_t arena)

    Return the total :term:`committed <mapped>` memory for an
    :term:`arena`.

    *arena* is the arena.

    Returns the total amount of memory that has been committed to RAM
    by the MPS, in bytes.

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

    .. topics::

        :ref:`topic-arena`.


.. c:function:: mps_res_t mps_arena_create(mps_arena_t *arena_o, mps_arena_class_t arena_class, ...)

    Create an :term:`arena`.

    *arena_o* points to a location that will hold a pointer to the new
    arena.

    *arena_class* is the :term:`arena class`.

    Some arena classes require additional arguments to be passed to
    :c:func:`mps_arena_create`. See the documentation for the arena
    class.

    Returns :c:macro:`MPS_RES_OK` if the arena is created
    successfully, or another :term:`result code` otherwise.

    .. topics::

        :ref:`topic-arena`.

    .. note::

        There's an alternative function :c:func:`mps_arena_create_v`
        that takes its extra arguments using the standard :term:`C`
        ``va_list`` mechanism.


.. c:function:: mps_res_t mps_arena_create_v(mps_arena_t *arena_o, mps_arena_class_t arena_class, va_list args)

    An alternative to :c:func:`mps_arena_create` that takes its extra
    arguments using the standard :term:`C` ``va_list`` mechanism.

    .. topics::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_expose(mps_arena_t arena)

    Ensure that the MPS is not protecting any :term:`page` in the
    :term:`arena` with a :term:`read barrier` or :term:`write
    barrier`.

    *mps_arena* is the arena to expose.

    This is expected to only be useful for debugging. The arena is
    left in the :term:`clamped state`.

    Since barriers are used during a collection, calling this function
    has the same effect as calling :c:func:`mps_arena_park`: all
    collections are run to completion, and the arena is clamped so
    that no new collections begin. The MPS also uses barriers to
    maintain :term:`remembered sets <remembered set>`, so calling this
    function will effectively destroy the remembered sets and any
    optimisation gains from them.

    Calling this function is time-consuming: any active collections
    will be run to completion; and the next collection will have to
    recompute all the remembered sets by scanning the entire arena.

    The recomputation of the remembered sets can be avoided by using
    :c:func:`mps_arena_unsafe_expose_remember_protection` instead of
    :c:func:`mps_arena_expose`, and by calling
    :c:func:`mps_arena_unsafe_restore_protection` before calling
    :c:func:`mps_arena_release`. Those functions have unsafe aspects
    and place restrictions on what the :term:`client program` can do
    (basically no exposed data can be changed).

    .. topics::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_formatted_objects_walk(mps_arena_t arena, mps_formatted_objects_stepper_t f, void *p, size_t s)

    Visit all :term:`formatted objects <formatted object>` in an
    :term:`arena`.

    *arena* is the arena whose formatted objects you want to visit.

    *f* is a formatted objects stepper function. It will be called for
    each formatted object in the arena. See
    :c:type:`mps_formatted_objects_stepper_t`.

    *p* and *s* are arguments that will be passed to *f* each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    Each :term:`pool class` determines for which objects the stepper
    function is called. Typically, all validly formatted objects are
    visited. During a :term:`trace` this will in general be only the
    :term:`black` objects, though the :ref:`leaf objects pool class
    <pool-lo>`, for example, will walk all
    objects since they are validly formatted whether they are black or
    :term:`white`. :term:`Padding objects <padding object>` may be
    visited at the pool classes discretion, the :term:`client program`
    should handle this case.

    The function *f* may not allocate memory or access any
    automatically-managed memory except within *object*.

    .. topics::

        :ref:`topic-arena`, :ref:`topic-format`.


.. c:function:: mps_bool_t mps_arena_has_addr(mps_arena_t arena, mps_addr_t addr)

    Test whether an :term:`address` is managed by an :term:`arena`. 

    *arena* is an arena.

    *addr* is an address.

    Returns true if *addr* is managed by *arena*; false otherwise.

    An arena manages a portion of :term:`address space`. No two arenas
    overlap, so for any particular address this function will return
    true for at most one arena. In general, not all addresses are
    managed by some arena; in other words, some addresses will not be
    managed by any arena. This is what allows the MPS to cooperate
    with other memory managers, shared object loaders, memory mapped
    file input/ouput, and so on: it does not steal the whole address
    space.

    The result from this function is valid only at the instant at
    which the function returned. In some circumstances the result may
    immediately become invalidated (for example, a :term:`garbage
    collection` may occur, the address in question may become free,
    the arena may choose to unmap the address and return storage to
    the operating system). For reliable results call this function
    whilst the arena is in the :term:`parked state`.

    .. topics::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_park(mps_arena_t arena)

    Put an :term:`arena` into the :term:`parked state`.

    *arena* is the arena to park.

    While an arena is parked, no object motion will occur and the
    staleness of :term:`location dependencies <location dependency>`
    will not change. All references to objects loaded while the arena
    is parked will keep the same binary representation until after it
    is released.

    Any current collection is run to completion before the arena is
    parked, and no new collections will start. When an arena is in the
    parked state, it is necessarily not in the middle of a collection.

    .. topics::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_release(mps_arena_t arena)

    Puts an arena into the :term:`unclamped state`.

    *arena* is the arena to unclamp.

    While an arena is unclamped, :term:`garbage collection`, object
    motion, and other background activity can take place.

    .. topics::

        :ref:`topic-arena`, :ref:`topic-collection`.


.. c:function:: void mps_arena_roots_walk(mps_arena_t arena, mps_roots_stepper_t f, void *p, size_t s)

    Visit references in registered :term:`roots <root>` in an
    :term:`arena`.

    *arena* is the arena whose roots you want to visit.

    *f* is a function that will be called for each reference to an
    object in an :term:`automatically <automatic memory management>`
    managed :term:`pool class` that was found in a registered root
    beloging to the arena. It takes four arguments: *ref* is the
    address of a reference to an object in the arena, *root* is the
    root in which *ref* was found, and *p* and *s* are the
    corresponding arguments that were passed to
    :c:func:`mps_arena_roots_walk`.

    *p* and *s* are arguments that will be passed to *f* each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    This function may only be called when the arena is in the
    :term:`parked state`.

    .. topics::

        :ref:`topic-arena`.

    .. note::

        If a root is :term:`ambiguous <ambiguous root>` then the
        reference might not be to the start of an object; the
        :term:`client program` should handle this case. There is no
        guarantee that the reference corresponds to the actual
        location that holds the pointer to the object (since this
        might be a register, for example), but the actual location
        will be passed if possible. This may aid analysis of roots via
        a debugger.


.. c:function:: size_t mps_arena_spare_commit_limit(mps_arena_t arena)

    Return the current :term:`spare commit limit` for an
    :term:`arena`.

    *arena* is the arena to return the spare commit limit for.

    Returns the spare commit limit in bytes. The spare commit limit
    can be changed by calling
    :c:func:`mps_arena_spare_commit_limit_set`.

    .. topics::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_spare_commit_limit_set(mps_arena_t arena, size_t limit)

    Change the :term:`spare commit limit` for an :term:`arena`.

    *arena* is the arena to change the spare commit limit for.

    *limit* is the new spare commit limit in bytes.

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

    .. topics::

        :ref:`topic-arena`.


.. c:function:: size_t mps_arena_spare_committed(mps_arena_t arena)

    Return the total :term:`spare committed memory` for an
    :term:`arena`.

    *arena* is the arena.

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

    The amount of "spare committed" memory can be limited by using
    :c:func:`mps_arena_spare_commit_limit_set`, and the value of that
    limit can be retrieved with
    :c:func:`mps_arena_spare_commit_limit`. This is analogous to the
    functions for limiting the amount of :term:`committed <mapped>`
    memory.

    .. topics::

        :ref:`topic-arena`.


.. c:type:: mps_arena_t

    The type of :term:`arenas <arena>`.

    An arena is responsible for requesting :term:`memory (3)` from
    the operating system, making it available to :term:`pools <pool>`,
    and for :term:`garbage collection`.

    .. topics::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_unsafe_expose_remember_protection(mps_arena_t arena)

    Ensure that the MPS is not protecting any :term:`page` in the
    :term:`arena` with a :term:`read barrier` or :term:`write
    barrier`. In addition, request the MPS to remember some parts of its
    internal state so that they can be restored later.

    *mps_arena* is the arena to expose.

    This function is the same as :c:func:`mps_arena_expose`, but
    additionally causes the MPS to remember its protection state. The
    remembered protection state can optionally be restored later by
    using the :c:func:`mps_arena_unsafe_restore_protection` function.
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

    .. topics::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_unsafe_restore_protection(mps_arena_t arena)

    Restore the remembered protection state for an :term:`arena`.

    *mps_arena* is the arena to restore the protection state for.

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

    .. topics::

        :ref:`topic-arena`.


.. c:type:: mps_bool_t

    The type of a Boolean value. It is an integral type equivalent to
    ``int``.

    When used as an input parameter to the MPS, a value of 0 means
    "false" and any other value means "true". As an output parameter
    or function return from the MPS, 0 means "false", and 1 means
    "true".


.. c:type:: mps_class_t

    The type of :term:`pool classes <pool class>`.


.. c:function:: mps_res_t mps_finalize(mps_arena_t arena, mps_addr_t *ref)

    Register a :term:`block` for :term:`finalization`.

    *arena* is the arena in which the block lives.

    *ref* points to a :term:`reference` to the block to be finalized.
 
    Returns :c:macro:`MPS_RES_OK` if successful, or another
    :term:`result code` if not.

    This function registers *block* for finalization. This block must
    have been allocated from a :term:`pool` in *arena*. Violations of
    this constraint may not be checked by the MPS, and may be unsafe,
    causing the MPS to crash in undefined ways.

    .. topics::

        :ref:`topic-finalization`.

    .. note::

        This function receives a pointer to a reference. This is to
        avoid placing the restriction on the :term:`client program`
        that the C call stack be a :term:`root`.


.. c:function:: mps_res_t mps_fix(mps_ss_t ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it.
    This function must only be called from within a :term:`scan
    method`.

    *ss* is the :term:`scan state` that was passed to the scan method.

    *ref_io* points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    reference may have been updated (see the topic
    :ref:`topic-moving`), and the scan method must continue to scan
    the :term:`block`. If it returns any other result, the scan method
    must return that result as soon as possible, without fixing any
    further references.

    .. deprecated:: 1.110

        Use :c:func:`MPS_FIX12` instead.

    .. topics::

        :ref:`topic-scanning` and :ref:`topic-moving`.

    .. note::

        If you want to call this between :c:func:`MPS_SCAN_BEGIN` and
        :c:func:`MPS_SCAN_END`, you must use :c:func:`MPS_FIX_CALL`
        to ensure that the scan state is passed correctly.


.. c:function:: mps_bool_t MPS_FIX1(mps_ss_t ss, mps_addr_t ref)

    Tell the MPS about a :term:`reference`. This macro must only be
    used within a :term:`scan method`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    *ss* is the :term:`scan state` that was passed to the scan method.

    *ref* is the reference.

    Returns a truth value (:c:type:`mps_bool_t`) indicating whether
    the reference is likely to be interesting to the MPS. If it
    returns false, the scan method must continue scanning the
    :term:`block`. If it returns true, the scan method must invoke
    :c:func:`MPS_FIX2`, to fix the reference.

    .. topics::

        :ref:`topic-scanning`.

    .. note::

        In the common case where the scan method does not need to do
        anything between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`,
        you can use the convenience macro :c:func:`MPS_FIX12`.


.. c:function:: MPS_FIX12(mps_ss_t ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it.
    This macro must only be used within a :term:`scan method`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    *ss* is the :term:`scan state` that was passed to the scan method.

    *ref_io* points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    reference may have been updated (see the topic
    :ref:`topic-moving`), and the scan method must continue to scan
    the :term:`block`. If it returns any other result, the scan method
    must return that result as soon as possible, without fixing any
    further references.

    .. topics::

        :ref:`topic-scanning`.

    .. note::

        The macro :c:func:`MPS_FIX12` is a convenience for the common
        case where :c:func:`MPS_FIX1` is immediately followed by
        :c:func:`MPS_FIX2`.


.. c:function:: MPS_FIX2(mps_ss_t ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it.
    This macro must only be used within a :term:`scan method`,
    between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    *ss* is the :term:`scan state` that was passed to the scan method.

    *ref_io* points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    reference may have been updated (see the topic
    :ref:`topic-moving`), and the scan method must continue to scan
    the :term:`block`. If it returns any other result, the scan method
    must return that result as soon as possible, without fixing any
    further references.

    .. topics::

        :ref:`topic-scanning`.

    .. note::

        In the common case where the scan method does not need to do
        anything between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`,
        you can use the convenience macro :c:func:`MPS_FIX12`.


.. c:function:: MPS_FIX_CALL(ss, call)

    Call a function from within a :term:`scan method`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, passing
    scan state correctly.

    *ss* is the :term:`scan state` that was passed to the scan method.

    *call* is an expression containing a call to a scan method.

    Returns the result of evaluating the expression *call*.

    Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, the
    scan state is in a special state, and must not be passed to a
    function. If you really need to do so, for example because you
    have an embedded structure shared between two scan methods, you
    must wrap the call with :c:func:`MPS_FIX_CALL` to ensure that the
    scan state is passed correctly.

    In this example, the scan method ``obj_scan`` fixes the object's
    ``left`` and ``right`` references, but delegates the scanning of
    references inside the object's ``data`` member to the function
    ``scan_data``. In order to ensure that the scan state is passed
    correctly to ``scan_data``, the call must be wrapped in
    :c:func:`MPS_FIX_CALL`. ::

        mps_res_t obj_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
        {
            Object *obj;
            mps_res_t res;
            MPS_SCAN_BEGIN(ss) {
                for (obj = base; obj < limit; obj++) {
                    if (MPS_FIX12(ss, &obj->left) != MPS_RES_OK)
                        return res;
                    MPS_FIX_CALL(ss, res = scan_data(ss, &obj->data));
                    if (res != MPS_RES_OK)
                        return res;
                    if (MPS_FIX12(ss, &obj->right) != MPS_RES_OK)
                        return res;
                }
            } MPS_SCAN_END(ss);
            return MPS_RES_OK;
        }

    .. topics::

        :ref:`topic-scanning`.


.. c:type:: mps_fmt_A_s

    The type of the structure used to create an :term:`object format`
    of variant A. ::

        typedef struct mps_fmt_A_s {
            mps_align_t     align;
            mps_fmt_scan_t  scan;
            mps_fmt_skip_t  skip;
            mps_fmt_copy_t  copy;
            mps_fmt_fwd_t   fwd;
            mps_fmt_isfwd_t isfwd;
            mps_fmt_pad_t   pad;
        } mps_fmt_A_s;

    Broadly speaking, object formats of variant A are suitable for use
    in :term:`copying <copying garbage collection>` or :term:`moving
    <moving garbage collector>` :term:`pools <pool>`.

    *align* is an integer value specifying the alignment of objects
    allocated with this format. It should be large enough to satisfy
    the alignment requirements of any field in the objects, and it
    must not be larger than the arena alignment.

    *scan* is a :term:`scan method` that identifies references
    within objects belonging to this format. See
    :c:type:`mps_fmt_scan_t`.

    *skip* is a :term:`skip method` that skips over objects
    belonging to this format. See :c:type:`mps_fmt_skip_t`.

    *copy* is not used. (In older versions of the MPS it was a
    :term:`copy method` that copied objects belonging to this
    format.)

    *fwd* is a :term:`forward method` that stores relocation
    information for an object belonging to this format that has moved.
    See :c:type:`mps_fmt_fwd_t`.

    *isfwd* is a :term:`is-forwarded method` that determines if an
    object belonging to this format has been moved. See
    :c:type:`mps_fmt_isfwd_t`.

    *pad* is a :term:`padding method` that creates :term:`padding
    objects <padding object>` belonging to this format. See
    :c:type:`mps_fmt_pad_t`.

    .. topics::

        :ref:`topic-format`.


.. c:type:: mps_fmt_auto_header_s

    The type of the structure used to create an :term:`object format`
    of variant auto_header. ::

        typedef struct mps_fmt_auto_header_s {
            mps_align_t     align;
            mps_fmt_scan_t  scan;
            mps_fmt_skip_t  skip;
            mps_fmt_fwd_t   fwd;
            mps_fmt_isfwd_t isfwd;
            mps_fmt_pad_t   pad;
            size_t          mps_headerSize;
        } mps_fmt_auto_header_s;

    Variant auto_header is the same as variant A except for the
    removal of the unused ``copy`` method, and the addition of the
    *mps_headerSize* method. See :c:type:`mps_fmt_A_s`.

    Broadly speaking, the object formats of this variant are suitable
    for use in :term:`automatic memory management` for objects with
    :term:`headers <in-band header>` (hence the name). More precisely,
    this variant is intended for formats where the :term:`client
    program's <client program>` pointers point some distance into the
    memory :term:`block` containing the object. This typically happens
    when the objects have a common header used for memory management
    or class system purposes, but this situation also arises when the
    low bits of a pointer are used for a tag. The MPS does not care
    what the reason is, only about the offset of the pointer in
    relation to the memory block.

    *mps_headerSize* is the size of the header, that is, the offset of
    a client pointer from the base of the memory block.

    .. topics::

        :ref:`topic-format`.

    .. note::

        For technical reasons, formatted objects must be longer than
        the header. In other words, objects consisting of only a
        header are not supported. However, if the header size is
        larger than or equal to :term:`alignment`, the :term:`padding
        method` must still be able to create :term:`padding objects
        <padding object>` down to the alignment size.

    .. note::

        The auto_header format is only supported by :ref:`pool-amc`
        and :ref:`pool-amcz`.


.. c:type:: mps_fmt_B_s

    The type of the structure used to create an :term:`object format`
    of variant B. ::

        typedef struct mps_fmt_B_s {
            mps_align_t     align;
            mps_fmt_scan_t  scan;
            mps_fmt_skip_t  skip;
            mps_fmt_copy_t  copy;
            mps_fmt_fwd_t   fwd;
            mps_fmt_isfwd_t isfwd;
            mps_fmt_pad_t   pad;
            mps_fmt_class_t mps_class;
        } mps_fmt_B_s;

    Variant B is the same as variant A except for the addition of the
    *mps_class* method. See :c:type:`mps_fmt_A_s`.

    Broadly speaking, object formats of variant B are suitable for use
    in :term:`copying <copying garbage collection>` or :term:`moving
    <moving garbage collector>` :term:`pools <pool>` (just like
    variant A); the addition of a :term:`class method` allows more
    information to be passed to various support tools (such as
    graphical browsers). See :c:type:`mps_fmt_class_t`.

    .. topics::

        :ref:`topic-format`.


.. c:type:: mps_addr_t (*mps_fmt_class_t)(mps_addr_t addr)

    The type of the :term:`class method` of an :term:`object format`.

    *addr* is the address of the object whose class is of interest.

    Returns an address that is related to the class or type of the
    object, for passing on to support tools (such as graphical
    browsers), or a null pointer if this is not possible.

    It is recommended that a null pointer be returned for
    :term:`padding objects <padding object>` and :term:`forwarded
    objects <forwarded object>`.

    The exact meaning of the return value is up to the :term:`client
    program`, but it would typically bear some relation to a class or
    type in the client program. The client may have objects that
    represent classes or types. These may be associated with strings
    via :c:func:`mps_telemetry_intern` and
    :c:func:`mps_telemetry_label`.

    .. topics::

        :ref:`topic-format`.


.. c:function:: mps_res_t mps_fmt_create_A(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_A_s *fmt_A)

    Create an :term:`object format` of variant A.

    *fmt_o* points to a location that will hold the address of the new
    object format.

    *arena* is the arena in which to create the format.

    *fmt_A* points to a description of an object format of variant A.

    Returns :c:macro:`MPS_RES_OK` if successful. The MPS may exhaust
    some resource in the course of :c:func:`mps_fmt_create_A` and will
    return an appropriate :term:`result code` if so.

    .. topics::

        :ref:`topic-format`.


.. c:function:: mps_res_t mps_fmt_create_B(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_B_s *fmt_B)

    Create an :term:`object format` of variant B.

    *fmt_o* points to a location that will hold the address of the new
    object format.

    *arena* is the arena in which to create the format.

    *fmt_B* points to a description of an object format of variant B.

    Returns :c:macro:`MPS_RES_OK` if successful. The MPS may exhaust
    some resource in the course of :c:func:`mps_fmt_create_B` and will
    return an appropriate :term:`result code` if so.

    .. topics::

        :ref:`topic-format`.


.. c:function:: mps_res_t mps_fmt_create_auto_header(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_auto_header_s *fmt_ah)

    Create an :term:`object format` of variant auto_header.

    *fmt_o* points to a location that will hold the address of the new
    object format.

    *arena* is the arena in which to create the format.

    *fmt_ah* points to a description of an object format of variant
    auto_header.

    Returns :c:macro:`MPS_RES_OK` if successful. The MPS may exhaust
    some resource in the course of
    :c:func:`mps_fmt_create_auto_header` and will return an
    appropriate :term:`result code` if so.

    .. topics::

        :ref:`topic-format`.


.. c:type:: void (*mps_fmt_fwd_t)(mps_addr_t old, mps_addr_t new)

    The type of the :term:`forward method` of an :term:`object format`.

    *old* is the address of an object.

    *new* is the address to where the object has been moved.

    The MPS calls the forward method for an object format when it has
    relocated an object belonging to that format. The forward method
    must replace the object at *old* with a :term:`forwarding marker`
    that points to the address 'new'. The forwarding marker must meet
    the following requirements:

    1. It must be possible for the MPS to call other methods in the
       object format (the :term:`scan method`, the :term:`skip method`
       and so on) with the address of a forwarding marker as the
       argument.

    2. The forwarding marker must not be bigger than the original
       object.

    3. It must be possible for the :term:`is-forwarded method` of the
       object format to distinguish the forwarding marker from
       ordinary objects, and the is-forwarded method method must
       return the address *new*. See :c:type:`mps_fmt_isfwd_t`.

    .. topics::

        :ref:`topic-format`.

    .. note::

        This method is never invoked by the :term:`garbage collector`
        on an object in a :term:`non-moving <non-moving garbage
        collector>` :term:`pool`.


.. c:type:: mps_addr_t (*mps_fmt_isfwd_t)(mps_addr_t addr)

    The type of the :term:`is-forwarded method` of an :term:`object
    format`.

    *addr* is the address of a candidate object.

    If the *addr* is the address of a :term:`forwarded object`, return
    the address where the object was moved to. This must be the value
    of the *new* argument supplied to the :term:`forward method` when
    the object was moved. If not, return a null pointer.

    .. topics::

        :ref:`topic-format`.

    .. note::

        This method is never invoked by the :term:`garbage collector`
        on an object in a :term:`non-moving <non-moving garbage
        collector>` :term:`pool`.


.. c:type:: void (*mps_fmt_pad_t)(mps_addr_t addr, size_t size)

    The type of the :term:`padding method` of an :term:`object
    format`.

    *addr* is the address at which to create a :term:`padding object`.

    *size* is the :term:`size` of the padding object to be created.

    The MPS calls a padding method when it wants to create a padding
    object. Typically the MPS creates padding objects to fill in
    otherwise unused gaps in memory; they allow the MPS to pack
    objects into fixed-size units (such as operating system
    :term:`pages <page>`).

    The padding method must create a padding object of the specified
    size at the specified address. The size can be any aligned (to the
    format alignment) size. A padding object must be acceptable to
    other methods in the format (the :term:`scan method`, the
    :term:`skip method`, and so on).

    .. topics::

        :ref:`topic-format`.


.. c:type:: mps_res_t (*mps_fmt_scan_t)(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)

    The type of the :term:`scan method` of an :term:`object format`.

    *ss* is the :term:`scan state`. It must be passed to
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END` to delimit a
    sequence of fix operations, and to the functions
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` when fixing a
    :term:`reference`.

    *base* points to the first :term:`formatted object` in the block
    of memory to be scanned.

    *limit* points to the location just beyond the end of the block to
    be scanned. Note that there might not be any object at this
    location.

    Returns a :term:`result code`. If a fix function returns a value
    other than :c:macro:`MPS_RES_OK`, the scan method must return that
    value, and may return without fixing any further references.
    Generally, itis better if it returns as soon as possible. If the
    scanning is completed successfully, the function should return
    :c:macro:`MPS_RES_OK`.

    The scan method for an object format is called when the MPS needs
    to scan objects in a block area of memory containing objects
    belonging to that format. The scan method is called with a scan
    state and the base and limit of the block of objects to scan. It
    must then indicate references within the objects by calling
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`.

    .. topics::

        :ref:`topic-format`, :ref:`topic-scanning`.


.. c:type:: mps_addr_t (*mps_fmt_skip_t)(mps_addr_t addr)

    The type of the :term:`skip method` of an :term:`object format`.

    *addr* is the address of the object to be skipped.

    Returns the address of the "next object". In an object format
    without headers (for example, a format of variant A), this is the
    address just past the end of this object. In an object format with
    headers (for example, a format of variant auto_header), it's the
    address just past where the header of next object would be, if
    there were one. It is always the case that the difference between
    *addr* and the return value is the size of the block containing
    the object.

    A skip method is not allowed to fail.

    .. topics::

        :ref:`topic-format`, :ref:`topic-scanning`.


.. c:type:: mps_fmt_t

    The type of an :term:`object format`.

    .. topics::

        :ref:`topic-format`.


.. c:type:: void (*mps_formatted_objects_stepper_t)(mps_addr_t addr, mps_fmt_t fmt, mps_pool_t pool, void *p, size_t s)

    The type of a :term:`formatted objects <formatted object>`
    :term:`stepper function`.
    
    A function of this type can be passed to
    :c:func:`mps_arena_formatted_objects_walk`, in which case it will
    be called for each formatted object in an :term:`arena`. It
    receives five arguments:
    
    *addr* is the address of the object.

    *fmt* is the :term:`object format` for that object.

    *pool* is the :term:`pool` to which the object belongs.

    *p* and *s* are the corresponding values that were passed to
    :c:func:`mps_arena_formatted_objects_walk`.

    .. topics::

        :ref:`topic-arena`, :ref:`topic-format`.


.. c:function:: void mps_free(mps_pool_t pool, mps_addr_t addr, size_t size)

    Free a :term:`block` of memory to a :term:`pool`.

    *pool* is the pool the block belongs to.

    *addr* is the address of the block to be freed.

    *size* is the :term:`size` of the block to be freed.

    The freed block of memory becomes available for allocation by the
    pool, or the pool might decide to make it available to other
    pools, or it may be returned to the operating system.

    .. topics::

        :ref:`topic-allocation`.

    .. note::

        :c:func:`mps_free` takes a *size* because it is most efficient
        to do so. In most programs, the type of an object is known at
        the point in the code that frees it, hence the size is
        trivially available. In such programs, storing the size on the
        MPS side would cost time and memory, and make it hard to get
        good virtual memory behaviour (as it is, the deallocation code
        doesn't have to touch the dead object at all).


.. c:function:: mps_clock_t mps_message_clock(mps_arena_t arena, mps_message_t message)

    Returns the time at which the MPS posted a :term:`message`.

    *arena* is the :term:`arena` which posted the message.

    *message* is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.

    If *message* belongs to one of the following supported message,
    return the time at which the MPS posted the message:

    * :c:type:`mps_message_type_gc`;
    * :c:type:`mps_message_type_gc_start`.

    For other message types, the value returned is always zero.

    Messages are asynchronous: they are posted by the MPS, wait on a
    queue, and are later collected by the :term:`client program`. Each
    message (of the supported message types) records the time that it
    was posted, and this is what :c:func:`mps_message_clock` returns.

    The time returned is the :c:func:`mps_clock_t` value returned by
    the library function :c:func:`mps_clock` at the time the message
    was posted. You can subtract one clock value from another to get
    the time interval between the posting of two messages.

    .. topics::

        :ref:`topic-message`.


.. c:function:: void mps_message_discard(mps_arena_t arena, mps_message_t message)

    Indicate to the MPS that the :term:`client program` has no further
    use for a :term:`message` and the MPS can now reclaim any storage
    associated with the message.

    *arena* is the :term:`arena` which posted the message.

    *message* is the message. After this call, *message* is invalid
    and should not be passed as an argument to any message functions.

    Messages are essentially :term:`manually <manual memory
    management>` managed. This function allows the MPS to reclaim
    storage associated with messages. If the client does not discard
    messages then the resources used may grow without bound.

    As well as consuming resources, messages may have other effects
    that require them to be tidied by calling this function. In
    particular finalization messages refer to a :term:`finalized
    block`, and prevent the object from being reclaimed (subject to
    the usual :term:`garbage collection` liveness analysis). A
    finalized block cannot be reclaimed until all its finalization
    messages have been discarded. See
    :c:func:`mps_message_type_finalization`.

    .. topics::

        :ref:`topic-finalization`, :ref:`topic-message`.


.. c:function:: void mps_message_finalization_ref(mps_addr_t *ref_o, mps_arena_t arena, mps_message_t message)

    Returns the finalization reference for a finalization message.

    *ref_o* points to a location that will hold the finalization
    reference.

    *arena* is the :term:`arena` which posted the message.

    *message* is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded. It must be a finalization message: see
    :c:func:`mps_message_type_finalization`.

    The reference returned by this method is a reference to the block
    that was originally registered for :term:`finalization` by a call
    to :c:func:`mps_finalize`.

    .. topics::

        :ref:`topic-finalization`, :ref:`topic-message`.

    .. note::

        The reference returned is subject to the normal constraints,
        such as might be imposed by a :term:`moving <moving garbage
        collector>` collection, if appropriate. For this reason, it is
        stored into the location pointed to by *ref_o* in order to
        enable the :term:`client program` to place it directly into
        scanned memory, without imposing the restriction that the C
        stack be a :term:`root`.

    .. note::

        The message itself is not affected by invoking this method.
        Until the client program calls :c:func:`mps_message_discard`
        to discard the message, it will refer to the object and
        prevent its reclamation.


.. c:function:: size_t mps_message_gc_condemned_size(mps_arena_t arena, mps_message_t message)

    Return the "condemned size" property of a :term:`message`.

    *arena* is the arena which posted the message.

    *message* is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    The "condemned size" property is the approximate :term:`size` of
    the :term:`condemned set` in the :term:`garbage collection` that
    generated the message.

    .. topics::

        :ref:`topic-collection`, :ref:`topic-message`.


.. c:function:: size_t mps_message_gc_live_size(mps_arena_t arena, mps_message_t message)

    Return the "live size" property of a :term:`message`.

    *arena* is the arena which posted the message.

    *message* is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    The "live size" property is the total size of the set of objects
    that survived the :term:`garbage collection` that generated the
    message.

    .. topics::

        :ref:`topic-collection`, :ref:`topic-message`.


.. c:function:: size_t mps_message_gc_not_condemned_size(mps_arena_t arena, mps_message_t message)

    Return the "not condemned size" property of a :term:`message`.

    *arena* is the arena which posted the message.

    *message* is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    The "not condemned size" property is the approximate size of the
    set of objects that were in collected :term:`pools <pool>`, but
    were not in the :term:`condemned set` in the :term:`garbage
    collection` that generated the message.

    .. topics::

        :ref:`topic-collection`, :ref:`topic-message`.


.. c:function:: const char *mps_message_gc_start_why(mps_arena_t arena, mps_message_t message)

    Return a string that describes why the :term:`garbage collection`
    that posted a :term:`message` started.

    *arena* is the arena which posted the message.

    *message* is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.  It must be a garbage collection message: see
    :c:func:`mps_message_type_gc`.

    Returns a pointer to a string that is describes (in English) why
    this collection started. The contents of the string must not be
    modified by the client. The string and the pointer are valid until
    the message is discarded with :c:func:`mps_message_discard`.

    .. topics::

        :ref:`topic-collection`, :ref:`topic-message`.


.. c:function:: mps_bool_t mps_message_get(mps_message_t *message_o, mps_arena_t arena, mps_message_type_t message_type)

    Get a :term:`message` of a specified type from the :term:`message
    queue` for an :term:`arena`.

    *message_o* points to a location that will hold the address of the
    message if the function succeeds.

    *arena* is the arena.

    *message_type* is the type of message to return.

    If there is at least one message of the specified type on the
    message queue of the specified arena, then this function removes
    one such message from the queue, stores a pointer to the message
    in the location pointed to by *message_o*, and returns true.
    Otherwise it returns false.

    .. topics::

        :ref:`topic-message`.


.. c:function:: mps_bool_t mps_message_poll(mps_arena_t arena)

    Determine whether there are currently any :term:`messages
    <message>` on a :term:`message queue` for an :term:`arena`.

    *arena* is the arena whose message queue will be polled.

    Returns true if there is at least one message on the message queue
    for *arena*, or false if the message queue is empty.

    .. topics::

        :ref:`topic-message`.

    .. note::

        If you are interested in a particular type of message, it is
        usually simpler to call :c:func:`mps_message_get`.


.. c:function:: mps_bool_t mps_message_queue_type(mps_message_type_t *message_type_o, mps_arena_t arena)

    Determine whether there are currently any :term:`messages
    <message>` on a :term:`message queue` for an :term:`arena`, and
    return the :term:`message type` of the first message, if any.

    *message_type_o* points to a location that will hold the message
    type of the first message on the queue, if any.

    *arena* is the arena whose message queue will be polled.

    If there is at least one message on the message queue of *arena*,
    then this function returns true, and also writes the message type
    of the first message on the queue into the location pointed to by
    *message_type_o*. If there are no messages on the message queue,
    it returns false.

    .. topics::

        :ref:`topic-message`.


.. c:type:: mps_message_t

    The type of a :term:`message`.

    Messages are :term:`manually <manual memory management>` managed.
    They are created at the instigation of the MPS (but see
    :c:func:`mps_message_type_enable`), and are deleted by the
    :term:`client program` by calling :c:func:`mps_message_discard`.

    An :term:`arena` has a :term:`message queue` from which messages
    can be obtained by calling :c:func:`mps_message_get`.

    An :c:func:`mps_message_t` is a :term:`reference` into MPS managed
    memory, and can safely be :term:`fixed <fix>`.

    .. topics::

        :ref:`topic-message`.


.. c:function:: mps_message_type_t mps_message_type(mps_arena_t arena, mps_message_t message)

    Return the :term:`message type` of a :term:`message`.

    *arena* is the arena that posted the message.

    *message* is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded.

    .. topics::

        :ref:`topic-message`.


.. c:function:: void mps_message_type_disable(mps_arena_t arena, mps_message_type_t message_type)

    Restore an :term:`arena` to the default state whereby
    :term:`messages <message>` of the specified :term:`message type`
    are not posted, reversing the effect of an earlier call to
    :c:func:`mps_message_type_enable`.

    *arena* is an arena.

    *message_type* is the message type to be disabled.

    Any existing messages of the specified type are flushed from the
    :term:`message queue` of *arena*.

    .. topics::

        :ref:`topic-message`.

    .. note::

        It is permitted to call this function when *message_type* is
        already disabled, in which case it has no effect.


.. c:function:: void mps_message_type_enable(mps_arena_t arena, mps_message_type_t message_type)

    Enable an :term:`arena` to post :term:`messages <message>` of a
    specified :term:`message type`.

    *arena* is an arena.

    *message_type* is the message type to be disabled.

    This function tells the MPS that *arena* may post messages of
    *message_type* to its :term:`message queue`. By default, the MPS
    does not generate any messages of any type.

    A :term:`client program` that enables messages for a message type
    must access messages using :c:func:`mps_message_get` and discard
    them using :c:func:`mps_message_discard`, or the message queue may
    consume unbounded resources.

    The client program may disable the posting of messages by calling
    :c:func:`mps_message_type_disable`.

    .. topics::

        :ref:`topic-message`.

    .. note::

        It is permitted to call this function when *message_type* is
        already enabled, in which case it has no effect.


.. c:function:: mps_message_type_t mps_message_type_finalization(void)

    Return the :term:`message type` of finalization messages.

    Finalization messages are used by the MPS to implement
    :term:`finalization`. When the MPS detects that a block that has
    been registered for finalization (by calling
    :c:func:`mps_finalize`) is finalizable, it finalizes it by posting
    a :term:`message` of this type.

    Note that there might be delays between the block becoming
    finalizable, the MPS detecting that, and the message being
    posted.

    In addition to the usual methods applicable to messages,
    finalization messages support the
    :c:func:`mps_message_finalization_ref` method which returns a
    reference to the block that was registered for finalization.

    .. topics::

        :ref:`topic-finalization`, :ref:`topic-message`.


.. c:function:: mps_message_type_t mps_message_type_gc(void)

    Return the :term:`message type` of garbage collection statistic
    messages.

    Garbage collection statistic messages are used by the MPS to give
    the :term:`client program` information about a :term:`garbage
    collection` that has taken place. Such information may be useful in
    analysing the client program's memory usage over time.

    The access methods specific to a message of this type are:

    * :c:func:`mps_message_gc_live_size` returns the total size of the
      :term:`condemned set` that survived the garbage collection that
      generated the message;

    * :c:func:`mps_message_gc_condemned_size` returns the approximate
      size of :term:`condemned set` in the garbage collection that
      generated the message;

    * :c:func:`mps_message_gc_not_condemned_size` returns the
      approximate size of the set of objects that were in collected
      :term:`pools <pool>`, but were not condemned in the garbage
      collection that generated the message.

    .. topics::

        :ref:`topic-collection`, :ref:`topic-message`.


.. c:function:: mps_message_type_t mps_message_type_gc_start(void)

    Return the :term:`message type` of garbage collection start
    messages.

    Garbage collection start messages contain information about why
    the :term:`garbage collection` started.

    The access method specific to a :term:`message` of this message
    type is:

    * :c:func:`mps_message_gc_start_why` returns a string that
      describes why the garbage collection started.

    .. topics::

        :ref:`topic-collection`, :ref:`topic-message`.


.. c:type:: mps_message_type_t

    The type of :term:`message types <message type>`.

    .. topics::

        :ref:`topic-message`.


.. c:type:: mps_pool_t

    The type of :term:`pools <pool>`.

    A pool is responsible for requesting memory from the :term:`arena`
    and making it available to the :term:`client program` via
    :c:func:`mps_alloc` or via an :term:`allocation point`.

    .. topics::

        :ref:`pool`.


.. c:function:: void mps_pool_check_fenceposts(mps_pool_t pool)

    Check all the :term:`fenceposts <fencepost>` in a :term:`pool`.

    *pool* is the pool whose fenceposts are to be checked.

    If a corrupted fencepost is found, the MPS will :term:`assert
    <assertion>`. It is only useful to call this on a :term:`debugging
    pool` that has fenceposts turned on. It does nothing on
    non-debugging pools.

    .. topics::

        :ref:`topic-debugging`.


.. c:type:: mps_pool_debug_option_s

    The type of the structure used to pass options to
    :c:func:`mps_pool_create` for debugging :term:`pool classes <pool
    class>`. ::

        typedef struct mps_pool_debug_option_s {
            void  *fence_template;
            size_t fence_size;
            void  *free_template;
            size_t free_size;
        } mps_pool_debug_option_s;

    *fence_template* points to a template for :term:`fenceposts
    <fencepost>`.

    *fence_size* is the :term:`size` of *fence_template* in bytes, or
    zero if the debugging pool should not use fenceposts.

    *free_template* points to a template for splatting free space.

    *free_size* is the :term:`size` of *free_template* in bytes, or
    zero if the debugging pool should not splat free space.

    Both *fence_size* and *free_size* must be a multiple of the
    :term:`alignment` of the :term:`pool`, and also a multiple of the
    alignment of the pool's :term:`object format` if it has one.

    The debugging pool will copy the *fence_size* bytes pointed to by
    *fence_template* in a repeating pattern onto each fencepost during
    allocation, and it will copy the bytes pointed to by
    *free_template* in a repeating pattern over free space after the
    space is reclaimed.

    The MPS may not always use the whole of a template: it may use
    pieces smaller than the given size, for example to pad out part of
    a block that was left unused because of alignment requirements.

    Fencepost and free space templates allow the :term:`client
    program` to specify patterns:

    * that mimic illegal data values;
  
    * that cause bus errors if wrongly interpreted as pointers;

    * that cause assertions to fire if wrongly interpreted as data values;

    * that contain an instruction sequence that wold cause the program
      to signal an error or stop if wrongly interpreted as executable
      code.

    .. topics::

        :ref:`topic-debugging`.


.. c:function:: mps_rank_t mps_rank_ambig(void)

    Return the :term:`rank` of :term:`ambiguous roots <ambiguous
    root>`.

    .. topics::

        :ref:`topic-root`.


.. c:function:: mps_rank_t mps_rank_exact(void)

    Return the :term:`rank` of :term:`exact roots <exact root>`.

    .. topics::

        :ref:`topic-root`.


.. c:type:: mps_rank_t

    The type of :term:`ranks <rank>`. It is an alias (via the C
    ``typedef`` mechanism) for ``unsigned int``, provided for
    convenience and clarity.

    .. topics::

        :ref:`topic-root`.


.. c:function:: mps_rank_t mps_rank_weak(void)

    Return the :term:`rank` of :term:`weak roots <weak root>`.

    .. topics::

        :ref:`topic-root`.


.. c:type:: mps_res_t (*mps_reg_scan_t)(mps_ss_t ss, mps_thr_t thr, void *p, size_t s)

    The type of a root scanning function for roots created with
    :c:func:`mps_root_create_reg`.

    *ss* is the :term:`scan state`. It must be passed to
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END` to delimit a
    sequence of fix operations, and to the functions
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` when fixing a
    :term:`reference`.

    *thr* is the :term:`thread`.

    *p* and *s* are the corresponding values that were passed to
    :c:func:`mps_root_create_reg`.

    Returns a :term:`result code`. If a fix function returns a value
    other than :c:macro:`MPS_RES_OK`, the scan method must return that
    value, and may return without fixing any further references.
    Generally, itis better if it returns as soon as possible. If the
    scanning is completed successfully, the function should return
    :c:macro:`MPS_RES_OK`.

    A root scan method is called whenever the MPS needs to scan the
    root. It must then indicate references within the root by calling
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`.

    .. topics::

        :ref:`topic-root`, :ref:`topic-scanning`.

    .. note::

        :term:`Client programs <client program>` are not expected to
        write scanning functions of this type. The built-in MPS
        function :c:func:`mps_stack_scan_ambig` should be used.


.. c:type:: mps_res_t

    The type of :term:`result codes <result code>`. It is an alias
    (via the C ``typedef`` mechanism) for ``int``, provided for
    convenience and clarity.

    A result code indicates the success or failure of an operation,
    along with the reason for failure. As with error numbers in Unix,
    the meaning of a result code depends on the call that returned it.
    Refer to the documentation of the function for the exact meaning
    of each result code.

    The result codes are:

    * :c:macro:`MPS_RES_OK`: operation succeeded.

    * :c:macro:`MPS_RES_FAIL`: operation failed.

    * :c:macro:`MPS_RES_IO`: an input/output error occurred.

    * :c:macro:`MPS_RES_LIMIT`: an internal limitation was exceeded.

    * :c:macro:`MPS_RES_MEMORY`: needed memory could not be obtained.

    * :c:macro:`MPS_RES_RESOURCE`: a needed resource could not be
      obtained.

    * :c:macro:`MPS_RES_UNIMPL`: operation is not implemented.

    * :c:macro:`MPS_RES_COMMIT_LIMIT`: the arena's :term:`commit
      limit` would be exceeded.

    * :c:macro:`MPS_RES_PARAM`: an invalid parameter was passed.

    .. topics::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_COMMIT_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested without exceeding the :term:`commit limit`.

    You need to deallocate something to make more space, or increase
    the commit limit by calling :c:func:`mps_arena_commit_limit_set`.

    .. topics::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_FAIL

    A :term:`result code` indicating that something went wrong that
    does not fall under the description of any other result code. The
    exact meaning depends on the function that returned this result
    code.

    .. topics::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_IO

    A :term:`result code` indicating that an input/output error
    occurred. The exact meaning depends on the function that returned
    this result code.

    .. topics::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested because of an internal limitation of the
    MPS. The exact meaning depends on the function that returned this
    result code.

    .. topics::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_MEMORY

    A :term:`result code` indicating that an operation could not be
    completed because there wasn't enough memory available.

    You need to deallocate something or allow the :term:`garbage
    collector` to reclaim something to free enough memory, or expand
    the :term:`arena` (if you're using an arena for which that does
    not happen automatically).

    .. topics::

        :ref:`topic-error`.

    .. note::

        Failing to acquire enough memory because the :term:`commit
        limit` would have been exceeded is indicated by returning
        :c:macro:`MPS_RES_COMMIT_LIMIT`, not ``MPS_RES_MEMORY``.

    .. note::

        Running out of :term:`address space` (as might happen in
        :term:`virtual memory` systems) is indicated by returning
        :c:macro:`MPS_RES_RESOURCE`, not ``MPS_RES_MEMORY``.


.. c:macro:: MPS_RES_OK

    A :term:`result code` indicating that an operation succeeded.

    If a function takes an :term:`out parameter` or an :term:`in/out
    parameter`, this parameter will only be updated if
    :c:macro:`MPS_RES_OK` is returned. If any other result code is
    returned, the parameter will be left untouched by the function.

    :c:macro:`MPS_RES_OK` is zero.

    .. topics::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_PARAM

    A :term:`result code` indicating that an operation could not be
    completed as requested because an invalid parameter was passed to
    the operation. The exact meaning depends on the function that
    returned this result code.

    .. topics::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_RESOURCE

    A :term:`result code` indicating that an operation could not be
    completed as requested because the MPS could not obtain a needed
    resource. The resource in question depends on the operation.

    Two special cases have their own result codes: when the MPS runs
    out of committed memory, it returns :c:macro:`MPS_RES_MEMORY`, and
    when it cannot proceed without exceeding the :term:`commit limit`,
    it returns :c:macro:`MPS_RES_COMMIT_LIMIT`.

    This result code can be returned when the MPS runs out of
    :term:`virtual memory`. If this happens, you need to reclaim
    memory within your process (as for the result code
    :c:macro:`MPS_RES_MEMORY`), or terminate other processes running
    on the same machine.

    .. topics::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_UNIMPL

    A :term:`result code` indicating that an operation, or some vital
    part of it, is not implemented.

    This might be returned by functions that are no longer supported,
    or by operations that are included for future expansion, but not
    yet supported.

    .. topics::

        :ref:`topic-error`.


.. c:macro:: MPS_RM_CONST

    The :term:`root mode` for :term:`constant roots <constant root>`.
    This tells the MPS that the :term:`client program` will not change
    the :term:`root` after it is registered: that is, scanning the
    root will produce the same set of :term:`references <reference>`
    every time. Furthermore, for roots registered by
    :c:func:`mps_root_create_fmt` and :c:func:`mps_root_create_table`,
    the client program will not write to the root at all.

    .. topics::

        :ref:`topic-root`.

    .. note::

        Currently ignored by the MPS.


.. c:macro:: MPS_RM_PROT

    The :term:`root mode` for :term:`protectable roots <protectable
    root>`. This tells the MPS that it may place a :term:`write
    barrier` on any :term:`page` which any part of the :term:`root`
    covers. No :term:`format method` or :term:`scan method` (except
    for the one for this root) may write data in this root. They may
    read it.

    .. topics::

        :ref:`topic-root`.

    .. note::

        You must not specify ``MPS_RM_PROT`` on a root allocated by
        the MPS.

    .. note::

        No page may contain parts of two or more protectable roots.
        You mustn't specify ``MPS_RM_PROT`` if the :term:`client
        program` or anything other than (this instance of) the MPS is
        going to protect or unprotect the relevant pages.


.. c:function:: mps_res_t mps_root_create(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_root_scan_t root_scan, void *p, size_t s)

    Register a :term:`root` that consists of the :term:`references
    <reference>` fixed by a scanning function.

    *root_o* points to a location that will hold the address of the
    new root description.

    *arena* is the arena.

    *rank* is the :term:`rank` of references in the root.

    *rm* is the :term:`root mode`.

    *root_scan* is the root scanning function. See
    :c:type:`mps_root_scan_t`.

    *p* and *s* are arguments that will be passed to *root_scan* each
    time it is called. This is intended to make it easy to pass, for
    example, an array and its size as parameters.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    .. topics::

        :ref:`topic-root`.


.. c:function:: mps_res_t mps_root_create_fmt(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_fmt_scan_t fmt_scan, mps_addr_t base, mps_addr_t limit)

    Register a :term:`root` that consists of the :term:`references
    <reference>` fixed by a scanning function in a block of
    :term:`formatted objects <formatted object>`.

    *root_o* points to a location that will hold the address of the
    new root description.

    *arena* is the arena.

    *rank* is the :term:`rank` of references in the root.

    *rm* is the :term:`root mode`.

    *fmt_scan* is a scanning function. See :c:type:`mps_fmt_scan_t`.

    *base* is the address of the base of the block of formatted
    objects.

    *limit* is the address just beyond the end of the block of
    formatted objects.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    .. topics::

        :ref:`topic-root`.

    .. note::

        This is like :c:func:`mps_root_create_table`, except you get
        to supply your own scanning function, and like
        :c:func:`mps_root_create`, except the scanning function takes
        a different argument list, and the MPS knows the location of
        the root.


.. c:function:: mps_res_t mps_root_create_reg(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_thr_t thr, mps_reg_scan_t reg_scan, void *p, size_t s)

    Register a :term:`root` that consists of the :term:`references
    <reference>` fixed in a :term:`thread's <thread>` stack by a
    scanning function.

    *root_o* points to a location that will hold the address of the
    new root description.

    *arena* is the arena.

    *rank* is the :term:`rank` of references in the root.

    *rm* is the :term:`root mode`.

    *thr* is the thread.

    *reg_scan* is a scanning function. See :c:type:`mps_reg_scan_t`.

    *p* and *s* are arguments that will be passed to *reg_scan* each
    time it is called. This is intended to make it easy to pass, for
    example, an array and its size as parameters.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    .. topics::

        :ref:`topic-root`.

    .. note::

        :term:`Client programs <client program>` are not expected to
        write their own scanning functions to pass to this function.
        The built-in MPS function :c:func:`mps_stack_scan_ambig`
        should be used.


.. c:function:: mps_res_t mps_root_create_table(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t count)

    Register a :term:`root` that consists of a vector of
    :term:`references <reference>`.

    *root_o* points to a location that will hold the address of the
    new root description.

    *arena* is the arena.

    *rank* is the :term:`rank` of references in the root.

    *rm* is the :term:`root mode`.

    *base* points to a vector of references.

    *count* is the number of references in the vector.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    .. topics::

        :ref:`topic-root`.


.. c:function:: mps_res_t mps_root_create_table_masked(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t count, mps_word_t mask)

    Register a :term:`root` that consists of a vector of :term:`tagged
    references <tagged reference>`.

    *root_o* points to a location that will hold the address of the
    new root description.

    *arena* is the arena.

    *rank* is the :term:`rank` of references in the root.

    *rm* is the :term:`root mode`.

    *base* points to a vector of tagged references.

    *count* is the number of tagged references in the vector.

    *mask* is a :term:`bitmask` whose set bits specify the location of
    the :term:`tag`. References are assumed to have a tag of zero: any
    value in the vector with a non-zero tag is ignored.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    .. topics::

        :ref:`topic-root`.


.. c:type:: typedef mps_res_t (*mps_root_scan_t)(mps_ss_t ss, void *p, size_t s)

    The type of root scanning functions for :c:func:`mps_root_create`.

    *ss* is the :term:`scan state`. It must be passed to
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END` to delimit a
    sequence of fix operations, and to the functions
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` when fixing a
    :term:`reference`.

    *p* and *s* are the corresponding values that were passed to
    :c:func:`mps_root_create`.

    Returns a :term:`result code`. If a fix function returns a value
    other than :c:macro:`MPS_RES_OK`, the scan method must return that
    value, and may return without fixing any further references.
    Generally, itis better if it returns as soon as possible. If the
    scanning is completed successfully, the function should return
    :c:macro:`MPS_RES_OK`.

    .. topics::

        :ref:`topic-root`.


.. c:type:: void (*mps_roots_stepper_t)(mps_addr_t *ref, mps_root_t root, void *p, size_t s)

    The type of a :term:`root` :term:`stepper function`.

    A function of this type can be passed to
    :c:func:`mps_arena_roots_walk`, in which case it will be called
    for each reference into the :term:`arena` from a root registered
    with the arena. It receives four arguments:

    *ref* points to a reference in a root. The reference points to
    something in the arena. If the root is :term:`exact <exact
    reference>` then the reference points to the start of an allocated
    block, but if the root is :term:`ambiguous <ambiguous reference>`
    it might point to somewhere in the middle of an allocated block.

    *root* is the description of the root which contains *ref*.

    *p* and *s* are the corresponding values that were passed to
    :c:func:`mps_arena_roots_walk`.

    .. topics::

        :ref:`topic-root`.


.. c:type:: mps_root_t

    The type of :term:`root` descriptions.

    The :term:`arena` uses root descriptions to find :term:`references
    <reference>` within the :term:`client program's <client program>`
    roots.

    .. topics::

        :ref:`topic-root`.


.. c:function:: mps_res_t mps_sac_alloc(mps_addr_t *p_o, mps_sac_t sac, size_t size, mps_bool_t has_reservoir_permit)

    Allocate a :term:`block` using a :term:`segregated allocation
    cache`. If no suitable block exists in the cache, ask for more
    memory from the associated :term:`pool`.

    *p_o* points to a location that will hold the address of the
    allocated block.

    *sac* is the segregated allocation cache.

    *size* is the :term:`size` of the block to allocate. It does not
    have to be one of the :term:`sizes classes <size class>` of the
    cache; nor does it have to be aligned.

    If *has_reservoir_permit* is true, the pool has permission to get
    more memory from the :term:`reservoir` to satisfy this request.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the
    address of the allocated block is ``*p_o``. The allocated block
    can be larger than requested. Blocks not matching any size class
    are allocated from the next largest class, and blocks larger than
    the largest size class are simply allocated at the requested size
    (rounded up to alignment, as usual).

    Returns :c:macro:`MPS_RES_MEMORY` if there wasn't enough memory,
    :c:macro:`MPS_RES_COMMIT_LIMIT` if the :term:`commit limit` was
    exceeded, or :c:macro:`MPS_RES_RESOURCE` if it ran out of
    :term:`virtual memory`.

    .. topics::

        :ref:`topic-cache`.

    .. note::

        There's also a macro :c:func:`MPS_SAC_ALLOC_FAST` that does
        the same thing. The macro is faster, but generates more code
        and does less checking.

    .. note::

        The :term:`client program` is responsible for synchronizing
        the access to the cache, but if the cache decides to access
        the pool, the MPS will properly synchronize with any other
        :term:`threads <thread>` that might be accessing the same
        pool.

    .. note::

        Blocks allocated through a segregated allocation cache should
        only be freed through a segregated allocation cache with the
        same :term:`class structure`. Using :c:func:`mps_free` on them
        can cause :term:`memory leaks <memory leak>`, because the size
        of the block might be larger than you think. Naturally, the
        cache must also be attached to the same pool.


.. c:function:: MPS_SAC_ALLOC_FAST(mps_res_t res_v, mps_addr_t *p_v, mps_sac_t sac, size_t size, mps_bool_t has_reservoir_permit)

    A macro alternative to :c:func:`mps_sac_alloc` that is faster than
    the function but does less checking. The macro takes an additional
    first argument, *res_v*, which must be an lvalue that will store
    the :term:`result code`, and it doesn't evaluate
    *has_reservoir_permit* unless it decides to access the pool. The
    second argument *p_v* must also be an lvalue.

    .. topics::

        :ref:`topic-cache`.


.. c:macro:: MPS_SAC_CLASS_LIMIT

    The number of :term:`size classes <size class>` that
    :c:func:`mps_sac_create` is guaranteed to accept.

    More might be accepted: in fact, there might not be any limit in
    the implementation on the maximum number of size classes, but if
    you specify more than this many, you should be prepared to handle
    the :term:`result code` :c:macro:`MPS_RES_LIMIT`.

    .. topics::

        :ref:`topic-cache`.


.. c:function:: void mps_sac_free(mps_sac_t sac, mps_addr_t p, size_t size)

    Free a :term:`block` using a :term:`segregated allocation
    cache`. If the cache would become too full, some blocks may be
    returned to the associated :term:`pool`.

    *sac* is the segregated allocation cache.

    *p* points to the block to be freed. This block must have been
    allocated through a segregated allocation cache with the same
    :term:`class structure`, attached to the same pool. (Usually,
    you'd use the same cache to allocate and deallocate a block, but
    the MPS is more flexible.)

    *size* is the :term:`size` of the block. It should be the size
    that was specified when the block was allocated (the cache knows
    what the real size of the block is).

    .. topics::

        :ref:`topic-cache`.

    .. note::

        The :term:`client program` is responsible for synchronizing
        the access to the cache, but if the cache decides to access
        the pool, the MPS will properly synchronize with any other
        :term:`threads <thread>` that might be accessing the same
        pool.

    .. note::

        There's also a macro :c:func:`MPS_SAC_FREE_FAST` that does the
        same thing. The macro is faster, but generates more code and
        does no checking.

    .. note::

        :c:func:`mps_sac_free` does very little checking: it's
        optimized for speed. :term:`Double frees <double free>` and
        other mistakes will only be detected when the cache is flushed
        (either by calling :c:func:`mps_sac_flush` or automatically),
        and may not be detected at all, if intervening operations have
        obscured symptoms.


.. c:function:: MPS_SAC_FREE_FAST(mps_sac_t sac, mps_addr_t p, size_t size)

    A macro alternative to :c:func:`mps_sac_free` that is faster than
    the function but does no checking. The arguments are identical to
    the function.

    .. topics::

        :ref:`topic-cache`.


.. c:type:: mps_sac_class_s

    The type of the structure describing a :term:`size class` in a
    :term:`segregated allocation cache`. ::

        typedef struct mps_sac_class_s {
            size_t   mps_block_size;
            size_t   mps_cached_count;
            unsigned mps_frequency;
        } mps_sac_class_s;

    An array of these structures must be passed to
    :c:func:`mps_sac_create` when creating a segregated allocation
    cache.

    *mps_block_size* is the maximum :term:`size` of any :term:`block`
    in this size class. It must be a multiple of the alignment of the
    :term:`alignment` of the :term:`pool` to which the cache belongs.

    *mps_cached_count* is the number of blocks of this size class to
    cache. It is advice to the MPS on how many blocks to cache, not an
    absolute limit. The cache policy tries to accommodate fluctuations
    in the population and minimize the cost of responding to client
    requests; the purpose of this parameter is to limit how much
    memory the :term:`client program` is willing to set aside for this
    purpose. However, a *cached_count* of zero prevents any caching of
    blocks falling into that size class.

    *mps_frequency* is a number that describes the frequency of
    requests (allocation and deallocation combined) in this size class
    relative to the other size classes in the cache.

    .. topics::

        :ref:`topic-cache`.


.. c:function:: mps_res_t mps_sac_create(mps_sac_t *sac_o, mps_pool_t pool, size_t classes_count, mps_sac_class_s *classes)

    Create a :term:`segregated allocation cache` for a :term:`pool`.

    *sac_o* points to a location that will hold the address of the
    segregated allocation cache.

    *pool* is the pool the cache is attached to.

    *classes_count* is the number of :term:`size classes <size class>`
    in the cache.

    *classes* points to the an array describing the size classes in
    the cache.

    Returns :c:macro:`MPS_RES_OK` if the segregated allocation cache
    is created successfully. Returns :c:macro:`MPS_RES_MEMORY` or
    :c:macro:`MPS_RES_COMMIT_LIMIT` when it fails to allocate memory
    for the internal cache structure. Returns :c:macro:`MPS_RES_LIMIT`
    if you ask for too many size classes: in this case, combine some
    small adjacent classes. Returns :c:macro:`MPS_RES_PARAM` if the
    pool doesn't support segregated allocation caches.

    This function creates an allocation cache whose :term:`free list`
    is segregated into the given size classes. The cache can get more
    memory from the given pool, or return memory to it.

    Segregated allocation caches can be associated with any pool that
    supports :term:`manual <manual memory management>` allocation
    using the functions :c:func:`mps_alloc` and :c:func:`mps_free`.

    The size classes are described by an array of element type
    :c:func:`mps_sac_class_s`. This array is used to initialize the
    segregated allocation cache, and is not needed
    after:c:func:`mps_sac_create` returns. The following constraints
    apply to the array:

    * You must specify at least one size class. 

    * All size classes must have different sizes.

    * The size classes must be given in the order of increasing size.

    * The smallest size must be at least as large as ``sizeof(void *)``.

    * Each size must be a multiple of the :term:`alignment` of the
      pool.

    * There might be a limit on how many classes can be described, but
      it will be at least :c:macro:`MPS_SAC_CLASS_LIMIT`.

    The MPS automatically provides an "overlarge" size class for
    arbitrarily large allocations above the largest size class
    described. Allocations falling into the overlarge size class are
    not cached.

    Any allocations whose size falls between two size classes are
    allocated from the larger size class.

    .. topics::

        :ref:`topic-cache`.

    .. note::

        Too many size classes will slow down allocation; too few size
        classes waste more space in internal fragmentation. It is
        assumed that overlarge allocations are rare; otherwise, you
        would add another size class for them, or even create separate
        allocation caches or pools for them.

    .. warning::

        Segregated allocation caches work poorly with debugging pool
        classes: the debugging checks only happen when blocks are
        moved between the cache and the pool.


.. c:function:: void mps_sac_destroy(mps_sac_t sac)

    Destroy a :term:`segregated allocation cache`.

    *sac* is the segregated allocation cache to destroy.

    Returns all memory in the cache to the associated :term:`pool`.
    The pool might then return some memory to the :term:`arena`, but
    that's up to the pool's usual policy.

    Destroying the cache has no effect on blocks allocated through it.

    .. topics::

        :ref:`topic-cache`.


.. c:function:: void mps_sac_flush(mps_sac_t sac)

    Flush a :term:`segregated allocation cache`, returning all memory
    held in it to the associated :term:`pool`.

    *sac* is the segregated allocation cache to flush.

    This is something that you'd typically do when you know you won't
    be using the segregated allocation cache for awhile, but want to
    hold on to the cache itself. Destroying a cache has the effect of
    flushing it.

    Flushing the segregated allocation cache might well cause the pool
    to return some memory to the :term:`arena`, but that's up to the
    pool's usual policy.

    Note that the MPS might also decide to take memory from the
    segregated allocation cache without the :term:`client program`
    requesting a flush.

    .. topics::

        :ref:`topic-cache`.

    .. note::

        The :term:`client program` is responsible for synchronizing
        the access to the cache, but if the cache decides to access
        the pool, the MPS will properly synchronize with any other
        :term:`threads <thread>` that might be accessing the same
        pool.


.. c:type:: mps_sac_t

    The type of :term:`segregated allocation caches <segregated
    allocation cache>`.

    .. topics::

        :ref:`topic-cache`.


.. c:function:: MPS_SCAN_BEGIN(mps_ss_t ss)

    Within a :term:`scan method`, set up local information required
    by :c:func:`MPS_FIX1`, :c:func:`MPS_FIX2` and
    :c:func:`MPS_FIX12`. The local information persists until
    :c:func:`MPS_SCAN_END`.

    *ss* is the :term:`scan state` that was passed to the scan method.

    .. topics::

        :ref:`topic-scanning`.

    .. note::

        Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`,
        the scan state is in a special state, and must not be passed
        to a function. If you really need to do so, for example
        because you have an embedded structure shared between two scan
        methods, you must wrap the call with :c:func:`MPS_FIX_CALL` to
        ensure that the scan state is passed correctly.


.. c:function:: MPS_SCAN_END(mps_ss_t ss)

    Within a :term:`scan method`, terminate a block started by
    :c:func:`MPS_SCAN_BEGIN`.

    *ss* is the :term:`scan state` that was passed to the scan
    method.

    .. topics::

        :ref:`topic-scanning`.

    .. note::

        :c:func:`MPS_SCAN_END` ensures that the scan is completed, so
        successful termination of a scan must invoke it. However, in
        case of an error it is allowed to return from the scan
        method without invoking :c:func:`MPS_SCAN_END`.

    .. note::

        Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, the
        scan state is in a special state, and must not be passed to a
        function. If you really need to do so, for example because you
        have an embedded structure shared between two scan methods, you
        must wrap the call with :c:func:`MPS_FIX_CALL` to ensure that the
        scan state is passed correctly.


.. c:type:: mps_ss_t

    The type of :term:`scan states <scan state>`.

    A scan state represents the state of the current :term:`scan`. The
    MPS passes a scan state to the :term:`scan method` of an
    :term:`object format` when it needs to :term:`scan` for
    :term:`references <reference>` within a region of memory. The
    scan method must pass the scan state to :c:func:`MPS_SCAN_BEGIN`
    and :c:func:`MPS_SCAN_END` to delimit a sequence of fix
    operations, and to the functions :c:func:`MPS_FIX1` and
    :c:func:`MPS_FIX2` when fixing a :term:`reference`.

    .. topics::

        :ref:`topic-scanning`.


.. c:function:: mps_reg_scan_t mps_stack_scan_ambig

    A root scanning function for :term:`ambiguous <ambiguous
    reference>` scanning of :term:`threads <thread>`, suitable for
    passing to :c:func:`mps_root_create_reg`.

    It scans all integer registers and everything on the stack of the
    thread given, and can therefore only be used with :term:`ambiguous
    roots <ambiguous root>`. It only scans locations that are at, or
    higher on the stack (that is, more recently added), the stack
    bottom that was passed to :c:func:`mps_thread_create`. References
    are assumed to be represented as machine words, and are required
    to be 4-byte-aligned; unaligned values are ignored.

    .. topics::

        :ref:`topic-platform`, :ref:`topic-root`.

    .. note::

        The MPS provides this function because it's hard to write: it
        depends on the operating system, the architecture, and in some
        cases the compiler.


.. c:function:: mps_word_t mps_telemetry_control(mps_word_t reset_mask, mps_word_t flip_mask)

    Update and return the :term:`telemetry filter`.

    *reset_mask* is a :term:`bitmask` indicating the bits in the
    telemetry filter that should be reset.

    *flip_mask* is a bitmask indicating the bits in the telemetry
    filter whose value should be flipped after the resetting.

    Returns the previous value of the telemetry filter, prior to the
    reset and the flip.

    The parameters *reset_mask* and *flip_mask* allow the
    specification of any binary operation on the filter control. For
    typical operations, the parameters should be set as follows:

    ============  ============  ===========
    Operation     *reset_mask*  *flip_mask*
    ============  ============  ===========
    ``set(M)``    ``M``         ``M``
    ------------  ------------  -----------
    ``reset(M)``  ``M``         ``0``
    ------------  ------------  -----------
    ``flip(M)``   ``0``         ``M``
    ------------  ------------  -----------
    ``read()``    ``0``         ``0``
    ============  ============  ===========

    The significance of the bits is liable to change, but the current
    meanings (zero being the least significant bit) are:

    0. per space or :term:`arena`;

    1. per :term:`pool`;

    2. per :term:`trace` or scan;

    3. per :term:`page` (segment);

    4. per :term:`reference` or :term:`fix`;

    5. per allocation, :term:`block`, or :term:`object`;

    6. "user" events: see :c:func:`mps_telemetry_intern`.

    .. topics::

        :ref:`topic-telemetry`.


.. c:function:: void mps_telemetry_flush(void)

    Flush the internal event buffers into the :term:`telemetry stream`.

    This function also calls :c:func:`mps_lib_io_flush` on the event
    stream itself. This ensures that even the latest events are now
    properly recorded, should the :term:`client program` terminate
    (uncontrollably as a result of a bug, for example) or some
    interactive tool require access to the event data. You could even
    try calling this from a debugger after a problem.

    .. topics::

        :ref:`topic-telemetry`.


.. c:function:: mps_word_t mps_telemetry_intern(char *label)

    Registers a string with the MPS, and receives a :term:`telemetry
    label`, suitable for passing to :c:func:`mps_telemetry_label`.

    *label* is a NUL-terminated string way. Its length should not
    exceed 256 characters, including the terminating NUL.

    Returns a telemtry label: a unique identifier that may be used to
    represent the string in future.

    The intention of this function is to provide an identifier that
    can be used to concisely represent a string for the purposes of
    :c:func:`mps_telemetry_label`. 

    .. topics::

        :ref:`topic-telemetry`.

    .. note::

        The appropriate setting must be turned on in the
        :term:`telemetry filter` (via :c:func:`mps_telemetry_control`)
        before this function is invoked; the associated event is of
        the "user" kind.


.. c:function:: void mps_telemetry_label(mps_addr_t addr, mps_word_t label)

    Associate a telemetry label returned from
    :c:func:`mps_telemetry_intern` with an address.

    *addr* is an address.

    *label* is a telemetry label returned from
    :c:func:`mps_telemetry_intern`.

    The label will be associated with the address when it appears in
    the :term:`telemetry stream`.

    .. topics::

        :ref:`topic-telemetry`.

    .. note::

       The "user" kind must be set in the :term:`telemetry filter`
       via :c:func:`mps_telemetry_control`.


.. c:type:: mps_thr_t

    The type of registered :term:`thread` descriptions.

    In a multi-threaded environment where :term:`incremental
    garbage collection` is used, threads must be registered
    with the MPS using :c:func:`mps_thread_reg` so that the MPS can
    examine their state.

    Even in a single-threaded environment it may be useful to register
    a thread with the MPS so that its stack can be registered as a
    :term:`root` using :c:func:`mps_root_create_reg`.

    .. topics::

        :ref:`topic-root`.


========================
Declared in ``mpsacl.h``
========================

.. c:function:: mps_arena_class_t mps_arena_class_cl(void)

    Return the :term:`arena class` for a :term:`client arena`.

    A client arena gets its managed memory from the :term:`client
    program`. This memory block is passed when the arena is created.

    When creating a client arena, :c:func:`mps_arena_create` takes two
    extra arguments::

        mps_res_t mps_arena_create(mps_arena_t *arena_o,
                                   mps_arena_class_t mps_arena_class_cl,
                                   size_t size, void *block)

    *block* is the :term:`address` of the block of memory that will be
    managed by the arena.

    *size* is its :term:`size`.

    If the block is too small to hold the internal arena structures,
    :c:func:`mps_arena_create` returns :c:macro:`MPS_RES_MEMORY`. In
    this case, you should allocate a (much) larger block, and try
    again.

    .. topics::

        :ref:`topic-arena`.


========================
Declared in ``mpsavm.h``
========================

.. c:function:: mps_arena_class_t mps_arena_class_vm(void)

    Return the :term:`arena class` for a :term:`virtual memory arena`.

    A virtual memory arena uses the operating system's :term:`virtual
    memory` interface to allocate memory. The chief consequence of
    this is that the arena can manage many more virtual addresses than
    it needs to commit memory to. This gives it flexibility as to
    where to place :term:`blocks <block>`, which reduces
    :term:`fragmentation` and helps make :term:`garbage collection`
    more efficient.

    This class is similar to :c:func:`mps_arena_class_vmnz` but uses a
    more complex placement policy, which is more suited to copying
    garbage collection.

    When creating a virtual memory arena, :c:func:`mps_arena_create`
    takes one extra argument::

        mps_res_t mps_arena_create(mps_arena_t *arena_o,
                                   mps_arena_class_t arena_class_vm(),
                                   size_t size)

    *size* is the initial amount of virtual address space, in bytes,
    that the arena will reserve (this space is initially reserved so
    that the arena can subsequently use it without interference from
    other parts of the program, but most of it is not committed, so
    it don't require any RAM or backing store). The arena may
    allocate more virtual address space beyond this initial
    reservation as and when it deems it necessary. The MPS is most
    efficient if you reserve an address space that is several times
    larger than your peak memory usage.

    If the MPS fails to reserve adequate address space to place the
    arena in, :c:func:`mps_arena_create` returns
    :c:macro:`MPS_RES_RESOURCE`. Possibly this means that other parts
    of the program are reserving too much virtual memory.

    If the MPS fails to allocate memory for the internal arena
    structures, :c:func:`mps_arena_create` returns
    :c:macro:`MPS_RES_MEMORY`. Either *size* was far too small or you
    ran out of swap space.

    .. topics::

        :ref:`topic-arena`.


.. c:function:: mps_arena_class_t mps_arena_class_vmnz(void)

    Return the :term:`arena class` for a :term:`virtual memory arena`.
    This class is similar to :c:func:`mps_arena_class_vm`, except that
    it has a simple placement policy ("no zones") that makes it slightly
    faster.

    When creating an arena of this class, :c:func:`mps_arena_create`
    takes one extra argument::

        mps_res_t mps_arena_create(mps_arena_t *arena_o,
                                   mps_arena_class_t arena_class_vmnz,
                                   size_t size)

    *size* is the total amount of virtual address space, in bytes,
    that the arena will reserve. The arena will not subsequently use
    any more address space: compare with :c:func:`mps_arena_class_vm`,
    which can grow.

    .. topics::

        :ref:`topic-arena`.


=========================
Declared in ``mpscamc.h``
=========================

.. c:function:: void mps_amc_apply(mps_pool_t pool, void (*f)(mps_addr_t object, void *p, size_t s), void *p, size_t s)

    Visit all :term:`formatted objects <formatted object>` in an
    :ref:`pool-amc`.

    *pool* is the pool whose formatted objects you want to visit.

    *f* is a function that will be called for each formatted object in
    the pool. It takes three arguments: *object* is the address of the
    object; *p* and *s* are the corresponding arguments that were
    passed to :c:func:`mps_amc_apply`.

    *p* and *s* are arguments that will be passed to *f* each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    You may only call this function when the :term:`arena` is in the
    :term:`parked state`, for example, after calling
    :c:func:`mps_arena_collect` or :c:func:`mps_arena_park`.

    The function *f* will be called on both :term:`client <client
    object>` and :term:`padding objects <padding object>`. It is the
    job of *f* to distinguish, if necessary, between the two. It may
    also be called on :term:`dead` objects that the collector has not
    recycled or has been unable to recycle.

    The function *f* may not allocate memory or access any
    automatically-managed memory except within *object*.

    .. topics::

        :ref:`topic-scanning`.

    .. note::

        There is no equivalent function for other pool classes, but
        there is a more general function
        :c:func:`mps_arena_formatted_objects_walk` that visits all
        formatted objects in the arena.


.. c:function:: mps_class_t mps_class_amc(void)

    Return the :term:`pool class` for an AMC (Automatic
    Mostly-Copying) :term:`pool`.

    When creating an AMC pool, :c:func:`mps_pool_create` takes one
    extra argument::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_amc(),
                                  mps_fmt_t fmt)

    *fmt* specifies the :term:`object format` for the objects
    allocated in the pool.

    .. topics::

        :ref:`pool-amc`.


==========================
Declared in ``mpscmvff.h``
==========================

.. c:function:: mps_class_t mps_class_mvff(void)

    Return the :term:`pool class` for an MVFF (Manual Variable-size
    First Fit) :term:`pool`.

    When creating an MVFF pool, :c:func:`mps_pool_create` takes six
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_mvff(),
                                  mps_size_t extendBy,
                                  mps_size_t avgSize,
                                  mps_align_t alignment,
                                  mps_bool_t slotHigh,
                                  mps_bool_t arenaHigh,
                                  mps_bool_t firstFit)

    *extendBy* is the :term:`size` of :term:`segment` to allocate by
    default.

    *avgSize* is the average size of blocks to be allocated.

    *alignment* is the :term:`alignment` of addresses for allocation
    (and freeing) in the pool. If an unaligned size is passed to
    :c:func:`mps_alloc` or :c:func:`mps_free`, it will be rounded up
    to the pool's alignment. The minimum alignment supported by pools
    of this class is ``sizeof(void *)``.

    *slotHigh*, *arenaHigh*, and *firstFit* are undocumented and may
    be set to (0, 0, 1) or (1, 1, 1). No other setting of these
    parameters is currently recommended.

    .. topics::

        :ref:`pool-mvff`.


=========================
Declared in ``mpscmv2.h``
=========================

.. c:function:: mps_class_t mps_class_mvt(void)

    Return the :term:`pool class` for an MVT (Manual Variable-size
    Temporal-fit) :term:`pool`.

    When creating an MVT pool, :c:func:`mps_pool_create` takes five
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_mvt(),
                                  size_t minimum_size,
                                  size_t mean_size,
                                  size_t maximum_size,
                                  mps_count_t reserve_depth,
                                  mps_count_t fragmentation_limit)

    *minimum_size*, *mean_size*, and *maximum_size* are the minimum,
    mean, and maximum (typical) :term:`size` of :term:`blocks <block>`
    expected to be allocated in the pool. Blocks smaller than
    *minimum_size* and larger than *maximum_size* may be allocated,
    but the pool is not guaranteed to manage them space-efficiently.
    Furthermore, partial freeing is not supported for blocks larger
    than *maximum_size*; doing so will result in the storage of the
    block never being reused. *mean_size* need not be an accurate
    mean, although the pool will manage *mean_size* blocks more
    efficiently if it is.

    *reserve_depth* is the expected hysteresis of the population of
    the pool. When blocks are freed, the pool will retain sufficient
    storage to allocate *reserve_depth* blocks of *mean_size* for near
    term allocations (rather than immediately making that storage
    available to other pools).

    *fragmentation_limit* is a percentage in (0, 100] that can be used
    to set an upper limit on the space overhead of MVT in case block
    death times and allocations do not correlate well. If the free
    space managed by the pool as a ratio of all the space managed by
    the pool exceeds *fragmentation_limit*, the pool falls back to a
    first fit allocation policy, exploiting space more efficiently at
    a cost in time efficiency. A fragmentation limit of 0 would cause
    the pool to operate as a first-fit pool, at a significant cost in
    time efficiency, therefore is not permitted.

    .. topics::

        :ref:`pool-mvt`


=========================
Declared in ``mpscsnc.h``
=========================

.. c:function:: mps_class_t mps_class_snc(void)

    Return the :term:`pool class` for an SNC (Stack No Check)
    :term:`pool`.

    When creating an SNC pool, :c:func:`mps_pool_create` takes one
    extra argument::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena,
                                  mps_class_t mps_class_snc(),
                                  mps_fmt_t fmt)

    *fmt* specifies the :term:`object format` for the objects
    allocated in the pool. The format should provide at least the
    methods scan, skip, and pad.

    .. topics::

        :ref:`pool-snc`.


========================
Declared in ``mpslib.h``
========================

.. c:function:: int mps_lib_memcmp(const void *s1, const void *s2, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function ``memcmp``.

    *s1* and *s2* point to :term:`blocks <block>` of memory to be
    compared.

    *n* is the :term:`size` of the blocks.

    Returns an integer that is greater than, equal to, or less than
    zero, accordingly as the block pointed to by *s1* is greater than,
    equal to, or less than the block pointed to by *s2*.

    This function is intended to have the same semantics as the
    ``memcmp`` function of the [ANSI C Standard]_ (section 7.11.4.1).

    .. topics::

        :ref:`topic-plinth`.


.. c:function:: void *mps_lib_memcpy(void *dest, const void *source, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function ``memcpy``.

    *dest* points to the destination.

    *source* points to the source.

    *n* is the number of bytes to copy from *source* to *dest*.

    Returns *dest*.

    This function is intended to have the same semantics as the
    ``memcpy`` function of the [ANSI C Standard]_ (section 7.11.2.1).

    The MPS never passes overlapping blocks to
    :c:func:`mps_lib_memcpy`.

    .. topics::

        :ref:`topic-plinth`.

.. c:function:: void *mps_lib_memset(void *s, int c, size_t n)

    A :term:`plinth` function similar to the standard :term:`C`
    function ``memset``.

    *s* points to the :term:`block` to fill with the byte *c*.

    *c* is the byte to fill with (when converted to ``unsigned char``).

    *n* is the :term:`size` of the block.

    Returns *s*.

    This function is intended to have the same semantics as the
    ``memset`` function of the [ANSI C Standard]_ (section 7.11.6.1).

    .. topics::

        :ref:`topic-plinth`.


.. c:function:: unsigned long mps_lib_telemetry_control()

    A :term:`plinth` function to supply a default value for the
    :term:`telemetry filter` from the environment. See
    :c:func:`mps_telemetry_control` for more information on the
    significant of the value.

    Returns the default value of the telemetry filter, as derived from
    the environment. It is recommended that the environment be
    consulted for a symbol analogous to
    :c:macro:`MPS_TELEMETRY_CONTROL`, subject to local restrictions.

    In the absence of environmental data, a default of zero is
    recommended.

    .. topics::

        :ref:`topic-plinth`, :ref:`topic-telemetry`.


=======================
Declared in ``mpstd.h``
=======================

.. c:type:: MPS_T_WORD

    An unsigned integral type that is the same size as an
    :term:`object pointer`, so that ``sizeof(MPS_T_WORD) ==
    sizeof(void*)``.

    The exact identity of this type is platform-dependent. Typical
    identities are ``unsigned long`` and ``unsigned __int_64``.

    .. topics::

        :ref:`topic-platform`.


.. c:macro:: MPS_WORD_SHIFT

    The logarithm to base 2 of the constant :c:macro:`MPS_WORD_WIDTH`,
    so that ``1 << MPS_WORD_SHIFT == MPS_WORD_WIDTH``.

    The value is platform-dependent. Typical values are 5 and 6.

    .. topics::

        :ref:`topic-platform`.


.. c:macro:: MPS_WORD_WIDTH

    The width in bits of the type :c:type:`MPS_T_WORD`, so that
    ``MPS_WORD_WIDTH == sizeof(MPS_T_WORD) * CHAR_BIT``.

    This value is platform-dependent. It is always a power of 2:
    typical values are 32 and 64.

    .. topics::

        :ref:`topic-platform`.


=========================
Undocumented in ``mps.h``
=========================

.. c:type:: mps_chain_t
.. c:type:: mps_ld_t
.. c:type:: mps_alloc_pattern_t
.. c:type:: mps_frame_t
.. c:type:: mps_word_t
.. c:type:: mps_shift_t
.. c:type:: mps_rm_t
.. c:type:: mps_sac_freelist_block_s
.. c:type:: mps_ld_s
.. c:type:: mps_fmt_fixed_s
.. c::function:: mps_bool_t mps_arena_step(mps_arena_t arena, double interval, double multiplier)
.. c:function:: mps_res_t mps_arena_start_collect(mps_arena_t arena)
.. c:function:: void mps_arena_destroy(mps_arena_t arena)
.. c:function:: size_t mps_arena_reserved(mps_arena_t arena)
.. c:function:: mps_res_t mps_arena_extend(mps_arena_t arena, mps_addr_t base, size_t size)
.. c:function:: mps_res_t mps_fmt_create_fixed(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_fixed_s *fmt_fixed)
.. c:function:: void mps_fmt_destroy(mps_fmt_t format)
.. c:function:: mps_bool_t mps_addr_pool(mps_pool_t *pool_o, mps_arena_t arena, mps_addr_t addr)
.. c:function:: mps_bool_t mps_addr_fmt(mps_fmt_t *fmt_o, mps_arena_t arena, mps_addr_t addr)
.. c:function:: mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, mps_class_t class, ...)
.. c:function:: mps_res_t mps_pool_create_v(mps_pool_t *pool_o, mps_arena_t arena, mps_class_t class, va_list args)
.. c:function:: void mps_pool_destroy(mps_pool_t pool)
.. c:type:: mps_gen_param_s
.. c:function:: mps_res_t mps_chain_create(mps_chain_t *chain_o, mps_arena_t arena, size_t gen_count, mps_gen_param_s *params)
.. c:function:: void mps_chain_destroy(mps_chain_t chain)
.. c:function:: mps_res_t mps_ap_create(mps_ap_t *ap_o, mps_pool_t pool, ...)
.. c:function:: mps_res_t mps_ap_create_v(mps_ap_t *ap_o, mps_pool_t pool, va_list args)
.. c:function:: void mps_ap_destroy(mps_ap_t mps_ap)
.. c:function:: mps_res_t mps_reserve(mps_addr_t *p_o, mps_ap_t ap, size_t size)
.. c:function:: mps_res_t mps_reserve_with_reservoir_permit(mps_addr_t *p_o, mps_ap_t ap, size_t size)
.. c:function:: mps_bool_t mps_commit(mps_ap_t ap, mps_addr_t p, size_t size)
.. c:function:: void mps_reservoir_limit_set(mps_arena_t arena, size_t size)
.. c:function:: size_t mps_reservoir_limit(mps_arena_t arena)
.. c:function:: size_t mps_reservoir_available(mps_arena_t arena)
.. c:function:: MPS_RESERVE_BLOCK(mps_res_t res_v, mps_addr_t p_v, mps_ap_t ap, size_t size)
.. c:function:: MPS_RESERVE_WITH_RESERVOIR_PERMIT_BLOCK(mps_res_t res_v, mps_addr_t p_v, mps_ap_t ap, size_t size)
.. c:function:: void mps_root_destroy(mps_root_t mps_root)
.. c:type:: void *(*mps_tramp_t)(void *p, size_t s)
.. c:function:: void mps_tramp(void **r_o, mps_tramp_t tramp, void *p, size_t s)
.. c:function:: mps_res_t mps_thread_reg(mps_thr_t *mps_thr_o, mps_arena_t arena)
.. c:function:: void mps_thread_dereg(mps_thr_t thread)
.. c:function:: void mps_ld_reset(mps_ld_t ld, mps_arena_t arena)
.. c:function:: void mps_ld_add(mps_ld_t ld, mps_arena_t arena, mps_addr_t addr)
.. c:function:: void mps_ld_merge(mps_ld_t ld, mps_arena_t arena, mps_ld_t from)
.. c:function:: mps_bool_t mps_ld_isstale(mps_ld_t ld, mps_arena_t arena, mps_addr_t addr)
.. c:function:: mps_word_t mps_collections(mps_arena_t arena)
.. c:function:: mps_res_t mps_definalize(mps_arena_t arena, mps_addr_t *refref)
.. c:function:: mps_res_t mps_alert_collection_set(mps_arena_t arena, mps_alert_collection_fn_t fn)
.. c:type:: void (*mps_alert_collection_fn_t)(int, int)
.. c:function:: void mps_pool_check_free_space(mps_pool_t mps_pool)


============================
Undocumented in ``mpslib.h``
============================

.. c:function:: int mps_lib_get_EOF(void)
.. c:type:: mps_lib_FILE
.. c:function:: mps_lib_FILE *mps_lib_get_stderr(void)
.. c:function:: mps_lib_FILE *mps_lib_get_stdout(void)
.. c:function:: int mps_lib_fputc(int c, mps_lib_FILE *stream)
.. c:function:: int mps_lib_fputs(const char *s, mps_lib_FILE *stream)
.. c:function:: void mps_lib_assert_fail(const char *message)
.. c:function:: mps_clock_t mps_clock(void)
.. c:type:: mps_clock_t
.. c:function:: mps_clock_t mps_clocks_per_sec(void)


=============================
Undocumented in ``mpscamc.h``
=============================

.. c:function:: mps_class_t mps_class_amcz(void)


=============================
Undocumented in ``mpscams.h``
=============================

.. c:function:: mps_class_t mps_class_ams(void)
.. c:function:: mps_class_t mps_class_ams_debug(void)


=============================
Undocumented in ``mpscawl.h``
=============================

.. c:function:: mps_class_t mps_class_awl(void)


============================
Undocumented in ``mpsclo.h``
============================

.. c:function:: mps_class_t mps_class_lo(void)


============================
Undocumented in ``mpscmv.h``
============================

.. c:function:: size_t mps_mv_free_size(mps_pool_t pool)
.. c:function:: size_t mps_mv_size(mps_pool_t pool)
.. c:function:: mps_class_t mps_class_mv(void)
.. c:function:: mps_class_t mps_class_mv_debug(void)


=============================
Undocumented in ``mpscmv2.h``
=============================

.. c:function:: mps_class_t mps_class_mvt(void)
.. c:function:: size_t mps_mvt_free_size(mps_pool_t pool)
.. c:function:: size_t mps_mvt_size(mps_pool_t pool)


==============================
Undocumented in ``mpscmvff.h``
==============================

.. c:function:: size_t mps_mvff_free_size(mps_pool_t mpspool)
.. c:function:: size_t mps_mvff_size(mps_pool_t pool)
.. c:function:: mps_class_t mps_class_mvff_debug(void)


===========================
Undocumented in ``mpsw3.h``
===========================

.. c:function:: LONG mps_SEH_filter(LPEXCEPTION_POINTERS info, void **hp_o, size_t *hs_o)
.. c:function:: void mps_SEH_handler(void *p, size_t s)

===========================
Undocumented in ``mpsio.h``
===========================

.. c:type:: mps_io_t
.. c:function:: mps_res_t mps_io_create(mps_io_t *mps_io_r)
.. c:function:: void mps_io_destroy(mps_io_t mps_io)
.. c:function:: mps_res_t mps_io_write(mps_io_t mps_io, void *buf, size_t size)
.. c:function:: mps_res_t mps_io_flush(mps_io_t mps_io)


===========================
Undocumented in ``mpstd.h``
===========================

.. c:macro:: MPS_PF_STRING
.. c:macro:: MPS_PF_ALIGN
.. c:macro:: MPS_ARCH_I3
.. c:macro:: MPS_ARCH_I4
.. c:macro:: MPS_ARCH_PP
.. c:macro:: MPS_ARCH_S8
.. c:macro:: MPS_ARCH_S9
.. c:macro:: MPS_BUILD_GC
.. c:macro:: MPS_BUILD_MV
.. c:macro:: MPS_BUILD_SC
.. c:macro:: MPS_OS_FR
.. c:macro:: MPS_OS_LI
.. c:macro:: MPS_OS_SO
.. c:macro:: MPS_OS_W3
.. c:macro:: MPS_OS_XC
.. c:macro:: MPS_PF_FRI3GC
.. c:macro:: MPS_PF_LII3GC
.. c:macro:: MPS_PF_LIPPGC
.. c:macro:: MPS_PF_SOS8GC
.. c:macro:: MPS_PF_SOS9SC
.. c:macro:: MPS_PF_W3I3MV
.. c:macro:: MPS_PF_XCPPGC

.. highlight:: c

.. Checklist of things to say about a symbol

    Signature
    Summary
    Arguments
    Result
    Status (deprecated?)
    Topic


================
Symbol reference
================


---------------------
Declared in ``mps.h``
---------------------

.. c:type:: mps_addr_t

    The type of :term:`addresses <address>` managed by the MPS, and
    also the type of :term:`references <reference>`.

    It is used in the MPS interface for any pointer that is under the
    control of the MPS. In accordance with standard C practice, null
    pointers of type :c:type:`mps_addr_t` will never be used to
    represent a reference to a block.

    .. seealso::

        :ref:`topic-allocation` and :ref:`topic-platform`.


.. c:type:: mps_align_t

    The type of an :term:`alignment`. It is an integral type
    equivalent to ``size_t``. An alignment must be a positive power of
    2.

    .. seealso::

        :ref:`topic-allocation`.


.. c:function:: mps_res_t mps_alloc(mps_addr_t *p_o, mps_pool_t pool, size_t size, ...)

    Allocate a :term:`block` of memory in a :term:`pool`.

    *p_o* points to a location that will hold the address of the
    allocated block.

    *pool* the pool to allocate in.

    *size* is the :term:`size` of the block to allocate.

    Some pool classes require additional arguments to be passed to
    :c:func:`mps_alloc`. See the documentation for the pool class.

    .. seealso::

        :ref:`topic-allocation`.

    .. note::

        There's an alternative function :c:func:`mps_alloc_v` that
        takes its extra arguments using the standard C ``va_list``
        mechanism.


.. c:function:: mps_res_t mps_alloc_v(mps_addr_t *p_o, mps_pool_t pool, size_t size, va_list args)

    An alternative to :c:func:`mps_alloc` that takes its extra
    arguments using the standard C ``va_list`` mechanism.


.. c:function:: mps_alloc_pattern_t mps_alloc_pattern_ramp(void)

    Return an :term:`allocation pattern` indicating that allocation
    will follow a :term:`ramp pattern`.

    This indicates to the MPS that most of the blocks allocated after
    the call to :c:func:`mps_ap_alloc_pattern_begin` are likely to be
    :term:`dead` by the time of the corresponding call to
    :c:func:`mps_ap_alloc_pattern_end`.

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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
    that the allocation point belongs to. Typically, :term:`manual`
    pool classes use this declaration to mean that the blocks are dead
    and their space can be reclaimed immediately, whereas
    :term:`automatic` pool classes use this declaration to mean that
    the blocks are likely to be mostly dead, and may use this
    declaration to alter its collection decisions. See the
    documentation for the pool class.

    In general a frame other than the current frame can be popped (all
    frames pushed more recently will be invalidated as well, as
    described above), but a pool class may impose the restriction that
    only the current frame may be popped. This restriction means that
    every push must have a corresponding pop. See the documentation
    for the pool class.

    It is illegal to pop frames out of order (so the sequence "A =
    push; B = push; pop A; pop B" is illegal) or to pop the same frame
    twice (so the sequence "A = push, pop A, pop A" is illegal).

    .. seealso::

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

    .. seealso::

        :ref:`topic-frame`.


.. c:function:: extern void mps_arena_clamp(mps_arena_t arena)

    Put an :term:`arena` into the :term:`clamped state`.
    
    *arena* is the arena to clamp.

    In the clamped state, no object motion will occur and the
    staleness of location dependencies will not change. All references
    to objects loaded while the arena is clamped will keep the same
    binary representation until after it is released.

    In a clamped arena, incremental collection may still occur, but it
    will not be visible to the mutator and no new collections will
    begin. Space used by unreachable objects will not be recycled
    until the arena is unclamped.

    .. seealso::

        :ref:`topic-arena`.


.. c:type:: mps_arena_class_t

    The type of :term:`arena classes <arena class>`.


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

    .. seealso::

        :ref:`topic-arena`.


.. c:function:: size_t mps_arena_commit_limit(mps_arena_t arena)

    Return the current :term:`commit limit` for
    an arena.

    *arena* is the arena to return the commit limit for.

    Returns the commit limit in bytes. The commit limit controls how
    much memory the MPS can obtain from the operating system, and can
    be changed using :c:func:`mps_commit_limit_set`.

    .. seealso::

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

    .. seealso::

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

    Return the total :term:`committed memory` for an :term:`arena`.

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

    * there might also be :term:`spare committed memory` (see
      :c:func:`mps_arena_spare_committed`).

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

    .. seealso::

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

    .. seealso::

        :ref:`topic-arena`.

    .. note::

        There's an alternative function :c:func:`mps_arena_create_v`
        that takes its extra arguments using the standard C
        ``va_list`` mechanism.


.. c:function:: mps_res_t mps_arena_create_v(mps_arena_t *arena_o, mps_arena_class_t arena_class, va_list args)

    An alternative to :c:func:`mps_arena_create` that takes its extra
    arguments using the standard C ``va_list`` mechanism.

    .. seealso::

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

    .. seealso::

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
    :term:`black` objects, though the leaf pool class
    (:c:func:`mps_class_lo`), for example, will walk all objects since
    they are validly formatted whether they are black or
    :term:`white`. :term:`Padding objects <padding object>` may be
    visited at the pool classes discretion, the :term:`client program`
    should handle this case.

    The function *f* may not allocate memory or access any
    automatically-managed memory except within *object*.

    .. seealso::

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
    immediately become invalidated (for example, a garbage collection
    may occur, the address in question may become free, the arena may
    choose to unmap the address and return storage to the operating
    system). For reliable results call this function whilst the arena
    is in the :term:`parked state`.

    .. seealso::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_park(mps_arena_t arena)

    Put an arena into the :term:`parked state`.

    *arena* is the arena to park.

    While an arena is parked, no object motion will occur and the
    staleness of location dependencies will not change. All references
    to objects loaded while the arena is parked will keep the same
    binary representation until after it is released.

    Any current collection is run to completion before the arena is
    parked, and no new collections will start. When an arena is in the
    parked state, it is necessarily not in the middle of a collection.

    .. seealso::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_release(mps_arena_t arena)

    Puts an arena into the :term:`unclamped state`.

    *arena* is the arena to unclamp.

    While an arena is unclamped, :term:`garbage collection`, object
    motion, and other background activity can take place.

    .. seealso::

        :ref:`topic-arena`.


.. c:function:: void mps_arena_roots_walk(mps_arena_t arena, mps_roots_stepper_t f, void *p, size_t s)

    Visit references in registered :term:`roots <root>` in an
    :term:`arena`.

    *arena* is the arena whose roots you want to visit.

    *f* is a function that will be called for each reference to an
    object in an :term:`automatically <automatic>` managed :term:`pool
    class` that was found in a registered root beloging to the arena.
    It takes four arguments: *ref* is the address of a reference to an
    object in the arena, *root* is the root in which *ref* was found,
    and *p* and *s* are the corresponding arguments that were passed
    to :c:func:`mps_arena_roots_walk`.

    *p* and *s* are arguments that will be passed to *f* each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    This function may only be called when the arena is in the
    :term:`parked state`.

    .. seealso::

        :ref:`topic-arena`.

    .. note::

        If a root has rank :c:macro:`MPS_RANK_AMBIG` then the
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
    can be changed using :c:func:`mps_spare_commit_limit_set`.

    .. seealso::

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
    arena`) do not have maintain spare committed memory, but they
    support the two functions :c:func:`mps_arena_spare_commit_limit`
    and :c:func:`mps_arena_spare_commit_limit_set`. The functions get
    and retrieve a value but do nothing else in that case.

    Initially the spare commit limit is a configuration-dependent
    value. The value of the limit can be retrieved by the function
    :c:func:`mps_arena_spare_commit_limit`.

    .. seealso::

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
    functions for limiting the amount of :term:`committed memory`.

    .. seealso::

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

    .. seealso::

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
    garbage collection to be slow.

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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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
    in :term:`copying` or :term:`moving` :term:`pools <pool>`.

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

    .. seealso::

        :ref:`topic-format`.



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
    in :term:`copying` or :term:`moving` :term:`pools <pool>` (just
    like variant A); the addition of a :term:`class method` allows
    more information to be passed to various support tools (such as
    graphical browsers). See :c:type:`mps_fmt_class_t`.

    .. seealso::

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
    for use in :term:`automatic` memory management for objects with
    :term:`headers <header>` (hence the name). More precisely, this
    variant is intended for formats where the :term:`client program's
    <client program>` pointers point some distance into the memory
    :term:`block` containing the object. This typically happens when
    the objects have a common header used for memory management or
    class system purposes, but this situation also arises when the low
    bits of a pointer are used for a tag. The MPS does not care what
    the reason is, only about the offset of the pointer in relation to
    the memory block.

    *mps_headerSize* is the size of the header, that is, the offset of
    a client pointer from the base of the memory block.

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

        :ref:`topic-format`.


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

    .. seealso::

        :ref:`topic-message`.


.. c:function:: void mps_message_discard(mps_arena_t arena, mps_message_t message)

    Indicate to the MPS that the :term:`client program` has no further
    use for a :term:`message` and the MPS can now reclaim any storage
    associated with the message.

    *arena* is the :term:`arena` which posted the message.

    *message* is the message. After this call, *message* is invalid
    and should not be passed as an argument to any message functions.

    Messages are essentially :term:`manually <manual>` managed. This
    function allows the MPS to reclaim storage associated with
    messages. If the client does not discard messages then the
    resources used may grow without bound.

    As well as consuming resources, messages may have other effects
    that require them to be tidied by calling this function. In
    particular finalization messages refer to a :term:`finalized
    object`, and prevent the object from being reclaimed (subject to
    the usual garbage collection liveness analysis). A finalized
    object cannot be reclaimed until all its finalization messages
    have been discarded. See :c:func:`mps_message_type_finalization`.

    .. seealso::

        :ref:`topic-finalization`, :ref:`topic-message`.


.. c:function:: void mps_message_finalization_ref(mps_addr_t *ref_o, mps_arena_t arena, mps_message_t message)

    Returns the finalization reference for a finalization message.

    *ref_o* points to a location that will hold the finalization
    reference.

    *arena* is the :term:`arena` which posted the message.

    *message* is a message retrieved by :c:func:`mps_message_get` and
    not yet discarded. It must be a finalization message: see
    :c:func:`mps_message_type_finalization`.

    The reference returned by this method is a reference to the object
    that was originally registered for :term:`finalization` by a call
    to :c:func:`mps_finalize`.

    .. seealso::

        :ref:`topic-finalization`, :ref:`topic-message`.

    .. note::

        The reference returned is subject to the normal constraints,
        such as might be imposed by a :term:`moving` collection, if
        appropriate. For this reason, it is stored into the location
        pointed to by *ref_o* in order to enable the :term:`client
        program` to place it directly into scanned memory, without
        imposing the restriction that the C stack be a :term:`root`.

    .. note::

        The message itself is not affected by invoking this method.
        Until the client program calls :c:func:`mps_message_discard`
        to discard the message, it will refer to the object and
        prevent its reclamation.


.. c:macro:: MPS_RES_COMMIT_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested without exceeding the :term:`commit limit`.

    You need to deallocate something to make more space, or increase
    the commit limit by calling :c:func:`mps_arena_commit_limit_set`.

    .. seealso::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested because of an internal limitation of the
    MPS. The precise meaning depends on the function that returned the
    code. Refer to the documentation of that function for details.

    .. seealso::

        :ref:`topic-error`.


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

    .. seealso::

        :ref:`topic-format`.

    .. note::

        This method is never invoked by the :term:`garbage collector`
        on an object in a :term:`non-moving` :term:`pool`.


.. c:type:: mps_addr_t (*mps_fmt_isfwd_t)(mps_addr_t addr)

    The type of the :term:`is-forwarded method` of an :term:`object
    format`.

    *addr* is the address of a candidate object.

    If the *addr* is the address of a :term:`forwarded object`, return
    the address where the object was moved to. This must be the value
    of the *new* argument supplied to the :term:`forward method` when
    the object was moved. If not, return a null pointer.

    .. seealso::

        :ref:`topic-format`.

    .. note::

        This method is never invoked by the :term:`garbage collector`
        on an object in a :term:`non-moving` :term:`pool`.


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

    .. seealso::

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

    .. seealso::

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

    .. seealso::

        :ref:`topic-format`, :ref:`topic-scanning`.


.. c:type:: mps_fmt_t

    The type of an :term:`object format`.

    .. seealso::

        :ref:`topic-format`.


.. c:type:: void (*mps_formatted_objects_stepper_t)(mps_addr_t addr, mps_fmt_t fmt, mps_pool_t pool, void *p, size_t s)

    The type of a :term:`formatted objects <formatted object>` stepper
    function.
    
    A function of this type can be passed to
    :c:func:`mps_arena_formatted_objects_walk`, in which case it will
    be called for each formatted object in an :term:`arena`. It
    receives five arguments:
    
    *addr* is the address of the object.

    *fmt* is the :term:`object format` for that object.

    *pool* is the :term:`pool` to which the object belongs.

    *p* and *s* are the corresponding values that were passed to
    :c:func:`mps_arena_formatted_objects_walk`.

    .. seealso::

        :ref:`topic-arena`, :ref:`topic-format`.


.. c:function:: void mps_free(mps_pool_t pool, mps_addr_t addr, size_t size)

    Free a :term:`block` of memory to a :term:`pool`.

    *pool* is the pool the block belongs to.

    *addr* is the address of the block to be freed.

    *size* is the :term:`size` of the block to be freed.

    The freed block of memory becomes available for allocation by the
    pool, or the pool might decide to make it available to other
    pools, or it may be returned to the operating system.

    .. seealso::

        :ref:`topic-allocation`.

    .. note::

        :c:func:`mps_free` takes a *size* because it is most efficient
        to do so. In most programs, the type of an object is known at
        the point in the code that frees it, hence the size is
        trivially available. In such programs, storing the size on the
        MPS side would cost time and memory, and make it hard to get
        good virtual memory behaviour (as it is, the deallocation code
        doesn't have to touch the dead object at all).


.. c:macro:: MPS_RES_MEMORY

    A :term:`result code` indicating that an operation could not be
    completed because there wasn't enough memory available.

    You need to deallocate something or allow the :term:`garbage
    collector` to reclaim something to free enough memory, or expand
    the :term:`arena` (if you're using an arena for which that does
    not happen automatically).

    .. seealso::

        :ref:`topic-error`.

    .. note::

        Failing to acquire enough memory because the :term:`commit
        limit` would have been exceeded is indicated by returning
        :c:macro:`MPS_RES_COMMIT_LIMIT`, not ``MPS_RES_MEMORY``.

    .. note::

        Running out of :term:`address space` (as might happen in
        :term:`virtual memory` systems) is indicated by returning
        :c:macro:`MPS_RES_RESOURCE`, not ``MPS_RES_MEMORY``.


.. c:macro:: MPS_RES_PARAM

    A :term:`result code` indicating that an operation could not be
    completed as requested because an invalid parameter was specified
    for the operation. The precise meaning depends on the function
    that returned this result code. Refer to the documentation of that
    function for details.

    .. seealso::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_RESOURCE

    A :term:`result code` indicating that an operation could not be
    completed as requested because the MPS ran out of :term:`virtual
    memory`. 

    You need to reclaim memory within your process (as for the result
    code :c:macro:`MPS_RES_MEMORY`), or terminate other processes
    running on the same machine.

    .. seealso::

        :ref:`topic-error`.


.. c:macro:: MPS_RM_CONST

    The :term:`root mode` for :term:`constant roots <constant
    root>`. This tells the MPS that the :term:`client program` will
    not change the :term:`root` after it is declared: tat is, scanning
    the root will produce the same set of :term:`references
    <reference>` every time. Furthermore, for :term:`formatted roots
    <formatted root>` and :term:`table roots <table root>`, the client
    program will not write to the root at all.

    .. seealso::

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

    .. seealso::

        :ref:`topic-root`.

    .. note::

        You must not specify ``MPS_RM_PROT`` on a root allocated by
        the MPS.

    .. note::

        No page may contain parts of two or more protectable roots.
        You mustn't specify ``MPS_RM_PROT`` if the :term:`client
        program` or anything other than (this instance of) the MPS is
        going to protect or unprotect the relevant pages.


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
    :c:macro:`MPS_RES_COMMIT_LIMIT` if the :term:`commit limit`
    was exceeded, or :c:macro:`MPS_RES_RESOURCE` if it ran out of
    :term:`virtual memory`.

    .. seealso::

        :ref:`topic-cache`.

    .. note::

        There's also a macro :c:func:`MPS_SAC_ALLOC_FAST` that does
        the same thing. The macro is faster, but generates more code
        and does less checking.

    .. note::

        The :term:`client program` is responsible for synchronizing
        the access to the cache, but if the cache decides to access
        the pool, the MPS will properly synchronize with any other
        threads that might be accessing the same pool.

    .. note::

        Blocks allocated through a segregated allocation cache should
        only be freed through a segregated allocation cache with the
        same :term:`class structure`. Using :c:func:`mps_free` on them
        can cause :term:`memory leaks <memory leak>`, because the size
        of the block might be larger than you think. Naturally, the
        cache must also be attached to the same pool.


.. c:function:: MPS_SAC_ALLOC_FAST(mps_res_t res_o, mps_addr_t *p_o, mps_sac_t sac, size_t size, mps_bool_t has_reservoir_permit)

    A macro alternative to :c:func:`mps_sac_alloc` that is faster than
    the function but does less checking. The macro takes an additional
    first argument, *res_o*, which must be an lvalue that will store
    the :term:`result code`, and it doesn't evaluate
    *has_reservoir_permit* unless it decides to access the pool.

    .. seealso::

        :ref:`topic-cache`.


.. c:macro:: MPS_SAC_CLASS_LIMIT

    The number of :term:`size classes <size class>` that
    :c:func:`mps_sac_create` is guaranteed to accept.

    .. seealso::

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

    .. seealso::

        :ref:`topic-cache`.

    .. note::

        The :term:`client program` is responsible for synchronizing
        the access to the cache, but if the cache decides to access
        the pool, the MPS will properly synchronize with any other
        threads that might be accessing the same pool.

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

    .. seealso::

        :ref:`topic-cache`.


.. c:function:: MPS_SCAN_BEGIN(mps_ss_t ss)

    Within a :term:`scan method`, set up local information required
    by :c:func:`MPS_FIX1`, :c:func:`MPS_FIX2` and
    :c:func:`MPS_FIX12`. The local information persists until
    :c:func:`MPS_SCAN_END`.

    *ss* is the :term:`scan state` that was passed to the scan method.

    .. seealso::

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

    .. seealso::

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


------------------------
Declared in ``mpsacl.h``
------------------------

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

    *size* is its :term:`size` in bytes.

    If the block is too small to hold the internal arena structures,
    :c:func:`mps_arena_create` returns :c:macro:`MPS_RES_MEMORY`. In
    this case, you should allocate a (much) larger block, and try
    again.

    .. seealso::

        :ref:`topic-arena`.


------------------------
Declared in ``mpsavm.h``
------------------------

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

    .. seealso::

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

    .. seealso::

        :ref:`topic-arena`.


-------------------------
Declared in ``mpscamc.h``
-------------------------

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

    The function *f* will be called on both :term:`data <data object>`
    and :term:`padding objects <padding object>`. It is the job of *f* to
    distinguish, if necessary, between the two. It may also be called
    on :term:`dead` objects that the collector has not recycled or has
    been unable to recycle.

    The function *f* may not allocate memory or access any
    automatically-managed memory except within *object*.

    .. seealso::

        :ref:`topic-scanning`.

    .. note::

        There is no equivalent function for other pool classes, but
        there is a more general function
        :c:func:`mps_arena_formatted_objects_walk` that visits all
        formatted objects in the arena.


.. c:function:: mps_class_t mps_class_amc(void)

    Return the :term:`pool class` for an AMC (Automatic Mostly
    Copying) :term:`pool`.

    When creating an AMC pool, :c:func:`mps_pool_create` takes one
    extra argument::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_amc(),
                                  mps_fmt_t fmt)

    *fmt* specifies the :term:`object format` for the objects
    allocated in the pool.

    .. seealso::

        :ref:`pool-amc`.


--------------------------
Declared in ``mpscmvff.h``
--------------------------

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

    .. seealso::

        :ref:`pool-mvff`.


-------------------------
Declared in ``mpscmv2.h``
-------------------------

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
    mean, and maximum (typical) size in bytes of blocks expected to be
    allocated in the pool. Blocks smaller than *minimum_size* and
    larger than *maximum_size* may be allocated, but the pool is not
    guaranteed to manage them space-efficiently. Furthermore, partial
    freeing is not supported for blocks larger than *maximum_size*;
    doing so will result in the storage of the block never being
    reused. *mean_size* need not be an accurate mean, although the
    pool will manage *mean_size* blocks more efficiently if it is.

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

    .. seealso::

        :ref:`pool-mvt`


-------------------------
Declared in ``mpscsnc.h``
-------------------------

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

    .. seealso::

        :ref:`pool-snc`.


------------------------
Declared in ``mpslib.h``
------------------------

.. c:function:: int mps_lib_memcmp(const void *s1, const void *s2, size_t n)

    A :term:`plinth` function similar to the standard C function
    ``memcmp``.

    *s1* and *s2* point to :term:`blocks <block>` of memory to be
    compared.

    *n* is the :term:`size` of the blocks, in bytes.

    Returns an integer that is greater than, equal to, or less than
    zero, accordingly as the block pointed to by *s1* is greater than,
    equal to, or less than the block pointed to by *s2*.

    This function is intended to have the same semantics as the
    ``memcmp`` function of the [ANSI C Standard]_ (section 7.11.4.1).

    .. seealso::

        :ref:`topic-plinth`.


.. c:function:: void *mps_lib_memcpy(void *dest, const void *source, size_t n)

    A :term:`plinth` function similar to the standard C function
    ``memcpy``.

    *dest* points to the destination.

    *source* points to the source.

    *n* is the number of bytes to copy from *source* to *dest*.

    Returns *dest*.

    This function is intended to have the same semantics as the
    ``memcpy`` function of the [ANSI C Standard]_ (section 7.11.2.1).

    The MPS never passes overlapping blocks to
    :c:func:`mps_lib_memcpy`.

    .. seealso::

        :ref:`topic-plinth`.

.. c:function:: void *mps_lib_memset(void *s, int c, size_t n)

    A :term:`plinth` function similar to the standard C function
    ``memset``.

    *s* points to the :term:`block` to fill with the byte *c*.

    *c* is the byte to fill with (when converted to ``unsigned char``).

    *n* is the :term:`size` of the block in bytes.

    Returns *s*.

    This function is intended to have the same semantics as the
    ``memset`` function of the [ANSI C Standard]_ (section 7.11.6.1).

    .. seealso::

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

    .. seealso::

        :ref:`topic-plinth`, :ref:`topic-telemetry`.


-----------------------
Declared in ``mpstd.h``
-----------------------

.. c:type:: MPS_T_WORD

    An unsigned integral type that is the same size as an
    :term:`object pointer`, so that ``sizeof(MPS_T_WORD) ==
    sizeof(void*)``.

    The exact identity of this type is platform-dependent. Typical
    identities are ``unsigned long`` and ``unsigned __int_64``.

    .. seealso::

        :ref:`topic-platform`.


.. c:macro:: MPS_WORD_SHIFT

    The logarithm to base 2 of the constant :c:macro:`MPS_WORD_WIDTH`,
    so that ``1 << MPS_WORD_SHIFT == MPS_WORD_WIDTH``.

    The value is platform-dependent. Typical values are 5 and 6.

    .. seealso::

        :ref:`topic-platform`.


.. c:macro:: MPS_WORD_WIDTH

    The width in bits of the type :c:type:`MPS_T_WORD`, so that
    ``MPS_WORD_WIDTH == sizeof(MPS_T_WORD) * CHAR_BIT``.

    This value is platform-dependent. It is always a power of 2:
    typical values are 32 and 64.

    .. seealso::

        :ref:`topic-platform`.


-------------
Other symbols
-------------


.. c:function:: size_t mps_message_gc_condemned_size(mps_arena_t arena, mps_message_tmessage)


<h4>Summary</h4>

:c:func:`mps_message_gc_condemned_size` returns the "condemned size" property of the specified message in the specified arena.


<h4>Associated Protocols</h4>

Message, GC.


<h4>Arguments</h4>

<code class="source"> arena</code>-- the arena


  <code class="source">
    message</code>-- a message of a message type that supports this method



<h4>Returned Values</h4>

An approximate size for the set of objects condemned in the collection that generated the message.


<h4>Resources</h4>


  <code class="filename">mps.h</code>



<h4>Description</h4>

Currently, the only type of message that supports this property is :c:type:`mps_message_type_gc`, such messages are generated whenever a garbage collection completes. This method returns an approximation to the size of the set of objects that were condemned in that collection.


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>



<code>mps_message_*</code>


<h3>function <code><a id="mps_message_gc_live_size" name="mps_message_gc_live_size">mps_message_gc_live_size</a></code></h3>


<h4>Summary</h4>

:c:func:`mps_message_gc_live_size` returns the "live size" property of the specified message in the specified arena.


<h4>Associated Protocols</h4>

Message, GC.


<h4>Syntax</h4>

<code class="source"> size_t mps_message_gc_live_size(mps_arena_t arena, mps_message_t message)</code>


<h4>Arguments</h4>

<code class="source">arena</code> -- the arena;

<code class="source">message</code> -- a message of a message type that supports this method.


<h4>Returned Values</h4>

The total size of the condemned objects that survived the collection that generated the message.


<h4>Resources</h4>


  <code class="filename">mps.h</code>



<h4>Description</h4>

Currently, the only type of message that supports this property is :c:type:`mps_message_type_gc`, such messages are generated whenever a garbage collection completes. This method returns the size of the set of objects that were condemned in that collection, but survived.


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>



<code>mps_message_*</code>


<h3>function <code><a id="mps_message_gc_not_condemned_size" name="mps_message_gc_not_condemned_size">mps_message_gc_not_condemned_size</a></code></h3>


<h4>Summary</h4>

<code class="source">mps_message_gc_not_condemned_size</code> returns the "not condemned size" property of the specified message in the specified arena.


<h4>Associated Protocols</h4>

Message, GC.


<h4>Syntax</h4>

<code class="source">size_t mps_message_gc_not_condemned_size(mps_arena_t arena, mps_message_t message)</code>


<h4>Arguments</h4>


  <code class="source">arena</code> -- the arena



  <code class="source">message</code> -- a message of a message type that supports this method



<h4>Returned Values</h4>

An approximate size for the set of objects that were in collected pools, but were not condemned in the collection that generated the message.


<h4>Resources</h4>

<code class="filename">mps.h</code>


<h4>Description</h4>

Currently, the only type of message that supports this property is :c:type:`mps_message_type_gc`; such messages are generated whenever a garbage collection completes. This method returns an approximation to the size of the set of objects that were in collected pools (so potentially subject to garbage collection), but were not condemned in that collection.


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>



<code>mps_message_*</code>


<h3>function <code><a id="mps_message_gc_start_why"
  name="mps_message_gc_start_why">mps_message_gc_start_why</a></code></h3>


<h4>Summary</h4>

<code class="source">mps_message_gc_start_why</code> returns the a string that describes why a particular collection started. 


<h4>Associated Protocols</h4>

Message, GC.


<h4>Syntax</h4>

<code class="source">const char * mps_message_gc_start_why(mps_arena_t arena, mps_message_t message)</code>


<h4>Arguments</h4>

<code class="source">arena</code> -- the arena

<code class="source">message</code> -- a message of a message type that supports this method (<code>mps_message_type_gc_start()</code>)


<h4>Returned Values</h4>

 A pointer to a string that is a   textual explanation of why this collection is starting. 


<h4>Resources</h4>

<code class="filename">mps.h</code>


<h4>Description</h4>

Currently, the only type of message that supports this property is :c:type:`mps_message_type_gc_start`; such messages are generated whenever a garbage collection starts. This method returns a string describing why the collection started. 

 The contents of the string must not be modified by the client.  The string and the pointer are only valid as long as the message has not been discarded (with <code>mps_message_discard</code>). 

<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>



<code>mps_message_*</code>


.. c:function:: mps_bool_t mps_message_get(mps_message_t *message_return, mps_arena_t arena, mps_message_type_tmessage_type)


<h4>Summary</h4>

Gets a message of the specified type from a message queue.


<h4>Associated Protocols</h4>

Message.


<h4>Arguments</h4>


  <code>message_return</code>
  -- the handle to the message that was removed from the queue



  <code>arena</code>
  -- the arena



  <code>message_type</code>
  -- the type of message



<h4>Returned Values</h4>

Returns true if a message has been removed from the queue, false if not.


<h4>Description</h4>


  If there is a message of the specified type on the message queue of the specified arena,then this function removes one such message from the queue, returns a handle to it via the<code>message_return</code> argument, and returns true. Otherwise it returns false.



<h4>Example</h4>


<h4>See Also</h4>



<code>mps_message_*</code>


<h3><code><a id="mps_message_poll" name="mps_message_poll">mps_message_poll</a></code></h3>


<h4>Summary</h4>

:c:func:`mps_message_poll` determines whether there are currently any messages on a message queue.


<h4>Associated Protocols</h4>

Message.


<h4>Syntax</h4>

<code>mps_bool_t mps_message_poll(mps_arena_t arena)</code>


<h4>Arguments</h4>


  <code>arena</code>
  -- the arena whose message queue you are interested in



<h4>Returned Values</h4>

A flag to indicate whether there are any messages on the queue.


<h4>Description</h4>

:c:func:`mps_message_poll` is used to determine whether there are currently any messages on the message queue of the specified arena.


<h4>Example</h4>

[missing]


<h4>Error Handling</h4>

Can't fail.


<h4>See Also</h4>



:c:func:`mps_message_get`


<h4>Notes</h4>

If you expect a particular type of message, it is usually more practical to just call :c:func:`mps_message_get`.


.. c:function:: mps_bool_t mps_message_queue_type(mps_message_type_t *message_type_return, mps_arena_t arena)


<h4>Summary</h4>

:c:func:`mps_message_queue_type` returns the type of the first message on a message queue.


<h4>Associated Protocols</h4>

Message.


<h4>Arguments</h4>

message_type_return -- the type of the first message on the queue of the specified arena

arena -- the arena


<h4>Returned Values</h4>

"True" if there are any messages on the queue of the specified arena, "false" if not.


<h4>Description</h4>

If there are any messages on the queue of the specified arena, then this function returns"true", and also returns the type of the first message via "message_type_return". Otherwise it returns "false".


<h4>Example</h4>


<h4>See Also</h4>



<code>mps_message_*</code>


.. c:type:: mps_message_t


<h4>Summary</h4>

:c:func:`mps_message_t` is used as a handle on an individual message.


<h4>Associated Protocols</h4>

Message.


<h4>Type</h4>

<code>typedef struct mps_message_s *mps_message_t</code>

:c:func:`mps_message_s` is an incomplete structure type used only to declare the opaque type :c:func:`mps_message_t`.


<h4>Description</h4>

The opaque type :c:func:`mps_message_t` is used as a handle on an individual message. Messages are manually managed. They are created at the instigation of the MPS (but see :c:func:`mps_message_type_enable`), and are deleted by the client.

An :c:func:`mps_message_t` is a reference into MPS managed memory, and can safely be stored as such in scannable memory.


<h4>Example</h4>


<h4>Error Handling</h4>

Not applicable.


<h4>See Also</h4>



<code>mps_message_*</code>


.. c:function:: mps_message_type_t mps_message_type(mps_arena_t arena, mps_message_t message)


<h4>Summary</h4>

:c:func:`mps_message_type` returns the type of a message.


<h4>Associated Protocols</h4>

Message.


<h4>Arguments</h4>

arena -- the arena containing the message

message -- a valid message; that is, one previously returned by :c:func:`mps_message_get`, and notdiscarded via :c:func:`mps_message_discard`


<h4>Returned Values</h4>

The type of the specified message.


<h4>Description</h4>

:c:func:`mps_message_type` returns the type of a message.


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>



:c:func:`mps_message_clock`


<h3>function <code><a id="mps_message_type_disable" name="mps_message_type_disable">mps_message_type_disable</a></code></h3>


<h4>Summary</h4>

:c:func:`mps_message_type_disable` restores the arena to the default state whereby messages of thespecified type are not generated.

This reverses the effect of an earlier call to "m ps_message_type_enable".


<h4>Associated Protocols</h4>

Message.


<h4>Syntax</h4>

<code>void mps_message_type_disable(mps_arena_t arena, mps_message_type_t message_type)</code>


<h4>Arguments</h4>

arena -- the arena

message_type -- the message type to be disabled



<h4>Description</h4>

This procedure may be used by the client to specify that messages of the specified type should not created for the specified arena.

Messages are not generated by default, but the client may enable the generation of messages with :c:func:`mps_message_type_enable`.

Any existing messages of the specified type are flushed from the message queue.


<h4>Example</h4>

[none]


<h4>Error Handling</h4>

Never fails.


<h4>See Also</h4>



<code>mps_message_*</code>


<h4>Notes</h4>

It is permitted to call this function when the message type is already disabled. Such a call will have no effect.


.. c:function:: void mps_message_type_enable(mps_arena_t arena, mps_message_type_t message_type)


<h4>Summary</h4>

:c:func:`mps_message_type_enable` allows messages of the specified type to be created for thespecified arena. Without such enabling, the MPS will, by default, not generate any messages of thattype.


<h4>Associated Protocols</h4>

Message.


<h4>Arguments</h4>

arena -- the arena

message_type -- the message type to be enabled



<h4>Description</h4>

This procedure may be used by the client to specify that messages of the specified type maybe created for the specified arena. Without such enabling, the MPS will by default not generate any messages of that type.

Note that the enabling of messages of a particular type implies that the client application will handle and discard message of that type, or the message queue may consume unbounded resources.

The client may disable message generation again by means of an equivalent call to :c:func:`mps_message_type_disable`.


<h4>Example</h4>

[none]


<h4>Error Handling</h4>

Never fails.


<h4>See Also</h4>



<code>mps_message_*</code>

"Message Protocol"


<h4>Notes</h4>

It is permitted to call this function when the message type is already enabled. Such a call will have no effect.




.. c:function:: mps_message_type_t mps_message_type_finalization(void)


<h4>Summary</h4>

:c:func:`mps_message_type_finalization` returns the type of finalization messages.


<h4>Associated Protocols</h4>

Message, Finalization.


<h4>Arguments</h4>

None.


<h4>Returned Values</h4>

The type of finalization messages.


<h4>Resources</h4>

Not applicable.


<h4>Description</h4>

:c:func:`mps_message_type_finalization` returns the type of finalization messages. Finalization messages are used by the MPS to implement finalization (see :c:func:`mps_finalize`).  When the MPS detects that an object is finalizable, it finalizes the object by posting a message of this type (note that there might be delays between the object becoming finalizable, the MPS detecting that, and the message being posted). 


In addition to the usual methods applicable to messages, finalization
messages support the :c:func:`mps_message_finalization_ref`
method which returns a reference to the object that was registered for
finalization.


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

<h4>See Also</h4>



<code>mps_message_*</code>,

:c:func:`mps_finalize`


<h3>function <code><a id="mps_message_type_gc" name="mps_message_type_gc">mps_message_type_gc</a></code></h3>


<h4>Summary</h4>

:c:type:`mps_message_type_gc` returns the type of garbage collection statistic messages.


<h4>Associated Protocols</h4>

Message.


<h4>Syntax</h4>

<code class="source">mps_message_type_t mps_message_type_gc(void)</code>


<h4>Arguments</h4>

None.


<h4>Returned Values</h4>

The type of garbage collection statistic messages.


<h4>Resources</h4>


  <code class="filename">mps.h</code>



<h4>Description</h4>

:c:type:`mps_message_type_gc` returns the type of garbage collection statistic messages. Garbage collection statistic messages are used by the MPS to give the client information about garbage collections that have occurred. Such information may be useful in analysing the client's memory usage over time.

The access methods specific to a message of this type are:

<ul>

  <li><code class="source">mps_message_gc_live_size</code> -- gives the total size of the condemned objects that survived the collection that generated the message</li>

  <li><code class="source">mps_message_gc_condemned_size</code>
  -- gives an approximate size for the set of objects condemned in the collection that generated the message.</li>

  <li><code class="source">mps_message_gc_not_condemned_size</code> -- gives an approximate size for the set of objects that were in collected pools, but were not condemned in the collection that generated the message.</li>

</ul>


<h4>Example</h4>

<pre>
{
  mps_message_t message;
  if(mps_message_get(&amp;message, arena, mps_message_type_gc())) {
    size_t live, condemned, not_condemned;
    live = mps_message_gc_live_size(arena, message);
    condemned = mps_message_gc_condemned_size(arena, message);
    not_condemned = mps_message_gc_not_condemned_size(arena,message);
    mps_message_discard(arena, message);
    process_collection_stats(live, condemned, not_condemned);
  }
}
</pre>


<h4>Error Handling</h4>

Cannot fail.


<h4>See Also</h4>



<code>mps_message_*</code>.


<h3>function <code><a id="mps_message_type_gc_start"
name="mps_message_type_gc_start">mps_message_type_gc_start</a></code></h3>


<h4>Summary</h4>


:c:type:`mps_message_type_gc_start`
returns the type of garbage collection start messages.


<h4>Associated Protocols</h4>

Message.


<h4>Syntax</h4>

<code class="source">mps_message_type_t mps_message_type_gc_start(void)</code>


<h4>Arguments</h4>

None.


<h4>Returned Values</h4>

The type of garbage collection start messages.


<h4>Resources</h4>


  <code class="filename">mps.h</code>



<h4>Description</h4>


:c:type:`mps_message_type_gc_start`
returns the type of garbage collection start messages.
The messages contain information about why the collection started. See
<code>mps_message_gc_start_why</code>.


The access methods specific to a message of this type are:

<ul>

  <li><code class="source">mps_message_gc_start_why</code> --
  Returns a string that is a description of why the collection started.
  </li>

</ul>


<h4>Example</h4>

<pre>
{
  mps_message_t message;
  if(mps_message_get(&amp;message, arena, mps_message_type_gc_start())) {
    printf("Collection started; reason: %s\n",
      mps_message_gc_start_why(arena, message));
  }
}
</pre>


<h4>Error Handling</h4>

Cannot fail.


<h4>See Also</h4>



<code>mps_message_*</code>.


.. c:type:: mps_message_type_t


<h4>Summary</h4>

:c:func:`mps_message_type_t` is the type of message types.


<h4>Associated Protocols</h4>

Message.


<h4>Description</h4>

:c:func:`mps_message_type_t` is the type whose values are the various message types. It is opaque.


<h4>Example</h4>


<h4>See Also</h4>



<code>mps_message_*</code>


.. c:function:: void mps_pool_check_fenceposts(mps_pool_t pool)


<h4>Summary</h4>

Check all the fenceposts in the pool.


<h4>Associated Protocols</h4>

Debug


<h4>Arguments</h4>

pool the pool whose fenceposts are to be checked


<h4>Description</h4>

This function is a debugging feature to check all the fenceposts in the pool. If a corrupted fencepost is found, an assert will fire. It is only useful to call this on a debug pool that had fenceposting turned, it does nothing on other pools.


<h4>Example</h4>

<code>mps_pool_check_fenceposts(gene_pool);</code>


<h4>Error Handling</h4>

If a corrupted fencepost is found, an assert will fire. You will probably want to look at the problem with a debugger.


<h4>See Also</h4>



<code>mps_class_*_debug</code>


<h3>structure <code><a id="mps_pool_debug_option_s" name="mps_pool_debug_option_s">mps_pool_debug_option_s</a></code></h3>


<h4>Summary</h4>

This structure is used to pass debug options to :c:func:`mps_pool_create` for debug classes.


<h4>Associated Protocols</h4>

Debug.


<h4>Type</h4>

<pre>
typedef struct mps_pool_debug_option_s {
  void *fence_template;
  size_t fence_size;
} mps_pool_debug_option_s;
</pre>


<h4>Members</h4>


  <code>fence_template</code>
  the template for fencepost contents



  <code>fence_size</code>
  the size of the template in bytes



<h4>Description</h4>

Structures of this type are used to pass debug options to :c:func:`mps_pool_create` when creating instances of debug classes.

Fenceposting is enabled by specifying a non-zero <code>fence_size</code>; the size must be a multiple of the [pool/format] alignment. The content of fenceposts is given as a template that is simply copied onto each fencepost (although sometimes the MPS will create fenceposts smaller than the given size, for example, to pad out some bit that was left unused because of alignmentrequirements).


<h4>Example</h4>

<pre>
static mps_pool_debug_option_s debugOptions = { (void *)"postpost", 8 };
if(mps_pool_create(&amp;pool, arena, mps_class_ams_debug(),
                   &amp;debugOptions, 8192, 135, 8)
   != MPS_RES_OK) {
  printf("Error creating pool!"); exit(2);
}
</pre>


<h4>See Also</h4>



:c:func:`mps_pool_check_fenceposts`


<h4>Notes</h4>

Fencepost templates allow the client to specify complicated patterns that mimic illegal datavalues, that would cause an assert to fire if read by mistake, and that would never be written by any operation that writes at the wrong address by mistake.

Another trick is to make the pattern contain an instruction sequence that would cause theprogram to error or stop if executed by mistake.


<h3>function <code><a id="mps_rank_ambig" name="mps_rank_ambig">mps_rank_ambig</a></code></h3>


<h4>Summary</h4>

Function returning the value representing "rank ambig".


<h4>Associated Protocols</h4>

Allocation, Root, Scanning.


<h4>Syntax</h4>

<code>mps_rank_ambig()</code>


<h4>Type</h4>

<code>mps_rank_t mps_rank_ambig(void)</code>


<h4>Arguments</h4>

None.


<h4>Returned Values</h4>

Returns a value of type :c:func:`mps_rank_t` representing "rank ambig".


<h4>Description</h4>

Used to get a value for "rank ambig", which is used to denote that certain references (in a root, for example) are ambiguous references.


<h4>Example</h4>


<h4>See Also</h4>



:c:func:`mps_rank_t`,

:c:func:`mps_rank_exact`


.. c:function:: mps_rank_t mps_rank_exact(void);


<h4>Summary</h4>

Used to declare references which the client wishes to be exact references.


<h4>Associated Protocols</h4>

Allocation, Root, Scanning.


<h4>Arguments</h4>

No arguments.


<h4>Returned Values</h4>

Returns a rank (see :c:func:`mps_rank_t`) which can be used to declare references to be exact references.


<h4>Description</h4>

Used to declare references which the client wishes to be exact, non-weak references.


<h4>Example</h4>

[missing]


<h4>See Also</h4>



:c:func:`mps_rank_t`,

:c:func:`mps_rank_ambig`,

:c:func:`mps_rank_weak`


.. c:type:: mps_rank_t


<h4>Summary</h4>

A type whose values are "reference ranks".


<h4>Associated Protocols</h4>

Allocation, Root.


<h4>Type</h4>

<code>typedef unsigned int mps_rank_t;</code>


<h4>Description</h4>

:c:func:`mps_rank_t` is a concrete type. It is an alias (via the C typedef mechanism) for "unsigned int" provided for convenience and clarity. An object of type :c:func:`mps_rank_t` can store a value representing one reference rank. Reference ranks are used to conveniently express specific semantics of particular references. See "MPS Scanning Protocol" for descriptions of these semantics, and <code>mps_rank_*</code> for the actual ranks used to declare these semantics.


<h4>Example</h4>

(Probably won't be used explicitly, most likely to be seen in the prototype declaration for other MPS functions. For example, :c:func:`mps_root_create`.)


<h4>See Also</h4>



<code>mps_rank_*</code>


.. c:function:: extern mps_rank_t mps_rank_weak(void);


<h4>Summary</h4>

Function to return a value used to represent "rank weak".


<h4>Associated Protocols</h4>

Allocation, Scanning.


<h4>Arguments</h4>

None.


<h4>Returned Values</h4>

Returns a value of type :c:func:`mps_rank_t` that represent "rank weak".


<h4>Description</h4>

:c:func:`mps_rank_weak` returns a value used to represent "rank weak".

"Rank weak" is often used to denote that certain references (in a root or in objects allocated in a pool) are weak references.


<h4>Example</h4>

&lt;example of how to use the symbol&gt;


<h4>See Also</h4>



:c:func:`mps_rank_t`,

:c:func:`mps_rank_exact`


.. c:type:: mps_reg_scan_t


<h4>Summary</h4>

Type of root scanning functions for :c:func:`mps_root_create_reg`.


<h4>Associated Protocols</h4>

Root.


<h4>Syntax</h4>

<code>typedef mps_res_t (*mps_reg_scan_t)( mps_ss_t scan_state, mps_thr_t thread, void *p, size_t s)</code>


<h4>Arguments</h4>

scan_state a scan state

thread the thread

p a value passed through from root registration

s a value passed through from root registration


<h4>Returned Values</h4>

A result code.


<h4>Description</h4>

This is the type of root scanning functions the client provides to :c:func:`mps_root_create_reg`.These functions will be called, whenever the root needs to be scanned, and passed the "p" and "s"values specified in the call to :c:func:`mps_root_create_reg`.


<h4>See Also</h4>



:c:func:`mps_root_create_reg`,

:c:func:`mps_stack_scan_ambig`


<h4>Notes</h4>

Users are not expected to write any scanning functions of this type. The one function supplied with the MPS, :c:func:`mps_stack_scan_ambig`, should be enough for most purposes.


.. c:type:: mps_res_t


<h4>Summary</h4>

:c:func:`mps_res_t` is the type of result codes returned by operations that may fail.


<h4>Type</h4>

<code>typedef int mps_res_t;</code>


<h4>Description</h4>

A result code indicates the success or failure of an operation, along with the reason for failure. Like UNIX error codes, the meaning of the code depends on the call that returned it. Refer to the documentation of the function for the exact meaning. This documentation describes the broad categories with mnemonic names for various sorts of problems.

<code><a id="MPS_RES_OK" name="MPS_RES_OK">MPS_RES_OK</a></code>: The operation succeeded. Out and in/out parameters will only be updated if OK is returned, otherwise they will be left untouched. :c:macro:`MPS_RES_OK` is zero.

<code><a id="MPS_RES_FAIL" name="MPS_RES_FAIL">MPS_RES_FAIL</a></code>: Something went wrong that does not fall into any of the other categories. The exact meaning depends on the call. See the documentation of the function.

<code><a id="MPS_RES_RESOURCE" name="MPS_RES_RESOURCE">MPS_RES_RESOURCE</a></code>: A needed resource could not be obtained. Which resource, depends on the call. Compare with :c:macro:`MPS_RES_MEMORY`, which is a special case of this.

<code><a id="MPS_RES_MEMORY" name="MPS_RES_MEMORY">MPS_RES_MEMORY</a></code>: Needed memory (committed memory, not address space) could not be obtained. (A <a href="#MPS_RES_MEMORY_detailed">more detailed explanation</a>).

<code><a id="MPS_RES_LIMIT" name="MPS_RES_LIMIT">MPS_RES_LIMIT</a></code>: An internal limitation was reached. For example, the maximum number of something was reached. (A <a href="#MPS_RES_LIMIT_detailed">more detailed explanation</a>).

<code><a id="MPS_RES_UNIMPL" name="MPS_RES_UNIMPL">MPS_RES_UNIMPL</a></code>: The operation, or some vital part of it, is unimplemented. This might be returned by functions that are no longer supported, or by operations that are included for future expansion, but not yet supported.

<code><a id="MPS_RES_IO" name="MPS_RES_IO">MPS_RES_IO</a></code>: An I/O error occurred. Exactly what depends on the function.

<code><a id="MPS_RES_COMMIT_LIMIT" name="MPS_RES_COMMIT_LIMIT">MPS_RES_COMMIT_LIMIT</a></code>: The arena's commit limit would have been exceeded as a result of (explicit or implicit) allocation. See protocol.arena.commit.

<code><a id="MPS_RES_PARAM" name="MPS_RES_PARAM">MPS_RES_PARAM</a></code>: A parameter of the operation was invalid. (A <a href="#MPS_RES_PARAM_detailed">more detailed explanation</a>).


Any function that might fail will return a result code. Any other results of the function are passed back in "return" parameters. See MPS Interface Conventions for more information.


<h4>Example</h4>

<pre>
mps_addr_t p;
mps_res_t res;

res = mps_alloc(&amp;p, pool, sizeof(struct spong));
if(res != MPS_RES_OK) {
  handle_memory_error(res);
  abort();
}
</pre>

For more examples, s ee doc.mps.ref-man.if-conv.


<h4>See Also</h4>



<code>MPS_RES_*</code>


.. c:function:: mps_res_t mps_root_create(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_trm, mps_root_scan_t scan, void *p, size_t s)


<h4>Summary</h4>

The function :c:func:`mps_root_create` declares a root that consists of all the references indicated by a scanning function.


<h4>Associated Protocols</h4>

Root.


<h4>Arguments</h4>

root_o a pointer to a variable to store the new root structure

arena the arena

rank the rank of references in the root

rm the root mode

scan the scanning function

p a value to be passed to the scanning function

s a value to be passed to the scanning function


<h4>Returned Values</h4>

If the return value is :c:macro:`MPS_RES_OK`, a new root structure in "*root_o".


<h4>Description</h4>

The client provides a scanning function, that will be called with a scan state and "p" and"s", whenever the root needs to be scanned. See :c:func:`mps_root_scan_t` for details.

If the rank of the root is not :c:macro:`MPS_RANK_AMBIG`, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.


<h4>Example</h4>

<pre>
static mps_root_t mmRoot;

int main(void)
{
  mps_res_t res;

  /* ... */

  res = mps_root_create(&amp;mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                        &amp;rootScanner, NULL, 0);
  /* see doc of mps_root_scan_t for definition of rootScanner */
  if(res != MPS_RES_OK)
    exit(1);

  /* ... */
}
</pre>


<h4>Error Handling</h4>

:c:func:`mps_root_create` returns :c:macro:`MPS_RES_MEMORY` when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space, or expand the arena.


<h4>See Also</h4>



:c:func:`mps_root_scan_t`,

:c:func:`mps_rm_t`,

:c:func:`mps_rank_t`,

:c:func:`mps_root_t`,

:c:func:`mps_root_create_fmt`,

:c:func:`mps_root_create_table`,

:c:macro:`MPS_RM_CONST`


<h4>Notes</h4>

"p" and "s" are just arbitrary data that scanning function can use. This is needed because Clacks local functions.


.. c:function:: mps_res_t mps_root_create_fmt(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_fmt_scan_t scan, mps_addr_t base, mps_addr_t limit)


<h4>Summary</h4>

The function :c:func:`mps_root_create_fmt` declares a root that consists of a block of objects, and provides a scanning function for them.


<h4>Associated Protocols</h4>

Root.


<h4>Arguments</h4>

root_o a pointer to a variable to store the new root structure

arena the arena

rank the rank of references in the root

rm the root mode

scan the scanning function

base the address of the start of the root

limit the address just beyond the end of the root


<h4>Returned Values</h4>

If the return value is :c:macro:`MPS_RES_OK`, the new root in "*root_o".


<h4>Description</h4>

The client provides a scanning function, that will be called with a scan state and an area of memory, whenever the root needs to be scanned. See :c:type:`mps_fmt_scan_t` for details.

If the rank of the root is not :c:macro:`MPS_RANK_AMBIG`, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.


<h4>Example</h4>

<pre>
static mps_root_t mmRoot;
SegmentDescriptor DataSegment;

int main(void)
{
  mps_res_t res;

  /* ... */

  res = mps_root_create_fmt(&amp;mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
    &amp;scan_objs,
    (mps_addr_t)DataSegment.base,
    (mps_addr_t) (DataSegment.base + SegmentLength) );

  /* see doc of mps_fmt_scan_t for definition of scan_objs */

  if(res != MPS_RES_OK)
    exit( EXIT_FAILURE );

  /* ... */
}
</pre>


<h4>Error Handling</h4>

:c:func:`mps_root_create_fmt` returns :c:macro:`MPS_RES_MEMORY` when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space, or expand the arena.


<h4>See Also</h4>



:c:type:`mps_fmt_scan_t`,

:c:func:`mps_rm_t`,

:c:func:`mps_rank_t`,

:c:func:`mps_root_t`,

:c:func:`mps_root_create`,

:c:func:`mps_root_create_table`,

:c:macro:`MPS_RM_PROT`,

:c:macro:`MPS_RM_CONST`


<h4>Notes</h4>

This is like :c:func:`mps_root_create_table`, except you get to supply your own scanning function.This is like :c:func:`mps_root_create`, except the scanning function has a slightly different argument list(and the MPS knows where the root is).


.. c:function:: mps_res_t mps_root_create_reg(mps_root_t * root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_thr_t thread, mps_reg_scan_t scan, void *p, size_t s)


<h4>Summary</h4>

:c:func:`mps_root_create_reg` registers a thread as a root.


<h4>Associated Protocols</h4>

Root.


<h4>Arguments</h4>

root_o a pointer to a variable to store the new root structure

arena the arena

rank the rank of references in the root

rm the root mode

thread the thread to the registered as a root

scan the scanning function

p a value to be passed to the scanning function

s a value to be passed to the scanning function


<h4>Returned Values</h4>

If the return value is :c:macro:`MPS_RES_OK`, a new root structure in "*root_o".


<h4>Description</h4>

:c:func:`mps_root_create_reg` declares the state of a thread as a root. The client provides a scanning function that will be called and passed "p" and "s", whenever the root needs to be scanned. See :c:func:`mps_reg_scan_t` for details.

If the rank of the root is not :c:macro:`MPS_RANK_AMBIG`, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.


<h4>Example</h4>

<pre>
typedef struct {
  mps_root_t mmRoot;
  mps_thr_t thread;
  /* ...  */
} ThreadLocals;

void InitThread(ThreadLocals *thr)
{
  /* This is a hack to find the bottom of the stack. */
  void *stackBottom=&amp;stackBottom;

  mps_thread_reg(&amp;thr-&gt;thread, arena);
  mps_root_create_reg(&amp;thr-&gt;mmRoot, arena, MPS_RANK_AMBIG, (mps_rm_t) 0,
    thr-&gt;thread, mps_stack_scan_ambig, stackBottom, 0);

  /* ...  */

}
</pre>


<h4>Error Handling</h4>

:c:func:`mps_root_create_reg` returns :c:macro:`MPS_RES_MEMORY` when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space, or expand the arena.


<h4>See Also</h4>



:c:func:`mps_stack_scan_ambig`,

:c:func:`mps_reg_scan_t`


<h4>Notes</h4>

Only one suitable scanning function is supplied with the MPS, namely :c:func:`mps_stack_scan_ambig`.


.. c:function:: mps_res_t mps_root_create_table(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t size)


<h4>Summary</h4>

:c:func:`mps_root_create_table` create s a root that is a vector of references.


<h4>Associated Protocols</h4>

Root.


<h4>Arguments</h4>

root_o a pointer to a variable for storing the new root structure in

arena the arena

rank the rank of the references in this root

rm the root mode

base a pointer to the vector of references that is being registered

size the number of references in the vector being registered


<h4>Returned Values</h4>

If the return value is :c:macro:`MPS_RES_OK`, the new root in "*root_o".


<h4>Description</h4>

This function declares a root that is a vector of references.

If the rank of the root is not :c:macro:`MPS_RANK_AMBIG`, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.


<h4>Example</h4>

<pre>
static mps_root_t mmRoot;
Object *Objects[rootCOUNT];

int main(void)
{
  mps_res_t res;

  /* ... */

  res = mps_root_create_table(&amp;mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                              (mps_addr_t) &amp;Objects, rootCOUNT );

  if(res != MPS_RES_OK)
    exit(1);

  /* ... */
}
</pre>


<h4>Error Handling</h4>

:c:func:`mps_root_create_table` returns :c:macro:`MPS_RES_MEMORY` when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space, or expand the arena.


<h4>See Also</h4>



:c:func:`mps_root_create_table_masked`,

:c:macro:`MPS_RM_PROT`,

:c:macro:`MPS_RM_CONST`


.. c:function:: mps_res_t mps_root_create_table_masked(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t size, mps_word_t mask);


<h4>Summary</h4>

:c:func:`mps_root_create_table_masked` creates a root that is a vector of tagged values.


<h4>Associated Protocols</h4>

Root.


<h4>Arguments</h4>

root_o a pointer to a variable for storing the new root structure in

arena the arena

rank the rank of the references in this root

rm the root mode

base a pointer to the vector of references that is being registered


  size the number of references in the vector being registered
  <br />
  mask any element that has any of the bits in mask set is ignored



<h4>Returned Values</h4>

If the return value is :c:macro:`MPS_RES_OK`, the new root in "*root_o".


<h4>Description</h4>

:c:func:`mps_root_create_table_masked` creates a root that is a table of tagged values. The mask parameter indicates which bits of a pointer are tag bits. References are assumed to have a tag of zero, values with other tags are ignored.

If the rank of the root is not :c:macro:`MPS_RANK_AMBIG`, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.


<h4>Example</h4>

<pre>
#define tagMASK 0x0003

static mps_root_t mmRoot;
Object *Objects[rootCOUNT];

int main(void)
{
  mps_res_t res;

  /* ... */

  res = mps_root_create_table_masked(&amp;mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                                     (mps_addr_t)&amp;Objects, rootCOUNT,
                                     (mps_word_t)tagMASK);
  if(res != MPS_RES_OK)
    exit(1);

  /* ... */
}
</pre>


<h4>Error Handling</h4>

:c:func:`mps_root_create_table_masked` returns :c:macro:`MPS_RES_MEMORY` when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space,or expand the arena.


<h4>See Also</h4>



:c:func:`mps_root_create_table`,

:c:macro:`MPS_RM_PROT`,

:c:macro:`MPS_RM_CONST`


.. c:type:: mps_root_scan_t


<h4>Summary</h4>

Type of root scanning functions for :c:func:`mps_root_create`.


<h4>Associated Protocols</h4>

Root.


<h4>Syntax</h4>

<code>typedef mps_res_t (*mps_root_scan_t)(mps_ss_t scan_state, void * p, size_t s)</code>


<h4>Arguments</h4>

<code>scan_state</code> a scan state

<code>p</code> an argument passed through from :c:func:`mps_root_create`

<code>s</code> an argument passed through from :c:func:`mps_root_create`


<h4>Returned Values</h4>

A result code.


<h4>Description</h4>

This is the type of root scanning functions the client provides to:c:func:`mps_root_create`. The MPS will call these functions whenever the root needs to be scanned, with a scan state (of type :c:func:`mps_ss_t` ), and the <code>p</code> and<code>s</code> values specified in the call to :c:func:`mps_root_create`. Apart from the argument list, the scanning function works like the format scan methods: it needs to indicate all references using :c:func:`mps_fix` or <code>MPS_FIX*</code>.


<h4>Example</h4>

<pre>
static StackFrame *stackBottom;

/* root scanner for an imaginary interpreter for a stack-oriented language */
static mps_res_t rootScanner(mps_ss_t ss, void * p, size_t s)
{
  StackFrame *frame;
  size_t i;
  mps_res_t res;

  UNUSED(p);
  UNUSED(s);

  for(frame = stackBottom; frame != NULL; frame = frame-&gt;next)
    for(i = frame-&gt;size; i &gt; 0; --i) {
      res = mps_fix(ss, &amp;frame-&gt;locals[i]);
      if(res != MPS_RES_OK) return res;
    }

  return res;
}
</pre>


<h4>Error Handling</h4>

If a fixing operation returns a value other than :c:macro:`MPS_RES_OK`, the scanning function must return that value, and may return without scanning further references. Generally, it is better if it returns as soon as possible. If the scanning is completed successfully, the function should return :c:macro:`MPS_RES_OK`.


<h4>See Also</h4>



:c:func:`mps_root_create`,

:c:func:`mps_ss_t`,

:c:func:`mps_fix`,

:c:macro:`MPS_SCAN_BEGIN`,

:c:macro:`MPS_SCAN_END`,

<code><a href="#MPS_FIX12">MPS_FIX12</a></code>,

<code><a href="#MPS_FIX1">MPS_FIX1</a></code>,

<code><a href="#MPS_FIX2">MPS_FIX2</a></code>,

:c:macro:`MPS_FIX_CALL`,

:c:type:`mps_fmt_scan_t`


.. c:type:: mps_roots_stepper_t


<h4>Summary</h4>

Type of the client-supplied root walker component.


<h4>Associated Protocols</h4>

None.


<h4>Type</h4>


  <code>typedef void (*mps_roots_stepper_t)( mps_addr_t *, mps_root_t, void *, size_t )</code>



<h4>Arguments</h4>

The function pointed to by an object of type :c:func:`mps_roots_stepper_t` takes the followingargument list:

<code>(mps_addr_t *ref, mps_root_t root, void *p, size_t s)</code>

ref is the address of a root which references an object in the arena. It's a pointer to a root which points to "something" in the client heap. That "something" will be an object if the root is an exact root. But it might be an interior pointer to an object if the root is an ambiguous root.

root is the MPS root object which contains ref.

p and s are two closure values which are copies of the corresponding values which the client passed into :c:func:`mps_arena_roots_walk`.


<h4>Returned Values</h4>

he function pointed to by an object of type :c:func:`mps_roots_stepper_t` returns no values.


<h4>Description</h4>

A pointer to a function is passed into the function :c:func:`mps_arena_roots_walk`; the pointer has this type. The root walker arranges to apply this function to all objects which are directly referenced from the roots.


<h4>Example</h4>

&lt;example of how to use the symbol&gt;


<h4>Error Handling</h4>


<h4>T</h4>

he function pointed to by an object of type :c:func:`mps_roots_stepper_t` has no way of signalling an error to the caller.


<h4>See Also</h4>



:c:func:`mps_arena_roots_arena_walk`


<h4>Notes</h4>


.. c:type:: mps_sac_class_s


<h4>Summary</h4>

A structure describing a size class to be passed as an argument to :c:func:`mps_sac_create`.


<h4>Associated Protocols</h4>

Allocation cache


<h4>Type</h4>

<pre>
typedef struct mps_sac_class_s {
  size_t mps_block_size;
  size_t mps_cached_count;
  unsigned mps_frequency;
} mps_sac_class_s;
</pre>


<h4>Description</h4>

:c:func:`mps_sac_class_s` is the element type of the array passed to:c:func:`mps_sac_create` to describe the size classes. Each element of this array describes one class by specifying <code>block_size</code>, the maximum size (in bytes) in this class; <code>cached_count</code>, the number of objects of this class to cache; and <code>frequency</code>, a number that describes the frequency of requests (allocation and deallocation combined ) in this class relative to all the other classes. The classes should be given in the order of ascending size.

<code>block_size</code> s have to be aligned to the pool alignment. All sizes must be different, and the smallest size must be large enough to hold a <code>void *</code>.


  <code>cached_count</code>
  is advice to the MPS on how many blocks to cache, not an absolute limit. The cache policy tries to accommodate fluctuations in the population and minimize the cost of responding to client requests; the purpose of this parameter is to limit how much memory the client is willing to set aside for this purpose. However, a
  <code>cached_count</code>
  of zero prevents any caching of blocks falling into that class.


The MPS automatically provides an "overlarge" class for arbitrarily large objects above the largest class described. Allocations falling into the overlarge class are not cached.


<h4>Example</h4>

<pre>
  mps_sac_t sac;
  mps_sac_class_s classes[3] = { {8, 38, 1}, {136, 19, 3}, {512, 4, 1} };

  res = mps_sac_create(&amp;sac, pool, 3, classes);
  if (res != MPS_RES_OK) {
    printf("Failed to create the allocation cache!");
    exit(1);
  }
</pre>


<h4>See Also</h4>



:c:func:`mps_sac_create`


<h4>Notes</h4>

Any blocks whose size falls between two classes are allocated from the larger class.


.. c:function:: mps_res_t mps_sac_create(mps_sac_t *sac_o, mps_pool_t pool, size_t classes_count, mps_sac_class_s *classes);


<h4>Summary</h4>

This function creates a segregated allocation cache.


<h4>Associated Protocols</h4>

Allocation cache


<h4>Arguments</h4>

sac_o a pointer to a variable to hold the cache created

pool the pool the cache is attached to

classes_count the number of the size classes

classes pointer to the first element of an array describing the size classes


<h4>Returned Values</h4>

If the return value is :c:macro:`MPS_RES_OK`, a new cache in <code>*sac_o</code>.


<h4>Description</h4>

This function creates an allocation cache whose free-list is segregated into the given size classes. The cache can get more memory from the given pool, or return memory to it.

Segregated allocation caches can be associated with any pool that supports :c:func:`mps_alloc` and :c:func:`mps_free`.

The size classes are described by an array of element type :c:func:`mps_sac_class_s` (q.v.). This array is used to initialize the cache, and is not needed after:c:func:`mps_sac_create` returns. There might be a limit on how many classes can be described,but it will be no less than :c:macro:`MPS_SAC_CLASS_LIMIT`. You must specify at least one class.The MPS automatically provides an "overlarge" class for arbitrarily large objects above the largest class described. Allocations falling into the overlarge class are not cached.


<h4>Example</h4>

<pre>
  mps_sac_t sac;
  mps_sac_class_s classes[3] = { {8, 38, 1}, {136, 19, 3}, {512, 4, 1} };

  res = mps_sac_create(&amp;sac, pool, 3, classes);
  if (res != MPS_RES_OK) {
    printf("Failed to create the allocation cache!");
    exit(1);
  }
</pre>


<h4>Error Handling</h4>

:c:func:`mps_sac_create` returns :c:macro:`MPS_RES_MEMORY` or:c:macro:`MPS_RES_COMMIT_LIMIT` when it fails to allocate memory for the internal cache structure;see the documentation for those return codes for recovery options. It returns:c:macro:`MPS_RES_LIMIT` if you ask for too many size classes; combine some small adjacent classes. It returns :c:macro:`MPS_RES_PARAM` if the pool doesn't support segregated allocation caches.


<h4>See Also</h4>



:c:func:`mps_sac_class_s`,

:c:macro:`MPS_SAC_CLASS_LIMIT`,

:c:func:`mps_sac_destroy`,

:c:macro:`MPS_RES_MEMORY`,

:c:macro:`MPS_RES_COMMIT_LIMIT`,

:c:macro:`MPS_RES_LIMIT`,

:c:macro:`MPS_RES_PARAM`,

:c:func:`mps_sac_t`


<h4>Notes</h4>

Too many classes will slow down allocation; too few classes waste more space in internal fragmentation. It is assumed that overlarge allocations are rare; otherwise, you would add another class for them, or even create separate allocation caches or pools for them.

Some pools will work more efficiently with caches than others. In the future, the MPS might offer pools specially optimized for particular types of cache.

Segregated allocation caches work poorly with debug pool classes at the moment: the checking only happens when blocks are moved between the cache and the pool. This will be fixed, but the speed of allocation with a debug class will always be similar to :c:func:`mps_alloc`, rather than cached speed.


<h4>Type</h4>

size_t


<h4>Associated Protocols</h4>

Allocation cache


<h4>Description</h4>

:c:macro:`MPS_SAC_CLASS_LIMIT` specifies a lower limit on the maximum number of classes that can be described in a call to :c:func:`mps_sac_create`, i.e., the MPS guarantees to accept at least this many classes. More might be accepted -- in fact, there might not be any limit in the implementation on the maximum number of classes, but if you specify more than this, you should be prepared to handle the error.

:c:macro:`MPS_SAC_CLASS_LIMIT` is a macro suitable for use in a constant expression, both in a #if directive and wherever else constant expressions may be used.



<h4>See Also</h4>



:c:func:`mps_sac_create`


<h4>Notes</h4>

If you ask for too many size classes, :c:func:`mps_sac_create` returns :c:macro:`MPS_RES_LIMIT`; you can recover by combining some small adjacent classes.


.. c:function:: void mps_sac_destroy(mps_sac_t);


<h4>Summary</h4>

This function destroys a segregated allocation cache.


<h4>Associated Protocols</h4>

Allocation cache


<h4>Arguments</h4>

sac the segregated allocation cache



<h4>Description</h4>

This function destroys a segregated allocation cache. All memory held in it is returned to the associated pool.


<h4>Example</h4>

<pre>
  res = mps_sac_create(&amp;sac, pool, 3, classes);
  if (res != MPS_RES_OK) {
    printf("Failed to create the allocation cache!");
    exit(1);
  }

  /* Use sac. */

  mps_sac_destroy(sac);
  mps_pool_destroy(pool);
</pre>


<h4>See Also</h4>



:c:func:`mps_sac_create`,

:c:func:`mps_sac_t`


<h4>Notes</h4>

Destroying the cache might well cause the pool to return some memory to the arena, but that's up to the pool's usual policy.

Destroying the cache has no effect on objects allocated through it.


.. c:function:: void mps_sac_flush(mps_sac_t sac);


<h4>Summary</h4>

This function flushes the segregated allocation cache given.


<h4>Associated Protocols</h4>

Allocation cache


<h4>Arguments</h4>

sac the segregated allocation cache



<h4>Description</h4>

This function flushes the segregated allocation cache given, returning all memory held in it to the associated pool.

The client is responsible for synchronizing the access to the cache, but the MPS will properly synchronize with any other threads that might be accessing the same pool.


<h4>Example</h4>

<pre>
  mps_sac_t sac_small, sac_large;

  res = mps_sac_create(&amp;sac_small, pool, 3, small_classes);
  if (res != MPS_RES_OK) {
    printf("Failed to create the small allocation cache!");
    exit(1);
  }

  res = mps_sac_create(&amp;sac_large, pool, 3, large_classes);
  if (res != MPS_RES_OK) {
    printf("Failed to create the large allocation cache!");
    exit(1);
  }

  /* Use sac_small. */

  mps_sac_flush(sac_small);

  /* Use sac_large. */

  mps_sac_flush(sac_large);

  /* Use sac_small. */
</pre>


<h4>See Also</h4>



:c:func:`mps_sac_t`


<h4>Notes</h4>

This is something that you'd typically do when you know you won't be using the cache for awhile, but want to hold on to the cache itself. Destroying a cache has the effect of flushing it,naturally.

Flushing the cache might well cause the pool to return some memory to the arena, but that's up to the pool's usual policy.

Note that the MPS might also decide to take memory from the cache without the client requesting a flush.


.. c:type:: mps_sac_t


<h4>Summary</h4>

Type of segregated allocation caches.


<h4>Associated Protocols</h4>

Allocation cache


<h4>Type</h4>

<code>typedef struct mps_sac_s *mps_sac_t;</code>


<h4>Description</h4>

A value of this type represents an allocation cache with segregated free lists. It is an opaque type.


<h4>Example</h4>

<pre>
  mps_sac_t sac;
  mps_sac_class_s classes[3] = { {8, 38, 1}, {136, 19, 3}, {512, 4, 1} };

  res = mps_sac_create(&amp;sac, pool, 3, classes);
  if (res != MPS_RES_OK) {
    printf("Failed to create the allocation cache!");
    exit(1);
  }
</pre>


<h4>See Also</h4>



:c:func:`mps_sac_create`,

:c:func:`mps_sac_destroy`,

:c:macro:`MPS_SAC_ALLOC`,

:c:func:`mps_sac_alloc`,

:c:macro:`MPS_SAC_FREE`,

:c:func:`mps_sac_free`,

:c:func:`mps_sac_flush`


<h4>Notes</h4>

None.


.. c:function:: mps_res_t mps_stack_scan_ambig(mps_ss_t scan_state, mps_thr_t thread, void *stack_bottom, size_t ignore)


<h4>Summary</h4>

A scanning function for ambiguous scanning of thread states.


<h4>Associated Protocols</h4>

Root.


<h4>Arguments</h4>

scan_state a scan state

thread the thread

stack_bottom a pointer to the bottom of the stack

ignore ignored


<h4>Returned Values</h4>

A result code.


<h4>Description</h4>

This is a root scanning function of type :c:func:`mps_reg_scan_t`. It will scan all integer registers and everything on the stack of the thread given, and can therefore only be used with roots of rank :c:macro:`MPS_RANK_AMBIG`. It will only scan things at the given stack bottom pointer or higher on the stack (that is, more recently added). References are assumed to be represented as machine words, and are required to be 4-byte-aligned; unaligned values are ignored.

Clients don't call this function, it is used as an argument of :c:func:`mps_root_create_reg`.


<h4>Example</h4>

<pre>
typedef struct {
  mps_root_t mmRoot;
  mps_thr_t thread;
  /* ... */
} ThreadLocals;

void InitThread(ThreadLocals *thr)
{
  /* This is a hack to find the bottom of the stack. */
  void *stackBottom=&amp;stackBottom;

  mps_thread_reg(&amp;thr-&gt;thread, arena);
  mps_root_create_reg(&amp;thr-&gt;mmRoot, arena, MPS_RANK_AMBIG, (mps_rm_t)0,
    thr-&gt;thread, mps_stack_scan_ambig, stackBottom, 0)

  /* ... */
}
</pre>


<h4>See Also</h4>



:c:func:`mps_reg_scan_t`,

:c:func:`mps_root_create_reg`


<h4>Notes</h4>

The MPS provides this function because it's hard to write (it's OS- and architecture-dependent and possibly compiler-dependent).


.. c:function:: mps_word_t mps_telemetry_control(mps_word_t reset_mask, mps_word_t flip_mask);


<h4>Summary</h4>

This function is used to read and change the filters on the telemetry stream.


<h4>Associated Protocols</h4>

Telemetry.


<h4>Arguments</h4>

reset_mask is a bit mask indicating the bits that should be reset, regardless of previous value.

flip_mask is a bit mask indicating the bits whose value should be flipped after the resetting.


<h4>Returned Values</h4>

The function returns the previous value of the telemetry filter control.


<h4>Description</h4>

This function is used to read and change the filters on the telemetry stream. It is generally for use by developers.

The parameters reset_mask and flip_mask allow specifying any binary operation on the filter control. To use this function for typical operations, the parameters should be set as follows:

Operation reset_mask flip_mask

set(M) M M

reset(M) M 0

flip(M) 0 M

read() 0 0

The significance of the bits is liable to change, but the current values (number the least significant bit as zero) are:

0 -- per space or arena

1 -- per pool

2 -- per trace or scan

3 -- per page (segment)

4 -- per reference or fix

5 -- per allocation or object

6 -- user events (e.g., :c:func:`mps_telemetry_intern`)


<h4>Example</h4>


<h4>See Also</h4>



:c:func:`mps_lib_telemetry_control`


.. c:function:: void mps_telemetry_flush(void);


<h4>Summary</h4>

This function is used to flush the internal event buffers.


<h4>Associated Protocols</h4>

Telemetry.


<h4>Description</h4>

This function is used to flush the internal event buffers into the event stream. This function also calls :c:func:`mps_lib_io_flush` on the event stream itself. This ensures that even the latest events are now properly recorded, should the application terminate (uncontrollably as a result of a bug, for example) or some interactive tool require access to the event data. You could even try calling this from a debugger after a problem.


<h4>Example</h4>

<code>mps_telemetry_flush();</code>


<h4>See Also</h4>



:c:func:`mps_lib_io_flush`


.. c:function:: mps_word_t mps_telemetry_intern(char *)


<h4>Summary</h4>

This function registers a string with the MPS, and receives a unique identifier in return.This identifier is suitable for use with :c:func:`mps_telemetry_label`.


<h4>Associated Protocols</h4>

Telemetry


<h4>Arguments</h4>

The function receives a name as a nul-terminated string in the usual C way. The string's length should not exceed 256 characters, including nul terminating character. In appropriate varieties this restriction is checked and will cause the MPS to issue an ASSERT. So don't do it.


<h4>Returned Values</h4>

The function returns a unique identifier that may be used to represent the string in future.


<h4>Description</h4>

The intention of this function is to provide an immediate identifier that can be used to concisely represent a string for the purposes of :c:func:`mps_telemetry_label`. Note that the appropriate settings must be made to the telemetry filter (via :c:func:`mps_telemetry_control`) before this function is invoked; the associate event is of the user kind.


<h4>Error Handling</h4>

The string's length should not exceed 256 characters, including nul terminating character.This will cause the MPS to issue an ASSERT in appropriate varieties.


<h4>See Also</h4>



:c:func:`mps_telemetry_label`



:c:func:`mps_telemetry_control`


.. c:function:: void mps_telemetry_label(mps_addr_t, mps_word_t);


<h4>Summary</h4>

This function associates an identifier returned from :c:func:`mps_telemetry_intern`, and hence a string, with an address, in the telemetry stream.


<h4>Associated Protocols</h4>

telemetry


<h4>Arguments</h4>

The function receives an address and an identifier. The identifier should be one returned by :c:func:`mps_telemetry_intern` in the same session.


<h4>Description</h4>

This function is intended to associate the address with an identifier in the telemetry stream. Note that the user kind must be set in the telemetry filter.


<h4>Example</h4>

Typical uses include:

- Label pools with a human-meaningful name;

- Label allocated objects with their type or class.


<h4>See Also</h4>



:c:func:`mps_telemetry_intern`,

:c:func:`mps_telemetry_control`,

:c:func:`mps_thr_t`


.. c:type:: mps_thr_t



<h4>Summary</h4>

:c:func:`mps_thr_t` is the type of thread records registered with the MPS.


<h4>Associated Protocols</h4>

Threads.


<h4>Type</h4>

<code>typedef mps_thr_s *mps_thr_t;</code>


<h4>Description</h4>

An object of the opaque type :c:func:`mps_thr_t` is a thread registration. In a multi-threaded environment where incremental garbage collection is used, threads must be registered with the MPS so that the MPS can examine their state.

An object of type :c:func:`mps_thr_t` is obtained using the thread registration function :c:func:`mps_thread_reg`.


<h4>Example</h4>

<pre>
  mps_thr_t this_thread;
  mps_res_t res;

  res = mps_thread_reg(&amp;this_thread, space);
  if(res != MPS_RES_OK) return res;
</pre>


<h4>See Also</h4>



:c:func:`mps_reg_t`,

:c:func:`mps_thread_reg`,

:c:func:`mps_thread_dereg`,

:c:func:`mps_reg_scan_t`,

:c:func:`mps_root_create_reg`,

:c:func:`mps_stack_scan_ambig`


<h2> <a id="section-4" name="section-4">4. Undocumented Symbols</a> </h2>

The following MPS symbols are used or defined in MPS header files, and intended for client use, but are not yet documented in this reference manual.

[This section is very out-of-date. RB 2012-08-15]

<pre>
mps_arena_t
mps_pool_t
mps_chain_t
mps_root_t
mps_ap_t
mps_ld_t
mps_ss_t
mps_alloc_pattern_t
mps_frame_t
mps_word_t
mps_shift_t
mps_rm_t
MPS_RES_OK
MPS_RES_FAIL
MPS_RES_UNIMPL
MPS_RES_IO
MPS_RES_COMMIT_LIMIT
mps_ap_s
mps_sac_freelist_block_s
mps_sac_s
mps_ld_s
mps_ss_s
mps_fmt_fixed_s
MPS_BEGIN
MPS_END
mps_arena_step
mps_arena_start_collect
mps_arena_destroy
mps_arena_reserved
mps_arena_extend
mps_arena_retract
mps_fmt_create_fixed
mps_fmt_destroy
mps_addr_pool
mps_addr_fmt
mps_pool_create
mps_pool_create_v
mps_pool_destroy
mps_gen_param_s
mps_chain_create
mps_chain_destroy
mps_alloc_v
mps_ap_create
mps_ap_create_v
mps_ap_destroy
mps_reserve
mps_commit
mps_ap_fill
mps_ap_fill_with_reservoir_permit
mps_ap_trip
MPS_SAC_ALLOC
MPS_SAC_FREE
mps_reservoir_limit_set
mps_reservoir_limit
mps_reservoir_available
mps_reserve_with_reservoir_permit
MPS_RESERVE_BLOCK
MPS_RESERVE_WITH_RESERVOIRf_PERMIT_BLOCK
mps_root_destroy
mps_tramp_t
mps_tramp
mps_thread_reg
mps_thread_dereg
mps_ld_reset
mps_ld_add
mps_ld_merge
mps_ld_isstale
mps_collections
mps_definalize
mps_alert_collection_set
mps_pool_check_free_space
mps_lib_get_EOF
mps_lib_stream_s
mps_lib_get_stderr
mps_lib_get_stdout
mps_lib_fputc
mps_lib_fputs
mps_lib_assert_fail
mps_clock_t
mps_clock
mps_clocks_per_sec
mps_class_amcz
mps_class_ams
mps_class_ams_debug
mps_class_awl
mps_class_lo
mps_mv_free_size
mps_mv_size
mps_class_mv
mps_class_mv_debug
mps_mvt_free_size
mps_mvt_size
mps_mvff_free_size
mps_mvff_size
mps_class_mvff_debug
mps_SEH_filter
mps_SEH_handler
mps_io_t
mps_io_create
mps_io_destroy
mps_io_write
mps_io_flush
MPS_PF_STRING
MPS_PF_ALIGN
MPS_ARCH_I3
MPS_ARCH_I4
MPS_ARCH_PP
MPS_ARCH_S8
MPS_ARCH_S9
MPS_BUILD_GC
MPS_BUILD_MV
MPS_BUILD_SC
MPS_OS_FR
MPS_OS_LI
MPS_OS_SO
MPS_OS_W3
MPS_OS_XC
MPS_PF_FRI3GC
MPS_PF_LII3GC
MPS_PF_LIPPGC
MPS_PF_SOS8GC
MPS_PF_SOS9SC
MPS_PF_W3I3MV
MPS_PF_XCPPGC
</pre>

<h2><a id="section-A" name="section-A">A. References</a></h2>


<h2><a id="section-B" name="section-B">B. Document History</a></h2>

<table>

<tr valign="top">

  <td>2002-05-27</td>

  <td><a href="mailto:rb@ravenbrook.com">RB</a></td>

  <td> Created from individual MPS reference pages, originally written and mainted in Lotus Notes by members of the Memory Management Group of Global Graphics (formerly Harlequin).  I found many errors caused by the various conversions that this text has been through.  There are probably many more. </td>

</tr>

<tr valign="top">

  <td>2002-06-17</td>

  <td><a href="mailto:rb@ravenbrook.com">RB</a></td>

  <td> Removed Global Graphics specific entries for confidential sources not included in open source release. </td>

</tr>

<tr valign="top">

  <td>2002-06-18</td>

  <td><a href="mailto:nb@ravenbrook.com">NB</a></td>

  <td> Added contents table to section 3.</td>

</tr>

<tr valign="top">

  <td>2002-06-20</td>

  <td><a href="mailto:nb@ravenbrook.com">NB</a></td>

  <td>Quite a bit of proof-reading, to insert missing spaces.  Also reformatted for easier editing, including the "See Also" sections.</td>

</tr>

<tr valign="top">

  <td>2002-06-21</td>

  <td><a href="mailto:nb@ravenbrook.com">NB</a></td>

  <td>Removed obsolete symbols.</td>

</tr>

<tr valign="top">
  <td>2003-01-01</td>
  <td><a href="mailto:drj@ravenbrook.com">DRJ</a></td>
  <td>[various edits in 2003]</td>
</tr>

<tr valign="top">
  <td>2006-06-06</td>
  <td><a href="mailto:rhsk@ravenbrook.com">RHSK</a></td>
  <td>Marked copy format method as obsolete.</td>
</tr>

<tr valign="top">
  <td>2007-06-19</td>
  <td><a href="mailto:drj@ravenbrook.com">DRJ</a></td>
  <td>mps_finalize and finalization messages: correct, clarify, and expand descriptions </td>
</tr>

<tr valign="top">
  <td>2008-10-24</td>
  <td><a href="mailto:rhsk@ravenbrook.com">RHSK</a></td>
  <td>mps_arena_has_addr entry: fix wrong title; remove from undocumented symbols list.  (Also: fix html tag error; add missing document history entries).</td>
</tr>

<tr valign="top">
  <td>2008-11-25</td>
  <td><a href="mailto:rhsk@ravenbrook.com">RHSK</a></td>
  <td>mps_message_clock: add entry.</td>
</tr>

<tr valign="top">
  <td>2010-03-02</td>
  <td><a href="mailto:rhsk@ravenbrook.com">RHSK</a></td>
  <td>mps_addr_pool, mps_addr_fmt, mps_alert_collection_set: as yet Undocumented.</td>
</tr>

</table>


<h2><a id="section-C" name="section-C">C. Copyright and License</a></h2>

<hr />

<div align="center">

<code>$Id: //info.ravenbrook.com/project/mps/branch/2012-10-09/user-guide/manual/reference/index.html#1 $</code>


<a href="/">Ravenbrook</a> /
<a href="/project/">Projects</a> /
<a href="/project/mps/">Memory Pool System</a> /
<a href="/project/mps/master/">Master Product Sources</a> /
<a href="/project/mps/master/manual/">Product Manuals</a>


</div>

</body>

</html>


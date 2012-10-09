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


.. c:type:: mps_addr_t

    The type of :term:`addresses <address>` managed by the MPS, and
    also the type of :term:`references <reference>`.

    It is used in the MPS interface for any pointer that is under the
    control of the MPS. In accordance with standard C practice, the
    value ``NULL`` of type :c:type:`mps_addr_t` will never be used to
    represent a reference to a block.

    .. seealso::

        :ref:`topic-allocation` and :ref:`topic-platform`.


.. c:type:: mps_align_t

    The type of an :term:`alignment`. It is an integral type
    equivalent to the standard C type ``size_t``. An alignment must be
    a positive powers of 2.

    .. seealso::

        :ref:`topic-allocation`.


.. c:function:: mps_res_t mps_alloc(mps_addr_t *p_o, mps_pool_t pool, size_t size, ...)

    Allocate a :term:`block` of memory in a :term:`pool`.

    *p_o* points to a location that will hold the address of the
    allocated block.

    *pool* the pool to allocate in.

    *size* is the :term:`size` of the block to allocate.

    Some pools require additional arguments to be passed to
    :c:func:`mps_alloc`. See the documentation for the pool class.

    .. seealso::

        :ref:`topic-allocation`.


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


.. c:function:: void mps_amc_apply(mps_pool_t pool, void (*f)(mps_addr_t object, void *p, size_t s), void *p, size_t s)

    Inspect :term:`formatted objects <formatted object>` in an
    :ref:`AMC pool <pool-amc>`.

    *pool* is the pool whose formatted objects you want to inspect.

    *f* is a function that will be called for each formatted object in
    the pool. It takes three arguments: *object* is the address of the
    object; *p* and *s* are the corresponding arguments that were
    passed to :c:func:`mps_amc_apply`.

    *p* and *s* are arguments that will be passed to *f* each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    You may only call this function when the :term:`arena` is
    :term:`parked`, for example, after calling
    :c:func:`mps_arena_collect` or :c:func:`mps_arena_park`.

    The function *f* will be called on both :term:`data <data object>`
    and :term:`pad objects <pad object>`. It is the job of *f* to
    distinguish, if necessary, between the two. It may also be called
    on :term:`dead` objects that the collector has not recycled or has
    been unable to recycle.

    The function *f* may not allocate memory or access any
    automatically-managed memory except within *object*.

    .. seealso::

        :ref:`topic-scanning`.

    .. notes::

        There is no equivalent function for other pool classes, but
        there is a more general function
        :c:func:`mps_arena_formatted_objects_walk` that inspects all
        formatted objects in the arena.


.. c:function:: mps_res_t mps_fix(mps_ss_t ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it. This
    function must only be called from within a :term:`scan function`.

    *ss* is the :term:`scan state` that was passed to the scan
    function.

    *ref_io* points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the reference
    may have been updated (see the topic :ref:`topic-moving`), and the
    scan function must continue to scan the :term:`block`. If it
    returns any other result, the scan function must return that
    result as soon as possible, without fixing any further references.

    .. deprecated:: 1.110

        Use :c:func:`MPS_FIX12` instead.

    .. seealso::

        :ref:`topic-scanning` and :ref:`topic-moving`.

    .. note::

        If you want to call this between :c:func:`MPS_SCAN_BEGIN` and
        :c:func:`MPS_SCAN_END`, you must use :c:func:`MPS_FIX_CALL`
        to ensure that the scan state is passed correctly.


.. c:function:: mps_bool_t MPS_FIX1(mps_ss_t mps_ss, mps_addr_t ref)

    Tell the MPS about a :term:`reference`. This macro must only be
    used within a :term:`scan function`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    *ss* is the :term:`scan state` that was passed to the scan function.

    *ref* is the reference.

    Returns a truth value (:c:type:`mps_bool_t`) indicating whether
    the reference is likely to be interesting to the MPS. If it
    returns false, the scan function must continue scanning the
    :term:`block`. If it returns true, the scan function must invoke
    :c:func:`MPS_FIX2`, to fix the reference.

    .. seealso::

        :ref:`topic-scanning`.

    .. note::

        In the common case where the scan function does not need to do
        anything between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`, you can
        use the convenience macro :c:func:`MPS_FIX12`.


.. c:function:: MPS_FIX12(mps_ss_t mps_ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it. This
    macro must only be used within a :term:`scan function`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    *ss* is the :term:`scan state` that was passed to the scan function.

    *ref_io* points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the reference
    may have been updated (see the topic :ref:`topic-moving`), and the
    scan function must continue to scan the :term:`block`. If it
    returns any other result, the scan function must return that
    result as soon as possible, without fixing any further references.

    .. seealso::

        :ref:`topic-scanning`.

    .. note::

        The macro :c:func:`MPS_FIX12` is a convenience for the common
        case where :c:func:`MPS_FIX1` is immediately followed by
        :c:func:`MPS_FIX2`.


.. c:function:: MPS_FIX2(mps_ss_t mps_ss, mps_addr_t *ref_io)

    Tell the MPS about a :term:`reference`, and possibly update it.
    This macro must only be used within a :term:`scan function`,
    between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`.

    *ss* is the :term:`scan state` that was passed to the scan function.

    *ref_io* points to the reference.

    Returns :c:macro:`MPS_RES_OK` if successful: in this case the reference
    may have been updated (see the topic :ref:`topic-moving`), and the
    scan function must continue to scan the :term:`block`. If it
    returns any other result, the scan function must return that
    result as soon as possible, without fixing any further references.

    .. seealso::

        :ref:`topic-scanning`.

    .. note::

        In the common case where the scan function does not need to do
        anything between :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2`,
        you can use the convenience macro :c:func:`MPS_FIX12`.


.. c:function:: MPS_FIX_CALL(ss, call)

    Call a function from within a :term:`scan function`, between
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, passing
    scan state correctly.

    *ss* is the :term:`scan state` that was passed to the scan function.

    *call* is an expression containing a call to a scan function.

    Returns the result of evaluating the expression *call*.

    Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, the
    scan state is in a special state, and must not be passed to a
    function. If you really need to do so, for example because you
    have an embedded structure shared between two scan methods, you
    must wrap the call with :c:func:`MPS_FIX_CALL` to ensure that the
    scan state is passed correctly.

    In this example, the scan function ``obj_scan`` fixes the object's
    ``left`` and ``right`` references, but delegates the scanning of
    references inside the object's ``data`` member to the function
    ``scan_data``. In order to ensure that the scan state is passed
    correctly to ``scan_data``, the call must be wrapped in
    ``MPS_FIX_CALL``. ::

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


.. c:macro:: MPS_RES_COMMIT_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested without exceeding the :term:`arena commit
    limit`.

    You need to deallocate something to make more space, or increase
    the arena commit limit by calling
    :c:func:`mps_arena_commit_limit_set`.

    .. seealso::

        :ref:`topic-error`.


.. c:macro:: MPS_RES_LIMIT

    A :term:`result code` indicating that an operation could not be
    completed as requested because of an internal limitation of the
    MPS. The precise meaning depends on the function that returned the
    code. Refer to the documentation of that function for details.

    .. seealso::

        :ref:`topic-error`.


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

        Failing to acquire enough memory because the :term:`arena
        commit limit` would have been exceeded is indicated by
        returning :c:macro:`MPS_RES_COMMIT_LIMIT`, not
        ``MPS_RES_MEMORY``.

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
    root>`. This tells the MPS that it may place a :term:`hardware
    write barrier` on any :term:`pages <page>` which any part of the
    :term:`root` covers. No :term:`format method` or :term:`scan
    function` (except for the one for this root) may write data in
    this root. They may read it.

    .. seealso::

        :ref:`topic-root`.

    .. note::

        You must not specify ``MPS_RM_PROT`` on a root allocated by
        the MPS.

    .. note::

        No page may contain parts of two or more protectable
        roots. You mustn't specify ``MPS_RM_PROT`` if the client
        program or anything other than (this instance of) the MPS is
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
    :c:macro:`MPS_RES_COMMIT_LIMIT` if the :term:`arena commit limit`
    was exceeded, or :c:macro:`MPS_RES_RESOURCE` if it ran out of
    :term:`virtual memory`.

    .. seealso::

        :ref:`topic-cache`.

    .. note::

        There's also a macro :c:func:`MPS_SAC_ALLOC_FAST` that does
        the same thing. The macro is faster, but generates more code
        and does less checking.

    .. note::

        The client is responsible for synchronizing the access to the
        cache, but if the cache decides to access the pool, the MPS will
        properly synchronize with any other threads that might be
        accessing the same pool.

    .. note::

        Blocks allocated through a segregated allocation cache should
        only be freed through a segregated allocation cache with the
        same :term:`class structure`. Using :c:func:`mps_free` on them
        can cause :term:`memory leaks <memory leak>`, because the size
        of the block might be larger than you think. Naturally, the
        cache must also be attached to the same pool.


.. c:function:: MPS_SAC_ALLOC_FAST(mps_res_t res_o, mps_addr_t *p_o, mps_sac_t sac, size_t size, mps_bool_t has_reservoir_permit)

    A macro alternative to :c:func:`mps_sac_alloc` that is faster
    than the function but does less checking. The arguments are
    identical to the function, except that the macro takes an
    additional first argument, *res_o*, which must be an lvalue that
    will store the :term:`result code`, and the macro doesn't evaluate
    ``has_reservoir_permit`` unless it decides to access the pool.

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

        The client is responsible for synchronizing the access to the
        cache, but if the cache decides to access the pool, the MPS
        will properly synchronize with any other threads that might be
        accessing the same pool.

    .. note::

        There's also a macro :c:func:`MPS_SAC_FREE_FAST` that does the
        same thing. The macro is faster, but generates more code and
        does no checking.

    .. note::

        ``mps_sac_free`` does very little checking: it's optimized for
        speed. :term:`Double frees <double free>` and other mistakes
        will only be detected when the cache is flushed (either by
        calling :c:func:`mps_sac_flush` or automatically), and may not
        be detected at all, if intervening operations have obscured
        symptoms.


.. c:function:: MPS_SAC_FREE_FAST(mps_sac_t sac, mps_addr_t p, size_t size)

    A macro alternative to :c:func:`mps_sac_free` that is faster than
    the function but does no checking. The arguments are identical to
    the function.

    .. seealso::

        :ref:`topic-cache`.


.. c:function:: MPS_SCAN_BEGIN(mps_ss_t ss)

    Within a :term:`scan function`, set up local information required
    by :c:func:`MPS_FIX1`, :c:func:`MPS_FIX2` and
    :c:func:`MPS_FIX12`. The local information persists until
    :c:func:`MPS_SCAN_END`.

    *ss* is the :term:`scan state` that was passed to the scan
    function.

    .. seealso::

        :ref:`topic-scanning`.

    .. note::

        Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, the
        scan state is in a special state, and must not be passed to a
        function. If you really need to do so, for example because you
        have an embedded structure shared between two scan methods, you
        must wrap the call with :c:func:`MPS_FIX_CALL` to ensure that the
        scan state is passed correctly.


.. c:function:: MPS_SCAN_END(mps_ss_t ss)

    Within a :term:`scan function`, terminate a block started by
    :c:func:`MPS_SCAN_BEGIN`.

    *ss* is the :term:`scan state` that was passed to the scan
    function.

    .. seealso::

        :ref:`topic-scanning`.

    .. note::

        :c:func:`MPS_SCAN_END` ensures that the scan is completed, so
        successful termination of a scan must invoke it. However, in
        case of an error it is allowed to return from the scan
        function without invoking :c:func:`MPS_SCAN_END`.

    .. note::

        Between :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END`, the
        scan state is in a special state, and must not be passed to a
        function. If you really need to do so, for example because you
        have an embedded structure shared between two scan methods, you
        must wrap the call with :c:func:`MPS_FIX_CALL` to ensure that the
        scan state is passed correctly.


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


.. c:function:: mps_res_t mps_ap_alloc_pattern_reset(mps_ap_t ap);

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
    that the allocation point belongs to. Typically, :term:`manual
    pool classes <manual pool class>` use this declaration to mean
    that the blocks are dead and their space can be reclaimed
    immediately, whereas :term:`automatic pool classes <automatic pool
    class>` use this declaration to mean that the objects are likely
    to be mostly dead, and may use this declaration to alter its
    collection decisions. See the documentation for the pool class.

    In general a frame other than the current frame can be popped (all
    frames pushed more recently will be invalidated as well, as
    described above), but a pool class may impose the restriction that
    only the current frame may be popped. This restriction means that
    every push must have a corresponding pop. See the documentation
    for the pool class.

    It is illegal to pass an invalid frame to any MPS function. In
    particular, it is illegal to pop frames out of order (so the
    sequence "A = push; B = push; pop A; pop B" is illegal) or to pop
    the same frame twice (so the sequence "A = push, pop A, pop A" is
    illegal).

    .. seealso::

        :ref:`topic-frame`.


.. c:function:: mps_res_t mps_ap_frame_push(mps_frame_t *frame_o, mps_ap_t ap)

    Push a new :term:`allocation frame` onto an :term:`allocation
    point's <allocation point>` frame stack.

    .. seealso::

        :ref:`topic-frame`.




<h4>Arguments</h4>

<p><code>mps_frame_t *frameReturn</code>  The frame return parameter. A new frame (declared by this function) is stored in this location if this function is successful.</p>

<p><code>mps_ap_t ap</code> The allocation point in which the new frame is declared.</p>


<h4>Returned Values</h4>

<p>A result code in the usual way. The creation of new frame objects (which is implicit in the action of this function) can consume resources, so this function can fail because there are insufficient resources. This function may fail if the correct protocol is not followed by the client.</p>


<h4>Description</h4>

<p>This function declares a new frame in the specified allocation point, makes that new frame a child of the current frame, changes the current frame to be the newly created frame, and returns a handle to the frame. Frames have two important features: A single frame identifies a set of objects(those objects that are "allocated in the frame") which can be destroyed (or declared dead) in a pop operation (see <code><a href="#mps_ap_frame_pop">mps_ap_frame_pop</a></code>); They are arranged in a partially ordered sequence (this is important when the pop operation is used). A fuller and more useful description is found in the APstack protocol document (protocol.mps.alloc-point.stack).</p>


<h4>Example</h4>

<p>[missing]</p>


<h4>Error Handling</h4>

<p>Errors can either be because the client hasn't followed the correct protocol in which case there isn't much that we can recommend or else because some needed resource isn't available. The usual course of actions when short of resources is recommended.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_ap_frame_pop">mps_ap_frame_pop</a></code>,
protocol.mps.alloc-point.stack</p>


<h4>Notes</h4>


.. c:function:: extern void mps_arena_clamp(mps_arena_t);


<h4>Summary</h4>

<p><code><a href="#mps_arena_clamp">mps_arena_clamp</a></code> puts the specified arena into the clamped state.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p>arena -- the arena to be put into the clamped state</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_clamp">mps_arena_clamp</a></code> puts the specified arena into the clamped state. In the clamped state, no object motion will occur and the staleness of location dependencies will not change. All references to objects loaded while the arena is clamped will keep the same binary representation until after it is released.</p>

<p>In a clamped arena, incremental collection may still occur, but it will not be visible to the mutator and no new collections will begin. Space used by unreachable objects will not be recycled until the arena becomes unclamped.</p>


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_park">mps_arena_park</a></code>,

<code><a href="#mps_arena_release">mps_arena_release</a></code></p>


<h4>Notes</h4>


.. c:function:: mps_arena_class_t mps_arena_class_cl(void)


<h4>Summary</h4>

<p><code><a href="#mps_arena_class_cl">mps_arena_class_cl</a></code> returns the client arena class.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>Returns the client arena class.</p>


<h4>Resources</h4>

<p>mpsacl.h</p>


<h4>Description</h4>

<p>This function is used to get hold of the client arena class, for the purpose of passing it to <code><a href="#mps_arena_create">mps_arena_create</a></code>.</p>


<h4>Example</h4>

<pre>
mps_arena_t arena;

int main(void)
{
  void *block;
  mps_res_t res;

  block = malloc(ARENA_SIZE);
  if(block == NULL) {
    printf("Not enough memory!");
    exit(1);
  }

  res = mps_arena_create(&amp;arena, mps_arena_class_cl(), ARENA_SIZE, block);
  if(res != MPS_RES_OK) {
    printf("ARENA_SIZE too small");
    exit(2);
  }

  /* rest of program */
}
</pre>


<h4>Error Handling</h4>

<p>None.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_create">mps_arena_create</a></code></p>


<h4>Notes</h4>

<p>A client arena gets its managed memory from the client. This memory block is passed when the arena is created. When creating a client arena, <code><a href="#mps_arena_create">mps_arena_create</a></code> takes two extra arguments:</p>

<p><code>mps_res_t mps_arena_create(mps_arena_t *mps_arena_o, mps_arena_class_t mps_arena_class_cl, size_t size, void *block)</code></p>

<p><code>block</code> is the address of the memory block managed by the arena, and<code>size</code> is its size in bytes. If <code><a href="#mps_arena_create">mps_arena_create</a></code> returns <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code>, then the block was too small to hold the internal arena structures.Allocate a (much) larger one, and try again. <code><a href="#mps_arena_create">mps_arena_create</a></code> returns <code><a href="#MPS_RES_FAIL">MPS_RES_FAIL</a></code>, if the MPS library is copy-protected by a security device, such as a dongle, and a valid security device cannot be found.</p>


.. c:type:: mps_arena_class_t


<h4>Summary</h4>

<p>"m ps_arena_class_t " is the type of arena classes.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Type</h4>

<p><code>typedef struct mps_arena_s  *mps_arena_t;</code></p>

<p><code><a href="#mps_arena_class_s">mps_arena_class_s</a></code> is an incomplete structure type used only to declare the opaque type <code><a href="#mps_arena_class_t">mps_arena_class_t</a></code>.</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_class_t">mps_arena_class_t</a></code> is the type of arena classes. It is opaque.</p>


<h4>Example</h4>

<p>The definition of the client arena class in the "mpsacl.h" header:</p>

<p><code>extern mps_arena_class_t mps_arena_class_cl(void);</code></p>


<h4>See Also</h4>


<h4>Notes</h4>

<p>None.</p>


.. c:function:: mps_arena_class_t mps_arena_class_vm(void)


<h4>Summary</h4>

<p><code><a href="#mps_arena_class_vm">mps_arena_class_vm</a></code> returns the virtual memory arena class.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>Returns the virtual memory arena class.</p>


<h4>Resources</h4>

<p>mpsavm.h</p>


<h4>Description</h4>

<p>This function is used to get hold of the virtual memory arena class, for the purpose of passing it to <code><a href="#mps_arena_create">mps_arena_create</a></code>. The VM arenas use the OS virtual memory interfaces to allocate memory. The chief consequence of this is that the arena can manage many more virtual addresses than it needs to commit memory to. This gives it flexibility as to where to place objects, which reduces fragmentation and helps make garbage collection more efficient.</p>

<p>This class is similar to <code><a href="#mps_arena_class_vmnz">mps_arena_class_vmnz</a></code> but uses a more complex placement policy, which is more suited to copying garbage collection.</p>


<h4>Example</h4>

<pre>
mps_arena_t arena;

int main(void)
{
  mps_res_t res;

  res = mps_arena_create(&amp;arena, mps_arena_class_vm(), ARENA_SIZE);
  if(res != MPS_RES_OK) {
    printf("Not enough memory!");
    exit(1);
  }

  /* rest of program */

}
</pre>


<h4>Error Handling</h4>

<p>None.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_create">mps_arena_create</a></code>,

<code><a
href="#mps_arena_class_vmnz">mps_arena_class_vmnz</a></code></p>


<h4>Notes</h4>

<p>A virtual memory arena gets its managed memory from the operating system's virtual memory services. An initial address space size is passed when the arena is created. When creating a virtual memory arena, <code><a href="#mps_arena_create">mps_arena_create</a></code> takes one extra argument:</p>

<pre>mps_res_t mps_arena_create(mps_arena_t *arena_o,

                           mps_arena_class_t arena_class_vm,

                           size_t size)</pre>

<p><code>size</code> is the initial amount of virtual address space, in bytes, that the arena will reserve (this space is initially reserved so that the arena can subsequently use it without interference from other parts of the program, but most of it is not committed, so it don't require any RAM or backing store). The arena may allocate more virtual address space beyond this initial reservation as and when it deems it necessary. The MPS is most efficient if you reserve an address space that is several times larger than your peak memory usage.</p>

<p><code><a href="#mps_arena_create">mps_arena_create</a></code> returns <code><a href="#MPS_RES_RESOURCE">MPS_RES_RESOURCE</a></code> if it fails to reserveadequate address space to place the arena in; possibly other parts of the program are reserving too much virtual memory. It returns <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code> when it fails to allocate memory for the internal arena structures; either <code>size</code> was far too small or you ran out of swap space.It returns <code><a href="#MPS_RES_FAIL">MPS_RES_FAIL</a></code>, if the library is copy-protected by a security device, such as a dongle, and a valid security device cannot be found.</p>

<p>Virtual memory arenas are not available on the Mac platforms, other than MacOS X. You will get a linking error, if you attempt to use this function.</p>


.. c:function:: mps_arena_class_t mps_arena_class_vmnz(void);


<h4>Summary</h4>

<p>An arena class like <code><a href="#mps_arena_class_vm">mps_arena_class_vm</a></code> but with a different placement policy.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>Returns the VMNZ arena class.</p>


<h4>Resources</h4>

<p>mpsavm.h</p>


<h4>Description</h4>

<p>Returns the VMNZ arena class (stands for Virtual Memory No Zones, if you really care.) This class can be passed to <code><a href="#mps_arena_create">mps_arena_create</a></code> in order to create a VMNZ arena. The VMNZ arenas use the OS virtual memory interfaces to allocate memory. The chief consequence of this is that the arena can manage many more virtual addresses than it needs to commit memory to. This gives it flexibility as to where to place objects.</p>

<p>This class is similar to <code><a href="#mps_arena_class_vm">mps_arena_class_vm</a></code> but uses a simpler placement policy, that makes it slightly faster.</p>


<h4>Example</h4>

<pre>
mps_arena_t arena;

int main(void)
{
  mps_res_t res;

  res = mps_arena_create(&amp;arena, mps_arena_class_vmnz(), ARENA_SIZE);
  if(res != MPS_RES_OK) {
    printf("Not enough memory!");
    exit(1);
  }

  /* rest of program */

}
</pre>

<p></p>


<h4>Error Handling</h4>

<p>No errors.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_create">mps_arena_create</a></code>,

<code><a href="#mps_arena_class_vm">mps_arena_class_vm</a></code></p>


<h4>Notes</h4>

<p>This class takes an extra argument when used in <code><a href="#mps_arena_create">mps_arena_create</a></code> (see example).The extra parameter should be of type <code>size_t</code>. It specifies the amount of virtual address space, in bytes, that this arena should use. The arena will reserve this amount of virtual address space from the OS during initialization. It will not subsequently use any more address space(compare with <code><a href="#mps_arena_class_vm">mps_arena_class_vm</a></code> which can grow).</p>

<p><code><a href="#mps_arena_create">mps_arena_create</a></code> returns <code><a href="#MPS_RES_RESOURCE">MPS_RES_RESOURCE</a></code> if it fails to reserve adequate address space to place the arena in; possibly other parts of the program are reserving too much virtual memory. It returns <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code> when it fails to allocate memory for the internal arena structures; either <code>size</code> was far too small or you ran out of swap space.It returns <code><a href="#MPS_RES_FAIL">MPS_RES_FAIL</a></code>, if the library is copy-protected by a security device, such as a dongle, and a valid security device cannot be found.</p>

<p>Virtual memory arenas are not available on the Mac platforms, other than MacOS X. You will get a linking error, if you attempt to use this function.</p>


.. c:function:: void mps_arena_collect(mps_arena_t arena);


<h4>Summary</h4>

<p><code><a href="#mps_arena_collect">mps_arena_collect</a></code> collects the arena and puts it in the parked state.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p>arena the arena to collect</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_collect">mps_arena_collect</a></code> collects the arena and puts it in the parked state. Collecting the arena attempts to recycle as many unreachable objects as possible and reduce the size of the arena as much as possible (though in some cases it may increase because it becomes more fragmented). If you do not want the arena to be in the parked state, you must explicitly call <code><a href="#mps_arena_release">mps_arena_release</a></code> after<code><a href="#mps_arena_collect">mps_arena_collect</a></code>.</p>

<p>Note that the collector may not be able to recycle some objects (such as those near the destination of ambiguous references) even though they are not reachable.</p>


<h4>Example</h4>

<p>[missing]</p>


<h4>Error Handling</h4>

<p>No errors.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_park">mps_arena_park</a></code>,

<code><a href="#mps_arena_release">mps_arena_release</a></code></p>


<h4>Notes</h4>

<p>None.</p>


.. c:function:: size_t mps_arena_commit_limit(mps_arena_t arena)


<h4>Summary</h4>

<p>Returns the current commit limit associated with the arena in bytes.</p>


<h4>Associated Protocols</h4>

<p>Arena</p>


<h4>Arguments</h4>

<p>arena -- the arena</p>


<h4>Returned Values</h4>

<p>Returns the current commit limit as a number of bytes in a size_t</p>


<h4>Description</h4>

<p>Returns the current commit limit associated with the arena in bytes. The commit limit can be changed using the function <code><a href="#mps_commit_limit_set">mps_commit_limit_set</a></code>. The commit limit is used to control how much memory the MPS can obtain from the OS. See Arena Protocol for details.</p>


<h4>Example</h4>

<p><code>limit = mps_arena_commit_limit(arena);</code></p>


<h4>Error Handling</h4>

<p>No errors.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_committed">mps_arena_committed</a></code>,

<code><a
href="#mps_arena_commit_limit_set">mps_arena_commit_limit_set</a></code></p>


<h4>Notes</h4>

<p>None.</p>


.. c:function:: mps_res_t mps_arena_commit_limit_set(mps_arena_t arena, size_t limit)


<h4>Summary</h4>

<p>Changes the current commit limit associated with the arena.</p>


<h4>Associated Protocols</h4>

<p>Arena</p>


<h4>Arguments</h4>

<p>arena -- the arena</p>

<p>limit -- the new commit limit in bytes</p>


<h4>Returned Values</h4>

<p>Returns a result code.</p>


<h4>Description</h4>

<p>The commit limit of the arena is set to the limit given. The commit limit controls how much memory the MPS will obtain from the OS. See Arena Protocol for details. The commit limit cannot beset to a value that is lower than the number of bytes that the MPS is using. If an attempt is made to set the commit limit to a value greater than or equal to that returned by<code><a href="#mps_arena_committed">mps_arena_committed</a></code> then it will succeed. If an attempt is made to set the commit limit to a value less than that returned by <code><a href="#mps_arena_committed">mps_arena_committed</a></code> then it will succeed only if the amount committed by the MPS can be reduced by reducing the amount of spare committed memory; in such a case the spare committed memory will be reduced appropriately and the attempt will succeed.</p>


<h4>Example</h4>

<pre>
do {
  res = mps_arena_commit_limit_set(arena, limit - 100 * 1024);
  if(res != MPS_RES_OK)
    flush_caches();
} while(res != MPS_RES_OK);
</pre>


<h4>Error Handling</h4>

<p>Returns <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code> when successful, and some other result code when not.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_committed">mps_arena_committed</a></code>,

<code><a
href="#mps_arena_commit_limit">mps_arena_commit_limit</a></code>,

<code><a
href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code></p>


<h4>Notes</h4>

<p><code><a href="#mps_arena_commit_limit_set">mps_arena_commit_limit_set</a></code> puts a limit on all memory committed by the MPS. The"spare committed" memory can be limited separately with <code><a href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code>. Note that "spare committed" memory is subject to both limits; there cannot be more spare committed memory than the spare commit limit, and there can't be so much spare committed memory that there is more committed memory than the commit limit.</p>


.. c:function:: extern size_t mps_arena_committed(mps_arena_t arena)


<h4>Summary</h4>

<p><code><a href="#mps_arena_committed">mps_arena_committed</a></code> returns the amount of memory (backing store) in use by the arena, both for storing client objects and for its own data structures.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p>arena -- the arena</p>


<h4>Returned Values</h4>

<p>Returns a number of bytes (the amount of committed memory) as a size_t.</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_committed">mps_arena_committed</a></code> returns the amount of memory (backing store) in use by the arena (also known as "committed memory"). The value returned is a number of bytes.</p>

<p>Committed memory may be used both for storing client objects and for storing MPS datastructures. In addition the MPS maintains committed memory which is not being used (for either of the above purposes). This memory is known as "spare committed" memory (see <code><a href="#mps_arena_spare_committed">mps_arena_spare_committed</a></code>). The amount of "spare committed" memory can change atany time, in particular in will be reduced as appropriate in order meet client requests.</p>

<p>The reasons that the committed memory (as return by this function) might be large than the sum of the sizes of client allocated objects are:</p>

<ul>

  <li><p>some memory is used internally by the MPS to manage its own data structures and to record information about client objects (such as free lists, page tables, colour tables, statistics, etc).</p></li>

  <li><p>operating systems (and hardware) typically restrict programs to requesting and releasing memory with a certain granularity (for example, pages), so extra memory is committed when this rounding is necessary.</p></li>

  <li><p>there might be "spare committed" memory.</p></li>

</ul>

<p>The amount of committed memory is a good measure of how much virtual memory resource ("swapspace") the MPS is using from the OS.</p>

<p>This function may be called whether the arena is unclamped, clamped or parked, if called when the arena in unclamped then the value may change after this function returns. A possible use might be to call it just after <code><a href="#mps_arena_collect">mps_arena_collect</a></code> to (over-)estimate the size of the heap.</p>

<p>If you want to know how much memory the MPS is using then you're probably interested in the value <code>mps_arena_committed() - mps_arena_spare_committed()</code>.</p>

<p>The amount of committed memory can be limited with the function <code><a href="#mps_arena_commit_limit">mps_arena_commit_limit</a></code>.</p>


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_collect">mps_arena_collect</a></code>,

<code><a href="#mps_arena_clamp">mps_arena_clamp</a></code>,

<code><a href="#mps_arena_park">mps_arena_park</a></code>,

<code><a href="#mps_arena_release">mps_arena_release</a></code></p>


<h4>Notes</h4>

<p>-</p>


.. c:function:: mps_res_t mps_arena_create(mps_arena_t *mps_arena_o, mps_arena_class_t mps_arena_class, ...)


<h4>Summary</h4>

<p><code><a href="#mps_arena_create">mps_arena_create</a></code> is used to create an arena.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p><code><a href="#mps_arena_o">mps_arena_o</a></code> pointer to a variable to store the new arena in</p>

<p><code><a href="#mps_arena_class">mps_arena_class</a></code> the arena class</p>

<p><code>...</code> initialization arguments for the arena class</p>


<h4>Initial/Default Values</h4>

<p>Different for each arena class. See <code>mps_arena_class_*</code>.</p>


<h4>Returned Values</h4>

<p>If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the new arena is in <code>*mps_arena_o</code>.</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_create">mps_arena_create</a></code> is used to create an arena.</p>


<h4>Example</h4>

<pre>
mps_arena_t arena;

int main(void)
{
  mps_res_t res;

  res = mps_arena_create(&amp;arena, mps_arena_class_vm(), ARENA_SIZE);
  if(res != MPS_ RES_OK) {
    printf("Not enough memory!");
    exit(1);
  }

  /* rest of program */
}
</pre>


<h4>Error Handling</h4>

<p><code><a href="#mps_arena_create">mps_arena_create</a></code> returns <code><a href="#MPS_RES_FAIL">MPS_RES_FAIL</a></code>, if the MPS library is copy-protected by a security device, such as a dongle, and a valid security device cannot be found.Other error codes are specific to each arena class. See <code>mps_arena_class_*</code>.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_create_v">mps_arena_create_v</a></code>,

<code>mps_arena_class_*</code>,

<code><a href="#mps_arena_destroy">mps_arena_destroy</a></code></p>


.. c:function:: mps_res_t mps_arena_create_v(mps_arena_t *mps_arena_o, mps_arena_class_t mps_arena_class, va_list args)


<h4>Summary</h4>

<p><code><a href="#mps_arena_create_v">mps_arena_create_v</a></code> is used to create an arena.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p><code><a href="#mps_arena_o">mps_arena_o</a></code> pointer to a variable to store the new arena in</p>

<p><code><a href="#mps_arena_class">mps_arena_class</a></code> the arena class</p>

<p><code>args</code> initialization arguments for the arena class</p>


<h4>Initial/Default Values</h4>

<p>Different for each arena class. See <code>mps_arena_class_*</code>.</p>


<h4>Returned Values</h4>

<p>If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the new arena is in <code>*mps_arena_o</code>.</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_create_v">mps_arena_create_v</a></code> is used to create an arena. It is exactly the same as <code><a href="#mps_arena_create">mps_arena_create</a></code>, except that it takes the arena class initialization arguments in a <code>va_list</code>.</p>


<h4>Error Handling</h4>

<p><code><a href="#mps_arena_create_v">mps_arena_create_v</a></code> returns <code><a href="#MPS_RES_FAIL">MPS_RES_FAIL</a></code>, if the MPS library is copy-protected by a security device, such as a dongle, and a valid security device cannot be found. Other error codes are specific to each arena class. See <code>mps_arena_class_*</code>.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_create">mps_arena_create</a></code>,

<code>mps_arena_class_*</code>,

<code><a href="#mps_arena_destroy">mps_arena_destroy</a></code></p>

<h3>function <code><a id="mps_arena_expose"
name="mps_arena_expose">mps_arena_expose</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_arena_expose">mps_arena_expose</a></code> ensures that the MPS is not protecting any pages in the arena with read- or write-memory protection barriers.</p>

<h4>Associated Protocols</h4>

<p>Arena, clamp, park, protection</p>

<h4>Syntax</h4>

<p><code>mps_arena_expose(mps_arena);</code></p>

<h4>Type</h4>

<pre>
extern void mps_arena_expose(mps_arena_t);
</pre>

<h4>Arguments</h4>

<p><code>(mps_arena_t mps_arena)</code></p>

<p><code><a href="#mps_arena">mps_arena</a></code> is an MPS arena object.</p>

<h4>Returned Values</h4>

<p>None.</p>

<h4>Resources</h4>

<p>mps.h</p>

<h4>Description</h4>

<p>
This function will ensure that the MPS is not protecting (with memory
read/write barriers) any page in the arena.
This is expected to only be useful for debugging.
The arena is left in the clamped state (see <code><a
href="#mps_arena_clamp">mps_arena_clamp</a></code>).
</p>

<p>
Since barriers are used during a collection, calling this function has
the same effect as calling <code><a
href="#mps_arena_park">mps_arena_park</a></code>; all collections are
run to completion and the arena is clamped so that no new collections
begin.  The MPS also uses barriers to maintain remembered sets (an
optimisation to help avoid scanning work); calling
this function will effectively destroy the remembered sets and any
optimisation gains.
</p>

<p>
Calling this function will introduce a slow down, primarily for two reasons:
any active collections will be run to completion before this function
returns; the next collection will have to recompute all the remembered
sets by scanning the entire heap.
</p>

<p>
The second aspect of the slow down, having the next collection recompute
the remembered sets, can be avoided by using <code><a
href="#mps_arena_unsafe_expose_remember_protection">mps_arena_unsafe_expose_remember_protection</a></code>
instead of <code><a
href="#mps_arena_expose">mps_arena_expose</a></code>, and calling
<code><a
href="#mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code>
before calling <code><a
href="#mps_arena_release">mps_arena_release</a></code>.
Those functions have unsafe aspects and place restrictions on what the
client can do (basically no exposed data can be changed).
</p>

<h4>Example</h4>

<p>None.</p>

<h4>Error Handling</h4>

<p>There can be no errors.</p>

<h4>See Also</h4>

<code><a href="#mps_arena_clamp">mps_arena_clamp</a></code>,
<code><a href="#mps_arena_park">mps_arena_park</a></code>,
<code><a href="#mps_arena_release">mps_arena_release</a></code>,
<code><a href="#mps_arena_unsafe_expose_remember_protection">mps_arena_unsafe_expose_remember_protection</a></code>,
<code><a href="#mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code>


<h3>function <code><a id="mps_arena_formatted_objects_walk" name="mps_arena_formatted_objects_walk">mps_arena_formatted_objects_walk</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_arena_formatted_objects_walk">mps_arena_formatted_objects_walk</a></code> is used to iterate over all formatted objects in the MPS heap.</p>


<h4>Associated Protocols</h4>

<p>None.</p>


<h4>Syntax</h4>

<p><code>mps_arena_formatted_objects_walk(mps_arena, client_step_function, client_step_closure_p,client_step_closure_s);</code></p>


<h4>Type</h4>

<pre>
extern void mps_arena_formatted_objects_walk(mps_arena_t,
  mps_formatted_objects_stepper_t, void *,
  size_t);
</pre>


<h4>Arguments</h4>

<p><code>(mps_arena_t mps_arena, mps_formatted_objects_stepper_t stepper, void *p, size_t s)</code></p>

<p><code><a href="#mps_arena">mps_arena</a></code> is an MPS arena object.</p>

<p><code>stepper</code> is a client-supplied function (pointer) of the right type (see <code><a href="#mps_formatted_objects_stepper_t">mps_formatted_objects_stepper_t</a></code>). This function is applied to every object in all formatted pools. This function should take the argument list <code>(mps_addr_t object, mps_fmt_t format,mps_pool_t pool, void *p, size_t s)</code> and return <code>void</code>. <code>object</code> is the object to which the function is being applied. <code>format</code> is the format (an MPS format object) of the object. <code>pool</code> is the pool in which the object resides. <code>p</code> and <code>s</code> are copies of the corresponding values that the client passed into <code><a href="#mps_arena_formatted_objects_walk">mps_arena_formatted_objects_walk</a></code> originally.</p>

<p><code>p</code> and <code>s</code> are passed into the function specified by the stepper argument whenever the MPS calls that function. See <code><a href="#mps_formatted_objects_stepper_t">mps_formatted_objects_stepper_t</a></code>.</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_formatted_objects_walk">mps_arena_formatted_objects_walk</a></code> is used to iterate over all formatted objects in the MPS heap. A client-supplied function is called for every object in all formatted pools; the object, the format, and the pool are passed to the user supplied function, as well as user supplied closure variables.</p>

<p>Applies stepper function to a pool-class-specific collection of objects (that is, the pool class determines which objects in its instances get walked). Typically pool classes will arrange that all validly formatted objects are walked. During a trace this will in general be only the black objects, though the leaf pool class (LO), for example, will walk all objects since they are validly formatted whether they are black or white. Padding objects may be walked at the pool classes discretion, the client should handle this case.</p>

<p>The user supplied stepper function is called in a restricted context. It may not in general call any MPS function.</p>

<h4>Example</h4>

<p>[not yet]</p>


<h4>Error Handling</h4>

<p>There are none.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_amc_apply">mps_amc_apply</a></code> (the
historical walker),

<code><a
href="#mps_formatted_objects_stepper_t">mps_formatted_objects_stepper_t</a></code></p>


<h4>Notes</h4>


.. c:function:: extern mps_bool_t mps_arena_has_addr(mps_arena_t arena, mps_addr_t addr);


<h4>Summary</h4>

<p><code><a href="#mps_arena_has_addr">mps_arena_has_addr</a></code> tests whether an address is managed by a particular arena. </p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p><code>arena</code> an arena</p>

<p><code>addr</code> an address</p>


<h4>Returned Values</h4>

<p>A boolean.  Returns true if the address is managed by the arena, false otherwise.</p>


<h4>Description</h4>

<p>
<code><a href="#mps_arena_has_addr">mps_arena_has_addr</a></code> determines
whether a particular address is managed by a particular arena.  An arena
manages a portion of total address space available on the hardware
architecture.  No two arenas overlap so for any particular address this
function will return true for at most one arena.  In general not all the
architecture addresses are managed by some arena; some addresses will not
be managed by any arena.  This is what allows the MPS to cooperate with
other memory managers, shared object loaders, memory mapped file I/O,
and such like - it does not steal the whole address space.
</p>
<p>
The results from this function are true only for the instant at which
the function returned.  In some circumstances the results may
immediately become invalidated (for example, a garbage collection may
occur, the address in question may become free, the arena may choose to
unmap the address and return storage to the operating system); for
reliable results call this function whilst the arena is parked.
</p>

<h4>Example</h4>


<h4>Error Handling</h4>

<p>Can't fail.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_clamp">mps_arena_park</a> used to park an
arena</code></p>

<h4>Notes</h4>

<p>None.</p>


.. c:function:: extern void mps_arena_park(mps_arena_t arena);


<h4>Summary</h4>

<p><code><a href="#mps_arena_park">mps_arena_park</a></code> puts the specified arena into the parked state.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p><code>arena</code>  the arena to park</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_park">mps_arena_park</a></code> puts the specified arena into the parked state. While an arena is parked,no object motion will occur and the staleness of location dependencies will not change. All references to objects loaded while the arena is parked will keep the same binary representation until after it is released.</p>

<p>Any current collection is run to completion before the arena is parked, and no new collections will start. When an arena is in the parked state, it is necessarily not in the middle of a collection.</p>


<h4>Example</h4>


<h4>Error Handling</h4>

<p>Can't fail.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_clamp">mps_arena_clamp</a></code>,

<code><a href="#mps_arena_release">mps_arena_release</a></code></p>


<h4>Notes</h4>

<p>None.</p>


.. c:function:: extern void mps_arena_release(mps_arena_t);


<h4>Summary</h4>

<p><code><a href="#mps_arena_release">mps_arena_release</a></code> puts the specified arena into the unclamped state.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_release">mps_arena_release</a></code> puts the specified arena into the unclamped state. While an arena is unclamped, garbage collection, object motion, and other background activity can take place.</p>


<h4>Example</h4>


<h4>Error Handling</h4>

<p>Can't fail.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_arena_clamp">mps_arena_clamp</a></code>,

<code><a href="#mps_arena_park">mps_arena_park</a></code></p>


<h4>Notes</h4>

<p>None.</p>


<h3>function <code><a id="mps_arena_roots_walk" name="mps_arena_roots_walk">mps_arena_roots_walk</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_arena_roots_walk">mps_arena_roots_walk</a></code> is used to iterate over all roots of the MPS heap.</p>


<h4>Associated Protocols</h4>

<p>None.</p>


<h4>Syntax</h4>

<p><code>mps_arena_roots_walk(mps_arena, client_step_function, client_step_closure_p,client_step_closure_s);</code></p>


<h4>Type</h4>

<pre>
  extern void mps_arena_roots_walk(mps_arena_t,
  mps_roots_stepper_t, void *,
  size_t);
</pre>


<h4>Arguments</h4>

<p><code>(mps_arena_t mps_arena, mps_roots_stepper_t stepper, void *p, size_t s)</code></p>

<p><code><a href="#mps_arena">mps_arena</a></code> in an MPS arena object.</p>

<p><code>stepper</code> is a client-supplied function (pointer) of the right type (see <code><a href="#mps_roots_stepper_t">mps_roots_stepper_t</a></code>). This function is applied to every reference to the heap from every root object registered with the arena. This function should take the argument list <code>(mps_addr_t *ref, mps_root_t root, void *p, size_t s)</code>. <code>ref</code> is the address of a root which references an object in the arena. <code>root</code> is the registered root (an MPS root object) of which <code>ref</code> is a single reference, <code>p</code> and <code>s</code> are copies of the corresponding values that the client passed into <code><a href="#mps_arena_roots_walk">mps_arena_roots_walk</a></code> originally.</p>

<p><code>p</code> and <code>s</code> are passed into the function specified by the stepper argument whenever the MPS calls that function. See <code><a href="#mps_roots_stepper_t">mps_roots_stepper_t</a></code></p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p><code><a href="#mps_arena_roots_walk">mps_arena_roots_walk</a></code> is used to iterate over all roots of the MPS heap. A client-supplied function is called for every root reference which points to an object in any automatically managed pools; the address of the root reference and the MPS root object are passed to the user supplied function, as well as some closure variables.</p>

<p>May only be called when the arena is in the parked state.</p>

<p>Applies stepper to each reference in any roots registered with the arena and which point to objects in automatically managed pools. If the root has rank <code><a href="#MPS_RANK_AMBIG">MPS_RANK_AMBIG</a></code> then the reference might not be to the start of an object; the client should handle this case. There is no guarantee that the reference corresponds to the actual location that holds the pointer to the object (since this might be a register, for example) - but the actual location will be passed if possible. This may aid analysis of roots via a debugger.</p>

<p></p>


<h4>Example</h4>

<p>[not yet]</p>


<h4>Error Handling</h4>

<p>There are none.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_roots_stepper_t">mps_roots_stepper_t</a></code></p>

<p>

<code><a
href="#mps_arena_formatted_objects_walk">mps_arena_formatted_objects_walk</a></code></p>


<h4>Notes</h4>


.. c:function:: extern size_t mps_arena_spare_commit_limit(mps_arena_t arena)


<h4>Summary</h4>

<p>Retrieves the value of the spare commit limit (previously set with <code><a href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code>).</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p><code><a href="#mps_arena_t">mps_arena_t</a></code> arena</p>

<p>Specifies the arena to retrieve the spare commit limit of.</p>


<h4>Returned Values</h4>

<p>Returns, as a size_t, the value of the spare commit limit.</p>


<h4>Description</h4>

<p>Returns the current value of the spare commit limit which is the value most recently set with <code><a href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code>. (See <code><a href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code> fordetails).</p>


<h4>Example</h4>

<p>[missing]</p>


<h4>Error Handling</h4>

<p>There are no errors.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code></p>


<h4>Notes</h4>

<p>None.</p>


.. c:function:: extern void mps_arena_spare_commit_limit_set(mps_arena_t arena, size_t limit)


<h4>Summary</h4>

<p>Sets the limit of the amount of spare committed memory.</p>


<h4>Associated Protocols</h4>

<p>Arena.</p>


<h4>Arguments</h4>

<p><code><a href="#mps_arena_t">mps_arena_t</a></code> arena</p>

<p>The arena to which the new limit should apply.</p>

<p>size_t limit</p>

<p>The value of the new limit (specified in bytes).</p>


<h4>Resources</h4>

<p>mps.h</p>

<p></p>


<h4>Description</h4>

<p>The limit argument specifies a new "spare commit limit". The spare commit limit specifies the maximum amount of bytes of "spare committed" memory the MPS is allowed to have. Setting it to a value lower than the current amount of spare committed memory would immediately cause sufficient spare committed memory to be uncommitted so as to bring the value under the limit. In particular setting to 0 will mean that the MPS will have no "spare committed" memory.</p>

<p>"spare committed" memory is the term for describing memory which the arena is managing as free memory (so not in use by any pool and not otherwise in use for obscure internal reasons) but which remains committed (mapped from the OS). It is used by the arena to (attempt to) avoid calling the OS to repeatedly unmap and map areas of VM. "spare committed" memory is counted as committed memory as counted by <code><a href="#mps_arena_committed">mps_arena_committed</a></code> and restricted by <code><a href="#mps_arena_commit_limit">mps_arena_commit_limit</a></code>.</p>

<p>Non-VM arenas do not have this concept, but they support the two functions <code><a href="#mps_arena_spare_commit_limit">mps_arena_spare_commit_limit</a></code> and <code><a href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code>. The functions simply get and retrieve a value but do nothing else in that case.</p>

<p>Initially the value is some configuration-dependent value.</p>

<p>The value of the limit can be retrieved with <code><a href="#mps_arena_spare_commit_limit">mps_arena_spare_commit_limit</a></code>.</p>


<h4>Example</h4>

<p>[missing]</p>


<h4>Error Handling</h4>

<p>There are no errors.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_arena_spare_commit_limit">mps_arena_spare_commit_limit</a></code></p>


<h4>Notes</h4>

<p>None.</p>


.. c:function:: size_t mps_arena_spare_committed(mps_arena_t);


<h4>Summary</h4>

<p>Returns the number of bytes of spare committed memory.</p>


<h4>Associated Protocols</h4>

<p>Memory</p>


<h4>Arguments</h4>

<p><code><a href="#mps_arena_t">mps_arena_t</a></code> <code><a href="#mps_arena">mps_arena</a></code></p>

<p>The arena to which the query applies.</p>


<h4>Returned Values</h4>

<p>Returns the number of bytes of spare committed memory.</p>


<h4>Description</h4>

<p>"Spare committed" memory is the term for describing memory which the arena is committed from the OS but which is free (so not in use by any pool and not otherwise in use for obscure internal reasons). It is used by the arena to (attempt to) avoid calling the OS to repeatedly uncommit and commit areas of VM (because calling the OS to commit and uncommit memory is typically expensive)."Spare committed" memory can be used for grant client requests; if this is done when the MPS would otherwise have had to call the OS to commit more memory then the MPS has avoid some OS calls.</p>

<p>"spare committed" memory is counted as part of committed memory. The amount of committed memory can be retrieved with <code><a href="#mps_arena_committed">mps_arena_committed</a></code> (see <code><a href="#mps_arena_committed">mps_arena_committed</a></code>).</p>

<p>The amount of "spare committed" memory can be limited by using <code><a href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code> (see <code><a href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code> ), and the value of that limit can be retrieved with <code><a href="#mps_arena_spare_commit_limit">mps_arena_spare_commit_limit</a></code> (see <code><a href="#mps_arena_spare_commit_limit">mps_arena_spare_commit_limit</a></code> ). This is analogous to the functions for limiting the amount of committed memory.</p>


<h4>Example</h4>

<p>[missing]</p>


<h4>Error Handling</h4>

<p>[missing]</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_arena_spare_commit_limit_set">mps_arena_spare_commit_limit_set</a></code>,

<code><a
href="#mps_arena_spare_commit_limit">mps_arena_spare_commit_limit</a></code></p>


<h4>Notes</h4>

<p>None.</p>


<h3>function <code><a id="mps_arena_unsafe_expose_remember_protection"
name="mps_arena_unsafe_expose_remember_protection">mps_arena_unsafe_expose_remember_protection</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_arena_unsafe_expose_remember_protection">mps_arena_unsafe_expose_remember_protection</a></code> is like <code><a href="#mps_arena_expose">mps_arena_expose</a></code> but additionally indicates that the MPS should remember some internal state which can be used later to avoid slow down.  This function is potentially unsafe and must be used carefully. </p>

<h4>Associated Protocols</h4>

<p>
Arena, clamp, park, protection
</p>

<h4>Syntax</h4>

<p><code>mps_arena_unsafe_expose_remember_protection(mps_arena);</code></p>

<h4>Type</h4>

<pre>
extern void mps_arena_unsafe_expose_remember_protection(mps_arena_t);
</pre>

<h4>Arguments</h4>

<p><code>(mps_arena_t mps_arena)</code></p>

<p><code><a href="#mps_arena">mps_arena</a></code> is an MPS arena object.</p>

<h4>Returned Values</h4>

<p>None.</p>

<h4>Resources</h4>

<p>mps.h</p>

<h4>Description</h4>

<p>
This function does the same as <code><a
href="#mps_arena_expose">mps_arena_expose</a></code> in that it ensures
the MPS is not protecting any page in the arena and also clamps the
arena.
Additionally, using this function indicates to the MPS that it should
remember the protection state internally.
Later on the client should indicate that the remembered protection state
should be restored by using the <code><a
href="#mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code>
function.
Restore the remembered protections is only safe if the contents of the
exposed pages have not been changed; therefore this function should only
be used if you do not intend changing the pages, and the remembered
protection must only be restored if the pages have not been changed.
</p>

<p>
Releasing the arena from the clamped state, by calling <code><a
href="#mps_arena_release">mps_arena_release</a></code>, will cause the MPS to
forget the remembered state.  Restoring the remembered protection state,
using <code><a
href="#mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code>,
will also cause the MPS to forget the remembered state.
</p>

<p>
The MPS will remember the protection state if resources (memory) are
available.
If memory is low then only some or possibly none of the protection state
will be remembered, with a corresponding inability to avoid slow down
later.
It is not possible for the client to tell whether the MPS has in fact
remembered the protection state.
</p>


<h4>Example</h4>

<p>None.</p>

<h4>Error Handling</h4>

<p>There can be no errors.</p>

<h4>See Also</h4>

<code><a href="#mps_arena_clamp">mps_arena_clamp</a></code>,
<code><a href="#mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code>


<h3>function <code><a id="mps_arena_unsafe_restore_protection"
name="mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code> restores the protection state that the MPS remembered when the client called <code><a href="#mps_arena_unsafe_expose_remember_protection">mps_arena_unsafe_expose_remember_protection</a></code>. If used correctly this should avoid any slow down that would otherwise occur. </p>

<h4>Associated Protocols</h4>

<p>
Arena, clamp, park, protection
</p>

<h4>Syntax</h4>

<p><code>mps_arena_unsafe_restore_protection(mps_arena);</code></p>

<h4>Type</h4>

<pre>
extern void mps_arena_unsafe_restore_protection(mps_arena_t);
</pre>

<h4>Arguments</h4>

<p><code>(mps_arena_t mps_arena)</code></p>

<p><code><a href="#mps_arena">mps_arena</a></code> is an MPS arena object.</p>

<h4>Returned Values</h4>

<p>None.</p>

<h4>Resources</h4>

<p>mps.h</p>

<h4>Description</h4>

<p>
This function restores the protection that the MPS has remembered
(during a period when the arena is exposed).
The client can cause the MPS to remember the protection state by using
the <code><a
href="#mps_arena_unsafe_expose_remember_protection">mps_arena_unsafe_expose_remember_protection</a></code>
function.
</p>

<p> The point of remember and restoring the protection state is to avoid the slow down that happens when <code><a href="#mps_arena_expose">mps_arena_expose</a></code> is used.  Normally when this function is used the next garbage collection will be slow because the MPS has to do a lot of work to recover remembered sets; normally the remembered sets are preserved by the MPS protecting the relevant pages, but if the protection is removed then the remembered sets have to be discarded and recomputed. This recomputation of remembered sets can be avoided by using <code><a href="#mps_arena_unsafe_expose_remember_protection">mps_arena_unsafe_expose_remeber_protection</a></code> instead, and using <code><a href="#mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code> to restore the remembered protections instead of recomputing them. </p>

<p>
This function has unsafe aspects.
In order for it to be used safely the client must not have changed the
exposed data between the call to <code><a
href="#mps_arena_unsafe_expose_remember_protection">mps_arena_unsafe_expose_remember_protection</a></code>
and <code><a
href="#mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code>.
If the client has changed the exposed data then
<code><a
href="#mps_arena_unsafe_restore_protection">mps_arena_unsafe_restore_protection</a></code>
must not be called - simply call <code><a
href="#mps_arena_release">mps_arena_release</a></code> to continue normal
collections.
</p>

<p>
Note that this function does not release the arena from the clamped
state;
<code><a href="#mps_arena_release">mps_arena_release</a></code> should be called to continue normal
collections.
</p>

<p>
Calling this function causes the MPS to forget the remember protection
state; as a consequence the same remembered state cannot be restored
more than once.
</p>

<h4>Example</h4>

<p>None.</p>

<h4>Error Handling</h4>

<p>There can be no errors.</p>

<h4>See Also</h4>

<code><a href="#mps_arena_release">mps_arena_release</a></code>,
<code><a href="#mps_arena_unsafe_expose_remember_protection">mps_arena_unsafe_expose_remember_protection</a></code>


<h3>function <code><a id="mps_bool_t" name="mps_bool_t">mps_bool_t</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_bool_t">mps_bool_t</a></code> is a transparent type, equivalent to <code>int</code>, that is used in the MPS C interfaceto indicate that a boolean value is intended.</p>


<h4>Associated Protocols</h4>

<p>Not applicable.</p>


<h4>Syntax</h4>

<p>Not applicable.</p>


<h4>Structure</h4>

<p>Not applicable.</p>


<h4>Type</h4>

<p><code>typedef int mps_bool_t;</code></p>


<h4>Description</h4>

<p>When used as an input parameter to the MPS, a value of 0 indicates "false" and any other value indicates "true". As an output parameter or function return from the MPS, 0 indicates "false",and 1 indicates "true". Note that an <code><a href="#mps_bool_t">mps_bool_t</a></code> value can be used in a conditional context, such as in an "if" statement.</p>


<h4>Example</h4>

<pre>
  if(mps_ld_isstale(&amp;ld, space, obj)) {
    mps_ld_reset(&amp;ld, space);
    mps_ld_add(&amp;ld, space, obj);
  }
</pre>


<h4>See Also</h4>


<h4>Notes</h4>

<p>None.</p>


<h3>function <code><a id="mps_class_amc" name="mps_class_amc">mps_class_amc</a></code></h3>



<p></p>


<h4>Summary</h4>

<p><code><a href="#mps_class_amc">mps_class_amc</a></code> returns the pool class object for the Automatic Mostly Copying pool class.</p>


<h4>Associated Protocols</h4>

<p>Pool</p>


<h4>Syntax</h4>

<p><code>mps_class_t mps_class_amc(void)</code></p>


<h4>Arguments</h4>

<p>No arguments.</p>


<h4>Returned Values</h4>

<p>Returns a pool class object.</p>


<h4>Resources</h4>

<p>mpscamc.h</p>


<h4>Description</h4>

<p>This function returns an object of type <code><a href="#mps_class_t">mps_class_t</a></code> which represents the Automatic MostlyCopying pool class.</p>

<p>This pool class requires an extra argument when used in <code><a href="#mps_pool_create">mps_pool_create</a></code>:</p>

<pre>  res = mps_pool_create(&amp;pool, arena, mps_class_amc(), format); </pre>

<p>The extra argument, format, should be of type <code><a href="#mps_fmt_t">mps_fmt_t</a></code> and specifies the format of the objects allocated in the pool.</p>

<p>An AMC pool is both scannable and collectable. Objects may contain exact references to other objects that will preserve such other objects. Objects may be reclaimed if they are not reachable from a root. Objects may move during collection, unless reachable via a (direct) ambiguous reference. Objects in an AMC pool may be registered for finalization. Exact (that is, non-ambiguous)references into an object in an AMC pool must be to the start of the object.</p>

<p>The AMC pool class exploits assumptions about object lifetimes and inter-connection variously referred to as "the generational hypothesis". In particular, the following tendencies will be efficiently exploited by such a pool:</p>

<p>- Most objects die young;</p>

<p>- Objects that don't die young will live a long time;</p>

<p>- Most references are backwards in time.</p>

<p><code><a href="#mps_ap_frame_push">mps_ap_frame_push</a></code> and <code><a href="#mps_ap_frame_pop">mps_ap_frame_pop</a></code> may be used on an allocation point in an AMC pool.They do not declare the affected objects to be definitely dead (compare with the SNC pool class),but have an undefined effect on the collection strategy.</p>

<p>If an allocation point is created in an AMC pool, the call to <code><a href="#mps_ap_create">mps_ap_create</a></code> will take no additional parameters.</p>

<p></p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code><a href="#mps_ap_frame_pop">mps_ap_frame_pop</a></code>,

<code><a href="#mps_ap_frame_push">mps_ap_frame_push</a></code>,

<code><a href="#mps_ap_create">mps_ap_create</a></code></p>


<h3>function <code><a id="mps_class_mvff" name="mps_class_mvff">mps_class_mvff</a></code></h3>



<p></p>


<h4>Summary</h4>

<p>Used as a parameter to <code><a href="#mps_pool_create">mps_pool_create</a></code> to create an MVFF pool.</p>


<h4>Associated Protocols</h4>

<p>Pool, Allocation Points.</p>


<h4>Type</h4>

<p><code>mps_class_t mps_class_mvff(void)</code></p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>The function returns a class object that can be passed to <code><a href="#mps_pool_create">mps_pool_create</a></code>.</p>


<h4>Resources</h4>

<p>mpscmvff.h</p>


<h4>Description</h4>

<p>MVFF pools implement a first-fit policy.  The pool requires six parameters to pool creation:</p>

<ul>

  <li><p><code><a href="#mps_size_t">mps_size_t</a></code> extendBy -- The size of segment to allocate by default;</p></li>

  <li><p><code><a href="#mps_size_t">mps_size_t</a></code> avgSize -- The average size of objects to be allocated;</p></li>

  <li><p><code><a href="#mps_align_t">mps_align_t</a></code> alignment -- The alignment of addresses for allocation (and freeing) in thepool;</p></li>

  <li><p><code><a href="#mps_bool_t">mps_bool_t</a></code> slotHigh</p></li>

  <li><p><code><a href="#mps_bool_t">mps_bool_t</a></code> arenaHigh</p></li>

  <li><p><code><a href="#mps_bool_t">mps_bool_t</a></code> firstFit</p></li>

</ul>

<p>
  The alignment is the alignment of ranges that can be allocated and freed. If an unaligned size is passed to <code><a href="#mps_alloc">mps_alloc</a></code> or <code><a href="#mps_free">mps_free</a></code>, it will be rounded up to the pool's alignment. The minimum alignment supported by pools of this class is
  <code>
    sizeof(void *)</code>.
</p>

<p>The three boolean parameters may be set to (0, 0, 1) or (1, 1, 1). No other settings of these parameters is currently recommended.</p>

<p>Buffered allocation (<code><a href="#mps_reserve">mps_reserve</a></code> and <code><a href="#mps_commit">mps_commit</a></code>) is also supported, but in that case, the policy is rather different: buffers are filled worst-fit, and allocation is always upwards from the base. The arenaHigh parameter regulates whether new segments are acquired at high or low addresses;the slotHigh and firstFit parameters do not affect buffered allocation. Buffered and unbuffered allocation can be used at the same time, but in that case, the first allocation point must be created before any call to <code><a href="#mps_alloc">mps_alloc</a></code>.</p>

<p>Cached allocation ( <code><a href="#MPS_SAC_ALLOC">MPS_SAC_ALLOC</a></code> and <code><a href="#MPS_SAC_FREE">MPS_SAC_FREE</a></code> ) is also supported, but in that case,the policy is a little different: allocation from the cache follows its own policy (typicallyfirst-fit), and only when the cache needs to acquire more blocks from the underlying MVFF pool does it use the usual algorithm to choose blocks for the cache.</p>


<h4>Example</h4>

<pre>
  if(mps_pool_create(&amp;pool, arena, mps_class_mvff(), 8 * 1024, 135, 4, 0, 0, 1)
     != MPS_RES_OK) {
    printf("Error creating pool!");
    exit(2);
  }
</pre>


<h4>See Also</h4>

<p>

<code><a href="#mps_pool_create">mps_pool_create</a></code>,

<code><a href="#mps_reserve">mps_reserve</a></code>,

<code><a href="#mps_commit">mps_commit</a></code>.</p>


<h4>Notes</h4>

<p>It is usually not advisable to use buffered and unbuffered allocation at the same time,because the worst-fit policy of buffer filling will grab all the large blocks, leading to severe fragmentation. Use two separate pools instead.</p>

<p>Note that using buffered allocation prevents (for obscure technical reasons) the pool from allocating across segment boundaries. This can cause added external fragmentation if objects are allocated that are a significant fraction of the segment size. (This quirk will disappear in a future version.)</p>


<h3>function <code><a id="mps_class_snc" name="mps_class_snc">mps_class_snc</a></code></h3>



<p></p>


<h4>Summary</h4>

<p>Returns the pool class object (of type <code><a href="#mps_class_t">mps_class_t</a></code>) for the Stack No Check pool class.</p>


<h4>Associated Protocols</h4>

<p>Pool.</p>


<h4>Syntax</h4>

<p><code>mps_class_t mps_class_snc(void)</code></p>


<h4>Arguments</h4>

<p>No arguments.</p>


<h4>Returned Values</h4>

<p>Returns a pool class object.</p>


<h4>Resources</h4>

<p>mpscsnc.h</p>


<h4>Description</h4>

<p>This function returns an object of type <code><a href="#mps_class_t">mps_class_t</a></code> which represents the Stack No Check pool class.</p>

<p>This pool class requires an extra argument when used in <code><a href="#mps_pool_create">mps_pool_create</a></code>:</p>

<pre>  res = mps_pool_create(&amp;pool, arena, mps_class_snc(), format); </pre>

<p>The extra argument, format, should be of type <code><a href="#mps_fmt_t">mps_fmt_t</a></code> and specifies the format of the objects allocated in the pool (in a similar way to <code><a href="#mps_class_amc">mps_class_amc</a></code>). The format should provide at least the methods: scan, skip, pad.</p>

<p>An SNC pool is scannable, in that objects may contain references to objects in other pools that will keep those objects alive (depending on rank). In this sense, an SNC pool is a de-facto root.</p>

<p>Exact references may point to (the start of) objects in an SNC pool, but will have no effect on whether those objects are either scanned or kept alive.</p>

<p>If <code><a href="#mps_ap_frame_pop">mps_ap_frame_pop</a></code> is used on an allocation point in an SNC pool (after a corresponding call to <code><a href="#mps_ap_frame_push">mps_ap_frame_push</a></code>), then the objects affected by the pop are effectively declared dead, and may be reclaimed by the collector. Extant references to such objects from reachable or de facto alive objects are safe, but such other objects should be dead; that is, such references must never be used.</p>

<p>If an allocation point is created in an SNC pool, then the call to <code><a href="#mps_ap_create">mps_ap_create</a></code> will take as an additional parameter the rank (of type <code><a href="#mps_rank_t">mps_rank_t</a></code>) of references in the objects to be created in that allocation point. Currently, only rank exact (<code><a href="#mps_rank_exact">mps_rank_exact</a></code>) is supported.</p>

<p>Objects in an SNC pool may not be registered for finalization.</p>

<p>Objects in an SNC pool will not move.</p>

<p></p>


<h4>Example</h4>


<h4>Nya</h4>


<h4>Error Handling</h4>

<p>Cannot fail.</p>

<p></p>


<h4>See Also</h4>

<p>

<code><a href="#mps_class_amc">mps_class_amc</a></code>,

<code><a href="#mps_ap_frame_pop">mps_ap_frame_pop</a></code>,

<code><a href="#mps_ap_frame_push">mps_ap_frame_push</a></code>,

<code><a href="#mps_ap_create">mps_ap_create</a></code></p>


<h3><code><a id="mps_class_mvt" name="mps_class_mvt">mps_class_mvt</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_class_mvt">mps_class_mvt</a></code> is a function that returns the MVT pool class object.</p>


<h4>Associated Protocols</h4>

<p>Allocation point.</p>


<h4>Syntax</h4>

<p><code>mps_class_t mps_class_mvt(void);</code></p>


<h4>Type</h4>

<p>C function</p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>The MVT pool class object.</p>


<h4>Resources</h4>

<p>mpscmv2.h</p>


<h4>Description</h4>

<p>The function <code><a href="#mps_class_mvt">mps_class_mvt</a></code> returns the MVT pool class object, which can be used to create an MVT pool instance by passing the class object as the <code><a href="#mps_class_t">mps_class_t</a></code> (third) argument to <code><a href="#mps_pool_create">mps_pool_create</a></code>.</p>

<p>The MVT pool class manually manages variable-sized, unformatted objects. The MVT pool uses an allocation policy termed "temporal fit". Temporal fit attempts to place consecutive allocations next to each other. It relies on delaying reuse as long as possible to permit freed blocks to coalesce, thus maximizing the number of consecutive allocations that can be co-located. Temporal fit permits a very fast allocator and a deallocator competitive in speed with all other known policies.</p>

<p>
  Temporal fit is intended to take advantage of knowledge of object lifetimes, either
  <cite>
    apriori
  </cite>
  knowledge or knowledge acquired by profiling. The best performance of the MVT pool will be achieved by allocating objects with similar expected deathtimes together.
</p>

<p>A simple policy can be implemented to take advantage of MVT: Object size is typically well-correlated with object life-expectancy, and birthtime plus lifetime gives deathtime, so allocating objects of similar size sequentially from the same pool instance should result in objects allocated close to each other dying at about the same time.</p>

<p>An application that has several classes of objects of widely differing life expectancy will best be served by creating a different MVT pool instance for each life-expectancy class. A more sophisticated policy can use either the programmer's knowledge of the expected lifetime of an objector any characteristic of objects that correlates with lifetime to choose an appropriate pool instance to allocate in.</p>

<p>Allocating objects with unknown or very different deathtimes together will pessimize the space performance of MVT.</p>


<h4>Example</h4>

<pre>
  if(mps_pool_create(&amp;pool, arena, mps_class_mvt(), 8, 32, 256, 70, 20)
     != MPS_RES_OK) {
   printf("Error creating pool!");
   exit(2);
 }
</pre>

<h4>Error Handling</h4>

<p><code><a href="#mps_class_mvt">mps_class_mvt</a></code> cannot result in an error.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_pool_create">mps_pool_create</a></code></p>


<h4>Notes</h4>

<p>
  <strong>
    Creation
  </strong>
</p>

<p>The MVT pool class has five creation parameters:</p>

<pre>
  mps_res_t mps_pool_create(mps_pool_t * pool, mps_arena_t arena,
  mps_class_t mvt_class, size_t minimum_size,
  size_t mean_size, size_t maximum_size,
  mps_count_t reserve_depth mps_count_t fragmentation_limit);
</pre>

<p>Sizes</p>

<p><code>minimum_size</code>, <code>mean_size</code>, and <code>maximum_size</code> are the minimum, mean, and maximum (typical) size in bytes of objects expected to be allocated in the pool. Objects smaller than minimum size may be allocated, but the pool is not guaranteed to manage them space-efficiently. Objects larger than maximum_size may be allocated, but the pool is not guaranteed to manage them space-efficiently.Furthermore, partial freeing is not supported for objects larger than maximum size; doing so will result in the storage of the object never being reused. Mean_size need not be an accurate mean,although the pool will manage mean_size objects more efficiently.</p>

<p>Reserve Depth</p>

<p>reserve_depth is the expected hysteresis of the object population. When pool objects are freed, the pool will retain sufficient storage to allocate reserve_depth objects of mean_size for near term allocations (rather than immediately making that storage available to other pools).</p>

<p>If a pool has a stable object population, one which only grows over the lifetime of the pool, or one which grows steadily and then shrinks steadily, use a reserve_depth of 0.</p>

<p>It is always safe to use a reserve depth of 0, but if the object population typically fluctuates in a range (e.g., the client program may repeatedly create and destroy a subset of objects in a loop), it is more efficient for the pool to retain enough storage to satisfy that fluctuation. For example, if a pool has an object population that typically fluctuates between 8,000and 10,000, use a reserve_depth of 2,000.</p>

<p>The reserve will not normally be available to other pools for allocation, even when it is not used by the pool. If this is undesirable, a reserve depth of 0 may be used for a pool whose object population does vary, at a slight cost in efficiency. The reserve does not guarantee any particular amount of allocation.</p>

<p>Fragmentation Limit</p>

<p>fragmentation_limit is a percentage in (0, 100] that can be used to set an upper limit on the space overhead of MVT in case object deathtimes and allocations do not correlate well.</p>

<p>If the free space managed by the pool as a ratio of all the space managed by the pool exceeds the specified percentage, the pool will fall back to a first fit allocation policy,exploiting space more efficiently at a cost in time efficiency.</p>

<p>A fragmentation_limit of 0 would cause the pool to operate as a first-fit pool, at a significant cost in time-efficiency, therefore is not permitted.</p>

<p>A fragmentation_limit of 100 will cause the pool to use temporal fit (unless resources are exhausted). If the objects allocated in the pool have similar lifetime expectancies, this mode will have the best time- and space-efficiency. If the objects have widely varying lifetime expectancies,this mode will be time-efficient, but may be space-inefficient. An intermediate setting can be used to limit the space-inefficiency of temporal fit due to varying object life expectancies.</p>

<p>
  <strong>
    Allocation
  </strong>
</p>

<p>The MVT pool class only supports allocation through allocation points. See <code><a href="#mps_ap_create">mps_ap_create</a></code>.</p>

<p>
  <strong>
    Deallocation
  </strong>
</p>

<p>The MVT pool class supports explicit freeing. See <code><a href="#mps_pool_free">mps_pool_free</a></code>.</p>


<h4>Internal Notes</h4>

<p>Need a life-expectancy parameter! How else will different instances choose their Loci?</p>

<p>Need an alignment parameter. Perhaps this is embedded in a format parameter (when all pools have at least a null format).</p>

<p>It is conceivable that a client would want to mix manual and automatic pools with the manual pool being able to be a root for the automatic. To do so, MVT would need to support formatted objects and scanning. This may be added someday.</p>

<p>Eventually the MM product will include profiling tools that will help determine object characteristics that correlate with object lifetime and suggest how to configure the appropriate number of MVT pool instances and what characteristics to dispatch on when choosing which instance to allocate from.</p>

<p>[From mail.ptw.1998-08-19.02-33(0) ]</p>

<p>Remember Wilson's statement that the goal of a memory manager is to exploit the regularities in allocation patterns? My intent in the interface parameters is to accept measurable regularities in object populations, then the implementation can exploit them.</p>

<p>Perhaps the pool should accept some description of the mean and deviation of the object sizes, object population, and object lifetimes. Is that what you are getting at? [Reserve_depth is in some sense a deviation.]</p>


.. c:type:: mps_class_t


<h4>Summary</h4>

<p><code><a href="#mps_class_t">mps_class_t</a></code> is the type of pool classes.</p>


<h4>Associated Protocols</h4>

<p>Pool.</p>


<h4>Description</h4>

<p><code><a href="#mps_class_t">mps_class_t</a></code> is the abstract type of pool classes. It is opaque. A pool class may be obtained by calling the class function for the appropriate class, such as <code><a href="#mps_class_amc">mps_class_amc</a></code> for the AMC class. A pool class is used when creating a pool with <code><a href="#mps_pool_create">mps_pool_create</a></code> or <code><a href="#mps_pool_create_v">mps_pool_create_v</a></code>.</p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code><a href="#mps_pool_create">mps_pool_create</a></code>,

<code><a href="#mps_pool_create_v">mps_pool_create_v</a></code></p>


<h4>Notes</h4>

<p><code><a href="#mps_class_s">mps_class_s</a></code> is an incomplete structure type used only to define <code><a href="#mps_class_t">mps_class_t</a></code>.</p>


.. c:function:: mps_res_t mps_finalize(mps_arena_t arena, mps_addr_t *object_ref)


<h4>Summary</h4>

<p>Registers an object for finalization.</p>


<h4>Associated Protocols</h4>

<p>Finalization, message.</p>


<h4>Arguments</h4>

<p>
  <code>arena</code>
  -- the arena in which the object lives
</p>

<p>
  <code>object_ref</code>
  -- a pointer to a reference to the object to be finalized
 </p>


<h4>Returned Values</h4>

<p>A result code.</p>


<h4>Description</h4>

<p>This function registers the specified object for finalization. This object must be an object allocated from a pool in the specified arena. Violations of this constraint may not be checked by the MPS, and may be unsafe (cause the MPS to crash in undefined ways).</p>

<p>An object becomes finalizable if it is registered for finalization and the collector observes that it would otherwise be reclaimable. Once an object is finalizable the MPS may choose to finalize it (by posting a finalization message, see below) at <em>any</em> future time. Note that the subsequent creation of strong references to the object (from, say, weak references) may cause finalization to occur when an object is not otherwise reclaimable. </p>

<p>When an object is finalizable, it may be finalized up to N times, where N is the number of times it has been registered for finalization. When an object is finalized, it is also deregistered for finalization (so that it will not be finalized again from the same registration).</p>

<p>Finalization is performed by passing a finalization message to the client, containing an exact reference to the object. See the message protocol, <code><a href="#mps_message_type_finalization">mps_message_type_finalization</a></code>, and <code><a href="#mps_message_finalization_ref">mps_message_finalization_ref</a></code> for details.</p>

<p>If an object is registered for finalization multiple times, then there may be multiple finalization messages on the queue at the same time. On the other hand it may be necessary to discard previous finalization messages for an object before all such messages are posted on the message queue. In other words a finalization message may prevent other finalizations of the same object from occurring until the message is deleted; or, it may not.  We don't provide any guarantees either way. Clients performing multiple registrations must cope with both behaviors. In any case we expect it to be unusual for clients to register the same object multiple times.</p>

<p>Note that there is no guarantee that finalization will be prompt.</p>

<p><a href="#mps_rank_weak">Weak references</a> do not prevent objects from being finalized.  At the point that an object is finalized, weak references will still validly refer to the object.  The fact that an object is registered for finalization prevents weak references to that object from being deleted.</p>

<p>Note that there will be no attempt to finalize objects in the context of <code><a href="#mps_arena_destroy">mps_arena_destroy</a></code> or <code><a href="#mps_pool_destroy">mps_pool_destroy</a></code>. <code><a href="#mps_pool_destroy">mps_pool_destroy</a></code> should therefore not be invoked on pools containing objects registered for finalization.</p>

<p>Not all pool classes support finalization of objects.  In general only pools that manage objects whose liveness is determined by garbage collection will support finalization of objects.  For more information, see the Pool Class Catalog.</p>


<h4>Example</h4>

<p>[missing]</p>


<h4>Error Handling</h4>

<p>[missing]</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_message_type_finalization">mps_message_type_finalization</a></code>,

<code><a href="#mps_rank_weak">mps_rank_weak</a></code>,

<code><a href="#mps_arena_destroy">mps_arena_destroy</a></code>,

<code><a href="#mps_pool_destroy">mps_pool_destroy</a></code></p>


<h4>Notes</h4>

<p>This function receives a pointer to a reference. This is to avoid placing the restriction on the client that the C call stack be a root.</p>


.. c:type:: mps_fmt_A_s


<h4>Summary</h4>

<p><code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code> is a structure used to create object formats of variant A.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<pre>
typedef struct mps_fmt_A_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_copy_t  copy;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
} mps_fmt_A_s;
</pre>


<h4>Resources</h4>

<p>
  <code class="filename">mps.h</code>
</p>


<h4>Description</h4>

<p>Objects of this type are intended to be used in the creation of object formats. Object formats describe the layout of client objects.</p>

<p><code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code> is a structure that represents the particular collection of methods and values that describes an object format of variant A.</p>

<p>Broadly speaking, the object formats of this variant are suitable for use in copying or moving memory managers.</p>

<p><code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code> has the following methods: <code>scan</code>, <code>skip</code>, <code>copy</code>, <code>fwd</code>, <code>isfwd</code>, <code>pad</code>, and the following value:<code>align</code>.</p>

<p><code>align</code> is an integer value defines the alignment of objects allocated with this format. It should be large enough to satisfy the alignment requirements of any field in the objects,and it cannot be larger than the arena alignment. For details of the methods, consult the reference pages for the type of each method.</p>


<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt my_format;
  mps_res_t res;
  mps_fmt_A_s my_format_A = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_copy, &amp;my_fwd,
                              &amp;my_isfwd, &amp;my_pad };

  res = mps_fmt_create_A(&amp;my_format, arena, &amp;my_format_A);
  assert(res != MPS_RES_OK);

  return my_format;
}
</pre>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_create_A">mps_fmt_create_A</a></code>,

<code><a href="#mps_fmt_scan_t">mps_fmt_scan_t</a></code>,

<code><a href="#mps_fmt_skip_t">mps_fmt_skip_t</a></code>

<code><a href="#mps_fmt_copy_t">mps_fmt_copy_t</a></code>,

<code><a href="#mps_fmt_fwd_t">mps_fmt_fwd_t</a></code>,

<code><a href="#mps_isfwd_t">mps_isfwd_t</a></code>,

<code><a href="#mps_pad_t">mps_pad_t</a></code>,

<code><a href="#mps_align_t">mps_align_t</a></code>,

<code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code></p>


.. c:type:: mps_fmt_A_t


<h4>Summary</h4>

<p><code><a href="#mps_fmt_A_t">mps_fmt_A_t</a></code> is the type pointer to <code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code>.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<pre>
typedef struct mps_fmt_A_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_copy_t  copy;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
} mps_fmt_A_s;

typedef struct mps_fmt_A_s *mps_fmt_A_t;
</pre>


<h4>Description</h4>

<p><code><a href="#mps_fmt_A_t">mps_fmt_A_t</a></code> is the type pointer to <code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code>. A value of this type represents a collection of methods and values that can be used to create a format object of type <code><a href="#mps_fmt_t">mps_fmt_t</a></code>. This type represents a particular collection of methods and values; other collections are represented by other types.</p>

<p>Objects of type <code><a href="#mps_fmt_A_t">mps_fmt_A_t</a></code> are intended to be used in the creation of object formats.Object formats describe the layout of client objects. The function <code><a href="#mps_fmt_create_A">mps_fmt_create_A</a></code> takes an <code><a href="#mps_fmt_A_t">mps_fmt_A_t</a></code> as one of its arguments and creates an object of type <code><a href="#mps_fmt_t">mps_fmt_t</a></code> (an object format).</p>

<p>See the documentation of <code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code> for further details.</p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code>,

<code><a href="#mps_fmt_t">mps_fmt_t</a></code>,

<code><a href="#mps_fmt_create_A">mps_fmt_create_A</a></code></p>


.. c:type:: mps_fmt_B_s


<h4>Summary</h4>

<p><code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code> is a transparent structure used to create object formats of variantB.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<pre>
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
</pre>


<h4>Description</h4>

<p>Objects of this type are intended to be used in the creation of object formats. Object formats describe the layout of client objects. <code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code> is a structure that represents the particular collection of methods and values that describes an object format of variant B.</p>

<p><code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code> is the same as <code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code> except for the addition of the <code><a href="#mps_class">mps_class</a></code> method. Broadly speaking, the object formats of variety B are suitable for use in copying or moving memory managers (just like variety A); the addition of the class method allows more information to be passed to various support tools (such as graphical browsers).</p>

<p><code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code> has the following methods: scan, skip, copy, fwd, isfwd, pad, <code><a href="#mps_class">mps_class</a></code>, and the following value: align.</p>

<p>align is an integer value defines the alignment of objects allocated with this format. It should be large enough to satisfy the alignment requirements of any field in the objects, and it cannot be larger than the arena alignment. For details of the methods, consult the reference pages for the type of each method.</p>


<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt_B_s my_format_B = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_copy,
                              &amp;my_fwd, &amp;my_isfwd, &amp;my_pad, &amp;my_class };
  mps_fmt my_format;
  mps_res_t res;

  res = mps_fmt_create_B(&amp;my_format, arena, &amp;my_format_B);
  assert(res != MPS_RES_OK);

  return my_format;
}
</pre>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_create_B">mps_fmt_create_B</a></code>,

<code><a href="#mps_fmt_scan_t">mps_fmt_scan_t</a></code>,

<code><a href="#mps_fmt_skip_t">mps_fmt_skip_t</a></code>,

<code><a href="#mps_fmt_copy_t">mps_fmt_copy_t</a></code>,

<code><a href="#mps_fmt_fwd_t">mps_fmt_fwd_t</a></code>,

<code><a href="#mps_isfwd_t">mps_isfwd_t</a></code>,

<code><a href="#mps_pad_t">mps_pad_t</a></code>,

<code><a href="#mps_align_t">mps_align_t</a></code>,

<code><a href="#mps_class_t">mps_class_t</a></code>,

<code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code></p>


<h4>Notes</h4>

<p>The <code><a href="#mps_class">mps_class</a></code> field used to be called "class", but that was problematic for C++, so we changed it.</p>


.. c:type:: mps_fmt_B_t


<h4>Summary</h4>

<p><code><a href="#mps_fmt_B_t">mps_fmt_B_t</a></code> is a type passed to <code><a href="#mps_fmt_create_B">mps_fmt_create_B</a></code>. It represents the collection of methods and values used to create a <code><a href="#mps_fmt_t">mps_fmt_t</a></code>. You are expected to declare and create structures of this type if you require an object of type <code><a href="#mps_fmt_B_t">mps_fmt_B_t</a></code>.</p>


<h4>Associated Protocols</h4>

<p>Format</p>


<h4>Structure</h4>

<pre>
typedef struct mps_fmt_B_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_copy_t  copy;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
  mps_fmt_class_t class;
} mps_fmt_B_s;
</pre>


<h4>Type</h4>

<p><code>typedef struct mps_fmt_B_s *mps_fmt_B_t;</code></p>


<h4>Description</h4>

<p><code><a href="#mps_fmt_B_t">mps_fmt_B_t</a></code> is the equivalent to <code><a href="#mps_fmt_A_t">mps_fmt_A_t</a></code> that should be passed to<code><a href="#mps_fmt_create_B">mps_fmt_create_B</a></code>. It is suitable for format variety A collectors that need to use tools that useclass information.</p>

<p>See the documentation for the symbol <code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code> for further details.</p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code>,

<code><a href="#mps_fmt_t">mps_fmt_t</a></code>,

<code><a href="#mps_fmt_create_B">mps_fmt_create_B</a></code>,

<code><a href="#mps_fmt_A_t">mps_fmt_A_t</a></code></p>


<h4>Notes</h4>

<p>None.</p>


.. c:type:: mps_fmt_auto_header_s


<h4>Summary</h4>

<p><code><a href="#mps_fmt_auto_header_s">mps_fmt_auto_header_s</a></code> is a structure used to create object formats of variant auto_header.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<blockquote>
<pre>
typedef struct mps_fmt_auto_header_s {
  mps_align_t     align;
  mps_fmt_scan_t  scan;
  mps_fmt_skip_t  skip;
  mps_fmt_fwd_t   fwd;
  mps_fmt_isfwd_t isfwd;
  mps_fmt_pad_t   pad;
  size_t          mps_headerSize;
} mps_fmt_auto_header_s;
</pre>
</blockquote>


<h4>Resources</h4>

<p>
  <code class="filename">mps.h</code>
</p>


<h4>Description</h4>

<p>Objects of this type are intended to be used in the creation of object formats. Object formats describe the layout of client objects. <code><a href="#mps_fmt_auto_header_s">mps_fmt_auto_header_s</a></code> isa structure that represents the particular collection of methods and values that describes an object format of variant auto_header.</p>

<p>Broadly speaking, the object formats of this variant are suitable for use in automatic memory management for objects with headers (hence the name). More precisely, this variant is intended for formats where the client's pointers point some distance into the memory block containing the object. This typically happens when the objects have a common header used for memory management or class system purposes, but this situation also arises when the low bits of a pointer are used for a tag. The MPS does not care what the reason is, only about the offset of the pointer in relation to the memory block.</p>

<p><code><a href="#mps_fmt_auto_header_s">mps_fmt_auto_header_s</a></code>has the following methods: <code class="source">scan</code>, <code class="source">skip</code>, <code class="source">fwd</code>, <code class="source">isfwd</code>, <code class="source">pad</code>, and the following values: <code class="source">align</code> and <code><a href="#mps_headerSize">mps_headerSize</a></code>.</p>

<p><code class="source">align</code> is an integer value defines the alignment of objects allocated with this format. It should be large enough to satisfy the alignment requirements of any field in the objects, and it cannot be larger than the arena alignment. </p>

<p><code><a href="#mps_headerSize">mps_headerSize</a></code> is the size of the header, i.e., the offset of a client pointer from the base the memory block. For details of the methods, consult the reference pages for the type of each method.</p>


<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt format;
  mps_res_t res;
  mps_fmt_auto_header_s format_desc = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_fwd,
                                        &amp;my_isfwd, &amp;my_pad, HEADER_SIZE };

  res = mps_fmt_create_auto_header(&amp;format, arena, &amp;format_desc);
  assert(res != MPS_RES_OK);

  return format;
}
</pre>


<h4>See Also</h4>

<p>

<code><a
href="#mps_fmt_create_auto_header">mps_fmt_create_auto_header</a></code>,

<code><a href="#mps_fmt_scan_t">mps_fmt_scan_t</a></code>,

<code><a href="#mps_fmt_skip_t">mps_fmt_skip_t</a></code>,

<code><a href="#mps_fmt_fwd_t">mps_fmt_fwd_t</a></code>,

<code><a href="#mps_isfwd_t">mps_isfwd_t</a></code>,

<code><a href="#mps_pad_t">mps_pad_t</a></code>,

<code><a href="#mps_align_t">mps_align_t</a></code>,

<code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code></p>


<h4>Notes</h4>

<p>For technical reasons, client objects must be longer than the header, i.e., objects consisting of only a header are not supported. However, if the header size is larger than or equal to alignment, the pad method must still be able to create padding objects down to alignment size.</p>

<p>At the moment, this format only works with pool classes AMC and AMCZ.</p>


.. c:type:: mps_fmt_class_t


<h4>Summary</h4>

<p><code><a href="#mps_fmt_class_t">mps_fmt_class_t</a></code> is a function pointer type for the class method of a format.</p>


<h4>Associated Protocols</h4>

<p>Format. Telemetry.</p>


<h4>Type</h4>

<p><code>typedef mps_addr_t (*mps_fmt_class_t)(mps_addr_t addr);</code></p>


<h4>Arguments</h4>

<p>addr the address of the object whose class is of interest</p>


<h4>Returned Values</h4>

<p>Returns an address that the client associates with the class or type of the object.</p>


<h4>Description</h4>

<p><code><a href="#mps_fmt_class_t">mps_fmt_class_t</a></code> is t he type of a format's class method. A class method returns an address that is related to the class of the object, for passing on to various support tools (such as graphical browsers).</p>

<p>A class method is provided by the client as part of a format (see Format Protocol).</p>

<p>The exact meaning of the return value is up to the client, but it would typically bear some relation to class or type in the client program. The client may have objects that represent classes or types. These may be associated with strings via <code><a href="#mps_telemetry_intern">mps_telemetry_intern</a></code> and <code><a href="#mps_telemetry_label">mps_telemetry_label</a></code>.</p>


<h4>Example</h4>

<pre>
mps_addr_t my_class_method(mps_addr_t object) {
  my_object_generic_t generic_object = object;
  return (mps_addr_t)(generic_object.class);
}
</pre>


<h4>Error Handling</h4>

<p>A class method is not allowed to fail, but may return NULL.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_t">mps_fmt_t</a></code>,

<code><a href="#mps_fmt_create_B">mps_fmt_create_B</a></code></p>


<h4>Notes</h4>

<p>It is recommended that NULL be returned for padding objects and forwarded objects.</p>


.. c:type:: mps_fmt_copy_t


<h4>Summary</h4>

<p><code><a href="#mps_fmt_copy_t">mps_fmt_copy_t</a></code> is a function pointer type for the copy method of a format.  [Obsolete.  RHSK 2006-06-06]</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<p><code>typedef void (*mps_fmt_copy_t)(mps_addr_t old, mps_addr_t new);</code></p>


<h4>Arguments</h4>

<p>old -- the address of the object</p>

<p>new -- the address to which the object should be copied</p>


<h4>Description</h4>

<p>[Note: <code><a href="#mps_fmt_copy_t">mps_fmt_copy_t</a></code> is obsolete: the MPS does not call this format method.  The MPS simply copies all the bytes to the new location (using the length reported by the skip format method).  RHSK 2006-06-06]</p>

<p><code><a href="#mps_fmt_copy_t">mps_fmt_copy_t</a></code> is a function pointer type for the copy method of a format. A copy method copies an object to a new location. It may be called by the MPS as part of copying garbage collection, for example.</p>

<p>A copy method is required in some formats (in particular formats A and B (see <code><a href="#mps_fmt_A_t">mps_fmt_A_t</a></code> and <code><a href="#mps_fmt_B_t">mps_fmt_B_t</a></code>)). A copy method takes the address of an object and another address, and copies the object to the new address. The new and the old locations are guaranteed not to overlap.</p>


<h4>Example</h4>

<pre>
void my_copy_method(mps_addr_t old, mps_addr_t new)
{
  size_t length = (char*)my_skip_method(old) - (char *)old;
  memcpy(new, old, length);
}
</pre>


<h4>Error Handling</h4>

<p>A copy method is not allowed to fail.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_t">mps_fmt_t</a></code>,

<code><a href="#mps_fmt_create_A">mps_fmt_create_A</a></code>,

<code><a href="#mps_fmt_A_t">mps_fmt_A_t</a></code>,

<code><a href="#mps_fmt_B_t">mps_fmt_B_t</a></code>,

<code><a href="#mps_fmt_create_B">mps_fmt_create_B</a></code></p>


<h4>Notes</h4>

<p>Most pools will just ignore Copy methods, and do the copy themselves.</p>


<h3>function <code><a id="mps_fmt_create_A" name="mps_fmt_create_A">mps_fmt_create_A</a></code></h3>


<h4>Summary</h4>

<p>Function for create a format of variety A.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>

<p></p>


<h4>Syntax</h4>

<p><code>mps_res_t mps_fmt_create_A(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_A_s *fmt_A);</code></p>


<h4>Arguments</h4>

<p>
  <code>fmt_o</code>
  - the address of a variable to hold the new format
</p>

<p>
  <code>arena</code>
  - the arena in which to create the format
</p>

<p>
  <code>fmt_A</code>
  - format description of variety A
 </p>


<h4>Returned Values</h4>

<p>Result status. If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the new format is in <code>*fmt_o</code>.</p>


<h4>Description</h4>

<p>This function creates a format from a user format specification of variety A.</p>


<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt_A_s my_format_A = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_copy,&amp;my_fwd,
    &amp;my_isfwd, &amp;my_pad };
  mps_fmt my_format;
  mps_res_t res;

  res = mps_fmt_create_A(&amp;my_format, arena, &amp;my_format_A);
  if(res != MPS_RES_OK) {
    fprintf(stderr, "Couldn't create format.\n");
    exit(1);
  }

  return my_format;
}
</pre>


<h4>Error Handling</h4>

<p>The MPS may exhaust some resource in the course of <code><a href="#mps_fmt_create_A">mps_fmt_create_A</a></code> and will return an appropriate error code in such circumstances.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code>,

<code><a href="#mps_fmt_t">mps_fmt_t</a></code>,

<code><a href="#mps_fmt_create_B">mps_fmt_create_B</a></code></p>


<h3>function <code><a id="mps_fmt_create_B" name="mps_fmt_create_B">mps_fmt_create_B</a></code></h3>


<h4>Summary</h4>

<p>Function for create a format of variety B.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>

<p></p>


<h4>Syntax</h4>

<p><code>mps_res_t mps_fmt_create_B(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_B_s *fmt_B);</code></p>


<h4>Arguments</h4>

<p><code>arena</code> - the arena in which to create the format</p>

<p><code>fmt_B</code> - format description of variety B</p>


<h4>Returned Values</h4>

<p>Result status. If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the new format is in<code>*fmt_o</code>.</p>

<p></p>


<h4>Description</h4>

<p>This function creates a format from a user format specification of variety B. It is very similar to <code><a href="#mps_fmt_create_A">mps_fmt_create_A</a></code>.</p>


<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt_B_s my_format_B = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_copy,
                              &amp;my_fwd, &amp;my_isfwd, &amp;my_pad, &amp;my_class };
  mps_fmt my_format;
  mps_res_t res;

  res = mps_fmt_create_B(&amp;my_format, arena, &amp;my_format_B);
  assert(res != MPS_RES_OK);

  return my_format;
}
</pre>


<h4>Error Handling</h4>

<p>The MPS may exhaust some resource in the course of <code><a href="#mps_fmt_create_B">mps_fmt_create_B</a></code> and will return an appropriate error code in such circumstances.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code>,

<code><a href="#mps_fmt_t">mps_fmt_t</a></code>,

<code><a href="#mps_fmt_create_A">mps_fmt_create_A</a></code></p>


.. c:function:: mps_res_t mps_fmt_create_auto_header(mps_fmt_t *fmt_o, mps_arena_t arena, mps_fmt_auto_header_s *fmt_st);


<h4>Summary</h4>

<p>Function for create a format of variety auto_header.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Arguments</h4>

<p>
  <code>fmt_o</code>
  - the address of a variable to hold the new format
</p>

<p>
  <code>arena</code>
  - the arena in which to create the format
</p>

<p>
  <code>fmt_st</code>
  - format description of variety auto_header
 </p>


<h4>Returned Values</h4>

<p>Result status. If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the new format is in <code>*fmt_o</code>.</p>


<h4>Description</h4>

<p>This function creates a format from a user format specification of variety auto_header.</p>


<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt_auto_header_s format_desc = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_fwd,
    &amp;my_isfwd, &amp;my_pad, HEADER_SIZE };
  mps_fmt format;
  mps_res_t res;

  res = mps_fmt_create_auto_header(&amp;format, arena, &amp;format_desc);
  assert(res != MPS_RES_OK);

  return format;
}
</pre>


<h4>Error Handling</h4>

<p>The MPS may exhaust some resource in the course of <code><a href="#mps_fmt_create_auto_header">mps_fmt_create_auto_header</a></code>and will return an appropriate error code in such circumstances.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_fmt_auto_header_s">mps_fmt_auto_header_s</a></code>,

<code><a href="#mps_fmt_t">mps_fmt_t</a></code>,

<code><a href="#mps_fmt_create_A">mps_fmt_create_A</a></code></p>


.. c:type:: mps_fmt_fwd_t


<h4>Summary</h4>

<p>The type of a format's forward method.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<p><code>typedef void (*mps_fmt_fwd_t)(mps_addr_t old, mps_addr_t new);</code></p>


<h4>Arguments</h4>

<p>old</p>

<p>the address of an object</p>

<p>new</p>

<p>the address where the object has been moved</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p><code><a href="#mps_fmt_fwd_t">mps_fmt_fwd_t</a></code> is the type of a format's forward method. A forward method is used to store relocation information in a heap. It may be called by the MPS as part of copying garbage collection.</p>

<p>A forward method is provided by the client as part of a format (see Format Protocol ). TheMPS calls a forward method when it has relocated an object. The forward method when called must replace the object at 'old' with a forwarding marker that points to the address 'new'. The forwarding marker must meet the following requirements:</p>

<ul>

  <li><p>it must be possible for the MPS to call other format methods with the address of a forwarding marker as the argument.</p></li>

  <li><p>he forwarding marker must not be bigger than the original object.</p></li>

  <li><p>t must be possible to distinguish the forwarding marker from ordinary objects using the isfwd method (see <code><a href="#mps_fmt_isfwd_t">mps_fmt_isfwd_t</a></code>), and the isfwd method must return the address'new'.</p></li>

</ul>


<h4>Example</h4>

<pre>
/* define the function */

void example_fwd(mps_addr_t old, mps_addr_t new)
{
  /* ... */
}

/* also define example_scan, example_skip, etc */
/* store pointer to function in the format variant struct */
struct mps_fmt_B_s example_fmt_B = {
  4, /* align */
  example_scan,
  example_skip,
  example_copy,
  example_fwd,
  example_isfwd,
  example_pad,
  example_class
};

/* The (address of the) example_fmt_B object can now be passed to */
/* mps_fmt_create_B to create a format. */
</pre>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code>,

<code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code>,

<code><a
href="#mps_fmt_auto_header_s">mps_fmt_auto_header_s</a></code>,

<code><a href="#mps_fmt_isfwd_t">mps_fmt_isfwd_t</a></code></p>


<h4>Notes</h4>

<p>This method is never invoked by the GC on an object in a non-moving pool.</p>


.. c:type:: mps_fmt_isfwd_t


<h4>Summary</h4>

<p>The type of a format's isfwd ("is forwarded") method.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<p><code>typedef mps_addr_t (*mps_fmt_isfwd_t)(mps_addr_t addr);</code></p>


<h4>Arguments</h4>

<p>addr</p>

<p>the address of a candidate object</p>


<h4>Returned Values</h4>

<p>
  Either a null pointer to indicate the object at
  <code>addr</code>
  has not been relocated, orthe new location of the object if there is a forwarding marker at
  <code>addr</code>
  indicating thatthe object has been relocated.
</p>


<h4>Description</h4>

<p>The type of a format's isfwd ("is forwarded") method. An isfwd method is used to test whether an object has been relocated using the format's forward method.</p>

<p>An isfwd method is provided by the client as part of a format (see protocol.mps.format(0) ).The MPS calls the isfwd method to determine whether an object in the heap has been relocated or not.Objects in the heap are relocated using the format's forward method (see <code><a href="#mps_fmt_fwd_t">mps_fmt_fwd_t</a></code>). When the isfwd method is called the parameter addr will be the address of either an object or a forwarding marker created with the forward method. If it is an object (so it has not been relocated)the method should return a null pointer; otherwise it is a forward marker indicating the address of the relocated object, the address of the relocated object should be returned (this should be the same as the 'new' parameter that was passed to the forward method that created the forwarding marker).</p>


<h4>Example</h4>

<p>&lt;example of how to use the symbol&gt;</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code>,

<code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code>,

<code><a
href="#mps_fmt_auto_header_s">mps_fmt_auto_header_s</a></code>,

<code><a href="#mps_fmt_fwd_t">mps_fmt_fwd_t</a></code></p>


<h4>Notes</h4>

<p>This method is never invoked by the GC on an object in a non-moving pool.</p>


.. c:type:: mps_fmt_pad_t


<h4>Summary</h4>

<p>The type of a format's pad method.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<p><code>typedef void (*mps_fmt_pad_t)(mps_addr_t addr, size_t size);</code></p>


<h4>Arguments</h4>

<p>addr</p>

<p>The address at which to create a padding object.</p>

<p>size</p>

<p>The size (in bytes) of the padding object to be created.</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p>The type of a format's pad method. A pad method is used to create padding objects.</p>

<p>A pad method is provided by the client as part of a format (see Format Protocol ). The MPS calls a pad method when it wants to create a padding object. Typically the MPS creates padding objects to fill in otherwise unused gaps in memory; they allow the MPS to pack objects in fixed-size units (such as OS pages). The pad method should create a padding object of the specified size at the specified address. The size can be any aligned (to the format alignment) size. A padding object should be acceptable to other methods in the format (scan, skip, isfwd, etc.).</p>


<h4>Example</h4>

<p>&lt;example of how to use the symbol&gt;</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code>,

<code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code></p>


.. c:type:: mps_fmt_scan_t


<h4>Summary</h4>

<p>Type of the scan method of a format.</p>


<h4>Associated Protocols</h4>

<p>Format, Scanning.</p>


<h4>Syntax</h4>

<p><code>typedef mps_res_t (*mps_fmt_scan_t)(mps_ss_t scan_state, mps_addr_t base, mps_addr_t limit)</code></p>


<h4>Arguments</h4>

<p>
  <code>scan_state</code>
  a scan state
</p>

<p>
  <code>base</code>
  a client pointer to the first object in the block to be scanned
</p>

<p>
  <code>limit</code>
  a client pointer to the object just beyond the end of the block
</p>


<h4>Returned Values</h4>

<p>A result code.</p>


<h4>Description</h4>

<p>This is the type of scanning functions provided by the client in some format variants and <code><a href="#mps_root_create_fmt">mps_root_create_fmt</a></code>. When the MPS needs to scan objects in an area of memory that this scanning function has been registered for, it will be called with a scan state and the limits of the block of objects to scan. It must then indicate references within the objects by using<code><a href="#mps_fix">mps_fix</a></code> or one of the alternatives.</p>

<p>The <code>base</code> and <code>limit</code> arguments are client pointers, as usual. Note that there might not be any object at the location indicated by <code>limit</code>.</p>


<h4>Example</h4>

<pre>
/* Scanner for a simple Scheme-like language with just two interesting types */

mps_res_t scan_objs(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  mps_res_t res;
  mps_addr_t obj;

  MPS_SCAN_BEGIN(ss)
  for(obj = base; obj &lt; limit;) { /* obj maps over the objects to scan */
    switch(((Object*)obj)-&gt;type) {
    case ArrayType:
      {
        size_t i;
        Array *array = (Array *)obj;

        for(i = 0; i &lt; array-&gt;length; ++i) { /* fix each element */
          res = MPS_FIX12(ss, &amp;array-&gt;contents[i]);
          if(res != MPS_RES_OK) return res;
        }

        obj = AddrAdd(obj, ArraySize(array)); /* move to next object */
        break;
      }

    case StackFrameType:
      {
        StackFrame *frame = (StackFrame *)obj;
        for(i = frame-&gt;size; i &gt; 0; --i) { /* fix each local var */
          res = MPS_FIX12(ss, &amp;frame-&gt;locals[i]);
          if(res != MPS_RES_OK) return res;
        }

        res = MPS_FIX12(ss, &amp;frame-&gt;next);
        if(res != MPS_RES_OK) return res;
        obj = AddrAdd(obj, StackFrameSize(frame));
        break;
      }

    default: /* other types don't contain references */
      obj = AddrAdd(obj, DefaultSize(obj));
      break;

    }
  }
  MPS_SCAN_END(ss);

  return res;
}
</pre>


<h4>Error Handling</h4>

<p>If a fixing operation returns a value other than <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the scanning function must return that value, and may return without scanning further references. Generally, itis better if it returns as soon as possible. If the scanning is completed successfully, the function should return <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code>,

<code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code>,

<code><a
href="#mps_fmt_auto_header_s">mps_fmt_auto_header_s</a></code>,

<code><a href="#mps_root_create_fmt">mps_root_create_fmt</a></code>,

<code><a href="#mps_fix">mps_fix</a></code>,

<code><a href="#MPS_FIX12">MPS_FIX12</a></code>,

<code><a href="#MPS_FIX1">MPS_FIX1</a></code>,

<code><a href="#MPS_FIX2">MPS_FIX2</a></code>,

<code><a href="#MPS_FIX_CALL">MPS_FIX_CALL</a></code>,

<code><a href="#MPS_SCAN_BEGIN">MPS_SCAN_BEGIN</a></code>,

<code><a href="#MPS_SCAN_END">MPS_SCAN_END</a></code></p>


.. c:type:: mps_fmt_skip_t


<h4>Summary</h4>

<p><code><a href="#mps_fmt_skip_t">mps_fmt_skip_t</a></code> is a function pointer type for the skip method of a format.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<p><code>typedef mps_addr_t (*mps_fmt_skip_t)(mps_addr_t obj);</code></p>


<h4>Arguments</h4>

<p>
  <code>obj</code>
  the client pointer to the object to be skipped
</p>


<h4>Returned Values</h4>

<p>The skip method should return the address of the next object.</p>


<h4>Description</h4>

<p><code><a href="#mps_fmt_skip_t">mps_fmt_skip_t</a></code> is a function pointer type for the skip method of a format.</p>

<p>These methods are provided by the client as part of a format and invoked by the MPS (seeFormat Protocol). The skip method takes the client pointer to the object. The method should return the client pointer to the next object, whether there is one or not. With no headers, this is the address just past the end of this object; with headers, it's the address just past where the header of next object would be. It is always the case that the difference between the argument and the return value is the size of the block containing the object.</p>


<h4>Example</h4>

<pre>
mps_addr_t my_skip_method(mps_addr_t object)
{
  char *p = (char *)object;
  my_object_t my_object = (my_object_t)object;
  return((mps_addr_t)(p + my_object-&gt;length));
}
</pre>


<h4>Error Handling</h4>

<p>A skip method is not allowed to fail.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_A_s">mps_fmt_A_s</a></code>,

<code><a href="#mps_fmt_B_s">mps_fmt_B_s</a></code>,

<code><a
href="#mps_fmt_auto_header_s">mps_fmt_auto_header_s</a></code></p>


.. c:type:: mps_fmt_t


<h4>Summary</h4>

<p><code><a href="#mps_fmt_t">mps_fmt_t</a></code> is the type of object formats.</p>


<h4>Associated Protocols</h4>

<p>Format.</p>


<h4>Type</h4>

<p><code>typedef struct mps_fmt_s *mps_fmt_t;</code></p>

<p><code><a href="#mps_fmt_s">mps_fmt_s</a></code> is an incomplete structure type used only to declare the opaque type <code><a href="#mps_fmt_t">mps_fmt_t</a></code>.</p>


<h4>Description</h4>

<p><code><a href="#mps_fmt_t">mps_fmt_t</a></code> is the opaque type of object formats. An object format is a way for the MPS and client programs to communicate regarding the layout of client objects. For more information, seeFormat Protocol.</p>


<h4>Example</h4>

<pre>
#include "mps.h"
#include "mpscamc.h"
#include &lt;stdlib.h&gt;

struct mps_fmt_A_s fmt_A_s = {
  (mps_align_t)4,
  scan, skip, copy, move, isMoved, pad
};

void go(mps_space_t space)
{
  mps_fmt_t format;
  mps_res_t res;
  mps_pool_t pool;

  res = mps_fmt_create_A(&amp;format, space, &amp;mps_fmt_A_s);
  if(res != MPS_RES_OK)
    abort();

  res = mps_pool_create(&amp;pool, space, mps_class_amc(), format);
  if(res != MPS_RES_OK)
    abort();

  /* do some stuff here */

  mps_pool_destroy(pool);
  mps_format_destroy(format);
}
</pre>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_create_A">mps_fmt_create_A</a></code>,

<code><a href="#mps_fmt_create_B">mps_fmt_create_B</a></code>,

<code><a href="#mps_fmt_destroy">mps_fmt_destroy</a></code>,

<code><a href="#mps_fmt_A_t">mps_fmt_A_t</a></code></p>


.. c:type:: mps_formatted_objects_stepper_t


<h4>Summary</h4>

<p>Type of the client supplied heap walker function.</p>


<h4>Associated Protocols</h4>

<p>Heap walking.</p>


<h4>Type</h4>

<p><code>typedef void (*mps_formatted_objects_stepper_t)(mps_addr_t, mps_fmt_t, mps_pool_t, void *,size_t )</code></p>


<h4>Arguments</h4>

<p><code><a href="#mps_formatted_objects_stepper_t">mps_formatted_objects_stepper</a></code> is a type not a function so it doesn't take any arguments; however the function pointed to by an object of this type does.  Such functions take the following argument list:</p>

<p><code>(mps_addr_t object, mps_fmt_t format, mps_pool_t pool, void *p, size_t s)</code></p>

<p><code>object</code> is a pointer to the (client) object.</p>

<p><code>format</code> is the MPS format of the client object.</p>

<p><code>pool</code> in the MPS pool in which the client object resides.</p>

<p><code>p</code> and <code>s</code> are two closure values which are copies of the corresponding values which the client passed into the heap walking function, <code><a href="#mps_arena_formatted_objects_walk">mps_arena_formatted_objects_walk</a></code>.</p>


<h4>Returned Values</h4>

<p>The function pointed to by an object of type <code><a href="#mps_formatted_objects_stepper_t">mps_formatted_objects_stepper_t</a></code> returns no arguments.</p>


<h4>Description</h4>

<p>This symbol describe the type of pointers passed into the heap walking function <code><a href="#mps_arena_formatted_objects_walk">mps_arena_formatted_objects_walk</a></code>. The heap walker arranges to apply this function to all objects on the heap, see <code><a href="#mps_arena_formatted_objects_walk">mps_arena_formatted_objects_walk</a></code> for details.</p>


<h4>Example</h4>

<p>&lt;example of how to use the symbol&gt;</p>


<h4>Error Handling</h4>

<p>The function pointed to by an object of type <code><a href="#mps_formatted_objects_stepper_t">mps_formatted_objects_stepper_t</a></code> have no way to return an error code to the caller.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_arena_formatted_objects_arena_walk">mps_arena_formatted_objects_arena_walk</a></code></p>


<h4>Notes</h4>


.. c:function:: void mps_free(mps_pool_t pool, mps_addr_t p, size_t size);


<h4>Summary</h4>

<p>Frees a block of memory to a pool.</p>


<h4>Associated Protocols</h4>

<p>Allocation</p>


<h4>Arguments</h4>

<p><code>pool</code> the pool of the object to be freed</p>

<p><code>p</code> a pointer to the object to the freed</p>

<p><code>size</code> the size of the object to the freed in bytes</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p>Frees an object of memory, returning the memory block to the pool it was allocated from.The pool might then decide to make it available to other pools, but the way this happens depends onthe pool class and the current situation.</p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code><a href="#mps_alloc">mps_alloc</a></code></p>


<h4>Notes</h4>

<p><code><a href="#mps_free">mps_free</a></code> takes a size argument, because it is most efficient to do so. In practical programs, the type of an object is usually known at the point in the code that calls the deallocation function, and hence the size is trivially available. In such cases. storing the size on the MPS side would cost time and memory, and make it hard to get good virtual memory behaviour (as it is, the deallocation code doesn't have to touch the dead object at all).</p>

<p>Undoubtedly, one day, we'll get around to writing a pool that stores the size of each object.</p>


.. c:function:: int mps_lib_memcmp(const void *s1, const void *s2, size_t n);


<h4>Summary</h4>

<p>A plinth function similar to C's "memcmp".</p>


<h4>Associated Protocols</h4>

<p>Plinth</p>


<h4>Arguments</h4>

<p>s1, s2 pointers to memory blocks to be compared</p>

<p>n length of the blocks, in bytes</p>


<h4>Returned Values</h4>

<p>An integer that is greater than, equal to, or less than zero, accordingly as the block pointed to by "s1" is greater than, equal to, or less than the block pointer to by "s2".</p>


<h4>Resources</h4>

<p>mpslib.h</p>


<h4>Description</h4>

<p>This function is intended to have the same semantics as the "memcmp" function of the ANSI C standard (section 7.11.4.1).</p>

<p>Like other plinth features, it is used by the MPS and provided by the client (possibly using the ANSI plinth, mpsliban.c).</p>


<h4>Example</h4>

<p>None, clients don't use it.</p>


<h4>Error Handling</h4>

<p>None.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_lib_memset">mps_lib_memset</a></code>,

<code><a href="#mps_lib_memcpy">mps_lib_memcpy</a></code>,
mpsliban.c</p>


<h4>Notes</h4>

<p>None.</p>


<h3>function <code><a id="mps_lib_memcpy" name="mps_lib_memcpy">mps_lib_memcpy</a></code></h3>


<h4>Summary</h4>

<p>A plinth function similar to C's "memcpy".</p>


<h4>Associated Protocols</h4>

<p>Plinth</p>


<h4>Syntax</h4>

<p><code>void *mps_lib_memcpy(void *dest, const void *source, size_t n);</code></p>


<h4>Arguments</h4>

<p>dest destination of copy</p>

<p>source source of copy</p>

<p>n length of the blocks, in bytes</p>


<h4>Returned Values</h4>

<p>Returns the value of the dest argument.</p>


<h4>Resources</h4>

<p>mpslib.h</p>


<h4>Description</h4>

<p>This function is intended to have the same semantics as the "memcpy" function of the ANSI C standard (section 7.11.2.1).</p>

<p>Like other plinth features, it is used by the MPS and provided by the client (possibly using the ANSI plinth, mpsliban.c).</p>


<h4>Example</h4>

<p>None, clients don't use it.</p>


<h4>Error Handling</h4>

<p>None.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_lib_memset">mps_lib_memset</a></code>,

<code><a href="#mps_lib_memcmp">mps_lib_memcmp</a></code>,
mpsliban.c</p>


<h4>Notes</h4>

<p>None.</p>


<h3>function <code><a id="mps_lib_memset" name="mps_lib_memset">mps_lib_memset</a></code></h3>


<h4>Summary</h4>

<p>A plinth function similar to C's "memset".</p>


<h4>Associated Protocols</h4>

<p>Plinth</p>


<h4>Syntax</h4>

<p><code>void *mps_lib_memset(void *s, int c, size_t n);</code></p>


<h4>Arguments</h4>

<p>s destination of copy</p>

<p>c byte (when converted to an unsigned char) to copy</p>

<p>n length of the block, in bytes</p>


<h4>Returned Values</h4>

<p>Returns the value of s.</p>


<h4>Resources</h4>

<p>mpslib.h</p>


<h4>Description</h4>

<p>This function is intended to have the same semantics as the "memset" function of the ANSI C standard (section 7.11.6.1).</p>

<p>Like other plinth features, it is used by the MPS and provided by the client (possibly using the ANSI plinth, mpsliban.c).</p>


<h4>Example</h4>

<p>None, clients don't use it.</p>


<h4>Error Handling</h4>

<p>None.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_lib_memcpy">mps_lib_memcpy</a></code>,

<code><a href="#mps_lib_memcmp">mps_lib_memcmp</a></code>,
mpsliban.c</p>


<h4>Notes</h4>

<p>None.</p>


<h3><code><a id="mps_lib_telemetry_control" name="mps_lib_telemetry_control">mps_lib_telemetry_control</a></code></h3>


<h4>Summary</h4>

<p>Plinth function to supply a default value for telemetry filters from environment.</p>


<h4>Associated Protocols</h4>

<p>Telemetry</p>


<h4>Type</h4>

<p><code>unsigned long mps_lib_telemetry_control();</code></p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Initial/Default Values</h4>

<p>In the absence of environmental data, a default of zero is recommended.</p>


<h4>Returned Values</h4>

<p>The default value of the telemetry filter, as derived from the environment. It is recommended that the environment be consulted for a symbol analogous to <code><a href="#MPS_TELEMETRY_CONTROL">MPS_TELEMETRY_CONTROL</a></code>, subject to local restrictions.</p>


<h4>Resources</h4>

<p>Depends on access to the environment.</p>


<h4>Description</h4>

<p>See <code><a href="#mps_telemetry_control">mps_telemetry_control</a></code> for more information on the significant of the values.</p>

<p></p>


<h4>Example</h4>

<p>See the supplied ANSI plinth for an example implementation.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_telemetry_control">mps_telemetry_control</a></code></p>


.. c:function:: mps_clock_t mps_message_clock(mps_arena_t arena, mps_message_t message)


<h4>Summary</h4>

<p><code><a href="#mps_message_clock">mps_message_clock</a></code> returns the time at which the MPS posted the message (only for certain message types).</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Arguments</h4>

<p>
  <code>arena</code>
  -- the arena
</p>

<p>
  <code>message</code>
  -- any message retrieved with <code><a href="#mps_message_get">mps_message_get</a></code> and not yet discarded
</p>


<h4>Returned Values</h4>

<p>For supported message types: the time at which the MPS posted the message.  For other message types: zero.</p>


<h4>Description</h4>

<p>Messages are asynchronous: they are posted by the MPS, wait on a queue, and are later collected by the client.  Each message (of supported types) records the time that it was posted, and this is what <code><a href="#mps_message_clock">mps_message_clock</a></code> returns.</p>

<p>The time returned is the <code><a href="#mps_clock_t">mps_clock_t</a></code> value returned by the library function <code><a href="#mps_clock">mps_clock</a></code> at the time the message was posted.  You can subtract one clock value from another to get the time interval between the posting of two messages.</p>

<p>Only the following supported message types record the time of posting:</p>
<ul>
  <li><code><a href="#mps_message_type_gc">mps_message_type_gc</a></code>;</li>
  <li><code><a href="#mps_message_type_gc_start">mps_message_type_gc_start</a></code>.</li>
</ul>

<p>For other message types, the value returned is always zero.</p>


<h4>Example</h4>

<pre>
  mps_message_t message;
  mps_clock_t posted_at;

  if(mps_message_get(&amp;message, arena, mps_message_type_gc_start())) {
    posted_at = mps_message_clock(arena, message);
    printf("Collection started at %ul.\n", (unsigned long)posted_at);
  }
</pre>


<h4>Error Handling</h4>

<p>Can't fail.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_message_type">mps_message_type</a></code></p>


<h4>Notes</h4>

<p>The example ANSI plinth, mpsliban.c, implements <code><a href="#mps_clock">mps_clock</a></code> by calling ISO C time.h's clock().  The difference between two of these clock values may be converted to seconds by dividing by ISO C time.h's CLOCKS_PER_SEC conversion factor.</p>


.. c:function:: void mps_message_discard(mps_arena_t arena, mps_message_t message)


<h4>Summary</h4>

<p><code><a href="#mps_message_discard">mps_message_discard</a></code> is used to indicate that the client is done with the message and the MPS can now reclaim any storage associated with the message.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Arguments</h4>

<p>
  <code>arena</code>
  -- the arena
</p>

<p>
  <code>message</code>
  -- the message
</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p><code><a href="#mps_message_discard">mps_message_discard</a></code> is used to indicate that the client has no further use for the specified message in the specified arena. After this call, the message is invalid and should not be passed as argument to any message functions.</p>

<p>Messages are essentially manually managed.  This call allows the MPS to reclaim storage associated with messages.  If the client does not discard their messages then the resources used may grow without bound. </p>

<p>As well as consuming resources, messages may have other visible effects that require them to be tidied by calling this function.  In particular <a href="#mps_message_type_finalization">finalization messages</a> refer to their finalized object, and will prevent the object from being reclaimed (subject to the usual garbage collection liveness analysis).  A finalized object cannot possibly be reclaimed until its corresponding finalization messages have been discarded (all such messages in the case of multiple messages for the same object). </p>


<h4>Example</h4>

<p>[missing]</p>


<h4>Error Handling</h4>

<p>Can't fail.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_message_get">mps_message_get</a></code></p>


<h4>Notes</h4>

<p>None.</p>


.. c:function:: void mps_message_finalization_ref(mps_addr_t *object_ref, mps_arena_t arena, mps_message_tmessage)


<h4>Summary</h4>

<p><code><a href="#mps_message_finalization_ref">mps_message_finalization_ref</a></code> returns the "finalization reference" property of the specified message in the specified arena.</p>


<h4>Associated Protocols</h4>

<p>Message, finalization.</p>


<h4>Arguments</h4>

<p>object_ref -- a pointer to a reference to the object to which the message pertains</p>

<p>arena -- the arena that the message is in</p>

<p>message -- a message of a message type that supports this method</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p>This method returns the "finalization reference" property of the specified message in the specified arena. The message must be of a message type that supports this method; currently, the only such type is that of finalization messages, as returned by <code><a href="#mps_message_type_finalization">mps_message_type_finalization</a></code>.</p>

<p>The reference returned by this method is a reference to the object that was originally registered for finalization (by a call to <code><a href="#mps_finalize">mps_finalize</a></code>).</p>

<p>Note that the reference returned is subject to the normal constraints, such as might be imposed by a moving collection, if appropriate. For this reason, it is returned indirectly via "object_ref" to enable the client to place it directly into scanned memory, without imposing the restriction that the C stack be a root.</p>

<p>The message itself is not affected by invoking this method.  Until the client calls <code><a href="#mps_message_discard">mps_message_discard</a></code> to discard the message it will refer to the object and prevent its reclamation. </p>


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>

<p>

<code>mps_message_*</code>,
<code><a
href="#mps_message_type_finalization">mps_message_type_finalization</a></code>,
<code><a href="#mps_finalize">mps_finalize</a></code></p>


.. c:function:: size_t mps_message_gc_condemned_size(mps_arena_t arena, mps_message_tmessage)


<h4>Summary</h4>

<p><code><a href="#mps_message_gc_condemned_size">mps_message_gc_condemned_size</a></code> returns the "condemned size" property of the specified message in the specified arena.</p>


<h4>Associated Protocols</h4>

<p>Message, GC.</p>


<h4>Arguments</h4>

<p><code class="source"> arena</code>-- the arena</p>

<p>
  <code class="source">
    message</code>-- a message of a message type that supports this method
</p>


<h4>Returned Values</h4>

<p>An approximate size for the set of objects condemned in the collection that generated the message.</p>


<h4>Resources</h4>

<p>
  <code class="filename">mps.h</code>
</p>


<h4>Description</h4>

<p>Currently, the only type of message that supports this property is <code><a href="#mps_message_type_gc">mps_message_type_gc</a></code>, such messages are generated whenever a garbage collection completes. This method returns an approximation to the size of the set of objects that were condemned in that collection.</p>


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>


<h3>function <code><a id="mps_message_gc_live_size" name="mps_message_gc_live_size">mps_message_gc_live_size</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_message_gc_live_size">mps_message_gc_live_size</a></code> returns the "live size" property of the specified message in the specified arena.</p>


<h4>Associated Protocols</h4>

<p>Message, GC.</p>


<h4>Syntax</h4>

<p><code class="source"> size_t mps_message_gc_live_size(mps_arena_t arena, mps_message_t message)</code></p>


<h4>Arguments</h4>

<p><code class="source">arena</code> -- the arena;</p>

<p><code class="source">message</code> -- a message of a message type that supports this method.</p>


<h4>Returned Values</h4>

<p>The total size of the condemned objects that survived the collection that generated the message.</p>


<h4>Resources</h4>

<p>
  <code class="filename">mps.h</code>
</p>


<h4>Description</h4>

<p>Currently, the only type of message that supports this property is <code><a href="#mps_message_type_gc">mps_message_type_gc</a></code>, such messages are generated whenever a garbage collection completes. This method returns the size of the set of objects that were condemned in that collection, but survived.</p>


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>


<h3>function <code><a id="mps_message_gc_not_condemned_size" name="mps_message_gc_not_condemned_size">mps_message_gc_not_condemned_size</a></code></h3>


<h4>Summary</h4>

<p><code class="source">mps_message_gc_not_condemned_size</code> returns the "not condemned size" property of the specified message in the specified arena.</p>


<h4>Associated Protocols</h4>

<p>Message, GC.</p>


<h4>Syntax</h4>

<p><code class="source">size_t mps_message_gc_not_condemned_size(mps_arena_t arena, mps_message_t message)</code></p>


<h4>Arguments</h4>

<p>
  <code class="source">arena</code> -- the arena
</p>

<p>
  <code class="source">message</code> -- a message of a message type that supports this method
</p>


<h4>Returned Values</h4>

<p>An approximate size for the set of objects that were in collected pools, but were not condemned in the collection that generated the message.</p>


<h4>Resources</h4>

<p><code class="filename">mps.h</code></p>


<h4>Description</h4>

<p>Currently, the only type of message that supports this property is <code><a href="#mps_message_type_gc">mps_message_type_gc</a></code>; such messages are generated whenever a garbage collection completes. This method returns an approximation to the size of the set of objects that were in collected pools (so potentially subject to garbage collection), but were not condemned in that collection.</p>


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>


<h3>function <code><a id="mps_message_gc_start_why"
  name="mps_message_gc_start_why">mps_message_gc_start_why</a></code></h3>


<h4>Summary</h4>

<p><code class="source">mps_message_gc_start_why</code> returns the a string that describes why a particular collection started. </p>


<h4>Associated Protocols</h4>

<p>Message, GC.</p>


<h4>Syntax</h4>

<p><code class="source">const char * mps_message_gc_start_why(mps_arena_t arena, mps_message_t message)</code></p>


<h4>Arguments</h4>

<p><code class="source">arena</code> -- the arena</p>

<p><code class="source">message</code> -- a message of a message type that supports this method (<code>mps_message_type_gc_start()</code>)</p>


<h4>Returned Values</h4>

<p> A pointer to a string that is a   textual explanation of why this collection is starting. </p>


<h4>Resources</h4>

<p><code class="filename">mps.h</code></p>


<h4>Description</h4>

<p>Currently, the only type of message that supports this property is <code><a href="#mps_message_type_gc_start">mps_message_type_gc_start</a></code>; such messages are generated whenever a garbage collection starts. This method returns a string describing why the collection started. </p>

<p> The contents of the string must not be modified by the client.  The string and the pointer are only valid as long as the message has not been discarded (with <code>mps_message_discard</code>). </p>

<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>


.. c:function:: mps_bool_t mps_message_get(mps_message_t *message_return, mps_arena_t arena, mps_message_type_tmessage_type)


<h4>Summary</h4>

<p>Gets a message of the specified type from a message queue.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Arguments</h4>

<p>
  <code>message_return</code>
  -- the handle to the message that was removed from the queue
</p>

<p>
  <code>arena</code>
  -- the arena
</p>

<p>
  <code>message_type</code>
  -- the type of message
</p>


<h4>Returned Values</h4>

<p>Returns true if a message has been removed from the queue, false if not.</p>


<h4>Description</h4>

<p>
  If there is a message of the specified type on the message queue of the specified arena,then this function removes one such message from the queue, returns a handle to it via the<code>message_return</code> argument, and returns true. Otherwise it returns false.
</p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>


<h3><code><a id="mps_message_poll" name="mps_message_poll">mps_message_poll</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_message_poll">mps_message_poll</a></code> determines whether there are currently any messages on a message queue.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Syntax</h4>

<p><code>mps_bool_t mps_message_poll(mps_arena_t arena)</code></p>


<h4>Arguments</h4>

<p>
  <code>arena</code>
  -- the arena whose message queue you are interested in
</p>


<h4>Returned Values</h4>

<p>A flag to indicate whether there are any messages on the queue.</p>


<h4>Description</h4>

<p><code><a href="#mps_message_poll">mps_message_poll</a></code> is used to determine whether there are currently any messages on the message queue of the specified arena.</p>


<h4>Example</h4>

<p>[missing]</p>


<h4>Error Handling</h4>

<p>Can't fail.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_message_get">mps_message_get</a></code></p>


<h4>Notes</h4>

<p>If you expect a particular type of message, it is usually more practical to just call <code><a href="#mps_message_get">mps_message_get</a></code>.</p>


.. c:function:: mps_bool_t mps_message_queue_type(mps_message_type_t *message_type_return, mps_arena_t arena)


<h4>Summary</h4>

<p><code><a href="#mps_message_queue_type">mps_message_queue_type</a></code> returns the type of the first message on a message queue.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Arguments</h4>

<p>message_type_return -- the type of the first message on the queue of the specified arena</p>

<p>arena -- the arena</p>


<h4>Returned Values</h4>

<p>"True" if there are any messages on the queue of the specified arena, "false" if not.</p>


<h4>Description</h4>

<p>If there are any messages on the queue of the specified arena, then this function returns"true", and also returns the type of the first message via "message_type_return". Otherwise it returns "false".</p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>


.. c:type:: mps_message_t


<h4>Summary</h4>

<p><code><a href="#mps_message_t">mps_message_t</a></code> is used as a handle on an individual message.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Type</h4>

<p><code>typedef struct mps_message_s *mps_message_t</code></p>

<p><code><a href="#mps_message_s">mps_message_s</a></code> is an incomplete structure type used only to declare the opaque type <code><a href="#mps_message_t">mps_message_t</a></code>.</p>


<h4>Description</h4>

<p>The opaque type <code><a href="#mps_message_t">mps_message_t</a></code> is used as a handle on an individual message. Messages are manually managed. They are created at the instigation of the MPS (but see <code><a href="#mps_message_type_enable">mps_message_type_enable</a></code>), and are deleted by the client.</p>

<p>An <code><a href="#mps_message_t">mps_message_t</a></code> is a reference into MPS managed memory, and can safely be stored as such in scannable memory.</p>


<h4>Example</h4>


<h4>Error Handling</h4>

<p>Not applicable.</p>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>


.. c:function:: mps_message_type_t mps_message_type(mps_arena_t arena, mps_message_t message)


<h4>Summary</h4>

<p><code><a href="#mps_message_type">mps_message_type</a></code> returns the type of a message.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Arguments</h4>

<p>arena -- the arena containing the message</p>

<p>message -- a valid message; that is, one previously returned by <code><a href="#mps_message_get">mps_message_get</a></code>, and notdiscarded via <code><a href="#mps_message_discard">mps_message_discard</a></code></p>


<h4>Returned Values</h4>

<p>The type of the specified message.</p>


<h4>Description</h4>

<p><code><a href="#mps_message_type">mps_message_type</a></code> returns the type of a message.</p>


<h4>Example</h4>


<h4>Error Handling</h4>


<h4>See Also</h4>

<p>

<code><a href="#mps_message_clock">mps_message_clock</a></code></p>


<h3>function <code><a id="mps_message_type_disable" name="mps_message_type_disable">mps_message_type_disable</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_message_type_disable">mps_message_type_disable</a></code> restores the arena to the default state whereby messages of thespecified type are not generated.</p>

<p>This reverses the effect of an earlier call to "m ps_message_type_enable".</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Syntax</h4>

<p><code>void mps_message_type_disable(mps_arena_t arena, mps_message_type_t message_type)</code></p>


<h4>Arguments</h4>

<p>arena -- the arena</p>

<p>message_type -- the message type to be disabled</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p>This procedure may be used by the client to specify that messages of the specified type should not created for the specified arena.</p>

<p>Messages are not generated by default, but the client may enable the generation of messages with <code><a href="#mps_message_type_enable">mps_message_type_enable</a></code>.</p>

<p>Any existing messages of the specified type are flushed from the message queue.</p>


<h4>Example</h4>

<p>[none]</p>


<h4>Error Handling</h4>

<p>Never fails.</p>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>


<h4>Notes</h4>

<p>It is permitted to call this function when the message type is already disabled. Such a call will have no effect.</p>


.. c:function:: void mps_message_type_enable(mps_arena_t arena, mps_message_type_t message_type)


<h4>Summary</h4>

<p><code><a href="#mps_message_type_enable">mps_message_type_enable</a></code> allows messages of the specified type to be created for thespecified arena. Without such enabling, the MPS will, by default, not generate any messages of thattype.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Arguments</h4>

<p>arena -- the arena</p>

<p>message_type -- the message type to be enabled</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p>This procedure may be used by the client to specify that messages of the specified type maybe created for the specified arena. Without such enabling, the MPS will by default not generate any messages of that type.</p>

<p>Note that the enabling of messages of a particular type implies that the client application will handle and discard message of that type, or the message queue may consume unbounded resources.</p>

<p>The client may disable message generation again by means of an equivalent call to <code><a href="#mps_message_type_disable">mps_message_type_disable</a></code>.</p>


<h4>Example</h4>

<p>[none]</p>


<h4>Error Handling</h4>

<p>Never fails.</p>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>

<p>"Message Protocol"</p>


<h4>Notes</h4>

<p>It is permitted to call this function when the message type is already enabled. Such a call will have no effect.</p>

<p></p>


.. c:function:: mps_message_type_t mps_message_type_finalization(void)


<h4>Summary</h4>

<p><code><a href="#mps_message_type_finalization">mps_message_type_finalization</a></code> returns the type of finalization messages.</p>


<h4>Associated Protocols</h4>

<p>Message, Finalization.</p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>The type of finalization messages.</p>


<h4>Resources</h4>

<p>Not applicable.</p>


<h4>Description</h4>

<p><code><a href="#mps_message_type_finalization">mps_message_type_finalization</a></code> returns the type of finalization messages. Finalization messages are used by the MPS to implement finalization (see <code><a href="#mps_finalize">mps_finalize</a></code>).  When the MPS detects that an object is finalizable, it finalizes the object by posting a message of this type (note that there might be delays between the object becoming finalizable, the MPS detecting that, and the message being posted). </p>

<p>
In addition to the usual methods applicable to messages, finalization
messages support the <code><a
href="#mps_message_finalization_ref">mps_message_finalization_ref</a></code>
method which returns a reference to the object that was registered for
finalization.</p>


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

<p>

<code>mps_message_*</code>,

<code><a href="#mps_finalize">mps_finalize</a></code></p>


<h3>function <code><a id="mps_message_type_gc" name="mps_message_type_gc">mps_message_type_gc</a></code></h3>


<h4>Summary</h4>

<p><code><a href="#mps_message_type_gc">mps_message_type_gc</a></code> returns the type of garbage collection statistic messages.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Syntax</h4>

<p><code class="source">mps_message_type_t mps_message_type_gc(void)</code></p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>The type of garbage collection statistic messages.</p>


<h4>Resources</h4>

<p>
  <code class="filename">mps.h</code>
</p>


<h4>Description</h4>

<p><code><a href="#mps_message_type_gc">mps_message_type_gc</a></code> returns the type of garbage collection statistic messages. Garbage collection statistic messages are used by the MPS to give the client information about garbage collections that have occurred. Such information may be useful in analysing the client's memory usage over time.</p>

<p>The access methods specific to a message of this type are:</p>

<ul>

  <li><p><code class="source">mps_message_gc_live_size</code> -- gives the total size of the condemned objects that survived the collection that generated the message</p></li>

  <li><p><code class="source">mps_message_gc_condemned_size</code>
  -- gives an approximate size for the set of objects condemned in the collection that generated the message.</p></li>

  <li><p><code class="source">mps_message_gc_not_condemned_size</code> -- gives an approximate size for the set of objects that were in collected pools, but were not condemned in the collection that generated the message.</p></li>

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

<p>Cannot fail.</p>


<h4>See Also</h4>

<p>

<code>mps_message_*</code>.</p>


<h3>function <code><a id="mps_message_type_gc_start"
name="mps_message_type_gc_start">mps_message_type_gc_start</a></code></h3>


<h4>Summary</h4>

<p>
<code><a href="#mps_message_type_gc_start">mps_message_type_gc_start</a></code>
returns the type of garbage collection start messages.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Syntax</h4>

<p><code class="source">mps_message_type_t mps_message_type_gc_start(void)</code></p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>The type of garbage collection start messages.</p>


<h4>Resources</h4>

<p>
  <code class="filename">mps.h</code>
</p>


<h4>Description</h4>

<p>
<code><a href="#mps_message_type_gc_start">mps_message_type_gc_start</a></code>
returns the type of garbage collection start messages.
The messages contain information about why the collection started. See
<code>mps_message_gc_start_why</code>.
</p>

<p>The access methods specific to a message of this type are:</p>

<ul>

  <li><p><code class="source">mps_message_gc_start_why</code> --
  Returns a string that is a description of why the collection started.
  </p></li>

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

<p>Cannot fail.</p>


<h4>See Also</h4>

<p>

<code>mps_message_*</code>.</p>


.. c:type:: mps_message_type_t


<h4>Summary</h4>

<p><code><a href="#mps_message_type_t">mps_message_type_t</a></code> is the type of message types.</p>


<h4>Associated Protocols</h4>

<p>Message.</p>


<h4>Description</h4>

<p><code><a href="#mps_message_type_t">mps_message_type_t</a></code> is the type whose values are the various message types. It is opaque.</p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code>mps_message_*</code></p>


.. c:function:: void mps_pool_check_fenceposts(mps_pool_t pool)


<h4>Summary</h4>

<p>Check all the fenceposts in the pool.</p>


<h4>Associated Protocols</h4>

<p>Debug</p>


<h4>Arguments</h4>

<p>pool the pool whose fenceposts are to be checked</p>


<h4>Description</h4>

<p>This function is a debugging feature to check all the fenceposts in the pool. If a corrupted fencepost is found, an assert will fire. It is only useful to call this on a debug pool that had fenceposting turned, it does nothing on other pools.</p>


<h4>Example</h4>

<p><code>mps_pool_check_fenceposts(gene_pool);</code></p>


<h4>Error Handling</h4>

<p>If a corrupted fencepost is found, an assert will fire. You will probably want to look at the problem with a debugger.</p>


<h4>See Also</h4>

<p>

<code>mps_class_*_debug</code></p>


<h3>structure <code><a id="mps_pool_debug_option_s" name="mps_pool_debug_option_s">mps_pool_debug_option_s</a></code></h3>


<h4>Summary</h4>

<p>This structure is used to pass debug options to <code><a href="#mps_pool_create">mps_pool_create</a></code> for debug classes.</p>


<h4>Associated Protocols</h4>

<p>Debug.</p>


<h4>Type</h4>

<pre>
typedef struct mps_pool_debug_option_s {
  void *fence_template;
  size_t fence_size;
} mps_pool_debug_option_s;
</pre>


<h4>Members</h4>

<p>
  <code>fence_template</code>
  the template for fencepost contents
</p>

<p>
  <code>fence_size</code>
  the size of the template in bytes
</p>


<h4>Description</h4>

<p>Structures of this type are used to pass debug options to <code><a href="#mps_pool_create">mps_pool_create</a></code> when creating instances of debug classes.</p>

<p>Fenceposting is enabled by specifying a non-zero <code>fence_size</code>; the size must be a multiple of the [pool/format] alignment. The content of fenceposts is given as a template that is simply copied onto each fencepost (although sometimes the MPS will create fenceposts smaller than the given size, for example, to pad out some bit that was left unused because of alignmentrequirements).</p>


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

<p>

<code><a
href="#mps_pool_check_fenceposts">mps_pool_check_fenceposts</a></code></p>


<h4>Notes</h4>

<p>Fencepost templates allow the client to specify complicated patterns that mimic illegal datavalues, that would cause an assert to fire if read by mistake, and that would never be written by any operation that writes at the wrong address by mistake.</p>

<p>Another trick is to make the pattern contain an instruction sequence that would cause theprogram to error or stop if executed by mistake.</p>


<h3>function <code><a id="mps_rank_ambig" name="mps_rank_ambig">mps_rank_ambig</a></code></h3>


<h4>Summary</h4>

<p>Function returning the value representing "rank ambig".</p>


<h4>Associated Protocols</h4>

<p>Allocation, Root, Scanning.</p>


<h4>Syntax</h4>

<p><code>mps_rank_ambig()</code></p>


<h4>Type</h4>

<p><code>mps_rank_t mps_rank_ambig(void)</code></p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>Returns a value of type <code><a href="#mps_rank_t">mps_rank_t</a></code> representing "rank ambig".</p>


<h4>Description</h4>

<p>Used to get a value for "rank ambig", which is used to denote that certain references (in a root, for example) are ambiguous references.</p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code><a href="#mps_rank_t">mps_rank_t</a></code>,

<code><a href="#mps_rank_exact">mps_rank_exact</a></code></p>


.. c:function:: mps_rank_t mps_rank_exact(void);


<h4>Summary</h4>

<p>Used to declare references which the client wishes to be exact references.</p>


<h4>Associated Protocols</h4>

<p>Allocation, Root, Scanning.</p>


<h4>Arguments</h4>

<p>No arguments.</p>


<h4>Returned Values</h4>

<p>Returns a rank (see <code><a href="#mps_rank_t">mps_rank_t</a></code>) which can be used to declare references to be exact references.</p>


<h4>Description</h4>

<p>Used to declare references which the client wishes to be exact, non-weak references.</p>


<h4>Example</h4>

<p>[missing]</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_rank_t">mps_rank_t</a></code>,

<code><a href="#mps_rank_ambig">mps_rank_ambig</a></code>,

<code><a href="#mps_rank_weak">mps_rank_weak</a></code></p>


.. c:type:: mps_rank_t


<h4>Summary</h4>

<p>A type whose values are "reference ranks".</p>


<h4>Associated Protocols</h4>

<p>Allocation, Root.</p>


<h4>Type</h4>

<p><code>typedef unsigned int mps_rank_t;</code></p>


<h4>Description</h4>

<p><code><a href="#mps_rank_t">mps_rank_t</a></code> is a concrete type. It is an alias (via the C typedef mechanism) for "unsigned int" provided for convenience and clarity. An object of type <code><a href="#mps_rank_t">mps_rank_t</a></code> can store a value representing one reference rank. Reference ranks are used to conveniently express specific semantics of particular references. See "MPS Scanning Protocol" for descriptions of these semantics, and <code>mps_rank_*</code> for the actual ranks used to declare these semantics.</p>


<h4>Example</h4>

<p>(Probably won't be used explicitly, most likely to be seen in the prototype declaration for other MPS functions. For example, <code><a href="#mps_root_create">mps_root_create</a></code>.)</p>


<h4>See Also</h4>

<p>

<code>mps_rank_*</code></p>


.. c:function:: extern mps_rank_t mps_rank_weak(void);


<h4>Summary</h4>

<p>Function to return a value used to represent "rank weak".</p>


<h4>Associated Protocols</h4>

<p>Allocation, Scanning.</p>


<h4>Arguments</h4>

<p>None.</p>


<h4>Returned Values</h4>

<p>Returns a value of type <code><a href="#mps_rank_t">mps_rank_t</a></code> that represent "rank weak".</p>


<h4>Description</h4>

<p><code><a href="#mps_rank_weak">mps_rank_weak</a></code> returns a value used to represent "rank weak".</p>

<p>"Rank weak" is often used to denote that certain references (in a root or in objects allocated in a pool) are weak references.</p>


<h4>Example</h4>

<p>&lt;example of how to use the symbol&gt;</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_rank_t">mps_rank_t</a></code>,

<code><a href="#mps_rank_exact">mps_rank_exact</a></code></p>


.. c:type:: mps_reg_scan_t


<h4>Summary</h4>

<p>Type of root scanning functions for <code><a href="#mps_root_create_reg">mps_root_create_reg</a></code>.</p>


<h4>Associated Protocols</h4>

<p>Root.</p>


<h4>Syntax</h4>

<p><code>typedef mps_res_t (*mps_reg_scan_t)( mps_ss_t scan_state, mps_thr_t thread, void *p, size_t s)</code></p>


<h4>Arguments</h4>

<p>scan_state a scan state</p>

<p>thread the thread</p>

<p>p a value passed through from root registration</p>

<p>s a value passed through from root registration</p>


<h4>Returned Values</h4>

<p>A result code.</p>


<h4>Description</h4>

<p>This is the type of root scanning functions the client provides to <code><a href="#mps_root_create_reg">mps_root_create_reg</a></code>.These functions will be called, whenever the root needs to be scanned, and passed the "p" and "s"values specified in the call to <code><a href="#mps_root_create_reg">mps_root_create_reg</a></code>.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_root_create_reg">mps_root_create_reg</a></code>,

<code><a
href="#mps_stack_scan_ambig">mps_stack_scan_ambig</a></code></p>


<h4>Notes</h4>

<p>Users are not expected to write any scanning functions of this type. The one function supplied with the MPS, <code><a href="#mps_stack_scan_ambig">mps_stack_scan_ambig</a></code>, should be enough for most purposes.</p>


.. c:type:: mps_res_t


<h4>Summary</h4>

<p><code><a href="#mps_res_t">mps_res_t</a></code> is the type of result codes returned by operations that may fail.</p>


<h4>Type</h4>

<p><code>typedef int mps_res_t;</code></p>


<h4>Description</h4>

<p>A result code indicates the success or failure of an operation, along with the reason for failure. Like UNIX error codes, the meaning of the code depends on the call that returned it. Refer to the documentation of the function for the exact meaning. This documentation describes the broad categories with mnemonic names for various sorts of problems.</p>

<p><code><a id="MPS_RES_OK" name="MPS_RES_OK">MPS_RES_OK</a></code>: The operation succeeded. Out and in/out parameters will only be updated if OK is returned, otherwise they will be left untouched. <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code> is zero.</p>

<p><code><a id="MPS_RES_FAIL" name="MPS_RES_FAIL">MPS_RES_FAIL</a></code>: Something went wrong that does not fall into any of the other categories. The exact meaning depends on the call. See the documentation of the function.</p>

<p><code><a id="MPS_RES_RESOURCE" name="MPS_RES_RESOURCE">MPS_RES_RESOURCE</a></code>: A needed resource could not be obtained. Which resource, depends on the call. Compare with <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code>, which is a special case of this.</p>

<p><code><a id="MPS_RES_MEMORY" name="MPS_RES_MEMORY">MPS_RES_MEMORY</a></code>: Needed memory (committed memory, not address space) could not be obtained. (A <a href="#MPS_RES_MEMORY_detailed">more detailed explanation</a>).</p>

<p><code><a id="MPS_RES_LIMIT" name="MPS_RES_LIMIT">MPS_RES_LIMIT</a></code>: An internal limitation was reached. For example, the maximum number of something was reached. (A <a href="#MPS_RES_LIMIT_detailed">more detailed explanation</a>).</p>

<p><code><a id="MPS_RES_UNIMPL" name="MPS_RES_UNIMPL">MPS_RES_UNIMPL</a></code>: The operation, or some vital part of it, is unimplemented. This might be returned by functions that are no longer supported, or by operations that are included for future expansion, but not yet supported.</p>

<p><code><a id="MPS_RES_IO" name="MPS_RES_IO">MPS_RES_IO</a></code>: An I/O error occurred. Exactly what depends on the function.</p>

<p><code><a id="MPS_RES_COMMIT_LIMIT" name="MPS_RES_COMMIT_LIMIT">MPS_RES_COMMIT_LIMIT</a></code>: The arena's commit limit would have been exceeded as a result of (explicit or implicit) allocation. See protocol.arena.commit.</p>

<p><code><a id="MPS_RES_PARAM" name="MPS_RES_PARAM">MPS_RES_PARAM</a></code>: A parameter of the operation was invalid. (A <a href="#MPS_RES_PARAM_detailed">more detailed explanation</a>).</p>


<p>Any function that might fail will return a result code. Any other results of the function are passed back in "return" parameters. See MPS Interface Conventions for more information.</p>


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

<p>For more examples, s ee doc.mps.ref-man.if-conv.</p>


<h4>See Also</h4>

<p>

<code>MPS_RES_*</code></p>


.. c:function:: mps_res_t mps_root_create(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_trm, mps_root_scan_t scan, void *p, size_t s)


<h4>Summary</h4>

<p>The function <code><a href="#mps_root_create">mps_root_create</a></code> declares a root that consists of all the references indicated by a scanning function.</p>


<h4>Associated Protocols</h4>

<p>Root.</p>


<h4>Arguments</h4>

<p>root_o a pointer to a variable to store the new root structure</p>

<p>arena the arena</p>

<p>rank the rank of references in the root</p>

<p>rm the root mode</p>

<p>scan the scanning function</p>

<p>p a value to be passed to the scanning function</p>

<p>s a value to be passed to the scanning function</p>


<h4>Returned Values</h4>

<p>If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, a new root structure in "*root_o".</p>


<h4>Description</h4>

<p>The client provides a scanning function, that will be called with a scan state and "p" and"s", whenever the root needs to be scanned. See <code><a href="#mps_root_scan_t">mps_root_scan_t</a></code> for details.</p>

<p>If the rank of the root is not <code><a href="#MPS_RANK_AMBIG">MPS_RANK_AMBIG</a></code>, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.</p>


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

<p><code><a href="#mps_root_create">mps_root_create</a></code> returns <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code> when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space, or expand the arena.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_root_scan_t">mps_root_scan_t</a></code>,

<code><a href="#mps_rm_t">mps_rm_t</a></code>,

<code><a href="#mps_rank_t">mps_rank_t</a></code>,

<code><a href="#mps_root_t">mps_root_t</a></code>,

<code><a href="#mps_root_create_fmt">mps_root_create_fmt</a></code>,

<code><a
href="#mps_root_create_table">mps_root_create_table</a></code>,

<code><a href="#MPS_RM_CONST">MPS_RM_CONST</a></code></p>


<h4>Notes</h4>

<p>"p" and "s" are just arbitrary data that scanning function can use. This is needed because Clacks local functions.</p>


.. c:function:: mps_res_t mps_root_create_fmt(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_fmt_scan_t scan, mps_addr_t base, mps_addr_t limit)


<h4>Summary</h4>

<p>The function <code><a href="#mps_root_create_fmt">mps_root_create_fmt</a></code> declares a root that consists of a block of objects, and provides a scanning function for them.</p>


<h4>Associated Protocols</h4>

<p>Root.</p>


<h4>Arguments</h4>

<p>root_o a pointer to a variable to store the new root structure</p>

<p>arena the arena</p>

<p>rank the rank of references in the root</p>

<p>rm the root mode</p>

<p>scan the scanning function</p>

<p>base the address of the start of the root</p>

<p>limit the address just beyond the end of the root</p>


<h4>Returned Values</h4>

<p>If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the new root in "*root_o".</p>


<h4>Description</h4>

<p>The client provides a scanning function, that will be called with a scan state and an area of memory, whenever the root needs to be scanned. See <code><a href="#mps_fmt_scan_t">mps_fmt_scan_t</a></code> for details.</p>

<p>If the rank of the root is not <code><a href="#MPS_RANK_AMBIG">MPS_RANK_AMBIG</a></code>, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.</p>


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

<p><code><a href="#mps_root_create_fmt">mps_root_create_fmt</a></code> returns <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code> when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space, or expand the arena.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_fmt_scan_t">mps_fmt_scan_t</a></code>,

<code><a href="#mps_rm_t">mps_rm_t</a></code>,

<code><a href="#mps_rank_t">mps_rank_t</a></code>,

<code><a href="#mps_root_t">mps_root_t</a></code>,

<code><a href="#mps_root_create">mps_root_create</a></code>,

<code><a
href="#mps_root_create_table">mps_root_create_table</a></code>,

<code><a href="#MPS_RM_PROT">MPS_RM_PROT</a></code>,

<code><a href="#MPS_RM_CONST">MPS_RM_CONST</a></code></p>


<h4>Notes</h4>

<p>This is like <code><a href="#mps_root_create_table">mps_root_create_table</a></code>, except you get to supply your own scanning function.This is like <code><a href="#mps_root_create">mps_root_create</a></code>, except the scanning function has a slightly different argument list(and the MPS knows where the root is).</p>


.. c:function:: mps_res_t mps_root_create_reg(mps_root_t * root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_thr_t thread, mps_reg_scan_t scan, void *p, size_t s)


<h4>Summary</h4>

<p><code><a href="#mps_root_create_reg">mps_root_create_reg</a></code> registers a thread as a root.</p>


<h4>Associated Protocols</h4>

<p>Root.</p>


<h4>Arguments</h4>

<p>root_o a pointer to a variable to store the new root structure</p>

<p>arena the arena</p>

<p>rank the rank of references in the root</p>

<p>rm the root mode</p>

<p>thread the thread to the registered as a root</p>

<p>scan the scanning function</p>

<p>p a value to be passed to the scanning function</p>

<p>s a value to be passed to the scanning function</p>


<h4>Returned Values</h4>

<p>If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, a new root structure in "*root_o".</p>


<h4>Description</h4>

<p><code><a href="#mps_root_create_reg">mps_root_create_reg</a></code> declares the state of a thread as a root. The client provides a scanning function that will be called and passed "p" and "s", whenever the root needs to be scanned. See <code><a href="#mps_reg_scan_t">mps_reg_scan_t</a></code> for details.</p>

<p>If the rank of the root is not <code><a href="#MPS_RANK_AMBIG">MPS_RANK_AMBIG</a></code>, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.</p>


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

<p><code><a href="#mps_root_create_reg">mps_root_create_reg</a></code> returns <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code> when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space, or expand the arena.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_stack_scan_ambig">mps_stack_scan_ambig</a></code>,

<code><a href="#mps_reg_scan_t">mps_reg_scan_t</a></code></p>


<h4>Notes</h4>

<p>Only one suitable scanning function is supplied with the MPS, namely <code><a href="#mps_stack_scan_ambig">mps_stack_scan_ambig</a></code>.</p>


.. c:function:: mps_res_t mps_root_create_table(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t size)


<h4>Summary</h4>

<p><code><a href="#mps_root_create_table">mps_root_create_table</a></code> create s a root that is a vector of references.</p>


<h4>Associated Protocols</h4>

<p>Root.</p>


<h4>Arguments</h4>

<p>root_o a pointer to a variable for storing the new root structure in</p>

<p>arena the arena</p>

<p>rank the rank of the references in this root</p>

<p>rm the root mode</p>

<p>base a pointer to the vector of references that is being registered</p>

<p>size the number of references in the vector being registered</p>


<h4>Returned Values</h4>

<p>If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the new root in "*root_o".</p>


<h4>Description</h4>

<p>This function declares a root that is a vector of references.</p>

<p>If the rank of the root is not <code><a href="#MPS_RANK_AMBIG">MPS_RANK_AMBIG</a></code>, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.</p>


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

<p><code><a href="#mps_root_create_table">mps_root_create_table</a></code> returns <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code> when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space, or expand the arena.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_root_create_table_masked">mps_root_create_table_masked</a></code>,

<code><a href="#MPS_RM_PROT">MPS_RM_PROT</a></code>,

<code><a href="#MPS_RM_CONST">MPS_RM_CONST</a></code></p>


.. c:function:: mps_res_t mps_root_create_table_masked(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t size, mps_word_t mask);


<h4>Summary</h4>

<p><code><a href="#mps_root_create_table_masked">mps_root_create_table_masked</a></code> creates a root that is a vector of tagged values.</p>


<h4>Associated Protocols</h4>

<p>Root.</p>


<h4>Arguments</h4>

<p>root_o a pointer to a variable for storing the new root structure in</p>

<p>arena the arena</p>

<p>rank the rank of the references in this root</p>

<p>rm the root mode</p>

<p>base a pointer to the vector of references that is being registered</p>

<p>
  size the number of references in the vector being registered
  <br />
  mask any element that has any of the bits in mask set is ignored
</p>


<h4>Returned Values</h4>

<p>If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the new root in "*root_o".</p>


<h4>Description</h4>

<p><code><a href="#mps_root_create_table_masked">mps_root_create_table_masked</a></code> creates a root that is a table of tagged values. The mask parameter indicates which bits of a pointer are tag bits. References are assumed to have a tag of zero, values with other tags are ignored.</p>

<p>If the rank of the root is not <code><a href="#MPS_RANK_AMBIG">MPS_RANK_AMBIG</a></code>, the contents of the root have to be valid whenever a GC happens, i.e., they have to be references to actual objects or "NULL". If you're using asynchronous GC, this could be right after the root is registered, so the root has to be valid when it is registered. It's OK for a root to have entries which point to memory not managed by the MPS --they will simply be ignored.</p>


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

<p><code><a href="#mps_root_create_table_masked">mps_root_create_table_masked</a></code> returns <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code> when it fails to allocate memory for the internal root structure; you need to deallocate or reclaim something to make enough space,or expand the arena.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_root_create_table">mps_root_create_table</a></code>,

<code><a href="#MPS_RM_PROT">MPS_RM_PROT</a></code>,

<code><a href="#MPS_RM_CONST">MPS_RM_CONST</a></code></p>


.. c:type:: mps_root_scan_t


<h4>Summary</h4>

<p>Type of root scanning functions for <code><a href="#mps_root_create">mps_root_create</a></code>.</p>


<h4>Associated Protocols</h4>

<p>Root.</p>


<h4>Syntax</h4>

<p><code>typedef mps_res_t (*mps_root_scan_t)(mps_ss_t scan_state, void * p, size_t s)</code></p>


<h4>Arguments</h4>

<p><code>scan_state</code> a scan state</p>

<p><code>p</code> an argument passed through from <code><a href="#mps_root_create">mps_root_create</a></code></p>

<p><code>s</code> an argument passed through from <code><a href="#mps_root_create">mps_root_create</a></code></p>


<h4>Returned Values</h4>

<p>A result code.</p>


<h4>Description</h4>

<p>This is the type of root scanning functions the client provides to<code><a href="#mps_root_create">mps_root_create</a></code>. The MPS will call these functions whenever the root needs to be scanned, with a scan state (of type <code><a href="#mps_ss_t">mps_ss_t</a></code> ), and the <code>p</code> and<code>s</code> values specified in the call to <code><a href="#mps_root_create">mps_root_create</a></code>. Apart from the argument list, the scanning function works like the format scan methods: it needs to indicate all references using <code><a href="#mps_fix">mps_fix</a></code> or <code>MPS_FIX*</code>.</p>


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

<p>If a fixing operation returns a value other than <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the scanning function must return that value, and may return without scanning further references. Generally, it is better if it returns as soon as possible. If the scanning is completed successfully, the function should return <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_root_create">mps_root_create</a></code>,

<code><a href="#mps_ss_t">mps_ss_t</a></code>,

<code><a href="#mps_fix">mps_fix</a></code>,

<code><a href="#MPS_SCAN_BEGIN">MPS_SCAN_BEGIN</a></code>,

<code><a href="#MPS_SCAN_END">MPS_SCAN_END</a></code>,

<code><a href="#MPS_FIX12">MPS_FIX12</a></code>,

<code><a href="#MPS_FIX1">MPS_FIX1</a></code>,

<code><a href="#MPS_FIX2">MPS_FIX2</a></code>,

<code><a href="#MPS_FIX_CALL">MPS_FIX_CALL</a></code>,

<code><a href="#mps_fmt_scan_t">mps_fmt_scan_t</a></code></p>


.. c:type:: mps_roots_stepper_t


<h4>Summary</h4>

<p>Type of the client-supplied root walker component.</p>


<h4>Associated Protocols</h4>

<p>None.</p>


<h4>Type</h4>

<p>
  <code>typedef void (*mps_roots_stepper_t)( mps_addr_t *, mps_root_t, void *, size_t )</code>
</p>


<h4>Arguments</h4>

<p>The function pointed to by an object of type <code><a href="#mps_roots_stepper_t">mps_roots_stepper_t</a></code> takes the followingargument list:</p>

<p><code>(mps_addr_t *ref, mps_root_t root, void *p, size_t s)</code></p>

<p>ref is the address of a root which references an object in the arena. It's a pointer to a root which points to "something" in the client heap. That "something" will be an object if the root is an exact root. But it might be an interior pointer to an object if the root is an ambiguous root.</p>

<p>root is the MPS root object which contains ref.</p>

<p>p and s are two closure values which are copies of the corresponding values which the client passed into <code><a href="#mps_arena_roots_walk">mps_arena_roots_walk</a></code>.</p>


<h4>Returned Values</h4>

<p>he function pointed to by an object of type <code><a href="#mps_roots_stepper_t">mps_roots_stepper_t</a></code> returns no values.</p>


<h4>Description</h4>

<p>A pointer to a function is passed into the function <code><a href="#mps_arena_roots_walk">mps_arena_roots_walk</a></code>; the pointer has this type. The root walker arranges to apply this function to all objects which are directly referenced from the roots.</p>


<h4>Example</h4>

<p>&lt;example of how to use the symbol&gt;</p>


<h4>Error Handling</h4>


<h4>T</h4>

<p>he function pointed to by an object of type <code><a href="#mps_roots_stepper_t">mps_roots_stepper_t</a></code> has no way of signalling an error to the caller.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_arena_roots_arena_walk">mps_arena_roots_arena_walk</a></code></p>


<h4>Notes</h4>


.. c:type:: mps_sac_class_s


<h4>Summary</h4>

<p>A structure describing a size class to be passed as an argument to <code><a href="#mps_sac_create">mps_sac_create</a></code>.</p>


<h4>Associated Protocols</h4>

<p>Allocation cache</p>


<h4>Type</h4>

<pre>
typedef struct mps_sac_class_s {
  size_t mps_block_size;
  size_t mps_cached_count;
  unsigned mps_frequency;
} mps_sac_class_s;
</pre>


<h4>Description</h4>

<p><code><a href="#mps_sac_class_s">mps_sac_class_s</a></code> is the element type of the array passed to<code><a href="#mps_sac_create">mps_sac_create</a></code> to describe the size classes. Each element of this array describes one class by specifying <code>block_size</code>, the maximum size (in bytes) in this class; <code>cached_count</code>, the number of objects of this class to cache; and <code>frequency</code>, a number that describes the frequency of requests (allocation and deallocation combined ) in this class relative to all the other classes. The classes should be given in the order of ascending size.</p>

<p><code>block_size</code> s have to be aligned to the pool alignment. All sizes must be different, and the smallest size must be large enough to hold a <code>void *</code>.</p>

<p>
  <code>cached_count</code>
  is advice to the MPS on how many blocks to cache, not an absolute limit. The cache policy tries to accommodate fluctuations in the population and minimize the cost of responding to client requests; the purpose of this parameter is to limit how much memory the client is willing to set aside for this purpose. However, a
  <code>cached_count</code>
  of zero prevents any caching of blocks falling into that class.
</p>

<p>The MPS automatically provides an "overlarge" class for arbitrarily large objects above the largest class described. Allocations falling into the overlarge class are not cached.</p>


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

<p>

<code><a href="#mps_sac_create">mps_sac_create</a></code></p>


<h4>Notes</h4>

<p>Any blocks whose size falls between two classes are allocated from the larger class.</p>


.. c:function:: mps_res_t mps_sac_create(mps_sac_t *sac_o, mps_pool_t pool, size_t classes_count, mps_sac_class_s *classes);


<h4>Summary</h4>

<p>This function creates a segregated allocation cache.</p>


<h4>Associated Protocols</h4>

<p>Allocation cache</p>


<h4>Arguments</h4>

<p>sac_o a pointer to a variable to hold the cache created</p>

<p>pool the pool the cache is attached to</p>

<p>classes_count the number of the size classes</p>

<p>classes pointer to the first element of an array describing the size classes</p>


<h4>Returned Values</h4>

<p>If the return value is <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, a new cache in <code>*sac_o</code>.</p>


<h4>Description</h4>

<p>This function creates an allocation cache whose free-list is segregated into the given size classes. The cache can get more memory from the given pool, or return memory to it.</p>

<p>Segregated allocation caches can be associated with any pool that supports <code><a href="#mps_alloc">mps_alloc</a></code> and <code><a href="#mps_free">mps_free</a></code>.</p>

<p>The size classes are described by an array of element type <code><a href="#mps_sac_class_s">mps_sac_class_s</a></code> (q.v.). This array is used to initialize the cache, and is not needed after<code><a href="#mps_sac_create">mps_sac_create</a></code> returns. There might be a limit on how many classes can be described,but it will be no less than <code><a href="#MPS_SAC_CLASS_LIMIT">MPS_SAC_CLASS_LIMIT</a></code>. You must specify at least one class.The MPS automatically provides an "overlarge" class for arbitrarily large objects above the largest class described. Allocations falling into the overlarge class are not cached.</p>


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

<p><code><a href="#mps_sac_create">mps_sac_create</a></code> returns <code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code> or<code><a href="#MPS_RES_COMMIT_LIMIT">MPS_RES_COMMIT_LIMIT</a></code> when it fails to allocate memory for the internal cache structure;see the documentation for those return codes for recovery options. It returns<code><a href="#MPS_RES_LIMIT">MPS_RES_LIMIT</a></code> if you ask for too many size classes; combine some small adjacent classes. It returns <code><a href="#MPS_RES_PARAM">MPS_RES_PARAM</a></code> if the pool doesn't support segregated allocation caches.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_sac_class_s">mps_sac_class_s</a></code>,

<code><a href="#MPS_SAC_CLASS_LIMIT">MPS_SAC_CLASS_LIMIT</a></code>,

<code><a href="#mps_sac_destroy">mps_sac_destroy</a></code>,

<code><a href="#MPS_RES_MEMORY">MPS_RES_MEMORY</a></code>,

<code><a href="#MPS_RES_COMMIT_LIMIT">MPS_RES_COMMIT_LIMIT</a></code>,

<code><a href="#MPS_RES_LIMIT">MPS_RES_LIMIT</a></code>,

<code><a href="#MPS_RES_PARAM">MPS_RES_PARAM</a></code>,

<code><a href="#mps_sac_t">mps_sac_t</a></code></p>


<h4>Notes</h4>

<p>Too many classes will slow down allocation; too few classes waste more space in internal fragmentation. It is assumed that overlarge allocations are rare; otherwise, you would add another class for them, or even create separate allocation caches or pools for them.</p>

<p>Some pools will work more efficiently with caches than others. In the future, the MPS might offer pools specially optimized for particular types of cache.</p>

<p>Segregated allocation caches work poorly with debug pool classes at the moment: the checking only happens when blocks are moved between the cache and the pool. This will be fixed, but the speed of allocation with a debug class will always be similar to <code><a href="#mps_alloc">mps_alloc</a></code>, rather than cached speed.</p>


<h4>Type</h4>

<p>size_t</p>


<h4>Associated Protocols</h4>

<p>Allocation cache</p>


<h4>Description</h4>

<p><code><a href="#MPS_SAC_CLASS_LIMIT">MPS_SAC_CLASS_LIMIT</a></code> specifies a lower limit on the maximum number of classes that can be described in a call to <code><a href="#mps_sac_create">mps_sac_create</a></code>, i.e., the MPS guarantees to accept at least this many classes. More might be accepted -- in fact, there might not be any limit in the implementation on the maximum number of classes, but if you specify more than this, you should be prepared to handle the error.</p>

<p><code><a href="#MPS_SAC_CLASS_LIMIT">MPS_SAC_CLASS_LIMIT</a></code> is a macro suitable for use in a constant expression, both in a #if directive and wherever else constant expressions may be used.</p>



<h4>See Also</h4>

<p>

<code><a href="#mps_sac_create">mps_sac_create</a></code></p>


<h4>Notes</h4>

<p>If you ask for too many size classes, <code><a href="#mps_sac_create">mps_sac_create</a></code> returns <code><a href="#MPS_RES_LIMIT">MPS_RES_LIMIT</a></code>; you can recover by combining some small adjacent classes.</p>


.. c:function:: void mps_sac_destroy(mps_sac_t);


<h4>Summary</h4>

<p>This function destroys a segregated allocation cache.</p>


<h4>Associated Protocols</h4>

<p>Allocation cache</p>


<h4>Arguments</h4>

<p>sac the segregated allocation cache</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p>This function destroys a segregated allocation cache. All memory held in it is returned to the associated pool.</p>


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

<p>

<code><a href="#mps_sac_create">mps_sac_create</a></code>,

<code><a href="#mps_sac_t">mps_sac_t</a></code></p>


<h4>Notes</h4>

<p>Destroying the cache might well cause the pool to return some memory to the arena, but that's up to the pool's usual policy.</p>

<p>Destroying the cache has no effect on objects allocated through it.</p>


.. c:function:: void mps_sac_flush(mps_sac_t sac);


<h4>Summary</h4>

<p>This function flushes the segregated allocation cache given.</p>


<h4>Associated Protocols</h4>

<p>Allocation cache</p>


<h4>Arguments</h4>

<p>sac the segregated allocation cache</p>


<h4>Returned Values</h4>

<p>None.</p>


<h4>Description</h4>

<p>This function flushes the segregated allocation cache given, returning all memory held in it to the associated pool.</p>

<p>The client is responsible for synchronizing the access to the cache, but the MPS will properly synchronize with any other threads that might be accessing the same pool.</p>


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

<p>

<code><a href="#mps_sac_t">mps_sac_t</a></code></p>


<h4>Notes</h4>

<p>This is something that you'd typically do when you know you won't be using the cache for awhile, but want to hold on to the cache itself. Destroying a cache has the effect of flushing it,naturally.</p>

<p>Flushing the cache might well cause the pool to return some memory to the arena, but that's up to the pool's usual policy.</p>

<p>Note that the MPS might also decide to take memory from the cache without the client requesting a flush.</p>


.. c:type:: mps_sac_t


<h4>Summary</h4>

<p>Type of segregated allocation caches.</p>


<h4>Associated Protocols</h4>

<p>Allocation cache</p>


<h4>Type</h4>

<p><code>typedef struct mps_sac_s *mps_sac_t;</code></p>


<h4>Description</h4>

<p>A value of this type represents an allocation cache with segregated free lists. It is an opaque type.</p>


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

<p>

<code><a href="#mps_sac_create">mps_sac_create</a></code>,

<code><a href="#mps_sac_destroy">mps_sac_destroy</a></code>,

<code><a href="#MPS_SAC_ALLOC">MPS_SAC_ALLOC</a></code>,

<code><a href="#mps_sac_alloc">mps_sac_alloc</a></code>,

<code><a href="#MPS_SAC_FREE">MPS_SAC_FREE</a></code>,

<code><a href="#mps_sac_free">mps_sac_free</a></code>,

<code><a href="#mps_sac_flush">mps_sac_flush</a></code></p>


<h4>Notes</h4>

<p>None.</p>


.. c:function:: mps_res_t mps_stack_scan_ambig(mps_ss_t scan_state, mps_thr_t thread, void *stack_bottom, size_t ignore)


<h4>Summary</h4>

<p>A scanning function for ambiguous scanning of thread states.</p>


<h4>Associated Protocols</h4>

<p>Root.</p>


<h4>Arguments</h4>

<p>scan_state a scan state</p>

<p>thread the thread</p>

<p>stack_bottom a pointer to the bottom of the stack</p>

<p>ignore ignored</p>


<h4>Returned Values</h4>

<p>A result code.</p>


<h4>Description</h4>

<p>This is a root scanning function of type <code><a href="#mps_reg_scan_t">mps_reg_scan_t</a></code>. It will scan all integer registers and everything on the stack of the thread given, and can therefore only be used with roots of rank <code><a href="#MPS_RANK_AMBIG">MPS_RANK_AMBIG</a></code>. It will only scan things at the given stack bottom pointer or higher on the stack (that is, more recently added). References are assumed to be represented as machine words, and are required to be 4-byte-aligned; unaligned values are ignored.</p>

<p>Clients don't call this function, it is used as an argument of <code><a href="#mps_root_create_reg">mps_root_create_reg</a></code>.</p>


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

<p>

<code><a href="#mps_reg_scan_t">mps_reg_scan_t</a></code>,

<code><a
href="#mps_root_create_reg">mps_root_create_reg</a></code></p>


<h4>Notes</h4>

<p>The MPS provides this function because it's hard to write (it's OS- and architecture-dependent and possibly compiler-dependent).</p>


.. c:function:: mps_word_t mps_telemetry_control(mps_word_t reset_mask, mps_word_t flip_mask);


<h4>Summary</h4>

<p>This function is used to read and change the filters on the telemetry stream.</p>


<h4>Associated Protocols</h4>

<p>Telemetry.</p>


<h4>Arguments</h4>

<p>reset_mask is a bit mask indicating the bits that should be reset, regardless of previous value.</p>

<p>flip_mask is a bit mask indicating the bits whose value should be flipped after the resetting.</p>


<h4>Returned Values</h4>

<p>The function returns the previous value of the telemetry filter control.</p>


<h4>Description</h4>

<p>This function is used to read and change the filters on the telemetry stream. It is generally for use by developers.</p>

<p>The parameters reset_mask and flip_mask allow specifying any binary operation on the filter control. To use this function for typical operations, the parameters should be set as follows:</p>

<p>Operation reset_mask flip_mask</p>

<p>set(M) M M</p>

<p>reset(M) M 0</p>

<p>flip(M) 0 M</p>

<p>read() 0 0</p>

<p>The significance of the bits is liable to change, but the current values (number the least significant bit as zero) are:</p>

<p>0 -- per space or arena</p>

<p>1 -- per pool</p>

<p>2 -- per trace or scan</p>

<p>3 -- per page (segment)</p>

<p>4 -- per reference or fix</p>

<p>5 -- per allocation or object</p>

<p>6 -- user events (e.g., <code><a href="#mps_telemetry_intern">mps_telemetry_intern</a></code>)</p>


<h4>Example</h4>


<h4>See Also</h4>

<p>

<code><a
href="#mps_lib_telemetry_control">mps_lib_telemetry_control</a></code></p>


.. c:function:: void mps_telemetry_flush(void);


<h4>Summary</h4>

<p>This function is used to flush the internal event buffers.</p>


<h4>Associated Protocols</h4>

<p>Telemetry.</p>


<h4>Description</h4>

<p>This function is used to flush the internal event buffers into the event stream. This function also calls <code><a href="#mps_lib_io_flush">mps_lib_io_flush</a></code> on the event stream itself. This ensures that even the latest events are now properly recorded, should the application terminate (uncontrollably as a result of a bug, for example) or some interactive tool require access to the event data. You could even try calling this from a debugger after a problem.</p>


<h4>Example</h4>

<p><code>mps_telemetry_flush();</code></p>


<h4>See Also</h4>

<p>

<code><a href="#mps_lib_io_flush">mps_lib_io_flush</a></code></p>


.. c:function:: mps_word_t mps_telemetry_intern(char *)


<h4>Summary</h4>

<p>This function registers a string with the MPS, and receives a unique identifier in return.This identifier is suitable for use with <code><a href="#mps_telemetry_label">mps_telemetry_label</a></code>.</p>


<h4>Associated Protocols</h4>

<p>Telemetry</p>


<h4>Arguments</h4>

<p>The function receives a name as a nul-terminated string in the usual C way. The string's length should not exceed 256 characters, including nul terminating character. In appropriate varieties this restriction is checked and will cause the MPS to issue an ASSERT. So don't do it.</p>


<h4>Returned Values</h4>

<p>The function returns a unique identifier that may be used to represent the string in future.</p>


<h4>Description</h4>

<p>The intention of this function is to provide an immediate identifier that can be used to concisely represent a string for the purposes of <code><a href="#mps_telemetry_label">mps_telemetry_label</a></code>. Note that the appropriate settings must be made to the telemetry filter (via <code><a href="#mps_telemetry_control">mps_telemetry_control</a></code>) before this function is invoked; the associate event is of the user kind.</p>


<h4>Error Handling</h4>

<p>The string's length should not exceed 256 characters, including nul terminating character.This will cause the MPS to issue an ASSERT in appropriate varieties.</p>


<h4>See Also</h4>

<p>

<code><a
href="#mps_telemetry_label">mps_telemetry_label</a></code></p>

<p>

<code><a
href="#mps_telemetry_control">mps_telemetry_control</a></code></p>


.. c:function:: void mps_telemetry_label(mps_addr_t, mps_word_t);


<h4>Summary</h4>

<p>This function associates an identifier returned from <code><a href="#mps_telemetry_intern">mps_telemetry_intern</a></code>, and hence a string, with an address, in the telemetry stream.</p>


<h4>Associated Protocols</h4>

<p>telemetry</p>


<h4>Arguments</h4>

<p>The function receives an address and an identifier. The identifier should be one returned by <code><a href="#mps_telemetry_intern">mps_telemetry_intern</a></code> in the same session.</p>


<h4>Description</h4>

<p>This function is intended to associate the address with an identifier in the telemetry stream. Note that the user kind must be set in the telemetry filter.</p>


<h4>Example</h4>

<p>Typical uses include:</p>

<p>- Label pools with a human-meaningful name;</p>

<p>- Label allocated objects with their type or class.</p>


<h4>See Also</h4>

<p>

<code><a href="#mps_telemetry_intern">mps_telemetry_intern</a></code>,

<code><a
href="#mps_telemetry_control">mps_telemetry_control</a></code>,

<code><a href="#mps_thr_t">mps_thr_t</a></code></p>


.. c:type:: mps_thr_t



<h4>Summary</h4>

<p><code><a href="#mps_thr_t">mps_thr_t</a></code> is the type of thread records registered with the MPS.</p>


<h4>Associated Protocols</h4>

<p>Threads.</p>


<h4>Type</h4>

<p><code>typedef mps_thr_s *mps_thr_t;</code></p>


<h4>Description</h4>

<p>An object of the opaque type <code><a href="#mps_thr_t">mps_thr_t</a></code> is a thread registration. In a multi-threaded environment where incremental garbage collection is used, threads must be registered with the MPS so that the MPS can examine their state.</p>

<p>An object of type <code><a href="#mps_thr_t">mps_thr_t</a></code> is obtained using the thread registration function <code><a href="#mps_thread_reg">mps_thread_reg</a></code>.</p>


<h4>Example</h4>

<pre>
  mps_thr_t this_thread;
  mps_res_t res;

  res = mps_thread_reg(&amp;this_thread, space);
  if(res != MPS_RES_OK) return res;
</pre>


<h4>See Also</h4>

<p>

<code><a href="#mps_reg_t">mps_reg_t</a></code>,

<code><a href="#mps_thread_reg">mps_thread_reg</a></code>,

<code><a href="#mps_thread_dereg">mps_thread_dereg</a></code>,

<code><a href="#mps_reg_scan_t">mps_reg_scan_t</a></code>,

<code><a href="#mps_root_create_reg">mps_root_create_reg</a></code>,

<code><a
href="#mps_stack_scan_ambig">mps_stack_scan_ambig</a></code></p>


<h2> <a id="section-4" name="section-4">4. Undocumented Symbols</a> </h2>

<p>The following MPS symbols are used or defined in MPS header files, and intended for client use, but are not yet documented in this reference manual.</p>

<p>[This section is very out-of-date. RB 2012-08-15]</p>

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

<p><code>$Id: //info.ravenbrook.com/project/mps/branch/2012-10-09/user-guide/manual/reference/index.html#1 $</code></p>

<p>
<a href="/">Ravenbrook</a> /
<a href="/project/">Projects</a> /
<a href="/project/mps/">Memory Pool System</a> /
<a href="/project/mps/master/">Master Product Sources</a> /
<a href="/project/mps/master/manual/">Product Manuals</a>
</p>

</div>

</body>

</html>


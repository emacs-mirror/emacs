.. _topic-root:

Roots
=====

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/root/index.html

:c:macro:`MPS_RM_CONST` is a preprocessor macro defining a constant that can be or'ed with other ``MPS_RM`` constants, and passed as the :term:`root mode` argument to certain root creation functions (:c:func:`mps_root_create`, :c:func:`mps_root_create_fmt`, :c:func:`mps_root_create_table`, :c:func:`mps_root_create_table_masked`, and :c:func:`mps_root_create_reg`).

from :c:macro:`MPS_RM_PROT`:

No page may contain parts of two or more roots with :c:macro:`MPS_RM_PROT` [how does one prevent
that?]. You mustn't specify :c:macro:`MPS_RM_PROT` if the client program or
anything other than (this instance of) the MPS is going to protect or
unprotect the relevant pages.


Internal Notes

Future meaning: The MPS may place a hardware read and/or write barrier on any pages which any part of the root covers. Format methods and scanning functions (except for the one for this root) may not read or write data in this root. You may specify :c:macro:`MPS_RM_PROT` on a root allocated from the MPS, as long as it's not from a GCd pool. - drj 1997-12-18</p>

This feature is far too technical for most of our clients: we should think about producing some guidelines on how to use it. - pekka 1998-01-27

There may be problems if the client wants the OS to access the root. Lots of OSes can't cope with writing to protected pages. So we'll need to document that caveat too. drj 1998-05-20

::

    static mps_root_t mmRoot;

    int main(void)
    {
        mps_res_t res;

        /* ... */

        res = mps_root_create(&mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                              &rootScanner, NULL, 0);
        /* see doc of mps_root_scan_t for definition of rootScanner */
        if (res != MPS_RES_OK)
            exit(1);

        /* ... */
    }




.. note::

    The contents of an :term:`ambiguous root` must be valid whenever a
    :term:`garbage collection` happens. That is, all the
    :term:`references <reference>` fixed by the root scanning function
    have to be references to actual objects or null pointers. If
    you're using :term:`asynchronous garbage collection <asynchronous
    garbage collector>`, this could be as soon as the root is
    registered, so the root has to be valid when it is registered. As
    with an ordinary :term:`scan method`, a root scanning function is
    allowed to fix references which point to memory not managed by the
    MPS. These references will be ignored.

::

    static mps_root_t mmRoot;
    SegmentDescriptor DataSegment;

    int main(void)
    {
        mps_res_t res;

        /* ... */

        mps_addr_t base = DataSegment.base;
        mps_addr_t limit = DataSegment.base + SegmentLength;
        res = mps_root_create_fmt(&mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                                  &scan_objs, base, limit);

        /* see doc of mps_fmt_scan_t for definition of scan_objs */

        if (res != MPS_RES_OK)
            exit( EXIT_FAILURE );

        /* ... */
    }

::

    typedef struct {
        mps_root_t mmRoot;
        mps_thr_t thread;
        /* ...  */
    } ThreadLocals;

    void InitThread(ThreadLocals *thr)
    {
        /* This is a hack to find the bottom of the stack. */
        void *stackBottom = &stackBottom;

        mps_thread_reg(&thr->thread, arena);
        mps_root_create_reg(&thr->mmRoot, arena, MPS_RANK_AMBIG, (mps_rm_t) 0,
                            thr->thread, mps_stack_scan_ambig, stackBottom, 0);

        /* ...  */
    }

::

    static mps_root_t mmRoot;
    Object *Objects[rootCOUNT];

    int main(void)
    {
      mps_res_t res;

      /* ... */

      res = mps_root_create_table(&mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                                  (mps_addr_t)&Objects, rootCOUNT);

      if (res != MPS_RES_OK)
          exit(1);

      /* ... */
    }

::

    #define tagMASK 0x0003

    static mps_root_t mmRoot;
    Object *Objects[rootCOUNT];

    int main(void)
    {
        mps_res_t res;

        /* ... */

        res = mps_root_create_table_masked(&mmRoot, arena, MPS_RANK_EXACT,
                                           (mps_rm_t)0,
                                           (mps_addr_t)&Objects, rootCOUNT,
                                           (mps_word_t)tagMASK);
        if (res != MPS_RES_OK)
            exit(1);

        /* ... */
    }

::

    static StackFrame *stackBottom;

    /* root scanner for an imaginary interpreter for a stack-oriented language */
    static mps_res_t rootScanner(mps_ss_t ss, void * p, size_t s)
    {
        StackFrame *frame;
        size_t i;
        mps_res_t res;

        UNUSED(p);
        UNUSED(s);

        for(frame = stackBottom; frame != NULL; frame = frame->next) {
            for(i = frame->size; i > 0; --i) {
                res = mps_fix(ss, &frame->locals[i]);
                if (res != MPS_RES_OK) return res;
            }
        }

        return res;
    }


Interface
---------

.. c:function:: void mps_arena_roots_walk(mps_arena_t arena, mps_roots_stepper_t f, void *p, size_t s)

    Visit references in registered :term:`roots <root>` in an
    :term:`arena`.

    ``arena`` is the arena whose roots you want to visit.

    ``f`` is a function that will be called for each reference to an
    object in an :term:`automatically <automatic memory management>`
    managed :term:`pool class` that was found in a registered root
    beloging to the arena. It takes four arguments: ``ref`` is the
    address of a reference to an object in the arena, ``root`` is the
    root in which ``ref`` was found, and ``p`` and ``s`` are the
    corresponding arguments that were passed to
    :c:func:`mps_arena_roots_walk`.

    ``p`` and ``s`` are arguments that will be passed to ``f`` each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    This function may only be called when the arena is in the
    :term:`parked state`.

    .. seealso::

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


.. c:function:: mps_rank_t mps_rank_ambig(void)

    Return the :term:`rank` of :term:`ambiguous roots <ambiguous
    root>`.


.. c:function:: mps_rank_t mps_rank_exact(void)

    Return the :term:`rank` of :term:`exact roots <exact root>`.


.. c:type:: mps_rank_t

    The type of :term:`ranks <rank>`. It is an alias (via the
    :term:`C` ``typedef`` mechanism) for ``unsigned int``, provided
    for convenience and clarity.


.. c:function:: mps_rank_t mps_rank_weak(void)

    Return the :term:`rank` of :term:`weak roots <weak root>`.


.. c:type:: mps_res_t (*mps_reg_scan_t)(mps_ss_t ss, mps_thr_t thr, void *p, size_t s)

    The type of a root scanning function for roots created with
    :c:func:`mps_root_create_reg`.

    ``ss`` is the :term:`scan state`. It must be passed to
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END` to delimit a
    sequence of fix operations, and to the functions
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` when fixing a
    :term:`reference`.

    ``thr`` is the :term:`thread`.

    ``p`` and ``s`` are the corresponding values that were passed to
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

    .. seealso::

        :ref:`topic-scanning`.

    .. note::

        :term:`Client programs <client program>` are not expected to
        write scanning functions of this type. The built-in MPS
        function :c:func:`mps_stack_scan_ambig` should be used.


.. c:macro:: MPS_RM_CONST

    The :term:`root mode` for :term:`constant roots <constant root>`.
    This tells the MPS that the :term:`client program` will not change
    the :term:`root` after it is registered: that is, scanning the
    root will produce the same set of :term:`references <reference>`
    every time. Furthermore, for roots registered by
    :c:func:`mps_root_create_fmt` and :c:func:`mps_root_create_table`,
    the client program will not write to the root at all.

    .. note::

        Currently ignored by the MPS.


.. c:macro:: MPS_RM_PROT

    The :term:`root mode` for :term:`protectable roots <protectable
    root>`. This tells the MPS that it may place a :term:`write
    barrier` on any :term:`page` which any part of the :term:`root`
    covers. No :term:`format method` or :term:`scan method` (except
    for the one for this root) may write data in this root. They may
    read it.

    .. note::

        You must not specify ``MPS_RM_PROT`` on a root allocated by
        the MPS.

        No page may contain parts of two or more protectable roots.
        You mustn't specify ``MPS_RM_PROT`` if the :term:`client
        program` or anything other than (this instance of) the MPS is
        going to protect or unprotect the relevant pages.


.. c:type:: mps_rm_t

    The type of :term:`root modes <root mode>`.

    A root mode describes whether a :term:`root` is :term:`constant
    <constant root>`, :term:`protectable <protectable root>`, or both,
    and lets the MPS know whether it may place a :term:`barrier (1)`
    on the root.

    It should be the sum of some subset of :c:macro:`MPS_RM_CONST` and
    :c:macro:`MPS_RM_PROT`, or zero (meaning neither constant or
    protectable).

    .. note::

        As of version 1.110, the MPS does not place barriers on roots,
        and so does not make use of the root mode. The feature may be
        added in a future release.


.. c:function:: mps_res_t mps_root_create(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_root_scan_t root_scan, void *p, size_t s)

    Register a :term:`root` that consists of the :term:`references
    <reference>` fixed by a scanning function.

    ``root_o`` points to a location that will hold the address of the
    new root description.

    ``arena`` is the arena.

    ``rank`` is the :term:`rank` of references in the root.

    ``rm`` is the :term:`root mode`.

    ``root_scan`` is the root scanning function. See
    :c:type:`mps_root_scan_t`.

    ``p`` and ``s`` are arguments that will be passed to ``root_scan`` each
    time it is called. This is intended to make it easy to pass, for
    example, an array and its size as parameters.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    The registered root destription persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.


.. c:function:: mps_res_t mps_root_create_fmt(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_fmt_scan_t fmt_scan, mps_addr_t base, mps_addr_t limit)

    Register a :term:`root` that consists of the :term:`references
    <reference>` fixed by a scanning function in a block of
    :term:`formatted objects <formatted object>`.

    ``root_o`` points to a location that will hold the address of the
    new root description.

    ``arena`` is the arena.

    ``rank`` is the :term:`rank` of references in the root.

    ``rm`` is the :term:`root mode`.

    ``fmt_scan`` is a scanning function. See :c:type:`mps_fmt_scan_t`.

    ``base`` is the address of the base of the block of formatted
    objects.

    ``limit`` is the address just beyond the end of the block of
    formatted objects.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    The registered root destription persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.

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

    ``root_o`` points to a location that will hold the address of the
    new root description.

    ``arena`` is the arena.

    ``rank`` is the :term:`rank` of references in the root.

    ``rm`` is the :term:`root mode`.

    ``thr`` is the thread.

    ``reg_scan`` is a scanning function. See :c:type:`mps_reg_scan_t`.

    ``p`` and ``s`` are arguments that will be passed to ``reg_scan`` each
    time it is called. This is intended to make it easy to pass, for
    example, an array and its size as parameters.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    The registered root destription persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.

    .. note::

        It is not supported for :term:`Client programs <client
        program>` to pass their own scanning functions to this
        function. The built-in MPS function
        :c:func:`mps_stack_scan_ambig` must be used.


.. c:function:: mps_res_t mps_root_create_table(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t count)

    Register a :term:`root` that consists of a vector of
    :term:`references <reference>`.

    ``root_o`` points to a location that will hold the address of the
    new root description.

    ``arena`` is the arena.

    ``rank`` is the :term:`rank` of references in the root.

    ``rm`` is the :term:`root mode`.

    ``base`` points to a vector of references.

    ``count`` is the number of references in the vector.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    The registered root destription persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.


.. c:function:: mps_res_t mps_root_create_table_masked(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t count, mps_word_t mask)

    Register a :term:`root` that consists of a vector of :term:`tagged
    references <tagged reference>`.

    ``root_o`` points to a location that will hold the address of the
    new root description.

    ``arena`` is the arena.

    ``rank`` is the :term:`rank` of references in the root.

    ``rm`` is the :term:`root mode`.

    ``base`` points to a vector of tagged references.

    ``count`` is the number of tagged references in the vector.

    ``mask`` is a :term:`bitmask` whose set bits specify the location of
    the :term:`tag`. References are assumed to have a tag of zero: any
    value in the vector with a non-zero tag is ignored.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    The registered root destription persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.


.. c:function:: void mps_root_destroy(mps_root_t root)

    Deregister a :term:`root` and destroy its description.

    ``root`` is the root.


.. c:type:: typedef mps_res_t (*mps_root_scan_t)(mps_ss_t ss, void *p, size_t s)

    The type of root scanning functions for :c:func:`mps_root_create`.

    ``ss`` is the :term:`scan state`. It must be passed to
    :c:func:`MPS_SCAN_BEGIN` and :c:func:`MPS_SCAN_END` to delimit a
    sequence of fix operations, and to the functions
    :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` when fixing a
    :term:`reference`.

    ``p`` and ``s`` are the corresponding values that were passed to
    :c:func:`mps_root_create`.

    Returns a :term:`result code`. If a fix function returns a value
    other than :c:macro:`MPS_RES_OK`, the scan method must return that
    value, and may return without fixing any further references.
    Generally, itis better if it returns as soon as possible. If the
    scanning is completed successfully, the function should return
    :c:macro:`MPS_RES_OK`.


.. c:type:: void (*mps_roots_stepper_t)(mps_addr_t *ref, mps_root_t root, void *p, size_t s)

    The type of a :term:`root` :term:`stepper function`.

    A function of this type can be passed to
    :c:func:`mps_arena_roots_walk`, in which case it will be called
    for each reference into the :term:`arena` from a root registered
    with the arena. It receives four arguments:

    ``ref`` points to a reference in a root. The reference points to
    something in the arena. If the root is :term:`exact <exact
    reference>` then the reference points to the start of an allocated
    block, but if the root is :term:`ambiguous <ambiguous reference>`
    it might point to somewhere in the middle of an allocated block.

    ``root`` is the description of the root which contains ``ref``.

    ``p`` and ``s`` are the corresponding values that were passed to
    :c:func:`mps_arena_roots_walk`.


.. c:type:: mps_root_t

    The type of :term:`root` descriptions.

    The :term:`arena` uses root descriptions to find :term:`references
    <reference>` within the :term:`client program's <client program>`
    roots.



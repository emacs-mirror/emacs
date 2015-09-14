.. sources:

    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/root/>`_

.. index::
   single: root; introduction

.. _topic-root:

Roots
=====

:term:`Roots` tell the :term:`garbage collector` where to start
:term:`tracing <trace>`. The garbage collector determines which blocks
are :term:`reachable` from the roots, and (in :term:`automatically
managed <automatic memory management>` :term:`pools`) reclaims
the :term:`unreachable` blocks. This is quite efficient and can be a
very good approximation to :term:`liveness <live>`.

It is therefore important that all :term:`references` that the
:term:`client program` can directly access are registered as roots,
otherwise the garbage collector might recycle an object that would be
used in the future. Some collectors, for example Boehm's, assume that
all references stored in static data are roots; the Memory Pool System
is more flexible, but requires the client program to declare which
references are roots.


.. index::
   single: root; registering

Registering roots
-----------------

You can register a root at any time by calling one of the
``mps_root_create`` functions. Roots may not be registered twice, and
no two roots may overlap (that is, each reference is :term:`fixed` by
at most one root). Roots may be:

#. in :term:`registers`;

#. on the program's :term:`control stack`;

#. in the program's static data;

#. in :term:`heap` not managed by the MPS (provided that you destroy
   the root before freeing it; see :ref:`the Scheme interpreter's
   global symbol table <guide-lang-roots-rehash>` for an example);

#. in :term:`manually managed <manual memory management>` pools
   (provided that you remove the root before freeing it).

Roots must not be in memory that is subject to :term:`garbage
collection` (and so roots must not be in :term:`automatically managed
<automatic memory management>` pools).

When you register a root you describe to the MPS how to :term:`scan`
it for references, providing your own scanning function in the cases
of :c:func:`mps_root_create` and :c:func:`mps_root_create_fmt`. Such a
root scanning function must follow the :ref:`topic-scanning-protocol`.

All the references in a root are of the same :term:`rank` (just as in
a :term:`formatted object`). So they are all :term:`exact <exact
reference>`, :term:`ambiguous <ambiguous reference>` or :term:`weak
<weak reference (1)>`.

.. note::

    If the rank of the root is :term:`exact <exact reference>`, or
    :term:`weak <weak reference (1)>`, the references in the root must
    always be valid while the root is registered: that is, they must
    be references to actual objects or null pointers. This could be
    immediately after the root is registered, so the root must be
    valid before it is registered.

.. note::

    As with :ref:`scanning <topic-scanning>` in general, it's safe to
    :term:`fix` references that point to memory not managed by the
    MPS. These will be ignored.

Roots can be deregistered at any time by calling
:c:func:`mps_root_destroy`. All roots registered in an :term:`arena`
must be deregistered before the arena is destroyed.

There are six ways to register a root, depending on how you need to
scan it for references:

#. :c:func:`mps_root_create` if you need a custom root scanning
   function (of type :c:type:`mps_root_scan_t`);

#. :c:func:`mps_root_create_fmt` if the root consists of a block of
   objects belonging to an :term:`object format`, which can be scanned
   by the format's :term:`scan method` (of type
   :c:type:`mps_fmt_scan_t`);

#. :c:func:`mps_root_create_table` if the root consists of a table of
   references;

#. :c:func:`mps_root_create_table_masked` if the root consists of a
   table of :term:`tagged references`;

#. :c:func:`mps_root_create_stack` if the root consists of the
   :term:`registers` and :term:`control stack` of a thread. See
   :ref:`topic-root-thread` below.

.. index::
   pair: root; cautions

Cautions
--------

Creating a root and then registering is similar to reserving a block
and then committing it (in the
:ref:`topic-allocation-point-protocol`), and similar :ref:`cautions
<topic-allocation-cautions>` apply. Before registering a root:

#. The root must be valid (that is, the appropriate root scanning
   function can scan it).

#. All :term:`exact references` in the root (references that are
   :term:`fixed` by the root scanning function) must contain valid
   references or null pointers.

#. You must not store a reference in the root to a block in an
   automatically managed pool (such a reference is hidden from the MPS
   until you register the root, and may become invalid).

So the typical sequence of operations when creating a root is:

#. Initialize references in the root with null pointers or other safe
   values.

#. Register the root.

#. Fill in the references in the root.


.. index::
   pair: root; thread

.. _topic-root-thread:

Thread roots
------------

Every thread's :term:`registers` and :term:`control stack` potentially
contain references to allocated objects, so should be registered as a
root by calling :c:func:`mps_root_create_stack`.

The MPS's stack scanner needs to know how to find the bottom of the
part of the stack to scan. The bottom of the relevant part of stack
can be found by taking the address of a local variable in the function
that calls the main work function of your thread. You should take care
to ensure that the work function is not inlined so that the address is
definitely in the stack frame below any potential roots.

.. index::
   single: Scheme; thread root

For example, here's the code from the toy Scheme interpreter that
registers a thread root and then calls the program::

    mps_thr_t thread;
    mps_root_t stack_root;
    int exit_code;
    void *marker = &marker;

    res = mps_thread_reg(&thread, arena);
    if (res != MPS_RES_OK) error("Couldn't register thread");

    res = mps_root_create_stack(&stack_root, arena, mps_rank_ambig(),
                                0, thread, 0, 0, marker);
    if (res != MPS_RES_OK) error("Couldn't create root");

    exit_code = start(argc, argv);

    mps_root_destroy(stack_root);
    mps_thread_dereg(thread);


.. index::
   pair: root; rank

Ranks
-----

.. c:type:: mps_rank_t

    The type of :term:`ranks`. It is a :term:`transparent alias
    <transparent type>` for ``unsigned int``, provided for convenience
    and clarity.


.. c:function:: mps_rank_t mps_rank_ambig(void)

    Return the :term:`rank` of :term:`ambiguous roots`.


.. c:function:: mps_rank_t mps_rank_exact(void)

    Return the :term:`rank` of :term:`exact roots`.


.. c:function:: mps_rank_t mps_rank_weak(void)

    Return the :term:`rank` of :term:`weak roots`.


.. index::
   pair: root; mode

Root modes
----------

The root mode provides a way for the client to declare various facts
about a root that allow the MPS to make optimizations. Roots that are
declared to be *constant* need not be re-scanned, and roots that are
declared to be *protectable* may have barriers placed on them,
allowing the MPS to detect whether they have changed.

.. note::

    The MPS does not currently perform either of these optimizations,
    so root modes have no effect. These features may be added in a
    future release.


.. c:type:: mps_rm_t

    The type of :term:`root modes`.

    It should be zero (meaning neither constant or protectable), or
    the sum of some of :c:macro:`MPS_RM_CONST`,
    :c:macro:`MPS_RM_PROT`, and :c:macro:`MPS_RM_PROT_INNER`.


.. c:macro:: MPS_RM_CONST

    .. deprecated:: starting with version 1.111.

        This was introduced in the hope of being able to maintain a
        :term:`remembered set` for the root without needing a
        :term:`write barrier`, but it can't work as described, since
        you can't reliably create a valid registered constant root that
        contains any references. (If you add the references before
        registering the root, they may have become invalid; but you
        can't add them afterwards because the root is supposed to be
        constant.)

    The :term:`root mode` for :term:`constant roots`.
    This tells the MPS that the :term:`client program` will not change
    the :term:`root` after it is registered: that is, scanning the
    root will produce the same set of :term:`references`
    every time. Furthermore, for roots registered by
    :c:func:`mps_root_create_fmt` and :c:func:`mps_root_create_table`,
    the client program will not write to the root at all.

.. c:macro:: MPS_RM_PROT

    The :term:`root mode` for :term:`protectable roots`. This tells
    the MPS that it may place a :term:`barrier (1)` on any
    :term:`page` containing any part of the :term:`root`. No
    :term:`format method` or :term:`scan method` (except for the one
    for this root) may write data in this root. They may read it.

    .. note::

        You must not specify ``MPS_RM_PROT`` on a root allocated by
        the MPS.

        No page may contain parts of two or more protectable roots.
        You mustn't specify ``MPS_RM_PROT`` if the :term:`client
        program` or anything other than (this instance of) the MPS is
        going to protect or unprotect the relevant pages.

        This mode may not be suitable if the :term:`client program`
        wants the operating system to be able to access the root. Many
        operating systems can't cope with writing to protected pages.

.. c:macro:: MPS_RM_PROT_INNER

    The :term:`root mode` for :term:`protectable roots` whose inner
    pages (only) may be protected. This mode must not be specified
    unless :c:macro:`MPS_RM_PROT` is also specified. It tells the MPS
    that it may not place a :term:`barrier (1)` on a :term:`page`
    that's partly (but not wholly) covered by the :term:`root`.


.. index::
   single: root; interface

Root interface
--------------

.. c:type:: mps_root_t

    The type of :term:`root` descriptions.

    The :term:`arena` uses root descriptions to find
    :term:`references` within the :term:`client program's <client
    program>` roots.


.. c:function:: mps_res_t mps_root_create(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_root_scan_t root_scan, void *p, size_t s)

    Register a :term:`root` that consists of the :term:`references` fixed by a scanning function.

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

    The registered root description persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.


.. c:type:: mps_res_t (*mps_root_scan_t)(mps_ss_t ss, void *p, size_t s)

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
    Generally, it is better if it returns as soon as possible. If the
    scanning is completed successfully, the function should return
    :c:macro:`MPS_RES_OK`.


.. c:function:: mps_res_t mps_root_create_fmt(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_fmt_scan_t fmt_scan, mps_addr_t base, mps_addr_t limit)

    Register a :term:`root` that consists of the :term:`references` fixed by a scanning function in a block of
    :term:`formatted objects`.

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

    The registered root description persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.


.. c:function:: mps_res_t mps_root_create_stack(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_thr_t thr, mps_word_t mask, mps_word_t pattern, void *stack)

    Register a :term:`root` that consists of the :term:`references` in
    a :term:`thread's <thread>` registers and stack that match a
    binary pattern, for instance tagged as pointers.

    ``root_o`` points to a location that will hold the address of the
    new root description.

    ``arena`` is the arena.

    ``rank`` is the :term:`rank` of references in the root.

    ``rm`` is the :term:`root mode`.

    ``thr`` is the thread.

    ``mask`` is an arbitrary mask that is applied to each word in the
    thread's registers and stack.

    ``pattern`` is an arbitrary pattern; any word that is unequal to
    this (after masking with ``mask``) is not considered to be a
    reference.

    ``stack`` is a pointer into the thread's stack. On platforms where
    the stack grows downwards (currently, all supported platforms),
    locations below this address will be scanned.

    Returns :c:macro:`MPS_RES_OK` if the root was registered
    successfully, :c:macro:`MPS_RES_MEMORY` if the new root
    description could not be allocated, or another :term:`result code`
    if there was another error.

    The registered root description persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.

    .. warning::

        A risk of using tagged pointers in registers and on the stack
        is that in some circumstances, an optimizing compiler might
        optimize away the tagged pointer, keeping only the untagged
        version of the pointer. In this situation the pointer would be
        ignored and if it was the last reference to the object the MPS
        might incorrectly determine that it was dead. 

        You can avoid this risk by setting ``mask`` and ``pattern`` to
        zero: in this case all words in registers and on the stack are
        scanned, leading to possible additional scanning and retention.

     .. note::
        
        An optimization that may be worth considering is setting some
        of the top bits in ``mask`` so that addresses that cannot be
        allocated by the MPS are rejected quickly. This requires
        expertise with the platform's virtual memory interface.

                
.. c:function:: mps_res_t mps_root_create_table(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t count)

    Register a :term:`root` that consists of a vector of
    :term:`references`.

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

    The registered root description persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.

    .. _topic-root-type-pun:

    .. warning::

        The ``base`` argument has type ``mps_addr_t *`` (a typedef for
        ``void **``) but the table of references most likely has some
        other pointer type, ``my_object *`` say. It is tempting to
        write::

            mps_root_create_table(..., (mps_addr_t *)my_table, ...)

        but this is :term:`type punning`, and its behaviour is not
        defined in ANSI/ISO Standard C. (GCC and Clang have a warning
        flag ``-Wstrict-aliasing`` which detects some errors of this
        form.)

        To ensure well-defined behaviour, the pointer must be
        converted via ``void *`` (or via :c:type:`mps_addr_t`, which
        is a typedef for ``void *``), like this::

            mps_addr_t base = my_table;
            mps_root_create_table(..., base, ...)

.. c:function:: mps_res_t mps_root_create_table_masked(mps_root_t *root_o, mps_arena_t arena, mps_rank_t rank, mps_rm_t rm, mps_addr_t *base, size_t count, mps_word_t mask)

    Register a :term:`root` that consists of a vector of :term:`tagged
    references`.

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

    The registered root description persists until it is destroyed by
    calling :c:func:`mps_root_destroy`.

    For example::

        #define TAG_MASK 0x3            /* bottom two bits */

        /* Global symbol table. */
        size_t symtab_size;
        struct {
            obj_t symbol;
            obj_t value;
        } *symtab;

        mps_res_t res;
        mps_root_t root;
        mps_addr_t base = symtab;
        res = mps_root_create_table_masked(&root, arena,
                                           mps_rank_exact(),
                                           (mps_rm_t)0,
                                           base, symtab_size * 2,
                                           (mps_word_t)TAG_MASK);
        if (res != MPS_RES_OK) errror("can't create symtab root");

    .. warning::

        See the warning for :c:func:`mps_root_create_table` above.

.. c:function:: void mps_root_destroy(mps_root_t root)

    Deregister a :term:`root` and destroy its description.

    ``root`` is the root.


.. index::
   pair: root; introspection

Root introspection
------------------

.. c:function:: void mps_arena_roots_walk(mps_arena_t arena, mps_roots_stepper_t f, void *p, size_t s)

    .. deprecated:: starting with version 1.111.

        If you think you need this, there's probably a better way to
        achieve what you're trying to do. :ref:`Contact us <contact>`.

    Visit references in registered :term:`roots` in an
    :term:`arena`.

    ``arena`` is the arena whose roots you want to visit.

    ``f`` is a function that will be called for each reference to an
    object in an :term:`automatically <automatic memory management>`
    managed :term:`pool class` that was found in a registered root
    belonging to the arena. It takes four arguments: ``ref`` is the
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

.. _glossary-i:

=============================
Memory Management Glossary: I
=============================

.. include:: alphabet.txt

.. glossary::

    immediate data

        Immediate data is the representation of a :term:`value object`
        as one or more machine :term:`words`, as a register, or
        as a field in an instruction.

        Immediate data takes its name from the value of the object
        being immediately available, rather than requiring a
        :term:`load` or indirection through a :term:`reference`.

        .. similar:: :term:`unboxed`.

        .. opposite:: :term:`boxed`, :term:`pointer`, :term:`reference`.

    immune set

        The set of :term:`objects` which are not
        :term:`condemned <condemned set>`.

        .. opposite:: :term:`condemned set`.

    immutable

        In some programming languages, :term:`objects` of
        some types are immutable, that is, they cannot be modified.
        For example, in Standard :term:`ML`, only arrays and refs are
        mutable; all other objects are immutable.

        This property can be very useful for :term:`garbage
        collection`. For instance, no immutable object may contain a
        :term:`reference` to an object younger than itself, and no
        immutable object will appear in a :term:`remembered set`.
        Garbage collectors for these languages often take advantage of
        this property.

        In lazy languages, the evaluation of an expression may require
        an object of a different size, and adjustment of references
        may take place. This means that, although objects might be
        immutable at the language level, they are not immutable at the
        implementation level, and may contain references to younger
        objects.

        .. opposite:: :term:`mutable`.

        .. seealso:: :term:`generational garbage collection`.

    immutable object

        .. see:: :term:`value object`.

    in-band header

        .. aka:: *frame*, *header*.

        Some :term:`memory managers` :term:`allocate`
        a fixed amount more than is necessary for each :term:`block`
        and use it to store information such as the size of the block
        or a :term:`tag`. This extra memory is known as an *in-band
        header* or a *frame*.

        This is a form of :term:`internal fragmentation`, although
        sometimes, :term:`alignment` requirements result in free space
        for the header.

        Storing control information *in-band* often results in bad
        :term:`locality <locality of reference>`, particularly for
        :term:`deallocation <free (1)>`.

        .. opposite:: :term:`out-of-band header`.

        .. seealso:: :term:`activation frame`, :term:`stack frame`.

        .. mps:specific::

            In-band headers are supported by some :term:`pool classes`
            and the size of the header is specified by passing the
            :c:macro:`MPS_KEY_FMT_HEADER_SIZE` :term:`keyword
            argument` to :c:func:`mps_fmt_create_k`.

            A pointer to the first word after the in-band header is
            called a :term:`client pointer`.

    in parameter

        A function parameter that supplies data from the caller to the
        function. (The usual case in :term:`C`.)

        .. opposite:: :term:`out parameter`.

    in/out parameter

        A function parameter that is both an :term:`in parameter` an
        :term:`out parameter`.

        .. mps:specific::

            In/out parameters are given names ending with ``_io``. See
            :ref:`topic-interface`.

    incremental garbage collection

        Some :term:`tracing garbage collection` algorithms can pause
        in the middle of a :term:`collection cycle` while the
        :term:`mutator` continues, without ending up with inconsistent
        data. Such collectors can operate incrementally and are
        suitable for use in an interactive system.

        Primitive garbage :term:`collectors (1) <garbage collector>`,
        once they start a :term:`collection cycle`, must either finish
        the task, or abandon all their work so far. This is often an
        appropriate restriction, but is unacceptable when the system
        must guarantee response times; for example, in systems with a
        user interface and in real-time hardware control systems. Such
        systems might use incremental garbage collection so that the
        time-critical processing and the garbage collection can
        proceed effectively in parallel, without wasted effort.

        .. similar:: :term:`parallel garbage collection`.

        .. opposite:: :term:`stop-and-copy collection`.

        .. seealso:: :term:`barrier (1)`, :term:`tri-color marking`.

        .. bibref:: :ref:`Appel et al. (1988) <AEL88>`, :ref:`Boehm et al. (1991) <BDS91>`.

        .. mps:specific::

            The MPS uses incremental collection, except for
            collections started by calling
            :c:func:`mps_arena_collect`.

    incremental update

        Incremental-update algorithms for :term:`tracing <trace>`,
        :term:`incremental garbage collection` note changes made by
        the :term:`mutator` to the :term:`graph` of :term:`objects`
        and update the :term:`collector (2)` state to make it
        correctly trace the new graph.

        In order for the collector to miss a :term:`reachable`
        :term:`object`, the following two conditions need to hold at
        some point during tracing:

        1. The mutator stores a :term:`reference` to a :term:`white`
           object into a :term:`black` object.

        2. All paths from any :term:`gray` objects to that white
           object are destroyed.

        Incremental-update algorithms ensure the first condition
        cannot occur, by painting either the black or the white object
        gray (see :ref:`Pirinen (1998) <PIRINEN98>` for details).

        They are so called because they incrementally update the
        collector's view of the graph to track changes made by the
        mutator.

        .. historical::

            This distinction between incremental update and
            snapshot at the beginning was first introduced for
            write-barrier algorithms, but it applies to any type of
            tracing algorithm.

        .. opposite:: :term:`snapshot at the beginning`.

        .. seealso:: :term:`barrier (1)`, :term:`strong tri-color invariant`, :term:`tri-color marking`.

        .. bibref:: :ref:`Wilson (1994) <WIL94>`, :ref:`Pirinen (1998) <PIRINEN98>`.

    indefinite extent

        An :term:`object` has indefinite extent if its
        :term:`lifetime` is independent of the block or function-call
        structure of the program.

        The :term:`lifetime` of such an object can sometimes be
        determined by the programmer, and specified by :term:`freeing
        <free (1)>` the object explicitly. This becomes harder to do
        correctly as the program becomes more complex, especially if
        objects are passed across module boundaries, or if
        higher-order functions are used. In some languages it is
        impossible to determine the extent at compile-time. In these
        situations, a :term:`garbage collector` can be used to
        :term:`recycle` objects whose :term:`lifetime` has come to an
        end.

        .. opposite:: :term:`dynamic extent`.

    indexed fit

        A class of :term:`allocation mechanisms` that use an indexing
        data structure, such as a tree or hash table, to identify
        suitable :term:`free blocks`, according to the
        :term:`allocation policy`. For instance, a tree ordered by
        block size may be used to implement the :term:`best fit`
        policy.

        .. seealso:: :term:`allocation mechanism`, :term:`allocation policy`, :term:`bitmapped fit`, :term:`sequential fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    indirect method

        Indirect methods of :term:`automatic memory management` are
        those in which the information necessary to determine whether
        an :term:`object` can be :term:`reclaimed` is not
        stored in or associated with that object, but is derived from
        other objects.

        Indirect methods detect :term:`garbage` by :term:`tracing
        <trace>` :term:`reachable` objects.

        Indirect methods cannot always reclaim :term:`memory (2)` as
        soon as it becomes :term:`dead`, because it may be necessary
        to inspect many other objects to determine this. However, not
        having to store and update information on each object may
        reduce the overhead for the :term:`collector (1)`. In
        :term:`distributed garbage collection`, this can reduce the
        amount of communication between processors.

        .. similar:: :term:`tracing garbage collection`.

        .. opposite:: :term:`direct method`.

        .. bibref:: :ref:`Jones et al. (2012) <JONES12>`.

    infant mortality

        .. see:: :term:`generational hypothesis`.

    inline allocation (1)

        Allocation of objects by inline code, that is, without calling
        an allocation function. This is vital for performance in
        languages that allocate many small objects.

        .. mps:specific::

            This is achieved by the
            :ref:`topic-allocation-point-protocol`.

    inline allocation (2)

        Allocation of child objects inside their parent, as opposed
        to allocating child objects on the :term:`heap` and storing
        :term:`pointers` to them in the parent.

    inter-generational pointer

        An inter-generational pointer is a :term:`reference` that is
        stored in an :term:`object` in one :term:`generation` and
        references an object in another generation.

        If the referent's generation is :term:`condemned <condemned
        set>` and the referrer's generation is not, then the reference
        is important in two ways. First, the reference keeps the
        referent :term:`alive <live>`, so the referrer must be
        :term:`scanned <scan>` during the :term:`collection cycle`.
        Second, the reference must always refer to the referent, so if
        the referent is moved, then the referrer must be updated.

        During a collection, the only objects in non-condemned areas
        that must be scanned are the ones that contain
        inter-generational pointers. :term:`Generational garbage
        collectors <generational garbage collection>` make use of
        :term:`write barriers` and data structures like :term:`entry
        tables (2)`, :term:`exit tables`, and :term:`remembered sets`
        to track those objects at run-time.

        Inter-generational pointers can cause :term:`floating
        garbage`: even if both referrer and referent die, the
        inter-generational pointer will stop the referent from being
        reclaimed until the referrer's generation is condemned.

    interior pointer

        .. aka:: *derived pointer*.

        An *interior pointer* is a pointer to :term:`memory (2)`
        occupied by an :term:`object` which does not point to the
        start location. Also called a *derived pointer* when it's
        derived from a :term:`base pointer`.

        A :term:`pointer` to an object will usually take as its value
        the :term:`address` of the start of that object.

        It is common to have interior pointers into string buffers or
        to embedded structures. A :term:`suballocator` may place a
        :term:`header <in-band header>` at the start of each object
        and pass on an interior pointer.

        .. relevance::

            In a system where interior pointers are used, the
            :term:`garbage collector` must be able to :term:`mark
            <marking>` an object as :term:`reachable` without being
            told the start of the object. In a system where interior
            pointers are not used, the collector should either ignore
            them (in particular, if it is :term:`scanning <scan>`
            :term:`conservatively <conservative garbage collection>`)
            and not retain :term:`garbage` because of them, or
            possibly report them as bugs.

        .. opposite:: :term:`base pointer`.

    internal fragmentation

        Internal :term:`fragmentation` is where the :term:`memory
        manager` :term:`allocates` more for each allocation
        than is actually requested. There are three reasons for this:
        :term:`padding`; :term:`buddy system`; :term:`in-band headers`.

        .. seealso:: :term:`external fragmentation`.

    invalid page fault

        An exception when using :term:`virtual memory` resulting
        from an access to a virtual memory location for which no
        translation is defined.

        This is usually an error, often, anachronistically, known as a
        :term:`segmentation violation`.

        .. similar:: :term:`bus error`.

        .. seealso:: :term:`page fault`.

    inverted page table
    inverted page-table

        In a :term:`virtual memory` system, conventional
        :term:`page tables` have an entry for every
        :term:`page` in the :term:`virtual address space`. An
        *inverted page table* has only as many entries as there are
        pages in :term:`physical memory (1)`, and uses a hash lookup
        to translate :term:`virtual addresses` to
        :term:`physical addresses` in nearly
        constant time.

        The entire virtual address space of each process is described
        in an auxiliary structure, typically a B*-tree, that can
        efficiently store contiguous, sparse, or large :term:`address
        space` descriptions. This auxiliary structure may itself be
        paged to avoid permanently consuming :term:`physical memory
        (1)` resources.

        Inverted page tables are ideal for schemes that store
        information about :term:`objects` in the high-order
        bits of their :term:`address`. Such schemes may perform poorly
        with conventional page tables as the sparse address space may
        cause the page table structures to become so large as to
        compete with the program :term:`working set` for
        :term:`physical memory (1)`.

        .. historical::

            The :term:`Lisp Machine` was an early workstation that
            used an inverted page table with hardware lookup. The
            UltraSPARC, PowerPC, and IA-64 architectures all include
            inverted page tables. Some implementations of these
            architectures have hardware-assisted lookup.

    is-forwarded method

        .. mps:specific::

            A :term:`format method` that is called by a :term:`moving
            <moving garbage collector>` :term:`pool` to determine if a
            :term:`formatted object` is a :term:`forwarding object`,
            and if so, to return the address where the object was
            moved to. See :c:type:`mps_fmt_isfwd_t`.

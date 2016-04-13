.. _glossary-r:

=============================
Memory Management Glossary: R
=============================

.. include:: alphabet.txt

.. glossary::

    RAM

        .. aka:: *random access memory*.

        RAM (random access memory) is a type of :term:`physical memory
        (2)` that can be read from and written to.

        .. similar:: :term:`main memory`.

        .. seealso:: :term:`ROM`, :term:`static RAM`, :term:`dynamic RAM`.

    random access memory

        .. see:: :term:`RAM`.

    ramp allocation

        .. mps:specific::

            An :term:`allocation pattern` indicating to the MPS that
            most of the blocks allocated after the call to
            :c:func:`mps_ap_alloc_pattern_begin` are likely to be
            :term:`dead` by the time of the corresponding call to
            :c:func:`mps_ap_alloc_pattern_end`. See
            :ref:`topic-pattern-ramp`.

    rank

        .. mps:specific::

            A value of :c:type:`mps_rank_t` indicating whether a
            :term:`reference` is :term:`ambiguous <ambiguous root>`
            (:c:func:`mps_rank_ambig`), :term:`exact <exact root>`
            (:c:func:`mps_rank_exact`) or :term:`weak <weak root>`
            (:c:func:`mps_rank_weak`).

    rash

        .. mps:specific::

            A :term:`variety` in which no MPS functions :term:`assert
            <assertion>` that their data structures are valid. Select
            it by defining :c:macro:`CONFIG_VAR_RASH`. Compare
            :term:`cool` and :term:`hot`.

    raw

        .. see:: :term:`unwrapped`.

    reachable

        An :term:`object` is *reachable* if it is :term:`referred
        <reference>` to by a :term:`root`, or is referred to by a
        reachable object; that is, if it can be reached from the roots
        by following :term:`references`.

        Reachability is used as an approximation to :term:`liveness
        <live>` in :term:`tracing garbage collection`.

        In :term:`Java`, the :term:`reference objects` together with
        ordinary references and :term:`finalization` generate a
        hierarchy of reachability that guides the :term:`collector
        (1)` on what to do when an object is about to :term:`die
        <dead>`. There are six strengths:

        1. :term:`strongly reachable`;
        2. :term:`softly reachable`;
        3. :term:`weakly reachable`;
        4. :term:`finalizable <finalization>`;
        5. :term:`phantom reachable`;
        6. :term:`unreachable`.

        Basically, an object is only as reachable as the weakest link
        in the strongest path from the roots. Note that the Java
        specification's description of the reachabilities is a bit
        patchy, but that's what it intends. It is unspecified where
        Java Native Interface's *weak global references* fit into
        this.

        .. similar:: :term:`live`.

        .. opposite:: :term:`unreachable`.

        .. link::

            `Package java.lang.ref <http://docs.oracle.com/javase/8/docs/api/java/lang/ref/package-summary.html>`_, `Reference Objects and Garbage Collection <http://pawlan.com/monica/articles/refobjs/>`_.

    read barrier

        A read :term:`barrier (1)` is a block on reading from certain
        :term:`memory (2)` :term:`locations <memory location>` by
        certain threads or processes.

        .. relevance::

            Read barriers are used for :term:`incremental <incremental
            garbage collection>` or :term:`concurrent <parallel
            garbage collection>` :term:`garbage collection`.

        .. seealso:: :term:`write barrier`.

    read fault

        An exception which occurs when reading from an address in
        :term:`virtual memory`.

        This is probably either a :term:`page fault`, an
        :term:`invalid page fault` or a :term:`protection fault`.

        .. similar:: :term:`segmentation violation`.

        .. seealso:: :term:`write fault`.

    read-only memory

        .. see:: :term:`ROM`.

    real memory (1)

        A system with no :term:`virtual memory` capability can be
        said to have *real memory*.

        .. historical::

            On older architectures, programs could only directly
            access data in real memory. Where this was inefficient,
            they had to store data on disk, and sometimes had
            alternate portions of program image called *overlays*.

        .. opposite:: :term:`virtual memory`.

    real memory (2)

        .. see:: :term:`physical memory (1)`.

    reclaim

        *Reclaiming* an :term:`object` or the :term:`memory (1)`
        occupied by it is making it available for reuse after the
        object is no longer needed.

        This word is usually used only in connection with
        :term:`automatic memory management`.

        .. similar:: :term:`recycle`.

    recycle

        *Recycling* :term:`memory (1)` means making it available for
        reuse after it has been occupied by an :term:`object` that is
        no longer needed.

        In simple cases, this might simply involve adding a
        :term:`memory (2)` :term:`block` to the :term:`free list`.
        Another possibility is :term:`unmapping <unmapped>` memory so
        that the :term:`backing store` can be allocated to another
        process.

        .. similar:: :term:`reclaim`.

    reference

        In memory management, *a reference* is the general term for a
        link from one :term:`object` to another. Some programming
        languages have more specific meanings for the term.

        The terms ":term:`pointer`" and "reference" are often
        interchangeable, but some programming languages differentiate
        the two in subtle ways.

        .. similar:: :term:`address`, :term:`pointer`.

        .. mps:specific::

            A reference is represented in the :term:`C` interface by a
            value of type :c:type:`mps_addr_t` (an alias for ``void
            *``) which points to a :term:`memory location` within the
            object (typically the base of the object, but for objects
            with :term:`headers <in-band header>` this may not be the
            case). The pointer returned by :c:func:`mps_alloc` and
            :c:func:`mps_reserve` is a reference to the object
            allocated.

            The :term:`client program` is free to represent references
            as it chooses (for example, with :term:`tags <tagged
            reference>`), provided that during :term:`scanning <scan>`
            it is able to decode a reference from its representation
            into the MPS interface representation and encode a
            reference from the MPS into its representation.

    reference counting

        Reference counting systems perform :term:`automatic memory
        management` by keeping a count in each :term:`object`, usually
        in a :term:`header <in-band header>`, of how many
        :term:`references` there are to the object.
        Objects to which there are no references cannot be accessed by
        the :term:`mutator`; they are therefore :term:`dead` and may
        be :term:`reclaimed <reclaim>`.

        The reference count is incremented for each new reference, and
        is decremented if a reference is overwritten, or if the
        referring object is recycled. If a reference count falls to
        zero, then the object is no longer required and can be
        recycled.

        There are four main problems with simple reference counting:

        1. The reference count field usually has to have limited size,
           and the system therefore breaks down if the number of
           possible references to an object is unbounded;

        2. Reference counting involves an operation on every
           modification of a pointer, which increases code size,
           increases demand for :term:`memory bandwidth`, decreases
           :term:`locality of reference` and can be a serious
           performance penalty (especially in multi-threaded
           environments where reference count updates require
           synchronization);

        3. Every object needs to be slightly larger in order to store
           the reference count;

        4. If any objects are part of a :term:`cyclic data structure`
           then they will always have a non-zero reference count, and
           hence won't be reclaimed when they are dead.

        .. figure:: ../diagrams/refloop.svg
            :align: center
            :alt: Diagram: Garbage with non-zero reference counts.

            Garbage with non-zero reference counts.

        Reference counting has the advantage that it can reclaim
        objects promptly, and for this reason it is often used to
        reclaim non-cyclic data structures in file systems, databases
        and operating system kernels. When there is a possibility of
        cyclic data structures, reference counting is sometimes used
        together with a :term:`tracing garbage collector <tracing
        garbage collection>` that runs infrequently. Such combinations
        are generally less efficient than using a tracing collector by
        itself, but the promptness of reference counting may be
        important.

        Pauses due to reference counting are typically fairly short,
        and it may be appropriate as a form of :term:`incremental
        garbage collection`. But removing a single reference may cause
        the recycling of a large number of objects at once, so it is
        not suited to real-time systems where minimum pause times must
        be guaranteed. There are more complex variations of the
        technique that address this problem.

        Reference counting is often used because it can be implemented
        without any support from the language or compiler. In
        :term:`C++` this can be encapsulated in a class, using a
        :term:`smart pointer`. However, it would normally be more
        efficient to use a tracing garbage collector instead. The
        performance of reference counting can be improved
        substantially with compiler support, using refinements such as
        :term:`deferred reference counting`, which has been
        successfully used in :term:`Smalltalk` and other languages.

        Despite the problems, reference counting is often used for
        :term:`distributed garbage collection`. This is because
        refinements such as :term:`weighted reference counting`
        require less inter-process communication than :term:`tracing
        <trace>`.

        .. seealso:: :term:`limited-field reference count`, :term:`one-bit reference count`.

    reference object

        In :term:`Java`, a *reference object*
        (``java.lang.ref.Reference``) encapsulates a :term:`reference`
        to some other object, in order to make the :term:`garbage
        collector` handle it specially. In particular, a Java program
        can use this to detect when the referent becomes
        :term:`unreachable`.

        Basically, the encapsulated reference is a :term:`weak
        reference (1)`; it will be cleared by the :term:`collector
        (1)` when all other references to the referent have
        disappeared. However, in order to better control what happens
        at the end of an object's :term:`lifetime`, Java 1.2 provides
        three classes of reference objects, each with its own
        peculiarities: ``SoftReference``, ``WeakReference``, and
        ``PhantomReference``. Each of these classes has its uses in
        managing memory. The reference objects together with ordinary
        references and :term:`finalization` generate a hierarchy of
        :term:`reachability <reachable>` that guides the collector on
        what to do when an object is about to :term:`die <dead>`.

        A reference object can be *registered* with a queue, and it
        will be enqueued when the collector determines that the
        referent is :term:`softly <softly reachable>`, :term:`weakly
        <weakly reachable>` or :term:`phantom reachable`, as the case
        may be. A program can use these queues to perform some action
        when an object is dying. This allows finer control than the
        older :term:`finalization` mechanism alone.

        .. historical::

            This feature was introduced in Java 1.2 (confusingly, part
            of the Java 2 Platform).

        .. seealso:: :term:`soft reference`, :term:`weak reference (2)`, :term:`phantom reference`.

        .. link::

            `Package java.lang.ref <http://docs.oracle.com/javase/8/docs/api/java/lang/ref/package-summary.html>`_, `Reference Objects and Garbage Collection <http://pawlan.com/monica/articles/refobjs/>`_.

        .. bibref:: :ref:`Dybvig et al. (1993) <DBE93>`.

    region inference

        Region inference is a technique for determining when
        :term:`objects` become :term:`dead` (even if they are
        :term:`reachable`) by a static analysis of the program.

        Region inference infers a *region* for each object. When a
        region dies, all the objects in it are known to be
        :term:`dead`, whether reachable or not. Regions obey a strict
        :term:`stack` discipline; that is, when a region dies, all
        younger regions also die. In this way, region inference
        occupies a middle ground between :term:`stack allocation` and
        :term:`heap allocation`.

        .. bibref:: :ref:`Tofte & Talpin (1997) <TT97>`.

    register

        A *register* is a small unit of :term:`memory (2)` that is
        attached to a processor and accessible very quickly. Registers
        typically form the highest level of a computer's
        :term:`storage hierarchy`.

        .. relevance::

            In some programs (for example, those compiled by typical
            :term:`C` or :term:`C++` compilers), a subset of the
            registers is always accessible by the :term:`mutator` and
            so forms a :term:`root`.

        .. mps:specific::

            The :term:`scan method` for the root containing the
            registers is hard to write (it depends on the operating
            system, the processor architecture, and in some cases the
            compiler), so the MPS provides (on its supported
            platforms) the function :c:func:`mps_stack_scan_ambig`.

    register set partitioning

        Run-time systems for :term:`garbage-collected <garbage
        collection>` languages sometimes partition the set of machine
        :term:`registers` *a priori* into two categories:
        those always :term:`traced <trace>` and updated by the
        :term:`garbage collector` and those ignored by it.

        The former are always maintained in a format understood by the
        collector; the latter are never used to hold
        :term:`references` to collectable :term:`objects`. More
        complicated schemes are also possible.

        This partitioning provides a separation of concerns between
        the compiler and the :term:`garbage collector`. The compiler
        can generate code that produces values the garbage collector
        would not be able to handle (say, because they have no
        :term:`tags`), as long as those values are kept in the
        ignored registers. The garbage collector can trust that the
        registers it looks at always contain valid data, and can
        perform :term:`exact garbage collection`.

        Register set partitioning increases the demand for registers
        (*register pressure*), but may reduce the amount of
        :term:`boxing <boxed>` needed.

    relocation

        *Relocating* means moving data from one location to another
        and updating all :term:`references`.

        Relocation is often performed to avoid :term:`external fragmentation`.

        Program loading sometimes relocates code and :term:`static
        <static allocation>` data.

        .. similar:: :term:`moving <moving garbage collector>`.

        .. seealso:: :term:`compaction`, :term:`moving memory manager`.

    remembered set

        A remembered set is the technique of keeping a separate list
        of interesting :term:`references` between two sets
        of :term:`objects`, so you don't have to find them by
        :term:`scanning <scan>`.

        Many :term:`memory management` algorithms depend on
        partitioning the objects and require special handling for
        references between partitions. Keeping track of such
        references in a remembered set eliminates the need to scan the
        originating partition to find them.

        A typical use in :term:`generational garbage collection` is
        remembering :term:`references` from an older
        :term:`generation` to a younger one.

        .. similar:: :term:`entry table (2)`.

        .. bibref:: :ref:`Ungar (1984) <UNGAR84>`, :ref:`Jones et al. (2012) <JONES12>`.

    remote reference

        .. mps:specific::

            A :term:`reference` that logically belongs to a
            :term:`formatted object` and so must be :term:`fixed` when
            the object is :term:`scanned <scan>`, but which is not
            stored within the block containing the object. (For
            example, in an auxiliary table of some sort.)

            The MPS does not generally support remote references
            because those references may be :term:`protected
            <protection>` and so if :term:`scan method` attempts to
            :term:`fix` them this will hit a :term:`barrier (1)` and
            cause a re-entrant call to the MPS.

    replicating garbage collector

        A variant of :term:`copying garbage collection`, which does
        not destroy the original :term:`object` when making a copy.

        This is useful in an :term:`incremental <incremental garbage
        collection>` or :term:`concurrent <parallel garbage
        collection>` :term:`collector (1)`, as no :term:`read barrier`
        is required; the :term:`mutator` can continue to use old
        objects. The collector uses a :term:`write barrier` to
        replicate the writes to the new copies.

        .. seealso:: :term:`copying garbage collection`, :term:`broken heart`.

        .. bibref:: :ref:`Nettles et al. (1992) <NOPH92>`, :ref:`Nettles & O'Toole (1993) <NO93>`, :ref:`Nettles & O'Toole (1993a) <NO93A>`, :ref:`O'Toole & Nettles (1994) <ON94>`.

    reserved

        In a :term:`virtual memory` system, it is usually possible
        to hold range of :term:`virtual addresses`
        *reserved* without making it :term:`mapped`.

        Reserving addresses prevents other components of the program
        using the same addresses, without consuming :term:`swap
        space`. This technique is often used in :term:`BIBOP` schemes,
        where one might want to reserve a large amount of
        :term:`address space` but only sparsely map it.

        On some systems there are special calls for reserving; on
        others one can create :term:`mappings` that don't
        need :term:`backing store`. For example, on some Unix systems,
        ``mmap /dev/zero`` with no access.

        .. seealso:: :term:`mapping`, :term:`mmap`.

        .. mps:specific::

            The function :c:func:`mps_arena_reserved` returns the
            total address space reserved by an arena.

    resident

        In a :term:`cache (2)` system, that part of the cached storage
        which currently has a copy in the cache is called *resident*.
        Ideally, the :term:`working set` should be resident.

        .. seealso:: :term:`cache (2)`, :term:`storage hierarchy`, :term:`resident set`.

    resident set

        In a :term:`virtual memory` system, a process' resident
        set is that part of a process' :term:`address space` which is
        currently in :term:`main memory`. If this does not include all
        of the process' :term:`working set`, the system may
        :term:`thrash`.

    result code

        .. mps:specific::

            A value returned from an MPS function, represented by the
            type :c:type:`mps_res_t`. The result code
            :c:macro:`MPS_RES_OK` indicates success; other values
            indicate errors. See :ref:`topic-error`.

    resurrection

        An object is said to have been *resurrected* if it was
        determined to be :term:`finalizable <finalization>` by the
        :term:`garbage collector` (that is, the only thing keeping it
        alive was the fact that it required finalization), but then a
        new :term:`strong reference` was created to it.

        This can happen via a :term:`weak reference (1)` or by the
        finalization procedure storing a permanent copy of its
        reference to the object.

        .. mps:specific:: See :ref:`topic-finalization`.

    retention

        The failure to :term:`recycle` :term:`floating garbage`, due
        to some approximation or optimization in the :term:`garbage
        collector`; also the amount of memory thus retained.

        .. bibref:: :ref:`Boehm (2001) <BOEHM01>`.

    ROM

        .. aka:: *read-only memory*.

        ROM (read-only memory) is a type of :term:`physical memory
        (2)` that can be read from, but not written to. The contents
        of ROM are usually set in the factory.

        .. seealso:: :term:`RAM`.

    root

        In :term:`tracing garbage collection`, a root holds a
        :term:`reference` or set of references to :term:`objects` that
        are *a priori* :term:`reachable`. The :term:`root set` is used
        as the starting point in determining all reachable data.

        Roots basically comprise the references in the state of the
        :term:`mutator`. Typical roots are global variables, other
        :term:`static <static allocation>` data, and the
        :term:`control stack`.

        .. seealso:: :term:`weak root`, :term:`strong root`, :term:`ambiguous root`, :term:`exact root`.

        .. mps:specific:: See :ref:`topic-root`.

    root description

        .. mps:specific::

            The :term:`arena` uses root descriptions to find
            :term:`references` within the :term:`client
            program's <client program>` :term:`roots`. Root
            descriptions belong to the type :c:type:`mps_root_t`.

    root mode

        .. mps:specific::

            A value of type :c:type:`mps_rm_t` describing whether a
            :term:`root` is :term:`constant <constant root>`,
            :term:`protectable <protectable root>`, or both. The root
            mode tells the MPS whether it may place a :term:`barrier
            (1)` on the root.

    root set

        The *root set* is the collection of :term:`roots` that
        the :term:`mutator` declares to the :term:`collector (2)`.

        .. seealso:: :term:`garbage collection`.

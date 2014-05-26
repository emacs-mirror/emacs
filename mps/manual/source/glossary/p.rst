.. _glossary-p:

=============================
Memory Management Glossary: P
=============================

.. include:: alphabet.txt

.. glossary::

    padding

        Padding is redundant :term:`memory (2)` within the memory
        :term:`allocated` to an :term:`object`. It is usually inserted
        because of :term:`alignment` restrictions on the fields of the
        object or on the object itself.

        Padding is a form of :term:`internal fragmentation`.

    padding method

        .. mps:specific::

            A :term:`format method` that is called by a :term:`moving
            <moving garbage collector>` :term:`pool <pool>` to create
            a :term:`padding object`. See :c:type:`mps_fmt_pad_t`.

    padding object

        .. mps:specific::

            A :term:`formatted object` that consists of
            :term:`padding`. One of three types of formatted objects,
            the other two being :term:`client objects`
            and :term:`forwarding objects`.

    page

        A :term:`virtual memory` system usually deals with
        :term:`memory (1)` :term:`blocks` of fixed size as
        units for :term:`paging`. These are known as *pages*.

        Pages are often 4 :term:`kB <kilobyte>` or 8 kB in size. This
        size is determined by the addressing hardware of the machine.

    page fault

        An exception when accessing :term:`virtual memory`,
        usually resulting in a :term:`page` being fetched from disk.

        A page fault is an exception occurring during the translation
        of :term:`virtual addresses` to
        :term:`physical addresses`. "Page fault"
        usually means an access to a page that has been :term:`paged
        out` and hence requires fetching from disk, but it is
        sometimes also used to mean :term:`invalid page fault` or
        :term:`protection fault`.

        .. seealso:: :term:`paging`, :term:`paged in`, :term:`paged out`, :term:`read fault`, :term:`write fault`.

    page marking

        Page marking is a form of :term:`card-marking <card marking>`
        where the :term:`card` is the same size as a :term:`page`

    page protection

        .. see:: :term:`protection`.

        Many operating systems support protection of :term:`memory
        (2)` :term:`pages`. Individual pages may be protected
        against a combination of read, write or execute accesses by a
        process.

    page table

        In a :term:`virtual memory` system, it is common to map
        between :term:`virtual addresses` and
        :term:`physical addresses` by means of a
        data structure called a *page table*.

        The :term:`page` number of an address is usually found from
        the most significant bits of the address; the remaining bits
        yield the offset of the :term:`memory location` within the
        page. The page table is normally indexed by page number and
        contains information on whether the page is currently in
        :term:`main memory`, and where it is in main memory or on
        disk.

        Conventional page tables are sized to the virtual
        :term:`address space` and store the entire virtual address
        space description of each process. Because of the need to keep
        the virtual-to-physical translation time low, a conventional
        page table is structured as a fixed, multi-level hierarchy,
        and can be very inefficient at representing a sparse virtual
        address space, unless the allocated pages are carefully
        aligned to the page table hierarchy.

        .. seealso:: :term:`inverted page table`.

    paged in

        In a :term:`virtual memory` system, :term:`memory (2)` is
        described as *paged in* if it is available in :term:`physical
        memory (1)`.

        .. similar:: :term:`swapped in`.

        .. opposite:: :term:`paged out`.

        .. seealso:: :term:`paging`.

    paged out

        In a :term:`virtual memory` system, :term:`memory (2)` is
        described as *paged out* if it is not available in
        :term:`physical memory (1)`.

        .. similar:: :term:`swapped out`.

        .. opposite:: :term:`paged in`.

        .. seealso:: :term:`paging`.

    paging

        In a :term:`virtual memory` system, *paging* is the act of
        transferring :term:`pages` between :term:`physical
        memory (1)` and :term:`backing store` (usually disk).

        When pages need to be paged out, a heuristic is used to select
        ones that will not be needed soon; "least recently used" is a
        popular one.

        .. similar:: :term:`swapping`.

        .. seealso:: :term:`paged in`, :term:`paged out`.

    palimpsest

        A :term:`block` of :term:`memory (2)` that has been
        :term:`allocated`, :term:`freed (1)` (or :term:`reclaimed`),
        and then allocated again. Such memory may contain data from
        the previous use if portions of it remain uninitialised.

        This commonly occurs on the :term:`stack`, especially if the
        compiler allocates large :term:`stack frames` in
        anticipation of allocating data structures on the stack.

        If the palimpsest is being :term:`scanned <scan>`
        :term:`conservatively <conservative garbage collection>`, such
        left-over data may cause :term:`unreachable` :term:`objects`
        to appear :term:`reachable` and thus become :term:`floating
        garbage`. If it is scanned :term:`precisely <exact garbage
        collection>`, such left-over data, if treated as
        :term:`pointers`, is a bug.

    parallel garbage collection

        .. aka:: *concurrent garbage collection*.

        A parallel or concurrent :term:`collector (2)` executes
        simultaneously with the :term:`mutator`, usually on a
        multi-processor machine.

        Concurrent :term:`garbage collection` must cope with the
        mutator changing :term:`objects` while collection
        occurs. The problem is similar to that of :term:`incremental
        GC <incremental garbage collection>`, but harder. The solution
        typically involves :term:`barrier (1)`.

        .. similar:: :term:`incremental <incremental garbage collection>`.

        .. seealso:: :term:`replicating garbage collector`.

        .. bibref:: :ref:`Doligez & Leroy (1993) <DOLIGEZ93>`, :ref:`Doligez & Gonthier (1994) <DOLIGEZ94>`.

    parked state

        .. mps:specific::

            One of the three states an :term:`arena` can be in (the
            others being the :term:`clamped state` and the
            :term:`unclamped state`). In the parked state, no
            :term:`garbage collection` is in progress, no object
            motion occurs and the staleness of :term:`location
            dependencies` does not change. Call
            :c:func:`mps_arena_park` or :c:func:`mps_arena_collect` to
            put an arena into the parked state.

    perfect fit

        If an :term:`allocation <allocate>` request is satisfied
        exactly from a :term:`free block` with no
        :term:`fragmentation`, this is said to be a :term:`perfect
        fit`.

        .. seealso:: :term:`free block`, :term:`allocation mechanism`, :term:`best fit`.

    phantom reachable
    phantomly reachable

        In :term:`Java`, an object is *phantom reachable* if it is
        neither :term:`strongly <strongly reachable>` nor
        :term:`softly <softly reachable>` nor :term:`weakly reachable`
        and has been :term:`finalized <finalization>` and there is a
        path from the :term:`roots` to it that contains at
        least one :term:`phantom reference`.

        When the Java :term:`collector (1)` determines that an object
        is phantom reachable, the :term:`reference objects` containing
        the phantom references are enqueued.

        The Java specification says that the phantom reference is not
        cleared when the reference object is enqueued, but actually,
        there's no way in the language to tell whether that has been
        done or not. In some implementations, JNI weak global
        references are weaker than phantom references, and provide a
        way to access phantom reachable objects.

        .. seealso:: :term:`reachability <reachable>`.

        .. link::

            `Class java.lang.ref.PhantomReference <http://docs.oracle.com/javase/8/docs/api/java/lang/ref/PhantomReference.html>`_, `Reference Objects and Garbage Collection <http://pawlan.com/monica/articles/refobjs/>`_.

    phantom reference

        In :term:`Java` terminology, *phantom reference* is used to
        mean a :term:`reference` encapsulated in a :term:`reference
        object` of class ``PhantomReference``.

        Phantom references form one of three kinds of :term:`weak
        reference (1)` in Java. They are handy for performing
        clean-ups after an object has :term:`died <dead>` and been
        :term:`finalized <finalization>`.

        .. seealso:: :term:`phantom reachable`.

        .. link::

            `Class java.lang.ref.PhantomReference <http://docs.oracle.com/javase/8/docs/api/java/lang/ref/PhantomReference.html>`_, `Reference Objects and Garbage Collection <http://pawlan.com/monica/articles/refobjs/>`_.

    physical address

        .. aka:: *absolute address*.

        Physical :term:`addresses` are used to index into
        :term:`physical memory (1)`. On some systems, they are called
        *absolute addresses*.

        In a :term:`virtual memory` system the application program
        handles :term:`virtual addresses` and these
        are translated to physical addresses by the :term:`MMU`.

        .. opposite:: :term:`virtual address`.

    physical address space

        The physical :term:`address space` is the space of
        :term:`physical addresses`.

        .. opposite:: :term:`virtual address space`.

    physical memory (1)

        .. aka:: *real memory*.

        Physical memory is :term:`memory (1)` that is wired to
        directly to the processor, addressable by :term:`physical
        address`.

        This term is basically synonymous to :term:`main memory`, but
        is used in contrast to :term:`virtual memory` and
        :term:`backing store`.

        While modern computers usually have lots of :term:`virtual
        memory`, performance is still closely related to the
        quantity of physical memory available. If a system has
        insufficient physical memory, it may :term:`thrash`.

        .. similar:: :term:`main memory`.

    physical memory (2)

        .. aka:: *physical storage*.

        Physical memory is :term:`memory (1)` on physical storage
        devices, such as :term:`RAM` or disks.

        This term is often contrasted to :term:`virtual address space`
        that might not be mapped to any actual storage.

        .. similar:: :term:`memory (1)`.

    physical storage

        .. see:: :term:`physical memory (2)`.

    pig in the python

        .. aka:: *pig in the snake*.

        In a :term:`generational <generational garbage collection>`
        collector, when a large and long-lived :term:`object` is
        :term:`allocated` in :term:`nursery space`, collection effort
        will be wasted as that object survives and is :term:`promoted
        <promotion>` from :term:`generation` to generation. This is
        especially noticeable in a :term:`copying collector <copying
        garbage collection>`, where the large object will be copied
        many times. This difficulty is similar to that of a python
        which swallows its prey whole and is somewhat immobilized as
        it digests it.

        Modern collectors permit objects to be allocated directly into
        appropriate generations or pools to avoid this problem.
        Long-lived objects can be allocated directly into long-term
        generations. Large objects can be allocated directly into
        pools with special support for large objects (such as copying
        by remapping, incremental copying, or not copying at all).

        .. seealso:: :term:`generational garbage collection`.

        .. mps:specific::

            A :term:`pool` can be configured to allocate into a
            specific :term:`generation` in its :term:`generation
            chain` by setting the :c:macro:`MPS_KEY_GEN`
            :term:`keyword argument` when calling
            :c:func:`mps_pool_create_k`.

    pig in the snake

        .. see:: :term:`pig in the python`.

    pinning

        .. aka:: *nailing*.

        In :term:`copying garbage collection`, an object may not be
        movable because it is the target of an :term:`ambiguous
        reference` or because it is referenced by :term:`foreign code`
        that does not co-operate with the collector. Such an object is
        said to be *pinned*.

    placement policy

        .. see:: :term:`allocation policy`.

    platform

        .. mps:specific::

            The term *platform* is used to refer to the combination of
            operating system, processor architecture, and compiler.
            See :ref:`topic-platform`.

    plinth

        .. mps:specific::

            The plinth is a program module providing the MPS with all
            the support functions it needs from the execution
            environment. The plinth removes the need for external
            libraries, by getting the support from the :term:`client
            program`. See :ref:`topic-plinth`.

    pointer

        *Pointer* data types represent a reference to an
        :term:`object` or a :term:`location <memory location>`.

        Pointers may be specialized by the type of the object referred
        to.

        Typically, pointers are represented by an :term:`address`, but
        they can be more complicated when they need to carry more
        information. For example, when the referent is smaller than a
        :term:`word`, an offset within the word might be needed.

        .. similar:: :term:`reference`, :term:`address`.

        .. seealso:: :term:`tag`.

    pool

        .. mps:specific::

            A pool is responsible for requesting memory from the
            :term:`arena` and making it available to the :term:`client
            program` via :c:func:`mps_alloc` or via an
            :term:`allocation point`. Multiple pools can coexist in
            one arena. Pools belong to the type
            :c:type:`mps_pool_t`. See :ref:`topic-pool` and the
            :ref:`pool`.

    pool class

        .. mps:specific::

            A value of type :c:type:`mps_class_t` describing a class
            of :term:`pools` that manage memory according to
            particular policy. See :ref:`pool`.

    precise garbage collection

        .. see:: :term:`exact garbage collection`.

    precise reference

        .. see:: :term:`exact reference`.

    precise root

        .. see:: :term:`exact root`.

    premature free

        .. aka:: *use after free*.

        A *premature free* or *use after free* occurs when
        :term:`memory (2)` is :term:`deallocated <free (1)>`, but is
        later accessed.

        Under :term:`manual memory management`, this usually occurs
        when one part of a program decides it has finished using a
        memory :term:`block`, and is unaware that another part of the
        program is still using it. This is rare under :term:`automatic
        memory management`.

        .. seealso:: :term:`double free`.

    premature promotion

        .. see:: :term:`premature tenuring`.

    premature tenuring

        .. aka:: *premature promotion*.

        When a short-lived :term:`object` :term:`allocated` in a
        :term:`generational garbage collector <generational garbage
        collection>` is :term:`promoted <promotion>` (due to poor
        timing) into a less-frequently collected :term:`generation`.
        This *prematurely tenured* object may become :term:`garbage`
        very soon after promotion, but will not be :term:`reclaimed`
        for some time because it is now in a less frequently collected
        generation.

        This problem is essentially due to quantization error: all
        objects in a generation are treated as if they have the same
        age, even though they range from as old as the previous
        promotion cycle to new-born.

        Modern :term:`collectors (1) <garbage collector>` offer
        several remedies for premature tenuring. If the client program
        knows that it is entering a phase that will create many
        short-lived objects, it can forestall all promotion until it
        knows it is done with those objects. Thus no objects will be
        prematurely promoted: they will all be seen as garbage.
        Another solution is to create :term:`buckets` within
        generations to more accurately classify objects by age and
        only promote those which have reached a certain minimum.

    primary storage

        .. see:: :term:`main memory`.

    promotion

        .. aka:: *tenuring*.

        Promotion or tenuring is the act of moving an :term:`object`
        from its current :term:`generation` to an *older* one (one
        that contains objects that are expected to survive longer).

        "Tenuring" is used particularly about promotion to the oldest
        generation.

        .. seealso:: :term:`generational garbage collection`.

    protectable root

        .. mps:specific::

            A :term:`root` which the MPS may :term:`protect
            <protection>` with a :term:`write barrier`. A protectable
            root is created by specifying the :term:`root mode`
            :c:macro:`MPS_RM_PROT` when calling a registration
            function such as :c:func:`mps_root_create`.

    protection

        .. aka:: *memory protection*, *page protection*.

        Many operating systems support protection of :term:`memory
        (2)` :term:`pages`. Individual pages may be protected
        against a combination of read, write or execute accesses by a
        process.

        A process which attempts a protected access will trigger a
        :term:`protection fault`. Protection is typically implemented
        in hardware by the :term:`MMU` as part of the support for
        :term:`virtual memory` .

        Pages can be protected for a number of reasons: a
        :term:`generational <generational garbage collection>` or
        :term:`incremental <incremental garbage collection>`
        :term:`garbage collector` may want to place :term:`barriers
        (1)` on pages; an operating system may want to protect pages
        for security, or to implement "copy-on-write" or
        "demand-zero-filled" pages.

        .. seealso:: :term:`read fault`, :term:`write fault`.

        .. bibref:: :ref:`Appel et al. (1988) <AEL88>`, :ref:`Singhal et al. (1992) <SINGHAL92>`, :ref:`Hosking & Moss (1993) <HM93>`.

    protection exception

        .. see:: :term:`protection fault`.

    protection fault

        .. aka:: *barrier hit*, *protection exception*, *protection violation*.

        A protection fault is an exception or trap which occurs when a
        process attempts to access :term:`memory (2)` which has been
        :term:`protected <protection>`.

        .. relevance::

            Some :term:`garbage collectors` use handlers for
            protection faults to provide :term:`barriers (1)`.

        .. seealso:: :term:`segmentation violation`, :term:`General Protection Fault`.

    protection violation

        .. see:: :term:`protection fault`.


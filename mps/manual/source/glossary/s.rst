.. _glossary-s:

=============================
Memory Management Glossary: S
=============================

.. include:: alphabet.txt

.. glossary::

    sbrk

        ``sbrk`` is a Unix library function that adjusts the limit of
        the data segment; this limit is known as the *break*.

        ``sbrk`` returns the previous value of the break, so
        ``sbrk(0)`` is a common idiom for getting the current value.

        Note that, if you use :term:`brk`, you probably can't safely
        use ``sbrk`` as well, because it may store the last value of
        the break in a private variable.

    scalar data type

        A scalar data type is a type that is representable in a single
        dimension and whose objects have only magnitude as value.

        Examples of scalar data types include: integers,
        floating-point numbers, enumerations, and characters.

        .. relevance::

            The objects of a scalar data type are :term:`leaf
            objects`. Scalar data types with bounded magnitude can be
            represented compactly using :term:`value objects`.

        .. historical::

            Because compact representation solves many memory
            management issues, many older programming languages only
            offered bounded scalar data types. For example, the
            ``int`` type in :term:`C` is defined to have a magnitude
            that can be represented by a :term:`word`.

        .. seealso:: :term:`vector data type`, :term:`algebraic data type`, :term:`value object`, :term:`leaf object`.

    scan

        The examination of an :term:`object` or an area of
        :term:`memory (2)` to find :term:`references`,
        typically as part of :term:`tracing <trace>`.

        Scanning examines memory that has been decided to be
        non-:term:`garbage`, to find references to objects that have
        been :term:`condemned <condemned set>`.

        .. mps:specific::

            See :ref:`topic-scanning`.

    scan method

        .. mps:specific::

            A function that examines a block of memory to find
            :term:`references` and indicate them to the
            MPS. A scan method forms part of an :term:`object format`.
            See :c:type:`mps_fmt_scan_t`.

    scan state

        .. mps:specific::

            A scan state represents the state of the current
            :term:`scan`. The MPS passes a scan state to the
            :term:`scan method` of an :term:`object format` when it
            needs to :term:`scan` for :term:`references`
            within a region of memory. Scan states belong to the type
            :c:type:`mps_ss_t`.


    scavenging garbage collection

        .. see:: :term:`copying garbage collection`.

    SDRAM

        Synchronous Dynamic Random Access Memory. A high performance
        variant of :term:`DRAM <dynamic memory>`.

        SDRAM uses an external clock signal to synchronize its data
        input and output. It is capable of achieving very high data
        rates for linear access to memory.

    segmentation violation

        A segmentation violation occurs when an attempt is made to
        access :term:`memory (2)` whose :term:`address` is
        well-formed, but to which access cannot be granted. This might
        be due to either a :term:`protection fault` or an
        :term:`invalid page fault`.

        The term is sometimes used more loosely as a synonym for any
        memory access error, including a :term:`bus error`.

        .. similar:: :term:`general protection fault`, :term:`read fault`, :term:`write fault`.

    segmented addressing

        In segmented addressing, :term:`addresses` are in
        two parts: a segment identifier and an offset into that
        segment.

        Each segment has a base address and a limit. If the offset is
        greater than the limit, the address is invalid (see
        :term:`segmentation violation`). Otherwise, the offset is
        added to the segment's base address, giving the unsegmented
        address. Segment identifiers may be implicit; for instance,
        they may be obtained from a *current segment* register.

        Segmentation may be layered on top of :term:`virtual memory`,
        in which case the unsegmented address is a :term:`virtual
        address`, or not, in which case it is a :term:`physical
        address`.

        Note that, in segmented architectures, you can have a
        two-dimensional :term:`address space`.

        Segments are a feature of some processor architectures and
        operating systems. This description does not cover all
        possible variations on segmentation.

        .. historical::

            Segment terminology may be used on unsegmented systems for
            historical reasons. For instance, Unix processes have
            *text segments*, even when running on an unsegmented
            system.

        .. opposite:: :term:`linear addressing`.

    segregated allocation cache

        .. mps:specific::

            A mechanism for adding a :term:`segregated free list` to a
            :term:`manual <manual memory management>` :term:`pool
            class`. See :ref:`topic-cache`.

    segregated fit

        One of the :term:`segregated free list` class of
        :term:`allocation mechanisms`. There is
        an array of :term:`free lists`, each holding
        :term:`free blocks` of a particular range of
        sizes. The :term:`allocator` identifies the appropriate free
        list and allocates from it (often using a :term:`sequential
        fit` mechanism such as :term:`first fit`). If this fails, a
        larger block is taken from another list and split.

        The details of the mechanism depend on the division of sizes
        between free lists. See :term:`exact segregated fit` and
        :term:`strict segregated fit`.

        This implements a :term:`good fit` :term:`allocation policy`.

        .. seealso:: :term:`segregated free list`, :term:`allocation mechanism`, :term:`free list`, :term:`exact segregated fit`, :term:`strict segregated fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    segregated free list
    segregated free-list

        A class of :term:`allocation mechanism` which divides the
        :term:`free list` into several subsets, according to the size
        of the :term:`free blocks`. A :term:`freed <free
        (1)>` or :term:`coalesced <coalesce>` block is placed on the
        appropriate list. An allocation request is serviced from the
        appropriate list.

        This class of mechanism implements a :term:`good fit` or
        :term:`best fit` policy.

        Variations within this class include :term:`simple segregated
        storage`, :term:`segregated fit`, and :term:`buddy systems`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

        .. mps:specific::

            :term:`Segregated allocation caches` are a general
            mechanism for adding a segregated free list to any
            manually managed pool. See :ref:`topic-cache`.

    semi-conservative garbage collection

        .. aka:: *mostly-precise garbage collection*, *mostly-exact garbage collection*.

        A variant of :term:`conservative garbage collection` which
        deals with :term:`exact references` as well
        as :term:`ambiguous references`.

        For example, references from the :term:`root set` might be
        ambiguous, but :term:`objects` on the :term:`heap`
        might be fully described and precisely :term:`scanned <scan>`.

        .. seealso:: :term:`mostly-copying garbage collection`.

        .. bibref:: :ref:`Bartlett (1988) <BARTLETT88>`.

    semi-space

        When an area of :term:`memory (2)` is divided into two parts
        for the purposes of :term:`copying garbage collection`, the
        parts are known as *semi-spaces*, or sometimes just *spaces*.

        Each semi-space is a contiguous area of memory. Semi-spaces
        are usually used for :term:`two space collection <two-space
        collector>`, but can be used for :term:`generational
        collection <generational garbage collection>`.

        The semi-space where :term:`objects` reside at the start of
        the collection is known as the :term:`fromspace`; the
        :term:`tospace` is where objects will reside, and where new
        objects will be :term:`allocated`, when the collection is
        complete.

        .. seealso:: :term:`two-space collector`.

    semi-space collector

        .. see:: :term:`two-space collector`.

    sequential fit

        A class of :term:`allocation mechanisms` that maintain the
        :term:`free list` as a single linear list of :term:`free
        blocks` (a :term:`free block chain`). Sequential fit
        mechanisms include :term:`first fit` and :term:`next fit`.

        To quote :ref:`Wilson et al. (1995) <WIL95>`:

            The list is often doubly-linked and/or circularly linked.
            Typically, sequential fit algorithms use Knuth's boundary
            tag technique, and a doubly-linked list to make
            :term:`coalescing <coalesce>` simple and fast. […] In
            considering sequential fits, it is probably most important
            to keep strategy and policy issues in mind. The classic
            linear-list implementations may not scale well to large
            :term:`heaps`, in terms of time costs; as the
            number of free blocks grows the time to search the list
            may become unacceptable. More efficient and scalable
            techniques are available, using totally or partially
            ordered trees, or :term:`segregated fits`.

        .. seealso:: :term:`bitmapped fit`, :term:`indexed fit`.

    sequential store buffer

        .. aka:: *SSB*.

        A sequential store buffer is a technique for dividing the cost
        of a :term:`write barrier` by remembering which
        :term:`objects` are modified and updating :term:`remembered
        sets` (and so on) at a later stage.

        This turns out to be extremely efficient on pipelined
        architectures with branch prediction.

    shared memory

        :term:`Memory locations` are *shared* if
        they are in the range of multiple :term:`address spaces`.

    simple object

        In the :term:`PostScript` language, *simple objects* are the
        :term:`unboxed` objects.

        Unlike a :term:`composite object`, a simple object contains
        all its data in the object itself.

        .. similar:: :term:`unboxed`.

        .. opposite:: :term:`composite object`.

    simple segregated storage

        A :term:`segregated free list` :term:`allocation mechanism`
        which divides :term:`memory (1)` into :term:`pages` or
        other areas and only allocates :term:`objects` of a
        single size, or small range of sizes, within each area. This
        makes allocation fast and avoids :term:`headers <in-band
        header>`, but may lead to high :term:`external fragmentation`,
        as unused parts of areas cannot be reused for other object
        sizes.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    size

        .. mps:specific::

            The term *size* in the documentation always refers to a
            size that is measured in :term:`bytes (1)`. The term
            *count* is used for the number of elements in an array.

    size class

        .. mps:specific::

            A :term:`segregated allocation cache` maintains a reserve
            of free :term:`blocks` in a set of :term:`sizes`: each
            such size is known as a *size class*. When creating a
            segregated allocation cache by calling
            :c:func:`mps_sac_create`, the :term:`client program`
            describes the desired set of size classes by passing an
            array of structures of type :c:type:`mps_sac_class_s`. See
            :ref:`topic-cache`.

    skip method
    
        .. mps:specific::

             A :term:`format method` that returns the address of the
             "next object" in a block of :term:`formatted objects`. See :c:type:`mps_fmt_skip_t`.

    smart pointer

        A smart pointer is an instance of a :term:`C++` class that
        encapsulates a :term:`pointer` and performs :term:`reference
        counting`.

        By overloading certain operators it is possible for the class
        to present the illusion of being a pointer, so that
        ``operator*``, ``operator->``, etc. can be used as normal.
        Reference counting allows the objects that are referred to
        using the smart pointer class to have their :term:`memory (1)`
        automatically :term:`reclaimed` when they are no longer
        :term:`referenced`. It is a common technique used when trying
        to solve :term:`memory management` problems in C++
        applications.

        However, reference counting is not always an appropriate
        memory management technique and smart pointers can be hard to
        implement properly in C++. A :term:`tracing garbage collector
        <tracing garbage collection>` might be worth considering.

        .. bibref:: :ref:`Edelson (1992a) <EDELSON92A>`.

    snap-out

        .. aka:: *transport snap-out*.

        In a :term:`copying collector <copying garbage collection>`,
        when there is a :term:`reference` to an :term:`object` that
        was :term:`condemned <condemned set>`, but has been
        :term:`transported`, snap-out is the adjustment of that
        reference to point to the preserved copy.

        Typically the first transport leaves a :term:`forwarding
        pointer` that enables the snap-out.

        .. figure:: ../diagrams/snap-out.svg
            :align: center
            :alt: Diagram: Snap-out.

            Snap-out.

        .. seealso:: :term:`broken heart`.

    snapshot at the beginning

        Snapshot-at-the-beginning algorithms for :term:`tracing
        <trace>`, :term:`incremental GC <incremental garbage
        collection>` note changes made by the :term:`mutator` to the
        :term:`graph` of :term:`objects` and update the
        :term:`collector (2)` state to make it trace relevant
        :term:`edges` that the mutator deletes.

        In order for the collector to miss a :term:`reachable`
        :term:`object`, the following two conditions need to hold at
        some point during tracing:

        1. The mutator stores a :term:`reference` to a :term:`white`
           object into a :term:`black` object.

        2. All paths from any :term:`gray` objects to that white
           object are destroyed.

        Snapshot-at-the-beginning algorithms ensure the second
        condition cannot occur, by causing the collector to process
        any reference that the mutator overwrites and that might be
        part of such a path.

        They are so called because they keep track of references that
        existed at the beginning of the :term:`collection cycle`. Note
        that this does not mean all modifications need to be seen by
        the collector, only those needed to complete tracing without
        missing a reachable object (see :ref:`Pirinen (1998)
        <PIRINEN98>` for details), nor does it mean that it won't
        trace some references created during the collection.

        .. historical::

            This distinction between incremental update and
            snapshot at the beginning was first introduced for
            write-barrier algorithms, but it applies to any type of
            tracing algorithm.

        .. opposite:: :term:`incremental update`.

        .. seealso:: :term:`tri-color marking`, :term:`weak tri-color invariant`, :term:`barrier (1)`.

        .. bibref:: :ref:`Wilson (1994) <WIL94>`, :ref:`Pirinen (1998) <PIRINEN98>`.

    soft reference

        In :term:`Java` terminology, *soft reference* is used to mean
        a :term:`reference` encapsulated in a :term:`reference object`
        of class ``SoftReference``.

        Soft references form one of three kinds of :term:`weak
        reference (1)` in Java. They are handy for building
        :term:`caches (3) <caching (3)>` that are automatically
        flushed when memory is low.

        .. seealso:: :term:`softly reachable`.

        .. link::

            `Class java.lang.ref.SoftReference <http://docs.oracle.com/javase/8/docs/api/java/lang/ref/SoftReference.html>`_, `Reference Objects and Garbage Collection <http://pawlan.com/monica/articles/refobjs/>`_.

    softly reachable

        In :term:`Java`, an object is *softly reachable* if it is not
        :term:`strongly reachable` and there is a path from the
        :term:`roots` to it that contains at least one
        :term:`soft reference` but no :term:`weak (2) <weak reference
        (2)>` or :term:`phantom references`.

        When the Java :term:`collector (1)` determines that an object
        is softly reachable, it has the option of clearing the soft
        references involved, which will usually allow the object to be
        :term:`recycled`. The idea is that they will only be cleared
        if the process is running short of :term:`memory (2)`. If it
        is done, all soft references involved are cleared, so that the
        object is no longer softly reachable, and any affected
        :term:`reference objects` which are registered with a queue
        are enqueued.

        .. seealso:: :term:`reachability <reachable>`, :term:`weakly reachable`, :term:`phantom reachable`.

        .. link::

            `Class java.lang.ref.SoftReference <http://docs.oracle.com/javase/8/docs/api/java/lang/ref/SoftReference.html>`_, `Reference Objects and Garbage Collection <http://pawlan.com/monica/articles/refobjs/>`_.

    space leak

        .. see:: :term:`memory leak`.

    spare commit limit

        .. mps:specific::

            The spare commit limit is a limit on the :term:`spare
            committed memory` that the MPS will obtain from the
            operating system. It can be retrieved by calling
            :c:func:`mps_arena_spare_commit_limit` and changed by
            passing the :c:macro:`MPS_KEY_ARENA_SPARE_COMMIT_LIMIT`
            :term:`keyword argument` to :c:func:`mps_arena_configure`.

    spare committed memory

        .. mps:specific::

            Memory which is not in use by any :term:`pool` and not
            otherwise in use for internal MPS data structures, but
            which remains :term:`committed <mapped>` (mapped to
            :term:`RAM` by the operating system). It is used by the
            :term:`arena` to (attempt to) avoid calling the operating
            system to repeatedly map and unmap areas of :term:`virtual
            memory` as the amount of memory in use goes up and down.
            It is subject to the :term:`spare commit limit`. The total
            spare committed memory can be retrieved by calling
            :c:func:`mps_arena_spare_committed`.

    spaghetti stack

        .. see:: :term:`cactus stack`.

    splat

        .. mps:specific::

            To overwrite a :term:`weak reference (1)` with a null
            pointer, when the MPS has determined that there are no
            remaining :term:`strong references` to the block referred
            to. See :ref:`topic-weak`.

    split

        To divide a :term:`free block` into two smaller free blocks in
        the process of satisfying an allocation request.

        Deciding when to split a block is an important aspect of an
        :term:`allocation policy`.

        .. opposite:: :term:`coalesce`.

        .. seealso:: :term:`coalesce`, :term:`allocation policy`, :term:`free block`.

    SRAM

        .. see:: :term:`static memory (1)`.

    SSB

        .. see:: :term:`sequential store buffer`.

    stack

        A stack is a LIFO (last in, first out) collection:
        :term:`objects` may be *pushed* onto the stack, and
        *popped* off it in reverse order of pushing.

        When people say "the stack", they usually mean the
        :term:`control stack` supported by the OS and/or the
        processor.

        .. relevance::

            :term:`Stack allocation` is an important technique.
            Control stacks are central to the performance of the
            system and often require special handling.

        .. historical::

            The terms "stack", "push", and "pop" are taken from the
            spring-loaded dish stack found in cafeterias and salad
            bars where removing the top plate causes the others to
            rise up, exposing the next one, and adding a plate causes
            the spring to compress, leaving only that plate
            accessible.

        So originally, the latest item was the "top", "down the stack"
        meant towards earlier items, and "up" towards later ones, but
        today many use "up" and "down" in the opposite sense.

        .. similar:: :term:`control stack`.

        .. seealso:: :term:`data stack`, :term:`cactus stack`.

    stack allocation

        *Stack allocation* means run-time :term:`allocation
        <allocate>` and :term:`deallocation <free (1)>` of
        :term:`memory (1)` in last-in/first-out order.

        Typically, stack allocation is performed on top of the main
        :term:`stack`, but one can have a separate :term:`data stack`
        for this purpose as well, as in Forth, or even multiple ones,
        as in the :term:`PostScript` language.

        Allocation and deallocation are typically fast, since they can
        be done simply by adding or subtracting the size of the
        :term:`block` from the stack pointer.

        Using only stack allocation, without heap allocation, is
        somewhat restrictive, as only objects whose size is known at
        compile-time can be returned from a procedure.

        Some programming languages (such as some versions of
        :term:`Lisp` and :term:`C`) provide program-controlled stack
        :term:`allocation <allocate>` and :term:`deallocation <free
        (1)>` of dynamic extent objects for efficiency, despite its
        being unsafe.

        .. similar:: :term:`automatic storage duration`.

        .. opposite:: :term:`heap allocation`, :term:`static allocation`.

        .. seealso:: :term:`region inference`, :term:`dynamic extent`.

    stack frame

        .. aka:: *stack record*.

        A stack frame or record is an :term:`activation record` that
        is stored on the :term:`stack`.

        In a register-based architecture, where the current activation
        record may be partially stored in registers, there may be
        hardware instructions that facilitate storing registers on the
        stack when another activation record is made current. Such
        instructions may prescribe a particular layout for activation
        records.

        .. relevance::

            Hardware support for saving and restoring registers, for
            stacks and for stack addressing may limit or otherwise
            prescribe the size and type of data that can be stored in
            a stack frame. Knowledge of the layout of each stack frame
            may assist a :term:`garbage collector` in finding
            :term:`roots`.

        .. similar:: :term:`activation record`.

        .. seealso:: :term:`stack`.

    stack record

        .. see:: :term:`stack frame`.

    static allocation

        *Static allocation* means :term:`allocation <allocate>` of
        :term:`memory (1)` before the program starts and retention
        until the end.

        The locations of :term:`objects` are basically
        decided at compile-time, although they might be
        :term:`relocated <relocation>` at load-time. This implies the
        sizes of the objects must be known then.

        Using only static allocation is restrictive, as sizes of data
        structures can't be dynamically varied, and procedures cannot
        be recursive. However, it is also fast and eliminates the
        possibility of running out of memory. For this reason, this
        scheme is sometimes used in real-time systems.

        .. similar:: :term:`static storage duration`.

        .. opposite:: :term:`stack allocation`, :term:`heap allocation`.

        .. seealso:: :term:`region inference`, :term:`static memory (2)`.

    static memory (1)

        .. aka:: *static RAM*, *SRAM*.

        Static :term:`memory (2)` or static RAM (SRAM) is a type of
        :term:`physical memory (2)` that does not need to be refreshed
        periodically to avoid losing state.

        Static memory is typically faster than :term:`dynamic memory`,
        or requires essentially no power to preserve its state, but
        rarely both. These benefits result in static RAM being used
        for :term:`cache (1)` memory, and also in portable, low-power
        applications (such as PDAs). It is, however, more expensive
        than dynamic RAM and requires more transistors, making dynamic
        RAM the choice for large amounts of memory (the :term:`main
        memory` of desktop machines, for example).

        .. opposite:: :term:`dynamic memory`.

    static memory (2)

        The :term:`memory (2)` where :term:`statically allocated
        <static allocation>` objects are stored is sometimes known as
        *static memory*. In the context of :term:`garbage collection`,
        the term is used mean memory used to store :term:`static
        objects`.

        .. seealso:: :term:`static storage duration`.

    static object

        A static :term:`object` is non-:term:`moving <moving garbage
        collector>`. That is, it is not :term:`relocated <relocation>`
        by a :term:`memory manager`; its :term:`address` does not
        change.

    static RAM

        .. see:: :term:`static memory (1)`.

    static storage duration

        In :term:`C` and :term:`C++`, the ``static`` keyword applied
        to a file scope variable or function means it is local to the
        file; the ``static`` keyword applied to a function or a block
        scope variable means it is :term:`allocated` and initialized
        once only.

        Objects declared locally in blocks with the ``static`` keyword
        are :term:`allocated` in :term:`static memory (2)`, and
        initialized once (usually by the compiler/linker) instead of
        each time the block is entered.

        Static variables within functions retain their value between
        function invocations, and therefore must form part of the
        :term:`root set` of any :term:`collector (1)`.

        .. opposite:: :term:`automatic storage duration`.

        .. seealso:: :term:`lifetime`.

    stepper function

        .. aka:: *visitor function*.

        .. mps:specific::

            A function that will be called on each element in a
            collection. For example, a stepper function of type
            :c:type:`mps_formatted_objects_stepper_t` can be passed to
            :c:func:`mps_arena_formatted_objects_walk` and it will be
            called on all :term:`formatted objects`
            in an :term:`arena`.

    sticky reference count

        .. see:: :term:`limited-field reference count`.

    stop-and-copy collection

        :term:`Copying garbage collection` that stops the
        :term:`mutator` while the collector runs.

        .. figure:: ../diagrams/two-space.svg
            :align: center
            :alt: Diagram: Two-space collector.

            Stop-and-copy in a :term:`two-space collector`.

        .. opposite:: :term:`incremental garbage collection`,  :term:`parallel garbage collection`.

    storage

        .. see:: :term:`memory (1)`.

    storage hierarchy

        .. aka:: *memory hierarchy*.

        A typical computer has several different *levels* of
        :term:`storage <memory (1)>`. Each level of storage has a
        different speed, cost, and size. The levels form a *storage
        hierarchy*, in which the topmost levels (those nearest the
        processor) are fastest, most expensive and smallest.

        Levels typically include processor :term:`registers`, possibly some levels of :term:`cache (1)`,
        :term:`main memory`, and possibly some levels of
        :term:`backing store`.

        Each level is commonly used as a :term:`cache (2)` for the
        next level. For instance, :term:`virtual memory` systems
        use main memory as a cache for backing store.

        .. figure:: ../diagrams/storage.svg
            :align: center
            :alt: Diagram: Storage hierarchy with (typical) relative cost, speed, and size.

            Storage hierarchy with (typical) relative cost, speed, and
            size.

    storage level

        One level in a :term:`storage hierarchy`, for instance a
        :term:`cache (1)`, :term:`main memory`, :term:`backing store`,
        and so on.

        .. seealso:: :term:`storage hierarchy`.

    storage management

        .. see:: :term:`memory management`.

    store (1)

        To transfer data from a processor's :term:`registers` to :term:`memory (2)`.

        Store can also be used in the more general sense of
        transferring data from a part of the :term:`memory hierarchy`
        that is fast to access to one that is slow to access.

        ``STORE`` (or an abbreviation) is also commonly used in many
        processor architectures as the mnemonic for the machine code
        instructions that store data into memory.

        .. opposite:: :term:`load`.

    store (2)

        .. see:: :term:`memory (1)`.

    stretchy vector

        A :term:`vector <vector data type>` that may grow or shrink to
        accommodate adding or removing elements. Named after the
        ``<stretchy-vector>`` abstract class in Dylan.

        .. relevance::

            In the presence of an :term:`asynchronous garbage
            collector`, the vector and its size may need to be updated
            atomically.

        .. link::

            `Dylan Reference Manual: Collections <http://opendylan.org/books/drm/Collection_Classes>`_.

        .. mps:specific::

            See :ref:`guide-stretchy-vector`.

    strict segregated fit

        A :term:`segregated fit` :term:`allocation mechanism` which
        has only one block size on each :term:`free list`. A requested
        block size is rounded up to the next provided size, and the
        first block on that list is returned. The sizes must be chosen
        so that any block of a larger size can be :term:`split` into a
        number of smaller sized blocks. :term:`Buddy systems <buddy
        system>` are a special case of strict segregated fit
        allocators.

        .. seealso:: :term:`buddy system`, :term:`segregated fit`, :term:`segregated free list`, :term:`allocation mechanism`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    strong reference

        In a :term:`tracing garbage collector <tracing garbage
        collection>`, a strong reference is a :term:`reference` that
        keeps the :term:`object` it refers to :term:`alive <live>`.

        A strong reference is the usual sort of reference: the term is
        usually used to draw a contrast with :term:`weak reference
        (1)`.

        .. opposite:: :term:`weak reference (1)`.

        .. seealso:: :term:`strong root`.

    strong root

        A strong root is a :term:`root` such that all
        :term:`references` in it are :term:`strong references`.

        A strong root is the usual sort of root: the term is usually
        used to draw a contrast with :term:`weak root`.

        .. opposite:: :term:`weak root`.

        .. mps:specific::

            Strong roots have :term:`rank` :c:func:`mps_rank_ambig` or
            :c:func:`mps_rank_exact`.

    strong tri-color invariant
    strong tri-colour invariant
    strong tricolor invariant
    strong tricolour invariant

        The strong :term:`tri-color invariant` is the property of a
        :term:`reference` :term:`graph` that there is no :term:`edge`
        from a :term:`black` :term:`node` to a :term:`white` node.

        By preserving this property throughout :term:`tri-color
        marking`, a :term:`tracing <trace>` algorithm can ensure that
        the :term:`collector (2)` will not miss reachable objects,
        even if the :term:`mutator` manipulates the graph during the
        collection. This invariant can also be used to ensure that a
        :term:`copying garbage collector <copying garbage collection>`
        doesn't confuse the mutator. Mutator actions might need to
        change the :term:`color` of the nodes affected in order to
        preserve the invariant.

        Algorithms using this invariant are :term:`incremental update`
        algorithms.

        .. similar:: :term:`tri-color invariant`.

        .. seealso:: :term:`barrier (1)`, :term:`weak tri-color invariant`.

        .. bibref:: :ref:`Johnstone (1997) <JOHNSTONE97>`, :ref:`Pirinen (1998) <PIRINEN98>`.

    strongly reachable

        In :term:`Java`, an object is *strongly reachable*, if there
        is a path from the :term:`roots` to it that contains
        only :term:`strong references`, that is,
        contains no :term:`reference objects`.

        .. seealso:: :term:`reachability <reachable>`, :term:`softly reachable`, :term:`weakly reachable`, :term:`phantom reachable`.

        .. link::

            `Reference Objects and Garbage Collection <http://pawlan.com/monica/articles/refobjs/>`_.

    suballocator

        A *suballocator* is an :term:`allocator` functioning on top of
        another allocator.

        Suballocators work by :term:`allocating <allocate>` large
        :term:`blocks` and :term:`splitting <split>` them for
        use, or by :term:`recycling <recycle>` blocks locally.

        Application programmers sometimes write their own
        suballocators when faced with an inefficient or inadequate
        :term:`memory manager`. Suballocators can take advantage of
        special knowledge of program behavior, but are less efficient
        in general than fixing the underlying allocator, mainly
        because :term:`memory management` is a *global* issue for an
        application, and a global strategy can make a big difference.
        For example, different suballocators can interact
        catastrophically with each other and with the :term:`virtual
        memory` system, causing the application's memory
        requirements to grow unnecessarily due to
        :term:`fragmentation`.

    subgraph

        A subgraph S of a :term:`graph` G is a graph such that all the
        :term:`nodes` in S are also in G and all the
        :term:`edges` in S are also in G; that is, it is a part
        of a graph.

    superpage

        .. see:: :term:`huge page`.

    sure reference

        .. see:: :term:`exact reference`.

    swap space

        :term:`Backing store` used by a :term:`swapping` system.

        .. seealso:: :term:`swapping`, :term:`backing store`.

    swapped in

        A process or :term:`page` is *swapped in* if it is available
        in :term:`physical memory (1)`. This usually applies to the
        entire program image.

        .. similar:: :term:`paged in`.

        .. opposite:: :term:`swapped out`.

        .. seealso:: :term:`swapping`.

    swapped out

        A process or :term:`page` is *swapped out* if it is not
        available in :term:`physical memory (1)`. This usually applies
        to the entire program image.

        .. similar:: :term:`paged out`.

        .. opposite:: :term:`swapped in`.

        .. seealso:: :term:`swapping`.

    swapping

        Historically, swapping was the technique of moving entire
        program images to disk (or drum) and back into :term:`physical
        memory (1)`, an early form of :term:`virtual memory`.
        Nowadays, it is used as a synonym for :term:`paging`.

        .. similar:: :term:`paging`.

        .. seealso:: :term:`swapped in`, :term:`swapped out`.

    sweeping

        Sweeping is the second phase ("the sweep phase") of the
        :term:`mark-sweep` algorithm. It performs a sequential
        (address-order) pass over memory to :term:`recycle` unmarked
        blocks.

        Sweeping typically gathers all unmarked blocks into one or
        more :term:`free lists`.

        .. seealso:: :term:`marking`.

    synchronous garbage collector

        A :term:`collector (2)` is asynchronous with respect to the
        :term:`mutator` if it runs at predictable times, for example
        only when a collection function is called.

        This means that mutator need not ensure that :term:`formatted
        objects` are always :term:`scannable <scan>`, as long as it
        makes them scannable before the collector runs.

        .. opposite:: :term:`asynchronous garbage collector`.

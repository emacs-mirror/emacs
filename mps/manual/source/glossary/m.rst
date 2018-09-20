.. _glossary-m:

=============================
Memory Management Glossary: M
=============================

.. include:: alphabet.txt

.. glossary::

    machine word

        .. see:: :term:`word`.

    main memory

        .. aka:: *memory*, *primary storage*.

        The *main memory* (or *primary storage*) of a computer is
        :term:`memory (1)` that is wired directly to the processor,
        consisting of :term:`RAM` and possibly :term:`ROM`.

        These terms are used in contrast to mass storage devices and
        :term:`cache memory` (although we may note that when a program
        accesses main memory, it is often actually interacting with a
        cache).

        Main memory is the middle level of the :term:`memory
        hierarchy`: it is slower and cheaper than :term:`caches (1)`,
        but faster and more expensive than :term:`backing store`.

        It is common to refer only to the main memory of a computer;
        for example, "This server has 128 GB of memory" and "macOS
        High Sierra requires at least 2 GB of memory".

        .. historical::

            Main memory used to be called :term:`core`, and is now
            likewise often called :term:`RAM`.

        .. similar:: :term:`core`, :term:`physical memory (1)`, :term:`RAM`.

    malloc

        A function in the standard :term:`C` library that performs
        :term:`dynamic allocation` of :term:`memory (2)`.

        Many people use "malloc" as a verb to mean "allocate
        dynamically".

        .. similar:: :term:`allocate`.

        .. opposite:: :term:`free (2)`.

    manual memory management

        In some systems or languages, it is up to the application
        program to manage all the bookkeeping details of
        :term:`allocating <allocate>` :term:`memory (2)` from the
        :term:`heap` and :term:`freeing <free (1)>` it when no longer
        required; this is known as manual :term:`memory management`.

        Manual memory management may be appropriate for small
        programs, but it does not scale well in general, nor does it
        encourage modular or object-oriented programming.

        To quote :ref:`Joyner (1996) <JOYNER96>`:

            In C++ the programmer must manually manage storage due to
            the lack of :term:`garbage collection`. This is the most
            difficult bookkeeping task C++ programmers face, that
            leads to two opposite problems: firstly, an object can be
            :term:`deallocated <free (1)>` prematurely, while valid
            :term:`references` still exist (:term:`dangling
            pointers`); secondly, :term:`dead` objects might not be
            deallocated, leading to memory filling up with dead
            objects (:term:`memory leaks`). Attempts to correct either
            problem can lead to overcompensation and the opposite
            problem occurring. A correct system is a fine balance.

        .. historical::

            Manual memory management was common in early languages,
            but :term:`garbage collection` has been around since the
            late 1950s, in languages like :term:`Lisp`. Most modern
            languages use :term:`automatic memory management`, and
            some older languages have :term:`conservative garbage
            collection` extensions.

        .. opposite:: :term:`automatic memory management`.

        .. mps:specific::

            Manual memory management can be used with :term:`pools`
            such as :ref:`pool-mvff` via the functions
            :c:func:`mps_alloc` and :c:func:`mps_free`.

    mapped

        .. aka:: *committed*.

        A range of :term:`virtual addresses` is said
        to be *mapped* (*committed* on Windows) if there is
        :term:`physical memory (2)` associated with the range.

        Note that, in some circumstances, the :term:`virtual memory`
        system could actually :term:`overcommit` mapped memory.

        .. opposite:: :term:`unmapped`.

        .. seealso:: :term:`mapping`, :term:`memory mapping`, :term:`mmap`.

        .. mps:specific::

            The term *committed* is used. The function
            :c:func:`mps_arena_committed` returns the total committed
            memory for an :term:`arena`.

    mapping

        A *mapping* is a correspondence between a range of
        :term:`virtual addresses` and some
        :term:`memory (1)` (or a :term:`memory-mapped <memory
        mapping>` object). The physical location of the memory will be
        managed by the :term:`virtual memory` system.

        Each :term:`page` in a mapping could be :term:`paged out` or
        :term:`paged in`, and the locations it occupies in :term:`main
        memory` and/or :term:`swap space` might change over time.

        The :term:`virtual address space` can contain of a complex set
        of mappings. Typically, parts of the address space are
        :term:`mapped` (have a mapping assigned), others are
        :term:`reserved` but unmapped, and most of it is entirely
        :term:`unmapped`.

        .. figure:: ../diagrams/mapped.svg
            :align: center
            :alt: Diagram: Virtual memory with different kinds of mappings.

            Virtual memory with different kinds of mappings.

        .. seealso:: :term:`backing store`.

    mark-compact

        Mark-compact collection is a kind of :term:`tracing garbage
        collection` that operates by :term:`marking` :term:`reachable`
        :term:`objects`, then :term:`compacting <compaction>`
        the marked objects (which must include all the :term:`live`
        objects).

        The mark phase follows :term:`reference` chains to mark all
        reachable objects; the compaction phase typically performs a
        number of sequential passes over :term:`memory (2)` to move
        objects and update references. As a result of compaction, all
        the marked objects are moved into a single contiguous
        :term:`block` of memory (or a small number of such blocks);
        the memory left unused after compaction is :term:`recycled`.

        Mark-compact collection can be regarded as a variation of
        :term:`mark-sweep collection <mark-sweep>`, with extra effort
        spent to eliminate the resulting :term:`fragmentation`.
        Compaction also allows the use of more efficient
        :term:`allocation mechanisms`, by
        making large free blocks available.

        .. bibref:: :ref:`Edwards <EDWARDS>`.

    mark-sweep
    mark-and-sweep

        Mark-sweep collection is a kind of :term:`tracing garbage
        collection` that operates by :term:`marking` :term:`reachable`
        :term:`objects`, then :term:`sweeping` over
        :term:`memory (2)` and :term:`recycling <recycle>` objects
        that are unmarked (which must be :term:`unreachable`), putting
        them on a :term:`free list`.

        The mark phase follows :term:`reference` chains to mark all
        reachable objects; the sweep phase performs a sequential
        (:term:`address`-order) pass over memory to recycle all
        unmarked objects. A mark-sweep :term:`collector (1)` doesn't
        move objects.

        .. historical::

            This was the first garbage collection algorithm, devised
            by John McCarthy for :term:`Lisp`.

        .. seealso:: :term:`mark-compact`.

        .. bibref:: :ref:`McCarthy (1960) <MCCARTHY60>`.

    marking

        Marking is the first phase ("the mark phase") of the
        :term:`mark-sweep` algorithm or :term:`mark-compact`
        algorithm. It follows all :term:`references` from
        a set of :term:`roots` to mark all the
        :term:`reachable` :term:`objects`.

        Marking follows :term:`reference` chains and makes some sort
        of mark for each object it reaches.

        Marking is often achieved by setting a bit in the object,
        though any conservative representation of a predicate on the
        :term:`memory location` of the object can be used. In
        particular, storing the mark bit within the object can lead to
        poor :term:`locality of reference` and to poor cache
        performance, because the marking phases ends up setting the
        :term:`dirty bit` on all :term:`pages` in the :term:`working
        set`. An alternative is to store the mark bits separately:
        see :term:`bitmap marking`.

        .. seealso:: :term:`compact <compaction>`, :term:`sweep <sweeping>`.

    MB

        .. see:: :term:`megabyte`.

    megabyte

        .. aka:: *MB*.

        A megabyte is 1024 :term:`kilobytes`, or 1048576
        :term:`byte (1)`.

        See :term:`byte (1)` for general information on this and
        related quantities.

    memoization

        .. see:: :term:`caching (3)`.

    memory (1)

        .. aka:: *storage*, *store*.

        *memory* or *storage* (or *store*) is where data and
        instructions are stored. For example, :term:`caches (1)
        <cache (1)>`, :term:`main memory`, floppy and hard disks are
        all storage devices.

        These terms are also used for the capacity of a system to
        store data, and may be applied to the sum total of all the
        storage devices attached to a computer.

        .. historical::

            "Store" is old-fashioned, but survives in expressions such
            as ":term:`backing store`".

    memory (2)

        *Memory* refers to :term:`memory (1)` that can be accessed by
        the processor directly (using memory addressing instructions).

        This could be :term:`real memory (1)` or :term:`virtual memory`.

    memory (3)

        .. see:: :term:`main memory`.

    memory (4)

        A :term:`memory location`; for example, "My digital watch has
        256 memories."

    memory bandwidth

        Memory bandwidth (by analogy with the term *bandwidth* from
        communication theory) is a measure of how quickly information
        (expressed in terms of bits) can be transferred between two
        places in a computer system.

        Often the term is applied to a measure of how quickly the
        processor can obtain information from the :term:`main memory`
        (for example, "My new bus design has a bandwidth of over 400
        Megabytes per second").

    memory cache

        .. see:: :term:`cache (1)`.

    memory hierarchy

        .. see:: :term:`storage hierarchy`.

    memory leak

        .. aka:: *leak*, *space leak*, *space-leak*.

        A memory leak is where :term:`allocated` :term:`memory (2)` is
        not :term:`freed (1)` although it is never used again.

        In :term:`manual memory management`, this usually occurs
        because :term:`objects` become :term:`unreachable` without
        being :term:`freed (1)`.

        In :term:`tracing garbage collection`, this happens when
        objects are :term:`reachable` but not :term:`live`.

        In :term:`reference counting`, this happens when objects are
        :term:`referenced` but not :term:`live`. (Such objects may or
        may not be :term:`reachable`.)

        Repeated memory leaks cause the memory usage of a process to
        grow without bound.

    memory location

        .. aka:: *location*.

        Each separately-:term:`addressable <address>` unit of
        :term:`memory (2)` in which data can be stored is called a
        *memory location*. Usually, these hold a :term:`byte (2)`, but
        the term can refer to :term:`words`.

    memory management

        .. aka:: *storage management*.

        Memory management is the art and the process of coordinating
        and controlling the use of :term:`memory (1)` in a computer
        system.

        Memory management can be divided into three areas:

        1. Memory management hardware (:term:`MMUs`,
           :term:`RAM`, etc.);

        2. Operating system memory management (:term:`virtual memory`,
           :term:`protection`);

        3. Application memory management (:term:`allocation
           <allocate>`, :term:`deallocation <free (1)>`, :term:`garbage
           collection`).

        Memory management hardware consists of the electronic devices
        and associated circuitry that store the state of a computer.
        These devices include RAM, MMUs (memory management units),
        :term:`cache (1)`, disks, and processor
        :term:`registers`. The design of memory hardware is
        critical to the performance of modern computer systems. In
        fact, :term:`memory bandwidth` is perhaps the main limiting
        factor on system performance.

        Operating system memory management is concerned with using the
        memory management hardware to manage the resources of the
        :term:`storage hierarchy` and allocating them to the various
        activities running on a computer. The most significant part of
        this on many systems is :term:`virtual memory`, which
        creates the illusion that every process has more memory than
        is actually available. OS memory management is also concerned
        with :term:`memory protection` and security, which help to
        maintain the integrity of the operating system against
        accidental damage or deliberate attack. It also protects user
        programs from errors in other programs.

        Application memory management involves obtaining :term:`memory
        (2)` from the operating system, and managing its use by an
        application program. Application programs have dynamically
        changing storage requirements. The application :term:`memory
        manager` must cope with this while minimizing the total CPU
        overhead, interactive pause times, and the total memory used.

        While the operating system may create the illusion of nearly
        infinite memory, it is a complex task to manage application
        memory so that the application can run most efficiently.
        Ideally, these problems should be solved by tried and tested
        tools, tuned to a specific application.

        The Memory Management Reference is mostly concerned with
        application memory management.

        .. seealso:: :term:`automatic memory management`, :term:`manual memory management`.

    Memory Management Unit

        .. see:: :term:`MMU`.

    memory manager

        The memory manager is that part of the system that manages
        :term:`memory (2)`, servicing :term:`allocation <allocate>`
        requests, and :term:`recycling <recycle>` memory, either
        :term:`manually <manual memory management>` or
        :term:`automatically <automatic memory management>`.

        The memory manager can have a significant effect on the
        efficiency of the program; it is not unusual for a program to
        spend 20% of its time managing memory.

        .. similar:: :term:`allocator`, :term:`collector (1)`.

        .. seealso:: :term:`memory management`.

    memory mapping

        .. aka:: *file mapping*.

        *Memory mapping* is the technique of making a part of the
        :term:`address space` appear to contain an "object", such as a
        file or device, so that ordinary :term:`memory (2)` accesses
        act on that object.

        The object is said to be *mapped* to that range of addresses.
        (The term "object" does not mean a program :term:`object`. It
        comes from Unix terminology on the :term:`mmap` man page.)

        .. figure:: ../diagrams/mapping.svg
            :align: center
            :alt: Diagram: An address space with a range mapped to part of an object.

            An address space with a range mapped to part of an object.

        Memory mapping uses the same mechanism as :term:`virtual
        memory` to "trap" accesses to parts of the :term:`address
        space`, so that data from the file or device can be
        :term:`paged in` (and other parts :term:`paged out`) before
        the access is completed.

        .. historical::

            File mapping is available on most modern Unix and Windows
            systems. However, it has a much longer history. In
            Multics, it was the primary way of accessing files.

        .. seealso:: :term:`mapped`.

    memory protection

        .. see:: :term:`protection`.

    message

        .. mps:specific::

            A data structure which the MPS uses to communicate with
            the :term:`client program`. See :ref:`topic-message`.

    message queue

        .. mps:specific::

            A queue of :term:`messages` posted by an
            :term:`arena`. It can be queried by calling
            :c:func:`mps_message_poll`,
            :c:func:`mps_message_queue_type`, or
            :c:func:`mps_message_get`. See :ref:`topic-message`.

    message type

        .. mps:specific::

            A value of type :c:type:`mps_message_type_t` describing
            the type of a :term:`message`. There are three message
            types: :c:func:`mps_message_type_finalization`,
            :c:func:`mps_message_type_gc`, and
            :c:func:`mps_message_type_gc_start`. See
            :ref:`topic-message`.

    misaligned

        .. see:: :term:`unaligned`.

    miss

        A miss is a lookup failure in any form of :term:`cache (3)
        <caching (3)>`, most commonly at some level of a
        :term:`storage hierarchy`, such as a :term:`cache (1)` or
        :term:`virtual memory` system.

        The cost of a miss in a virtual memory system is considerable:
        it may be five orders of magnitude more costly than a hit. In
        some systems, such as multi-process operating systems, other
        work may be done while a miss is serviced.

        .. opposite:: :term:`hit`.

        .. seealso:: :term:`miss rate`.

    miss rate

        At any level of a :term:`storage hierarchy`, the miss rate is
        the proportion of accesses which :term:`miss`.

        Because misses are very costly, each level is designed to
        minimize the miss rate. For instance, in :term:`caches (1)`,
        miss rates of about 0.01 may be acceptable, whereas in
        :term:`virtual memory` systems, acceptable miss rates are much
        lower (say 0.00005). If a system has a miss rate which is too
        high, it will spend most of its time servicing the misses, and
        is said to :term:`thrash`.

        Miss rates may also be given as a number of misses per unit
        time, or per instruction.

        .. opposite:: :term:`hit rate`.

    mmap

        ``mmap`` is a system call provided on many Unix systems to
        create a :term:`mapping` for a range of :term:`virtual
        addresses`.

    MMU

        .. aka:: *Memory Management Unit*.

        The MMU (Memory Management Unit) is a hardware device
        responsible for handling :term:`memory (2)` accesses requested
        by the main processor.

        This typically involves translation of :term:`virtual
        addresses` to :term:`physical addresses`, :term:`cache (1)`
        control, bus arbitration, :term:`memory protection`, and the
        generation of various exceptions. Not all processors have an
        MMU.

        .. seealso:: :term:`page fault`, :term:`segmentation violation`, :term:`virtual memory`.

    mostly-copying garbage collection

        A type of :term:`semi-conservative <semi-conservative garbage
        collection>` :term:`tracing garbage collection` which permits
        :term:`objects` to :term:`move <moving garbage collector>` if
        no :term:`ambiguous references` point to them.

        The techniques used are a hybrid of :term:`copying garbage
        collection` and :term:`mark-sweep`.

        Mostly-copying garbage collectors share many of the benefits
        of copying collectors, including :term:`compaction`. Since
        they support ambiguous references they are additionally
        suitable for use with uncooperative compilers, and may be an
        efficient choice for multi-threaded systems.

        .. bibref:: :ref:`Bartlett (1989) <BARTLETT89>`, :ref:`Yip (1991) <YIP91>`.

        .. mps:specific::

            The :ref:`pool-amc` pool class implements mostly-copying
            garbage collection.

    mostly-exact garbage collection

        .. see:: :term:`semi-conservative garbage collection`.

    mostly-precise garbage collection

        .. see:: :term:`semi-conservative garbage collection`.

    moving garbage collector
    moving memory manager

        A memory manager (often a :term:`garbage collector`) is said
        to be *moving* if :term:`allocated` :term:`objects` can move
        during their lifetimes.

        .. relevance::

            In the garbage collecting world this will apply to
            :term:`copying <copying garbage collection>` collectors
            and to :term:`mark-compact` collectors. It may also refer
            to :term:`replicating <replicating garbage collector>`
            collectors.

        .. similar:: :term:`copying garbage collection`.

        .. opposite:: :term:`non-moving garbage collector`.

    mutable

        Any :term:`object` which may be changed by a program is
        *mutable*.

        .. opposite:: :term:`immutable`.

    mutator

        .. aka:: *client program*.

        In a :term:`garbage-collected <garbage collection>` system,
        the part that executes the user code, which :term:`allocates` :term:`objects` and modifies, or
        *mutates*, them.

        For purposes of describing :term:`incremental garbage
        collection`, the system is divided into the *mutator* and the
        :term:`collector (2)`. These can be separate threads of
        computation, or interleaved within the same thread.

        The user code issues allocation requests, but the allocator
        code is usually considered part of the collector. Indeed, one
        of the major ways of scheduling the other work of the
        collector is to perform a little of it at every allocation.

        While the mutator mutates, it implicitly :term:`frees <free
        (1)>` :term:`memory (1)` by overwriting :term:`references`.

        .. historical::

            This term is due to :ref:`Dijkstra et al. (1976) <DLMSS76>`.

        .. opposite:: :term:`collector (2)`.

        .. mps:specific::

            The MPS documentation uses the term :term:`client program`
            to refer to the mutator.

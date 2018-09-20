.. _glossary-l:

=============================
Memory Management Glossary: L
=============================

.. include:: alphabet.txt

.. glossary::

    large object area

        An :term:`allocation mechanism` designed to optimize the
        management of large :term:`objects` by separating
        them from small ones.

        Large objects, typically objects one or more orders of
        magnitude larger than the :term:`virtual memory`
        :term:`page` of a platform, can be costly to :term:`allocate`,
        initialize, and :term:`recycle`. By segregating those objects
        into a separate area, they can be managed using specific
        mechanisms that would be inefficient for smaller objects but
        which can reduce the cost of manipulating large ones.

        Some example mechanisms:

        1. In a :term:`copying collector <copying garbage collection>`
           large objects can be managed separately using a
           :term:`mark-and-sweep collector <mark-sweep>` to avoid
           copying costs. See :ref:`Ungar (1988) <UNGAR88>`.

        2. By aligning large objects on page boundaries, they can be
           :term:`compacted <compaction>` or copied by adjusting their
           :term:`mapping` in :term:`virtual memory`. See
           :ref:`Withington (1991) <WITHINGTON91>`.

        3. Large objects may be split into a header and a body, where
           the header is fixed size and the bulk of the object is in
           the body. See :ref:`Ungar (1988) <UNGAR88>`.

        4. By using a page-based :term:`read barrier`, large objects
           can be initialized incrementally. For example, each page of
           the large object is initialized to zero when it is first
           read, rather than all at once at creation time.

        5. In a copying collector, large objects can be copied
           incrementally using a similar technique (the new copy is
           initialized from the old copy). See :ref:`Baker (1978)
           <BAKER78>`.

        6. Large objects are often :term:`leaf objects`,
           so do not need to be :term:`scanned <scan>`, or are known
           to have a fixed :term:`format` with only a few
           :term:`references` so they can be scanned more
           efficiently by a specialized scanner.

        7. Large objects often have longer than average
           :term:`lifetimes`, so are not allocated in a
           :term:`nursery space` of a :term:`generational garbage
           collector <generational garbage collection>`.

    large page

        .. see:: :term:`huge page`.

    leaf object

        .. aka:: *atomic object*.

        A leaf object is an :term:`object` that does not
        :term:`reference` any other objects.

        In a typed language, the compiler can often determine at
        compile time that certain types can be represented as leaf
        objects. Usually these types are either a :term:`scalar data
        type` or a :term:`vector data type` of scalars with bounded
        magnitude.

        .. relevance::

            If leaf objects can be identified, a :term:`garbage
            collector` can make certain optimizations: leaf objects do
            not have to be :term:`scanned <scan>` for references nor
            are :term:`barriers (1)` needed to detect
            and maintain references in the object.

        .. mps:specific::

            The :ref:`pool-amcz` and :ref:`pool-lo` pool classes are
            designed for the storage of leaf objects.

    leak

        .. see:: :term:`memory leak`.

    life

        .. see:: :term:`lifetime`.

    lifetime

        .. aka:: *extent*, *life*.

        The lifetime or extent of an :term:`object` is the time for
        which the object is :term:`live`.

        .. seealso:: :term:`dynamic extent`, :term:`indefinite extent`.

    LIFO-ordered first fit

        The :term:`allocation policy` that always uses the
        most-recently :term:`freed (1)` suitable :term:`free block`.
        Commonly implemented by pushing freed blocks on the front of a
        :term:`free block chain`, and then using :term:`first fit`
        allocation on this chain. :term:`free (1)` can be very quick,
        depending on the :term:`coalescing <coalesce>` policy.

        This policy may suffer from severe :term:`fragmentation` in
        the presence of short-lived large objects of a single size. As
        smaller objects are allocated, the free block chain fills up
        with fragments a little smaller than the large object size.

        .. seealso:: :term:`address-ordered first fit`, :term:`FIFO-ordered first fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    limited-field reference count

        .. aka:: *sticky reference count*.

        A :term:`reference counting` technique whereby the field used
        to store the number of :term:`references` to an
        :term:`object` has a limited size. In particular, the field is
        not large enough to represent the maximum possible number of
        references to an object.

        Using the observation that most objects are not referenced a
        great number of times, some systems that use reference counts
        only store the count accurately up to a certain maximum value.
        If an object has more references than the maximum then the
        count "sticks" at the maximum and is never decremented. Such
        objects are expected to be rare, but their :term:`memory (1)`
        can never be :term:`reclaimed` using reference counting. A
        separate (infrequently run) :term:`tracing garbage collector
        <tracing garbage collection>` is often employed to reclaim
        this storage.

        A degenerate form of limited-field reference counting is
        :term:`one-bit reference counting <one-bit reference count>`
        where an object is considered to be referenced either exactly
        once or many times.

    linear addressing

        In linear addressing, :term:`addresses` form a
        single, continuous :term:`address space`. This term is used
        mostly in opposition to :term:`segmented addressing`.

        .. opposite:: :term:`segmented addressing`.

    live

        .. aka:: *active*, *alive*.

        :term:`Memory (2)` or an :term:`object` is live if the program
        will read from it in future. The term is often used more
        broadly to mean :term:`reachable`.

        It is not possible, in general, for :term:`garbage collectors`
        to determine exactly which :term:`objects` are still live.
        Instead, they use some approximation to detect objects that
        are provably :term:`dead`, such as those that are not
        :term:`reachable`.

        .. similar:: :term:`reachable`.

        .. opposite:: :term:`dead`.

        .. seealso:: :term:`undead`.

    load

        To transfer data from :term:`memory (2)` to a processor's
        :term:`registers`.

        Load can also be used in the more general sense of moving data
        from a part of the :term:`memory hierarchy` that is slow to
        access to one that is fast to access (For example, "it takes
        about 3 ms for the :term:`virtual memory` system to load a
        :term:`page` from disk on this system"). When used in this
        sense, the qualified term :term:`cache (2)` load is common.

        ``LOAD`` (or an abbreviation) is also commonly used in many
        processor architectures as the mnemonic name for the machine
        code instructions that are used primarily to make data
        accessible to the CPU (by loading the data into registers
        usually). In RISC architectures it is common for the load
        instructions to be the only means of making data accessible to
        the CPU; in CISC architectures it is common for a wide variety
        of instructions to implicitly or explicitly load data from
        memory.

        .. opposite:: :term:`store (1)`.

    locality of reference

        Locality of reference is the extent to which successive
        accesses of nearby :term:`memory (1)` :term:`locations <memory
        location>` are nearby in time; for example, a program that
        reads all the elements of a contiguous array in turn or that
        repeatedly uses the same memory variable has good locality of
        reference.

        Good locality of reference interacts well with :term:`virtual
        memory` and :term:`memory caches <cache (1)>`, as it reduces
        the :term:`working set` and improves the :term:`hit rate`.

        There are a number of specialized senses of locality of
        reference in certain fields such as distributed systems; these
        are not covered in depth here.

        .. relevance::

            A :term:`mutator` may exhibit predictable properties such
            as accessing in turn :term:`objects` which were
            :term:`allocated` in turn, or accessing in turn objects
            which have :term:`references` to each other. An
            intelligent :term:`allocator` or :term:`copying garbage
            collector <copying garbage collection>` can use this
            observation to improve locality of reference.

        .. bibref:: :ref:`Grunwald et al. (1993) <GZH93>`, :ref:`Wilson et al. (1992) <WLM92>`.

    location

        .. see:: :term:`memory location`.

    location dependency

        .. mps:specific::

            A *location dependency* records the fact that the
            :term:`client program` depends on the bit patterns of some
            :term:`references` (and not merely on the
            identity of the :term:`block` to which the reference
            refers), and provides a function
            (:c:func:`mps_ld_isstale`) to find out whether a
            reference might have been changed because a block has
            been :term:`moved <moving garbage collector>`. See
            :ref:`topic-location`.

    lock free

        A multi-threaded program is *lock free* if all schedules for
        the threads make progress: in particular, no schedule leads to
        deadlock. This is most easily implemented by avoiding taking
        locks.

    logical address

        .. see:: :term:`virtual address`.

    longword

        .. see:: :term:`doubleword`.


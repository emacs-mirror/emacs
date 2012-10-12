.. _glossary-d:

===========
Glossary: D
===========

.. glossary::

    dangling pointer

        A dangling :term:`pointer` is a surviving :term:`reference` to
        an :term:`object` that no longer exists at that
        :term:`address`

        In :term:`manual memory management`, dangling pointers
        typically arise from one of:

        1. A :term:`premature free`, where an object is :term:`freed
           <free (1)>`, but a reference is retained;

        2. Retaining a reference to a :term:`stack-allocated <stack
           allocation>` object, after the relevant :term:`stack frame`
           has been popped.

        Dangling pointers can occur under :term:`automatic memory
        management`, because of a :term:`garbage collection` bug, such
        as premature collection or :term:`moving <moving garbage
        collector>` without updating all :term:`references
        <reference>`, but this is much rarer because :term:`GC` code
        is usually a single common core of reused code. FIXME

    data object

        .. mps:: ??

    data stack

        A :term:`stack` used to manage the storage of
        :term:`stack-allocated <stack allocation>` :term:`objects
        <object>`, other than :term:`activation records <activation
        record>`, often under program control.

        Because of the limitations that may be imposed on the
        :term:`control stack`, or to support stack-like semantics for
        certain data structures, some language implementations manage
        additional data stacks in software for storing objects that
        have :term:`dynamic extent` but that do not fit within the
        constraints of the control stack.

        .. seealso:: :term:`control stack`.

    dead

        An :term:`object` is dead if it is not :term:`live`; that is,
        when the :term:`mutator` cannot reach any state in which it
        accesses the object.

        It is not possible, in general, for :term:`garbage collectors
        <garbage collector>` to determine exactly which :term:`objects
        <object>` are dead and which are live. Instead, they use some
        approximation to detect objects that are provably dead, such
        as those that are :term:`unreachable`.

        .. opposite:: :term:`live`.

        .. seealso:: :term:`garbage`, :term:`undead`, :term:`free (3)`.

    deallocate

        .. see:: :term:`free (1)`.

    deferred coalescing

        Deferred coalescing is a policy which :term:`coalesces
        <coalesce>` :term:`free blocks <free block>` some time after
        the blocks are freed, as opposed to coalescing free blocks
        immediately as they are freed.

        Adjacent free blocks can be coalesced to form larger free
        blocks; deferred coalescing is a catch-all for policies which
        perform this coalescing sometime after the blocks were freed.

        Given this rather flexible definition there are a number of
        choices for when to coalesce: as the :term:`free list` is
        traversed during allocation, when the allocation cannot be
        satisfied from the free list, periodically, and so on. In
        addition there are choices to be made regarding how much
        coalescing to perform at any one time.

    deferred reference counting

        Deferred :term:`reference counting` reduces the cost of
        maintaining reference counts by avoiding adjustments when the
        :term:`reference` is stored on the :term:`stack`.

        On many systems, the majority of stores are made into local
        variables, which are kept on the stack. Deferred reference
        counting leaves those out and counts only references stored in
        :term:`heap` objects. This requires compiler support, but can
        lead to substantial performance improvements.

        :term:`Objects <object>` cannot be :term:`reclaimed <reclaim>`
        as soon as their reference count becomes zero, because there
        might still be references to them from the stack. Such objects
        are added to a :term:`zero count table` (ZCT) instead. If a
        reference to an object with a count of zero is stored into the
        heap, then the object is removed from the ZCT. Periodically
        the stack is :term:`scanned <scan>`, and any objects in the
        ZCT which were not referenced from the stack are reclaimed.

        Deferred reference counting has been used successfully with
        several languages, notably :term:`Smalltalk`. However, since
        it fails to collect objects with :term:`cyclic <cyclic data
        structure>` references, it is often used alongside a
        :term:`tracing garbage collector <tracing garbage
        collection>`.

        .. bibref:: [DB76]_.

    debugging pool

        .. mps:: ??

    derived pointer

        .. see:: :term:`interior pointer`.

    destructor (1)

        A destructor is a function or a method that performs the
        explicit :term:`deallocation <free (1)>` of an :term:`object`.
        It may also perform clean-up actions.

        .. opposite:: :term:`constructor (1)`.

    destructor (2)

        In :term:`C++`, a *destructor* is a member function that is
        used to clean up when an object is being :term:`deallocated
        <free (1)>`.

        When an object is being destroyed (by ``delete`` or
        automatically), the appropriate destructor is called, and then
        the actual deallocation of :term:`memory (2)` is performed by
        ``operator delete`` or the run-time system (for :term:`static
        <static allocation>` and :term:`stack allocation`).

        .. seealso:: :term:`constructor (2)`.

    DGC

        .. see:: :term:`distributed garbage collection`.

    direct method

        Direct methods of :term:`automatic memory management` maintain
        information about the :term:`liveness <live>` of each
        :term:`object`, detecting :term:`garbage` directly.

        Such bits of information, e.g., :term:`reference counts
        <reference counting>`, are typically stored within the objects
        themselves.

        Direct :term:`garbage collection` can allow :term:`memory (2)`
        to be :term:`reclaimed <reclaim>` as soon as it becomes
        :term:`unreachable`. However, the stored information must be
        updated as the :term:`graph` of objects changes; this may be
        an expensive operation, especially in :term:`distributed
        garbage collection` where it can lead to intensive
        communication between processors, and make garbage collection
        less robust to network failures.

        .. opposite:: :term:`indirect method`.

        .. bibref:: [JONES96]_.

    dirty bit

        A dirty bit is a flag indicating that a :term:`page` (or
        similar) has been written to since it was last examined.

        Dirty bits are used by :term:`caches (2) <cache (2)>` to
        determine which pages must be written out, and by garbage
        collectors in conjunction with :term:`write barriers <write
        barrier>`.

    distributed garbage collection

        .. aka:: *DGC*.

        Distributed garbage collection is :term:`garbage collection`
        in a system where :term:`objects <object>` might not reside in
        the same :term:`address space` or even on the same machine.

        Distributed garbage collection is difficult to achieve in
        widely-distributed systems (over wide-area networks) because
        of the costs of synchronization and communication between
        processes. These costs are particularly high for a
        :term:`tracing garbage collector <tracing garbage
        collection>`, so other techniques, including :term:`weighted
        reference counting`, are commonly used instead.

    double buddies

        A :term:`buddy system` :term:`allocation mechanism` using a
        pair of :term:`binary buddy <binary buddies>` systems with
        staggered size classes.

        One system is a pure binary buddy, with powers-of-two classes
        (2, 4, 8,...). The other uses some fixed multiple of
        powers-of-two (e.g., 3, 6, 12, ...). This resembles
        :term:`weighted buddies`, but the two buddy systems are
        treated independently: blocks cannot be :term:`split` or
        :term:`coalesced <coalesce>` from one to the other.

        .. bibref:: [WISE78]_.

    double free

        A double free is when an attempt is made to :term:`free (1)` a
        :term:`memory (2)` :term:`block` that has already been freed.

        This usually occurs in :term:`manual memory management` when
        two parts of a program believe they are responsible for the
        management of the same block.

        Many manual :term:`memory managers <memory manager>` have
        great trouble with double frees, because they cannot cheaply
        determine that :term:`deallocated <free (1)>` blocks were
        already free. Instead, they corrupt their :term:`free block
        chain`, which leads to mysterious problems when the same block
        is subsequently :term:`allocated <allocate>`.

        .. seealso:: :term:`premature free`.

    doubleword

        .. aka:: *longword*.

        A *doubleword* is a unit of memory consisting of two adjacent
        :term:`words <word>`. In digital's Alpha architecture, it's
        called a *longword*.

        .. historical::

            On the Intel 80386, 80486. and Pentium; processors, the
            doubleword of 32 bits is actually the *natural word size*,
            but the term *word* is still used for the 16-bit unit, as
            it was on earlier processors of this series.

        .. seealso:: :term:`quadword`.

    DRAM

        .. see:: :term:`dynamic memory`.

    dynamic allocation

        .. see:: :term:`heap allocation`.

    dynamic extent

        An :term:`object` has dynamic :term:`extent` if its
        :term:`lifetime` is bounded by the execution of a function or
        some other block construct.

        Objects of dynamic extent are usually :term:`stack-allocated
        <stack allocation>`.

        .. similar:: :term:`automatic storage duration`.

        .. opposite:: :term:`indefinite extent`.

    dynamic memory

        .. aka:: *dynamic RAM*, *DRAM*.

        Dynamic memory, or dynamic RAM (DRAM, pronounced "dee ram"),
        is a type of :term:`RAM`.

        Dynamic RAM requires periodic refreshing to avoid losing its
        contents (as opposed to :term:`static memory (1)`, the
        contents of which are preserved without any need for
        refreshing). The refreshing is performed by additional
        "refresh hardware" usually external to the dynamic RAM package
        itself, sometimes by the main CPU. Dynamic RAM is cheap and
        compact and is the choice for large amounts of relatively fast
        RAM, such as the :term:`main memory` of PCs. Dynamic RAM often
        comes packaged in SIMMs or DIMMs.

        .. seealso:: :term:`static memory (1)`, :term:`SDRAM`.

    dynamic RAM

        .. see:: :term:`dynamic memory`.


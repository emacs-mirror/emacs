.. _glossary-a:

=============================
Memory Management Glossary: A
=============================

.. include:: alphabet.txt

.. glossary::

    absolute address

        .. see:: :term:`physical address`.

    activation frame

        .. see:: :term:`activation record`.

    activation record

        .. aka:: *function record*, *activation frame*.

        An activation or function record is a data structure,
        associated with the invocation of a function, procedure, or
        control block that stores the variables, temporaries, and
        fixed-sized data that are local to the block, and the
        information required to return to the invoking context. It is
        often stored on a :term:`control stack`.

        In a register-based hardware architecture, the current
        activation record is typically partially stored in registers.

        :term:`Closures` and :term:`continuations` are specializations
        of activation records in support of particular language
        features of :term:`LISP`, :term:`Scheme` and related
        languages.

        .. relevance::

            The current activation record is part of the state of the
            :term:`mutator`, and is therefore a :term:`root` to the
            :term:`collector (2)`. In languages that permit recursion,
            activation records have :term:`dynamic extent`. In
            languages that permit closures or continuations,
            activation records may have :term:`indefinite extent`.
            Although they may not be visible to the programmer, their
            :term:`memory (1)` must be managed by the
            language run-time support. Because they are usually not
            visible to the programmer, they may be a source of
            inexplicable memory overhead.

        .. seealso:: :term:`stack frame`.

    activation stack

        .. see:: :term:`control stack`.

    active

        .. see:: :term:`live`.

    address

        An address is a specification of a :term:`memory location` in
        an :term:`address space`.

        An address is almost always represented as an unsigned integer
        stored in a single :term:`machine word`. The address is
        decoded by the hardware in order to access a location on a
        :term:`physical memory (2)` device (such as a :term:`RAM`) or
        some :term:`memory-mapped <memory mapping>` resource.

        .. figure:: ../diagrams/address.svg
            :align: center
            :alt: Diagram: A simplified view of addresses, address space, and locations on a 32-bit architecture.

            A simplified view of addresses, address space, and
            locations on a 32-bit architecture.

        .. similar:: :term:`pointer`.

        .. mps:specific::

            An address is represented by a value of the type
            :c:type:`mps_addr_t`.

    address space

        An *address space* is the set of possible :term:`addresses`.
        It can also be considered to be a partial function from
        addresses to :term:`locations <memory location>`.

        Typically, addresses start at zero and run to 2\ :sup:`n`\ −1,
        where *n* is the address width (for example, 15, 16, 24, 32,
        64), which is usually the same as the width of the address
        bus. This may not be true for :term:`segmented <segmented
        addressing>` architectures.

        In modern systems, large parts of the whole address space may
        be reserved by the operating system or architecture, or not
        :term:`mapped` at any given time. The mapped part of the
        address space may be discontiguous or sparse.

        .. seealso:: :term:`virtual address space`, :term:`physical address space`.

    address translation cache

        .. see:: :term:`translation lookaside buffer`.

    address-ordered first fit

        The :term:`allocation policy` that always uses the suitable
        :term:`free block` with the lowest address. One of the most
        common allocation policies in use. Commonly implemented by
        :term:`first fit` on a single address-ordered :term:`free
        block chain`. Sometimes just called "first fit".

        .. seealso:: :term:`FIFO-ordered first fit`, :term:`LIFO-ordered first fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    aging space

        In some :term:`generational garbage collection` systems, when
        :term:`generations` are divided into
        :term:`buckets`, the aging space is where
        :term:`objects` which survive a :term:`collection
        cycle` stay until they are old enough to be :term:`promoted
        <promotion>`.

        .. opposite:: :term:`creation space`.

    algebraic data type

        Algebraic data types aggregate or alternate a number of
        dissimilarly-typed objects. They are termed *algebraic*
        because they can be expressed using the sum and product
        operators, for example (a and b and c) or d.

        Examples of algebraic data types include: structures, records,
        tuples, and unions.

        .. relevance::

            Algebraic data types are usually represented using a
            :term:`heap`. Because of their non-uniformity, algebraic
            data types are more difficult to :term:`scan`.

        .. seealso:: :term:`scalar data type`, :term:`vector data type`, :term:`heap`.

    alignment

        Alignment is a constraint on the :term:`address` of an
        :term:`object` in :term:`memory (2)`.

        The constraint is usually that the object's address must be a
        multiple of a power of two, 2\ :sup:`n`, and therefore that
        the least significant *n* bits of the address must be zero.

        The bus hardware of many modern processors cannot access
        multi-:term:`byte (2)` objects at any memory address. Often
        :term:`word`-sized objects must be aligned to word boundaries,
        double-words to double-word boundaries, double-floats to
        8-byte boundaries, and so on. If a program attempts to access
        an object that is incorrectly aligned, a :term:`bus error`
        occurs.

        .. relevance::

            A memory manager must take care to :term:`allocate` memory
            with an appropriate alignment for the object that is going
            to be stored there. Implementations of :term:`malloc` have
            to allocate all :term:`blocks` at the largest
            alignment that the processor architecture requires. Other
            reasons for aligning objects include using the least
            significant bits of the address for a :term:`tag`.

        .. opposite:: :term:`unaligned`.

        .. seealso:: :term:`natural alignment`.

        .. mps:specific::

            An alignment is represented by the unsigned integral type
            :c:type:`mps_align_t`. It must be a positive power of 2.

    alive

        .. see:: :term:`live`.

    allocate

        .. aka:: *cons*.

        *Allocation* is the process of assigning resources. When
        requested to by the program, an application :term:`memory
        manager` or :term:`allocator` *allocates* a :term:`block` of
        :term:`memory (2)` for the program to store its data in.
        Allocation is also known as *consing*, from :term:`cons (1)`.

        When faced with a request for memory from the program, a
        memory manager must choose a suitable block and hand it over,
        or fail. The choices made by the memory manager at this point
        can have a significant effect on the future efficiency of the
        program.

        Allocation is rarely a simple issue. For example, programs
        usually allocate :term:`activation records` (:term:`automatic
        variables <automatic storage duration>`, and so on) for
        functions from a processor :term:`stack` simply by subtracting
        a number from their stack :term:`pointer`. However, in a
        :term:`virtual memory` system, this may extend the stack onto
        a previously unused :term:`page`, in which case the operating
        system memory manager must carry out some quite complex
        operations in order to supply the program with :term:`backing
        store` for the stack so that the program can continue.

        .. historical::

            The term *reserved* was often used to mean "allocated".

        .. similar:: :term:`malloc`.

        .. opposite:: :term:`free (1)`.

        .. seealso:: :term:`constructor (1)`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

        .. mps:specific::

            See :ref:`topic-allocation`.

    allocation frame

        .. mps:specific::

            An allocation frame is a marker that can pushed onto an
            :term:`allocation point` by calling
            :c:func:`mps_ap_frame_push`, and then popped by calling
            :c:func:`mps_ap_frame_pop` to indicate that all blocks
            allocated on the allocation point are :term:`dead` (in the
            case of :term:`manual <manual memory management>` pools), or
            very likely dead (in the case of :term:`automatic
            <automatic memory management>` pools). Allocation frames can
            be used by the :term:`client program` to efficiently
            implement stack-like patterns of allocation.

    allocation mechanism

        The algorithm by which an :term:`allocator` chooses a
        :term:`free block` from which to satisfy an allocation
        request. An allocation mechanism is the implementation of an
        :term:`allocation policy`.

        A common mechanism is ":term:`first fit` on an address-ordered
        :term:`free block chain`, with eager :term:`coalescing
        <coalesce>`". This implements the :term:`address-ordered first
        fit` policy, and commonly inherits that name, although there
        are many other mechanisms for implementing that policy, for
        example, "leftmost fit on a balanced tree of free blocks
        ordered by address".

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    allocation pattern

        .. mps:specific::

            A hint to the MPS to expect a particular pattern of
            allocation on an :term:`allocation point`. The MPS may use
            this hint to schedule its decisions as to when and what to
            collect. See :ref:`topic-pattern`.

    allocation point

        .. mps:specific::

            An allocation point is an interface to a :term:`pool`
            which provides fast :term:`buffered` allocation, and
            defers the need for synchronization in a multi-threaded
            environment. Allocation points belong to the type
            :c:type:`mps_ap_t`.

    allocation point protocol

        .. mps:specific::

            The protocol that ensures safe inline allocation on an
            :term:`allocation point`. See
            :ref:`topic-allocation-point-protocol`.

    allocation policy

        .. aka:: *placement policy*.

        The concrete policy used by an :term:`allocator` for choosing
        a :term:`free block` to satisfy an :term:`allocation
        <allocate>` request.

        For instance, "always allocate from the largest free block"
        (:term:`worst fit`) or "use the most recently freed block
        suitable" (:term:`LIFO-ordered first fit`).

        Each allocation policy is motivated by an :term:`allocation
        strategy` and implemented by an :term:`allocation mechanism`.

        .. seealso:: :term:`address-ordered first fit`, :term:`best fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    allocation strategy

        The high-level design motivation or strategy, of an
        :term:`allocator`, which uses observations or theories about
        patterns of allocation requests to justify an
        :term:`allocation policy`.

        For instance, "do not allow small long-lived :term:`objects`
        to fragment large :term:`free (3)` areas", "allocate
        consecutive objects close together", and so on. The allocation
        strategy motivates an :term:`allocation policy`, which is
        implemented by an :term:`allocation mechanism`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    allocator

        The term *allocator* is often used to refer to the
        :term:`memory manager`, usually when it is a simple manual
        one.

        .. similar:: :term:`memory manager`.

        .. seealso:: :term:`allocation <allocate>`.

    ambiguous reference

        .. aka:: *unsure reference*.

        An ambiguous or unsure :term:`reference` is a value that is
        potentially a reference, but the :term:`collector (1)` cannot
        prove that it is.

        The presence of ambiguous references in a
        :term:`garbage-collected <garbage collection>` system requires
        the use of :term:`conservative garbage collection`.

        .. opposite:: :term:`exact reference`.

        .. seealso:: :term:`floating garbage`.

    ambiguous root

        An ambiguous root is a :term:`root` containing
        :term:`ambiguous references`.

        .. opposite:: :term:`exact root`.

        .. mps:specific::

            An ambiguous root has :term:`rank`
            :c:func:`mps_rank_ambig`.

    arena

        The area of :term:`memory (2)` used by :term:`malloc` for
        allocation.

        So named from a semi-mythical "malloc: corrupted arena"
        message supposedly emitted when some early versions became
        terminally confused.

        .. seealso:: :term:`brk`.

        .. mps:specific::

            An arena is the data structure responsible for requesting
            :term:`memory (3)` from the operating system, making it
            available to :term:`pools`, and for :term:`garbage
            collection`. Arenas belong to the type
            :c:type:`mps_arena_t`. See :ref:`topic-arena`.

    arena class

        .. mps:specific::

            A value of type :c:type:`mps_arena_class_t` describing a
            class of :term:`arenas`. Arena classes include
            :term:`client arenas` and :term:`virtual memory arenas`.

    assertion

        A declaration in a program of a condition that is expected
        always to be true, or which must be true in order for the
        program to continue to execute correctly.

        .. mps:specific::

            Memory management mistakes often lead to
            :term:`overwriting errors` that
            corrupt the data structures used by the memory manager to
            maintain memory. Except in the :term:`rash`
            :term:`variety`, most MPS functions assert the validity of
            the data structures they operate on. This means that
            memory management mistakes are detected as early as
            possible, when there may still be enough evidence in the
            :term:`heap` to debug them. See :ref:`topic-error`.

    asynchronous garbage collector

        A :term:`collector (2)` is asynchronous with respect to the
        :term:`mutator` if it cannot be (easily) predicted when the
        collector will run.

        This means that the mutator must ensure that :term:`formatted
        objects` are always :term:`scannable <scan>`.

        .. opposite:: :term:`synchronous garbage collector`.

    ATC

        .. see:: :term:`translation lookaside buffer`.

    atomic object

        .. see:: :term:`leaf object`.

    automatic memory management

        Automatic :term:`memory management` is a general term for
        techniques that automatically :term:`recycle` unused
        :term:`memory (2)`.

        It is not possible, in general, to automatically determine
        which :term:`objects` are still :term:`live`. Even if
        it didn't depend on future input, there can be no general
        algorithm to prove that an object is live (cf. the Halting
        Problem). However, effective approximations are possible.

        In :term:`tracing garbage collection`, the approximation is
        that an object can't be live unless it is :term:`reachable`.
        In :term:`reference counting`, the approximation is that an
        object can't be live unless it is :term:`referenced`. Analysis
        of the program text can reveal where objects :term:`die
        <dead>`; A notable technique in this vein is :term:`region
        inference`.

        Hybrid algorithms are also possible.

        .. similar:: :term:`garbage collection`.

        .. opposite:: :term:`manual memory management`.

    automatic storage duration

        In :term:`C`, :term:`objects` that are declared with
        *automatic storage duration* are :term:`live` for the duration
        of a block of code.

        In most implementations of C, objects with automatic storage
        duration are :term:`allocated` on the :term:`stack` when a
        function is entered, and :term:`deallocated <free (1)>` when
        it returns.

        .. similar:: :term:`stack allocation`, :term:`dynamic extent`.

        .. opposite:: :term:`static storage duration`.

.. _glossary-v:

=============================
Memory Management Glossary: V
=============================

.. include:: alphabet.txt

.. glossary::

    value object

        .. aka:: *immutable object*.

        A *value object* or *immutable object* is an :term:`object`
        whose identity depends solely upon its value or magnitude.

        In a typed language, the compiler can often determine at
        compile time that certain types can be represented as value
        objects. Usually these types are a :term:`scalar data type`
        with bounded magnitude.

        .. relevance::

            If value objects can be identified, the compiler and the
            memory manager can make certain optimizations: Value
            objects can be represented as :term:`immediate data` to
            minimize storage overhead, they can be replicated to
            improve :term:`locality <locality of reference>`, and a
            :term:`vector data type` of value objects can be
            represented as a :term:`leaf object`.

        .. historical::

            Some programming languages expose representational details
            such as the use of value objects. In :term:`Lisp`, for
            example, numbers are often represented as value objects
            but not always as immediate data. The ``EQ`` predicate of
            Lisp tests if two objects have the same representation,
            whereas the ``EQL`` predicate tests if two objects
            represent the same type and value (are computationally
            identical). Because the choice of representation is an
            optimization, exposing it at the language level can
            cause programs to behave differently under different
            compilers or optimization settings. Modern languages, such
            as :term:`Dylan` hide this representational distinction,
            permitting the compiler greater freedom in optimization.

        .. similar:: :term:`immediate data`.

        .. seealso:: :term:`immutable`.

        .. bibref:: :ref:`Baker (1993a) <BAKER93A>`.

    variety

	.. mps:specific::

            A behaviour of the MPS that must be selected at
            compilation time. There are three varieties: :term:`cool`,
            :term:`hot` and :term:`rash`. See :ref:`guide-build`.

    vector data type

        A vector data type is an aggregate type of more than one
        dimension whose objects have a value for each dimension, where
        each dimension is of the same type.

        Examples of vector data types include: strings, arrays, and
        lists.

        .. relevance::

            Vector data types are seldom represented using
            :term:`value objects`, but may be
            represented using :term:`leaf objects` if
            they are an aggregate of a type that can be represented by
            :term:`value objects`. :term:`Scanning
            <scan>` information for vectors can be compactly encoded
            in terms of the aggregated type and the vector dimension.

        .. seealso:: :term:`scalar data type`, :term:`algebraic data type`, :term:`value object`, :term:`leaf object`.

    virtual address

        .. aka:: *logical address*.

        In a :term:`virtual memory` system, the :term:`addresses` that
        application programs deal with are known as *virtual
        addresses*.

        The virtual addresses used by the application program are
        translated by the virtual memory system (often using
        :term:`translation lookaside buffers` and :term:`page tables`)
        to :term:`physical addresses`. It is the physical address that
        is used to retrieve the contents from the :term:`memory (3)`.

        .. opposite:: :term:`physical address`.

    virtual address space

        The virtual :term:`address space` is the space of
        :term:`virtual addresses`.

        On :term:`virtual memory` systems, user processes see the
        virtual address space, and commonly have a separate virtual
        address space each, so that they map the same addresses to
        different data. These systems often have :term:`shared memory`
        as well.

        .. opposite:: :term:`physical address space`.

    virtual memory

        .. aka:: *VM*.

        In a *virtual memory* (*VM*) system, the program code deals
        with :term:`virtual addresses`. Upon use,
        the virtual address is translated by the :term:`MMU` to obtain
        a :term:`physical address` that is used to access
        :term:`physical memory (1)`.

        Some operating systems can simulate having more :term:`memory
        (2)` than is available as :term:`main memory`, by storing part
        of the data in :term:`backing store`, typically on disk. If
        the :term:`page` referenced by the virtual address is not
        currently in main memory, a :term:`page fault` occurs,
        triggering an operating system handler that :term:`swaps in
        <swapped in>` the page. Some other page might be
        :term:`swapped out` to make room.

        Each process typically has its own separate :term:`virtual
        address space` with its own :term:`mappings` and
        :term:`protections`.

        .. figure:: ../diagrams/virtual-memory.svg
            :align: center
            :alt: Diagram: Example of the relationship between the virtual address spaces of two processes, physical memory, and backing store.

            Example of the relationship between the virtual address
            spaces of two processes, physical memory, and backing
            store.

        Virtual memory technology can be used in many useful memory
        management techniques, such as :term:`barriers (1)`,
        copy-on-write, and :term:`memory mapping`.

            "Virtual" means never knowing where your next byte is
            coming from. --- ``fortune(6)``

        .. opposite:: :term:`real memory (1)`.

        .. seealso:: :term:`paging`, :term:`paged in`, :term:`paged out`, :term:`swapping`, :term:`swap space`, :term:`mapped`, :term:`reserved`, :term:`unmapped`, :term:`shared memory`.

    virtual memory arena

        .. mps:specific::

            An :term:`arena class` which gets its :term:`memory (2)`
            from the operating system's :term:`virtual memory`
            interface. See :ref:`topic-arena-vm`.

    visitor function

       .. see:: :term:`stepper function`.

    VM (1)

        .. see:: :term:`virtual memory`.

    VM (2)

        In the :term:`PostScript` language, *VM* is the :term:`memory
        (1)` where the values of the :term:`composite objects` reside.

        VM is short for "virtual memory", but this has nothing to do
        with the usual sense of the phrase (see :term:`virtual memory`).



.. _glossary-h:

=============================
Memory Management Glossary: H
=============================

.. include:: alphabet.txt

.. glossary::

    handle

        A handle is an object that represents a resource.

        Handles are used when the resource cannot be represented
        directly. For example, a file handle is an object passed
        between a process and the OS in order to access a file,
        because the file itself cannot be represented.

        .. relevance::

            In memory management, a handle is an object that
            represents another :term:`object`. Handles are usually
            used because the object itself needs to be :term:`moved
            <moving garbage collector>` in :term:`memory (2)`, or even
            :term:`swapped out` to disk. The program therefore cannot
            know the :term:`address` of the object.

        For example, Apple's Classic Mac OS made extensive use of
        handles in its `heap management
        <http://web.archive.org/web/200012120034/http://developer.apple.com/techpubs/mac/Memory/Memory-11.html>`_
        to avoid problems due to :term:`fragmentation`. If the Classic
        Mac OS Memory Manager could not satisfy a request for memory,
        it tried :term:`compacting <compaction>` the :term:`heap`:
        moving all the :term:`relocatable <relocation>` objects
        together to squeeze out gaps. It could do this because the
        program only had handles on the objects, and not their actual
        addresses.

        .. figure:: ../diagrams/handle.svg
            :align: center
            :alt: Diagram: Handle-based heap.

            Handle-based heap.

        .. similar:: :term:`pointer`.

    header

        .. see:: :term:`in-band header`.

    heap

        .. aka:: *free store*, *freestore*.

        The *heap* or *free store* is the :term:`memory (2)` area
        managed by :term:`dynamic allocation`.

        This use of *heap* is unconnected with the data structure used
        by the heapsort algorithm.

    heap allocation

        .. aka:: *dynamic allocation*.

        *Heap allocation* or *dynamic allocation* means run-time
        :term:`allocation <allocate>` and :term:`deallocation <free
        (1)>` of :term:`memory (1)` in arbitrary order.

        Dynamic allocation is usually for :term:`objects`
        whose size, quantity, or :term:`lifetime` could not be
        determined at compile-time. It is necessary to implement
        modern data structures, such as recursive trees and full
        :term:`closures`.

        Objects on the :term:`heap` can be managed :term:`manually
        <manual memory management>`, as in :term:`C`, or
        :term:`automatically <automatic memory management>`, as in
        :term:`Lisp` and :term:`Java`.

        .. opposite:: :term:`stack allocation`, :term:`static allocation`.

        .. seealso:: :term:`indefinite extent`.

    hit

        A hit is a successful lookup in any form of :term:`cache (3)
        <caching (3)>`, most commonly at some level of a
        :term:`storage hierarchy`, such as a :term:`cache (1)` or
        :term:`virtual memory` system.

        .. opposite:: :term:`miss`.

    hit rate

        At any level of a :term:`storage hierarchy`, the hit rate is
        the proportion of accesses which :term:`hit`.

        .. opposite:: :term:`miss rate`.

    hosted

        In the :term:`C` programming language, a hosted implementation
        is one that provides all the features of the standard C
        library.

        .. opposite:: :term:`freestanding`.

        .. bibref:: :ref:`ISO/IEC 9899:1990 <C1990>`, :ref:`ISO/IEC 9899:1999 <C1999>`.

    hot

        .. mps:specific::

            A :term:`variety` in which many MPS functions
            :term:`assert <assertion>` that their data structures are
            valid, but functions on the :term:`critical path` do not.
            Select it by defining :c:macro:`CONFIG_VAR_HOT`. Compare
            :term:`cool` and :term:`rash`.

    hot end

        .. opposite:: :term:`cold end`

        A :term:`control stack` has two ends: the oldest items are at
        the *cold end* and the newest items are at the *hot end*.
        Sometimes the hot end is called the "top" of the stack, but
        that is misleading when the stack grows downwards, as it does
        on common computing platforms.

    huge page

        .. aka:: *large page*, *superpage*.

        Some processor architectures support multiple :term:`page`
        sizes. This allows operating systems to better match the page
        size to the granularity of memory usage and so reduce the size
        of the :term:`page table`.

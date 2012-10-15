.. _glossary-t:

=============================
Memory Management Glossary: T
=============================

.. include:: alphabet.txt

.. glossary::

    tabling

        .. see:: :term:`caching (3)`.

    tag

        A tag is a piece of information associated with an
        :term:`object` or :term:`reference` that allows the
        representation of the object to be determined.

        Tags are often used to represent types in the implementation
        of a dynamically-typed language. In statically-typed
        languages, types are usually implicit and not permitted to
        change at run-time, so tagging is rarely required.

        One of the simplest forms of tag is a :term:`word` at the
        beginning of the object that points to a block of information
        about the object's :term:`format`.

        .. figure:: ../diagrams/tag-word.png
            :align: center
            :alt: Diagram: Example of a tag-word at the start of an object.

            Example of a tag-word at the start of an object.

        Another common form of tagging is to :term:`align <alignment>`
        objects and keep information in the least significant bits of
        the :term:`address`.

        .. figure:: ../diagrams/tag-ref.png
            :align: center
            :alt: Diagram: Example of reference tagging, using the least significant bits.

            Example of reference tagging, using the least significant bits.

        In :term:`C`, when a structure contains a union, it is common
        to add a field to the structure to indicate which union member
        is currently being used. This field is known as a
        *discriminator*, and is a form of tag. Analogues occur in
        other languages, sometimes with compiler or run-time support.

        .. seealso:: :term:`tagged architecture`, :term:`header`.

        .. bibref:: [GUDEMAN93]_.

    tagged architecture

        A tagged architecture is a hardware architecture where each
        memory :term:`word` is divided into a "data" and a :term:`tag`
        section. The data section is sufficiently large to contain a
        memory :term:`address` and the tag section is used to describe
        how the data section is to be interpreted (that is, it encodes
        the type of the data).

        .. relevance::

            Tagged architectures greatly simplify the implementation
            of a memory manager because each word of memory is
            self-describing.

        .. historical::

            The :term:`Lisp` Machine is an example of a tagged architecture.

    tagged reference

        A :term:`reference` containing a :term:`tag` in part of its
        address, for example by :term:`aligning <alignment>` objects
        and keepping the tag in the least significant bits of the
        address.

    TB (1)

        .. see:: :term:`terabyte`.

    TB (2)

        .. see:: :term:`translation lookaside buffer`.

    telemetry filter

        .. mps:specific::

            A :term:`bitmap` indicating which events the MPS should
            include in the :term:`telemetry stream`. It can be read or
            changed by calling :c:func:`mps_telemetry_control`.

    telemetry label

        .. mps:specific::

            An indentifier representing a string, returned from
            :c:func:`mps_telemetry_intern`, that can be associated
            with certain :term:`formatted objects <formatted object>`
            of variant B, and so appear in the :term:`telemetry
            stream` attached to events concerning those objects. See
            :ref:`topic-telemetry`.

    telemetry stream

        .. mps:specific::

            A sequence of events reported by the MPS to assist with
            debugging and profiling. The events that appear in the
            stream can be configured by setting the :term:`telemetry
            filter`. See :ref:`topic-telemetry`.

    tenuring

        .. see:: :term:`promotion`.

    terabyte

        .. aka:: *TB (1)*.

        A terabyte is 1024 :term:`gigabytes <gigabyte>`, or
        1099511627776 :term:`bytes (1) <byte (1)>`.

        See :term:`byte (1)` for general information on this and
        related quantities.

    termination

        .. see:: :term:`finalization`.

    thrash

        A :term:`cache (2)` is said to :term:`thrash` when its
        :term:`miss rate` is too high, and it spends most of its time
        servicing :term:`misses <miss>`. Thrashing is bad for
        performance, particularly :term:`virtual memory`
        thrashing, because the relative cost of a miss is so high: it
        may slow a machine down by a factor of a hundred or more.

        Thrashing is typically caused by a process or system having a
        :term:`working set` which is larger than its :term:`cache (1)`
        or :term:`main memory`. It may also be caused by a failure of
        :term:`cache policy`. A system with an inflexible cache policy
        may thrash even when the working set is quite small.

        For instance, a virtual memory system which has four megabytes
        of :term:`physical memory (1)` but which has a working set of
        ten megabytes will :term:`thrash` badly.

        .. bibref:: [DENNING68]_, [DENNING70]_, [DS72]_.

    thread

        A thread of execution is a sequence of instructions that take
        place sequentially. In a multi-threaded program, multiple
        threads of execution operate in parallel, and are generally
        asynchronous with respect to each other.

        .. relevance::

            Access to shared resources such as memory management
            interface must be thread-safe. Each thread has its own
            :term:`control stack` which may contain :term:`references
            <reference>` to blocks on the heap.

        .. mps:specific::

            Threads are represented by values of type
            :c:type:`mps_thr_t`, created by calling
            :c:func:`mps_thread_reg`. In order for the MPS to find
            references on the control of the thread, the thread must
            be also be registered as a root by calling
            :c:func:`mps_root_create_reg`.

    threatened set

        .. see:: :term:`condemned set`.

    TLB

        .. see:: :term:`translation lookaside buffer`.

    trace

        In :term:`tracing garbage collection`, tracing is the process
        of following the :term:`graph` from all :term:`roots <root>`
        to all :term:`reachable` data.

        .. similar:: :term:`scan`.

    tracing garbage collection

        Tracing garbage collection is :term:`garbage collection` based
        on :term:`reachability <reachable>`.

        Tracing garbage collection relies on the fact that if an
        :term:`object` is not :term:`reachable`, there is no way the
        :term:`mutator` could ever access it, and therefore it cannot
        be :term:`alive`. In each :term:`collection cycle`, some or
        all of the objects are :term:`condemned <condemned set>` and
        the :term:`graph` is :term:`traced <trace>` to find which of
        the condemned objects are reachable. Those that were not
        reachable may be :term:`reclaimed <reclaim>`.

    translation buffer
    translation lookaside buffer

        .. aka:: , *address translation cache*, *ATC*, *TB (2)*.

        The *translation lookaside buffer* or *address translation
        cache* is small piece of associative :term:`memory (1)` within
        a processor which caches part of the translation from
        :term:`virtual addresses <virtual address>` to :term:`physical
        addresses <physical address>`.

        In a :term:`virtual memory` system there is a translation
        from :term:`virtual addresses <virtual address>` to
        :term:`physical addresses <physical address>`. This
        translation can often be very large and complex and the data
        structures that implement the translation (often a
        :term:`page-table <page table>`) can be too large to store
        efficiently on the processor. Instead, a few elements of the
        translation are stored in the TLB; the processor can access
        the TLB extremely quickly. If a required translation for a
        particular virtual address is not present in the TLB then *a
        TLB miss* is taken and the address is resolved using the more
        general mechanism.

    transport

        In a :term:`copying collector <copying garbage collection>`,
        transporting is preventing an :term:`object` in the
        :term:`condemned set` from being collected by copying it and
        adjusting the :term:`reference` by which it was discovered to
        point to the new copy.

        .. seealso:: :term:`scavenging <copying garbage collection>`, :term:`snap-out`.

    transport snap-out

        .. see:: :term:`snap-out`.

    treadmill

        Henry Baker has devised an :term:`incremental <incremental
        garbage collection>` non-:term:`moving <moving garbage
        collector>` :term:`garbage collector` that uses a circular
        doubly-linked list, called the treadmill, to implement
        :term:`tri-color marking`.

        Every :term:`object` is on the list. The list has four
        sections corresponding to :term:`colors <color>`. The
        :term:`black`, :term:`gray` and :term:`white` sections are
        used for tri-color marking, and an additional
        :term:`off-white` section is used for :term:`free (3)`
        objects. The color of an object is changed by unlinking it
        from the list and relinking it to a different part of the
        list.

        .. figure:: ../diagrams/treadmill.png
            :align: center
            :alt: Diagram: A treadmill.

            A treadmill.

        .. bibref:: [BAKER92C]_.

    tri-color invariant
    tri-colour invariant
    tricolor invariant
    tricolour invariant

        The term "tri-color invariant" is used to refer to any of a
        number of properties of a :term:`reference` :term:`graph` that
        are preserved throughout a :term:`tri-color marking` algorithm
        to ensure the correctness.

        There are two important ones: the :term:`strong tri-color
        invariant` and the :term:`weak tri-color invariant`. When
        people say "the tri-color invariant" they probably mean the
        strong one.

        .. bibref:: [PIRINEN98]_.

    tri-color marking
    tri-colour marking
    tricolor marking
    tricolour marking

        Tri-color marking is a :term:`tracing garbage collection`
        algorithm that assigns a :term:`color` (:term:`black`,
        :term:`white`, or :term:`gray`) to each :term:`node` in the
        :term:`graph`. It is basic to :term:`incremental garbage
        collection`.

        Initially all nodes are colored white. The distinguished
        :term:`root set` is colored gray. The :term:`collector (2)`
        proceeds to discover the :term:`reachable` nodes by finding an
        :term:`edge` from a gray node to a white node and coloring the
        white node gray. Hence each tracing step involves choosing a
        gray node and graying its white children.

        When all the edges from a gray node lead only to other gray
        (or black) nodes, the node is colored black. When no gray
        nodes remain, the reachable part of the graph has been
        discovered and any nodes that are still white may be
        :term:`recycled <recycle>`.

        The :term:`mutator` is free to access any part of the graph
        and allocate new nodes while the :term:`collector (2)` is
        determining the reachable nodes, provided the :term:`tri-color
        invariant` is maintained, by changing the colors of the nodes
        affected, if necessary.

        .. historical::

            "Tri-color marking" is the term used to describe an
            algorithm developed in 1975 by E. W. Dijkstra and others,
            as an exercise in proving cooperating programs correct.
            They chose as their problem a :term:`parallel garbage
            collector <parallel garbage collection>`, with the intent
            of illustrating cooperating sequential processes with a
            large shared data space but minimal exclusion and
            synchronization constraints.

        Although the algorithm developed in the paper is not
        necessarily the most efficient algorithm for a
        :term:`collector (1)`, it has been generally accepted to be
        correct: an important feature that not all garbage collectors
        can claim. A number of other garbage collection algorithms
        have been shown to be isomorphic to the tri-color marking
        algorithm and thus are also believed to be correct.

        .. seealso:: :term:`barrier (1)`.

        .. bibref:: [DLMSS76]_.

    two-space collector
    two space collector

        .. aka:: *semi-space collector*.

        A two-space :term:`collector (1)` is a simple form of a
        :term:`copying garbage collector <copying garbage
        collection>`. The available :term:`memory (2)` is divided into
        two halves, called :term:`semi-spaces <semi-space>`.
        :term:`Objects <object>` are allocated in one semi-space until
        it is full. The :term:`reachable` objects are then copied into
        the other semi-space (usually using a :term:`Cheney scan`) and
        the old semi-space is :term:`reclaimed <reclaim>`.
        :term:`Allocation <allocate>` continues in the new semi-space
        until it is full, at which point the process is repeated in
        reverse.

        The main disadvantage of a two-space collector is that it only
        makes use of half of the available memory. This can be
        tolerable in a :term:`virtual memory` system if the
        :term:`garbage collector` is written carefully to preserve
        :term:`locality of reference`. Other forms of copying garbage
        collector, such as :term:`generational garbage collectors
        <generational garbage collection>`, have much lower overheads.

        .. figure:: ../diagrams/two-space-1.png
            :align: center
            :alt: Diagram: Allocation.

            Allocation.

        .. figure:: ../diagrams/two-space-2.png
            :align: center
            :alt: Diagram: Allocation space is full.

            Allocation space is full.

        .. figure:: ../diagrams/two-space-3.png
            :align: center
            :alt: Diagram: Copying garbage collection.

            Copying garbage collection.

        .. figure:: ../diagrams/two-space-4.png
            :align: center
            :alt: Diagram: Allocation continues.

            Allocation continues.

        .. seealso:: :term:`flip`.

    type-accurate garbage collection

        .. see:: :term:`exact garbage collection`.


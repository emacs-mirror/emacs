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

        .. figure:: ../diagrams/tag-word.svg
            :align: center
            :alt: Diagram: Example of a tag-word at the start of an object.

            Example of a tag-word at the start of an object.

        Another common form of tagging is to :term:`align <alignment>`
        objects and keep information in the least significant bits of
        the reference.

        .. figure:: ../diagrams/tag-ref.svg
            :align: center
            :alt: Diagram: Example of reference tagging, using the least significant bits.

            Example of reference tagging, with objects aligned to
            addresses that are multiples of four, and the tag stored
            in the least significant two bits of the reference.

        In :term:`C`, when a structure contains a union, it is common
        to add a field to the structure to indicate which union member
        is currently being used. This field is known as a
        *discriminator*, and is a form of tag. Analogues occur in
        other languages, sometimes with compiler or run-time support.

        .. seealso:: :term:`tagged architecture`, :term:`in-band header`.

        .. bibref:: :ref:`Gudeman (1993) <GUDEMAN93>`.

        .. mps:specific::

            See :ref:`topic-scanning-tag`.

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

            The :term:`Lisp Machine` was an example of a tagged
            architecture.

    tagged reference

        A :term:`reference` containing a :term:`tag` in part of its
        address, for example by :term:`aligning <alignment>` objects
        and keeping the tag in the least significant bits of the
        address.

        .. mps:specific:: See :ref:`topic-scanning-tag`.

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
            with certain :term:`addresses`, and so appear in
            the :term:`telemetry stream` attached to events concerning
            those addresses. See :ref:`topic-telemetry`.

    telemetry stream

        .. mps:specific::

            A sequence of events reported by the MPS to assist with
            debugging and profiling. The events that appear in the
            stream can be configured by setting the :term:`telemetry
            filter`. See :ref:`topic-telemetry`.

    telemetry system

        .. mps:specific::

            The subsystem of the MPS that outputs the :term:`telemetry
            stream`. See :ref:`topic-telemetry`.

    tenuring

        .. see:: :term:`promotion`.

    terabyte

        .. aka:: *TB*.

        A terabyte is 1024 :term:`gigabytes`, or
        1099511627776 :term:`bytes (1)`.

        See :term:`byte (1)` for general information on this and
        related quantities.

    termination

        .. see:: :term:`finalization`.

    thrash

        A :term:`cache (2)` is said to :term:`thrash` when its
        :term:`miss rate` is too high, and it spends most of its time
        servicing :term:`misses`. Thrashing is bad for
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

        .. bibref:: :ref:`Denning (1968) <DENNING68>`, :ref:`Denning (1970) <DENNING70>`, :ref:`Denning & Schwartz (1972) <DS72>`.

    thread

        A thread of execution is a sequence of instructions that take
        place sequentially. In a multi-threaded program, multiple
        threads of execution operate in parallel, and are generally
        asynchronous with respect to each other.

        .. relevance::

            Access to shared resources such as memory management
            interface must be thread-safe. Each thread has its own
            :term:`control stack` which may contain :term:`references`
            to blocks on the heap.

        .. mps:specific::

            Threads are represented by values of type
            :c:type:`mps_thr_t`, created by calling
            :c:func:`mps_thread_reg`. In order for the MPS to find
            references on the control stack of the thread, the thread
            must be also be registered as a :term:`root` by calling
            :c:func:`mps_root_create_thread`. See :ref:`topic-thread`.

    threatened set

        .. see:: :term:`condemned set`.

    TLB

        .. see:: :term:`translation lookaside buffer`.

    to space
    tospace

        .. aka:: *new space*, *newspace*.

        In :term:`copying garbage collection`, the space to which
        :term:`live` object are copied.

        .. opposite:: :term:`fromspace`.

    trace

        In :term:`tracing garbage collection`, tracing is the process
        of following the :term:`graph` from all :term:`roots`
        to all :term:`reachable` data.

        .. similar:: :term:`scan`.

    tracing garbage collection

        Tracing garbage collection is :term:`garbage collection` based
        on :term:`reachability <reachable>`.

        Tracing garbage collection relies on the fact that if an
        :term:`object` is not :term:`reachable`, there is no way the
        :term:`mutator` could ever access it, and therefore it cannot
        be :term:`live`. In each :term:`collection cycle`, some or all
        of the objects are :term:`condemned <condemned set>` and the
        :term:`graph` is :term:`traced` to find which of the condemned
        objects are reachable. Those that were not reachable may be
        :term:`reclaimed`.

    translation buffer
    translation lookaside buffer

        .. aka:: , *address translation cache*, *ATC*, *TB*.

        The *translation lookaside buffer* or *address translation
        cache* is small piece of associative :term:`memory (1)` within
        a processor which caches part of the translation from
        :term:`virtual addresses` to :term:`physical addresses`.

        In a :term:`virtual memory` system there is a translation from
        :term:`virtual addresses` to :term:`physical addresses`. This
        translation can often be very large and complex and the data
        structures that implement the translation (often a :term:`page
        table`) can be too large to store efficiently on the
        processor. Instead, a few elements of the translation are
        stored in the TLB; the processor can access the TLB extremely
        quickly. If a required translation for a particular virtual
        address is not present in the TLB then *a TLB miss* is taken
        and the address is resolved using the more general mechanism.

    transparent alias
    transparent type

        .. mps:specific::

            In the MPS interface, a *transparent type* is an alias
            defined using ``typedef``, and this is documented so that
            the :term:`client program` can rely on that fact. For
            example, :c:type:`mps_addr_t` is a transparent alias for
            ``void *``. See :ref:`topic-interface`.

        .. opposite:: :term:`derived type`, :term:`opaque type`.

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

        Henry Baker devised an :term:`incremental <incremental garbage
        collection>` non-:term:`moving <moving garbage collector>`
        :term:`garbage collector` that uses a circular doubly-linked
        list, called the *treadmill*, to implement :term:`tri-color
        marking`.

        Every :term:`object` is on the list. The list has four
        sections corresponding to :term:`colors`. The
        :term:`black`, :term:`gray` and :term:`white` sections are
        used for tri-color marking, and an additional
        :term:`off-white` section is used for :term:`free (3)`
        objects. The color of an object is changed by unlinking it
        from the list and relinking it to a different part of the
        list.

        .. figure:: ../diagrams/treadmill.svg
            :align: center
            :alt: Diagram: A treadmill.

            A treadmill. (Based on :ref:`Jones (2012) <JONES12>`.)

        .. bibref:: :ref:`Baker (1992c) <BAKER92C>`.

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

        .. bibref:: :ref:`Johnstone (1997) <JOHNSTONE97>`, :ref:`Pirinen (1998) <PIRINEN98>`.

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
        :term:`recycled`.

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

        .. bibref:: :ref:`Dijkstra et al. (1976) <DLMSS76>`.

    two-space collector
    two space collector

        .. aka:: *semi-space collector*.

        A two-space :term:`collector (1)` is a simple form of a
        :term:`copying garbage collector <copying garbage
        collection>`. The available :term:`memory (2)` is divided into
        two halves, called :term:`semi-spaces`. :term:`Objects` are
        allocated in one semi-space until it is full. The
        :term:`reachable` objects are then copied into the other
        semi-space (usually using a :term:`Cheney scan`) and the old
        semi-space is :term:`reclaimed`. :term:`Allocation <allocate>`
        continues in the new semi-space until it is full, at which
        point the process is repeated in reverse.

        The main disadvantage of a two-space collector is that it only
        makes use of half of the available memory. This can be
        tolerable in a :term:`virtual memory` system if the
        :term:`garbage collector` is written carefully to preserve
        :term:`locality of reference`. Other forms of copying garbage
        collector, such as :term:`generational garbage collectors
        <generational garbage collection>`, have much lower overheads.

        .. figure:: ../diagrams/two-space.svg
            :align: center
            :alt: Diagram: Two-space collector.

            Two-space collector.

        .. seealso:: :term:`flip`.

    type-accurate garbage collection

        .. see:: :term:`exact garbage collection`.


    type punning

        Interpreting a value of one type as if it were a value of
        another (for example, via a type cast in :term:`C`),
        especially if such interpretation is not defined by the
        language standard. For example, interpreting a value of type
        ``T**`` (pointer to pointer to ``T``) as ``U**`` is undefined.

        .. mps:specific:: See :ref:`topic-interface`.

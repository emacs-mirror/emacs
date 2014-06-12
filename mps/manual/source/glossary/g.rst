.. _glossary-g:

=============================
Memory Management Glossary: G
=============================

.. include:: alphabet.txt

.. glossary::

    garbage

        Garbage consists of :term:`objects` that are
        :term:`dead`.

        In :term:`tracing garbage collection`, the term is sometimes
        used to mean objects that are known to be dead; that is,
        objects that are :term:`unreachable`.

    garbage collection

        .. aka:: *GC*.

        Garbage collection (GC), also known as *automatic memory
        management*, is the automatic :term:`recycling <recycle>` of
        :term:`dynamically allocated <heap allocation>` :term:`memory
        (2)`. Garbage collection is performed by a :term:`garbage
        collector` which recycles memory that it can prove will never
        be used again. Systems and languages which use garbage
        collection can be described as *garbage-collected*.

        Garbage collection is a tried and tested memory management
        technique that has been in use since its invention in the
        1950s. It avoids the need for the programmer to
        :term:`deallocate <free (1)>` memory :term:`blocks`
        explicitly, thus avoiding a number of problems: :term:`memory
        leaks`, :term:`double frees`, and :term:`premature frees`. The
        burden on the programmer is reduced by not having to
        investigate such problems, thereby increasing productivity.

        Garbage collection can also dramatically simplify programs,
        chiefly by allowing modules to present cleaner interfaces to
        each other: the management of object storage between modules
        is unnecessary.

        It is not possible, in general, for a :term:`garbage
        collector` to determine exactly which :term:`objects` are
        still :term:`live`. Even if it didn't depend on future input,
        there can be no general algorithm to prove that an object is
        live (cf. the Halting Problem). All garbage collectors use
        some efficient approximation to liveness. In :term:`tracing
        garbage collection`, the approximation is that an object can't
        be live unless it is :term:`reachable`. In :term:`reference
        counting`, the approximation is that an object can't be live
        unless it is :term:`referenced`. Hybrid algorithms are also
        possible. Often the term *garbage collection* is used narrowly
        to mean only tracing garbage collection.

        There is a large body of published work on particular and
        general garbage collection algorithms.

        .. historical::

            Garbage collection was first invented by John McCarthy in
            1958 as part of the implementation of :term:`Lisp`.

        Other significant languages offering garbage collection
        include :term:`Java`, :term:`ML`, :term:`Modula-3`,
        :term:`Perl`, :term:`Prolog`, and :term:`Smalltalk`. Major
        applications using garbage collection include Emacs and
        AutoCAD; usually, you can't tell whether an application does
        or not, but these have extension languages that expose the
        fact.

        .. similar:: :term:`automatic memory management`.

        .. opposite:: :term:`manual memory management`.

        .. seealso:: :term:`conservative garbage collection`, :term:`copying garbage collection`, :term:`distributed garbage collection`, :term:`generational garbage collection`, :term:`incremental garbage collection`, :term:`parallel garbage collection`.

        .. bibref:: :ref:`McCarthy (1960) <MCCARTHY60>`.

    garbage collector

        .. aka:: *collector*.

        A (garbage) collector is (an implementation of) a
        :term:`garbage collection` algorithm.

        This term is often used when referring to particular
        implementations or algorithms, for example, "the
        Boehm--Demers--Weiser *collector*".

    GB

        .. see:: :term:`gigabyte`.

    GC

        .. see:: :term:`garbage collection`.

    General Protection Fault

        .. aka:: *GPF*.

        A General Protection Fault on the Windows platforms is the
        equivalent of a :term:`segmentation violation` on Unix.

    generation

        A generation is a set of :term:`objects` of similar
        *age*.

        A :term:`generational garbage collector <generational garbage
        collection>` will typically divide the set of all objects into
        generations, and :term:`condemn <condemned set>` all the
        objects in a generation together. Rather than allowing whole
        generations to age, the :term:`collector (1)` can
        :term:`promote <promotion>` objects into older generations as
        they survive successive :term:`collection cycles <collection
        cycle>`.

        New objects are usually allocated in the youngest or
        :term:`nursery generation`, but if we know that particular
        objects will be long-lived, we might want to allocate them
        directly in an older generation. Thus, more loosely, a
        generation is a set of objects which have similar expected
        :term:`lifetimes`.

        .. seealso:: :term:`bucket`.

        .. mps:specific::

            The :term:`client program` specifies the generational
            structure of a :term:`pool` (or group of pools) using a
            :term:`generation chain`. See :ref:`topic-collection`.

    generation chain

        .. mps:specific:: 

            A data structure that specifies the structure of the
            :term:`generations` in a :term:`pool` (or group of pools).
            See :ref:`topic-collection`.

    generation scavenging

        .. see:: :term:`generational garbage collection`.

    generational garbage collection

        .. aka:: *generation scavenging*.

        Generational garbage collection is :term:`tracing garbage
        collection` that makes use of the :term:`generational
        hypothesis`. :term:`Objects` are gathered together in
        :term:`generations`. New objects are allocated in
        the *youngest* or *nursery* generation, and :term:`promoted
        <promotion>` to *older* generations if they survive. Objects
        in older generations are :term:`condemned <condemned set>`
        less frequently, saving CPU time.

        It is typically rare for an object to refer to a younger
        object. Hence, objects in one generation typically have few
        :term:`references` to objects in younger
        generations. This means that the :term:`scanning <scan>` of
        old generations in the course of collecting younger
        generations can be done more efficiently by means of
        :term:`remembered sets`.

        In some purely functional languages (that is, without update),
        all references are backwards in time, in which case remembered
        sets are unnecessary.

        .. seealso:: :term:`remembered set`.

        .. mps:specific::

            The :ref:`pool-amc` and :ref:`pool-amcz` pool classes
            support generational garbage collection.

    generational hypothesis

        .. aka:: *infant mortality*.

        *Infant mortality* or *the generational hypothesis* is the
        observation that, in most cases, young :term:`objects` are
        much more likely to :term:`die <dead>` than old objects.

        Strictly, the hypothesis is that the probability of death as a
        function of age falls faster than exponential decay (inverse
        hyper-exponential), but this strict condition is not always
        required for techniques such as :term:`generational garbage
        collection` to be useful.

    gigabyte

        .. aka:: *GB*.

        A gigabyte is 1024 :term:`megabytes`, or 1073741824
        :term:`bytes (1)`.

        See :term:`byte (1)` for general information on this and
        related quantities.

    good fit

        The class of :term:`allocation policies` which approximate
        :term:`best fit`. Strict best fit may be costly to implement
        (depending on the details of the :term:`allocation
        mechanism`), so some implementors approximate it, choosing a
        block which is close in size to the allocation request.

        .. seealso:: :term:`best fit`, :term:`allocation policy`, :term:`next fit`, :term:`worst fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    GPF

        .. see:: :term:`General Protection Fault`.

    grain

        The grain of a platform is the smallest :term:`alignment` that
        is sufficient to accommodate all data accesses on that
        platform. Often this is a :term:`word` or a small multiple of
        a word. Double precision floating point numbers often have the
        strictest alignment requirements.

        .. seealso:: :term:`alignment`, :term:`word`.

    graph

        A graph is a set of :term:`nodes` together with a set
        of :term:`edges` connecting nodes.

        If the edges have direction like arrows (for example,
        :term:`references` in a graph of :term:`objects`), then the
        graph is said to be a *directed graph*.

        .. figure:: ../diagrams/graph.svg
            :align: center
            :alt: Ten white circles (the nodes of this graph), some of them joined by arrows (the edges of the graph). Most of the edges point in one direction, but one edge points both ways. Seven of the nodes are connected in one component, and three in another.

            Directed graph.

        .. relevance::

            Graphs are used to model :term:`reachability <reachable>`
            for :term:`tracing garbage collection`. The
            :term:`objects` are considered to form a graph, with the
            nodes of the graph being the objects and the edges of the
            graph being the references from one object to another.
            Usually, there is a single, distinguished :term:`root` to
            which the :term:`mutator` has *direct* access, and the
            nodes strongly connected to it are the reachable modes.

    gray
    grey

        In a :term:`tri-color marking` scheme, gray :term:`objects`
        are objects that are proved or assumed (see
        :term:`generational <generational garbage collection>` and
        :term:`condemn <condemned set>`) to be :term:`reachable`, but
        have not yet been :term:`scanned <scan>`.

        More precisely, gray objects have been noted reachable, but
        must still be visited by the :term:`collector (2)` in order to
        process their children.

        .. similar:: :term:`gray list`.

        .. opposite:: :term:`black`, :term:`white`.

    gray list
    grey list

        The gray list is the set of :term:`objects` that a
        :term:`tracing garbage collector <tracing garbage collection>`
        has noted :term:`reachable`, but hasn't :term:`scanned <scan>`
        yet.

        The gray list is so called because it corresponds to the set
        of :term:`gray` objects in the :term:`tri-color marking` model
        of graph tracing. The gray list changes as the garbage
        collector progresses.

        Each gray object is :term:`scanned <scan>`, and all
        :term:`white` objects referred to by it become gray and are
        added to the list. Scanning a gray object turns it
        :term:`black`. When the gray list is empty, the tracing is
        finished, and white objects may be :term:`reclaimed
        <reclaim>`.

        The representation of the gray list is a key part of garbage
        collector design. The size of the list is potentially
        proportional to the size of the :term:`heap`, and the
        operation of finding the next gray object to scan must be
        cheap.

        .. seealso:: :term:`Cheney scan`.

.. _glossary-c:

=============================
Memory Management Glossary: C
=============================

.. include:: alphabet.txt

.. glossary::

    C89

        .. see:: :term:`C90`.

    C90

        .. aka:: *C89*.

        A revision of the ANSI/ISO Standard for the :term:`C`
        programming language. Although more than twenty years old, it
        remains the only form of Standard C that is supported by all
        the major compilers, including Microsoft Visual C.

        .. mps:specific::

            The public interface conforms to this standard. See
            :ref:`topic-interface`.

        .. bibref:: :ref:`ISO/IEC 9899:1990 <C1990>`.

    C99

        A revision of the ANSI/ISO Standard for C the :term:`C`
        programming language.

        .. mps:specific::

            :term:`Keyword arguments` can be conveniently passed to
            functions using C99's compound literal syntax. See
            :ref:`topic-keyword`.

        .. bibref:: :ref:`ISO/IEC 9899:1999 <C1999>`.

    cache (1)

        .. aka:: *memory cache*, *cache memory*.

        A processor's memory cache is a small piece of fast, but more
        expensive memory, usually :term:`static memory (1)`, used for
        copies of parts of :term:`main memory`. The cache is
        automatically used by the processor for fast access to any
        data currently :term:`resident` there. Access to the cache
        typically takes only a few processor clock cycles, whereas
        access to :term:`main memory` may take tens or even hundreds
        of cycles.

        What part of main memory is resident in a cache, and the
        mechanisms by which it is kept consistent, are quite varied.
        See :term:`cache policy`.

        Some systems have more than one level of cache. "Level 1
        cache" is the fastest, smallest :term:`storage level`, "level
        2" the next fastest, and so on.

        .. seealso:: :term:`storage hierarchy`, :term:`cache (2)`.

    cache (2)

        A cache is any small, fast piece of :term:`memory (1)`, used
        for copies of data that normally reside in a larger, slower
        piece of storage. The cache is used to speed up access to data
        :term:`resident` in the slower storage.

        In a typical cache, recently used data is :term:`resident` in
        the cache (although the details of this depend on the
        :term:`cache policy`). A :term:`cache (1)` is the most common
        example of a cache(2).

        .. seealso:: :term:`storage hierarchy`.

    cache memory

        .. see:: :term:`cache (1)`.

    cache policy

        Any :term:`cache (3) <caching (3)>` uses a *cache policy* to
        decide which data to store. A cache policy is an attempt to
        predict the future, so that the cache will provide swift
        responses to future requests.

        Cache policy may be implemented in hardware, software, or a
        combination of both. Some systems allow programs to influence
        cache policy, by giving hints or directions about future use
        of data.

        There are three main aspects of cache behavior which the cache
        policy can affect:

        1. Fetch policy. This determines which data is fetched into
           the cache, usually as a result of receiving a request for
           data that isn't cached.

        2. Eviction policy. This determines which data is discarded
           from the cache to provide space for newly fetched data.

        3. Write policy This determines how and when modifications to
           cached data are synchronized with the underlying storage.

        .. seealso:: :term:`cache (1)`, :term:`cache (2)`, :term:`cache (3) <caching (3)>`.

        .. bibref:: :ref:`Baker (1991) <BAKER91>`, :ref:`Wilson et al. (1992) <WLM92>`, :ref:`Zorn (1991) <ZORN91>`.

    caching (3)

        .. aka:: *memoization*, *tabling*.

        *Caching* is a heuristic that stores answers to questions
        asked in the past in a *cache* or a *table*, in order that
        they may be more quickly answered in the future. This process
        is also called memoization and tabling (by the :term:`Prolog`
        community).

        A "look-ahead cache" attempts to store answers to questions
        that will be asked soon. A :term:`cache (2)` is a common
        example of a cache(3).

    cactus stack

        .. aka:: *spaghetti stack*.

        A cactus stack is a :term:`stack` with branches. When
        diagrammed, its shape resembles that of a `saguaro cactus
        <http://en.wikipedia.org/wiki/Saguaro>`_.

        In languages that support :term:`continuations`,
        :term:`activation records` can have :term:`indefinite extent`.
        One technique for implementing continuations is not to copy
        the activation records that are captured, but rather to create
        a fork in the stack below the captured :term:`stack frames`,
        so that new frames appear as a parallel branch. Often the
        process of forking is done lazily: captured frames are only
        duplicated if they are modified.

    card

        A card is a division of memory, all cards being of equal size
        (in a particular area of discourse). A card is usually bigger
        than a :term:`word` and smaller than a :term:`page`. Cards are
        used in a technique called :term:`card marking` whereby
        :term:`dirty bits` (which record which portions of old
        generations have been written into) are maintained for each
        card. Often the use of cards will also entail the use of a
        :term:`crossing map`.

    card marking

        A technique for managing :term:`pointer` :term:`stores (1)`
        into old :term:`generations` (which in turn is used to track
        :term:`inter-generational pointers`). Each generation is
        divided into a number of equal-sized :term:`cards`, and when a
        generation is written into, the particular card written to is
        recorded (often by using a :term:`bitmap`). Subsequently, when
        :term:`scanning <scan>` an older generation in order to
        collect a younger generation, only the recorded cards (in the
        old generation) need to be scanned.

        .. seealso:: :term:`generational garbage collection`.

        .. bibref:: :ref:`Azagury et al. (1998) <AKPY98>`, :ref:`Hosking & Hudson (1993) <HH93>`, :ref:`Sobalvarro (1988) <SOBALVARRO88>`.

    cell

        .. see:: :term:`object`.

    Cheney collector

        .. aka:: *Cheney scan*.

        A Cheney collector uses the :term:`tospace` of a
        :term:`two-space collector` as a queue of objects remaining to
        be :term:`scanned <scan>`, thus eliminating the need for
        recursion when :term:`tracing <trace>` the :term:`graph` of
        :term:`objects`.

        .. bibref:: :ref:`Cheney (1970) <CHENEY70>`.

    Cheney scan

        .. see:: :term:`Cheney collector`.

    clamped state

        .. mps:specific::

            One of the three states an :term:`arena` can be in (the
            others being the :term:`unclamped state` and the
            :term:`parked state`). In the clamped state, no object
            motion occurs and the staleness of :term:`location
            dependencies` does not change. However, a :term:`garbage
            collection` may be in progress. Call
            :c:func:`mps_arena_clamp` to put an arena into the clamped
            state.

    client arena

        .. mps:specific::

            An :term:`arena class` that gets its :term:`memory (2)`
            from the :term:`client program`. See
            :ref:`topic-arena-client`.

    client object

        .. mps:specific::

            A :term:`formatted object` that contains data from the
            :term:`client program`. One of three types of formatted
            objects, the other two being :term:`forwarding objects`
            and :term:`padding objects`.

    client pointer

        .. mps:specific::

            A pointer to the first word in an object that's not part
            of the :term:`in-band header`. See
            :ref:`topic-format-headers`.

        .. seealso:: :term:`base pointer`.

    client program

        .. see:: :term:`mutator`

    closure

        A closure is a function or procedure that is saved along with
        the current bindings from enclosing blocks for later
        invocation.

        Some programming languages, such as :term:`ALGOL`, permit
        nested blocks to access the local variables of enclosing
        blocks. :term:`Lisp`-like languages further permit such an
        inner block (in particular a function or procedure) to be
        saved for later invocation. The act of saving such an inner
        block along with the current bindings of variables in the
        enclosing blocks that are referenced by the inner block, is
        called *closing over* or *capturing* those variables. The
        object created is termed *a closure*. A closure is invoked
        just like the function from which it was built, passing
        whatever parameters the function accepts, but when the
        function executes, the variables that belong to enclosing
        blocks will have the bindings that were in effect when the
        closure was created.

        .. relevance::

            A closure is typically implemented by saving both the
            function and any :term:`activation records` that contain
            variables referenced by the function. The closure creates
            additional implicit :term:`references` to the bindings
            closed over and hence must be accounted for in any memory
            management scheme. The closure itself is an object that
            must be managed and may have either :term:`dynamic extent`
            or :term:`indefinite extent` depending on whether it is
            only used by inner blocks of the creating block or passed
            out of the creating block.

        .. seealso:: :term:`continuation`.

    coalesce

        Coalescing is the act of merging two adjacent :term:`free
        blocks`.

        Coalescing reduces :term:`external fragmentation`, but is not
        totally effective.

        Coalescing can be done as soon as blocks are freed, or it can
        be deferred until some time later (known as :term:`deferred
        coalescing`), or it might not be done at all.

        :ref:`Wilson et al. (1995) <WIL95>` has details about
        fragmentation, and which coalescing strategies are effective
        under what circumstances.

    cold end

        .. opposite:: :term:`hot end`

        A :term:`control stack` has two ends: the oldest items are at
        the *cold end* and the newest items are at the *hot end*.
        Sometimes the cold end is called the "bottom" of the stack,
        but that is misleading when the stack grows downwards, as it
        does on common computing platforms.

        .. mps:specific::

            In order for the MPS to be able to :term:`scan`
            :term:`references` on the stack, the :term:`client
            program` must pass the location of the cold end of the
            stack (or the part of the stack that might contain
            references to memory managed by the MPS) to
            :c:func:`mps_root_create_thread`.

    collect

        An :term:`object` is collected when it is :term:`reclaimed` by
        a :term:`garbage collector`.

        .. similar:: :term:`reclaim`.

    collection

        .. see:: :term:`collection cycle`.

    collection cycle

        .. aka:: *collection*.

        A collection cycle is a single complete execution of a
        :term:`tracing garbage collection` algorithm.

        Each collection cycle includes (not necessarily in strict
        order) choosing a :term:`condemned set`; :term:`scanning
        <scan>` :term:`roots` and :term:`objects` that have not been
        condemned; :term:`tracing <trace>` the object graph to find
        all condemned objects that are :term:`reachable`; and
        :term:`reclaiming <reclaim>` those that were not reachable.

        In non-incremental garbage collection, the :term:`mutator`
        pauses at the start of a collection cycle and cannot continue
        until it is complete. In :term:`incremental <incremental
        garbage collection>` and :term:`parallel <parallel garbage
        collection>` garbage collection, a collection cycle can be
        interleaved with, or simultaneous to, mutator activity.

    collector (1)

        .. see:: :term:`garbage collector`.

    collector (2)

        In a :term:`garbage-collected <garbage collection>` system,
        the part that executes the garbage collection code, which
        discovers unused :term:`memory (1)` and :term:`reclaims` it.

        For purposes of describing :term:`incremental garbage
        collection`, the system is divided into the :term:`mutator`
        and the *collector*. These can be separate threads of
        computation, or interleaved within the same thread.

        .. historical::

            This term is due to :ref:`Dijkstra et al. (1976)
            <DLMSS76>`.

        .. opposite:: :term:`mutator`.

    color
    colour

        In a :term:`tri-color marking` scheme, each :term:`node` has a
        one of three colors: :term:`black`, :term:`white`, or
        :term:`gray`. In a :term:`treadmill`, nodes may also be
        colored :term:`off-white`.

    commit limit

        .. mps:specific::

            The commit limit is a limit on the :term:`committed
            <mapped>` :term:`memory (2)` that the :term:`arena` will
            obtain from the operating system. It can be changed by
            calling :c:func:`mps_arena_commit_limit_set`.

    committed (1)

        .. see:: :term:`mapped`.

    committed (2)

        .. mps:specific::

            A block has been *committed* if it is fully initialized
            and is under the management of the MPS, as opposed to a
            block that is merely *reserved*. See
            :ref:`topic-allocation-point-protocol`.

    compactifying

        .. see:: :term:`compaction`.

    compaction

        .. aka:: *compactifying*.

        Compaction is the process of :term:`moving <moving garbage
        collector>` :term:`live` :term:`objects` to eliminate
        :term:`dead` space between them. Some people call this
        *compactifying*, to distinguish it from techniques for
        compressing data structures.

        Compaction is used to avoid :term:`external fragmentation` and
        to increase :term:`locality of reference`.

    composite object

        In the :term:`PostScript` language, *composite objects* are
        the :term:`boxed` objects.

        Unlike a :term:`simple object`, the main data (what PostScript
        calls *the value*) in a composite object are stored
        separately, in :term:`VM (2)`. Several composite objects can
        share the same value.

        .. similar:: :term:`boxed`.

        .. opposite:: :term:`simple object`.

    comprehensive

        A :term:`collector (1)` is *comprehensive* if all
        :term:`garbage` (or, all :term:`unreachable` :term:`objects`)
        is :term:`reclaimed` in one :term:`collection cycle`.

        .. seealso:: :term:`garbage collection`.

    concurrent garbage collection

        .. see:: :term:`parallel garbage collection`.

    condemned set

        .. aka:: *threatened set*.

        *Condemned* :term:`objects` are those which are
        candidates for :term:`recycling <recycle>` within a
        :term:`collection cycle`.

        At the start of a collection cycle, the :term:`collector (1)`
        may choose to condemn some objects (the *condemned set* or
        *threatened set*) but not to condemn others (the :term:`immune
        set`). Objects that are not condemned are assumed to be
        :term:`live` and behave as :term:`roots` for the
        purposes of that collection cycle.

        Many simple :term:`tracing garbage collection` algorithms
        begin by condemning all objects, but :term:`generational
        garbage collectors <generational garbage collection>` will
        condemn individual :term:`generations` or
        combinations of generations. Often young generations are
        condemned but older ones are not, because objects in older
        generations are less likely to have become
        :term:`unreachable`.

        In collectors using :term:`tri-color marking`, at the start of
        a collection cycle the condemned set is exactly the set of
        objects that the collector colors :term:`white`.

        .. opposite:: :term:`immune set`.

    connected

        :term:`Objects` are connected if and only if one
        contains a :term:`reference` to the other.

        .. seealso:: :term:`graph`.

    cons (1)

        In :term:`Lisp`, ``cons`` is a primitive operation creating a
        list element (from English "CONStruct"). By extension, a
        *cons* is the element created.

        .. link::

            `Function CONS in the Common Lisp HyperSpec <http://www.lispworks.com/documentation/lw60/CLHS/Body/f_cons.htm>`_.

    cons (2)

        .. see:: :term:`allocate`.

    conservative garbage collection

        In conservative :term:`garbage collection`, the layout of
        :term:`objects` and :term:`roots` is not
        known, instead the :term:`collector (1)` assumes that any
        field that looks like a :term:`pointer` *might* be a
        :term:`reference`.

        Conservative collectors can work with programs where
        information about the :term:`memory (2)` layout is not
        available, because, for example, the language doesn't support
        :term:`garbage collection`.

        A conservative collector doesn't need to know the
        :term:`format` of the objects, it just needs some idea of
        where the object boundaries are. It regards any field value
        that looks like a pointer to an object (or, sometimes, into
        the middle of one), as preventing the :term:`recycling
        <recycle>` of that object. It can't :term:`move <moving
        garbage collector>` objects, because then the references to
        the moved objects would need to be updated, and such
        :term:`ambiguous references` must not be modified, in case
        they weren't pointers after all. Therefore, conservative
        collectors are usually :term:`mark-sweep collectors
        <mark-sweep>`.

        Because references are ambiguous, some objects may be retained
        despite being actually :term:`unreachable`. In practice, this
        happens rarely, and refinements such as :term:`black-listing
        <blacklisting>` can further reduce the odds.

        .. opposite:: :term:`exact garbage collection`.

        .. seealso:: :term:`ambiguous root`, :term:`semi-conservative garbage collection`, :term:`interior pointer`.

        .. bibref:: :ref:`Boehm & Weiser (1988) <BW88>`, :ref:`Boehm (1993) <BOEHM93>`.

    constant root

        .. mps:specific::

            A :term:`root` that the :term:`client program` promises
            not change after it is registered, by specifying the
            :term:`root mode` :c:macro:`MPS_RM_CONST` when calling a
            registration function such as :c:func:`mps_root_create`.

    constructor (1)

        A constructor is a function or method that :term:`allocates` and initializes an :term:`object`.

        .. opposite:: :term:`destructor (1)`.

    constructor (2)

        In :term:`C++`, a *constructor* is a member function that is
        used to initialize a newly-:term:`allocated` object.

        The actual allocation of :term:`memory (2)` is performed by
        ``operator new`` or the compiler (for :term:`static <static
        allocation>` and :term:`stack allocation`), and the new
        :term:`block` is then passed to the appropriate constructor.

        .. seealso:: :term:`destructor (2)`.

    continuation

        A continuation is the data required to restore an execution
        context after invocation of another context, typically as a
        subroutine.

        .. relevance::

            If continuations can be represented as first-class
            objects, as in :term:`Scheme`, the execution contexts can
            no longer be stored on a :term:`stack`, instead, (at least
            some) :term:`activation records` have
            to be :term:`heap-allocated <heap allocation>`.

        .. seealso:: :term:`closure`.

    control stack

        .. aka:: *activation stack*, *execution stack*.

        A :term:`stack` that stores :term:`activation records`, particularly subroutine return
        information, is known as a *control stack*.

        Typically the control stack is supported and used by the
        hardware architecture and the operating system, limiting the
        types and sizes of :term:`objects` that can be stored
        on it. Often, only one type of object, a :term:`stack frame`,
        is permitted, and the layout of that is defined by the
        hardware architecture.

        .. relevance::

            Theoretically, a control stack is simply an array of
            activation records, and hence just another object managed
            by the :term:`memory manager`. In practice, the control
            stack is central to the performance of the hardware
            architecture and may require special treatment. In
            particular, it may not be accessible as ordinary
            :term:`memory (2)`, or it may have its own :term:`cache
            (2)` with specific updating requirements.

        .. similar:: :term:`stack`.

        .. seealso:: :term:`cold end`, :term:`data stack`, :term:`hot end`.

    cool

        .. mps:specific::

            A :term:`variety` in which most MPS functions
            :term:`assert <assertion>` that their data structures are
            valid, even functions on the :term:`critical path`. See
            :ref:`guide-build`. Compare :term:`hot` and :term:`rash`.

    copying garbage collection

        .. aka:: *scavenging garbage collection*.

        Copying garbage collection is a kind of :term:`tracing garbage
        collection` that operates by :term:`relocating <relocation>`
        :term:`reachable` :term:`objects` (this is sometimes called
        *scavenging*) and then :term:`reclaiming <reclaim>` objects
        that are left behind, which must be :term:`unreachable` and
        therefore :term:`dead`.

        A copying garbage collection relies on being able to find and
        correct all :term:`references` to copied objects.

        .. figure:: ../diagrams/copying.svg
            :align: center
            :alt: Diagram: Copying garbage collection.

            Copying garbage collection.

        .. similar:: :term:`moving <moving garbage collector>`.

        .. seealso:: :term:`broken heart`, :term:`forwarding pointer`, :term:`two-space collector`.

        .. mps:specific::

            The :ref:`pool-amc` pool class implements copying garbage
            collection (more precisely, :term:`mostly-copying garbage
            collection`).

    core

        A historical synonym for :term:`main memory`, deriving from
        the *cores* or ferrite rings which were once the main
        technology used to implement it.

        .. similar:: :term:`main memory`.

    creation space

        In :term:`generational garbage collection`, when
        :term:`generations` are divided into
        :term:`buckets`, the creation space is where new
        :term:`objects` are created in each generation.

        This term is sometimes used as a synonym for :term:`nursery space`.

        .. opposite:: :term:`aging space`.

        .. seealso:: :term:`generational garbage collection`.

    critical path

        .. mps:specific::

            The sequence of operations on which the MPS spends the
            majority of its time, consisting of :term:`scanning
            <scan>`, :term:`fixing <fix>`, :term:`marking` and
            :term:`copying <copying garbage collection>`. See
            :ref:`design-critical-path`.

    crossing map

        Where :term:`memory (2)` has already been divided into some
        fixed-sized unit (for example, :term:`pages` or
        :term:`cards`), a crossing map records where
        :term:`objects` lie across the boundaries of the
        fixed-sized units. In other words, which fixed-sized units do
        not start with the beginning of an object.

        A system which implements :term:`remembered sets` by
        :term:`page marking` or :term:`card marking` needs to scan all
        the :term:`pointers` in the page or card. If the system can
        not :term:`scan` partial objects (or requires information in
        the object :term:`header <in-band header>` in order to scan a
        partial object), a crossing map is necessary to find the
        beginning of the first object in the unit.

        .. relevance::

            In a sense, a crossing map is an optimization of
            :term:`tagged architecture`. It represents the minimum
            information necessary to determine how to interpret any
            word of memory.

    cyclic data structure

        A data structure is cyclic if some of its :term:`references`
        form a loop; that is, there's an :term:`object` that can be
        reached by following references from itself.

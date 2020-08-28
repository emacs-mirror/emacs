.. _bibliography:

Bibliography
************

* .. _AD97:

  Ole Agesen, David L. Detlefs. 1997.  "`Finding References in Java Stacks <http://www-plan.cs.colorado.edu/diwan/class-papers/finding-references-in-java.pdf>`_". Sun Labs. OOPSLA97 Workshop on Garbage Collection and Memory Management.

  .. admonition:: Abstract

    Exact garbage collection for the strongly-typed Java language may
    seem straightforward. Unfortunately, a single pair of bytecodes in
    the Java Virtual Machine instruction set presents an obstacle that
    has thus far not been discussed in the literature. We explain the
    problem, outline the space of possible solutions, and present a
    solution utilizing bytecode-preprocessing to enable exact garbage
    collection while maintaining compatibility with existing compiled
    Java class files.

* .. _ADM98:

  Ole Agesen, David L. Detlefs, J. Eliot B. Moss. 1998.  "`Garbage Collection and Local Variable Type-precision and Liveness in Java Virtual Machines  <http://pdf.aminer.org/000/542/332/garbage_collection_and_local_variable_type_precision_and_liveness_in.pdf>`_". ACM. Proceedings of the ACM SIGPLAN '98 conference on Programming language design and implementation, pp. 269--279.

  .. admonition:: Abstract

    Full precision in garbage collection implies retaining only those
    heap allocated objects that will actually be used in the future.
    Since full precision is not computable in general, garbage
    collectors use safe (i.e., conservative) approximations such as
    reachability from a set of root references. Ambiguous roots
    collectors (commonly called "conservative") can be overly
    conservative because they overestimate the root set, and thereby
    retain unexpectedly large amounts of garbage. We consider two more
    precise collection schemes for Java virtual machines (JVMs). One
    uses a type analysis to obtain a type-precise root set (only those
    variables that contain references); the other adds a live variable
    analysis to reduce the root set to only the live reference
    variables. Even with the Java programming language's strong
    typing, it turns out that the JVM specification has a feature that
    makes type-precise root sets difficult to compute. We explain the
    problem and ways in which it can be solved.

    Our experimental results include measurements of the costs of the
    type and liveness analyses at load time, of the incremental
    benefits at run time of the liveness analysis over the
    type-analysis alone, and of various map sixes and counts. We find
    that the liveness analysis often produces little or no improvement
    in heap size, sometimes modest improvements, and occasionally the
    improvement is dramatic. While further study is in order, we
    conclude that the main benefit of the liveness analysis is
    preventing bad surprises.

* .. _AEL88:

  Andrew Appel, John R. Ellis, Kai Li. 1988.  "`Real-time Concurrent Collection on Stock Multiprocessors <http://apotheca.hpl.hp.com/ftp/pub/compaq/SRC/research-reports/SRC-025.pdf>`_". ACM, SIGPLAN. ACM PLDI 88, SIGPLAN Notices 23, 7 (July 88), pp. 11--20.

  .. admonition:: Abstract

    We've designed and implemented a copying garbage-collection
    algorithm that is efficient, real-time, concurrent, runs on
    commercial uniprocessors and shared-memory multiprocessors, and
    requires no change to compilers. The algorithm uses standard
    virtual-memory hardware to detect references to "from space"
    objects and to synchronize the collector and mutator threads.
    We've implemented and measured a prototype running on SRC's
    5-processor Firefly. It will be straightforward to merge our
    techniques with generational collection. An incremental,
    non-concurrent version could be implemented easily on many
    versions of Unix.

* .. _APPLE94:

  Apple Computer, Inc. 1994. *Inside Macintosh: Memory*. Addison-Wesley. ISBN 0-201-63240-3.

  .. admonition:: Abstract

    Inside Macintosh: Memory describes the parts of the Macintosh®
    Operating System that allow you to directly allocate, release, or
    otherwise manipulate memory. Everyone who programs Macintosh
    computers should read this book.

    Inside Macintosh: Memory shows in detail how your application can
    manage the memory partition it is allocated and perform other
    memory-related operations. It also provides a complete technical
    reference for the Memory Manager, the Virtual Memory Manager, and
    other memory-related utilities provided by the system software.

* .. _ATTARDI94:

  Giuseppe Attardi & Tito Flagella. 1994.  "`A Customisable Memory Management Framework <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.50.257&rep=rep1&type=pdf>`_". TR-94-010.

  .. admonition:: Abstract

    Memory management is a critical issue for many large
    object-oriented applications, but in C++ only explicit memory
    reclamation through the delete operator is generally available. We
    analyse different possibilities for memory management in C++ and
    present a dynamic memory management framework which can be
    customised to the need of specific applications. The framework
    allows full integration and coexistence of different memory
    management techniques. The Customisable Memory Management (CMM) is
    based on a primary collector which exploits an evolution of
    Bartlett's mostly copying garbage collector. Specialised
    collectors can be built for separate memory heaps. A Heap class
    encapsulates the allocation strategy for each heap. We show how to
    emulate different garbage collection styles or user-specific
    memory management techniques. The CMM is implemented in C++
    without any special support in the language or the compiler. The
    techniques used in the CMM are general enough to be applicable
    also to other languages.

* .. _AFI98:

  Giuseppe Attardi, Tito Flagella, Pietro Iglio. 1998.  "`A customisable memory management framework for C++ <ftp://ftp.di.unipi.it/pub/Papers/attardi/SPE.ps.gz>`_". Software -- Practice and Experience. 28(11), 1143--1183.

  .. admonition:: Abstract

    Automatic garbage collection relieves programmers from the burden
    of managing memory themselves and several techniques have been
    developed that make garbage collection feasible in many
    situations, including real time applications or within traditional
    programming languages. However optimal performance cannot always
    be achieved by a uniform general purpose solution. Sometimes an
    algorithm exhibits a predictable pattern of memory usage that
    could be better handled specifically, delaying as much as possible
    the intervention of the general purpose collector. This leads to
    the requirement for algorithm specific customisation of the
    collector strategies. We present a dynamic memory management
    framework which can be customised to the needs of an algorithm,
    while preserving the convenience of automatic collection in the
    normal case. The Customisable Memory Manager (CMM) organises
    memory in multiple heaps. Each heap is an instance of a C++ class
    which abstracts and encapsulates a particular storage discipline.
    The default heap for collectable objects uses the technique of
    mostly copying garbage collection, providing good performance and
    memory compaction. Customisation of the collector is achieved
    exploiting object orientation by defining specialised versions of
    the collector methods for each heap class. The object oriented
    interface to the collector enables coexistence and coordination
    among the various collectors as well as integration with
    traditional code unaware of garbage collection. The CMM is
    implemented in C++ without any special support in the language or
    the compiler. The techniques used in the CMM are general enough to
    be applicable also to other languages. The performance of the CMM
    is analysed and compared to other conservative collectors for
    C/C++ in various configurations.

* .. _AKPY98:

  Alain Azagury, Elliot K. Kolodner, Erez Petrank, Zvi Yehudai. 1998.  "`Combining Card Marking with Remembered Sets: How to Save Scanning Time <http://pdf.aminer.org/000/465/100/combining_card_marking_with_remembered_sets_how_to_save_scanning.pdf>`_". ACM. ISMM'98 pp. 10--19.

  .. admonition:: Abstract

    We consider the combination of card marking with remembered sets
    for generational garbage collection as suggested by Hosking and
    Moss. When more than two generations are used, a naive
    implementation may cause excessive and wasteful scanning of the
    cards and thus increase the collection time. We offer a simple
    data structure and a corresponding algorithm to keep track of
    which cards need be scanned for which generation. We then extend
    these ideas for the Train Algorithm of Hudson and Moss. Here, the
    solution is more involved, and allows tracking of which card
    should be scanned for which car-collection in the train.

* .. _BAKER77:

  Henry G. Baker, Carl Hewitt. 1977.  "`The Incremental Garbage Collection of Processes <http://home.pipeline.com/~hbaker1/Futures.html>`_". ACM. SIGPLAN Notices 12, 8 (August 1977), pp. 55--59.

  .. admonition:: Abstract

    This paper investigates some problems associated with an argument
    evaluation order that we call "future" order, which is different
    from both call-by-name and call-by-value. In call-by-future, each
    formal parameter of a function is bound to a separate process
    (called a "future") dedicated to the evaluation of the
    corresponding argument. This mechanism allows the fully parallel
    evaluation of arguments to a function, and has been shown to
    augment the expressive power of a language.

    We discuss an approach to a problem that arises in this context:
    futures which were thought to be relevant when they were created
    become irrelevant through being ignored in the body of the
    expression where they were bound. The problem of irrelevant
    processes also appears in multiprocessing problem-solving systems
    which start several processors working on the same problem but
    with different methods, and return with the solution which
    finishes first. This "parallel method strategy" has the drawback
    that the processes which are investigating the losing methods must
    be identified, stopped, and reassigned to more useful tasks.

    The solution we propose is that of garbage collection. We propose
    that the goal structure of the solution plan be explicitly
    represented in memory as part of the graph memory (like Lisp's
    heap) so that a garbage collection algorithm can discover which
    processes are performing useful work, and which can be recycled
    for a new task. An incremental algorithm for the unified garbage
    collection of storage and processes is described.

* .. _BAKER78:

  Henry G. Baker. 1978.  "`List Processing in Real Time on a Serial Computer <http://home.pipeline.com/~hbaker1/RealTimeGC.html>`_". ACM. Communications of the ACM 21, 4 (April 1978), pp. 280--294.

  .. admonition:: Abstract

    A real-time list processing system is one in which the time
    required by the elementary list operations (e.g. CONS, CAR, CDR,
    RPLACA, RPLACD, EQ, and ATOM in LISP) is bounded by a (small)
    constant. Classical implementations of list processing systems
    lack this property because allocating a list cell from the heap
    may cause a garbage collection, which process requires time
    proportional to the heap size to finish. A real-time list
    processing system is presented which continuously reclaims
    garbage, including directed cycles, while linearizing and
    compacting the accessible cells into contiguous locations to avoid
    fragmenting the free storage pool. The program is small and
    requires no time-sharing interrupts, making it suitable for
    microcode. Finally, the system requires the same average time, and
    not more than twice the space, of a classical implementation, and
    those space requirements can be reduced to approximately classical
    proportions by compact list representation. Arrays of different
    sizes, a program stack, and hash linking are simple extensions to
    our system, and reference counting is found to be inferior for
    many applications.

* .. _BAKER79:

  Henry G. Baker. 1979.  "`Optimizing Allocation and Garbage Collection of Spaces <http://home.pipeline.com/~hbaker1/OptAlloc.html>`_". In Winston and Brown, eds. *Artificial Intelligence: An MIT Perspective.* MIT Press.

  .. admonition:: Abstract

    MACLISP, unlike some other implementations of LISP, allocates
    storage for different types of objects in noncontiguous areas
    called "spaces". These spaces partition the active storage into
    disjoint areas, each of which holds a different type of object.
    For example, "list cells" are stored in one space, "full-word
    integers" reside in another space, "full-word floating point
    numbers" in another, and so on.

    Allocating space in this manner has several advantages. An
    object's type can easily be computed from a pointer to it, without
    any memory references to the object itself. Thus, the LISP
    primitive ATOM(x) can easily compute its result without even
    paging in x. Another advantage is that the type of an object does
    not require any storage within the object, so that arithmetic with
    hardware data types such as full-word integers can use hardware
    instructions directly.

    There are problems associated with this method of storage and type
    management, however. When all data types are allocated from the
    same heap, there is no problem with varying demand for the
    different data types; all data types require storage from the same
    pool, so that only the total amount of storage is important. Once
    different data types must be allocated from different spaces,
    however, the relative sizes of the spaces becomes important.

* .. _BAKER91:

  Henry G. Baker. 1991.  "`Cache-Conscious Copying Collectors <http://home.pipeline.com/~hbaker1/CacheCGC.html>`_". OOPSLA'91/GC'91 Workshop on Garbage Collection.

  .. admonition:: Abstract

    Garbage collectors must minimize the scarce resources of cache
    space and off-chip communications bandwidth to optimize
    performance on modern single-chip computer architectures.
    Strategies for achieving these goals in the context of copying
    garbage collection are discussed. A multi-processor
    mutator/collector system is analyzed. Finally, the Intel 80860XP
    architecture is studied.

* .. _BAKER92A:

  Henry G. Baker. 1992.  "`Lively Linear Lisp -- 'Look Ma, No Garbage!' <http://home.pipeline.com/~hbaker1/LinearLisp.html>`_". ACM. SIGPLAN Notices 27, 8 (August 1992), pp. 89--98.

  .. admonition:: Abstract

    Linear logic has been proposed as one solution to the problem of
    garbage collection and providing efficient "update-in-place"
    capabilities within a more functional language. Linear logic
    conserves accessibility, and hence provides a "mechanical
    metaphor" which is more appropriate for a distributed-memory
    parallel processor in which copying is explicit. However, linear
    logic's lack of sharing may introduce significant inefficiencies
    of its own.

    We show an efficient implementation of linear logic called "Linear
    Lisp" that runs within a constant factor of non-linear logic. This
    Linear Lisp allows RPLACX operations, and manages storage as
    safely as a non-linear Lisp, but does not need a garbage
    collector. Since it offers assignments but no sharing, it occupies
    a twilight zone between functional languages and imperative
    languages. Our Linear Lisp Machine offers many of the same
    capabilities as combinator/graph reduction machines, but without
    their copying and garbage collection problems.

* .. _BAKER92C:

  Henry G. Baker. 1992.  "`The Treadmill: Real-Time Garbage Collection Without Motion Sickness <http://home.pipeline.com/~hbaker1/NoMotionGC.html>`_". ACM. SIGPLAN Notices 27, 3 (March 1992), pp. 66--70.

  .. admonition:: Abstract

    A simple real-time garbage collection algorithm is presented which
    does not copy, thereby avoiding some of the problems caused by the
    asynchronous motion of objects. This in-place "treadmill" garbage
    collection scheme has approximately the same complexity as other
    non-moving garbage collectors, thus making it usable in a
    high-level language implementation where some pointers cannot be
    traced. The treadmill is currently being used in a Lisp system
    built in Ada.

* .. _BAKER92:

  Henry G. Baker. 1992.  "`CONS Should not CONS its Arguments, or, a Lazy Alloc is a Smart Alloc <http://home.pipeline.com/~hbaker1/LazyAlloc.html>`_". ACM. SIGPLAN Notices 27, 3 (March 1992), 24--34.

  .. admonition:: Abstract

    "Lazy allocation" is a model for allocating objects on the
    execution stack of a high-level language which does not create
    dangling references. Our model provides safe transportation into
    the heap for objects that may survive the deallocation of the
    surrounding stack frame. Space for objects that do not survive the
    deallocation of the surrounding stack frame is reclaimed without
    additional effort when the stack is popped. Lazy allocation thus
    performs a first-level garbage collection, and if the language
    supports garbage collection of the heap, then our model can reduce
    the amortized cost of allocation in such a heap by filtering out
    the short-lived objects that can be more efficiently managed in
    LIFO order. A run-time mechanism called "result expectation"
    further filters out unneeded results from functions called only
    for their effects. In a shared-memory multi-processor environment,
    this filtering reduces contention for the allocation and
    management of global memory.

    Our model performs simple local operations, and is therefore
    suitable for an interpreter or a hardware implementation. Its
    overheads for functional data are associated only with
    *assignments*, making lazy allocation attractive for "mostly
    functional" programming styles. Many existing stack allocation
    optimizations can be seen as instances of this generic model, in
    which some portion of these local operations have been optimized
    away through static analysis techniques.

    Important applications of our model include the efficient
    allocation of temporary data structures that are passed as
    arguments to anonymous procedures which may or may not use these
    data structures in a stack-like fashion. The most important of
    these objects are functional arguments (funargs), which require
    some run-time allocation to preserve the local environment. Since
    a funarg is sometimes returned as a first-class value, its
    lifetime can survive the stack frame in which it was created.
    Arguments which are evaluated in a lazy fashion (Scheme "delays"
    or "suspensions") are similarly handled. Variable-length argument
    "lists" themselves can be allocated in this fashion, allowing
    these objects to become "first-class". Finally, lazy allocation
    correctly handles the allocation of a Scheme control stack,
    allowing Scheme continuations to become first-class values.

* .. _BAKER92B:

  Henry G. Baker. 1992.  "`NREVERSAL of Fortune -- The Thermodynamics of Garbage Collection <http://home.pipeline.com/~hbaker1/ReverseGC.html>`_". Springer-Verlag. LNCS Vol. 637.

  .. admonition:: Abstract

    The need to *reverse* a computation arises in many contexts --
    debugging, editor undoing, optimistic concurrency undoing,
    speculative computation undoing, trace scheduling, exception
    handling undoing, database recovery, optimistic discrete event
    simulations, subjunctive computing, etc. The need to *analyze* a
    reversed computation arises in the context of static analysis --
    liveness analysis, strictness analysis, type inference, etc.
    Traditional means for restoring a computation to a previous state
    involve checkpoints; checkpoints require time to copy, as well as
    space to store, the copied material. Traditional reverse abstract
    interpretation produces relatively poor information due to its
    inability to guess the previous values of assigned-to variables.

    We propose an abstract computer model and a programming language
    -- Psi-Lisp -- whose primitive operations are injective and hence
    reversible, thus allowing arbitrary undoing without the overheads
    of checkpointing. Such a computer can be built from reversible
    conservative logic circuits, with the serendipitous advantage of
    dissipating far less heat than traditional Boolean AND/OR/NOT
    circuits. Unlike functional languages, which have one "state" for
    all times, Psi-Lisp has at all times one "state", with unique
    predecessor and successor states.

    Compiling into a reversible pseudocode can have benefits even when
    targeting a traditional computer. Certain optimizations, e.g.,
    update-in-place, and compile-time garbage collection may be more
    easily performed, because the information may be elicited without
    the difficult and time-consuming iterative abstract interpretation
    required for most non-reversible models.

    In a reversible machine, garbage collection for recycling storage
    can always be performed by a reversed (sub)computation. While this
    "collection is reversed mutation" insight does not reduce space
    requirements when used for the computation as a whole, it does
    save space when used to recycle at finer scales. This insight also
    provides an explanation for the fundamental importance of the
    push-down stack both for recognizing palindromes and for managing
    storage.

    Reversible computers are related to *Prolog*, *linear logic* and
    *chemical abstract machines*.

* .. _BAKER93:

  Henry G. Baker. 1993.  "`'Infant Mortality' and Generational Garbage Collection <http://home.pipeline.com/~hbaker1/YoungGen.html>`_". ACM. SIGPLAN Notices 28, 4 (April 1993), pp. 55--57.

  .. admonition:: Abstract

    Generation-based garbage collection has been advocated by
    appealing to the intuitive but vague notion that "young objects
    are more likely to die than old objects". The intuition is, that
    if a generation-based garbage collection scheme focuses its effort
    on scanning recently created objects, then its scanning efforts
    will pay off more in the form of more recovered garbage, than if
    it scanned older objects. In this note, we show a counterexample
    of a system in which "infant mortality" is as high as you please,
    but for which generational garbage collection is ineffective for
    improving the average mark/cons ratio. Other benefits, such as
    better locality and a smaller number of large delays, may still
    make generational garbage collection attractive for such a system,
    however.

* .. _BAKER93A:

  Henry G. Baker. 1993.  "`Equal Rights for Functional Objects or, The More Things Change, The More They Are the Same <http://home.pipeline.com/~hbaker1/ObjectIdentity.html>`_". ACM. OOPS Messenger 4, 4 (October 1993), pp. 2--27.

  .. admonition:: Abstract

    We argue that intensional object identity in object-oriented
    programming languages and databases is best defined operationally
    by side-effect semantics. A corollary is that "functional" objects
    have extensional semantics. This model of object identity, which
    is analogous to the normal forms of relational algebra, provides
    cleaner semantics for the value-transmission operations and
    built-in primitive equality predicate of a programming language,
    and eliminates the confusion surrounding "call-by-value" and
    "call-by-reference" as well as the confusion of multiple equality
    predicates.

    Implementation issues are discussed, and this model is shown to
    have significant performance advantages in persistent, parallel,
    distributed and multilingual processing environments. This model
    also provides insight into the "type equivalence" problem of
    Algol-68, Pascal and Ada.

* .. _BAKER94:

  Henry G. Baker. 1994.  "`Minimizing Reference Count Updating with Deferred and Anchored Pointers for Functional Data Structures <http://home.pipeline.com/~hbaker1/LRefCounts.html>`_". ACM. SIGPLAN Notices 29, 9 (September 1994), pp. 38--43.

  .. admonition:: Abstract

    "Reference counting" can be an attractive form of dynamic storage
    management. It recovers storage promptly and (with a garbage stack
    instead of a free list) it can be made "real-time" -- i.e., all
    accesses can be performed in constant time. Its major drawbacks
    are its inability to reclaim cycles, its count storage, and its
    count update overhead. Update overhead is especially irritating
    for functional (read-only) data where updates may dirty pristine
    cache lines and pages.

    We show how reference count updating can be largely eliminated for
    functional data structures by using the "linear style" of
    programming that is inspired by Girard's linear logic, and by
    distinguishing normal pointers from "anchored pointers", which
    indicate not only the object itself, but also the depth of the
    stack frame that anchors the object. An "anchor" for a pointer is
    essentially an enclosing data structure that is temporarily locked
    from being collected for the duration of the anchored pointer's
    existence by a deferred reference count. An "anchored pointer"
    thus implies a reference count increment that has been deferred
    until it is either cancelled or performed.

    Anchored pointers are generalizations of "borrowed" pointers and
    "phantom" pointers. Anchored pointers can provide a solution to
    the "derived pointer problem" in garbage collection.

* .. _BAKER94A:

  Henry G. Baker. 1994.  "`Thermodynamics and Garbage Collection <http://home.pipeline.com/~hbaker1/ThermoGC.html>`_". ACM. SIGPLAN Notices 29, 4 (April 1994), pp. 58--63.

  .. admonition:: Abstract

    We discuss the principles of statistical thermodynamics and their
    application to storage management problems. We point out problems
    which result from imprecise usage of the terms "information",
    "state", "reversible", "conservative", etc.

* .. _BAKER95A:

  Henry G. Baker. 1995.  "`'Use-Once' Variables and Linear Objects -- Storage Management, Reflection and Multi-Threading <http://home.pipeline.com/~hbaker1/Use1Var.html>`_". ACM. SIGPLAN Notices 30, 1 (January 1995), pp. 45--52.

  .. admonition:: Abstract

    Programming languages should have 'use-once' variables in addition
    to the usual 'multiple-use' variables. 'Use-once' variables are
    bound to linear (unshared, unaliased, or singly-referenced)
    objects. Linear objects are cheap to access and manage, because
    they require no synchronization or tracing garbage collection.
    Linear objects can elegantly and efficiently solve otherwise
    difficult problems of functional/mostly-functional systems --
    e.g., in-place updating and the efficient initialization of
    functional objects. Use-once variables are ideal for directly
    manipulating resources which are inherently linear such as
    freelists and 'engine ticks' in reflective languages.

    A 'use-once' variable must be dynamically referenced exactly once
    within its scope. Unreferenced use-once variables must be
    explicitly killed, and multiply-referenced use-once variables must
    be explicitly copied; this duplication and deletion is subject to
    the constraint that some linear datatypes do not support
    duplication and deletion methods. Use-once variables are bound
    only to linear objects, which may reference other linear or
    non-linear objects. Non-linear objects can reference other
    non-linear objects, but can reference a linear object only in a
    way that ensures mutual exclusion.

    Although implementations have long had implicit use-once variables
    and linear objects, most languages do not provide the programmer
    any help for their utilization. For example, use-once variables
    allow for the safe/controlled use of reified language
    implementation objects like single-use continuations.

    Linear objects and use-once variables map elegantly into dataflow
    models of concurrent computation, and the graphical
    representations of dataflow models make an appealing visual linear
    programming language.

* .. _BAKER95:

  Henry G. Baker. 1995. *Memory Management: International Workshop IWMM'95*. Springer-Verlag. ISBN 3-540-60368-9.

  .. admonition:: From the Preface

    The International Workshop on Memory Management 1995 (IWMM'95) is
    a continuation of the excellent series started by Yves Bekkers and
    Jacques Cohen with IWMM'92. The present volume assembles the
    refereed and invited technical papers which were presented during
    this year's workshop.

* .. _BBW97:

  Nick Barnes, Richard Brooksby, David Jones, Gavin Matthews, Pekka P. Pirinen, Nick Dalton, P. Tucker Withington. 1997. "`A Proposal for a Standard Memory Management Interface <http://www.cs.utexas.edu/ftp/garbage/GC97/withingt.ps>`_". OOPSLA97 Workshop on Garbage Collection and Memory Management.

  .. admonition:: From the notes

    There is no well-defined memory-management library API which would
    allow programmers to easily choose the best memory management
    implementation for their application.

    Some languages allow replacement of their memory management
    functions, but usually only the program API is specified, hence
    replacement of the entire program interface is required.

    Few languages support multiple memory management policies within a
    single program. Those that do use proprietary memory management
    policies.

    We believe that the design of an abstract program API is a
    prerequisite to the design of a “server” API and eventually an API
    that would permit multiple cooperating memory “servers”. If the
    interface is simple yet powerful enough to encompass most memory
    management systems, it stands a good chance of being widely
    adopted.

* .. _ZORN93B:

  David A. Barrett, Benjamin Zorn. 1993. "`Using Lifetime Predictors to Improve Memory Allocation Performance <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.56.6712&rep=rep1&type=pdf>`_". ACM. SIGPLAN'93 Conference on Programming Language Design and Implementation, pp. 187--196.

  .. admonition:: Abstract

    Dynamic storage allocation is used heavily in many application
    areas including interpreters, simulators, optimizers, and
    translators. We describe research that can improve all aspects of
    the performance of dynamic storage allocation by predicting the
    lifetimes of short-lived objects when they are allocated. Using
    five significant, allocation-intensive C programs, we show that a
    great fraction of all bytes allocated are short-lived (> 90% in
    all cases). Furthermore, we describe an algorithm for lifetime
    prediction that accurately predicts the lifetimes of 42--99% of all
    objects allocated. We describe and simulate a storage allocator
    that takes advantage of lifetime prediction of short-lived objects
    and show that it can significantly improve a program's memory
    overhead and reference locality, and even, at times, improve CPU
    performance as well.

* .. _BARRETT93:

  David A. Barrett, Benjamin Zorn. 1995. "`Garbage Collection using a Dynamic Threatening Boundary <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.1835&rep=rep1&type=pdf>`_". ACM. SIGPLAN'95 Conference on Programming Language Design and Implementation, pp. 301--314.

  .. admonition:: Abstract

    Generational techniques have been very successful in reducing the
    impact of garbage collection algorithms upon the performance of
    programs. However, it is impossible for designers of collection
    algorithms to anticipate the memory allocation behavior of all
    applications in advance. Existing generational collectors rely
    upon the applications programmer to tune the behavior of the
    collector to achieve maximum performance for each application.
    Unfortunately, because the many tuning parameters require detailed
    knowledge of both the collection algorithm and the program
    allocation behavior in order to be used effectively, such tuning
    is difficult and error prone. We propose a new garbage collection
    algorithm that uses just two easily understood tuning parameters
    that directly reflect the maximum memory and pause time
    constraints familiar to application programmers and users.

    Like generational collectors, ours divides memory into two spaces,
    one for short-lived, and another for long-lived objects. Unlike
    previous work, our collector dynamically adjusts the boundary
    between these two spaces in order to directly meet the resource
    constraints specified by the user. We describe two methods for
    adjusting this boundary, compare them with several existing
    algorithms, and show how effectively ours meets the specified
    constraints. Our pause time collector saved memory by holding
    median pause times closer to the constraint than the other pause
    time constrained algorithm and, when not over-constrained, our
    memory constrained collector exhibited the lowest CPU overhead of
    the algorithms we measured yet was capable of maintaining a
    maximum memory constraint.

* .. _BARTLETT88:

  Joel F. Bartlett. 1988. "`Compacting Garbage Collection with Ambiguous Roots <http://computer-refuge.org/classiccmp/ftp.digital.com-jun2004/pub/Compaq/WRL/research-reports/WRL-TR-88.2.pdf>`_". Digital Equipment Corporation.

  .. admonition:: Abstract

    This paper introduces a copying garbage collection algorithm which
    is able to compact most of the accessible storage in the heap
    without having an explicitly defined set of pointers that contain
    all the roots of all accessible storage. Using "hints" found in
    the processor's registers and stack, the algorithm is able to
    divide heap allocated objects into two groups: those that might be
    referenced by a pointer in the stack or registers, and those that
    are not. The objects which might be referenced are left in place,
    and the other objects are copied into a more compact
    representation.

    A Lisp compiler and runtime system which uses such a collector
    need not have complete control of the processor in order to force
    a certain discipline on the stack and registers. A Scheme
    implementation has been done for the Digital WRL Titan processor
    which uses a garbage collector based on this "mostly copying"
    algorithm. Like other languages for the Titan, it uses the Mahler
    intermediate language as its target. This simplifies the compiler
    and allows it to take advantage of the significant machine
    dependent optimizations provided by Mahler. The common
    intermediate language also simplifies call-outs from Scheme
    programs to functions written in other languages and call-backs
    from functions in other languages.

    Measurements of the Scheme implementation show that the algorithm
    is efficient, as little unneeded storage is retained and only a
    very small fraction of the heap is left in place.

    Simple pointer manipulation protocols also mean that compiler
    support is not needed in order to correctly handle pointers. Thus
    it is reasonable to provide garbage collected storage in languages
    such as C. A collector written in C which uses this algorithm is
    included in the Appendix.

* .. _BARTLETT89:

  Joel F. Bartlett. 1989. "`Mostly-Copying Garbage Collection Picks Up Generations and C++ <http://www.hpl.hp.com/techreports/Compaq-DEC/WRL-TN-12.pdf>`_". Digital Equipment Corporation.

  .. admonition:: Abstract

    The "mostly-copying" garbage collection algorithm provides a way
    to perform compacting garbage collection in spite of the presence
    of ambiguous pointers in the root set. As originally defined, each
    collection required almost all accessible objects to be moved.
    While adequate for many applications, programs that retained a
    large amount of storage spent a significant amount of time garbage
    collecting. To improve performance of these applications, a
    generational version of the algorithm has been designed. This note
    reports on this extension of the algorithm, and its application in
    collectors for Scheme and C++.

* .. _BC92:

  Yves Bekkers & Jacques Cohen. 1992. "`Memory Management, International Workshop IWMM 92 <http://www.informatik.uni-trier.de/%7Eley/db/conf/iwmm/iwmm92.html>`_". Springer-Verlag. LNCS Vol. 637, ISBN 3-540-55940-X.

* .. _BB99:

  Emery D. Berger, Robert D. Blumofe. 1999. "`Hoard: A Fast, Scalable, and Memory-Efficient Allocator for Shared-Memory Multiprocessors <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.81.5049&rep=rep1&type=pdf>`_". University of Texas at Austin. UTCS TR99-22.

  .. admonition:: Abstract

    In this paper, we present Hoard, a memory allocator for
    shared-memory multiprocessors. We prove that its worst-case memory
    fragmentation is asymptotically equivalent to that of an optimal
    uniprocessor allocator. We present experiments that demonstrate
    its speed and scalability.

* .. _BERGER01:

  Emery D. Berger, Benjamin G. Zorn, Kathryn S. McKinley. 2001. "`Composing high-performance memory allocators <http://www.cs.utexas.edu/users/speedway/DaCapo/papers/pldi2001.pdf>`_" ACM SIGPLAN Conference on Programming Language Design and Implementation 2001, pp. 114--124.

  .. admonition:: Abstract

    Current general-purpose memory allocators do not provide
    sufficient speed or flexibility for modern high-performance
    applications. Highly-tuned general purpose allocators have
    per-operation costs around one hundred cycles, while the cost of
    an operation in a custom memory allocator can be just a handful of
    cycles. To achieve high performance, programmers often write
    custom memory allocators from scratch -- a difficult and
    error-prone process.

    In this paper, we present a flexible and efficient infrastructure
    for building memory allocators that is based on C++ templates and
    inheritance. This novel approach allows programmers to build
    custom and general-purpose allocators as “heap layers” that can be
    composed without incurring any additional runtime overhead or
    additional programming cost. We show that this infrastructure
    simplifies allocator construction and results in allocators that
    either match or improve the performance of heavily-tuned
    allocators written in C, including the Kingsley allocator and the
    GNU obstack library. We further show this infrastructure can be
    used to rapidly build a general-purpose allocator that has
    performance comparable to the Lea allocator, one of the best
    uniprocessor allocators available. We thus demonstrate a clean,
    easy-to-use allocator interface that seamlessly combines the power
    and efficiency of any number of general and custom allocators
    within a single application.

* .. _BW88:

  Hans-J. Boehm, Mark Weiser. 1988. "`Garbage collection in an uncooperative environment <http://hboehm.info/spe_gc_paper/preprint.pdf>`_". Software -- Practice and Experience. 18(9):807--820.

  .. admonition:: Abstract

    We describe a technique for storage allocation and garbage
    collection in the absence of significant co-operation from the
    code using the allocator. This limits garbage collection overhead
    to the time actually required for garbage collection. In
    particular, application programs that rarely or never make use of
    the collector no longer encounter a substantial performance
    penalty. This approach greatly simplifies the implementation of
    languages supporting garbage collection. It further allows
    conventional compilers to be used with a garbage collector, either
    as the primary means of storage reclamation, or as a debugging
    tool.

* .. _BDS91:

  Hans-J. Boehm, Alan J. Demers, Scott Shenker. 1991. "`Mostly Parallel Garbage Collection <http://hboehm.info/gc/papers/pldi91.ps.Z>`_". Xerox PARC. ACM PLDI 91, SIGPLAN Notices 26, 6 (June 1991), pp. 157--164.

  .. admonition:: Abstract

    We present a method for adapting garbage collectors designed to
    run sequentially with the client, so that they may run
    concurrently with it. We rely on virtual memory hardware to
    provide information about pages that have been updated or
    "dirtied" during a given period of time. This method has been used
    to construct a mostly parallel trace-and-sweep collector that
    exhibits very short pause times. Performance measurements are
    given.

* .. _BC92A:

  Hans-J. Boehm, David Chase. 1992. "`A Proposal for Garbage-Collector-Safe C Compilation <http://hboehm.info/gc/papers/boecha.ps.gz>`_". *Journal of C Language Translation.* vol. 4, 2 (December 1992), pp. 126--141.

  .. admonition:: Abstract

    Conservative garbage collectors are commonly used in combination
    with conventional C programs. Empirically, this usually works
    well. However, there are no guarantees that this is safe in the
    presence of "improved" compiler optimization. We propose that C
    compilers provide a facility to suppress optimizations that are
    unsafe in the presence of conservative garbage collection. Such a
    facility can be added to an existing compiler at very minimal
    cost, provided the additional analysis is done in a
    machine-independent source-to-source prepass. Such a prepass may
    also check the source code for garbage-collector-safety.

* .. _BOEHM93:

  Hans-J. Boehm. 1993. "`Space Efficient Conservative Garbage Collection <http://hboehm.info/gc/papers/pldi93.ps.Z>`_". ACM, SIGPLAN. Proceedings of the ACM SIGPLAN '91 Conference on Programming Language Design and Implementation, SIGPLAN Notices 28, 6, pp 197--206.

  .. admonition:: Abstract

    We call a garbage collector conservative if it has only partial
    information about the location of pointers, and is thus forced to
    treat arbitrary bit patterns as though they might be pointers, in
    at least some cases. We show that some very inexpensive, but
    previously unused techniques can have dramatic impact on the
    effectiveness of conservative garbage collectors in reclaiming
    memory. Our most significant observation is that static data that
    appears to point to the heap should not result in misidentified
    reference to the heap. The garbage collector has enough
    information to allocate around such references. We also observe
    that programming style has a significantly impact on the amount of
    spuriously retained storage, typically even if the collector is
    not terribly conservative. Some fairly common C and C++
    programming styles significantly decrease the effectiveness of any
    garbage collector. These observations suffice to explain some of
    the different assessments of conservative collection that have
    appeared in the literature.

* .. _BOEHM00:

  Hans-J. Boehm. 2000. "`Reducing Garbage Collector Cache Misses <http://www.hpl.hp.com/techreports/2000/HPL-2000-99.html>`_". ACM. ISMM'00 pp. 59--64.

  .. admonition:: Abstract

    Cache misses are currently a major factor in the cost of garbage
    collection, and we expect them to dominate in the future.
    Traditional garbage collection algorithms exhibit relatively litle
    temporal locality; each live object in the heap is likely to be
    touched exactly once during each garbage collection. We measure
    two techniques for dealing with this issue: prefetch-on-grey, and
    lazy sweeping. The first of these is new in this context. Lazy
    sweeping has been in common use for a decade. It was introduced as
    a mechanism for reducing paging and pause times; we argue that it
    is also crucial for eliminating cache misses during the sweep
    phase.

    Our measurements are obtained in the context of a non-moving
    garbage collector. Fully copying garbage collection inherently
    requires more traffic through the cache, and thus probably also
    stands to benefit substantially from something like the
    prefetch-on-grey technique. Generational garbage collection may
    reduce the benefit of these techniques for some applications, but
    experiments with a non-moving generational collector suggest that
    they remain quite useful.

* .. _BOEHM01:

  Hans-J. Boehm. 2001. "Bounding Space Usage of Conservative Garbage Collectors `<http://www.hpl.hp.com/techreports/2001/HPL-2001-251.html>`_". HP Labs technical report HPL-2001-251.

  .. admonition:: Abstract

    Conservative garbage collectors can automatically reclaim unused
    memory in the absence of precise pointer location information. If
    a location can possibly contain a pointer, it is treated by the
    collector as though it contained a pointer. Although it is
    commonly assumed that this can lead to unbounded space use due to
    misidentified pointers, such extreme space use is rarely observed
    in practice, and then generally only if the number of
    misidentified pointers is itself unbounded. We show that if the
    program manipulates only data structures satisfying a simple
    GC-robustness criterion, then a bounded number of misidentified
    pointers can result at most in increasing space usage by a
    constant factor. We argue that nearly all common data structures
    are already GC- robust, and it is typically easy to identify and
    replace those that are not. Thus it becomes feasible to prove
    space bounds on programs collected by mildly conservative garbage
    collectors, such as the one in Barabash et al. (2001). The
    worst-case space overhead introduced by such mild conservatism is
    comparable to the worst-case fragmentation overhead for inherent
    in any non-moving storage allocator. The same GC-robustness
    criterion also ensures the absence of temporary space leaks of the
    kind discussed in Rojemo (1995) for generational garbage
    collectors.

* .. _BOEHM02:

  Hans-J. Boehm. 2002. "`Destructors, Finalizers, and Synchronization <http://www.hpl.hp.com/techreports/2002/HPL-2002-335.html>`_". HP Labs technical report HPL-2002-335.

  .. admonition:: Abstract

    We compare two different facilities for running cleanup actions
    for objects that are about to reach the end of their life.
    Destructors, such as we find in C++, are invoked synchronously
    when an object goes out of scope. They make it easier to implement
    cleanup actions for objects of well-known lifetime, especially in
    the presence of exceptions. Languages like Java, Modula-3, and C#
    provide a different kind of "finalization" facility: Cleanup
    methods may be run when the garbage collector discovers a heap
    object to be otherwise inaccessible. Unlike C++ destructors, such
    methods run in a separate thread at some much less well-defined
    time. We argue that these are fundamentally different, and
    potentially complementary, language facilities. We also try to
    resolve some common misunderstandings about finalization in the
    process. In particular: 1. The asynchronous nature of finalizers
    is not just an accident of implementation or a shortcoming of
    tracing collectors; it is necessary for correctness of client
    code, fundamentally affects how finalizers must be written, and
    how finalization facilities should be presented to the user. 2. An
    object may legitimately be finalized while one of its methods are
    still running. This should and can be addressed by the language
    specification and client code.

* .. _BM77:

  Robert S. Boyer and J. Strother Moore. 1977. "`A Fast String Searching Algorithm <http://www.cs.utexas.edu/~moore/publications/fstrpos.pdf>`_". *Communications of the ACM* 20(10):762--772.

  .. admonition:: Abstract

    An algorithm is presented that searches for the location, "*i*,"
    of the first occurrence of a character string, "*pat*," in another
    string, "*string*." During the search operation, the characters of
    *pat* are matched starting with the last character of *pat*. The
    information gained by starting the match at the end of the pattern
    often allows the algorithm to proceed in large jumps through the
    text being searched. Thus the algorithm has the unusual property
    that, in most cases, not all of the first *i* characters of
    *string* are inspected. The number of characters actually
    inspected (on the average) decreases as a function of the length
    of *pat*. For a random English pattern of length 5, the algorithm
    will typically inspect *i*/4 characters of string before finding a
    match at *i*. Furthermore, the algorithm has been implemented so
    that (on the average) fewer than *i* + *patlen* machine
    instructions are executed. These conclusions are supported with
    empirical evidence and a theoretical analysis of the average
    behavior of the algorithm. The worst case behavior of the
    algorithm is linear in *i* + *patlen*, assuming the availability
    of array space for tables linear in *patlen* plus the size of the
    alphabet.

* .. _BL72:

  P. Branquart, J. Lewi. 1972. "A scheme of storage allocation and garbage collection for ALGOL 68". Elsevier/North-Holland. ALGOL 68 Implementation -- Proceedings of the IFIP Working Conference on ALGOL 68 Implementation, July 1970.

* .. _BROOKSBY02:

  Richard Brooksby. 2002. "`The Memory Pool System: Thirty person-years of memory management development goes Open Source <https://www.ravenbrook.com/project/mps/doc/2002-01-30/ismm2002-paper/>`_". ISMM'02.

  .. admonition:: Abstract

    The Memory Pool System (MPS) is a very general, adaptable,
    flexible, reliable, and efficient memory management system. It
    permits the flexible combination of memory management techniques,
    supporting manual and automatic memory management, in-line
    allocation, finalization, weakness, and multiple simultaneous
    co-operating incremental generational garbage collections. It also
    includes a library of memory pool classes implementing specialized
    memory management policies.

    Between 1994 and 2001, Harlequin (now part of Global Graphics)
    invested about thirty person-years of effort developing the MPS.
    The system contained many innovative techniques and abstractions
    which were kept secret. In 1997 Richard Brooksby, the manager and
    chief architect of the project, and Nicholas Barnes, a senior
    developer, left Harlequin to form their own consultancy company,
    Ravenbrook, and in 2001, Ravenbrook acquired the MPS technology
    from Global Graphics. We are happy to announce that we are
    publishing the source code and documentation under an open source
    licence. This paper gives an overview of the system.

* .. _C1990:

  International Standard ISO/IEC 9899:1990. "Programming languages — C".

* .. _C1999:

  International Standard ISO/IEC 9899:1999. "`Programming languages — C <http://www.open-std.org/jtc1/sc22/WG14/www/docs/n1256.pdf>`_".

* .. _CGZ94:

  Brad Calder, Dirk Grunwald, Benjamin Zorn. 1994. "`Quantifying Behavioral Differences Between C and C++ Programs <http://cseclassic.ucsd.edu/users/calder/papers/JplVersion.pdf>`_". *Journal of Programming Languages.* 2(4):313--351.

  .. admonition:: Abstract

    Improving the performance of C programs has been a topic of great
    interest for many years. Both hardware technology and compiler
    optimization research has been applied in an effort to make C
    programs execute faster. In many application domains, the C++
    language is replacing C as the programming language of choice. In
    this paper, we measure the empirical behavior of a group of
    significant C and C++ programs and attempt to identify and
    quantify behavioral differences between them. Our goal is to
    determine whether optimization technology that has been successful
    for C programs will also be successful in C++ programs. We
    furthermore identify behavioral characteristics of C++ programs
    that suggest optimizations that should be applied in those
    programs. Our results show that C++ programs exhibit behavior that
    is significantly different than C programs. These results should
    be of interest to compiler writers and architecture designers who
    are designing systems to execute object-oriented programs.

* .. _CPC00:

  Dante J. Cannarozzi, Michael P. Plezbert, Ron K. Cytron. 2000. "`Contaminated garbage collection <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.58.9649&rep=rep1&type=pdf>`_". ACM. Proceedings of the ACM SIGPLAN '00 conference on on Programming language design and implementation, pp. 264--273.

  .. admonition:: Abstract

    We describe a new method for determining when an object can be
    garbage collected. The method does not require marking live
    objects. Instead, each object *X* is *dynamically* associated with
    a stack frame *M*, such that *X* is collectable when *M* pops.
    Because *X* could have been dead earlier, our method is
    conservative. Our results demonstrate that the methos nonetheless
    idenitifies a large percentage of collectable objects. The method
    has been implemented in Sun's Java™ Virtual Machine interpreter,
    and results are presented based on this implementation.

* .. _CW86:

  Patrick J. Caudill, Allen Wirfs-Brock. 1986. "A Third-Generation Smalltalk-80 Implementation". ACM. SIGPLAN Notices. 21(11), OOPSLA'86 ACM Conference on Object-Oriented Systems, Languages and Applications.

  .. admonition:: Abstract

    A new, high performance Smalltalk-80™ implementation is described
    which builds directly upon two previous implementation efforts.
    This implementation supports a large object space while retaining
    compatibility with previous Smalltalk-80™ images. The
    implementation utilizes a interpreter which incorporates a
    generation based garbage collector and which does not have an
    object table. This paper describes the design decisions which lead
    to this implementation and reports preliminary performance
    results.

* .. _CHENEY70:

  C. J. Cheney. 1970. "`A non-recursive list compacting algorithm <http://people.cs.umass.edu/~emery/classes/cmpsci691s-fall2004/papers/p677-cheney.pdf>`_". CACM. 13-11 pp. 677--678.

  .. admonition:: Abstract

    A simple nonrecursive list structure compacting scheme or garbage
    collector suitable for both compact and LISP-like list structures
    is presented. The algorithm avoids the need for recursion by using
    the partial structure as it is built up to keep track of those
    lists that have been copied.

* .. _CHL98:

  Perry Cheng, Robert Harper, Peter Lee. 1998. "`Generational stack collection and profile-driven pretenuring <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.43.9229&rep=rep1&type=pdf>`_". ACM. Proceedings of SIGPLAN'98 Conference on Programming Language Design and Implementation, pp. 162--173.

  .. admonition:: Abstract

    This paper presents two techniques for improving garbage
    collection performance: generational stack collection and
    profile-driven pretenuring. The first is applicable to stack-based
    implementations of functional languages while the second is useful
    for any generational collector. We have implemented both
    techniques in a generational collector used by the TIL compiler,
    and have observed decreases in garbage collection times of as much
    as 70% and 30%, respectively.

    Functional languages encourage the use of recursion which can lead
    to a long chain of activation records. When a collection occurs,
    these activation records must be scanned for roots. We show that
    scanning many activation records can take so long as to become the
    dominant cost of garbage collection. However, most deep stacks
    unwind very infrequently, so most of the root information obtained
    from the stack remains unchanged across successive garbage
    collections. *Generational stack collection* greatly reduces the
    stack scan cost by reusing information from previous scans.

    Generational techniques have been successful in reducing the cost
    of garbage collection. Various complex heap arrangements and
    tenuring policies have been proposed to increase the effectiveness
    of generational techniques by reducing the cost and frequency of
    scanning and copying. In contrast, we show that by using profile
    information to make lifetime predictions, *pretenuring* can avoid
    copying data altogether. In essence, this technique uses a
    refinement of the generational hypothesis (most data die young)
    with a locality principle concerning the age of data: most
    allocations sites produce data that immediately dies, while a few
    allocation sites consistently produce data that survives many
    collections.

* .. _CL98:

  Trishul M. Chilimbi, James R. Larus. 1998. "`Using Generational Garbage Collection To Implement Cache-Conscious Data Placement <http://ftp2.cs.wisc.edu/wwt/ismm98_cache_gc.pdf>`_". ACM. ISMM'98 pp. 37--48.

  .. admonition:: Abstract

    Processor and memory technology trends show a continual increase
    in the cost of accessing main memory. Machine designers have tried
    to mitigate the effect of this trend through a variety of
    techniques that attempt to reduce or tolerate memory latency.
    These techniques, unfortunately, have only been partially
    successful for pointer-manipulating programs. Recent research has
    demonstrated that these programs can benefit greatly from the
    complementary approach of reorganizing pointer data structures to
    improve cache locality. This paper describes how a generational
    garbage collector can be used to achieve a cache-conscious data
    layout, in which objects with high temporal affinity are placed
    next to each other, so they are likely to reside in the same cache
    block. The paper demonstrates the feasibility of collecting low
    overhead, real-time profiling information about data access
    patterns for object-oriented languages, and describes a new
    copying algorithm that utilizes this information to produce a
    cache-conscious object layout. Preliminary results indicate that
    this technique reduces cache miss rates by 21-42\%, and improves
    program performance by 14-37\%.

* .. _CH97:

  William D Clinger & Lars T Hansen. 1997. "`Generational Garbage Collection and the Radioactive Decay Model <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.164.370&rep=rep1&type=pdf>`_". ACM. Proceedings of PLDI 1997.

  .. admonition:: Abstract

    If a fixed exponentially decreasing probability distribution
    function is used to model every object's lifetime, then the age of
    an object gives no information about its future life expectancy.
    This *radioactive decay model* implies that there can be no
    rational basis for deciding which live objects should be promoted
    to another generation. Yet there remains a rational basis for
    deciding how many objects to promote, when to collect garbage, and
    which generations to collect.

    Analysis of the model leads to a new kind of generational garbage
    collector whose effectiveness does not depend upon heuristics that
    predict which objects will live longer than others.

    This result provides insight into the computational advantages of
    generational garbage collection, with implications for the
    management of objects whose life expectancies are difficult to
    predict.

* .. _COHEN81:

  Jacques Cohen. 1981. "Garbage collection of linked data structures". Computing Surveys. Vol. 13, no. 3.

  .. admonition:: Abstract

    A concise and unified view of the numerous existing algorithms for
    performing garbage collection of linked data structures is
    presented. The emphasis is on garbage collection proper, rather
    than on storage allocation.

    First, the classical garbage collection algorithms and their
    marking and collecting phases, with and without compacting, are
    discussed.

    Algorithms describing these phases are classified according to the
    type of cells to be collected: those for collecting single-sized
    cells are simpler than those for varisized cells. Recently
    proposed algorithms are presented and compared with the classical
    ones. Special topics in garbage collection are also covered. A
    bibliography with topical annotations is included.

* .. _CCZ98:

  Dominique Colnet, Philippe Coucaud, Olivier Zendra. 1998. "`Compiler Support to Customize the Mark and Sweep Algorithm <http://pdf.aminer.org/000/465/134/compiler_support_to_customize_the_mark_and_sweep_algorithm.pdf>`_". ACM. ISMM'98 pp. 154--165.

  .. admonition:: Abstract

    Mark and sweep garbage collectors (GC) are classical but still
    very efficient automatic memory management systems. Although
    challenged by other kinds of systems, such as copying collectors,
    mark and sweep collectors remain among the best in terms of
    performance.

    This paper describes our implementation of an efficient mark and
    sweep garbage collector tailored to each program. Compiler support
    provides the type information required to statically and
    automatically generate this customized garbage collector. The
    segregation of object by type allows the production of a more
    efficient GC code. This technique, implemented in SmallEiffel, our
    compiler for the object-oriented language Eiffel, is applicable to
    other languages and other garbage collection algorithms, be they
    distributed or not.

    We present the results obtained on programs featuring a variety of
    programming styles and compare our results to a well-known and
    high-quality garbage collector.

* .. _CWZ93:

  Jonathan E. Cook, Alexander L. Wolf, Benjamin Zorn. 1994. "`Partition Selection Policies in Object Database Garbage Collection <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.3656&rep=rep1&type=pdf>`_". ACM. SIGMOD. International Conference on the Management of Data (SIGMOD'94), pp. 371--382.

  .. admonition:: Abstract

    The automatic reclamation of storage for unreferenced objects is
    very important in object databases. Existing language system
    algorithms for automatic storage reclamation have been shown to be
    inappropriate. In this paper, we investigate methods to improve
    the performance of algorithms for automatic storage reclamation of
    object databases. These algorithms are based on a technique called
    partitioned garbage collection, in which a subset of the entire
    database is collected independently of the rest. Specifically, we
    investigate the policy that is used to select what partition in
    the database should be collected. The new partition selection
    policies that we propose and investigate are based on the
    intuition that the values of overwritten pointers provide good
    hints about where to find garbage. Using trace-driven simulation,
    we show that one of our policies requires less I/O to collect more
    garbage than any existing implementable policy and performs close
    to an impractical-to-implement but near-optimal policy over a wide
    range of database sizes and connectivities.

* .. _CKWZ96:

  Jonathan E. Cook, Artur Klauser, Alexander L. Wolf, Benjamin Zorn. 1996. "`Semi-automatic, Self-adaptive Control of Garbage Collection Rates in Object Databases <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.46.8140&rep=rep1&type=pdf>`_". ACM, SIGMOD. International Conference on the Management of Data (SIGMOD'96), pp. 377--388.

  .. admonition:: Abstract

    A fundamental problem in automating object database storage
    reclamation is determining how often to perform garbage
    collection. We show that the choice of collection rate can have a
    significant impact on application performance and that the "best"
    rate depends on the dynamic behavior of the application, tempered
    by the particular performance goals of the user. We describe two
    semi-automatic, self-adaptive policies for controlling collection
    rate that we have developed to address the problem. Using
    trace-driven simulations, we evaluate the performance of the
    policies on a test database application that demonstrates two
    distinct reclustering behaviors. Our results show that the
    policies are effective at achieving user-specified levels of I/O
    operations and database garbage percentage. We also investigate
    the sensitivity of the policies over a range of object
    connectivities. The evaluation demonstrates that semi-automatic,
    self-adaptive policies are a practical means for flexibly
    controlling garbage collection rate.

* .. _CNS92:

  Eric Cooper, Scott Nettles, Indira Subramanian. 1992. "Improving the Performance of SML Garbage Collection using Application-Specific Virtual Memory Management". ACM Conference on LISP and Functional Programming, pp. 43--52.

  .. admonition:: Abstract

    We improved the performance of garbage collection in the Standard ML of
    New Jersey system by using the virtual memory facilities provided by
    the Mach kernel.  We took advantage of Mach's support for large sparse
    address spaces and user-defined paging servers.  We decreased the
    elapsed time for realistic applications by as much as a factor of 4.

* .. _DACONTA93:

  Michael C. Daconta. 1993. *C Pointers and Dynamic Memory Management.* Wiley. ISBN 0-471-56152-5.

* .. _DACONTA95:

  Michael C. Daconta. 1995. *C++ Pointers and Dynamic Memory Management.* Wiley. ISBN 0-471-04998-0.

  .. admonition:: From the back cover

    Using techniques developed in the classroom at America Online's
    Programmer's University, Michael Daconta deftly pilots programmers
    through the intricacies of the two most difficult aspects of C++
    programming: pointers and dynamic memory management. Written by a
    programmer for programmers, this no-nonsense, nuts-and-bolts guide
    shows you how to fully exploit advanced C++ programming features,
    such as creating class-specific allocators, understanding
    references versus pointers, manipulating multidimensional arrays
    with pointers, and how pointers and dynamic memory are the core of
    object-oriented constructs like inheritance, name-mangling, and
    virtual functions.

* .. _DAHL63:

  O.-J. Dahl. 1963. "The SIMULA Storage Allocation Scheme". Norsk Regnesentral. NCC Document no. 162.

* .. _DENNING68:

  P. J. Denning. 1968. "`Thrashing: Its Causes and Prevention <https://cs.uwaterloo.ca/~Brecht/courses/702/Possible-Readings/vm-and-gc/thrashing-denning-afips-1968.pdf>`_". Proceedings AFIPS,1968 Fall Joint Computer Conference, vol. 33, pp. 915--922.

  .. admonition:: From the introduction

    A particularly troublesome phenomenon, thrashing, may seriously
    interfere with the performance of paged memory systems, reducing
    computing giants (Multics, IBM System 360, and others not
    necessarily excepted) to computing dwarfs. The term thrashing
    denotes excessive overhead and severe performance degradation or
    collapse caused by too much paging. Thrashing inevitably turns a
    shortage of memory space into a surplus of processor time.

* .. _DENNING70:

  P. J. Denning. 1970. "`Virtual Memory <http://denninginstitute.com/pjd/PUBS/VirtMem_1970.pdf>`_". ACM. ACM Computing Surveys, vol. 2, no. 3, pp. 153--190, Sept. 1970.

  .. admonition:: Abstract

    The need for automatic storage allocation arises from desires for
    program modularity, machine independence, and resource sharing.
    Virtual memory is an elegant way of achieving these objectives. In
    a virtual memory, the addresses a program may use to identify
    information are distinguished from the addresses the memory system
    uses to identify physical storage sites, and program-generated
    addresses are translated automatically to the corresponding
    machine addresses. Two principal methods for implementing virtual
    memory, segmentation and paging, are compared and contrasted. Many
    contemporary implementations have experienced one or more of these
    problems: poor utilization of storage, thrashing, and high costs
    associated with loading information into memory. These and
    subsidiary problems are studied from a theoretic view, and are
    shown to be controllable by a proper combination of hardware and
    memory management policies.

* .. _DS72:

  P. J. Denning, S. C. Schwartz. 1972. "`Properties of the Working-set Model <http://denninginstitute.com/pjd/PUBS/WSProp_1972.pdf>`_". CACM. vol. 15, no. 3, pp. 191--198.

  .. admonition:: Abstract

    A program's working set *W*\ (*t*, *T*) at time *t* is the set of
    distinct pages among the *T* most recently referenced pages.
    Relations between the average working-set size, the missing-page
    rate, and the interreference-interval distribution may be derived
    both from time-average definitions and from ensemble-average
    (statistical) definitions. An efficient algorithm for estimating
    these quantities is given. The relation to LRU (least recently
    used) paging is characterized. The independent-reference model, in
    which page references are statistically independent, is used to
    assess the effects of interpage dependencies on working-set size
    observations. Under general assumptions, working-set size is shown
    to be normally distributed.

* .. _DETLEFS92:

  David L. Detlefs. 1992. "`Garbage collection and runtime typing as a C++ library <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.71.2755&rep=rep1&type=pdf>`_". USENIX C++ Conference.

  .. admonition:: From the introduction

    Automatic storage management, or *garbage collection*, is a
    feature that can ease program development and enhance program
    reliability. Many high-level languages other than C++ provide
    garbage collection. This paper proposes the use of "smart pointer"
    template classes as an interface for the use of garbage collection
    in C++. Template classes and operator overloading are techniques
    allowing language extension at the level of user code; I claim
    that using these techniques to create smart pointer classes
    provdes a syntax for manipulating garbage-collected storage safely
    and conveniently. Further, the use of a smart-pointer template
    class offers the possibility of implementing the collector at the
    user-level, without requiring support from the compiler. If such a
    compiler-independent implementation is possible with adequate
    performance, then programmers can start to write code using
    garbage collection without waiting for language and compiler
    modifications. If the use of such a garbage collection interface
    becomes widespread, then C++ compilation systems can be built to
    specially support tht garbage collection interface, thereby
    allowing the use of collection algorithms with enhanced
    performance.

* .. _ZORN93:

  David L. Detlefs, Al Dosser, Benjamin Zorn. 1994. "`Memory Allocation Costs in Large C and C++ Programs <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.30.3073&rep=rep1&type=pdf>`_". Software -- Practice and Experience. 24(6):527--542.

  .. admonition:: Abstract

    Dynamic storage allocation is an important part of a large class
    of computer programs written in C and C++. High-performance
    algorithms for dynamic storage allocation have been, and will
    continue to be, of considerable interest. This paper presents
    detailed measurements of the cost of dynamic storage allocation in
    11 diverse C and C++ programs using five very different dynamic
    storage allocation implementations, including a conservative
    garbage collection algorithm. Four of the allocator
    implementations measured are publicly-available on the Internet. A
    number of the programs used in these measurements are also
    available on the Internet to facilitate further research in
    dynamic storage allocation. Finally, the data presented in this
    paper is an abbreviated version of more extensive statistics that
    are also publicly-available on the Internet.

* .. _DB76:

  L. Peter Deutsch, Daniel G. Bobrow. 1976. "`An Efficient, Incremental, Automatic Garbage Collector <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.63.4603&rep=rep1&type=pdf>`_". CACM. vol. 19, no. 9, pp. 522--526.

  .. admonition:: Abstract

    This paper describes a new way of solving the storage reclamation
    problem for a system such as Lisp that allocates storage
    automatically from a heap, and does not require the programmer to
    give any indication that particular items are no longer useful or
    accessible. A reference count scheme for reclaiming
    non-self-referential structures, and a linearizing, compacting,
    copying scheme to reorganize all storage at the users discretion
    are proposed. The algorithms are designed to work well in systems
    which use multiple levels of storage, and large virtual address
    space. They depend on the fact that most cells are referenced
    exactly once, and that reference counts need only be accurate when
    storage is about to be reclaimed. A transaction file stores
    changes to reference counts, and a multiple reference table stores
    the count for items which are referenced more than once.

* .. _DLMSS76:

  E. W. Dijkstra, Leslie Lamport, A. J. Martin, C. S. Scholten, E. F. M. Steffens. 1976. "`On-the-fly Garbage Collection: An Exercise in Cooperation <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.63.4752&rep=rep1&type=pdf>`_". Springer-Verlag. Lecture Notes in Computer Science, Vol. 46.

  .. admonition:: Abstract

    As an example of cooperation between sequential processes with
    very little mutual interference despite frequent manipulations of
    a large shared data space, a technique is developed which allows
    nearly all of the activity needed for garbage detection and
    collection to be performed by an additional processor operating
    con- currently with the processor devoted to the computation
    proper. Exclusion and synchronization constraints have been kept
    as weak as could be achieved; the severe complexities engendered
    by doing so are illustrated.

* .. _DMH92:

  Amer Diwan, Richard L. Hudson, J. Eliot B. Moss. 1992. "`Compiler Support for Garbage Collection in a Statically Typed Language <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.87.71&rep=rep1&type=pdf>`_". ACM. Proceedings of the 5th ACM SIGPLAN conference on Programming language design and implementation, pp. 273--282.

  .. admonition:: Abstract

    We consider the problem of supporting compacting garbage
    collection in the presence of modern compiler optimizations. Since
    our collector may move any heap object, it must accurately locate,
    follow, and update all pointers and values derived from pointers.
    To assist the collector, we extend the compiler to emit tables
    describing live pointers, and values derived from pointers, at
    each program location where collection may occur. Significant
    results include identification of a number of problems posed by
    optimizations, solutions to those problems, a working compiler,
    and experimental data concerning table sizes, table compression,
    and time overhead of decoding tables during collection. While gc
    support can affect the code produced, our sample programs show no
    significant changes, the table sizes are a modest fraction of the
    size of the optimized code, and stack tracing is a small fraction
    of total gc time. Since the compiler enhancements are also modest,
    we conclude that the approach is practical.

* .. _DTM93:

  Amer Diwan, David Tarditi, J. Eliot B. Moss. 1993. "`Memory Subsystem Performance of Programs with Intensive Heap Allocation <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.27.9220&rep=rep1&type=pdf>`_". Carnegie Mellon University. CMU-CS-93-227.

  .. admonition:: Abstract

    Heap allocation with copying garbage collection is a general
    storage management technique for modern programming languages. It
    is believed to have poor memory subsystem performance. To
    investigate this, we conducted an in-depth study of the memory
    subsystem performance of heap allocation for memory subsystems
    found on many machines. We studied the performance of
    mostly-functional Standard ML programs which made heavy use of
    heap allocation. We found that most machines support heap
    allocation poorly. However, with the appropriate memory subsystem
    organization, heap allocation can have good performance. The
    memory subsystem property crucial for achieving good performance
    was the ability to allocate and initialize a new object into the
    cache without a penalty. This can be achieved by having subblock
    placement with a subblock size of one word with a write allocate
    policy, along with fast page-mode writes or a write buffer. For
    caches with subblock placement, the data cache overhead was under
    9% for a 64k or larger data cache; without subblock placement the
    overhead was often higher than 50%.

* .. _DTM93A:

  Amer Diwan, David Tarditi, J. Eliot B. Moss. 1994. "`Memory Subsystem Performance of Programs Using Copying Garbage Collection <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.27.9220&rep=rep1&type=pdf>`_". ACM. CMU-CS-93-210, also in POPL '94.

  .. admonition:: Abstract

    Heap allocation with copying garbage collection is believed to
    have poor memory subsystem performance. We conducted a study of
    the memory subsystem performance of heap allocation for memory
    subsystems found on many machines. We found that many machines
    support heap allocation poorly. However, with the appropriate
    memory subsystem organization, heap allocation can have good
    memory subsystem performance.

* .. _DOLIGEZ93:

  Damien Doligez & Xavier Leroy. 1993. "`A concurrent, generational garbage collector for a multithreaded implementation of ML <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.52.9494&rep=rep1&type=pdf>`_". ACM. POPL '93, 113--123.

  .. admonition:: Abstract

    This paper presents the design and implementation of a "quasi
    real-time" garbage collector for Concurrent Caml Light, an
    implementation of ML with threads. This two-generation system
    combines a fast, asynchronous copying collector on the young
    generation with a non-disruptive concurrent marking collector on
    the old generation. This design crucially relies on the ML
    compile-time distinction between mutable and immutable objects.

* .. _DOLIGEZ94:

  Damien Doligez & Georges Gonthier. 1994. "`Portable, unobtrusive garbage collection for multiprocessor systems <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.52.4710&rep=rep1&type=pdf>`_". ACM. POPL '94, 70--83.

  .. admonition:: Abstract

    We describe and prove the correctness of a new concurrent
    mark-and-sweep garbage collection algorithm. This algorithm
    derives from the classical on-the-fly algorithm from Dijkstra et
    al. A distinguishing feature of our algorithm is that it supports
    multiprocessor environments where the registers of running
    processes are not readily accessible, without imposing any
    overhead on the elementary operations of loading a register or
    reading or initializing a field. Furthermore our collector never
    blocks running mutator processes except possibly on requests for
    free memory; in particular, updating a field or creating or
    marking or sweeping a heap object does not involve
    system-dependent synchronization primitives such as locks. We also
    provide support for process creation and deletion, and for
    managing an extensible heap of variable-sized objects.

* .. _DBE93:

  R. Kent Dybvig, Carl Bruggeman, David Eby. 1993. "`Guardians in a Generation-Based Garbage Collector <http://www.cs.indiana.edu/~dyb/pubs/guardians-pldi93.pdf>`_". SIGPLAN. Proceedings of the ACM SIGPLAN '93 Conference on Programming Language Design and Implementation, June 1993.

  .. admonition:: Abstract

    This paper describes a new language feature that allows
    dynamically allocated objects to be saved from deallocation by an
    automatic storage management system so that clean-up or other
    actions can be performed using the data stored within the objects.
    The program has full control over the timing of clean-up actions,
    which eliminates several potential problems and often eliminates
    the need for critical sections in code that interacts with
    clean-up actions. Our implementation is "generation-friendly" in
    the sense that the additional overhead within the mutator is
    proportional to the number of clean-up actions actually performed.

* .. _EDELSON92A:

  Daniel R. Edelson. 1992. "`Smart pointers: They're smart, but they're not pointers <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.54.530&rep=rep1&type=pdf>`_". USENIX C++ Conference.

  .. admonition:: From the introduction

    This paper shows hhow the behaviour of smart pointers diverges
    from that of pointers in certain common C++ constructs. Given
    this, we conclude that the C++ programming language does not
    support seamless smart pointers: smart pointers cannot
    transparently replace raw pointers in all ways except declaration
    syntax. We show that this conclusion also applies to *accessors*.

* .. _EDELSON92:

  Daniel R. Edelson. 1992. "`Comparing Two Garbage Collectors for C++ <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.43.6011&rep=rep1&type=pdf>`_". University of California at Santa Cruz. Technical Report UCSC-CRL-93-20.

  .. admonition:: Abstract

    Our research is concerned with compiler- independent, tag-free
    garbage collection for the C++ programming language. This paper
    presents a mark-and-sweep collector, and explains how it
    ameliorates shortcomings of a previous copy collector. The new
    collector, like the old, uses C++'s facilities for creating
    abstract data types to define a *tracked reference* type, called
    *roots*, at the level of the application program. A programmer
    wishing to utilize the garbage collection service uses these roots
    in place of normal, raw pointers. We present a detailed study of
    the cost of using roots, as compared to both normal pointers and
    reference counted pointers, in terms of instruction counts. We
    examine the efficiency of a small C++ application using roots,
    reference counting, manual reclamation, and conservative
    collection. Coding the application to use garbage collection, and
    analyzing the resulting efficiency, helped us identify a number of
    memory leaks and inefficiencies in the original, manually
    reclaimed version. We find that for this program, garbage
    collection using roots is much more efficient than reference
    counting, though less efficient than manual reclamation. It is
    hard to directly compare our collector to the conservative
    collector because of the differing efficiencies of their
    respective memory allocators.

* .. _EDWARDS:

  Daniel J. Edwards. n.d. "`Lisp II Garbage Collector <ftp://publications.ai.mit.edu/ai-publications/0-499/AIM-019.ps>`_". MIT. AI Memo 19 (AIM-19).

  .. admonition:: Our summary

    (This short memo doesn't have an abstract. Basically, it describes
    the plan for the LISP II Relocating Garbage Collector. It has four
    phases: marking, collection, relocation and moving. Marking is by
    recursive descent using a bit table. The remaining phases are
    linear sweeps through the bit table. The collection phase
    calculates how much everything needs to move, storing this
    information in the free blocks. The relocation phase updates all
    relocatable addresses. The moving phase moves the surviving
    objects into one contiguous block.)

* .. _ELLIS93:

  John R. Ellis, David L. Detlefs. 1993. "`Safe, Efficient Garbage Collection for C++ <http://www.hpl.hp.com/techreports/Compaq-DEC/SRC-RR-102.pdf>`_". Xerox PARC.

  .. admonition:: Abstract

    We propose adding safe, efficient garbage collection to C++,
    eliminating the possibility of storage-management bugs and making
    the design of complex, object-oriented systems much easier. This
    can be accomplished with almost no change to the language itself
    and only small changes to existing implementations, while
    retaining compatibility with existing class libraries.

* .. _FERREIRA96:

  Paulo Ferreira. 1996. "`Larchant: garbage collection in a cached distributed shared store with persistence by reachability <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.31.8434&rep=rep1&type=pdf>`_". Université Paris VI. Thése de doctorat.

  .. admonition:: Abstract

    The model of Larchant is that of a *Shared Address Space*
    (spanning every site in a network including secondary storage)
    with *Persistence By Reachability*. To provide the illusion of a
    shared address space across the network, despite the fact that
    site memories are disjoint, Larchant implements a *distributed
    shared memory* mechanism. Reachability is accessed by tracing the
    pointer graph, starting from the persistent root, and reclaiming
    unreachable objects. This is the task of *Garbage Collection*
    (GC).

    GC was until recently thought to be intractable in a large-scale
    system, due to problems of scale, incoherence, asynchrony, and
    performance. This thesis presents the solutions that Larchant
    proposes to these problems.

    The GC algorithm in Larchant combines tracing and
    reference-listing. It traces whenever economically feasible, i.e.,
    as long as the memory subset being collected remains local to a
    site, and counts references that would cost I/O traffic to trace.
    GC is orthogonal to coherence, i.e., makes progress even if only
    incoherent replicas are locally available. The garbage collector
    runs concurrently and asynchronously to applications. The
    reference-listing boundary changes dynamically and seamlessly, and
    independently at each site, in order to collect cycles of
    unreachable objects.

    We prove formally that our GC algorithm is correct, i.e., it is
    safe and live. The performance results from our Larchant prototype
    show that our design goals (scalability, coherence orthogonality,
    and good performance) are fulfilled.

* .. _FS98:

  Paulo Ferreira & Marc Shapiro. 1998. "`Modelling a Distributed Cached Store for Garbage Collection <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.6176&rep=rep1&type=pdf>`_". Springer-Verlag. Proceedings of 12th European Conference on Object-Oriented Programming, ECOOP98, LNCS 1445.

  .. admonition:: Abstract

    Caching and persistence support efficient, convenient and
    transparent distributed data sharing. The most natural model of
    persistence is persistence by reachability, managed automatically
    by a garbage collector (GC). We propose a very general model of
    such a system (based on distributed shared memory) and a scalable,
    asynchronous distributed GC algorithm. Within this model, we show
    sufficient and widely applicable correctness conditions for the
    interactions between applications, store, memory, coherence, and
    GC.

    The GC runs as a set of processes (local to each participating
    machine) communicating by asynchronous messages. Collection does
    not interfere with applications by setting locks, polluting
    caches, or causing I/O; this requirement raised some novel and
    interesting challenges which we address in this article. The
    algorithm is safe and live; it is not complete, i.e. it collects
    some distributed cycles of garbage but not necessarily all.

* .. _FW76:

  Daniel P Friedman, David S. Wise. 1976. "`Garbage collecting a heap which includes a scatter table <http://www.cs.indiana.edu/pub/techreports/TR34.pdf>`_". *Information Processing Letters.* 5, 6 (December 1976): 161--164.

  .. admonition:: Abstract

    A new algorithm is introduced for garbage collecting a heap which
    contains shared data structures accessed from a scatter table. The
    scheme provides for the purging of useless entries from the
    scatter table with no traverslas beyond the two required by
    classic collection schemes. For languages which use scatter tables
    to sustain unique existence of complex structures, like natural
    variables of SNOBOL, it indirectly allows liberal use of a single
    scatter table by ensuring efficient deletion of useless entries.
    Since the scatter table is completely restructured during the
    course of execution, the hashing scheme itself is easily altered
    during garbage collection whenever skewed loading of the scatter
    table warrants abandonment of the old hashing. This procedure is
    applicable to the maintenance of dynamic structures such as those
    in information retrieval schemes or in languages like LISP and
    SNOBOL.

* .. _FW77:

  Daniel P Friedman, David S. Wise. 1977. "`The One Bit Reference Count <http://www.cs.indiana.edu/pub/techreports/TR57.pdf>`_". *BIT.* (17)3: 351--359.

  .. admonition:: Abstract

    Deutsch and Bobrow propose a storage reclamation scheme for a heap
    which is a hybrid of garbage collection and reference counting.
    The point of the hybrid scheme is to keep track of very low
    reference counts between necessary invocation of garbage
    collection so that nodes which are allocated and rather quickly
    abandoned can be returned to available space, delaying necessity
    for garbage collection. We show how such a scheme may be
    implemented using the mark bit already required in every node by
    the garbage collector. Between garbage collections that bit is
    used to distinguish nodes with a reference count known to be one.
    A significant feature of our scheme is a small cache of references
    to nodes whose implemented counts "ought to be higher" which
    prevents the loss of logical count information in simple
    manipulations of uniquely referenced structures.

* .. _FW79:

  Daniel P Friedman, David S. Wise. 1979. "`Reference counting can manage the circular environments of mutual recursion <http://www.cs.indiana.edu/pub/techreports/TR73.pdf>`_". *Information Processing Letters.* 8, 1 (January 1979): 41--45.

  .. admonition:: From the introduction

    In this note we advance reference counting as a storage management
    technique viable for implementing recursive languages like ISWIM
    or pure LISP with the ``labels`` construct for implementing mutual
    recursion from SCHEME. ``Labels`` is derived from ``letrec`` and
    displaces the ``label`` operator, a version of the paradoxical
    Y-combinator. The key observation is that the requisite circular
    structure (which ordinarily cripples reference counts) occurs only
    within the language--rather than the user--structure, and that the
    references into this structure are well-controlled.

* .. _GZH93:

  Dirk Grunwald, Benjamin Zorn, R. Henderson. 1993. "`Improving the Cache Locality of Memory Allocation <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.43.6621&rep=rep1&type=pdf>`_". SIGPLAN. SIGPLAN '93, Conference on PLDI, June 1993, Albuquerque, New Mexico.

  .. admonition:: Abstract

    The allocation and disposal of memory is a ubiquitous operation in
    most programs. Rarely do programmers concern themselves with
    details of memory allocators; most assume that memory allocators
    provided by the system perform well. This paper presents a
    performance evaluation of the reference locality of dynamic
    storage allocation algorithms based on trace-driven simulation of
    five large allocation-intensive C programs. In this paper, we show
    how the design of a memory allocator can significantly affect the
    reference locality for various applications. Our measurements show
    that poor locality in sequential-fit algorithms reduces program
    performance, both by increasing paging and cache miss rates. While
    increased paging can be debilitating on any architecture, cache
    misses rates are also important for modern computer architectures.
    We show that algorithms attempting to be space-efficient, by
    coalescing adjacent free objects show poor reference locality,
    possibly negating the benefits of space efficiency. At the other
    extreme, algorithms can expend considerable effort to increase
    reference locality yet gain little in total execution performance.
    Our measurements suggest an allocator design that is both very
    fast and has good locality of reference.

* .. _GRUN92:

  Dirk Grunwald & Benjamin Zorn. 1993. "`CustoMalloc: Efficient Synthesized Memory Allocators <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.35.5260&rep=rep1&type=pdf>`_". Software -- Practice and Experience. 23(8):851--869.

  .. admonition:: Abstract

    The allocation and disposal of memory is a ubiquitous operation in
    most programs. Rarely do programmers concern themselves with
    details of memory allocators; most assume that memory allocators
    provided by the system perform well. Yet, in some applications,
    programmers use domain-specific knowledge in an attempt to improve
    the speed or memory utilization of memory allocators. In this
    paper, we describe a program (CustoMalloc) that synthesizes a
    memory allocator customized for a specific application. Our
    experiments show that the synthesized allocators are uniformly
    faster than the common binary-buddy (BSD) allocator, and are more
    space efficient. Constructing a custom allocator requires little
    programmer effort. The process can usually be accomplished in a
    few minutes, and yields results superior even to domain-specific
    allocators designed by programmers. Our measurements show the
    synthesized allocators are from two to ten times faster than
    widely used allocators.

* .. _GUDEMAN93:

  David Gudeman. 1993. "`Representing Type Information in Dynamically Typed Languages <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.39.4394&rep=rep1&type=pdf>`_". University of Arizona at Tucson. Technical Report TR 93-27.

  .. admonition:: Abstract

    This report is a discussion of various techniques for representing
    type information in dynamically typed languages, as implemented on
    general-purpose machines (and costs are discussed in terms of
    modern RISC machines). It is intended to make readily available a
    large body of knowledge that currently has to be absorbed
    piecemeal from the literature or re-invented by each language
    implementor. This discussion covers not only tagging schemes but
    other forms of representation as well, although the discussion is
    strictly limited to the representation of type information. It
    should also be noted that this report does not purport to contain
    a survey of the relevant literature. Instead, this report gathers
    together a body of folklore, organizes it into a logical
    structure, makes some generalizations, and then discusses the
    results in terms of modern hardware.

* .. _HARRIS99:

  Timothy Harris. 1999. "`Early storage reclamation in a tracing garbage collector <http://www.timharris.co.uk/papers/1999-sigplan.pdf>`_". ACM. ACM SIG-PLAN Notices 34:4, pp. 46--53.

  .. admonition:: Abstract

    This article presents a technique for allowing the early recovery
    of storage space occupied by garbage data. The idea is similar to
    that of generational garbage collection, except that the heap is
    partitioned based on a static analysis of data type definitions
    rather than on the approximate age of allocated objects. A
    prototype implementation is presented, along with initial results
    and ideas for future work.

* .. _HENRIK94:

  Roger Henriksson. 1994. "Scheduling Real Time Garbage Collection". Department of Computer Science at Lund University. LU-CS-TR:94-129.

  .. admonition:: Abstract

    This paper presents a new model for scheduling the work of an
    incremental garbage collector in a system with hard real time
    requirements. The method utilizes the fact that just some of the
    processes in the system have to meet hard real time requirements
    and that these processes typically run periodically, a fact that
    we can make use of when scheduling the garbage collection. The
    work of the collector is scheduled to be performed in the pauses
    between the critical processes and is suspended when the processes
    with hard real time requirements run. It is shown that this
    approach is feasible for many real time systems and that it leaves
    the time-critical parts of the system undisturbed from garbage
    collection induced delays.

* .. _HENRIK96:

  Roger Henriksson. 1996. "`Adaptive Scheduling of Incremental Copying Garbage Collection for Interactive Applications <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.51.1554&rep=rep1&type=pdf>`_". NWPER96.

  .. admonition:: Abstract

    Incremental algorithms are often used to interleave the work of a
    garbage collector with the execution of an application program,
    the intention being to avoid long pauses. However, overestimating
    the worst-case storage needs of the program often causes all the
    garbage collection work to be performed in the beginning of the
    garbage collection cycles, slowing down the application program to
    an unwanted degree. This paper explores an approach to
    distributing the work more evenly over the garbage collection
    cycle.

* .. _HENRIKSSON98:

  Roger Henriksson. 1998. "`Scheduling Garbage Collection in Embedded Systems <http://lup.lub.lu.se/luur/download?func=downloadFile&recordOId=18921&fileOId=630830>`_". Department of Computer Science at Lund University. Ph.D. thesis.

  .. admonition:: Abstract

    The complexity of systems for automatic control and other
    safety-critical applications grows rapidly. Computer software
    represents an increasing part of the complexity. As larger systems
    are developed, we need to find scalable techniques to manage the
    complexity in order to guarantee high product quality. Memory
    management is a key quality factor for these systems. Automatic
    memory management, or garbage collection, is a technique that
    significantly reduces the complex problem of correct memory
    management. The risk of software errors decreases and development
    time is reduced.

    Garbage collection techniques suitable for interactive and soft
    real-time systems exist, but few approaches are suitable for
    systems with hard real-time requirements, such as control systems
    (embedded systems). One part of the problem is solved by
    incremental garbage collection algorithms, which have been
    presented before. We focus on the scheduling problem which forms
    the second part of the problem, i.e. how the work of a garbage
    collector should be scheduled in order to disturb the application
    program as little as possible. It is studied how a priori
    scheduling analysis of systems with automatic memory management
    can be made. The field of garbage collection research is thus
    joined with the field of scheduling analysis in order to produce a
    practical synthesis of the two fields.

    A scheduling strategy is presented that employs the properties of
    control systems to ensure that no garbage collection work is
    performed during the execution of critical processes. The hard
    real-time part of the system is thus never disturbed by garbage
    collection work. Existing incremental garbage collection
    algorithms are adapted to the presented strategy. Necessary
    modifications of the algorithms and the real-time kernel are
    discussed. A standard scheduling analysis technique, rate
    monotonic analysis, is extended in order to make a priori analysis
    of the schedulability of the garbage collector possible.

    The scheduling algorithm has been implemented in an industrially
    relevant real-time environment in order to show that the strategy
    is feasible in practice. The experimental evaluation shows that
    predictable behaviour and sub-millisecond worst-case delays can be
    achieved on standard hardware even by a non-optimized prototype
    garbage collector.

* .. _HOSKING91:

  Antony L. Hosking. 1991. "`Main memory management for persistence <ftp://ftp.cs.purdue.edu/pub/hosking/papers/oopsla91gc-alh.pdf>`_". ACM. Proceedings of the ACM OOPSLA'91 Workshop on Garbage Collection.

  .. admonition:: Abstract

    Reachability-based persistence imposes new requirements for main
    memory management in general, and garbage collection in
    particular. After a brief introduction to the characteristics and
    requirements of reachability-based persistence, we present the
    design of a run-time storage manager for Persistent Smalltalk and
    Persistent Modula-3, which allows the reclamation of storage from
    both temporary objects and buffered persistent objects.

* .. _HMS92:

  Antony L. Hosking, J. Eliot B. Moss, Darko Stefanovic. 1992. "`A comparative performance evaluation of write barrier implementations <ftp://ftp.cs.purdue.edu/pub/hosking/papers/oopsla92.pdf>`_". ACM. OOPSLA'92 Conference Proceedings, ACM SIGPLAN Notices 27(10), pp 92--109.

  .. admonition:: Abstract

    Generational garbage collectors are able to achieve very small
    pause times by concentrating on the youngest (most recently
    allocated) objects when collecting, since objects have been
    observed to die young in many systems. Generational collectors
    must keep track of all pointers from older to younger generations,
    by “monitoring” all stores into the heap. This *write barrier* has
    been implemented in a number of ways, varying essentially in the
    granularity of the information observed and stored. Here we
    examine a range of write barrier implementations and evaluate
    their relative performance within a generation scavenging garbage
    collector for Smalltalk.

* .. _HH93:

  Antony L. Hosking, Richard L. Hudson. 1993. "`Remembered sets can also play cards <ftp://ftp.cs.purdue.edu/pub/hosking/papers/gc-workshop93c.pdf>`_". ACM. Proceedings of the ACM OOPSLA'93 Workshop on Memory Management and Garbage Collection.

  .. admonition:: Abstract

    Remembered sets and dirty bits have been proposed as alternative
    implementations of the write barrier for garbage collection. There
    are advantages to both approaches. Dirty bits can be efficiently
    maintained with minimal, bounded overhead per store operation,
    while remembered sets concisely, and accurately record the
    necessary information. Here we present evidence to show that
    hybrids can combine the virtues of both schemes and offer
    competitive performance. Moreover, we argue that a hybrid can
    better avoid the devils that are the downfall of the separate
    alternatives.

* .. _HM93:

  Antony L. Hosking, J. Eliot B. Moss. 1993. "`Protection traps and alternatives for memory management of an object-oriented language <ftp://ftp.cs.purdue.edu/pub/hosking/papers/sosp93.pdf>`_". ACM. Proceedings of the Fourteenth ACM Symposium on Operating Systems Principles, ACM Operating Systems Review 27(5), pp 106--119.

  .. admonition:: Abstract

    Many operating systems allow user programs to specify the
    protection level (inaccessible, read-only, read-write) of pages in
    their virtual memory address space, and to handle any protection
    violations that may occur. Such page-protection techniques have
    been exploited by several user-level algorithms for applications
    including generational garbage collection and persistent stores.
    Unfortunately, modern hardware has made efficient handling of page
    protection faults more difficult. Moreover, page-sized granularity
    may not match the natural granularity of a given application. In
    light of these problems, we reevaluate the usefulness of
    page-protection primitives in such applications, by comparing the
    performance of implementations that make use of the primitives
    with others that do not. Our results show that for certain
    applications software solutions outperform solutions that rely on
    page-protection or other related virtual memory primitives.

* .. _HMDW91:

  Richard L. Hudson, J. Eliot B. Moss, Amer Diwan, Christopher F. Weight. 1991. "`A Language-Independent Garbage Collector Toolkit <http://scholarworks.umass.edu/cgi/viewcontent.cgi?article=1210&context=cs_faculty_pubs>`_". University of Massachusetts at Amherst. COINS Technical Report 91--47.

  .. admonition:: Abstract

    We describe a memory management toolkit for language implementors.
    It offers efficient and flexible generation scavenging garbage
    collection. In addition to providing a core of
    language-independent algorithms and data structures, the toolkit
    includes auxiliary components that ease implementation of garbage
    collection for programming languages. We have detailed designs for
    Smalltalk and Modula-3 and are confident the toolkit can be used
    with a wide variety of languages. The toolkit approach is itself
    novel, and our design includes a number of additional innovations
    in flexibility, efficiency, accuracy, and cooperation between the
    compiler and the collector.

* .. _HM92:

  Richard L. Hudson, J. Eliot B. Moss. 1992. "`Incremental Collection of Mature Objects <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.3883&rep=rep1&type=pdf>`_". Springer-Verlag. LNCS #637  International Workshop on Memory Management, St. Malo, France, Sept. 1992, pp. 388--403.

  .. admonition:: Abstract

    We present a garbage collection algorithm that extends
    generational scavenging to collect large older generations (mature
    objects) non-disruptively. The algorithm's approach is to process
    bounded-size pieces of mature object space at each collection; the
    subtleties lie in guaranteeing that it eventually collects any and
    all garbage. The algorithm does not assume any special hardware or
    operating system support, e.g., for forwarding pointers or
    protection traps. The algorithm copies objects, so it naturally
    supports compaction and reclustering.

* .. _HMMM97:

  Richard L. Hudson, Ron Morrison, J. Eliot B. Moss, David S. Munro. 1997. "`Garbage Collecting the World: One Car at a Time <http://www.cs.umass.edu/~moss/papers/oopsla-1997-gc-world.pdf>`_". ACM. Proc. OOPSLA 97, pp. 162--175.

  .. admonition:: Abstract

    A new garbage collection algorithm for distributed object systems,
    called DMOS (Distributed Mature Object Space), is presented. It is
    derived from two previous algorithms, MOS (Mature Object Space),
    sometimes called the train algorithm, and PMOS (Persistent Mature
    Object Space). The contribution of DMOS is that it provides the
    following unique combination of properties for a distributed
    collector: safety, completeness, non-disruptiveness,
    incrementality, and scalability. Furthermore, the DMOS collector
    is non-blocking and does not use global tracing.

* .. _JOHNSTONE97:

  Mark S. Johnstone. 1997. "`Non-Compacting Memory Allocation and Real-Time Garbage Collection <ftp://ftp.cs.utexas.edu/pub/garbage/johnstone-dissertation.ps.gz>`_". University of Texas at Austin.

  .. admonition:: Abstract

    Dynamic memory use has been widely recognized to have profound
    effects on program performance, and has been the topic of many
    research studies over the last forty years. In spite of years of
    research, there is considerable confusion about the effects of
    dynamic memory allocation. Worse, this confusion is often
    unrecognized, and memory allocators are widely thought to be
    fairly well understood.

    In this research, we attempt to clarify many issues for both
    manual and automatic non-moving memory management. We show that
    the traditional approaches to studying dynamic memory allocation
    are unsound, and develop a sound methodology for studying this
    problem. We present experimental evidence that fragmentation costs
    are much lower than previously recognized for most programs, and
    develop a framework for understanding these results and enabling
    further research in this area. For a large class of programs using
    well-known allocation policies, we show that fragmentation costs
    are near zero. We also study the locality effects of memory
    allocation on programs, a research area that has been almost
    completely ignored. We show that these effects can be quite
    dramatic, and that the best allocation policies in terms of
    fragmentation are also among the best in terms of locality at both
    the cache and virtual memory levels of the memory hierarchy.

    We extend these fragmentation and locality results to real-time
    garbage collection. We have developed a hard real-time,
    non-copying generational garbage collector which uses a
    write-barrier to coordinate collection work only with
    modifications of pointers, therefore making coordination costs
    cheaper and more predictable than previous approaches. We combine
    this write-barrier approach with implicit non-copying reclamation,
    which has most of the advantages of copying collection (notably
    avoiding both the sweep phase required by mark-sweep collectors,
    and the referencing of garbage objects when reclaiming their
    space), without the disadvantage of having to actually copy the
    objects. In addition, we present a model for non-copying
    implicit-reclamation garbage collection. We use this model to
    compare and contrast our work with that of others, and to discuss
    the tradeoffs that must be made when developing such a garbage
    collector.

* .. _JW98:

  Mark S. Johnstone, Paul R. Wilson. 1998. "`The Memory Fragmentation Problem: Solved? <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.3382&rep=rep1&type=pdf>`_". ACM. ISMM'98 pp. 26--36.

  .. admonition:: Abstract

    We show that for 8 real and varied C and C++ programs, several
    conventional dynamic storage allocators provide near-zero
    fragmentation, once overheads due to implementation details
    (headers, alignment, etc.) are properly accounted for. This
    substantially strengthens our previous results showing that the
    memory fragmentation problem has generally been misunderstood, and
    that good allocator policies can provide good memory usage for
    most programs. The new results indicate that for most programs,
    excellent allocator policies are readily available, and efficiency
    of implementation is the major challenge. While we believe that
    our experimental results are state-of-the-art and our methodology
    is superior to most previous work, more work should be done to
    identify and study unusual problematic program behaviors not
    represented in our sample.

* .. _JONES92:

  Richard E. Jones. 1992. "`Tail recursion without space leaks <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.5083&rep=rep1&type=pdf>`_". *Journal of Functional Programming.* 2(1):73--79.

  .. admonition:: Abstract

    The G-machine is a compiled graph reduction machine for lazy
    functional languages. The G-machine compiler contains many
    optimisations to improve performance. One set of such
    optimisations is designed to improve the performance of tail
    recursive functions. Unfortunately the abstract machine is subject
    to a space leak--objects are unnecessarily preserved by the
    garbage collector.

    This paper analyses why a particular form of space leak occurs in
    the G-machine, and presents some ideas for fixing this problem.
    This phenomena in other abstract machines is also examined
    briefly.

* .. _JL92:

  Richard E. Jones, Rafael Lins. 1992. "`Cyclic weighted reference counting without delay <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.43.8499&rep=rep1&type=pdf>`_". Computing Laboratory, The University of Kent at Canterbury. Technical Report 28-92.

  .. admonition:: Abstract

    Weighted Reference Counting is a low-communication distributed
    storage reclamation scheme for loosely-coupled multiprocessors.
    The algorithm we present herein extends weighted reference
    counting to allow the collection of cyclic data structures. To do
    so, the algorithm identifies candidate objects that may be part of
    cycles and performs a tricolour mark-scan on their subgraph in a
    lazy manner to discover whether the subgraph is still in use. The
    algorithm is concurrent in the sense that multiple useful
    computation processes and garbage collection processes can be
    performed simultaneously.

* .. _JONES96:

  Richard E. Jones, Rafael Lins. 1996. "`Garbage Collection: Algorithms for Automatic Dynamic Memory Management <http://www.cs.ukc.ac.uk/people/staff/rej/gcbook/gcbook.html>`_". Wiley. ISBN 0-471-94148-4.

  .. admonition:: From the back cover

    The memory storage requirements of complex programs are extremely
    difficult to manage correctly by hand. A single error may lead to
    indeterminate and inexplicable program crashes. Worse still,
    failures are often unrepeatable and may surface only long after
    the program has been delivered to the customer. The eradication of
    memory errors typically consumes a substantial amount of
    development time. And yet the answer is relatively easy -- garbage
    collection; removing the clutter of memory management from module
    interfaces, which then frees the programmer to concentrate on the
    problem at hand rather than low-level book-keeping details. For
    this reason, most modern object-oriented languages such as
    Smalltalk, Eiffel, Java and Dylan, are supported by garbage
    collection. Garbage collecting libraries are even available for
    such uncooperative languages as C and C++.

    This book considers how dynamic memory can be recycled
    automatically to guarantee error-free memory management. There is
    an abundant but disparate literature on the subject, largely
    confined to research papers. This book sets out to pool this
    experience in a single accessible and unified framework.

    Each of the important algorithms is explained in detail, often
    with illustrations of its characteristic features and animations
    of its use. Techniques are described and compared for declarative
    and imperative programming styles, for sequential, concurrent and
    distributed architectures.

    For professionals developing programs from simple software tools
    to complex systems, as well as for researchers and students
    working in compiler construction, functional, logic and
    object-oriented programming design, this book will provide not
    only a clear introduction but also a convenient reference source
    for modern garbage collection techniques.

* .. _ACM98:

  Richard E. Jones. 1998. "`ISMM'98 International Symposium on Memory Management <http://www.acm.org/pubs/contents/proceedings/plan/286860/>`_". ACM. ISBN 1-58113-114-3.

  .. admonition:: From the Preface

    The International Symposium on Memory Management is a forum for
    research in several related areas of memory management, especially
    garbage collectors and dynamic storage allocators. [...] The
    nineteen papers selected for publication in this volume cover a
    remarkably broad range of memory management topics from explicit
    malloc-style allocation to automatic memory management, from
    cache-conscious data layout to efficient management of distributed
    references, from conservative to type-accurate garbage collection,
    for applications ranging from user application to long-running
    servers, supporting languages as different as C, C++, Modula-3,
    Java, Eiffel, Erlang, Scheme, ML, Haskell and Prolog.

* .. _JONES12:

  Richard E. Jones, Antony Hosking, and Eliot Moss. 2012. "`The Garbage Collection Handbook <http://gchandbook.org/>`_". Chapman & Hall.

* .. _JOYNER96:

  Ian Joyner. 1996. "`C++??: A Critique of C++ <http://www.emu.edu.tr/aelci/Courses/D-318/D-318-Files/cppcrit/index.htm>`_.".

  .. admonition:: Abstract

    The C++?? Critique is an analysis of some of the flaws of C++. It
    is by no means exhaustive, nor does it attempt to document every
    little niggle with C++, rather concentrating on main themes. The
    critique uses Java and Eiffel as comparisons to C++ to give a more
    concrete feel to the criticisms, viewing conceptual differences
    rather than syntactic ones as being more important. Some C++
    authors realising there are glaring deficiencies in C++ have
    chosen to defend C++ by also being critical within their own work.
    Most notable are Bjarne Stroustup's "Design and Evolution of C++,"
    and Scott Meyers' "Effective" and "More Effective C++." These warn
    of many traps and pitfalls, but reach the curious conclusion that
    since "good" C++ programmers are aware of these problems and know
    how to avoid them, C++ is alright.

    The C++ critique makes many of the same criticisms, but comes to
    the different conclusion that these pitfalls are not acceptable,
    and should not be in a language used for modern large scale
    software engineering. Clean design is more important than after
    the fact warnings, and it is inconceivable that purchasers of end
    user software would tolerate this tactic on the part of vendors.
    The critique also takes a look at C, and concludes that many of
    the features of C should be left out of modern languages, and that
    C is a flawed base for a language.

* .. _KANEFSKY89:

  Bob Kanefsky. 1989. "`Recursive Memory Allocation <http://www.songworm.com/db/songworm-parody/RecursiveMemoryAllocation.html>`_". Bob Kanefsky. Songworm 3, p.?.

* .. _KQH98:

  Jin-Soo Kim, Xiaohan Qin, Yarsun Hsu. 1998. "`Memory Characterization of a Parallel Data Mining Workload <http://csl.skku.edu/papers/wwc98.pdf>`_". IEEE. Proc. Workload Characterization: Methodology and Case Studies, pp. .

  .. admonition:: Abstract

    This paper studies a representative of an important class of
    emerging applications, a parallel data mining workload. The
    application, extracted from the IBM Intelligent Miner, identifies
    groups of records that are mathematically similar based on a
    neural network model called self-organizing map. We examine and
    compare in details two implementations of the application:
    (1) temporal locality or working set sizes; (2) spatial locality
    and memory block utilization; (3) communication characteristics
    and scalability; and (4) TLB performance.

    First, we find that the working set hierarchy of the application
    is governed by two parameters, namely the size of an input record
    and the size of prototype array; it is independent of the number
    of input records. Second, the application shows good spatial
    locality, with the implementation optimized for sparse data sets
    having slightly worse spatial locality. Third, due to the batch
    update scheme, the application bears very low communication.
    Finally, a 2-way set associative TLB may result in severely skewed
    TLB performance in a multiprocessor environment caused by the
    large discrepancy in the amount of conflict misses. Increasing the
    set associativity is more effective in mitigating the problem than
    increasing the TLB size.

* .. _KH00:

  Jin-Soo Kim & Yarsun Hsu. 2000. "Memory system behavior of Java programs: methodology and analysis". ACM. Proc. International conference on measurements and modeling of computer systems, pp. 264--274.

  .. admonition:: Abstract

    This paper studies the memory system behavior of Java programs by
    analyzing memory reference traces of several SPECjvm98
    applications running with a Just-In-Time (JIT) compiler. Trace
    information is collected by an exception-based tracing tool called
    JTRACE, without any instrumentation to the Java programs or the
    JIT compiler.First, we find that the overall cache miss ratio is
    increased due to garbage collection, which suffers from higher
    cache misses compared to the application. We also note that going
    beyond 2-way cache associativity improves the cache miss ratio
    marginally. Second, we observe that Java programs generate a
    substantial amount of short-lived objects. However, the size of
    frequently-referenced long-lived objects is more important to the
    cache performance, because it tends to determine the application's
    working set size. Finally, we note that the default heap
    configuration which starts from a small initial heap size is very
    inefficient since it invokes a garbage collector frequently.
    Although the direct costs of garbage collection decrease as we
    increase the available heap size, there exists an optimal heap
    size which minimizes the total execution time due to the
    interaction with the virtual memory performance.

* .. _KOLODNER92:

  Elliot K. Kolodner. 1992. "Atomic Incremental Garbage Collection and Recovery for a Large Stable Heap". Laboratory for Computer Science at MIT. MIT-LCS-TR-534.

  .. admonition:: Abstract

    A stable heap is a storage that is managed automatically using
    garbage collection, manipulated using atomic transactions, and
    accessed using a uniform storage model. These features enhance
    reliability and simplify programming by preventing errors due to
    explicit deallocation, by masking failures and concurrency using
    transactions, and by eliminating the distinction between accessing
    temporary storage and permanent storage. Stable heap management is
    useful for programming language for reliable distributed
    computing, programming languages with persistent storage, and
    object-oriented database systems. Many applications that could
    benefit from a stable heap (e.g., computer-aided design,
    computer-aided software engineering, and office information
    systems) require large amounts of storage, timely responses for
    transactions, and high availability. We present garbage collection
    and recovery algorithms for a stable heap implementation that meet
    these goals and are appropriate for stock hardware. The collector
    is incremental: it does not attempt to collect the whole heap at
    once. The collector is also atomic: it is coordinated with the
    recovery system to prevent problems when it moves and modifies
    objects . The time for recovery is independent of heap size, and
    can be shortened using checkpoints.

* .. _LK98:

  Per-Åke Larson & Murali Krishnan. 1998. "`Memory Allocation for Long-Running Server Applications <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.1947&rep=rep1&type=pdf>`_". ACM. ISMM'98 pp. 176--185.

  .. admonition:: Abstract

    Prior work on dynamic memory allocation has largely neglected
    long-running server applications, for example, web servers and
    mail servers. Their requirements differ from those of one-shot
    applications like compilers or text editors. We investigated how
    to build an allocator that is not only fast and memory efficient
    but also scales well on SMP machines. We found that it is not
    sufficient to focus on reducing lock contention. Only limited
    improvement can be achieved this way; higher speedups require a
    reduction in cache misses and cache invalidation traffic. We then
    designed and prototyped a new allocator, called Lkmalloc, targeted
    for both traditional applications and server applications.
    LKmalloc uses several subheaps, each one with a separate set of
    free lists and memory arena. A thread always allocates from the
    same subheap but can free a block belonging to any subheap. A
    thread is assigned to a subheap by hashing on its thread ID. We
    compared its performance with several other allocators on a
    server-like, simulated workload and found that it indeed scales
    well and is quite fast but could use memory more efficiently.

* .. _LH83:

  Henry Lieberman & Carl Hewitt. 1983. "`A real-time garbage collector based on the lifetimes of objects <http://web.media.mit.edu/~lieber/Lieberary/GC/Realtime/Realtime.html>`_". ACM. 26(6):419--429.

  .. admonition:: Abstract

    In previous heap storage systems, the cost of creating objects and
    garbage collection is independent of the lifetime of the object.
    Since objects with short lifetimes account for a large portion of
    storage use, it is worth optimizing a garbage collector to reclaim
    storage for these objects more quickly. The garbage collector
    should spend proportionately less effort reclaiming objects with
    longer lifetimes. We present a garbage collection algorithm that
    (1) makes storage for short-lived objects cheaper than storage for
    long-lived objects, (2) that operates in real-time--object
    creation and access times are bounded, (3) increases locality of
    reference, for better virtual memory performance, (4) works well
    with multiple processors and a large address space.

* .. _MM59:

  J. McCarthy, M. L. Minsky. 1959. "`Artificial Intelligence, Quarterly Progress Report no. 53 <http://dspace.mit.edu/bitstream/handle/1721.1/52263/RLE_QPR_053_XIII.pdf>`_". Research Laboratory of Electronics at MIT.

* .. _MCCARTHY60:

  J. McCarthy. 1960. "`Recursive Functions of Symbolic Expressions and Their Computation by Machine <http://www-formal.stanford.edu/jmc/recursive.html>`_". CACM.

  .. admonition:: Abstract

    A programming system called LISP (for LISt Processor) has been
    developed for the IBM 704 computer by the Artificial Intelligence
    group at M.I.T. The system was designed to facilitate experiments
    with a proposed system called the Advice Taker, whereby a machine
    could be instructed to handle declarative as well as imperative
    sentences and could exhibit "common sense" in carrying out its
    instructions. The original proposal for the Advice Taker was made
    in November 1958. The main requirement was a programming system
    for manipulating expressions representing formalized declarative
    and imperative sentences so that the Advice Taker could make
    deductions.

    In the course of its development the LISP system went through
    several stages of simplification and eventually came to be based
    on a scheme for representing the partial recursive functions of a
    certain class of symbolic expressions. This representation is
    independent of the IBM 704 computer, or of any other electronic
    computer, and it now seems expedient to expound the system by
    starting with the class of expressions called S-expressions and
    the functions called S-functions.

* .. _MCCARTHY79:

  John McCarthy. 1979. "`History of Lisp <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.16.4634&rep=rep1&type=pdf>`_". In *History of programming languages I*, pp. 173--185. ACM.

* .. _PTM98:

  Veljko Milutinovic, Jelica Protic, Milo Tomasevic. 1997. "`Distributed shared memory: concepts and systems <http://www.cs.umass.edu/~mcorner/courses/691J/papers/VM/protic_dsm/protic_dsm.pdf>`_". IEEE Computer Society Press. ISBN 0-8186-7737-6.

  .. admonition:: From the publisher's catalog

    Presents a survey of both distributed shared memory (DSM) efforts
    and commercial DSM systems. The book discusses relevant issues
    that make the concept of DSM one of the most attractive approaches
    for building large-scale, high-performance multiprocessor systems.
    Its text provides a general introduction to the DSM field as well
    as a broad survey of the basic DSM concepts, mechanisms, design
    issues, and systems.

    Distributed Shared Memory concentrates on basic DSM algorithms,
    their enhancements, and their performance evaluation. In addition,
    it details implementations that employ DSM solutions at the
    software and the hardware level. The book is a research and
    development reference that provides state-of-the art information
    that will be useful to architects, designers, and programmers of
    DSM systems.

* .. _MINSKY63:

  M. L. Minsky. 1963. "`A LISP Garbage Collector Algorithm Using Serial Secondary Storage <http://dspace.mit.edu/bitstream/handle/1721.1/6080/AIM-058.pdf>`_". MIT. Memorandum MAC-M-129, Artificial Intelligence Project, Memo 58 (revised).

  .. admonition:: Abstract

    This paper presents an algorithm for reclaiming unused free
    storage memory cells is LISP. It depends on availability of a fast
    secondary storage device, or a large block of available temporary
    storage. For this price, we get 1. Packing of free-storage into a
    solidly packed block. 2. Smooth packing of arbitrary linear blocks
    and arrays. 3. The collector will handle arbitrarily complex
    re-entrant list structure with no introduction of spurious copies.
    4. The algorithm is quite efficient; the marking pass visits words
    at most twice and usually once, and the loading pass is linear.
    5. The system is easily modified to allow for increase in size of
    already fixed consecutive blocks, provide one can afford to
    initiate a collection pass or use a modified array while waiting
    for such a pass to occur.

* .. _MOON84:

  David Moon. 1984. "`Garbage Collection in a Large Lisp System <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.125.2438&rep=rep1&type=pdf>`_". ACM. Symposium on Lisp and Functional Programming, August 1984.

  .. admonition:: Abstract

    This paper discusses garbage collection techniques used in a
    high-performance Lisp implementation with a large virtual memory,
    the Symbolics 3600. Particular attention is paid to practical
    issues and experience. In a large system problems of scale appear
    and the most straightforward garbage-collection techniques do not
    work well. Many of these problems involve the interaction of the
    garbage collector with demand-paged virtual memory. Some of the
    solutions adopted in the 3600 are presented, including incremental
    copying garbage collection, approximately depth-first copying,
    ephemeral objects, tagged architecture, and hardware assists. We
    discuss techniques for improving the efficiency of garbage
    collection by recognizing that objects in the Lisp world have a
    variety of lifetimes. The importance of designing the architecture
    and the hardware to facilitate garbage collection is stressed.

* .. _MOON85:

  David Moon. 1985. "Architecture of the Symbolics 3600". IEEE. 12th International Symposium on Computer Architecture, pp. 76--83.

* .. _MOON87:

  David Moon. 1990. "Symbolics Architecture". Wiley. Chapter 3 of *Computers for Artificial Intelligence Processing*, ISBN 0-471-84811-5.

* .. _MOON91:

  David Moon. 1991. "Genera Retrospective". IEEE. 1991 International Workshop on Object Orientation in Operating Systems, order #2265.

* .. _MORDEC84:

  Ben-Ari Mordechai. 1984. "Algorithms for On-the-fly Garbage Collection". *TOPLAS* 6(3): 333--344 (1984).

* .. _MOREAU98:

  Luc Moreau. 1998. "`Hierarchical Distributed Reference Counting <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.49.4593&rep=rep1&type=pdf>`_". ACM. ISMM'98 pp. 57--67.

  .. admonition:: Abstract

    Massively distributed computing is a challenging problem for
    garbage collection algorithm designers as it raises the issue of
    scalability. The high number of hosts involved in a computation
    can require large tables for reference listing, whereas the lack
    of information sharing between hosts in a same locality can entail
    redundant GC traffic. In this paper, we argue that a conceptual
    hierarchical organisation of massive distributed computations can
    solve this problem. By conceptual hierarchical organisation, we
    mean that processors are still able to communicate in a peer to
    peer manner using their usual communication mechanism, but GC
    messages will be routed as if processors were organised in
    hierarchy. We present an extension of a distributed reference
    counting algorithm that uses such a hierarchical organisation. It
    allows us to bound table sizes by the number of hosts in a domain,
    and it allows us to share GC information between hosts in a same
    locality in order to reduce cross-network GC traffic.

* .. _MFH95:

  Greg Morrisett, Matthias Felleisen, Robert Harper. 1995. "`Abstract Models of Memory Management <http://www.eecs.harvard.edu/~greg/papers/fpca_gc.ps>`_". Carnegie Mellon University. CMU-CS-FOX-95-01.

  .. admonition:: Abstract

    Most specifications of garbage collectors concentrate on the
    low-level algorithmic details of how to find and preserve
    accessible objects. Often, they focus on bit-level manipulations
    such as "scanning stack frames," "marking objects," "tagging
    data," etc. While these details are important in some contexts,
    they often obscure the more fundamental aspects of memory
    management: what objects are garbage and why?

    We develop a series of calculi that are just low-level enough that
    we can express allocation and garbage collection, yet are
    sufficiently abstract that we may formally prove the correctness
    of various memory management strategies. By making the heap of a
    program syntactically apparent, we can specify memory actions as
    rewriting rules that allocate values on the heap and automatically
    dereference pointers to such objects when needed. This formulation
    permits the specification of garbage collection as a relation that
    removes portions of the heap without affecting the outcome of
    evaluation.

    Our high-level approach allows us to specify in a compact manner a
    wide variety of memory management techniques, including standard
    trace-based garbage collection (i.e., the family of copying and
    mark/sweep collection algorithms), generational collection, and
    type-based, tag-free collection. Furthermore, since the definition
    of garbage is based on the semantics of the underlying language
    instead of the conservative approximation of inaccessibility, we
    are able to specify and prove the idea that type inference can be
    used to collect some objects that are accessible but never used.

* .. _MBMM99:

  David S. Munro, Alfred Brown, Ron Morrison, J. Eliot B. Moss. 1999. "`Incremental Garbage Collection of a Persistent Object Store using PMOS <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.36.3687&rep=rep1&type=pdf>`_". Morgan Kaufmann. in Advances in Persistent Object Systems, pp. 78--91.

  .. admonition:: Abstract

    PMOS is an incremental garbage collector designed specifically to
    reclaim space in a persistent object store. It is complete in that
    it will, after a finite number of invocations, reclaim all
    unreachable storage. PMOS imposes minimum constraints on the order
    of collection and offers techniques to reduce the I/O traffic
    induced by the collector. Here we present the first implementation
    of the PMOS collector called PMOS#1. The collector has been
    incorporated into the stable heap layer of the generic persistent
    object store used to support a number of languages including
    Napier88. Our main design goals are to maintain the independence
    of the language from the store and to retain the existing store
    interface. The implementation has been completed and tested using
    a Napier88 system. The main results of this work show that the
    PMOS collector is implementable in a persistent store and that it
    can be built without requiring changes to the language
    interpreter. Initial performance measurements are reported. These
    results suggest however, that effective use of PMOS requires
    greater co-operation between language and store.

* .. _NOPH92:

  Scott Nettles, James O'Toole, David Pierce, Nickolas Haines. 1992. "`Replication-Based Incremental Copying Collection <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.28.4233&rep=rep1&type=pdf>`_". IWMM'92.

  .. admonition:: Abstract

    We introduce a new replication-based copying garbage collection
    technique. We have implemented one simple variation of this method
    to provide incremental garbage collection on stock hardware with
    no special operating system or virtual memory support. The
    performance of the prototype implementation is excellent: major
    garbage collection pauses are completely eliminated with only a
    slight increase in minor collection pause times.

    Unlike the standard copying algorithm, the replication-based
    method does not destroy the original replica when a copy is
    created. Instead, multiple copies may exist, and various standard
    strategies for maintaining consistency may be applied. In our
    implementation for Standard ML of New Jersey, the mutator
    continues to use the from-space replicas until the collector has
    achieved a consistent replica of all live data in to-space.

    We present a design for a concurrent garbage collector using the
    replication-based technique. We also expect replication-based GC
    methods to be useful in providing services for persistence and
    distribution, and briefly discuss these possibilities.

* .. _NETTLES92:

  Scott Nettles. 1992. "`A Larch Specification of Copying Garbage Collection <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.1498&rep=rep1&type=pdf>`_". Carnegie Mellon University. CMU-CS-92-219.

  .. admonition:: Abstract

    Garbage collection (GC) is an important part of many language
    implementations. One of the most important garbage collection
    techniques is copying GC. This paper consists of an informal but
    abstract description of copying collection, a formal specification
    of copying collection written in the Larch Shared Language and the
    Larch/C Interface Language, a simple implementation of a copying
    collector written in C, an informal proof that the implementation
    satisfies the specification, and a discussion of how the
    specification applies to other types of copying GC such as
    generational copying collectors. Limited familiarity with copying
    GC or Larch is needed to read the specification.

* .. _NO93A:

  Scott Nettles & James O'Toole. 1993. "Implementing Orthogonal Persistence: A Simple Optimization Using Replicating Collection". USENIX. IWOOOS'93.

  .. admonition:: Abstract

    Orthogonal persistence provides a safe and convenient model of
    object persistence. We have implemented a transaction system which
    supports orthogonal persistence in a garbage-collected heap. In
    our system, replicating collection provides efficient concurrent
    garbage collection of the heap. In this paper, we show how
    replicating garbage collection can also be used to reduce commit
    operation latencies in our implementation.

    We describe how our system implements transaction commit. We
    explain why the presence of non-persistent objects can add to the
    cost of this operation. We show how to eliminate these additional
    costs by using replicating garbage collection. The resulting
    implementation of orthogonal persistence should provide
    transaction performance that is independent of the quantity of
    non-persistent data in use. We expect efficient support for
    orthogonal persistence to be valuable in operating systems
    applications which use persistent data.

* .. _NO93:

  Scott Nettles & James O'Toole. 1993. "`Real-Time Replication Garbage Collection <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.69.1875&rep=rep1&type=pdf>`_". ACM. PLDI'93.

  .. admonition:: Abstract

    We have implemented the first copying garbage collector that
    permits continuous unimpeded mutator access to the original
    objects during copying. The garbage collector incrementally
    replicates all accessible objects and uses a mutation log to bring
    the replicas up-to-date with changes made by the mutator. An
    experimental implementation demonstrates that the costs of using
    our algorithm are small and that bounded pause times of 50
    milliseconds can be readily achieved.

* .. _NIELSEN77:

  Norman R. Nielsen. 1977. "Dynamic Memory Allocation in Computer Simulation". ACM. CACM 20:11.

  .. admonition:: Abstract

    This paper investigates the performance of 35 dynamic memory
    allocation algorithms when used to service simulation programs as
    represented by 18 test cases. Algorithm performance was measured
    in terms of processing time, memory usage, and external memory
    fragmentation. Algorithms maintaining separate free space lists
    for each size of memory block used tended to perform quite well
    compared with other algorithms. Simple algorithms operating on
    memory ordered lists (without any free list) performed
    surprisingly well. Algorithms employing power-of-two block sizes
    had favorable processing requirements but generally unfavorable
    memory usage. Algorithms employing LIFO, FIFO, or memory ordered
    free lists generally performed poorly compared with others.

* .. _OTOOLE90:

  James O'Toole. 1990. "Garbage Collecting Locally".

  .. admonition:: Abstract

    Generational garbage collection is a simple technique for
    automatic partial memory reclamation. In this paper, I present the
    basic mechanics of generational collection and discuss its
    characteristics. I compare several published algorithms and argue
    that fundamental considerations of locality, as reflected in the
    changing relative speeds of processors, memories, and disks,
    strongly favor a focus on explicit optimization of I/O
    requirements during garbage collection. I show that this focus on
    I/O costs due to memory hierarchy debunks a well-known claim about
    the relative costs of garbage collection and stack allocation. I
    suggest two directions for future research in this area and
    discuss some simple architectural changes in virtual memory
    interfaces which may enable efficient garbage collector
    utilization of standard virtual memory hardware.

* .. _ON94:

  James O'Toole & Scott Nettles. 1994. "`Concurrent Replicating Garbage Collection <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.49.5001&rep=rep1&type=pdf>`_". ACM. LFP'94.

  .. admonition:: Abstract

    We have implemented a concurrent copying garbage collector that
    uses replicating garbage collection. In our design, the client can
    continuously access the heap during garbage collection. No
    low-level synchronization between the client and the garbage
    collector is required on individual object operations. The garbage
    collector replicates live heap objects and periodically
    synchronizes with the client to obtain the client's current root
    set and mutation log. An experimental implementation using the
    Standard ML of New Jersey system on a shared-memory multiprocessor
    demonstrates excellent pause time performance and moderate
    execution time speedups.

* .. _JRR99:

  Simon Peyton Jones, Norman Ramsey, Fermin Reig. 1999. "`C--: a portable assembly language that supports garbage collection <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.11.1815&rep=rep1&type=pdf>`_". Springer-Verlag. International Conference on Principles and Practice of Declarative Programming 1999, LNCS 1702, pp. 1--28.

  .. admonition:: Abstract

    For a compiler writer, generating good machine code for a variety
    of platforms is hard work. One might try to reuse a retargetable
    code generator, but code generators are complex and difficult to
    use, and they limit one's choice of implementation language. One
    might try to use C as a portable assembly language, but C limits
    the compiler writer's flexibility and the performance of the
    resulting code. The wide use of C, despite these drawbacks, argues
    for a portable assembly language. C-- is a new language designed
    expressly for this purpose. The use of a portable assembly
    language introduces new problems in the support of such high-level
    run-time services as garbage collection, exception handling,
    concurrency, profiling, and debugging. We address these problems
    by combining the C-- language with a C-- run-time interface. The
    combination is designed to allow the compiler writer a choice of
    source-language semantics and implementation techniques, while
    still providing good performance.

* .. _PIEPER93:

  John S. Pieper. 1993. "Compiler Techniques for Managing Data Motion". Carnegie Mellon University. Technical report number CMU-CS-93-217.

  .. admonition:: Abstract

    Software caching, automatic algorithm blocking, and data overlays
    are different names for the same problem: compiler management of
    data movement throughout the memory hierarchy. Modern
    high-performance architectures often omit hardware support for
    moving data between levels of the memory hierarchy: iWarp does not
    include a data cache, and Cray supercomputers do not have virtual
    memory. These systems have effectively traded a more complicated
    programming model for performance by replacing a
    hardware-controlled memory hierarchy with a simple fast memory.
    The simpler memories have less logic in the critical path, so the
    cycle time of the memories is improved.

    For programs which fit in the resulting memory, the extra
    performance is great. Unfortunately, the driving force behind
    supercomputing today is a class of very large scientific problems,
    both in terms of computation time and in terms of the amount of
    data used. Many of these programs do not fit in the memory of the
    machines available. When architects trade hardware support for
    data migration to gain performance, control of the memory
    hierarchy is left to the programmer. Either the program size must
    be cut down to fit into the machine, or every loop which accesses
    more data than will fit into memory must be restructured by hand.
    This thesis describes how a compiler can relieve the programmer of
    this burden, and automate data motion throughout the memory
    hierarchy without direct hardware support.

    This works develops a model of how data is accessed within a
    nested loop by typical scientific programs. It describes
    techniques which can be used by compilers faced with the task of
    managing data motion. The concentration is on nested loops which
    process large data arrays using linear array subscripts. Because
    the array subscripts are linear functions of the loop indices and
    the loop indices form an integer lattice, linear algebra can be
    applied to solve many compilation problems.

    The approach it to tile the iteration space of the loop nest.
    Tiling allows the compiler to improve locality of reference. The
    tiling basis matrix is chosen from a set of candidate vectors
    which neatly divide the data set. The execution order of the tiles
    is selected to maximize locality between tiles. Finally, the tile
    sizes are chosen to minimize execution time.

    The approach has been applied to several common scientific loop
    nests: matrix-matrix multiplication, QR-decomposition, and
    LU-decomposition. In addition, an illustrative example from the
    Livermore Loop benchmark set is examined. Although more compiler
    time can be required in some cases, this technique produces better
    code at no cost for most programs.

* .. _PIRINEN98:

  Pekka P. Pirinen. 1998. "Barrier techniques for incremental tracing". ACM. ISMM'98 pp. 20--25.

  .. admonition:: Abstract

    This paper presents a classification of barrier techniques for
    interleaving tracing with mutator operation during an incremental
    garbage collection. The two useful tricolour invariants are
    derived from more elementary considerations of graph traversal.
    Barrier techniques for maintaining these invariants are classified
    according to the action taken at the barrier (such as scanning an
    object or changing its colour), and it is shown that the
    algorithms described in the literature cover all the possibilities
    except one. Unfortunately, the new technique is impractical. Ways
    of combining barrier techniques are also discussed.

* .. _PRINTEZIS96:

  Tony Printezis. 1996. "Disk Garbage Collection Strategies for Persistent Java". Proceedings of the First International Workshop on Persistence and Java.

  .. admonition:: Abstract

    This paper presents work currently in progress on Disk Garbage
    Collection issues for PJava, an orthogonally persistent version of
    Java. In particular, it concentrates on the initial Prototype of
    the Disk Garbage Collector of PJava0 which has already been
    implemented. This Prototype was designed to be very simple and
    modular in order to be easily changed, evolved, improved, and
    allow experimentation. Several experiments were performed in order
    to test possible optimisations; these experiments concentrated on
    the following four areas: a) efficient access to the store; b)
    page-replacement algorithms; c) efficient discovery of live
    objects during compaction; and d) dealing with forward references.
    The paper presents a description of the Prototype's architecture,
    the results of these experiments and related discussion, and some
    future directions based on the experience gained from this work.

* .. _PC96:

  Tony Printezis & Quentin Cutts. 1996. "Measuring the Allocation Rate of Napier88". Department of Computing Science at University of Glasgow. TR ?.

* .. _REINHOLD93:

  M. B. Reinhold. 1993. "`Cache Performance of Garbage Collected Programming Languages <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.18.5454&rep=rep1&type=pdf>`_". Laboratory for Computer Science at MIT. MIT/LCS/TR-581.

  .. admonition:: Abstract

    As processor speeds continue to improve relative to main-memory
    access times, cache performance is becoming an increasingly
    important component of program performance. Prior work on the
    cache performance of garbage-collected programming languages has
    either assumed or argued that conventional garbage-collection
    methods will yield poor performance, and has therefore
    concentrated on new collection algorithms designed specifically to
    improve cache-level reference locality. This dissertation argues
    to the contrary: Many programs written in garbage-collected
    languages are naturally well-suited to the direct-mapped caches
    typically found in modern computer systems.

    Using a trace-driven cache simulator and other analysis tools,
    five nontrivial, long-running Scheme programs are studied. A
    control experiment shows that the programs have excellent cache
    performance without any garbage collection at all. A second
    experiment indicates that the programs will perform well with a
    simple and infrequently-run generational compacting collector.

    An analysis of the test programs' memory usage patterns reveals
    that the mostly-functional programming style typically used in
    Scheme programs, in combination with simple linear storage
    allocation, causes most data objects to be dispersed in time and
    space so that references to them cause little cache interference.
    From this it follows that other Scheme programs, and programs
    written in similar styles in different languages, should perform
    well with a simple generational compacting collector;
    sophisticated collectors intended to improve cache performance are
    unlikely to be effective. The analysis also suggests that, as
    locality becomes ever more important to program performance,
    programs written in garbage-collected languages may turn out to
    have significant performance advantage over programs written in
    more conventional languages.

* .. _ROBSON77:

  J. M. Robson. 1977. "Worst case fragmentation of first fit and best fit storage allocation strategies". ACM. ACM Computer Journal, 20(3):242--244.

* .. _RR97:

  Gustavo Rodriguez-Rivera & Vince Russo. 1997. "Non-intrusive Cloning Garbage Collection with Stock Operating System Support". Software -- Practice and Experience. 27:8.

  .. admonition:: Abstract

    It is well accepted that automatic garbage collection simplifies
    programming, promotes modularity, and reduces development effort.
    However it is commonly believed that these advantages do not
    counteract the perceived price: excessive overheads, possible long
    pause times while garbage collections occur, and the need to
    modify existing code. Even though there are publically available
    garbage collector implementations that can be used in existing
    programs, they do not guarantee short pauses, and some
    modification of the application using them is still required. In
    this paper we describe a snapshot-at-beginning concurrent garbage
    collector algorithm and its implementation. This algorithm
    guarantees short pauses, and can be easily implemented on stock
    UNIX-like operating systems. Our results show that our collector
    performs comparable to other garbage collection implementations on
    uniprocessor machines and outperforms similar collectors on
    multiprocessor machines. We also show our collector to be
    competitive in performance with explicit deallocation. Our
    collector has the added advantage of being non-intrusive. Using a
    dynamic linking technique and effective root set inferencing, we
    have been able to successfully run our collector even in
    commercial programs where only the binary executable and no source
    code is available. In this paper we describe our algorithm, its
    implementation, and provide both an algorithmic and a performance
    comparison between our collector and other similar garbage
    collectors.

* .. _ROJEMO95:

  Niklas Röjemo. 1995. "Highlights from nhc -- a space-efficient Haskell compiler". Chalmers University of Technology.

  .. admonition:: Abstract

    Self-compiling implementations of Haskell, i.e., those written in
    Haskell, have been and, except one, are still space consuming
    monsters. Object code size for the compilers themselves are 3-8Mb,
    and they need 12-20Mb to recompile themselves. One reason for the
    huge demands for memory is that the main goal for these compilers
    is to produce fast code. However, the compiler described in this
    paper, called "nhc" for "Nearly a Haskell Compiler", is the one
    above mentioned exception. This compiler concentrates on keeping
    memory usage down, even at a cost in time. The code produced is
    not fast, but nhc is usable, and the resulting programs can be run
    on computers with small memory.

    This paper describes some of the implementation choices done, in
    the Haskell part of the source code, to reduce memory consumption
    in nhc. It is possible to use these also in other Haskell
    compilers with no, or very small, changes to their run-time
    systems.

    Time is neither the main focus of nhc nor of this paper, but there
    is nevertheless a small section about the speed of nhc. The most
    notable observation concerning speed is that nhc spends
    approximately half the time processing interface files, which is
    much more than needed in the type checker. Processing interface
    files is also the most space consuming part of nhc in most cases.
    It is only when compiling source files with large sets of mutually
    recursive functions that more memory is needed to type check than
    to process interface files.

* .. _ROJEMO95A:

  Niklas Röjemo. 1995. "Generational garbage collection for lazy functional languages without temporary space leaks". Chalmers University of Technology.

  .. admonition:: Abstract

    Generational garbage collection is an established method for
    creating efficient garbage collectors. Even a simple
    implementation where all nodes that survive one garbage collection
    are *tenured*, i.e., moved to an old generation, works well in
    strict languages. In lazy languages, however, such an
    implementation can create severe *temporary space leaks*. The
    temporary space leaks appear in programs that traverse large
    lazily built data structures, e.g., a lazy list representing a
    large file, where only a small part is needed at any time. A
    simple generational garbage collector cannot reclaim the memory,
    used by the lazily built list, at minor collections. The reason is
    that at least one of the nodes in the list belongs to the old
    generation, after the first minor collection, and will hold on to
    the rest of the nodes in the list until the next major collection.

* .. _RR96:

  Niklas Röjemo & Colin Runciman. 1996. "Lag, drag, void and use -- heap profiling and space-efficient compilation revisited". ACM, SIGPLAN. ICFP'96, ACM SIGPLAN Notices 31:6, ISBN 0-89791-770-7, pp. 34--41.

  .. admonition:: Abstract

    The context for this paper is functional computation by graph
    reduction. Our overall aim is more efficient use of memory. The
    specific topic is the detection of dormant cells in the live graph
    -- those retained in heap memory though not actually playing a
    useful role in computation. We describe a profiler that can
    identify heap consumption by such 'useless' cells. Unlike heap
    profilers based on traversals of the live heap, this profiler
    works by examining cells post-mortem. The new profiler has
    revealed a surprisingly large proportion of 'useless' cells, even
    in some programs that previously seemed space-efficient such as
    the bootstrapping Haskell compiler "nhc".

* .. _RW99:

  David J. Roth, David S. Wise. 1999. "`One-bit counts between unique and sticky <http://www.cs.indiana.edu/pub/techreports/TR516.pdf>`_". ACM. ISMM'98, pp. 49--56.

  .. admonition:: Abstract

    Stoye's one-bit reference tagging scheme can be extended to local
    counts of two or more via two strategies. The first, suited to
    pure register transactions, is a cache of referents to two shared
    references. The analog of Deutch's and Bobrow's multiple-reference
    table, this cache is sufficient to manage small counts across
    successive assignment statements. Thus, accurate reference counts
    above one can be tracked for short intervals, like that bridging
    one function's environment to its successor's.

    The second, motivated by runtime stacks that duplicate references,
    avoids counting any references from the stack. It requires a local
    pointer-inversion protocol in the mutator, but one still local to
    the referent and the stack frame. Thus, an accurate reference
    count of one can be maintained regardless of references from the
    recursion stack.

* .. _ROVNER85:

  Paul Rovner. 1985. "`On Adding Garbage Collection and Runtime Types to a Strongly-Typed, Statically-Checked, Concurrent Language <https://archive.org/details/bitsavers_xeroxparctddingGarbageCollectionandRuntimeTypestoa_1765837>`_". Xerox PARC. TR CSL-84-7.

  .. admonition:: Abstract

    Enough is known now about garbage collection, runtime types,
    strong-typing, static-checking and concurrency that it is possible
    to explore what happens when they are combined in a real
    programming system.

    Storage management is one of a few central issues through which
    one can get a good view of the design of an entire system.
    Tensions between ease of integration and the need for protection;
    between generality, simplicity, flexibility, extensibility and
    efficiency are all manifest when assumptions and attitudes about
    managing storage are studied. And deep understanding follows best
    from the analysis of systems that people use to get real work
    done.

    This paper is not for those who seek arguments pro or con about
    the need for these features in programming systems; such issues
    are for other papers. This one assumes these features to be good
    and describes how they combine and interact in Cedar, a
    programming language and environment designed to help programmers
    build moderate-sized experimental systems for moderate numbers of
    people to test and use.

* .. _RUNCIMAN92:

  Colin Runciman & David Wakeling. 1992. "`Heap Profiling of Lazy Functional Programs <ftp://ftp.cs.york.ac.uk/reports/YCS-92-172.ps.Z>`_". University of York.

  .. admonition:: Abstract

    We describe the design, implementation, and use of a new kind of
    profiling tool that yields valuable information about the memory
    use of lazy functional programs. The tool has two parts: a
    modified functional language implementation which generated
    profiling implementation during the execution of programs, and a
    separate program which converts this information to graphical
    form. With the aid of profile graphs, one can make alterations to
    a functional program which dramatically reduce its space
    consumption. We demonstrate that this is the case of a genuine
    example -- the first to which the tool has been applied -- for
    which the results are strikingly successful.

* .. _RR94:

  Colin Runciman & Niklas Röjemo. 1994. "`New dimensions in heap profiling <http://www.cs.york.ac.uk/plasma/publications/pdf/RuncimanWakelingJFP93.pdf>`_". University of York.

  .. admonition:: Abstract

    First-generation heap profilers for lazy functional languages have
    proved to be effective tools for locating some kinds of space
    faults, but in other cases they cannot provide sufficient
    information to solve the problem. This paper describes the design,
    implementation and use of a new profiler that goes beyond the
    two-dimensional "who produces what" view of heap cells to provide
    information about their more dynamic and structural attributes.
    Specifically, the new profiler can distinguish between cells
    according to their *eventual lifetime*, or on the basis of the
    *closure retainers* by virtue of which they remain part of the
    live heap. A bootstrapping Haskell compiler (nhc) hosts the
    implementation: among examples of the profiler's use we include
    self-application to nhc. Another example is the original
    heap-profiling case study "clausify", which now consumes even less
    memory and is much faster.

* .. _RR96A:

  Colin Runciman & Niklas Röjemo. 1996. "Two-pass heap profiling: a matter of life and death". Department of Computer Science, University of York.

  .. admonition:: Abstract

    A heap profile is a chart showing the contents of heap memory
    throughout a computation. Contents are depicted abstractly by
    showing how much space is occupied by memory cells in each of
    several classes. A good heap profiler can use a variety of
    attributes of memory cells to de-fine a classification. Effective
    profiling usually involves a combination of attributes. The ideal
    profiler gives full support for combination in two ways. First, a
    section of the heap of interest to the programmer can be specified
    by constraining the values of any combination of cell attributes.
    Secondly, no matter what attributes are used to specify such a
    section, a heap profile can be obtained for that section only, and
    any other attribute can be used to define the classification.

    Achieving this ideal is not simple For some combinations of
    attributes. A heap profile is derived by interpolation of a series
    of censuses of heap contents at different stages. The obvious way
    to obtain census data is to traverse the live heap at intervals
    throughout the computation. This is fine for static attributes
    (e.g. What type of value does this memory cell represent?), and
    for dynamic attributes that can be determined for each cell by
    examining the heap at any given moment (e.g. From which function
    closures can this cell be reached?). But some attributes of cells
    can only be determined retrospectively by post-mortem inspection
    asa cell is overwritten or garbage-collected (e.g. Is this cell
    ever used again?). Now we see the problem: if a profiler supports
    both live and pose-mortem attributes, how can we implement the
    ideal of unrestricted combinations? That is the problem me solve
    in this paper. We give techniques for profiling a. heap section
    specified in terms of both live and post-mortem attributes. We
    show how to generate live-attribute profiles of a section of the
    heal, specified using post-mortem attributes, and vice versa.

* .. _SG95:

  Jacob Seligmann & Steffen Grarup. 1995. "`Incremental Mature Garbage Collection Using the Train Algorithm <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.32.7307&rep=rep1&type=pdf>`_". Springer-Verlag. ECOOP'95, Lecture Notes in Computer Science, Vol. 952, pp. 235--252, ISBN 3-540-60160-0.

  .. admonition:: Abstract

    We present an implementation of the Train Algorithm, an
    incremental collection scheme for reclamation of mature garbage in
    generation-based memory management systems. To the best of our
    knowledge, this is the first Train Algorithm implementation ever.
    Using the algorithm, the traditional mark-sweep garbage collector
    employed by the Mj&oslash;lner run-time system for the
    object-oriented BETA programming language was replaced by a
    non-disruptive one, with only negligible time and storage
    overheads.

* .. _SB00:

  Manuel Serrano, Hans-J. Boehm. 2000. "`Understanding memory allocation of Scheme programs <http://www.hpl.hp.com/techreports/2000/HPL-2000-62.html>`_". ACM. Proceedings of International Conference on Functional Programming 2000.

  .. admonition:: Abstract

    Memory is the performance bottleneck of modern architectures.
    Keeping memory consumption as low as possible enables fast and
    unobtrusive applications. But it is not easy to estimate the
    memory use of programs implemented in functional languages, due to
    both the complex translations of some high level constructs, and
    the use of automatic memory managers. To help understand memory
    allocation behavior of Scheme programs, we have designed two
    complementary tools. The first one reports on frequency of
    allocation, heap configurations and on memory reclamation. The
    second tracks down memory leaks. We have applied these tools to
    our Scheme compiler, the largest Scheme program we have been
    developing. This has allowed us to drastically reduce the amount
    of memory consumed during its bootstrap process, without requiring
    much development time. Development tools will be neglected unless
    they are both conveniently accessible and easy to use. In order to
    avoid this pitfall, we have carefully designed the user interface
    of these two tools. Their integration into a real programming
    environment for Scheme is detailed in the paper.

* .. _SHAPIRO94:

  Marc Shapiro & Paulo Ferreira. 1994. "`Larchant-RDOSS: a distributed shared persistent memory and its garbage collector <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.52.8468&rep=rep1&type=pdf>`_". INRIA. INRIA Rapport de Recherche no. 2399; Cornell Computer Science TR94-1466.

  .. admonition:: Abstract

    Larchant-RDOSS is a distributed shared memory that persists on
    reliable storage across process lifetimes. Memory management is
    automatic: including consistent caching of data and of locks,
    collecting objects unreachable from the persistent root, writing
    reachable objects to disk, and reducing store fragmentation.
    Memory management is based on a novel garbage collection
    algorithm, that approximates a global trace by a series of local
    traces, with no induced I/O or locking traffic, and no
    synchronization between the collector and the application
    processes. This results in a simple programming model, and
    expected minimal added application latency. The algorithm is
    designed for the most unfavorable environment (uncontrolled
    programming language, reference by pointers, distributed system,
    non-coherent shared memory) and should work well also in more
    favorable settings.

* .. _SHAW87:

  Robert A. Shaw. 1987. "Improving Garbage Collector Performance in Virtual Memory". Stanford University. CSL-TR-87-323.

* .. _SHAW88:

  Robert A. Shaw. 1988. "Empirical Analysis of a LISP System". Stanford University. CSL-TR-88-351.

* .. _SINGHAL92:

  Vivek Singhal, Sheetal V. Kakkad, Paul R. Wilson. 1992. "`Texas: An Efficient, Portable Persistent Store <ftp://ftp.cs.utexas.edu/pub/garbage/texaspstore.ps>`_". University of Texas at Austin.

  .. admonition:: Abstract

    Texas is a persistent storage system for C++, providing high
    performance while emphasizing simplicity, modularity and
    portability. A key component of the design is the use of pointer
    swizzling at page fault time, which exploits existing virtual
    memory features to implement large address spaces efficiently on
    stock hardware, with little or no change to existing compilers.
    Long pointers are used to implement an enormous address space, but
    are transparently converted to the hardware-supported pointer
    format when pages are loaded into virtual memory.

    Runtime type descriptors and slightly modified heap allocation
    routines support pagewise pointer swizzling by allowing objects
    and their pointer fields to be identified within pages. If
    compiler support for runtime type identification is not available,
    a simple preprocessor can be used to generate type descriptors.

    This address translation is largely independent of issues of data
    caching, sharing, and checkpointing; it employs operating systems'
    existing virtual memories for caching, and a simple and flexible
    log-structured storage manager to improve checkpointing
    performance.

    Pagewise virtual memory protections are also used to detect writes
    for logging purposes, without requiring any changes to compiled
    code. This may degrade checkpointing performance for small
    transactions with poor locality of writes, but page diffing and
    sub-page logging promise to keep performance competitive with
    finer-grained checkpointing schemes.

    Texas presents a simple programming interface; an application
    creates persistent objects by simply allocating them on the
    persistent heap. In addition, the implementation is relatively
    small, and is easy to incorporate into existing applications. The
    log-structured storage module easily supports advanced extensions
    such as compressed storage, versioning, and adaptive
    reorganization.

* .. _SOBALVARRO88:

  P. G. Sobalvarro. 1988. "`A Lifetime-based Garbage Collector for LISP Systems on General-Purpose Computers <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.57.2188&rep=rep1&type=pdf>`_". MIT. AITR-1417.

  .. admonition:: Abstract

    Garbage collector performance in LISP systems on custom hardware has been substantially improved by the adoption of lifetime-based garbage collection techniques.  To date, however, successful lifetime-based garbage collectors have required special-purpose hardware, or at least privileged access to data structures maintained by the virtual memory system.  I present here a lifetime-based garbage collector requiring no special-purpose hardware or virtual memory system support, and discuss its performance.

* .. _STEELE75:

  Guy L. Steele. 1975. "Multiprocessing Compactifying Garbage Collection". CACM. 18:9 pp. 495--508.

  .. admonition:: Abstract

    Algorithms for a multiprocessing compactifying garbage collector
    are presented and discussed. The simple case of two processors,
    one performing LISP-like list operations and the other performing
    garbage collection continuously, is thoroughly examined. The
    necessary capabilities of each processor are defined, as well as
    interprocessor communication and interlocks. Complete procedures
    for garbage collection and for standard list processing primitives
    are presented and thoroughly explained. Particular attention is
    given to the problems of marking and relocating list cells while
    another processor may be operating on them. The primary aim
    throughout is to allow the list processor to run unimpeded while
    the other processor reclaims list storage The more complex case
    involving several list processors and one or more garbage
    collection processors are also briefly discussed.

* .. _STEELE76:

  Guy L. Steele. 1976. "Corrigendum: Multiprocessing Compactifying Garbage Collection". CACM. 19:6 p.354.

* .. _STEELE77:

  Guy L. Steele. 1977. "`Data Representation in PDP-10 MACLISP <http://dspace.mit.edu/bitstream/handle/1721.1/6278/AIM-420.pdf>`_". MIT. AI Memo 420.

  .. admonition:: Abstract

    The internal representations of the various MacLISP data types are
    presented and discussed. Certain implementation tradeoffs are
    considered. The ultimate decisions on these tradeoffs are
    discussed in the light of MacLISP's prime objective of being an
    efficient high-level language for the implementation of large
    systems such as MACSYMA. The basic strategy of garbage collection
    is outlined, with reference to the specific representations
    involved. Certain "clever tricks" are explained and justified. The
    "address space crunch" is explained and some alternative solutions
    explored.

* .. _SLC99:

  James M. Stichnoth, Guei-Yuan Lueh, Michal Cierniak. 1999. "`Support for Garbage Collection at Every Instruction in a Java Compiler <http://www.cs.tufts.edu/~nr/cs257/archive/james-stichnoth/p118-stichnoth.pdf>`_". SIGPLAN. Proceedings of the 1999 ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI). SIGPLAN Notices 34(5). pp. 118--127.

  .. admonition:: Abstract

    A high-performance implementation of a Java Virtual Machine
    requires a compiler to translate Java bytecodes into native
    instructions, as well as an advanced garbage collector (e.g.,
    copying or generational). When the Java heap is exhausted and the
    garbage collector executes, the compiler must report to the
    garbage collector all live object references contained in physical
    registers and stack locations. Typical compilers only allow
    certain instructions (e.g., call instructions and backward
    branches) to be GC-safe; if GC happens at some other instruction,
    the compiler may need to advance execution to the next GC-safe
    point. Until now, no one has ever attempted to make every
    compiler-generated instruction GC-safe, due to the perception that
    recording this information would require too much space. This kind
    of support could improve the GC performance in multithreaded
    applications. We show how to use simple compression techniques to
    reduce the size of the GC map to about 20% of the generated code
    size, a result that is competitive with the best previously
    published results. In addition, we extend the work of Agesen,
    Detlefs, and Moss, regarding the so-called “JSR Problem” (the
    single exception to Java’s type safety property), in a way that
    eliminates the need for extra runtime overhead in the generated
    code.

* .. _SCN84:

  Will R Stoye, T J W Clarke, Arthur C Norman. 1984. "Some Practical Methods for Rapid Combinator Reduction". In LFP 1984, 159--166.

  .. admonition:: Abstract

    The SKIM II processor is a microcoded hardware machine for the
    rapid evaluation of functional languages. This paper gives details
    of some of the more novel methods employed by SKIM II, and
    resulting performance measurements. The authors conclude that
    combinator reduction can still form the basis for the efficient
    implementation of a functional language.

* .. _TD95:

  David Tarditi & Amer Diwan. 1995. "`Measuring the Cost of Storage Management <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.4550&rep=rep1&type=pdf>`_". Carnegie Mellon University. CMU-CS-94-201.

  .. admonition:: Abstract

    We study the cost of storage management for garbage-collected
    programs compiled with the Standard ML of New Jersey compiler. We
    show that the cost of storage management is not the same as the
    time spent garbage collecting. For many of the programs, the time
    spent garbage collecting is less than the time spent doing other
    storage-management tasks.

* .. _TJ94:

  Stephen Thomas, Richard E. Jones. 1994. "Garbage Collection for Shared Environment Closure Reducers". Computing Laboratory, The University of Kent at Canterbury. Technical Report 31-94.

  .. admonition:: Abstract

    Shared environment closure reducers such as Fairbairn and Wray's
    TIM incur a comparatively low cost when creating a suspension, and
    so provide an elegant method for implementing lazy functional
    evaluation. However, comparatively little attention has been given
    to the problems involved in identifying which portions of a shared
    environment are needed (and ignoring those which are not) during a
    garbage collection. Proper consideration of this issue has subtle
    consequences when implementing a storage manager in a TIM-like
    system. We describe the problem and illustrate the negative
    consequences of ignoring it.

    We go on to describe a solution in which the compiler determines
    statically which portions of that code's environment are required
    for each piece of code it generates, and emits information to
    assist the run-time storage manager to scavenge environments
    selectively. We also describe a technique for expressing this
    information directly as executable code, and demonstrate that a
    garbage collector implemented in this way can perform
    significantly better than an equivalent, table-driven interpretive
    collector.

* .. _THOMAS95:

  Stephen Thomas. 1995. "Garbage Collection in Shared-Environment Closure Reducers: Space-Efficient Depth First Copying using a Tailored Approach". *Information Processing Letters.* 56:1, pp. 1--7.

  .. admonition:: Abstract

    Implementations of abstract machines such as the OP-TIM and the
    PG-TIM need to use a tailored garbage collector which seems to
    require an auxiliary stack,with a potential maximum size that is
    directly proportional to the amount of live data in the heap.
    However, it turns out that it is possible to build a recursive
    copying collector that does not require additional space by
    reusing already-scavenged space. This paper is a description of
    this technique.

* .. _TT97:

  Mads Tofte & Jean-Pierre Talpin. 1997. "`Region-Based Memory Management <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.75.9105&rep=rep1&type=pdf>`_". Information and Computation 132(2), pp. 109--176.

  .. admonition:: Abstract

    This paper describes a memory management discipline for programs
    that perform dynamic memory allocation and de-allocation. At
    runtime, all values are put into regions. The store consists of a
    stack of regions. All points of region allocation and
    de-allocation are inferred automatically, using a type and effect
    based program analysis. The scheme does not assume the presence of
    a garbage collector. The scheme was first presented in 1994 (M.
    Tofte and J.-P. Talpin, in *Proceedings of the 21st ACM
    SIGPLAN-SIGACT Symposium on Principles of Programming Languages,*
    pp. 188--201); subsequently, it has been tested in the ML Kit with
    Regions, a region-based, garbage-collection free implementation of
    the Standard ML Core Language, which includes recursive datatypes,
    higher-order functions and updatable references (L. Birkedal, M.
    Tofte, and M. Vejlstrup, (1996), in *Proceedings of the 23rd ACM
    SIGPLAN-SIGACT Symposium on Principles of Programming Languages,*
    pp. 171--183). This paper defines a region-based dynamic semantics
    for a skeletal programming language extracted from Standard ML. We
    present the inference system which specifies where regions can be
    allocated and de-allocated and a detailed proof that the system is
    sound with respect to a standard semantics. We conclude by giving
    some advice on how to write programs that run well on a stack of
    regions, based on practical experience with the ML Kit.

* .. _UNGAR84:

  Dave Ungar. 1984. "`Generation Scavenging: A Non-disruptive High Performance Storage Reclamation Algorithm <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.122.4295&rep=rep1&type=pdf>`_". ACM, SIGSOFT, SIGPLAN. Practical Programming Environments Conference.

  .. admonition:: Abstract

    Many interactive computing environments provide automatic storage
    reclamation and virtual memory to ease the burden of managing
    storage. Unfortunately, many storage reclamation algorithms impede
    interaction with distracting pauses. *Generation Scavenging* is a
    reclamation algorithm that has no noticeable pauses, eliminates
    page faults for transient objects, compacts objects without
    resorting to indirection, and reclaims circular structures, in one
    third the time of traditional approaches.

* .. _UNGAR88:

  Dave Ungar & Frank Jackson. 1988. "`Tenuring Policies for Generation-Based Storage Reclamation <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.115.2810&rep=rep1&type=pdf>`_". SIGPLAN. OOPSLA '88 Conference Proceedings, ACM SIGPLAN Notices, Vol. 23, No. 11, pp. 1--17.

  .. admonition:: Abstract

    One of the most promising automatic storage reclamation
    techniques, generation-based storage reclamation, suffers poor
    performance if many objects live for a fairly long time and then
    die. We have investigated the severity of the problem by
    simulating Generation Scavenging automatic storage reclamation
    from traces of actual four-hour sessions. There was a wide
    variation in the sample runs, with garbage-collection overhead
    ranging from insignificant, during interactive runs, to sever,
    during a single non-interactive run. All runs demonstrated that
    performance could be improved with two techniques: segregating
    large bitmaps and strings, and mediating tenuring with demographic
    feedback. These two improvements deserve consideration for any
    generation-based storage reclamation strategy.

* .. _VO96:

  Kiem-Phong Vo. 1996. "Vmalloc: A General and Efficient Memory Allocator". Software -- Practice and Experience. 26(3): 357--374 (1996).

  .. admonition:: Abstract

    On C/Unix systems, the malloc interface is standard for dynamic
    memory allocation. Despite its popularity, malloc's shortcomings
    frequently cause programmers to code around it. The new library
    Vmalloc generalizes malloc to give programmers more control over
    memory allocation. Vmalloc introduces the idea of organizing
    memory into separate regions, each with a discipline to get raw
    memory and a method to manage allocation. Applications can write
    their own disciplines to manipulate arbitrary type of memory or
    just to better organize memory in a region by creating new regions
    out of its memory. The provided set of allocation methods include
    general purpose allocations, fast special cases and aids for
    memory debugging or profiling. A compatible malloc interface
    enables current applications to select allocation methods using
    environment variables so they can tune for performance or perform
    other tasks such as profiling memory usage, generating traces of
    allocation calls or debugging memory errors. A performance study
    comparing Vmalloc and currently popular malloc implementations
    shows that Vmalloc is competitive to the best of these allocators.
    Applications can gain further performance improvement by using the
    right mixture of regions with different Vmalloc methods.

* .. _WW76:

  Daniel C. Watson, David S. Wise. 1976. "Tuning Garwick's algorithm for repacking sequential storage". *BIT.* 16, 4 (December 1976): 442--450.

  .. admonition:: Abstract

    Garwick's algorithm, for repacking LIFO lists stored in a
    contiguous block of memory, bases the allocation of remaining
    space upon both sharing and previous stack growth. A system
    whereby the weight applied to each method can be adjusted
    according to the current behaviour of the stacks is discussed.

    We also investigate the problem of determining during memory
    repacking that the memory is used to saturation and the driving
    program should therefore be aborted. The tuning parameters studied
    here seem to offer no new grasp on this problem.

* .. _WLM92:

  Paul R. Wilson, Michael S. Lam, Thomas G. Moher. 1992. "Caching Considerations for Generational Garbage Collection". ACM. L&FP 92.

  .. admonition:: Abstract

    GC systems allocate and reuse memory cyclically; this imposes a
    cyclic pattern on memory accesses that has its own distinctive
    locality characteristics. The cyclic reuse of memory tends to
    defeat caching strategies if the reuse cycle is too large to fit
    in fast memory. Generational GCs allow a smaller amount of memory
    to be reused more often. This improves VM performance, because the
    frequently-reused area stays in main memory. The same principle
    can be applied at the level of high-speed cache memories, if the
    cache is larger than the youngest generation. Because of the
    repeated cycling through a fixed amount of memory, however,
    generational GC interacts with cache design in unusual ways, and
    modestly set-associative caches can significantly outperform
    direct-mapped caches.

    While our measurements do not show very high miss rates for GCed
    systems, they indicate that performance problems are likely in
    faster next-generation systems, where second-level cache misses
    may cost scores of cycles. Software techniques can improve cache
    performance of garbage-collected systems, by decreasing the cache
    "footprint" of the youngest generation; compiler techniques that
    reduce the amount of heap allocation also improve locality. Still,
    garbage-collected systems with a high rate of heap allocation
    require somewhat more cache capacity and/or main memory bandwidth
    than conventional systems.

* .. _WIL92A:

  Paul R. Wilson, Sheetal V. Kakkad. 1992. "`Pointer Swizzling at Page Fault Time <ftp://ftp.cs.utexas.edu/pub/garbage/swizz.ps>`_". University of Texas at Austin.

  .. admonition:: Abstract

    Pointer swizzling at page fault time is a novel address
    translation mechanism that exploits conventional address
    translation hardware. It can support huge address spaces
    efficiently without long hardware addresses; such large address
    spaces are attractive for persistent object stores, distributed
    shared memories, and shared address space operating systems. This
    swizzling scheme can be used to provide data compatibility across
    machines with different word sizes, and even to provide binary
    code compatibility across machines with different hardware address
    sizes.

    Pointers are translated ("swizzled") from a long format to a
    shorter hardware-supported format at page fault time. No extra
    hardware is required, and no continual software overhead is
    incurred by presence checks of indirection of pointers. This
    pagewise technique exploits temporal and spatial locality in much
    the same way as normal virtual memory; this gives it many
    desirable performance characteristics, especially given the trend
    toward larger main memories. It is easy to implement using common
    compilers and operating systems.

* .. _WIL94:

  Paul R. Wilson. 1994. "`Uniprocessor Garbage Collection Techniques <ftp://ftp.cs.utexas.edu/pub/garbage/bigsurv.ps>`_". University of Texas.

  .. admonition:: Abstract

    We survey basic garbage collection algorithms, and variations such
    as incremental and generational collection; we then discuss
    low-level implementation considerations and the relationships
    between storage management systems, languages, and compilers.
    Throughout, we attempt to present a unified view based on abstract
    traversal strategies, addressing issues of conservatism,
    opportunism, and immediacy of reclamation; we also point out a
    variety of implementation details that are likely to have a
    significant impact on performance.

* .. _WIL95:

  Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. "`Dynamic Storage Allocation: A Survey and Critical Review <ftp://ftp.cs.utexas.edu/pub/garbage/allocsrv.ps>`_". University of Texas at Austin.

  .. admonition:: Abstract

    Dynamic memory allocation has been a fundamental part of most
    computer systems since roughly 1960, and memory allocation is
    widely considered to be either a solved problem or an insoluble
    one. In this survey, we describe a variety of memory allocator
    designs and point out issues relevant to their design and
    evaluation. We then chronologically survey most of the literature
    on allocators between 1961 and 1995. (Scores of papers are
    discussed, in varying detail, and over 150 references are given.)

    We argue that allocator designs have been unduly restricted by an
    emphasis on mechanism, rather than policy, while the latter is
    more important; higher-level strategic issues are still more
    important, but have not been given much attention.

    Most theoretical analyses and empirical allocator evaluations to
    date have relied on very strong assumptions of randomness and
    independence, but real program behavior exhibits important
    regularities that must be exploited if allocators are to perform
    well in practice.

* .. _WISE78:

  David S. Wise. 1978. "`The double buddy system <http://www.cs.indiana.edu/ftp/techreports/TR79.pdf>`_". Department of Computer Science at Indiana University. Technical Report 79.

  .. admonition:: Abstract

    A new buddy system is described in which the region of storage
    being managed is partitioned into two sub-regions, each managed by
    a fairly standard "binary" buddy system. Like the weighted buddy
    systems of Shen and Peterson, the block sizes are of sizes 2\
    :superscript:`n+1` or 3·2\ :superscript:`n`, but unlike theirs
    there is no extra overhead for typing information or for buddy
    calculation, and an allocation which requires splitting an extant
    available block only rarely creates a block smaller than the one
    being allocated. Such smaller blocks are carved out only when the
    boundary between the two subregions floats; the most interesting
    property of this system is that the procedures for allocation and
    deallocation are designed to keep blocks immediately adjacent to
    the subregion boundary free, so that the boundary may be moved
    within a range of unused space without disturbing blocks in use.
    This option is attained with a minimum of extra computation beyond
    that of a binary buddy system, and provides this scheme with a new
    approach to the problem of external fragmentation.

* .. _WISE79:

  David S. Wise. 1979. "`Morris's garbage compaction algorithm restores reference counts <http://www.cs.indiana.edu/ftp/techreports/TR75.pdf>`_". TOPLAS. 1, 1 (July 1979): 115--120.

  .. admonition:: Abstract

    The two-pass compaction algorithm of F.L. Morris, which follows
    upon the mark phase in a garbage collector, may be modified to
    recover reference counts for a hybrid storage management system.
    By counting the executions of two loops in that algorithm where
    upward and downward references, respectively, are forwarded to the
    relocation address of one node, we can initialize a count of
    active references and then update it but once. The reference count
    may share space with the mark bit in each node, but it may not
    share the additional space required in each pointer by Morris's
    algorithm, space which remains unused outside the garbage
    collector.

* .. _WISE85:

  David S. Wise. 1985. "`Design for a multiprocessing heap with on-board reference counting <http://www.cs.indiana.edu/ftp/techreports/TR163.pdf>`_". Springer-Verlag. In J.-P. Jouannaud (ed.), Functional Programming Languages and Computer Architecture, Lecture Notes in Computer Science 201: 289--304.

  .. admonition:: Abstract

    A project to design a pair of memory chips with a modicum of
    intelligence is described. Together, the two allow simple
    fabrication of a small memory bank, a heap of binary (LISP-like)
    nodes that offers the following features: 64-bit nodes; two
    pointer fields per node up to 29 bits each; reference counts
    implicitly maintained on writes; 2 bits per node for marking
    (uncounted) circular references; 4 bits per node for
    conditional-store testing at the memory; provision for
    processor-driven, recounting garbage collection.

* .. _WISE92:

  .. _WISE93:

  David S. Wise. 1993. "`Stop-and-copy and one-bit reference counting <http://www.cs.indiana.edu/ftp/techreports/TR360.pdf>`_". *Information Processing Letters.* 46, 5 (July 1993): 243--249.

  .. admonition:: Abstract

    A stop-and-copy garbage collector updates one-bit reference
    counting with essentially no extra space and minimal memory cycles
    beyond the conventional collection algorithm. Any object that is
    uniquely referenced during a collection becomes a candidate for
    cheap recovery before the next one, or faster recopying then if it
    remains uniquely referenced. Since most objects stay uniquely
    referenced, subsequent collections run faster even if none are
    recycled between garbage collections. This algorithm extends to
    generation scavenging, it admits uncounted references from roots,
    and it corrects conservatively stuck counters, that result from
    earlier uncertainty whether references were unique.

* .. _WW95:

  David S. Wise, Joshua Walgenbach. 1996. "`Static and Dynamic Partitioning of Pointers as Links and Threads <http://www.cs.indiana.edu/ftp/techreports/TR437.pdf>`_". SIGPLAN. Proc. 1996 ACM SIGPLAN Intl. Conf. on Functional Programming, SIGPLAN Not. 31, 6 (June 1996), pp. 42--49.

  .. admonition:: Abstract

    Identifying some pointers as invisible threads, for the purposes
    of storage management, is a generalization from several widely
    used programming conventions, like threaded trees. The necessary
    invariant is that nodes that are accessible (without threads) emit
    threads only to other accessible nodes. Dynamic tagging or static
    typing of threads ameliorates storage recycling both in functional
    and imperative languages.

    We have seen the distinction between threads and links sharpen
    both hardware- and software-supported storage management in
    SCHEME, and also in C. Certainly, therefore, implementations of
    languages that already have abstract management and concrete
    typing, should detect and use this as a new static type.

* .. _WHHHO94:

  David S. Wise, Brian Heck, Caleb Hess, Willie Hunt, Eric Ost. 1997. "`Uniprocessor Performance of a Reference-Counting Hardware Heap <http://www.cs.indiana.edu/ftp/techreports/TR401.pdf>`_". *LISP and Symbolic Computation.* 10, 2 (July 1997), pp. 159--181.

  .. admonition:: Abstract

    A hardware self-managing heap memory (RCM) for languages like
    LISP, SMALLTALK, and JAVA has been designed, built, tested and
    benchmarked. On every pointer write from the processor,
    reference-counting transactions are performed in real time within
    this memory, and garbage cells are reused without processor
    cycles. A processor allocates new nodes simply by reading from a
    distinguished location in its address space. The memory hardware
    also incorporates support for off-line, multiprocessing,
    mark-sweep garbage collection.

    Performance statistics are presented from a partial implementation
    of SCHEME over five different memory models and two garbage
    collection strategies, from main memory (no access to RCM) to a
    fully operational RCM installed on an external bus. The
    performance of the RCM memory is more than competitive with main
    memory.

* .. _WITHINGTON91:

  P. Tucker Withington. 1991. "`How Real is 'Real-Time' Garbage Collection? <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.116.3169&rep=rep1&type=pdf>`_". ACM. OOPSLA/ECOOP '91 Workshop on Garbage Collection in Object-Oriented Systems.

  .. admonition:: Abstract

    A group at Symbolics is developing a Lisp runtime kernel, derived
    from its Genera operating system, to support real-time control
    applications. The first candidate application has strict
    response-time requirements (so strict that it does not permit the
    use of paged virtual memory). Traditionally, Lisp's automatic
    storage-management mechanism has made it unsuitable to real-time
    systems of this nature. A number of garbage collector designs and
    implementations exist (including the Genera garbage collector)
    that purport to be "real-time", but which actually have only
    mitigated the impact of garbage collection sufficiently that it
    usually goes unnoticed by humans. Unfortunately,
    electro-mechanical systems are not so forgiving. This paper
    examines the limitations of existing real-time garbage collectors
    and describes the avenues that we are exploring in our work to
    develop a CLOS-based garbage collector that can meet the real-time
    requirements of real real-time systems.

* .. _YIP91:

  G. May Yip. 1991. "`Incremental, Generational Mostly-Copying Garbage Collection in Uncooperative Environments <http://www.hpl.hp.com/techreports/Compaq-DEC/WRL-91-8.pdf>`_". Digital Equipment Corporation.

  .. admonition:: Abstract

    The thesis of this project is that incremental collection can be
    done feasibly and efficiently in an architecture and compiler
    independent manner. The design and implementation of an
    incremental, generational mostly-copying garbage collector for C++
    is presented. The collector achieves, simultaneously, real-time
    performance (from incremental collection), low total garbage
    collection delay (from generational collection), and the ability
    to function without hardware and compiler support (from
    mostly-copying collection).

    The incremental collector runs on commercially-available
    uniprocessors, such as the DECStation 3100, without any special
    hardware support. It uses UNIX's user controllable page protection
    facility (mprotect) to synchronize between the scanner (of the
    collector) and the mutator (of the application program). Its
    implementation does not require any modification to the C++
    compiler. The maximum garbage collection pause is well within the
    100-millisecond limit imposed by real-time applications executing
    on interactive workstations. Compared to its non-incremental
    version, the total execution time of the incremental collector is
    not adversely affected.

* .. _YUASA90:

  Taiichi Yuasa. 1990. "Real-Time Garbage Collection on General-Purpose Machines". Journal of Software and Systems. 11:3 pp. 181--198.

  .. admonition:: Abstract

    An algorithm for real-time garbage collection is presented, proved
    correct, and evaluated. This algorithm is intended for
    list-processing systems on general-purpose machines, i.e., Von
    Neumann style serial computers with a single processor. On these
    machines, real-time garbage collection inevitably causes some
    overhead on the overall execution of the list-processing system,
    because some of the primitive list-processing operations must
    check the status of garbage collection. By removing such overhead
    from frequently used primitives such as pointer references (e.g.,
    Lisp car and cdr) and stack manipulations, the presented algorithm
    reduces the execution overhead to a great extent. Although the
    algorithm does not support compaction of the whole data space, it
    efficiently supports partial compaction such as array relocation.

* .. _ZORN88:

  Benjamin Zorn & Paul Hilfinger. 1988. "`A Memory Allocation Profiler for C and Lisp Programs <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.6.1689&rep=rep1&type=pdf>`_". USENIX. Proceedings for the Summer 1988 USENIX Conference, pp. 223--237.

  .. admonition:: Abstract

    This paper describes inprof, a tool used to study the memory
    allocation behavior of programs. mprof records the amount of
    memory each function allocates, breaks down allocation information
    by type and size, and displays a program's dynamic cal graph so
    that functions indirectly responsible for memory allocation are
    easy to identify. mprof is a two-phase tool. The monitor phase is
    linked into executing programs and records information each time
    memory is allocated. The display phase reduces the data generated
    by the monitor and displays the information to the user in several
    tables. mprof has been implemented for C and Kyoto Common Lisp.
    Measurements of these implementations are presented.

* .. _ZORN89:

  Benjamin Zorn. 1989. "`Comparative Performance Evaluation of Garbage Collection Algorithms <http://www.eecs.berkeley.edu/Pubs/TechRpts/1989/CSD-89-544.pdf>`_". Computer Science Division (EECS) of University of California at Berkeley. Technical Report UCB/CSD 89/544 and PhD thesis.

  .. admonition:: Abstract

    This thesis shows that object-level, trace-driven simulation can
    facilitate evaluation of language runtime systems and reaches new
    conclusions about the relative performance of important garbage
    collection algorithms. In particular, I reach the unexpected
    conclusion that mark-and-sweep garbage collection, when augmented
    with generations, shows comparable CPU performance and much better
    reference locality than the more widely used copying algorithms.
    In the past, evaluation of garbage collection algorithms has been
    limited by the high cost of implementing the algorithms.
    Substantially different algorithms have rarely been compared in a
    systematic way.

    With the availability of high-performance, low-cost workstations,
    trace-driven performance evaluation of these algorithms is now
    economical. This thesis describes MARS, a runtime system simulator
    that is driven by operations on program objects, and not memory
    addresses. MARS has been attached to a commercial Common Lisp
    system and eight large Lisp applications are used in the thesis as
    test programs. To illustrate the advantages of the object-level
    tracing technique used by MARS, this thesis compares the relative
    performance of stop-and-copy, incremental, and mark-and-sweep
    collection algorithms, all organized with multiple generations.
    The comparative evaluation is based on several metrics: CPU
    overhead, reference locality, and interactive availability.

    Mark-and-sweep collection shows slightly higher CPU overhead than
    stop-and-copy ability (5 percent), but requires significantly less
    physical memory to achieve the same page fault rate (30-40
    percent). Incremental collection has very good interactive
    availability, but implementing the read barrier on stock hardware
    incurs a substantial CPU overhead (30-60 percent). In the future,
    I will use MARS to investigate other performance aspects of
    sophisticated runtime systems.

* .. _ZORN90B:

  Benjamin Zorn. 1990. "Comparing Mark-and-sweep and Stop-and-copy Garbage Collection". ACM. Conference on Lisp and Functional Programming, pp. 87--98.

  .. admonition:: Abstract

    Stop-and-copy garbage collection has been preferred to
    mark-and-sweep collection in the last decade because its
    collection time is proportional to the size of reachable data and
    not to the memory size. This paper compares the CPU overhead and
    the memory requirements of the two collection algorithms extended
    with generations, and finds that mark-and-sweep collection
    requires at most a small amount of additional CPU overhead (3-6%)
    but requires an average of 20% (and up to 40%) less memory to
    achieve the same page fault rate. The comparison is based on
    results obtained using trace-driven simulation with large Common
    Lisp programs.

* .. _ZORN90:

  Benjamin Zorn. 1990. "`Barrier Methods for Garbage Collection <http://www.cs.colorado.edu/department/publications/reports/docs/CU-CS-494-90.pdf>`_". University of Colorado at Boulder. Technical Report CU-CS-494-90.

  .. admonition:: Abstract

    Garbage collection algorithms have been enhanced in recent years
    with two methods: generation-based collection and Baker
    incremental copying collection. Generation-based collection
    requires special actions during certain store operations to
    implement the "write barrier". Incremental collection requires
    special actions on certain load operations to implement the "read
    barrier". This paper evaluates the performance of different
    implementations of the read and write barriers and reaches several
    important conclusions. First, the inlining of barrier checks
    results in surprisingly low overheads, both for the write barrier
    (2%-6%) and the read barrier (&lt; 20%). Contrary to previous
    belief, these results suggest that a Baker-style read barrier can
    be implemented efficiently without hardware support. Second, the
    use of operating system traps to implement garbage collection
    methods results in extremely high overheads because the cost of
    trap handling is so high. Since this large overhead is completely
    unnecessary, operating system memory protection traps should be
    reimplemented to be as fast as possible. Finally, the performance
    of these approaches on several machine architectures is compared
    to show that the results are generally applicable.

* .. _ZORN91:

  Benjamin Zorn. 1991. "`The Effect of Garbage Collection on Cache Performance <http://www.cs.colorado.edu/department/publications/reports/docs/CU-CS-528-91.pdf>`_". University of Colorado at Boulder. Technical Report CU-CS-528-91.

  .. admonition:: Abstract

    Cache performance is an important part of total performance in
    modern computer systems. This paper describes the use of
    trace-driven simulation to estimate the effect of garbage
    collection algorithms on cache performance. Traces from four large
    Common Lisp programs have been collected and analyzed with an
    all-associativity cache simulator. While previous work has focused
    on the effect of garbage collection on page reference locality,
    this evaluation unambiguously shows that garbage collection
    algorithms can have a profound effect on cache performance as
    well. On processors with a direct-mapped cache, a generation
    stop-and-copy algorithm exhibits a miss rate up to four times
    higher than a comparable generation mark-and-sweep algorithm.
    Furthermore, two-way set-associative caches are shown to reduce
    the miss rate in stop-and-copy algorithms often by a factor of two
    and sometimes by a factor of almost five over direct-mapped
    caches. As processor speeds increase, cache performance will play
    an increasing role in total performance. These results suggest
    that garbage collection algorithms will play an important part in
    improving that performance.

* .. _ZORN92B:

  Benjamin Zorn & Dirk Grunwald. 1992. "`Empirical Measurements of Six Allocation-intensive C Programs <http://www.cs.colorado.edu/department/publications/reports/docs/CU-CS-604-92.pdf>`_". ACM, SIGPLAN. SIGPLAN notices, 27(12):71--80.

  .. admonition:: Abstract

    Dynamic memory management is an important part of a large class of
    computer programs and high-performance algorithms for dynamic
    memory management have been, and will continue to be, of
    considerable interest. This paper presents empirical data from a
    collection of six allocation-intensive C programs. Extensive
    statistics about the allocation behavior of the programs measured,
    including the distributions of object sizes, lifetimes, and
    interarrival times, are presented. This data is valuable for the
    following reasons: first, the data from these programs can be used
    to design high-performance algorithms for dynamic memory
    management. Second, these programs can be used as a benchmark test
    suite for evaluating and comparing the performance of different
    dynamic memory management algorithms. Finally, the data presented
    gives readers greater insight into the storage allocation patterns
    of a broad range of programs. The data presented in this paper is
    an abbreviated version of more extensive statistics that are
    publicly available on the internet.

* .. _ZORN92:

  Benjamin Zorn. 1993. "`The Measured Cost of Conservative Garbage Collection <http://www.cs.colorado.edu/department/publications/reports/docs/CU-CS-573-92.pdf>`_". Software -- Practice and Experience. 23(7):733--756.

  .. admonition:: Abstract

    Because dynamic memory management is an important part of a large
    class of computer programs, high-performance algorithms for
    dynamic memory management have been, and will continue to be, of
    considerable interest. Experience indicates that for many
    programs, dynamic storage allocation is so important that
    programmers feel compelled to write and use their own
    domain-specific allocators to avoid the overhead of system
    libraries. Conservative garbage collection has been suggested as
    an important algorithm for dynamic storage management in C
    programs. In this paper, I evaluate the costs of different dynamic
    storage management algorithms, including domain-specific
    allocators; widely-used general-purpose allocators; and a publicly
    available conservative garbage collection algorithm. Surprisingly,
    I find that programmer enhancements often have little effect on
    program performance. I also find that the true cost of
    conservative garbage collection is not the CPU overhead, but the
    memory system overhead of the algorithm. I conclude that
    conservative garbage collection is a promising alternative to
    explicit storage management and that the performance of
    conservative collection is likely to be improved in the future. C
    programmers should now seriously consider using conservative
    garbage collection instead of malloc/free in programs they write.

* .. _ZORN92A:

  Benjamin Zorn & Dirk Grunwald. 1994. "`Evaluating Models of Memory Allocation <http://www.cs.colorado.edu/department/publications/reports/docs/CU-CS-603-92.pdf>`_". ACM. Transactions on Modeling and Computer Simulation 4(1):107--131.

  .. admonition:: Abstract

    Because dynamic memory management is an important part of a large
    class of computer programs, high-performance algorithms for
    dynamic memory management have been, and will continue to be, of
    considerable interest. We evaluate and compare models of the
    memory allocation behavior in actual programs and investigate how
    these models can be used to explore the performance of memory
    management algorithms. These models, if accurate enough, provide
    an attractive alternative to algorithm evaluation based on
    trace-driven simulation using actual traces. We explore a range of
    models of increasing complexity including models that have been
    used by other researchers. Based on our analysis, we draw three
    important conclusions. First, a very simple model, which generates
    a uniform distribution around the mean of observed values, is
    often quite accurate. Second, two new models we propose show
    greater accuracy than those previously described in the
    literature. Finally, none of the models investigated appear
    adequate for generating an operating system workload.


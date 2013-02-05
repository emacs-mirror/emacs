.. _glossary-f:

=============================
Memory Management Glossary: F
=============================

.. include:: alphabet.txt

.. glossary::

    fencepost
    fence post

        A fencepost is spare :term:`memory (1)` between
        :term:`allocated` :term:`blocks` for checking purposes.

        Some :term:`memory management` systems leave spare memory
        between allocated blocks and store special values in it. If a
        checking routine finds that these memory :term:`locations
        <memory location>` have been modified, this probably indicates
        an :term:`overwriting error` in the application that was
        allocated the adjacent block.

        Such checking can help application programmers to find bugs
        that would otherwise be difficult to reproduce and track down.

        .. similar:: :term:`in-band header`.

    fencepost error
    fence post error

        The term *fencepost error* refers to errors arising from the
        fact that, to enclose *n* consecutive intervals, you need
        *n* + 1 end-points, from the number of posts required to
        support fence rails.

        An example of a fencepost error would be, in :term:`C`::

            void f(void)
            {
              int i;
              int a[10];
              for(i = 0; i <= 10; i++)
                a[i] = 0;
            }

        because the declaration ``int a[10];`` creates an array of ten
        integers, with indices from 0 to 9, but the ``for`` loop index
        ``i`` runs from 0 to 10.

    Fibonacci buddies

        A common :term:`buddy system` :term:`allocation mechanism`, in
        which block sizes form a Fibonacci series (each block size is
        the sum of the two previous sizes). Each block can therefore
        be :term:`split` to form two blocks of valid sizes, and the
        sizes are more closely spaced than in :term:`binary buddies`.
        However, if the same size is allocated repeatedly, performance
        may suffer as the remainder blocks may have to be split again
        (or become fragments).

        .. seealso:: :term:`buddy system`, :term:`allocation mechanism`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    FIFO-ordered first fit

        The :term:`allocation policy` that always uses the
        least-recently :term:`freed (1)` suitable :term:`free
        block`. Commonly implemented by adding freed blocks to the end
        of a :term:`free block chain`, and then using :term:`first
        fit` allocation on this chain. :term:`free (1)` can be very
        quick, depending on the :term:`coalescing <coalesce>` policy.

        According to :ref:`Wilson et al. (1995) <WIL95>`, this policy
        controls fragmentation quite well, better than
        :term:`LIFO-ordered first fit` and as well as
        :term:`address-ordered first fit` in some cases, although
        :term:`locality <locality of reference>` may be worse.

    file mapping

        .. see:: :term:`memory mapping`.

    finalization

        .. aka:: *termination*.

        In :term:`garbage-collected <garbage collection>` languages,
        it is often necessary to perform actions on some
        :term:`objects` after they are no longer in use and
        before their :term:`memory (2)` can be :term:`recycled
        <recycle>`. These actions are known as *finalization* or
        *termination*.

        A common use of finalization is to release resources when the
        corresponding "proxy" object dies. For example, an open file
        might be represented by a stream object. When this object has
        been proven :term:`dead` by the :term:`collector (1)`, it is
        certain that the file is no longer in use by the program, and
        it can and should be closed before the stream is recycled.

        Note that finalization is not, in general, guaranteed to be
        prompt, and this can cause problems if it is used to manage
        scarce operating system resources such as file descriptors.

        Many object-oriented languages provide support for
        finalization, for example, Cedar, :term:`Java`, :term:`Perl`
        5, and :term:`Smalltalk`.

        The term *finalization* is sometimes used to refer to the use
        of :term:`destructors (1)`, for example in Ada.

        .. mps:specific:: See :ref:`topic-finalization`.

    finalized block

        .. mps:specific::

           A :term:`block` that has been registered for finalization
           using :c:func:`mps_finalize`, and which the MPS has
           determined is :term:`dead`, but whose finalization message
           has not been discarded. See
           :c:func:`mps_message_type_finalization`.

    first fit

        First fit is a :term:`sequential fit` :term:`allocation
        mechanism`.

        To quote :ref:`Wilson et al. (1995) <WIL95>`:

            First fit simply searches the :term:`free list` from the
            beginning, and uses the first :term:`free block` large
            enough to satisfy the request. If the block is larger than
            necessary, it is split and the remainder is put on the
            free list.

        The first fit mechanism provides a class of first fit
        :term:`allocation policies`, depending on the order in which
        the free list is stored. :term:`Address-ordered first fit`
        stores the list in order of (usually increasing) address.
        :term:`LIFO-ordered first fit` puts blocks on the front of the
        free list when they are :term:`freed (1)`. :term:`FIFO-ordered
        first fit` puts blocks on the end of the free list when they
        are :term:`freed (1)`.

        .. seealso:: :term:`address-ordered first fit`, :term:`LIFO-ordered first fit`, :term:`FIFO-ordered first fit`, :term:`sequential fit`, :term:`next fit`, :term:`best fit`, :term:`worst fit`.

    fix

        .. mps:specific::

            To *fix* a :term:`reference` from one :term:`block` to
            another is to declare it to the MPS by calling
            :c:func:`MPS_FIX1` and :c:func:`MPS_FIX2` within a
            :term:`scan method`. In a :term:`moving <moving garbage
            collector>` :term:`pool`, fixing a reference may also
            update it to point to the new location of the block. See
            :ref:`topic-scanning`.

    flip

        The instant in a :term:`two-space collector` when the roles of
        the two :term:`semi-spaces` are reversed. What
        was the :term:`tospace` is now marked as :term:`fromspace` and
        :term:`condemned <condemned set>`. What was the fromspace
        becomes the site for all new :term:`allocations
        <allocate>`. Also used in a more general sense to mean the
        initiation of a new :term:`collection cycle`.

        .. figure:: ../diagrams/two-space.svg
            :align: center
            :alt: Diagram: In a two-space collector, the *flip* occurs just before the fromspace is condemned and copying starts.

            In a two-space collector, the *flip* occurs just before the
            fromspace is condemned and copying starts.

    floating garbage

        Floating garbage is :term:`garbage` that is not
        :term:`recycled` promptly due to some approximation
        or optimization in the :term:`garbage collector`.

        Floating garbage results from conservatively estimating an
        :term:`object` that is really :term:`unreachable` to be
        :term:`reachable` for the purposes of a particular
        :term:`collection cycle`. Using estimates can have
        considerable performance benefits but also result in higher
        :term:`memory (2)` consumption.

        Typical estimates that cause floating garbage are:

        1. Every register or :term:`activation frame` slot holds a
           reachable value: this is not always true, as objects stored
           in dead registers or slots may be otherwise unreachable.
           This estimate can simplify the compiler as well as the
           interface between the compiler and the garbage collector.

        2. Every object in a :term:`remembered set` is reachable: this
           is not always true, because remembered objects can have
           become unreachable since they were added to the remembered
           set. This estimate allows remembered sets to be effective;
           the alternative—determining whether each remembered object
           is reachable—is equivalent to a full garbage collection.

        3. Anything that looks like a :term:`reference` is one: this
           is not generally true, because random data can have the
           same bit pattern as a pointer. :term:`Conservative garbage
           collectors <conservative garbage collection>` use this
           estimate.

        4. Any object referenced from another is reachable: this is
           not generally true, because garbage can reference other
           garbage. :term:`Reference counting` collectors use this
           estimate, resulting in their not being able to reclaim
           self-referential structures.

        5. Any object reached during collection remains live until the
           next collection: this may not be true when the garbage
           collector runs interleaved with the mutator, as do
           :term:`incremental <incremental garbage collection>` and
           :term:`concurrent <parallel garbage collection>`
           collectors.

        A more subtle kind of floating garbage is an unreachable data
        structure that spans multiple regions that are never
        :term:`condemned <condemned set>` together.

    foreign code

        .. mps:specific::

            From the point of view of the :term:`client program`,
            *foreign code* is external code (not part of the client
            program, or the MPS), which is not aware of and does not
            co-operate with the MPS. The client program must take care
            when passing the address of a block in a :term:`moving
            <moving memory manager>` :term:`pool` to foreign code.

            The :ref:`pool-lo` :term:`pool class` is designed for this
            use case: blocks allocated from this pool do not move and
            are never protected, and so may be passed safely to
            foreign code.

    format

        A format describes the representation of an :term:`object`;
        that is, how the object is laid out in memory.

        A format usually specifies where the fields of the objects are
        located and what their type is.

        .. relevance::

            If formats are provided by a language or the application
            program, :term:`exact garbage collection` can be used,
            because the :term:`collector (1)` can determine which
            fields are :term:`references`.

        .. seealso:: :term:`conservative garbage collection`.

    format method

        .. mps:specific::

            One of the methods in an :term:`object format`, defined by
            the :term:`client program` in order to describe its
            objects to the MPS. May be a :term:`scan method`,
            :term:`skip method`, :term:`forward method`,
            :term:`is-forwarded method`, or :term:`padding method`.

    formatted object

        .. mps:specific::

            An allocated :term:`block` that belongs to an
            :term:`object format` and may be :term:`scanned <scan>` by
            the :term:`garbage collector`. See :ref:`topic-format`.

    forward method
    
        .. mps:specific::

            A :term:`format method` that is called by a :term:`moving
            <moving garbage collector>` :term:`pool <pool>` when it
            has moved an object. The forward method replaces the old
            object with a :term:`forwarding marker` that points to the
            new location of the object. See :c:type:`mps_fmt_fwd_t`.

    forwarding marker
    forwarding object
    forwarding pointer

        Some :term:`garbage collectors`
        :term:`move <moving garbage collector>` :term:`reachable`
        :term:`objects` into another space. They leave a
        forwarding pointer, a special :term:`reference` pointing to
        the new :term:`location <memory location>`, in the old
        location.

        .. similar:: :term:`broken heart`.

        .. seealso:: :term:`copying garbage collection`, :term:`two-space collector`.

        .. mps:specific::

            The term *forwarding object* is used. This is a
            :term:`formatted object` that has been replaced by a
            :term:`forwarding marker`. One of three types of formatted
            objects, the other two being :term:`client objects` and
            :term:`padding objects`.

    fragmentation

        Fragmentation is the inability to use :term:`memory (1)`
        because of the arrangement of memory already in use. It is
        usually divided into :term:`external fragmentation` and
        :term:`internal fragmentation`.

        .. bibref:: :ref:`Johnstone & Wilson (1998) <JW98>`.

    frame

        .. see:: :term:`in-band header`.

    free (1)

        .. aka:: *deallocate*.

        In :term:`manual memory management`, to free or deallocate an
        :term:`object` is to tell the :term:`memory manager` that it
        is no longer needed. The :term:`memory (1)` may then be
        :term:`recycled` by being used for subsequent
        :term:`allocation <allocate>`, or by being returned to the
        operating system.

        .. opposite:: :term:`allocate`.

        .. seealso:: :term:`free (2)`, :term:`destructor (1)`.

    free (2)

        In :term:`C`, the system function used for explicit
        :term:`deallocation <free (1)>` is called ``free``.

    free (3)

        :term:`Memory (2)` is *free* if it is not currently
        :term:`allocated`.

        .. historical::

            The term *available* was commonly used to mean "free".

        .. opposite:: :term:`allocated`.

        .. seealso:: :term:`free (1)`.

    free (4)

        .. see:: :term:`unmapped`.

    free block

        A single contiguous area of :term:`memory (2)` available to
        satisfy an :term:`allocation <allocate>` request.

        For the purpose of discussing :term:`allocation mechanisms`,
        two adjacent free blocks are not considered to be a single
        free block, until they are :term:`coalesced`. Free blocks may
        be :term:`split`.

        .. seealso:: :term:`allocation mechanism`, :term:`free list`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    free block chain

        Some systems store the :term:`free list` as a linked list, or
        chain.

        Usually the links are stored within the :term:`free (3)`
        :term:`blocks`. This means that all :term:`allocated` blocks
        must be large enough to store these, and implies a minimum
        size.

        Sometimes, the free block chain is ordered by :term:`address`.
        This makes :term:`coalescence <coalesce>` considerably
        cheaper, but :term:`deallocation <free (1)>` more expensive.

        .. seealso:: :term:`free list`.

    free list

        The free list is the set of :term:`free blocks`.

        Originally this term meant the single linked list of all free
        blocks, but as :term:`allocation mechanisms` have become more
        varied, it has become more generic, and now may be implemented
        as a tree or other data structure rather than a linked list.
        If the implementation actually is a linked list of free
        blocks, this is called a :term:`free block chain` to
        distinguish it from the abstract term.

        There may be several free lists, classed by size or other
        characteristic. For instance, :term:`segregated free list`
        systems classify free lists by block size.

        .. seealso:: :term:`free block`, :term:`free block chain`.

    free store

        .. see:: :term:`heap`.

    freestore

        .. see:: :term:`heap`.

    from space
    fromspace

        .. aka:: *old space*, *oldspace*.

        In :term:`copying garbage collection`, the space containing a
        mixture of :term:`live` and :term:`dead` objects, out of which
        the former are copied.

        .. opposite:: :term:`tospace`.

    function pointer

        In the :term:`C` programming language, a :term:`pointer` to an
        function, as distinct from a :term:`object pointer`. The C
        programming language does not guarantee that function and
        object pointers are the same size, or that a pointer of one
        type can be cast to a pointer of the the other type without
        losing information (but on every mainstream C implementation,
        including all those supported by the MPS, they are in fact the
        same).

        .. opposite:: :term:`object pointer`.

    function record

        .. see:: :term:`activation record`.


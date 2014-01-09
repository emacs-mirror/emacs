.. _glossary-b:

=============================
Memory Management Glossary: B
=============================

.. include:: alphabet.txt

.. glossary::

    backing store

        Backing :term:`store (2)` is typically part of a hard disk
        that is used by a :term:`paging` or :term:`swapping` system to
        store information not currently in :term:`main memory`.
        Backing store is slower and cheaper than main memory.

        Other :term:`storage <memory (1)>` may, less commonly, be used
        in place of a hard disk (for instance, magnetic tape, floppy
        disk, or historically, magnetic drum).

        In general, backing store may mean any locations used to store
        information when its preferred or natural location is
        otherwise being used: for example, memory used by a graphical
        interface to keep a copy of the contents of obscured windows.

        .. similar:: :term:`swap space`.

    barrier (1)

        A barrier is a block on reading from or writing to certain
        :term:`memory (2)` :term:`locations <memory location>` by
        certain threads or processes.

        Barriers can be implemented in either software or hardware.
        Software barriers involve additional instructions around
        :term:`load` or :term:`store (1)` operations, which would
        typically be added by a cooperative compiler. Hardware
        barriers don't require compiler support, and may be
        implemented on common operating systems by using :term:`memory
        protection`.

        .. relevance::

            Barriers are used for :term:`incremental <incremental
            garbage collection>` or :term:`concurrent <parallel
            garbage collection>` :term:`garbage collection`.

        .. seealso:: :term:`read barrier`, :term:`write barrier`.

        .. bibref:: :ref:`Pirinen (1998) <PIRINEN98>`, :ref:`Zorn (1990) <ZORN90>`.

    barrier (2)

        A memory barrier is an instruction on certain processor
        architectures that will ensure certain guarantees about the
        order of accesses to memory.

        Some processor architectures make very few guarantees about
        the relative orders of :term:`load` and :term:`store (1)`
        operations in the instruction stream and the actual order of
        accesses to :term:`main memory`. These architectures will
        often have special instructions that make stronger guarantees.

        For example, the ARM has the ``DMB`` (Data Memory Barrier)
        instruction:

            It ensures that all explicit memory accesses that appear
            in program order before the DMB instruction are observed
            before any explicit memory accesses that appear in program
            order after the DMB instruction.

        These instructions are vital for certain synchronization
        operations.

    barrier hit

        .. see:: :term:`protection fault`.

    base pointer

        A *base pointer* is a :term:`pointer` to the base or start of
        an :term:`object`.

        This term is commonly used in opposition to :term:`derived
        pointer`.

        Note that :ref:`Boehm & Chase (1992) <BC92A>` define "base
        pointer" to be "any pointer value directly recognizable by the
        :term:`collector (1)`", and this may well include
        :term:`interior pointers`.

        .. opposite:: :term:`derived pointer`.

        .. mps:specific::

            For objects with :term:`in-band headers`, the MPS
            distinguishes between the base pointer, which points to
            the start of the header, and the :term:`client pointer`,
            which points to the first word after the end of the
            header.

    best fit

        The :term:`allocation policy` that always allocates from the
        smallest suitable :term:`free block`. Suitable
        :term:`allocation mechanisms` include :term:`sequential fit`
        searching for a :term:`perfect fit`, :term:`first fit` on a
        size-ordered :term:`free block chain`, :term:`segregated
        fits`, and :term:`indexed fits`. Many :term:`good fit`
        allocators are also described as :term:`best fit`.

        In theory, best fit may exhibit bad :term:`fragmentation`, but
        in practice this is not commonly observed.

        .. seealso:: :term:`allocation policy`, :term:`first fit`, :term:`sequential fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    BIBOP

        .. aka:: *big bag of pages*.

        BIBOP, or *BIg Bag Of Pages*, is a technique that encodes
        :term:`object` type in the high-order bits of their
        :term:`address`, by using a lookup table that maps from those
        bits to a type.

        Despite the name, the blocks involved need not be the size of
        a :term:`page`.

        BIBOP requires storing only objects of the same type in a
        block, but this has the same advantages as :term:`segregated
        fits` in general.

        .. historical::

            This technique was invented for the PDP-10 MACLISP by JonL
            White and Stavros Macrakis. It was an advance on earlier
            techniques that divided the :term:`address space` into
            contiguous blocks for each type.

        .. bibref:: :ref:`Baker (1979) <BAKER79>`, :ref:`Steele (1977) <STEELE77>`.

    big bag of pages

        .. see:: :term:`BIBOP`.

    binary buddies

        The most common :term:`buddy system` :term:`allocation
        mechanism`, in which all block sizes are a power of two.
        Finding a block's buddy is then a matter of flipping the
        appropriate bit in the block's address.

        :term:`Internal fragmentation` is usually high, because
        objects are often not a good fit for power-of-two sized
        blocks.

        .. seealso:: :term:`buddy system`, :term:`allocation mechanism`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    bit array

        .. see:: :term:`bitmap`.

    bit table

        .. see:: :term:`bitmap`.

    bit vector

        .. see:: :term:`bitmap`.

    bitmap

        .. aka:: *bit array*, *bit table*, *bit vector*, *bitset*.

        A table of bits.

        .. relevance::

            Bitmaps are sometimes used to represent the marks in a
            :term:`mark-sweep` collector, or the used memory in a
            :term:`bitmapped fits` :term:`allocator`.

    bitmapped fit

        A class of :term:`allocation mechanisms` that use a
        :term:`bitmap` to represent the usage of the :term:`heap`.
        Each bit in the map corresponds to a part of the heap,
        typically a :term:`word`, and is set if that part is in use.
        Allocation is done by searching the bitmap for a run of clear
        bits.

        Bitmapped fit mechanisms have good :term:`locality of
        reference`, as they avoid examining :term:`in-band headers`
        when allocating.

        .. seealso:: :term:`allocation mechanism`, :term:`sequential fit`, :term:`indexed fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    bitmask

        A :term:`bitmap` used to select or exclude a set of bits in
        another bitmap.

    bitset

        .. see:: :term:`bitmap`.

    black

        In a :term:`tri-color marking` scheme, black :term:`objects`
        are objects that have been :term:`scanned <scan>`.

        More precisely, black objects have been noted
        :term:`reachable` and the :term:`collector (2)` has finished
        with them and need not visit them again (for the purposes of
        :term:`tracing <trace>`).

        .. opposite:: :term:`white`, :term:`gray`.

    blacklisting
    black-listing

        A :term:`conservative garbage collector <conservative garbage
        collection>` can be made more effective by *blacklisting*
        values which resemble :term:`addresses` that may be
        :term:`allocated` at in the future, but are known not to be
        :term:`pointers` . This list is then used to avoid allocation
        at those addresses.

        For example, such values can be gathered by scanning the
        :term:`roots` before any :term:`objects` have been allocated.

        .. bibref:: :ref:`Boehm (1993) <BOEHM93>`.

    block

        Block is a vague term for an (often contiguous) area of
        :term:`memory (1)`. Often used to describe :term:`memory (2)`
        :term:`allocated` by an :term:`allocator` such as
        :term:`malloc`.

        .. mps:specific::

            The term *block* is used as a general term for a unit of
            allocation, with *object* being reserved for
            :term:`formatted objects`.

    bounds error

        .. see:: :term:`overwriting error`.

    boxed

        Boxed :term:`objects` are represented by a :term:`pointer` to
        a :term:`block` of :term:`memory (2)` that contains the object
        data. Sometimes the pointer is :term:`tagged <tag>` to
        distinguish it from an :term:`unboxed` object, or to represent
        its type. Only the pointer is duplicated when the object is
        passed around, so updates to the object are reflected
        everywhere.

        .. opposite:: :term:`unboxed`.

        .. seealso:: :term:`tag`, :term:`BIBOP`.

        .. bibref:: :ref:`Gudeman (1993) <GUDEMAN93>`.

    break-table

        A break-table is a data structure used by a
        :term:`mark-compact` collector to store the :term:`relocation`
        information.

        .. seealso:: :term:`mark-compact`.

    brk

        ``brk`` is a Unix system call that sets the limit of the data
        segment. This limit is known as the *break*.

        The :term:`C` library implementation of :term:`malloc` usually
        :term:`allocates` :term:`memory (2)` for the
        :term:`heap` by extending the data segment using ``brk`` or
        :term:`sbrk`.

        Most implementations of ``malloc`` never shrink the data
        segment, so the memory usage of a process never decreases. In
        most Unix systems, the data segment resides immediately above
        the program code (text segment) in the :term:`address space`.

        .. figure:: ../diagrams/brk.svg
            :align: center
            :alt: Diagram: A simplified view of the address space of a Unix process.

            A simplified view of the address space of a Unix process.

    broken heart

        :term:`Copying garbage collectors <copying garbage
        collection>` :term:`move <moving garbage collector>`
        :term:`reachable` :term:`objects` into another
        :term:`semi-space`. They leave a :term:`forwarding pointer` in
        the old :term:`location <memory location>`, pointing to the
        new. The object at the old location is known as a broken
        heart.

        .. similar:: :term:`forwarding pointer`.

    bucket

        In a :term:`generational garbage collector <generational
        garbage collection>`, it is often desirable to divide
        :term:`generations` by the age of the
        :term:`object`. These divisions are known as buckets.

        .. seealso:: :term:`generational garbage collection`, :term:`aging space`, :term:`creation space`.

    buddy system

        Buddy systems are a subclass of :term:`strict segregated fit`
        :term:`allocation mechanisms` which
        make :term:`splitting <split>` and :term:`coalescing
        <coalesce>` fast by pairing each block with a unique adjacent
        *buddy* block.

        There is an array of :term:`free lists`, one for
        each allowable block size. Allocation rounds up the requested
        size to an allowable size and allocates from the corresponding
        free list. If the free list is empty, a larger block is
        selected and split. A block may only be split into a pair of
        buddies.

        A block may only be coalesced with its buddy, and this is only
        possible if the buddy has not been split into smaller blocks.

        The advantage of buddy systems is that the buddy of a block
        being freed can be quickly found by a simple address
        computation. The disadvantage of buddy systems is that the
        restricted set of block sizes leads to high :term:`internal
        fragmentation`, as does the limited ability to coalesce.

        Different sorts of buddy system are distinguished by the
        available block sizes and the method of splitting. They
        include :term:`binary buddies` (the most common),
        :term:`Fibonacci buddies`, :term:`weighted buddies`, and
        :term:`double buddies`.

        .. seealso:: :term:`allocation mechanism`, :term:`segregated free lists`, :term:`segregated fit`, :term:`strict segregated fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    buffer

        A *buffer* is a large :term:`block` of :term:`memory (2)` from
        which blocks are :term:`allocated` contiguously, as a simple
        technique for fast :term:`allocation <allocate>`.

        By keeping only a *high-water* mark (that is, a
        :term:`pointer` to the start of unused memory), the buffer
        technique avoids expensive :term:`in-band headers` and the
        searching of :term:`free block chains`. Buffers tend to,
        however, lead to :term:`external fragmentation`.

        .. bibref:: :ref:`Appel et al. (1988) <AEL88>`.

        .. mps:specific::

            Buffers are implemented using :term:`allocation points`
            attached to :term:`pools`.

    bus error

        Strictly speaking, *a bus error* is a fault on a hardware bus,
        such as when an invalid :term:`address` is issued.

        Generally, any hardware exception caused by a :term:`memory
        (2)` access (for example, :term:`loading <load>` an
        :term:`unaligned` :term:`word`) is termed a *bus error*. The
        term is often used more loosely as a synonym for any memory
        access error.

        .. seealso:: :term:`segmentation violation`.

    byte (1)

        A unit of storage measurement, equal to 8 bits.

        It does not matter how the bits are arranged: a byte is just a
        quantity.

        This is the sense of byte used in the terms :term:`kilobyte`,
        :term:`megabyte`, :term:`gigabyte`, :term:`terabyte`, etc. The
        prefixes in these terms derive from the SI prefixes for powers
        of 1000, but since powers of two are much more common in
        binary computers, they are used to denote powers of 1024 (2\
        :sup:`10`).

        .. seealso:: :term:`word`.

    byte (2)

        A data type defined by a processor architecture.

        For example, the smallest :term:`addressable <address>`
        :term:`memory location` on the Intel x86 family is the 8-bit
        byte.

        .. historical::

            The PDP-10 had 36-bit :term:`words`, and defined "byte" to
            be a general sub-:term:`word` bit-field: compare
            :term:`byte (3)`. On this machine it was commonplace for
            characters to be packed four or five to a word using 9- or
            7-bit bytes respectively.

        .. seealso:: :term:`word`.

    byte (3)

        A contiguous set of bits used to represent a range of values
        compactly.

        The number of bits in a byte is a measure of the information
        content of the byte. An *n*-bit byte can represent 2\ :sup:`n`
        distinct values.

        Bytes may be packed into (or otherwise stored in bit-fields
        of) integers, words, or other aligned values for space
        efficiency.

    byte (4)

        A data type or storage unit defined by a programming language.

        In ANSI/ISO :term:`C`, "the unit of data storage large enough
        to hold the basic character set of the execution environment".
        In this sense, it is often used synonymously with the C type
        ``char``. C defines ``sizeof(char)`` to be 1. Many
        architectures that run C programs equate this sense of byte
        and :term:`byte (2)`.


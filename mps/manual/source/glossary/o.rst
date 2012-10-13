.. _glossary-o:

=============================
Memory Management Glossary: O
=============================

.. include:: alphabet.txt

.. glossary::

    object

        .. aka:: *cell*.

        In :term:`memory management`, we use the term *object* or
        *cell* to mean a contiguous :term:`block` of :term:`memory
        (2)` forming a single logical structure.

        Objects are the units of :term:`allocation <allocate>`,
        :term:`deallocation <free (1)>`, etc. No connection to an
        object-oriented system is implied.

        .. mps::

            The MPS documentation reserves the term *object* for
            :term:`formatted objects <formatted object>` only. For
            units of allocation in general, it uses the term
            :term:`block`.

    object format

        .. mps:: ??

    object pointer

        .. mps:: ??

    off-white

        In a :term:`treadmill` :term:`garbage collector`, the
        :term:`color` off-white is used to describe :term:`objects
        <object>` which are :term:`free (3)`.

        .. opposite:: :term:`white`, :term:`gray`, :term:`black`.

        .. seealso:: :term:`treadmill`, :term:`color`.

    one-bit reference count

        The one-bit :term:`reference count <reference counting>` is a
        heuristic mechanism that lets a program test, at low cost,
        whether an :term:`object` is :term:`dead`.

        The one-bit reference count is a special case of the
        :term:`limited-field reference count`. A single bit in an
        object, called the MRB (Multiple Reference Bit), is cleared
        when the object is :term:`allocated <allocate>`. Whenever
        another :term:`reference` to the object is created, the bit is
        set. Thus, MRB=0 indicates that there is exactly one reference
        to the object, and MRB=1 indicates that there may be more than
        one reference to the object.

        The MRB can be stored in the reference rather than in the
        object; doing so reduces the number of memory accesses due to
        MRB checking and setting. When a reference is copied, the
        copy's MRB is set. If the MRB in the old reference is 0, it
        also needs to be set. Setting the MRB in the old reference
        requires that the program knows the location the old reference
        came from, and that it can prove that location has not since
        been overwritten with other data.

        The one-bit reference count is used by a compiler to augment
        an object lifetime analysis. When compile-time analysis
        predicts that a particular object may be dead (typically
        because the variable that references the object is dead), the
        compiler can generate code that will check the object's MRB at
        run-time. If the MRB is 0, then the object is dead.

        Using a one-bit reference count does have a cost: the MRB uses
        space that could sometimes be put to other use, and the MRB
        must be set every time the number of references to the object
        increases. The one-bit reference count is cheaper than other
        kinds of reference counting, however, since the space cost is
        only one bit and the reference count is not adjusted when
        references are destroyed.

        .. historical::

            The one-bit reference count was suggested by Friedman and
            Wise [FW77]_. Storing the MRB in the reference was
            suggested by Stoye, Clarke, and Norman [SCN84]_.

        .. bibref:: [JONES96]_.

    out parameter

        A function parameter that points to a location for the caller
        to receive data from the function.

        .. opposite:: :term:`in parameter`.

        .. mps::

            Out parameters are given names ending with ``_o``.

    out-of-band header

        In some :term:`memory managers <memory manager>`, each
        :term:`allocated <allocate>` :term:`block` has additional
        information (such as the size of the block or a :term:`tag`)
        stored in a separate block; this is called *an out-of-band
        header*.

        .. opposite:: :term:`in-band header`.

    overcommit

        In some circumstances, although a range of :term:`virtual
        addresses <virtual address>` has been :term:`mapped` as far as
        the user program is concerned, the :term:`physical storage`
        might not be allocated until it is accessed. This is called
        *overcommitting*.

        Overcommitting shares :term:`swap space` resources more
        flexibly, especially when crude :term:`suballocators
        <suballocator>` are involved, but it can lead to an
        out-of-resource error during a :term:`memory (2)` access; few
        environments deal with this situation gracefully.

        Unix systems such as IRIX and AIX can do this on :term:`sbrk`
        and :term:`mmap` calls.

    overwriting error

        .. aka:: *bounds error*.

        An overwriting or bounds error occurs when the programmer
        intends his program to write to a particular :term:`block` of
        :term:`memory (1)`, but a program error causes the program to
        write outside the bounds of that block.

        .. seealso:: :term:`fencepost`.

.. _glossary-n:

=============================
Memory Management Glossary: N
=============================

.. include:: alphabet.txt

.. glossary::

    nailing

        .. see:: :term:`pinning`.

    natural alignment

        Natural alignment is an :term:`alignment` constraint such that
        all :term:`objects` must be aligned to an address
        that is a multiple of their size.

        Natural alignment is not usually required for objects larger
        than a :term:`word` or :term:`grain`, which usually only need
        to be word- or grain-aligned.

        .. seealso:: :term:`alignment`, :term:`padding`.

        .. mps:specific::

            The MPS platform interface defines the :term:`C`
            preprocessor macro :c:macro:`MPS_PF_ALIGN` to be the
            natural alignment of the platform.

    nepotism

        In :term:`generational garbage collection` nepotism is the
        tendency for :term:`dead` :term:`objects` in old
        :term:`generations` to preserve younger dead
        objects that are referenced by them. In other words, dead
        parents can cause their children to get promoted.

        This happens when an object gets :term:`promoted <promotion>`
        to an old generation and dies there, but does not get
        :term:`reclaimed` because the generation it is in does not get
        considered for garbage collection very often. The old object
        might refer to objects in younger generations that are also
        dead; until the old object is reclaimed the younger objects
        will be preserved by virtue of the :term:`reference` from the
        older, assumed alive, object.

        This is a form of :term:`floating garbage` introduced by
        partitioning the objects into generations.

    next fit

        A variant of the :term:`first fit` :term:`allocation
        mechanism` that uses a *roving pointer* on a circular
        :term:`free block chain`. The pointer is advanced along the
        chain when searching for a fit. Thus each allocation begins
        looking where the previous one finished. The rationale is to
        avoid creating an accumulation of small fragments at the head
        of the free block chain, which would have to be examined on
        every allocation.

        There are several variants, according to the order of blocks
        on the free block chain. The most common variant is
        address-ordered next fit.

        This has a tendency to spread related objects out in memory,
        and also gives quite poor :term:`locality <locality of
        reference>` for the allocator (as the roving pointer rotates
        around memory, the free blocks touched are those
        least-recently used).

        .. seealso:: :term:`allocation mechanism`, :term:`first fit`.

        .. bibref:: :ref:`Wilson et al. (1995) <WIL95>`.

    new space
    newspace

        .. see:: :term:`tospace`.

    node

        In a :term:`graph`, a node is a representation of an
        :term:`object` at the junction of zero or more :term:`edges`.

        .. opposite:: :term:`edge`.

        .. seealso:: :term:`graph`.

    non-moving garbage collector
    non-moving memory manager

        A memory manager is said to be *non-moving* if
        :term:`allocated` :term:`objects` do not move during their
        lifetimes.

        Non-moving memory management techniques include
        :term:`mark-sweep` collection, :term:`reference counting`, and
        most kinds of :term:`manual memory management`.

        .. opposite:: :term:`moving garbage collector`.

    nursery generation

        .. see:: :term:`nursery space`.

    nursery space

        .. aka:: *nursery generation*.

        In :term:`generational garbage collection`, the *nursery
        generation* or *space* is the area used for new
        :term:`allocation <allocate>`.

        The size of the nursery space must be chosen carefully. Often
        it is related to the size of :term:`physical memory (1)`.

        .. mps:specific::

            By default, a garbage-collected :term:`pool` allocates
            into the first :term:`generation` in its :term:`generation
            chain`, but this can be altered by setting the
            :c:macro:`MPS_KEY_GEN` :term:`keyword argument` when
            calling :c:func:`mps_pool_create_k`.

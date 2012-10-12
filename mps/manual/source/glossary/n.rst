.. _glossary-n:

===========
Glossary: N
===========

.. glossary::

    natural alignment

        Natural alignment is an :term:`alignment` constraint such that
        all :term:`objects <object>` must be aligned to an address
        that is a multiple of their size.

        Natural alignment is not usually required for objects larger
        than a :term:`word` or :term:`grain`, which usually only need
        to be word- or grain-aligned.

        .. seealso:: :term:`alignment`, :term:`padding`.

    nepotism

        In :term:`generational garbage collection` nepotism is the
        tendency for :term:`dead` :term:`objects <object>` in old
        :term:`generations <generation>` to preserve younger dead
        objects that are referenced by them. In other words, dead
        parents can cause their children to get promoted.

        This happens when an object gets :term:`promoted <promotion>`
        to an old generation and dies there, but does not get
        :term:`reclaimed <reclaim>` because the generation it is in
        does not get considered for garbage collection very often. The
        old object might refer to objects in younger generations that
        are also dead; until the old object is reclaimed the younger
        objects will be preserved by virtue of the :term:`reference`
        from the older, assumed alive, object.

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

        .. seealso:: :term:`first fit`, :term:`allocation mechanism`.

        .. bibref:: [WIL95]_.

    node

        In a :term:`graph`, a node is a representation of an
        :term:`object` at the junction of zero or more :term:`edges
        <edge>`.

        .. opposite:: :term:`edge`.

        .. seealso:: :term:`graph`.

    non-moving

        .. mps:: ??

    nursery generation

        .. see:: :term:`nursery space`.

    nursery space

        .. aka:: *nursery generation*.

        In :term:`generational garbage collection`, the *nursery
        :term:`generation`* or *space* is the area used for new
        :term:`allocation <allocate>`.

        The size of the nursery space must be chosen carefully. Often
        it is related to the size of :term:`physical memory (1)`.



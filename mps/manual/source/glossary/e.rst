.. _glossary-e:

=============================
Memory Management Glossary: E
=============================

.. include:: alphabet.txt

.. glossary::

    edge

        In a :term:`graph`, an edge is a connection between two
        :term:`nodes <node>`.

        In a directed graph (digraph), edges have a direction;
        otherwise the start and end nodes are interchangeable. By
        convention, two directed edges between the same two nodes, but
        in different directions, are depicted as a bi-directional
        edge.

        Typically an edge represents some relation between nodes.

        .. relevance::

            In memory management, edges normally represent the fact
            that an :term:`object` holds a :term:`reference` to
            another object.

        .. seealso:: :term:`graph`.

    entry table (1)

        An entry table is a table of :term:`references <reference>`
        into a set of :term:`objects <object>` used to indirect
        references from the outside.

        The Lieberman-Hewitt :term:`collector (1)` represented
        references from older :term:`generations <generation>` to
        younger ones by indirect pointers through an entry table in
        the younger generation that contained the actual
        :term:`address` of the young object. This is fairly expensive
        without special hardware; other :term:`generational
        <generational garbage collection>` collectors generally use
        :term:`remembered sets <remembered set>`.

        .. seealso:: :term:`generational garbage collection`, :term:`exit table`.

        .. bibref:: [LH83]_.

    entry table (2)

        An entry table is an implementation of a :term:`remembered
        set`, where, for a given :term:`generation`, there is a list
        of :term:`objects <object>` in older generations which contain
        :term:`references <reference>` into that generation.

        One could also store the actual :term:`locations <memory
        location>` of the references, which would save time when
        :term:`scanning <scan>`, but incur other costs.

        .. similar:: :term:`remembered set`.

        .. seealso:: :term:`generational garbage collection`, :term:`exit table`.

    exact garbage collection

        .. aka:: *precise garbage collection*, *type-accurate garbage collection*.

        :term:`Garbage collection` is exact (or precise) if it deals
        only with :term:`exact references <exact reference>`.

        An exact :term:`collector (1)` needs to know the
        :term:`format` of the :term:`objects <object>` and the
        :term:`roots <root>`, so that it can tell which fields are
        references.

        .. opposite:: :term:`conservative garbage collection`.

    exact reference

        .. aka:: *precise reference*, *sure reference*.

        An exact or precise or sure reference is a value the
        :term:`collector (1)` knows is a :term:`reference`.

        This is the usual sort of reference. The term is used to draw
        a contrast with :term:`ambiguous reference`.

        .. opposite:: :term:`ambiguous reference`.

    exact root

        .. aka:: *precise root*.

        An exact or precise root is a :term:`root` that contains only
        :term:`exact references <exact reference>`.

        .. opposite:: :term:`ambiguous root`.

        .. seealso:: :term:`exact reference`.

        .. mps:specific::

            An exact root has :term:`rank` :c:func:`mps_rank_exact`.

    exact segregated fit

        A :term:`segregated fit` :term:`allocation mechanism` which
        has a separate :term:`free list` for each possible block size.
        The array of free lists may be represented sparsely. Large
        blocks may be treated separately.

        .. seealso:: :term:`segregated fit`, :term:`segregated free list`, :term:`allocation mechanism`.

        .. bibref:: [WIL95]_.

    execution stack

        .. see:: :term:`control stack`.

    exit table

        An exit table is a table of all :term:`references <reference>`
        from a set of :term:`objects <object>` to objects outside the
        set.

        .. seealso:: :term:`entry table (1)`, :term:`entry table (2)`.

        .. bibref:: [LH83]_.

    extent

        .. see:: :term:`lifetime`.

    external fragmentation

        External :term:`fragmentation` is the inability to use
        :term:`memory (1)` because :term:`free (3)` memory is divided
        into many small :term:`blocks <block>`.

        If :term:`live` :term:`objects <object>` are scattered, the
        free blocks cannot be :term:`coalesced <coalesce>`, and hence
        no large blocks can be :term:`allocated <allocate>`.

        Common solutions to external fragmentation include:

        1. :term:`Moving garbage collection <moving garbage collector>`;

        2. :term:`Handles <handle>`;

        3. Making all your objects the same size.

        .. seealso:: :term:`internal fragmentation`.

        .. bibref:: [JW98]_.


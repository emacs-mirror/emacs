.. _glossary-u:

=============================
Memory Management Glossary: U
=============================

.. include:: alphabet.txt

.. glossary::

    unaligned

        .. aka:: *misaligned*.

        An :term:`address` is unaligned or misaligned if it does not
        comply with some :term:`alignment` constraint on it.

        For example, typically double precision floating point numbers
        occupy 8 :term:`byte (1)` and have an alignment of 4 bytes;
        that is, their address must be a multiple of four. If a
        program tries to access such a number using an address that is
        not a multiple of four, a :term:`bus error` may result,
        depending on the processor architecture and instruction used.

        .. opposite:: :term:`aligned <alignment>`.

        .. seealso:: :term:`alignment`, :term:`bus error`.

    unboxed

        Unboxed :term:`objects` are represented by an
        encoding of the data itself, and not by a :term:`pointer` to
        that data.

        Representations are typically chosen so that unboxed values
        are the same size as the pointer part of a :term:`boxed`
        object. Sometimes the value is :term:`tagged <tag>` to
        distinguish it from a boxed object. The entire object is
        duplicated when the object is passed around, so updates to it,
        if allowed, only affect one copy.

        .. similar:: :term:`immediate data`.

        .. opposite:: :term:`boxed`.

        .. bibref:: :ref:`Gudeman (1993) <GUDEMAN93>`.

    unclamped state

        .. mps:specific::

            One of the four states an :term:`arena` can be in (the
            others being the :term:`clamped state`, the :term:`parked
            state` and the :term:`postmortem state`). In the unclamped
            state, object motion and other background activity may
            occur. Call :c:func:`mps_arena_release` to put an arena
            into the unclamped state.

    undead

        An undead object is an :term:`object` that cannot be proven to
        be :term:`dead` by the :term:`garbage collector`, but whose
        :term:`liveness <live>` is dubious.

        For example, an :term:`ambiguous reference` to an object on a
        :term:`page` may mark the entire page as :term:`reachable`. No
        further data is collected about that page. The other objects
        on the page will survive, even though their reachability has
        not been determined. They are *undead*.

    unmapped

        .. aka:: *free*.

        A range of :term:`virtual addresses` is said
        to be *unmapped* (*free* on Windows) if there is no
        :term:`physical memory (2)` associated with the range.

        An unmapped range may or may not be :term:`reserved`.

        .. opposite:: :term:`mapped`.

    unprotected

        A region of :term:`memory (2)` is said to be unprotected if
        there are no :term:`barriers (1)` on that region.

        .. opposite:: :term:`protected`

    unreachable

        An :term:`object` is unreachable if there is no
        :term:`reference` chain to it from any :term:`root`.

        An object will become unreachable when the :term:`mutator`
        overwrites its last (direct or indirect) reference to the
        object.

        .. similar:: :term:`dead`.

        .. opposite:: :term:`reachable`, :term:`live`.

        .. seealso:: :term:`reachable`, :term:`garbage collection`.

    unsure reference

        .. see:: :term:`ambiguous reference`.

    unwrapped

        .. aka:: *raw*.

        A value is *unwrapped* or *raw* if it is not encoded with type
        information.

        In a dynamically-typed language, the compiler may sometimes be
        able to pick a more compact or efficient representation for a
        value if it can prove that the type can be determined at
        compile-time. This is a particularly useful optimization for
        numeric values such as integers or floats.

        .. opposite:: :term:`wrapped`.

        .. seealso:: :term:`boxed`, :term:`tag`, :term:`value object`.

        .. bibref:: :ref:`Gudeman (1993) <GUDEMAN93>`.

    use after free

        .. see:: :term:`premature free`.


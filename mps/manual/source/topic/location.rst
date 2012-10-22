.. _topic-location:

Location dependency
===================

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/ld/index.html


Interface
---------

.. c:function:: void mps_ld_add(mps_ld_t ld, mps_arena_t arena, mps_addr_t addr)

    Add a dependency on a :term:`block` to a :term:`location
    dependency`.

    ``ld`` is a location dependency.

    ``arena`` is the :term:`arena` to which ``addr`` belongs.

    ``addr`` is the address of the block.

    After calling :c:func:`mps_ld_add`, and until ``ld`` is passed to
    :c:func:`mps_ld_reset`, the call ::

        mps_ld_isstale(ld, arena, addr)

    will return true if the block has moved.

    .. note::

        It is an error to call :c:func:`mps_ld_add` on the same
        location dependency with addresses from two different arenas.
        If you need to test for staleness against multiple arenas,
        then you need at least one location dependency for each arena.

        :c:func:`mps_ld_add` is not thread-safe with respect to
        :c:func:`mps_ld_add`, :c:func:`mps_ld_merge`, or
        :c:func:`mps_ld_reset` on the same location dependency, but it
        is thread-safe with respect to :c:func:`mps_ld_isstale`
        operations. This means that calls to :c:func:`mps_ld_add` from
        different :term:`threads <thread>` must interlock if they are
        using the same location dependency. The practical upshot of
        this is that there should be a lock associated with each
        location dependency.


.. c:function:: mps_bool_t mps_ld_isstale(mps_ld_t ld, mps_arena_t arena, mps_addr_t addr)

    Determine if any of the depdencies in a :term:`location
    dependency` are stale with respect to an :term:`arena`.

    ``ld`` is the location dependency.

    ``arena`` is the arena to test for staleness against. It must be
    the same arena that was passed to all calls to
    :c:func:`mps_ld_add` on ``ld``.

    ``addr`` is an address that may appear in :term:`telemetry
    <telemetry stream>` events related to this call (it will *not* be
    tested for staleness).

    The location dependency is examined to determine whether any of
    the dependencies encapsulated in it have been made stale with
    respect to ``arena``. If any of the dependencies encapsulated in
    the location dependency are stale (that is, the blocks whose
    location has been depended on have been moved by ``arena``) then
    :c:func:`mps_ld_isstale` will return true. If there have been no
    calls to :c:func:`mps_ld_add` on ``ld`` since the last call to
    :c:func:`mps_ld_reset`, then :c:func:`mps_ld_isstale` will return
    false. :c:func:`mps_ld_isstale` may return any value in other
    circumstances (but will strive to return false if the objects
    encapsulated in the location dependency have not moved).

    .. note::

        :c:func:`mps_ld_isstale` may report a false positive
        (returning true despite none of the added addresses having
        being moved by the arena) but never a false negative
        (returning false when an added address has been moved).

        :c:func:`mps_ld_isstale` is thread-safe with respect to itself
        and with respect to :c:func:`mps_ld_add`, but not with respect
        to :c:func:`mps_ld_reset`.


.. c:function:: void mps_ld_merge(mps_ld_t dest_ld, mps_arena_t arena, mps_ld_t src_ld)

    Merge one :term:`location dependency` into another.

    ``dest_ld`` is the destination of the merge.

    ``arena`` is the :term:`arena` .

    ``src_ld`` is the source of the merge.

    The effect of this is to add all the addresses that were added to
    ``src_ld`` to the ``dest_ld``.
    
    :c:func:`mps_ld_merge` has the same thread-safety properties as
    :c:func:`mps_ld_add`.


.. c:function:: void mps_ld_reset(mps_ld_t ld, mps_arena_t arena)

    Reset a :term:`location dependency`.

    ``ld`` is the location dependency.

    ``arena`` is an arena.

    After this call, ``ld`` encapsulates no dependencies. After the
    call to :c:func:`mps_ld_reset` and prior to any call to
    :c:func:`mps_ld_add` on ``ld``, :c:func:`mps_ld_isstale` on ``ld``
    will return false for all arenas.

    :c:func:`mps_ld_reset` is not thread-safe with respect to any
    other location dependency function.


.. c:type:: mps_ld_s

    The type of the structure used to represent a :term:`location
    dependency`. ::

        typedef struct mps_ld_s { 
            mps_word_t w0, w1;
        } mps_ld_s;

    It is an opaque structure type: it is supplied so that the
    :term:`client program` can inline the structure (because its size
    is known), but the client not access it other than through the
    functions :c:func:`mps_ld_add`, :c:func:`mps_ld_isstale`,
    :c:func:`mps_ld_merge`, and :c:func:`mps_ld_reset`.


.. c:type:: mps_ld_t

    The type of :term:`location dependencies <location dependency>`.
    It is a :term:`transparent alias <transparent type>` for a pointer
    to :c:type:`mps_ld_s`.

    A location dependency records the fact that the :term:`client
    program` depends on the bit patterns of some :term:`references
    <reference>` (and not merely on the :term:`block` to which the
    reference refers), and provides a function
    (:c:func:`mps_ld_isstale`) to find out whether any of these
    references have been changed because a block has been
    :term:`moved <moving garbage collector>`.

    A typical use is in the implementation of a hash table whiches
    hashes blocks by hashing their addresses. After a block has moved,
    the hash table needs to be rehashed, otherwise it will not be
    found in the table.



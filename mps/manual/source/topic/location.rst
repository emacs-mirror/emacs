.. sources:

    <https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/ld/index.html>`_


.. index::
   single: location dependency
   single: dependency; location

.. _topic-location:

Location dependency
===================

Location dependencies provide a means by which the :term:`client
program` can depend on the :dfn:`location` of blocks (that is, on the
bits in pointers to the blocks) in the presence of a :term:`moving
memory manager` (where the location of blocks may change and the
client program needs to recognize and correctly deal with such cases).

The interface is intended to support (amongst other things)
address-based hash tables and that will be used as a running example.
See the section :ref:`guide-advanced-location` in the Guide for a more
detailed look at this example.


Terminology
-----------

A :dfn:`location dependency` is represented by an structure of type
:c:type:`mps_ld_s`. It encapsulates a set of dependencies on the
locations of blocks. It can be used to determine whether any of the
blocks have been moved by the memory manager.

To :dfn:`depend` on the location of a block is to perform a computation
whose result depends on the particular representation (that is, the
"bit-pattern") of a reference to the block. This includes any sort of
hash operation on a pointer to the block (such as treating the
pointer as an integer and taking it modulo 257). It is possible to
depend on the location of more than one block.

A dependency has been made :dfn:`stale` if the block whose location was
depended on might have moved since the dependency was made. If this is
the case, then computations that depend on the location of a block
may give different results. A location dependency has been made stale
if any of the blocks whose location has been depended on might have
moved since the respective dependency was made.


.. index::
   single: location dependency; creating

Creating dependencies
---------------------

The :term:`client program` must provide space for the
:c:type:`mps_ld_s` structure. Typically, this will be inlined in some
larger structure. This structure can be in memory managed by the MPS
or elsewhere; that doesn't matter.

For example, the toy Scheme interpreter inlines the location
dependency in its hash table structure:

.. code-block:: c
    :emphasize-lines: 5

    typedef struct table_s {
      type_t type;                  /* TYPE_TABLE */
      hash_t hash;                  /* hash function */
      cmp_t cmp;                    /* comparison function */
      mps_ld_s ld;                  /* location dependency */
      obj_t buckets;                /* hash buckets */
    } table_s;

Before the first use, the location dependency must be reset by calling
the function :c:func:`mps_ld_reset`.

.. note::

    It is not possible to statically create a location dependency that
    has been reset.

You can call :c:func:`mps_ld_reset` at any later point to clear all
dependencies from the structure. For example, this is normally done
whenever :c:func:`mps_ld_isstale` returns true.


.. index::
   single: location dependency; adding

Adding dependencies
-------------------

*Before* the location of a block is depended on (for example,
hashed) a reference to the block may be added to a location
dependency by calling :c:func:`mps_ld_add`. Dependencies on many
blocks can be added to the same location dependency.

It is also possible to merge two location dependencies by calling
:c:func:`mps_ld_merge`, which has the same effect as adding all of the
references from one dependency to another.

For example, in an address-based hash table implementation, each key
that is added to the table must be added to the dependency before its
address is hashed. In the toy Scheme interpreter this is most easily
done in the function that hashes an address:

.. code-block:: c
    :emphasize-lines: 4

    static unsigned long eq_hash(obj_t obj, mps_ld_t ld)
    {
        union {char s[sizeof(obj_t)]; obj_t addr;} u;
        if (ld) mps_ld_add(ld, arena, obj);
        u.addr = obj;
        return hash(u.s, sizeof(obj_t));
    }


.. index::
   single: location dependency; testing staleness
   single: staleness; testing

Testing dependencies for staleness
----------------------------------

When the location of a block is used, first carry out the computation
in the normal way. For example, when looking up a key in an
address-based hash table, start by hashing the pointer and looking up
the corresponding index in the table.

If this succeeds (for example, the key was found in the table at the
place indicated by the hash of its address), then no further test is
required: the operation can proceed as usual.

But if the operation fails, you might be in one of two cases:

#. the location of the block has not been depended on before (for
   example, the key has never been added to the hash table);

#. the location of the block has been depended on before (for example,
   the key was added to the hash table), but the block has moved and
   the dependency has become stale.

At this point you should call :c:func:`mps_ld_isstale`. If it returns
false, then you know that the block has not moved, so you must be in case
(1).

But if :c:func:`mps_ld_isstale` returns true, you could still be in
either case (1) or case (2). All :c:func:`mps_ld_isstale` tells you is
that the block *might* have moved, not whether the block *has* moved.
At this point you must:

#. reset the location dependency;

#. repeat the computation in some way that doesn't depend on the old
   locations of all the blocks that were added to that dependency; and

#. re-add a dependency on each block.

For example, in the case of a hash table you should rehash based on
the new locations of the blocks.

In the toy Scheme interpreter this behaviour is encapsulated into ``table_find``:

.. code-block:: c
    :emphasize-lines: 7

    static struct bucket_s *table_find(obj_t tbl, obj_t buckets, obj_t key, int add)
    {
        struct bucket_s *b;
        assert(TYPE(tbl) == TYPE_TABLE);
        b = buckets_find(tbl, tbl->table.buckets, key, add);
        if ((b == NULL || b->key == NULL || b->key == obj_deleted)
            && mps_ld_isstale(&tbl->table.ld, arena, key))
        {
            b = table_rehash(tbl, tbl->table.buckets->buckets.length, key);
        }
        return b;
    }

After :c:func:`mps_ld_isstale` has returned true, and you've rehashed
the table, it might be tempting to repeat the usual address-based
lookup. But the MPS does not guarantee that :c:func:`mps_ld_isstale`
will not return true again: if the re-hashing took a long time or
touched lots of memory, there might have been another garbage
collection. (The only time that :c:func:`mps_ld_isstale` guarantees to
return false is immediately after :c:func:`mps_ld_reset`.)

You might put in a loop here, but for reliability it is better to fall
back to a non-address-based version of the computation: here, since
``table_rehash`` has to loop over all the entries in the table anyway,
it might as well find the bucket containing ``key`` at the same time
and return it.

.. warning::

    Don't forget to check for staleness when setting a key in a table.
    If the key is stale then it would be a mistake to add it to the
    table as then the key will be present twice, at the positions
    given by the hash of its old and new addresses, thus violating the
    invariant of the hash table (that a key appears at most once).

    Similarly, staleness must be tested when deleting a key from a
    table.


.. index::
   pair: location dependency; thread safety

Thread safety
-------------

The functions are all thread-safe with respect to operations on
different location dependencies. That means that it is not necessary
for threads to interlock if they are performing operations on
different location dependencies. The descriptions of the individual
functions detail their thread-safety attributes if multiple threads
need to access the same location dependency.


.. index::
   single: location dependency; interface

Location dependency interface
-----------------------------

.. c:type:: mps_ld_t

    The type of :term:`location dependencies`. It is a
    :term:`transparent alias <transparent type>` for a pointer to
    :c:type:`mps_ld_s`.

    A location dependency records the fact that the :term:`client
    program` depends on the bit patterns of some :term:`references`
    (and not merely on the identity of the :term:`block` to which the
    reference refers), and provides a function
    (:c:func:`mps_ld_isstale`) to find out whether any of these
    references have been changed because a block has been :term:`moved
    <moving garbage collector>`.

    A typical use is in the implementation of a hash table which
    hashes blocks by hashing their addresses. After a block has moved,
    the table needs to be rehashed, otherwise it will not be
    found in the table.


.. c:type:: mps_ld_s

    The type of the structure used to represent a :term:`location
    dependency`. ::

        typedef struct mps_ld_s { 
            mps_word_t w0, w1;
        } mps_ld_s;

    It is an opaque structure type: it is supplied so that the
    :term:`client program` can inline the structure (because its size
    is known), but the client must not access it other than via the
    functions :c:func:`mps_ld_add`, :c:func:`mps_ld_isstale`,
    :c:func:`mps_ld_merge`, and :c:func:`mps_ld_reset`.


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
        different :term:`threads` must interlock if they are
        using the same location dependency. The practical upshot of
        this is that there should be a lock associated with each
        location dependency.


.. c:function:: mps_bool_t mps_ld_isstale(mps_ld_t ld, mps_arena_t arena, mps_addr_t addr)

    Determine if a dependency on the location of a block in a
    :term:`location dependency` might be stale with respect to an
    :term:`arena`.

    ``ld`` is the location dependency.

    ``arena`` is the arena to test for staleness against. It must be
    the same arena that was passed to all calls to
    :c:func:`mps_ld_add` on ``ld``.

    ``addr`` is the address of the block that is to be tested for
    staleness.

    If there have been no calls to :c:func:`mps_ld_add` on ``ld``
    since the last call to :c:func:`mps_ld_reset`, then return false.

    If the block at ``addr`` was formerly added to the location
    dependency ``ld`` and subsequently moved by ``arena``, then return
    true.

    Otherwise, :c:func:`mps_ld_isstale` may return either true or
    false. (The function strives to return true in the case where
    ``addr`` was added to the location dependency and subsquently
    moved, and false otherwise, but cannot ensure this.)

    .. note::

        :c:func:`mps_ld_isstale` may report a false positive: it may
        return true in the case where ``addr`` was not added to the
        location dependency, or in the case where it was added but not
        moved. It never reports a false negative.

        :c:func:`mps_ld_isstale` is thread-safe with respect to itself
        and with respect to :c:func:`mps_ld_add`, but not with respect
        to :c:func:`mps_ld_reset`.


.. c:function:: mps_bool_t mps_ld_isstale_any(mps_ld_t ld, mps_arena_t arena)

    Determine if a dependency on the location of *any* block in a
    :term:`location dependency` might be stale with respect to an
    :term:`arena`.

    ``ld`` is the location dependency.

    ``arena`` is the arena to test for staleness against. It must be
    the same arena that was passed to all calls to
    :c:func:`mps_ld_add` on ``ld``.

    If there have been no calls to :c:func:`mps_ld_add` on ``ld``
    since the last call to :c:func:`mps_ld_reset`, then return false.

    If any block added to the location dependency ``ld`` has been
    moved by ``arena``, then return true.

    Otherwise, :c:func:`mps_ld_isstale_any` may return either true or
    false. (The function strives to return true in the case where a
    block was added to the location dependency and subsquently moved,
    and false otherwise, but cannot ensure this.)

    .. note::

        :c:func:`mps_ld_isstale_any` has the same thread-safety
        properties as :c:func:`mps_ld_isstale`.


.. c:function:: void mps_ld_merge(mps_ld_t dest_ld, mps_arena_t arena, mps_ld_t src_ld)

    Merge one :term:`location dependency` into another.

    ``dest_ld`` is the destination of the merge.

    ``arena`` is the :term:`arena` .

    ``src_ld`` is the source of the merge.

    The effect of this is to add all the addresses that were added to
    ``src_ld`` to the ``dest_ld``.

    .. note::

        :c:func:`mps_ld_merge` has the same thread-safety properties
        as :c:func:`mps_ld_add`.


.. c:function:: void mps_ld_reset(mps_ld_t ld, mps_arena_t arena)

    Reset a :term:`location dependency`.

    ``ld`` is the location dependency.

    ``arena`` is an arena.

    After this call, ``ld`` encapsulates no dependencies. After the
    call to :c:func:`mps_ld_reset` and prior to any call to
    :c:func:`mps_ld_add` on ``ld``, :c:func:`mps_ld_isstale` on ``ld``
    will return false for all arenas.

    .. note::

        :c:func:`mps_ld_reset` is not thread-safe with respect to any
        other location dependency function.

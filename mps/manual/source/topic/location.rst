.. sources:

    <https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/ld/index.html>`_


.. _topic-location:

Location dependency
===================

Location dependencies provide a means by which the :term:`client
program` can depend on the :dfn:`location` of blocks (that is, on the
representation of pointers to blocks) in the presence of a
:term:`moving memory manager` (where the location of blocks may change
and the client program needs to recognize and correctly deal with such
cases).

The interface is intended to support (amongst other things)
address-based hash tables and that will be used as a running example.


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


Example: ``eq?`` hash table
---------------------------

The toy Scheme interpreter contains a simple address-based (``eq?``)
hash table implementation. It hashes the addresses of its keys, and so
depends on their location.

Without taking account of this location dependency, the hash tables
become invalid after a garbage collection. In the interaction shown
below (with the naïve version of the code) you'll see that although
the keys remain present in the table after garbage collection, they
cannot be found. This is because their locations (and hence their
hashes) have changed, but their positions in the table have not been
updated to match.

.. code-block:: none

    MPS Toy Scheme Example
    10232, 0> (define ht (make-hash-table))
    ht
    10560, 0> (hash-table-set! ht 'one 1)
    10744, 0> (hash-table-set! ht 'two 2)
    10928, 0> (hash-table-set! ht 'three 3)
    11112, 0> ht
    #[hash-table (two 2) (three 3) (one 1)]
    11112, 0> (hash-table-ref ht 'two)
    2
    11232, 0> (gc)
    11256, 1> (hash-table-ref ht 'two)
    11376, 1> (hash-table-ref ht 'one)
    11496, 1> (hash-table-ref ht 'three)
    11616, 1> ht
    #[hash-table (two 2) (three 3) (one 1)]


Creating dependencies
---------------------

The :term:`client program` must provide space for the
:c:type:`mps_ld_s` structure. Typically, this will be inlined in some
larger structure (for example, a hash table might have a location
dependency structure inlined at the "beginning" of it). This structure
can be in memory managed by the MPS or elsewhere; that doesn't matter.

For example:

.. code-block:: c
    :emphasize-lines: 3

    typedef struct table_s {
      type_t type;                  /* TYPE_TABLE */
      mps_ld_s ld;                  /* location dependency */
      obj_t buckets;                /* hash buckets */
    } table_s;

Before the first use, the location dependency must be reset by calling
function :c:func:`mps_ld_reset`.

.. note::

    This means that it is not possible to statically create a location
    dependency that has been reset.

For example:

.. code-block:: c
    :emphasize-lines: 15

    static obj_t make_table(void)
    {
        obj_t obj;
        mps_addr_t addr;
        size_t size = ALIGN(sizeof(table_s));
        do {
            mps_res_t res = mps_reserve(&addr, obj_ap, size);
            if (res != MPS_RES_OK) error("out of memory in make_table");
            obj = addr;
            obj->table.type = TYPE_TABLE;
            obj->table.buckets = NULL;
        } while (!mps_commit(obj_ap, addr, size));
        total += size;
        obj->table.buckets = make_buckets(8);
        mps_ld_reset(&obj->table.ld, arena);
        return obj;
    }

You can call :c:func:`mps_ld_reset` at any later point to clear all
dependencies from the structure. For example, this is normally done
whenever :c:func:`mps_ld_isstale` returns true.


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
address is hashed. In the Scheme example, addresses are hashed during
the call to the function ``buckets_find``, so the key must be added to
the location dependency before that:

.. code-block:: c
    :emphasize-lines: 4

    static int table_try_set(obj_t tbl, obj_t key, obj_t value)
    {
        struct bucket_s *b;
        mps_ld_add(&tbl->table.ld, arena, key);
        b = buckets_find(tbl->table.buckets, key);
        if (b == NULL)
            return 0;
        if (b->key == NULL)
            b->key = key;
        b->value = value;
        return 1;
    }

    static void table_set(obj_t tbl, obj_t key, obj_t value)
    {
        if (!table_try_set(tbl, key, value)) {
            int res;
            table_rehash(tbl, tbl->table.buckets->buckets.length * 2, NULL);
            res = table_try_set(tbl, key, value);
            assert(res);            /* rehash should have made room */
        }
    }

.. note::

    The garbage collector may run at any time during this operation,
    so the table may already be stale while the new key and value are
    being added. We postpone worrying about this until the next
    lookup, when the staleness will be discovered.


Testing dependencies for staleness
----------------------------------

When the locations of blocks are used (during a hash table lookup for
example), the computation should be carried out and the result used in
the usual way (for example, the pointer is hashed and the has used to
index into the table). At this point one of three situations can
occur:

1. success (for example, the key was found in the hash table at the
   place indicated by the hash of its address);

2. failure: the location of these blocks has not been depended on
   before (for example, the key has never been added to the hash
   table);

3. failure: the location of these blocks has been depended on before,
   but the one or more of the blocks has moved and the dependency has
   been made stale (in this case the table would need to be rehashed
   and the lookup repeated).

Success requires no further test: the operation can proceed. In case
of failure, you should call :c:func:`mps_ld_isstale`. If it returns
false, then no blocks have moved, so you must be in case (2).

But if :c:func:`mps_ld_isstale` returns true, you could still be in
either case (2) or case (3). All :c:func:`mps_ld_isstale` tells you is
that some blocks that have been depended on might have moved. At this
point you need to:

1. reset the location dependency;

2. repeat the computation in some way that doesn't depend on the
   old locations of the blocks; and

3. re-add a dependency on each block.

For example, in the case of a hash table you should rehash based on
the new locations of the blocks:

.. code-block:: c
    :emphasize-lines: 13, 19, 37

    /* Rehash 'tbl' so that it has 'new_length' buckets. If 'key' is found
     * during this process, return the bucket containing 'key', otherwise
     * return NULL.
     */
    static struct bucket_s *table_rehash(obj_t tbl, size_t new_length, obj_t key)
    {
        size_t i;
        obj_t new_buckets;
        struct bucket_s *key_bucket = NULL;

        assert(tbl->type.type == TYPE_TABLE);
        new_buckets = make_buckets(new_length);
        mps_ld_reset(&tbl->table.ld, arena);

        for (i = 0; i < tbl->table.buckets->buckets.length; ++i) {
            struct bucket_s *old_b = &tbl->table.buckets->buckets.bucket[i];
            if (old_b->key != NULL) {
                struct bucket_s *b;
                mps_ld_add(&tbl->table.ld, arena, old_b->key);
                b = buckets_find(new_buckets, old_b->key);
                assert(b != NULL);      /* new table shouldn't be full */
                assert(b->key == NULL); /* shouldn't be in new table */
                *b = *old_b;
                if (b->key == key) key_bucket = b;
            }
        }

        tbl->table.buckets = new_buckets;
        return key_bucket;
    }

    static obj_t table_ref(obj_t tbl, obj_t key)
    {
        struct bucket_s *b = buckets_find(tbl->table.buckets, key);
        if (b && b->key != NULL)
            return b->value;
        if (mps_ld_isstale(&tbl->table.ld, arena, key)) {
            b = table_rehash(tbl, tbl->table.buckets->buckets.length, key);
            if (b) return b->value;
        }
        return NULL;
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
``table_rehash`` has to loop over all the entries in the hash table
anyway, it might as well find the bucket containing ``key`` at the
same time and return it.

By adding the line::

    puts("Stale!");

after :c:func:`mps_ld_isstale` returns true, we get to see when the
location dependency becomes stale and the table has to be rehashed.

.. code-block:: none
    :emphasize-lines: 21, 23

    MPS Toy Scheme Example
    10232, 0> (define ht (make-hash-table))
    ht
    10576, 0> (hash-table-set! ht 'one 1)
    10760, 0> ht
    #[hash-table (one 1)]
    10760, 0> (gc)
    10784, 1> (hash-table-ref ht 'one)
    Stale!
    1
    11048, 1> (hash-table-set! ht 'two 2)
    11232, 1> (gc)
    11256, 2> (hash-table-ref ht 'one)
    Stale!
    1
    11520, 2> (hash-table-set! ht 'three 3)
    11704, 2> (hash-table-ref ht 'two)
    2
    11824, 2> (gc)
    11848, 3> (hash-table-ref ht 'one)
    1
    11968, 3> (hash-table-ref ht 'two)
    Stale!
    2
    12232, 3> (hash-table-ref ht 'three)
    3

.. note::

    You might be puzzled by the highlighted lines: the table wasn't
    stale when ``'one`` was looked up, even though objects did move
    during the garbage collection cycle, as shown by the table being
    found to be stale when ``'two`` is looked up. This is the magic of
    :term:`incremental garbage collection`!


Performance
-----------

:c:func:`mps_ld_add` are :c:func:`mps_ld_isstale` are intended to be
fast operations. :c:func:`mps_ld_reset` is inexpensive, but not
intended to be very fast. To be more precise, the following are what
one could expect from a good implementation:

:c:func:`mps_ld_add` performs one read memory-cycle and one write
memory-cycle to the same location; it performs a few ALU operations.

:c:func:`mps_ld_merge` performs a few ALU operations, four reads, and
two writes to the destination location dependency structure.

:c:func:`mps_ld_isstale` performs up to four read memory-cycles; it
performs a few ALU operations.

:c:func:`mps_ld_reset` claims a lock.

.. note::

    In each case there is function call overhead as well, but this
    could in theory be avoided by providing macro implementations. If
    you have an application that needs this, please :ref:`contact us
    <contact>`.


Thread safety
-------------

The functions are all thread-safe with respect to operations on
different location dependencies. That means that it is not necessary
for threads to interlock if they are performing operations on
different location dependencies. The descriptions of the individual
functions detail their thread-safety attributes if multiple threads
need to access the same location dependency.


Location dependency interface
-----------------------------

.. c:type:: mps_ld_t

    The type of :term:`location dependencies <location dependency>`.
    It is a :term:`transparent alias <transparent type>` for a pointer
    to :c:type:`mps_ld_s`.

    A location dependency records the fact that the :term:`client
    program` depends on the bit patterns of some :term:`references
    <reference>` (and not merely on the identity of the :term:`block`
    to which the reference refers), and provides a function
    (:c:func:`mps_ld_isstale`) to find out whether any of these
    references have been changed because a block has been :term:`moved
    <moving garbage collector>`.

    A typical use is in the implementation of a hash table whiches
    hashes blocks by hashing their addresses. After a block has moved,
    the hash table needs to be rehashed, otherwise it will not be
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
    circumstances (but will strive to return false if the blocks
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

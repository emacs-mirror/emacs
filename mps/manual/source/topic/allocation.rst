.. _topic-allocation:

Allocation
==========

See design/buffer

See https://github.com/dylan-lang/opendylan/issues/235

This example seems to be wrong (no function "mps_pool_alloc" in the public interface).

::

    {
        mps_addr_t new_block;
        mps_res_t res;
        thingy *tp;

        res = mps_pool_alloc(&new_block, pool, sizeof(thingy));
        if (res != MPS_RES_OK) return res;
        tp = new_block;

        /* ... */
    }


Some :term:`pools <pool>` and allocation protocols accept an :term:`alignment` as an option. This
ensures that objects in the pool or objects allocated observe a
stricter alignment than that of the :term:`object format`.

::

    mps_res_t res;
    mps_addr_t p;

    res = mps_alloc(&p, pool, size);
    if (res != MPS_RES_OK) {
        /* p hasn't been touched in this case. */
        handle error;
    }

    /* p now contains the result, which is the address of the new block */
    /* in this case. */




This wiki article contains incomplete and informal notes about the MPS, the precursor to more formal documentation. Not confidential. Readership: MPS users and developers.

These notes on Allocation Points were written by RHSK between 2006-06-07 and 2006-06-22, following research and discussion with RB and NB. Warning: the text in this User's Guide is preliminary, but believed to be 'conservatively correct'. In other words, I think if you follow these guidelines, your code will be correct, and will not violate the current or future definitions of the MPS ap protocol. But this is not (yet) an accurate statement of the MPS ap protocol. RHSK 2006-06-13.

[Note: some constraints may be mentioned only here, and not yet in other places they should be mentioned, such as the Reference Manual. Notably, the client's obligation to ensure there are no exact references to a failed new-object, before it calls mps_ap_trip, is suspected to be new. RHSK 2006-06-13]


Introduction
------------

Allocation points are an MPS protocol that the client uses to allocate memory with low overhead, and low synchronization cost (with an asynchronous collector and with other threads).

The allocation point protocol is designed to work with incremental collections and multi-threaded clients. (And even if your particular client is single-threaded and non-incremental, for the purposes of using allocation points it's easiest to assume you are coding for this case).

The assumption is that, between any two client instructions, the MPS can interrupt you, move objects around (if they are in a moving pool) and collect garbage. To cope with this, allocating is a two-step process.

**Important:** .assume.ambig-workspace: this User's Guide assumes that you have declared your stack and registers to be a root that is ambiguously scanned, using mps_root_create_reg and passing the mps_stack_scan_ambig function to it. This is the simplest way to write a client. Other scenarios are possible, but their implications for correct Allocation Point use are not yet documented here.

The rest of this Allocation Points User's Guide contains the following sections:

* Creating and destroying allocation points
* Overview of two-step allocation
* The graph of managed references
* mps_reserve
* Building the object
* mps_commit
* Common mistakes


Creating and destroying allocation points
-----------------------------------------

To create an allocation point in a pool, call mps_ap_create. This may require additional arguments, depending on the pool class. See pool class documentation.

An allocation point MUST NOT be used by more than one thread. (Each thread must have its own allocation point or points).

Destroy an allocation point with mps_ap_destroy.

Overview of two-step allocation
-------------------------------

When the client is building (creating and formatting) a new object, you can think of it as being 'in a race' with the MPS. The object is 'under construction', and the MPS cannot manage it in the normal way. So the client should build the object quickly, and then commit it to MPS management. Rarely, the MPS has to move other objects around right in the middle of this build phase: that's a (small) price you pay for having an asynchronous collector. If this happens, the MPS will tell the client that it has 'lost the race'. Objects have moved around, and the new object is invalid. The client must start building it again from scratch.

The client starts building the new object with mps_reserve, and marks it complete by calling mps_commit. Almost always, mps_commit succeeds. But if the client did not complete the object in time, then mps_commit fails (returns 0).

This is how the client should build a new object:

1. mps_reserve some memory,

2. build a new object in it,

3. store a reference to the new object in an ambiguously-scanned place (but NOT in any exactly-scanned place),

4. mps_commit the new object to MPS management.

If commit succeeds, the object is complete, and immediately becomes just a normal allocated object. The client may write a reference to the new object into some older object (thereby connecting the new object into the client's graph of objects).

If commit fails, the new object no longer exists: the data has gone and any references that used to refer to it are now dangling pointers. The client should simply try to build the object again.

In pseudo-code, the standard allocation point idiom is::

    do
        mps_reserve
        initialize new object
        make an ambiguous reference to new object
    while (! mps_commit)
    link new object into my object graph

(Do not worry about getting stuck in this loop: commit usually fails at most once per collection, so it is very rare for commit to fail even once, let alone twice).

In C, this typically looks like this::

    int make_object(mps_ap_t ap, object *parent)
    {
      void *p;
      object *neo = NULL;

      do {
        if (mps_reserve(&p, ap, SIZE_OBJECT) != MPS_RES_OK) {
          goto fail_make_object;
        }
        /* Build the new object */
        neo = p;
        neo->formatcode = FORMAT_CLIENT;  /* (not fwd or pad) */
        neo->type = TYPE_OBJECT;
        neo->size = SIZE_OBJECT;
        neo->parent = parent;
        neo->tribe = parent->tribe;
        neo->child = NULL;
        /* neo (ambiguous reference) preserves the new object */
      } while (! mps_commit(ap, p, SIZE_OBJECT));

      /* Success: link the new object into my object graph */
      parent->child = neo;
      return TRUE;

    fail_make_object:
      return FALSE;  /* out of memory, etc */
    }

Note that, throughout this User's Guide, we assume that the stack and registers are declared as ambiguous roots (.assume.ambig-workspace) which means that the neo pointer keeps the new object alive for us.

The rest of this User's Guide goes through these steps in more detail.

The graph of managed references
-------------------------------

The MPS is a moving garbage collector: it supports preserve-by-copying pools, whose objects are 'mobile'. Whenever the MPS moves an object, it will ensure that all managed references are updated to point to the new location -- and this happens instantaneously as far as the client sees it.

The client should assume that, between any pair of instructions, the MPS may 'shake' this graph, moving all the mobile objects, and updating all the managed references.

Any parts of the graph that are no longer connected (no longer reachable from declared roots) may be collected, and the memory that those objects occupied may be unmapped, or re-used for different objects.

The client usually takes care to ensure that all the references it holds are managed. To be managed, the reference must be in a declared root (such as a scanned stack or a global variable), or in a formatted object that is reachable from a root.

It is okay for a careful client to hold unmanaged references, but:

they'd better not be to a mobile object! Remember, mobile objects could move at any time, and unmanaged references will be left 'dangling'.
they'd better not be the only reference to an object, or that object might get collected, again leaving a dangling reference.

mps_reserve
-----------

Call mps_reserve, passing the size of the new object you wish to create. The size must be aligned to the pool alignment. This is in contrast to mps_alloc, which (for some pools) allows unaligned sizes.

[Normally, use mps_reserve (the lower-case C macro). But if you are using a weak compiler that does not detect common subexpressions, you may find that using MPS_RESERVE_BLOCK (functionally identical) generates faster code. Or it may generate slower code. It depends on your compiler, and you will have to conduct tests to find out.]

mps_reserve returns a reference to a piece of new memory for the client to build a new object in. During this build, the MPS pins the piece of memory, and treats it as raw data.

"Pinned" means: it will not move, be collected, be unmapped, or anything like that. You may keep an unmanaged reference to it at this time.

"Raw data" means two things:

Firstly, "raw data" means that any references stored IN the new object are unmanaged. This means:

* references in the new object will not get updated if the graph of managed references to mobile objects is 'shaken';
* references in the new object do not preserve any old objects they point to.

Secondly, "raw data" means that any references TO the new object are treated like other references to unmanaged memory:

* the MPS will not call the client's format code to answer questions about the new object.

Building the object
-------------------

The client will typically do all these things:

* write data that makes the new object 'valid' for the client's format;
* write other data into the new object;
* store references to existing objects IN the new object;
* keep (in a local variable) an ambiguous reference TO the new object.

However, during the build, there are a couple of restrictions:

* Once the client has stored a reference IN the new object, it MUST NOT read it out again — any reference stored in the new object is unmanaged, and may have become stale.

  (Actually, the restriction is: the moment a reference to an existing mobile object is written into the new object, that reference (in the new object) may become stale. And you'd better not use (dereference) a stale reference. And you'd better not write it into any exactly-scanned cell (such as in an existing object). Reading it into an ambiguously-scanned cell (such as an ambiguously scanned register or stack cell) is okay as long as you don't dereference it. Writing it back into another part of the new object is okay too. Just don't trust it to be a valid reference.)

* The client MUST NOT store a reference TO the new object in any exactly-scanned place.

  [Note: this is in fact possible, but the protocol for doing it is more complex, and beyond the scope of this guide. RHSK 2006-06-22]

  This means the client should NOT connect the new object into the graph of managed objects during the build.

Before the end of the build phase:

* the new object must be validly formatted;
* all exactly-scanned cells in the new object must contain valid references;
* the new object must be ambiguously reachable.

Optionally, for improved robustness to bugs, consider initialising all parts of the new object, including parts that are not yet being used to store useful data (such as a string buffer). You might want to make this compile-time switchable, for debugging.

.. note::

    If you leave these unused parts uninitialised, they may contain data that looks like a valid object -- this is called a "spoof object". (This might be the 'ghost' of a previous object, or just random junk that happens to look like a valid object).

    This is completely legal: spoof objects do not cause a problem for the MPS.

    However, this might leave you with less safety margin than you want, especially when developing a new client. If there were to be a bug in your code (or indeed in the MPS) that resulted in a bogus exact reference to this spoof, it might go undetected, and arbitrary corruption might occur before the bug came to light. So, consider filling these as-yet unused parts with specially chosen dummy values, at least as an option for debugging. Choose dummy values that your format code will recognise as not permitted at the start of a valid formatted object. You will then detect bogus exact references more promptly.

    [RHSK 2006-06-15: In poolamc, these ghosts will be forwarding pointers, and they will usually get unmapped (though unless we use zeroed / secure / etc VM they may get mapped-in again intact). But if the tract is nailed they won't even get unmapped. And ghost forwarding pointers are just as bad news as any other spoof. There's currently no format method "destroy". If there was, we could call it in the reclaim phase, to allow format code to safely mark these ghosts as dead. Actually, perhaps that's a valid use of the 'pad' method? ]


mps_commit
----------

When you call mps_commit, it will either fail or succeed.

Almost always, mps_commit succeeds. If it succeeds, that means:

* all the references written IN the new object are valid (in other words, a successful commit is the MPS's way of telling you that these references did not become stale while they were sitting unmanaged in the new object);
* all the references TO the new object are valid;
* the new object is now just a normal object like any other;
* it may get collected if there are no references to it;
* if the pool supports mps_free, you may manually free the new object.

Occasionally but rarely, mps_commit fails. This means:

* the new object no longer exists — the memory may even be unmapped by the time mps_commit returns;
* there must be no exact references to the new object.

If commit fails, the client usually tries making the object again (although this is not required: it is allowed to just give up!). This is why the standard allocation point idiom has a do...while loop.

Common mistakes
---------------

Here are some examples of mistakes to avoid::

    /* This example below is INCORRECT. */

    typedef struct object_s {
      int              formatcode;  /* FORMAT_CLIENT, _FWD, or _PAD */
      int              type;
      size_t           size;
      struct object_s *tribe;
      struct object_s *parent;
      struct object_s *child;
    } object; 

    int make_object(mps_ap_t ap, object *parent)
    {
      void *p;
      object *neo = NULL;

      do {
        if (mps_reserve(&p, ap, SIZE_OBJECT) != MPS_RES_OK) {
          goto fail_make_object;
        }
        /* Build the new object */
        neo = p;
        neo->formatcode = FORMAT_CLIENT;
        neo->type = TYPE_OBJECT;
        neo->size = SIZE_OBJECT;
        neo->parent = parent;
        neo->tribe = neo->parent->tribe;  /*--- incorrect-1 ---*/
        parent->child = neo;  /*--- incorrect-2 ---*/

        /* neo (ambiguous reference) preserves the new object */
      } while (! mps_commit(ap, p, SIZE_OBJECT));

      neo->child = NULL;  /*--- incorrect-3 ---*/
      return TRUE;

    fail_make_object:
      return FALSE;  /* out of memory, etc */
    }

    /* The example above is INCORRECT. */

Incorrect-1: do not read references from the new object. Dereferencing neo->parent is illegal. (The code should use parent->tribe).

Incorrect-2: making an exact reference to the new object is illegal. (The code should only do this after a successful commit).

Incorrect-3: the child slot (in this example) is exactly scanned, and it MUST be initialised before the call to commit. (The code shown is initialising it too late).

Conclusion and further details
------------------------------

Although this User's Guide explains the protocol in terms of the pre-packaged macros mps_reserve and mps_commit, that is a simplification. The MPS allocation point protocol is designed as a binary protocol, defined at the level of atomic machine operations. The precise specification of the binary protocol is beyond the scope of this document.

For further discussion of Allocation Points, see Allocation Points -- Internals in the Wiki.


Interface
---------

.. c:function:: mps_res_t mps_alloc(mps_addr_t *p_o, mps_pool_t pool, size_t size, ...)

    Allocate a :term:`block` of memory in a :term:`pool`.

    ``p_o`` points to a location that will hold the address of the
    allocated block.

    ``pool`` the pool to allocate in.

    ``size`` is the :term:`size` of the block to allocate. If it is
    unaligned, it will be rounded up to the pool's :term:`alignment`
    (unless the pool documentation says otherwise).

    Some pool classes require additional arguments to be passed to
    :c:func:`mps_alloc`. See the documentation for the pool class.

    .. note::

        There's an alternative function :c:func:`mps_alloc_v` that
        takes its extra arguments using the standard :term:`C`
        ``va_list`` mechanism.


.. c:function:: mps_res_t mps_alloc_v(mps_addr_t *p_o, mps_pool_t pool, size_t size, va_list args)

    An alternative to :c:func:`mps_alloc` that takes its extra
    arguments using the standard :term:`C` ``va_list`` mechanism.


.. c:function:: mps_res_t mps_ap_create(mps_ap_t *ap_o, mps_pool_t pool, ...)

    Create an :term:`allocation point` in a :term:`pool`.

    ``ap_o`` points to a location that will hold the address of the
    allocation point, if successful.

    ``pool`` is the pool.

    Returns :c:macro:`MPS_RES_OK` if successful, or another
    :term:`result code` if not.

    Some pool classes require additional arguments to be passed to
    :c:func:`mps_ap_create`. See the documentation for the pool class.

    .. note::

        There's an alternative function :c:func:`mps_ap_create_v` that
        takes its extra arguments using the standard :term:`C`
        ``va_list`` mechanism.


.. c:function:: mps_res_t mps_ap_create_v(mps_ap_t *ap_o, mps_pool_t pool, va_list args)

    An alternative to :c:func:`mps_ap_create` that takes its extra
    arguments using the standard :term:`C` ``va_list`` mechanism.


.. c:function:: void mps_ap_destroy(mps_ap_t ap)

    Destroy an :term:`allocation point`.

    ``ap`` is the allocation point to destroy.

    Destroying an allocation point has no effect on blocks that were
    allocated from it, so long as they were successfully
    :term:`committed (2)` by :c:func:`mps_commit`.


.. c:function:: mps_res_t mps_ap_fill(mps_addr_t *p_o, mps_ap_t ap, size_t size)

    Reserve a :term:`block` of memory on an :term:`allocation point`.

    :c:func:`mps_ap_fill` has same interface as :c:func:`mps_reserve`.

    .. note::

        :c:func:`mps_ap_fill` must only be called according to the
        :term:`allocation point protocol`.


.. c:type:: mps_ap_s

   The type of the structure used to represent :term:`allocation
   points <allocation point>`::

        typedef struct mps_ap_s {
          mps_addr_t init;
          mps_addr_t alloc;
          mps_addr_t limit;
          /* ... private fields ... */
        } mps_ap_s;

   ``init`` is the limit of initialized memory.

   ``alloc`` is the limit of allocated memory.

   ``limit`` is the limit of available memory.

    An allocation point is an interface to a :term:`pool` which
    provides very fast allocation, and defers the need for
    synchronization in a multi-threaded environment.

    Create an allocation point for a pool by calling
    :c:func:`mps_ap_create`, and allocate memory via one by calling
    :c:func:`mps_reserve` and :c:func:`mps_commit`.


.. c:type:: mps_ap_t

    The type of :term:`allocation points <allocation point>`. It is a
    :term:`transparent alias <transparent type>` for a pointer to
    :c:type:`mps_ap_s`.


.. c:function:: mps_bool_t mps_ap_trip(mps_ap_t ap, mps_addr_t p, size_t size)

    :term:`Commit <committed (2)>` a reserved :term:`block` on an
    :term:`allocation point`.

    :c:func:`mps_ap_trip` has the same interface as :c:func:`mps_commit`.

    .. note::

        :c:func:`mps_ap_trip` must only be called according to the
        :term:`allocation point protocol`.


.. c:function:: mps_bool_t mps_commit(mps_ap_t ap, mps_addr_t p, size_t size)

    :term:`Commit <committed (2)>` a reserved :term:`block` on an
    :term:`allocation point`.

    ``ap`` is an allocation point.

    ``p`` points to a block that was reserved by :c:func:`mps_reserve`
    but has not yet been committed.

    ``size`` is the :term:`size` of the block to allocate. It must be
    the same size that was passed to :c:func:`mps_reserve`.

    If :c:func:`mps_commit` returns true, the block was successfully
    committed, which means that the :term:`client program` may use it,
    create references to it, and rely on references from it. It also
    means that the MPS may scan it, move it, protect it, or reclaim it
    (if ``ap`` was attached to a pool with those features).

    If :c:func:`mps_commit` returns false, the block was not
    committed. This means that the client program must not create
    references to the block, rely on references from it, or otherwise
    use it. It is normal to attempt the reserve operation again when
    this happens.

    It is very rare for :c:func:`mps_commit` to return false: this
    only happens if there was a :term:`flip` between the call to
    :c:func:`mps_reserve` and the call to
    :c:func:`mps_commit`. Nonetheless, it can happen, so it is
    important not to perform operations with side effects (that you
    aren't prepared to repeat) between calling :c:func:`mps_reserve`
    and :c:func:`mps_commit`. Also, the shorter the interval, the less
    likely :c:func:`mps_commit` is to return false.

    .. note::

        :c:func:`mps_commit` must only be called according to the
        :term:`allocation point protocol`.

        :c:func:`mps_commit` is implemented as a macro for speed. It
        may evaluate its arguments multiple times.


.. c:function:: void mps_free(mps_pool_t pool, mps_addr_t addr, size_t size)

    Free a :term:`block` of memory to a :term:`pool`.

    ``pool`` is the pool the block belongs to.

    ``addr`` is the address of the block to be freed.

    ``size`` is the :term:`size` of the block to be freed. If it is
    unaligned, it will be rounded up to the pool's :term:`alignment`
    (unless the pool documentation says otherwise).

    The freed block of memory becomes available for allocation by the
    pool, or the pool might decide to make it available to other
    pools, or it may be returned to the operating system.

    .. note::

        :c:func:`mps_free` takes a ``size`` parameter because it is
        most efficient to do so. In most programs, the type of an
        object is known at the point in the code that frees it, hence
        the size is trivially available. In such programs, storing the
        size on the MPS side would cost time and memory, and make it
        hard to get good virtual memory behaviour (as it is, the
        deallocation code doesn't have to touch the dead object at
        all).


.. c:function:: mps_res_t mps_reserve(mps_addr_t *p_o, mps_ap_t ap, size_t size)

    Reserve a :term:`block` of memory on an :term:`allocation point`.

    ``p_o`` points to a location that will hold the address of the
    reserve block.

    ``ap`` is the allocation point.

    ``size`` is the :term:`size` of the block to allocate. It must be
    a multiple of the :term:`alignment` of the pool (or of the pool's
    :term:`object format` if it has one).

    Returns :c:macro:`MPS_RES_OK` if the block was reserved
    successfully, or another :term:`result code` if not.

    The reserved block may be initialized but must not otherwise be
    used until after it has been :term:`committed (2)` via a
    successful call to :c:func:`mps_commit`.

    .. note::

        :c:func:`mps_reserve` must only be called according to the
        :term:`allocation point protocol`.

        :c:func:`mps_reserve` is implemented as a macro for speed. It
        may evaluate its arguments multiple times.

        There is an alternative, :c:func:`MPS_RESERVE_BLOCK`, which
        may generate faster code, but may only be used in statement
        context (not as an expression), and requires an lvalue instead
        of a pointer to a location to store the result.


.. c:function:: MPS_RESERVE_BLOCK(mps_res_t res_v, mps_addr_t *p_v, mps_ap_t ap, size_t size)

    An alternative to :c:func:`mps_reserve`. It may generate faster
    code than :c:func:`mps_reserve`, but it may only be used in
    statement context (not as an expression), and it requires an
    lvalue instead of a pointer to a location to store the result.

    The second argument is an lvalue ``p_v``, which is assigned the
    address of the reserved block. It takes an additional first
    argument, the lvalue ``res_v``, which is assigned the
    :term:`result code`.



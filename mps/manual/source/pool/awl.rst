.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/manual/wiki/pool_classes.html>`_
    `<https://info.ravenbrook.com/project/mps/master/design/poolawl/>`_
    DRJ: `<https://info.ravenbrook.com/mail/2003/03/17/13-51-24/0.txt>`_
    NB: `<https://info.ravenbrook.com/mail/2002/04/12/15-52-29/0.txt>`_
    NB: `<https://info.ravenbrook.com/mail/2002/04/12/15-56-15/0.txt>`_

.. index::
   single: AWL
   single: pool class; AWL

.. _pool-awl:

AWL (Automatic Weak Linked)
===========================

**AWL** is an :term:`automatically managed <automatic memory
management>` :term:`non-moving <non-moving garbage collector>`
:term:`pool class` that may contain :term:`weak references (1)`.

The purpose of this pool class is to allow the client to implement
:term:`weak-key <weak-key hash table>`, :term:`weak-value <weak-value
hash table>`, and :term:`doubly weak hash tables`.

In a weak-key hash table, the keys are weakly referenced, so their
presence in the table will not prevent the key object from being
garbage collected. Once the key is no longer :term:`reachable`, weak
references to it may get :term:`splatted <splat>` (that is, replaced
with null pointers). Once that has happened, the client program can't
get at the value corresponding to the key any more, so the
implementation is free to splat the value slot as well.

AWL allows the implementation to splat the value slot at the same time
that the weak key slot is splatted. (Or the other way around for
weak-value tables.) See :ref:`pool-awl-dependent`.

See :ref:`guide-advanced-weak` in the :ref:`guide-advanced` section of
the user guide for a detailed example of using this pool class.

.. note::

    AWL is the only pool in the open source MPS that allows its
    formatted objects to contain weak references. It was designed to
    support the weak hash tables in `Open Dylan
    <http://opendylan.org/>`_, and may be awkward to use for other use
    cases. If you need more general handling of weak references,
    :ref:`contact us <contact>`.


.. index::
   single: AWL; properties

AWL properties
--------------

* Does not support allocation via :c:func:`mps_alloc` or deallocation
  via :c:func:`mps_free`.

* Supports allocation via :term:`allocation points`. If an allocation
  point is created in an AWL pool, the call to
  :c:func:`mps_ap_create_k` accepts one keyword argument,
  :c:macro:`MPS_KEY_RANK`.

* Supports :term:`allocation frames` but does not use them to improve
  the efficiency of stack-like allocation.

* Does not support :term:`segregated allocation caches`.

* Garbage collections are scheduled automatically. See
  :ref:`topic-collection-schedule`.

* Does not use :term:`generational garbage collection`, so blocks are
  never promoted out of the generation in which they are allocated.

* Blocks may contain :term:`exact references` or :term:`weak
  references (1)` to blocks in the same or other pools (but may not
  contain :term:`ambiguous references`, and may not use :term:`remote
  references`).

* Allocations may be variable in size.

* The :term:`alignment` of blocks is configurable.

* Blocks may have :term:`dependent objects`.

* Blocks that are not :term:`reachable` from a :term:`root` are
  automatically :term:`reclaimed`.

* Blocks are :term:`scanned <scan>`.

* Blocks may only be referenced by :term:`base pointers` (unless they
  have :term:`in-band headers`).

* Blocks may be protected by :term:`barriers (1)`.

* Blocks do not :term:`move <moving garbage collector>`.

* Blocks may be registered for :term:`finalization`.

* Blocks must belong to an :term:`object format` which provides
  :term:`scan <scan method>` and :term:`skip <skip method>` methods.

* Blocks may have :term:`in-band headers`.


.. index::
   pair: AWL; dependent object

.. _pool-awl-dependent:

Dependent objects
-----------------

In order to support prompt deletion of values in a :term:`weak-key
hash table` when the key is :term:`splatted <splat>` (and prompt
deletion of keys in a :term:`weak-value hash table`), an AWL pool
allows each object to have a :dfn:`dependent object`. (This is where
the "Linked" in the name of the pool class comes from.)

The dependent object is specified by the
:c:macro:`MPS_KEY_AWL_FIND_DEPENDENT` keyword argument to
:c:func:`mps_pool_create_k` when creating an AWL pool. This is a
function of type :c:type:`mps_awl_find_dependent_t` that takes the
address of an object in the pool and returns the address of its
dependent object (or a null pointer if there is no corresponding
dependent object).

When :term:`scanning <scan>` an object in an AWL pool, the MPS ensures
that the dependent object is not protected. This means that the
:term:`scan method` in the pool's :term:`object format` can read or
write the dependent object.

If an object contains a reference to its dependent object, you should
:term:`fix` that reference, and be aware that if it is a weak
reference then it may be splatted when the dependent object dies.

The way you would normally use this feature in a weak hash table would
be to put the table's keys in one object, and its values in another.
(This would be necessary in any case, because the MPS does not support
a mixture of :term:`exact references` and :term:`weak references (1)`
in the same object.) The dependent object for the keys objects is the
values object, and vice versa (if necessary). The scan method looks
out for the splatting of a reference, and when this is detected, it
splats the corresponding reference in the dependent object.

For example::

    obj_t obj_deleted;              /* deleted entry in hash table */

    typedef struct weak_array_s {
        struct weak_array_s *dependent;
        size_t length;              /* tagged as "length * 2 + 1" */
        obj_t slot[1];
    } weak_array_s, *weak_array_t;

    typedef weak_table_s {
        type_s type;                /* TYPE_WEAK_TABLE */
        weak_array_t keys, values;
    } weak_table_s, *weak_table_t;

    mps_addr_t weak_array_find_dependent(mps_addr_t addr)
    {
        weak_array_t a = addr;
        return a->dependent;
    }

    mps_res_t weak_array_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
    {
        MPS_SCAN_BEGIN(ss) {
            while (base < limit) {
                mps_addr_t p;
                weak_array_t a = base;
                size_t i, length = a->length >> 1; /* untag */
                p = a->dependent;
                MPS_FIX12(ss, &p);
                a->dependent = p;
                for (i = 0; i < length; ++i) {
                    p = a->slot[i];
                    if (MPS_FIX1(ss, p)) {
                        mps_res_t res = MPS_FIX2(ss, &p);
                        if (res != MPS_RES_OK) return res;
                        if (p == NULL && a->dependent) {
                            /* key/value was splatted: splat value/key too */
                            a->dependent->slot[i] = obj_deleted;
                            a->slot[i] = obj_deleted;
                        } else {
                            a->slot[i] = p;
                        }
                    }
                }
                base += offsetof(weak_array_s, slot) + a->length * sizeof a->slot[0];
            }
        } MPS_SCAN_END(ss);
        return MPS_RES_OK;
    }

.. note::

    The ``length`` field of the ``weak_array_s`` structure contains
    the value ``length * 2 + 1`` so that it cannot be mistaken for a
    pointer. See :ref:`pool-awl-caution` below.


.. index::
   pair: AWL; protection faults

.. _pool-awl-barrier:

Protection faults
-----------------

AWL has another special power: it enables better handing of
:term:`protection faults` on :dfn:`weak objects` (objects containing
:term:`weak references (1)`).

To explain the benefit we first need to describe the problem. The MPS
uses a :term:`read barrier` to perform :term:`incremental garbage
collection`. When the client program tries to read an object
containing :term:`weak references (1)`, the MPS may have
:term:`protected <protection>` it so that the MPS can process the
object before the client gets to see it.

The problem is that the client program may try to access a weak object
at a point in the :term:`collection cycle` when the MPS cannot yet
determine the status of the objects that the weak object refers to.
What the MPS does in this situation is assume that all the referenced
objects are going to live. This assumption is correct but
conservative; it may result in objects that are weakly referenced
staying alive for longer than they need to. In the worst case this can
result in a very large amount of memory being used by objects that are
no longer needed.

In order to combat this problem the MPS sometimes does the following:
Instead of processing the entire weak object and unprotecting it, so
that the client program can access the object, the MPS may emulate the
processor instruction. When this happens, the MPS doesn't process the
entire weak object; it only processes the exact location that was
being accessed (typically a single word). It emulates the processor
instruction, and it keeps the object protected. This happens invisibly
from the client program's perspective: it's exactly as if the
instruction executed as normal.

Naturally this emulation business is delicate and involves staring at
the most badly written parts of low-level processor architecture
manuals for days.

Emulation of accesses to protected objects happens when all of the
following are true:

#. The object is a weak object allocated in an AWL pool.

#. The MPS is running on Linux/IA-32 or Windows/IA-32. Extending this
   list to new (reasonable) operating systems should be tolerable (for
   example, OS X/IA-32). Extending this to new processor architectures
   requires more work.

#. The processor instruction that is accessing the object is of a
   suitable simple form. The MPS doesn't contain an emulator for all
   possible instructions that might access memory, so currently it
   only recognizes and emulates a simple ``MOV`` from memory to a
   register or vice-versa.

:ref:`Contact us <contact>` if you need emulation of access to weak
references for new operating systems, processor architectures, or
memory access instructions.


.. index::
   pair: AWL; cautions

.. _pool-awl-caution:

Caution
-------

Because of the instruction emulation described in
:ref:`pool-awl-barrier` above, AWL places the following restriction on
the format of objects allocated in it:

* Each slot in an object must either be a valid word-aligned
  reference, or else the bottom bits of the word must be non-zero so
  that it does not look like an aligned pointer.

  "Aligned pointer" means a word whose numeric value (that is, its
  value when treated as an unsigned integer) is a multiple of the size
  of a pointer. If you're using a 64-bit architecture, that means that
  an aligned pointer is a multiple of 8 and its bottom three bits are
  zero.

  The bottom line is that references from an object in an AWL pool
  must be untagged and aligned, and integers must be tagged with a
  non-zero tag.

Normally one would cope with this restriction by allocating the table
metadata in a pool belonging to another pool class, and only
allocating the arrays of keys and values in an AWL pool. See :ref:`the
example <pool-awl-dependent>` above.


.. index::
   single: AWL; interface

AWL interface
-------------

::

   #include "mpscawl.h"

.. c:function:: mps_class_t mps_class_awl(void)

    Return the :term:`pool class` for an AWL (Automatic Weak Linked)
    :term:`pool`.

    When creating an AWL pool, :c:func:`mps_pool_create_k` requires
    one :term:`keyword argument`:

    * :c:macro:`MPS_KEY_FORMAT` (type :c:type:`mps_fmt_t`) specifies
      the :term:`object format` for the objects allocated in the pool.
      The format must provide a :term:`scan method` and a :term:`skip
      method`.

    It accepts three optional keyword arguments:

    * :c:macro:`MPS_KEY_AWL_FIND_DEPENDENT` (type

      :c:type:`mps_awl_find_dependent_t`) is a function that specifies
      how to find the :term:`dependent object` for an object in the
      pool. This defaults to a function that always returns ``NULL``
      (meaning that there is no dependent object).

    * :c:macro:`MPS_KEY_CHAIN` (type :c:type:`mps_chain_t`) specifies
      the :term:`generation chain` for the pool. If not specified, the
      pool will use the arena's default chain.

    * :c:macro:`MPS_KEY_GEN` (type :c:type:`unsigned`) specifies the
      :term:`generation` in the chain into which new objects will be
      allocated. If you pass your own chain, then this defaults to
      ``0``, but if you didn't (and so use the arena's default chain),
      then an appropriate generation is used.

      Note that AWL does not use generational garbage collection, so
      blocks remain in this generation and are not promoted.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
            MPS_ARGS_ADD(args, MPS_KEY_AWL_FIND_DEPENDENT, find_dependent);
            res = mps_pool_create_k(&pool, arena, mps_class_awl(), args);
        } MPS_ARGS_END(args);

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the format and
        find-dependent function like this::

            mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                      mps_class_t mps_class_awl(),
                                      mps_fmt_t fmt,
                                      mps_awl_find_dependent_t find_dependent)

    When creating an :term:`allocation point` on an AWL pool,
    :c:func:`mps_ap_create_k` accepts one keyword argument:

    * :c:macro:`MPS_KEY_RANK` (type :c:type:`mps_rank_t`, default
      :c:func:`mps_rank_exact`) specifies the :term:`rank` of
      references in objects allocated on this allocation point. It
      must be :c:func:`mps_rank_exact` (if the objects allocated on
      this allocation point will contain :term:`exact references`), or
      :c:func:`mps_rank_weak` (if the objects will contain :term:`weak
      references (1)`).

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_weak());
            res = mps_ap_create_k(&ap, awl_pool, args);
        } MPS_ARGS_END(args);

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_ap_create`, pass the rank like this::

            mps_res_t mps_ap_create(mps_ap_t *ap_o, mps_pool_t pool,
                                    mps_rank_t rank)


.. c:type:: mps_addr_t (*mps_awl_find_dependent_t)(mps_addr_t addr)

    The type of functions that find the :term:`dependent object` for
    an object in an AWL pool.

    ``addr`` is the address of an object in an AWL pool.

    Returns the address of the corresponding dependent object, or a
    null pointer if there is none.

    The dependent object need not be in memory managed by the MPS, but
    if it is, then it must be in a :term:`non-moving <non-moving
    garbage collector>` pool in the same arena as ``addr``.

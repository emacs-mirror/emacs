.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/manual/wiki/pool_classes.html>`_
    `<https://info.ravenbrook.com/project/mps/master/design/poolamc/>`_

.. index::
   single: AMC pool class
   single: pool class; AMC

.. _pool-amc:

AMC (Automatic Mostly-Copying)
==============================

**AMC** is a general-purpose :term:`automatically managed <automatic
memory management>` :term:`pool class`. This is the most mature pool
class in the MPS, intended for the majority of objects in the client
program. Use this pool class unless you need a particular feature that
it doesn't provide.

"Mostly Copying" means that it uses :term:`copying garbage collection`
except for blocks that are :term:`pinned <pinning>` by
:term:`ambiguous references`. 

It uses :term:`generational garbage collection`. That is, it exploits
assumptions about object lifetimes and inter-connection variously
referred to as "the :term:`generational hypothesis`". In particular,
the following tendencies will be efficiently exploited by an AMC pool:

- most objects die young;

- objects that don't die young will live a long time.


.. index::
   single: AMC pool class; properties

AMC properties
--------------

* Does not support allocation via :c:func:`mps_alloc` or deallocation
  via :c:func:`mps_free`.

* Supports allocation via :term:`allocation points`. If an allocation
  point is created in an AMC pool, the call to
  :c:func:`mps_ap_create_k` takes no keyword arguments.

* Supports :term:`allocation frames` but does not use them to improve
  the efficiency of stack-like allocation.

* Does not support :term:`segregated allocation caches`.

* Garbage collections are scheduled automatically. See
  :ref:`topic-collection-schedule`.

* Uses :term:`generational garbage collection`: blocks are promoted
  from generation to generation in the pool's chain.

* Blocks may contain :term:`exact references` to blocks in the same or
  other pools (but may not contain :term:`ambiguous references` or
  :term:`weak references (1)`, and may not use :term:`remote
  references`).

* Allocations may be variable in size.

* The :term:`alignment` of blocks is configurable.

* Blocks do not have :term:`dependent objects`.

* Blocks that are not :term:`reachable` from a :term:`root` are
  automatically :term:`reclaimed`.

* Blocks are :term:`scanned <scan>`.

* Blocks may be referenced by :term:`interior pointers` (unless
  :c:macro:`MPS_KEY_INTERIOR` is set to ``FALSE``, in which case only
  :term:`base pointers`, or :term:`client pointers` if the blocks
  have :term:`in-band headers`, are supported).

* Blocks may be protected by :term:`barriers (1)`.

* Blocks may :term:`move <moving garbage collector>`.

* Blocks may be registered for :term:`finalization`.

* Blocks must belong to an :term:`object format` which provides
  :term:`scan <scan method>`, :term:`skip <skip method>`,
  :term:`forward <forward method>`, :term:`is-forwarded <is-forwarded
  method>`, and :term:`padding <padding method>` methods.

* Blocks may have :term:`in-band headers`.


.. index::
   single: AMC pool class; interface

AMC interface
-------------

::

   #include "mpscamc.h"

.. c:function:: mps_pool_class_t mps_class_amc(void)

    Return the :term:`pool class` for an AMC (Automatic
    Mostly-Copying) :term:`pool`.

    When creating an AMC pool, :c:func:`mps_pool_create_k` requires
    one :term:`keyword argument`:

    * :c:macro:`MPS_KEY_FORMAT` (type :c:type:`mps_fmt_t`) specifies
      the :term:`object format` for the objects allocated in the pool.
      The format must provide a :term:`scan method`, a :term:`skip
      method`, a :term:`forward method`, an :term:`is-forwarded
      method` and a :term:`padding method`.

    It accepts three optional keyword arguments:

    * :c:macro:`MPS_KEY_CHAIN` (type :c:type:`mps_chain_t`) specifies
      the :term:`generation chain` for the pool. If not specified, the
      pool will use the arena's default chain.

    * :c:macro:`MPS_KEY_INTERIOR` (type :c:type:`mps_bool_t`, default
      ``TRUE``) specifies whether :term:`ambiguous <ambiguous
      reference>` :term:`interior pointers` to blocks in the pool keep
      objects alive. If this is ``FALSE``, then only :term:`client
      pointers` keep objects alive.

    * :c:macro:`MPS_KEY_EXTEND_BY` (type :c:type:`size_t`,
      default 4096) is the minimum :term:`size` of the memory segments
      that the pool requests from the :term:`arena`. Larger segments
      reduce the per-segment overhead, but increase
      :term:`fragmentation` and :term:`retention`.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
            res = mps_pool_create_k(&pool, arena, mps_class_amc(), args);
        } MPS_ARGS_END(args);

    When creating an :term:`allocation point` on an AMC pool,
    :c:func:`mps_ap_create_k` accepts one optional keyword argument:

    * :c:macro:`MPS_KEY_AP_HASH_ARRAYS` (type :c:type:`mps_bool_t`,
      defaulting to false) specifies (if true) that blocks allocated
      from the allocation point do not contribute to the *new size* of
      the :term:`nursery space` for the purposes of deciding whether
      to start a collection of that generation. See
      :ref:`pool-amc-hash-arrays`.


.. index::
   pair: AMC pool class; hash arrays

.. _pool-amc-hash-arrays:

Hash arrays
-----------

The :term:`location dependency` feature of the MPS allows the
:term:`client program` to implement address-based hash tables in pools
like AMC that use a :term:`moving memory manager`, re-hashing the
tables when the addresses they contain might have moved.

However, when a frequently-used hash table grows large enough, the
following sequence of events may take place:

1. The hash table discovers that its location dependency is stale.

2. A new array is allocated to contain the re-hashed keys.

3. The new array is large enough to push the *new size* of the
   :term:`nursery space` (that is, the amount of newly allocated
   memory since the last collection in the first :term:`generation` in
   the :term:`generation chain` for the pool containing the array)
   close to its capacity.

4. A small amount of additional allocation causes the new size of the
   nursery generation to exceed its capacity, which causes the MPS to
   start a new collection of that generation. This in turn causes the
   hash table to become stale again.

When the hash table reaches this critical size, the client program may
find that a large fraction of its time is being spent re-hashing the
table.

In order to avoid this happening, the MPS provides a mechanism for
specifying that the newly allocated array does not contribute to the
new size of the nursery space: this cuts off the vicious cycle at step
3.

To enable this mechanism, use the optional :c:macro:`MPS_KEY_AP_HASH_ARRAYS`
keyword argument when creating an allocation point with
:c:func:`mps_ap_create_k`. This interface is documented in the AMC Interface
section of the :ref:`pool-amc` documentation above.

See :ref:`topic-collection-schedule` for an explanation of the *new
size* of a generation, and how the MPS uses this to determine when to
start a collection of that generation.

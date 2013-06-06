.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/manual/wiki/pool_classes.html>`_
    `<https://info.ravenbrook.com/project/mps/master/design/poolamc/>`_

.. index::
   single: AMC
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
referred to as "the generational hypothesis". In particular, the
following tendencies will be efficiently exploited by an AMC pool:

- most objects die young;

- objects that don't die young will live a long time.

In the pool's :term:`generation chain`, specify the capacity and
mortality of generations 0 to *n*\−1. Survivors from generation *n*\−1
get promoted into an arena-wide "top" generation.


.. index::
   single: AMC; properties

AMC properties
--------------

* Does not support allocation via :c:func:`mps_alloc` or deallocation
  via :c:func:`mps_free`.

* Supports allocation via :term:`allocation points`. If an allocation
  point is created in an AMC pool, the call to :c:func:`mps_ap_create`
  takes no additional parameters.

* Supports :term:`allocation frames` but does not use them to improve
  the efficiency of stack-like allocation.

* Does not support :term:`segregated allocation caches`.

* Garbage collections are scheduled automatically. See
  :ref:`topic-collection-schedule`.

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

* Blocks may only be referenced by :term:`base pointers` (unless they
  have :term:`in-band headers`).

* Blocks may be protected by :term:`barriers (1)`.

* Blocks may :term:`move <moving garbage collector>`.

* Blocks may be registered for :term:`finalization`.

* Blocks must belong to an :term:`object format` which provides
  :term:`scan <scan method>`, :term:`skip <skip method>`,
  :term:`forward <forward method>`, :term:`is-forwarded <is-forwarded
  method>`, and :term:`padding <padding method>` methods.

* Blocks may have :term:`in-band headers`.


.. index::
   single: AMC; interface

AMC interface
-------------

::

   #include "mpscamc.h"

.. c:function:: mps_class_t mps_class_amc(void)

    Return the :term:`pool class` for an AMC (Automatic
    Mostly-Copying) :term:`pool`.

    When creating an AMC pool, :c:func:`mps_pool_create_k` requires
    two :term:`keyword arguments`:

    * :c:macro:`MPS_KEY_FORMAT` (type :c:type:`mps_fmt_t`) specifies
      the :term:`object format` for the objects allocated in the pool.
      The format must provide a :term:`scan method`, a :term:`skip
      method`, a :term:`forward method`, an :term:`is-forwarded
      method` and a :term:`padding method`.

    * :c:macro:`MPS_KEY_CHAIN` (type :c:type:`mps_chain_t`) specifies
      the :term:`generation chain` for the pool.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
            MPS_ARGS_DONE(args);
            res = mps_pool_create_k(&pool, arena, mps_class_amc(), args);
        } MPS_ARGS_END(args);

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the format and
        chain like this::

            mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                      mps_class_t mps_class_amc(),
                                      mps_fmt_t fmt,
                                      mps_chain_t chain)


.. index::
   pair: AMC; introspection

AMC introspection
-----------------

::

   #include "mpscamc.h"

.. c:function:: void mps_amc_apply(mps_pool_t pool, mps_amc_apply_stepper_t f, void *p, size_t s)

    Visit all :term:`formatted objects` in an AMC pool.

    ``pool`` is the pool whose formatted objects you want to visit.

    ``f`` is a function that will be called for each formatted object in
    the pool.

    ``p`` and ``s`` are arguments that will be passed to ``f`` each time it
    is called. This is intended to make it easy to pass, for example,
    an array and its size as parameters.

    It is an error to call this function when the :term:`arena` is not
    in the :term:`parked state`. You need to call
    :c:func:`mps_arena_collect` or :c:func:`mps_arena_park` before
    calling :c:func:`mps_amc_apply`.

    The function ``f`` will be called on both :term:`client <client
    object>` and :term:`padding objects`. It is the job of ``f`` to
    distinguish, if necessary, between the two. It may also be called
    on :term:`dead` objects that the collector has not recycled or has
    been unable to recycle.

    .. note::

        There is no equivalent function for other pool classes, but
        there is a more general function
        :c:func:`mps_arena_formatted_objects_walk` that visits all
        formatted objects in the arena.

    .. note::

        This function is intended for heap analysis, tuning, and
        debugging, not for frequent use in production.


.. c:type:: void (*mps_amc_apply_stepper_t)(mps_addr_t addr, void *p, size_t s)

    The type of a :term:`stepper function` for :term:`formatted
    objects` in an AMC pool.

    ``addr`` is the address of an object in the pool.
    
    ``p`` and ``s`` are the corresponding arguments that were passed
    to :c:func:`mps_amc_apply`.

    The function may not call any function in the MPS. It may access:

    a. memory inside the object or block pointed to by ``addr``;

    b. memory managed by the MPS that is in pools that do not protect
       their contents;

    c. memory not managed by the MPS;

    It must not access other memory managed by the MPS.

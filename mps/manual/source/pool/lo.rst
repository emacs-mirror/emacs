.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/manual/wiki/pool_classes.html>`_
    `<https://info.ravenbrook.com/project/mps/master/design/poollo/>`_

.. index::
   single: LO
   single: pool class; LO

.. _pool-lo:

LO (Leaf Object)
================

**LO** is an :term:`automatically managed <automatic memory
management>` :term:`pool class` for :term:`leaf objects` (objects that
contain no references). It does not move or protect its objects.

This pool class is intended for unstructured data that needs to be
accessed by :term:`foreign code`. It's ideal for allocating a buffer
that needs to be passed to an operating system I/O function.

.. note::

    A thread that reads or writes from blocks allocated in this pool
    need not be :ref:`registered with the arena
    <topic-thread-register>` so long as the :term:`liveness <live>` of
    the block is independent of that thread.

    This means that you can launch a thread to read or write a buffer
    allocated in this pool, without having to register the thread, so
    long as you ensure that the buffer remains alive until the thread
    has finished (for example, by keeping a reference to the buffer in
    a :term:`root` or a :term:`scanned <scan>` object).

If LO is used to allocate large numbers of small objects, the garbage
collection performance will degrade. For leaf objects that can move
and be protected, it is better to use :ref:`pool-amcz` instead.


.. index::
   single: LO; properties

LO properties
-------------

* Does not support allocation via :c:func:`mps_alloc` or deallocation
  via :c:func:`mps_free`.

* Supports allocation via :term:`allocation points`. If an allocation
  point is created in a LO pool, the call to
  :c:func:`mps_ap_create_k` takes no keyword arguments.

* Supports :term:`allocation frames` but does not use them to improve
  the efficiency of stack-like allocation.

* Does not support :term:`segregated allocation caches`.

* Garbage collections are scheduled automatically. See
  :ref:`topic-collection-schedule`.

* Does not use :term:`generational garbage collection`, so blocks are
  never promoted out of the generation in which they are allocated.

* Blocks may not contain :term:`references` to blocks in automatically
  managed pools.

* Allocations may be variable in size.

* The :term:`alignment` of blocks is configurable.

* Blocks do not have :term:`dependent objects`.

* Blocks that are not :term:`reachable` from a :term:`root` are
  automatically :term:`reclaimed`.

* Blocks are not :term:`scanned <scan>`. A consequence of this is that
  the pool's :term:`object format` need not provide a :term:`scan
  method`.

* Blocks may only be referenced by :term:`base pointers` (unless they
  have :term:`in-band headers`).

* Blocks are not protected by :term:`barriers (1)`.

* Blocks do not :term:`move <moving garbage collector>`.

* Blocks may be registered for :term:`finalization`.

* Blocks must belong to an :term:`object format` which provides
  :term:`scan <scan method>` and :term:`skip <skip method>` methods.

* Blocks may have :term:`in-band headers`.


.. index::
   single: LO; interface

LO interface
------------

::

   #include "mpsclo.h"

.. c:function:: mps_class_t mps_class_lo(void)

    Return the :term:`pool class` for an LO (Leaf Object)
    :term:`pool`.

    When creating an LO pool, :c:func:`mps_pool_create_k` requires one
    :term:`keyword argument`:

    * :c:macro:`MPS_KEY_FORMAT` (type :c:type:`mps_fmt_t`) specifies
      the :term:`object format` for the objects allocated in the pool.
      The format must provide a :term:`skip method`.

    It accepts two optional keyword arguments:

    * :c:macro:`MPS_KEY_CHAIN` (type :c:type:`mps_chain_t`) specifies
      the :term:`generation chain` for the pool. If not specified, the
      pool will use the arena's default chain.

    * :c:macro:`MPS_KEY_GEN` (type :c:type:`unsigned`) specifies the
      :term:`generation` in the chain into which new objects will be
      allocated. If you pass your own chain, then this defaults to
      ``0``, but if you didn't (and so use the arena's default chain),
      then an appropriate generation is used.

      Note that LO does not use generational garbage collection, so
      blocks remain in this generation and are not promoted.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
            MPS_ARGS_DONE(args);
            res = mps_pool_create_k(&pool, arena, mps_class_lo(), args);
        } MPS_ARGS_END(args);

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the format like
        this::

            mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                      mps_class_t mps_class_lo(),
                                      mps_fmt_t fmt)

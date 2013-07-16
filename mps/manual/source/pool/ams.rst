.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/manual/wiki/pool_classes.html>`_
    `<https://info.ravenbrook.com/project/mps/master/design/poolams/>`_

.. index::
   single: AMS
   single: pool class; AMS

.. _pool-ams:

AMS (Automatic Mark and Sweep)
==============================

**AMS** is an :term:`automatically managed <automatic memory
management>` but :term:`non-moving <non-moving garbage collector>`
:term:`pool class`. It should be used instead of :ref:`pool-amc` for
blocks that need to be automatically managed, but cannot be moved.

AMS does not use :term:`generational garbage collection`, but when
creating a pool you use a :term:`generation chain` to specify the
capacity and mortality of a single "generation". These numbers are
used to schedule the collection of the whole pool.

.. note::

    AMS is likely to be useful as a step in integrating a program with
    the MPS. It allows you to work on scanning (and investigate errors
    resulting from underscanning) without having to deal with objects
    moving as well. When you are confident that scanning is correct,
    you can switch to :ref:`pool-amc`.

    AMS is not currently suitable for production use. However, it
    could be developed into a solid mark-and-sweep pool. If you have a
    use case that needs this, :ref:`contact us <contact>`.


.. index::
   single: AMS; properties

AMS properties
--------------

* Does not support allocation via :c:func:`mps_alloc` or deallocation
  via :c:func:`mps_free`.

* Supports allocation via :term:`allocation points`. If an allocation
  point is created in an AMS pool, the call to
  :c:func:`mps_ap_create_k` takes no keyword arguments.

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

* Blocks are not protected by :term:`barriers (1)`.

* Blocks do not :term:`move <moving garbage collector>`.

* Blocks may be registered for :term:`finalization`.

* Blocks must belong to an :term:`object format` which provides
  :term:`scan <scan method>` and :term:`skip <skip method>` methods.

* Blocks may have :term:`in-band headers`.


.. index::
   single: AMS; interface

AMS interface
-------------

::

   #include "mpscams.h"


.. c:function:: mps_class_t mps_class_ams(void)

    Return the :term:`pool class` for an AMS (Automatic Mark & Sweep)
    :term:`pool`.

    When creating an AMS pool, :c:func:`mps_pool_create_k` requires
    two :term:`keyword arguments`:

    * :c:macro:`MPS_KEY_FORMAT` (type :c:type:`mps_fmt_t`) specifies
      the :term:`object format` for the objects allocated in the pool.
      The format must provide a :term:`scan method` and a :term:`skip
      method`.

    * :c:macro:`MPS_KEY_CHAIN` (type :c:type:`mps_chain_t`) specifies
      the :term:`generation chain` for the pool. It must have a single
      generation.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
            MPS_ARGS_DONE(args);
            res = mps_pool_create_k(&pool, arena, mps_class_ams(), args);
        } MPS_ARGS_END(args);

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the format and
        chain like this::

            mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                      mps_class_t mps_class_ams(),
                                      mps_fmt_t fmt,
                                      mps_chain_t chain)


.. c:function:: mps_class_t mps_class_ams_debug(void)

    A :ref:`debugging <topic-debugging>` version of the AMS pool
    class.

    When creating a debugging AMS pool, :c:func:`mps_pool_create_k`
    requires three keyword arguments: :c:macro:`MPS_KEY_FORMAT` and
    :c:macro:`MPS_KEY_CHAIN` are as described above, and
    :c:macro:`MPS_KEY_POOL_DEBUG_OPTIONS` specifies the debugging
    options. See :c:type:`mps_debug_option_s`.

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the format,
        chain, and debugging options like this::

            mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                      mps_class_t mps_class_ams_debug(),
                                      mps_debug_option_s debug_option,
                                      mps_fmt_t fmt,
                                      mps_chain_t chain)

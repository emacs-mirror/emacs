.. Sources:

    `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/stack-alloc/>`_

.. index::
   single: SNC pool class
   single: pool class; SNC

.. _pool-snc:

SNC (Stack No Checking)
=======================

**SNC** is a :term:`manually managed <manual memory management>`
:term:`pool class` that supports a stack-like protocol for allocation
and deallocation using :term:`allocation frames` on :term:`allocation
points`. See :ref:`topic-frame`.

If :c:func:`mps_ap_frame_pop` is used on an allocation point in an SNC
pool (after a corresponding call to :c:func:`mps_ap_frame_push`), then
the objects affected by the pop are assumed to be dead, and are
reclaimed by the collector without checking whether there are any
references to them.

This pool class is intended to be used to implement stack languages
like Forth and PostScript, where some objects are allocated in stack
frames and are known to be dead when the stack is popped, because the
language can ensure that objects that are kept alive when the stack is
popped are copied to the heap.


.. index::
   single: SNC pool class; properties

SNC properties
--------------

* Does not support allocation via :c:func:`mps_alloc`.

* Supports allocation via :term:`allocation points` only. If an
  allocation point is created in an SNC pool, the call to
  :c:func:`mps_ap_create_k` accepts one optional keyword argument,
  :c:macro:`MPS_KEY_RANK`.

* Does not support deallocation via :c:func:`mps_free`.

* Supports :term:`allocation frames`.

* Does not support :term:`segregated allocation caches`.

* Blocks may contain :term:`exact references` to blocks in the same or
  other pools (but may not contain :term:`ambiguous references` or
  :term:`weak references (1)`, and may not use :term:`remote
  references`).

* There are no garbage collections in this pool.

* Allocations may be variable in size.

* The :term:`alignment` of blocks is configurable.

* Blocks do not have :term:`dependent objects`.

* Blocks are not automatically :term:`reclaimed`.

* Blocks are :term:`scanned <scan>`.

* Blocks may only be referenced by :term:`base pointers`.

* Blocks are not protected by :term:`barriers (1)`.

* Blocks do not :term:`move <moving garbage collector>`.

* Blocks may not be registered for :term:`finalization`.

* Blocks must belong to an :term:`object format` which provides
  :term:`scan <scan method>`, :term:`skip <skip method>`, and
  :term:`padding <padding method>` methods.

* Blocks must not have :term:`in-band headers`.


.. index::
   single: SNC pool class; interface

SNC interface
-------------

::

   #include "mpscsnc.h"


.. c:function:: mps_pool_class_t mps_class_snc(void)

    Return the :term:`pool class` for an SNC (Stack No Check)
    :term:`pool`.

    When creating an SNC pool, :c:func:`mps_pool_create_k` requires one
    :term:`keyword argument`:

    * :c:macro:`MPS_KEY_FORMAT` (type :c:type:`mps_fmt_t`) specifies
      the :term:`object format` for the objects allocated in the pool.
      The format must provide a :term:`scan method`, a :term:`skip
      method`, and a :term:`padding method`.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
            res = mps_pool_create_k(&pool, arena, mps_class_snc(), args);
        } MPS_ARGS_END(args);

    When creating an :term:`allocation point` on an SNC pool,
    :c:func:`mps_ap_create_k` accepts one optional keyword argument:

    * :c:macro:`MPS_KEY_RANK` (type :c:type:`mps_rank_t`, default
      :c:func:`mps_rank_exact`) specifies the :term:`rank` of references
      in objects allocated on this allocation point.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_RANK, mps_rank_exact());
            res = mps_ap_create_k(&ap, awl_pool, args);
        } MPS_ARGS_END(args);

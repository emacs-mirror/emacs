.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/manual/wiki/pool_classes.html>`_
    `<https://info.ravenbrook.com/project/mps/master/design/poollo/>`_

.. index::
   single: LO; introduction
   single: pool class; LO

.. _pool-lo:

LO (Leaf Only)
==============

**LO** is an :term:`automatically managed <automatic memory
management>` :term:`pool class` for :term:`leaf objects` (objects that
contain no references). It does not move or protect its objects.

This pool class is intended for unstructured data that needs to be
accessed by :term:`foreign code`. It's ideal for allocating a buffer
that needs to be passed to an operating system I/O function.

For leaf objects that can move and be protected, consider
:ref:`pool-amcz` instead.


.. index::
   single: LO; properties

LO properties
-------------

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

* Blocks may not contain :term:`references`.

* Allocations may be variable in size.

* The :term:`alignment` of blocks is configurable.

* Blocks do not have :term:`dependent objects`.

* Blocks that are not :term:`reachable` from a :term:`root` are
  automatically :term:`reclaimed`.

* Blocks are not :term:`scanned <scan>`. A consequence of this is that
  the pool's :term:`object format` need not provide a :term:`scan
  method`.

* Blocks may only be referenced by :term:`base pointers` (unless they
  belong to an object format of variant auto-header).

* Blocks are not protected by :term:`barriers (1)`.

* Blocks do not :term:`move <moving garbage collector>`. A consequence
  of this is that the pool's :term:`object format` need not provide a
  :term:`forward method` or an :term:`is-forwarded method`. (It also
  does not need a :term:`padding method`.)

* Blocks may be registered for :term:`finalization`.

* Blocks must belong to an :term:`object format`.


.. index::
   single: LO; interface

LO interface
------------

::

   #include "mpsclo.h"

.. c:function:: mps_class_t mps_class_lo(void)

    Return the :term:`pool class` for an LO (Leaf Only) :term:`pool`.

    When creating an LO pool, :c:func:`mps_pool_create` takes one
    extra argument::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_lo(),
                                  mps_fmt_t fmt)

    ``fmt`` specifies the :term:`object format` for the objects
    allocated in the pool.

.. index::
   single: MV pool class
   single: pool class; MV

.. _pool-mv:

MV (Manual Variable)
====================

**MV** is a general-purpose :term:`manually managed <manual memory
management>` :term:`pool class` that manages :term:`blocks` of
variable size.

.. deprecated:: starting with version 1.117.

    Use :ref:`pool-mvff` instead.


.. index::
   single: MV pool class; properties

MV properties
-------------

* Supports allocation via :c:func:`mps_alloc` and deallocation via
  :c:func:`mps_free`.

* Does not support allocation via :term:`allocation points`.

* Does not support :term:`allocation frames`.

* Supports :term:`segregated allocation caches`.

* There are no garbage collections in this pool.

* Blocks may not contain :term:`references` to blocks in automatically
  managed pools (unless these are registered as :term:`roots`).

* Allocations may be variable in size.

* The :term:`alignment` of blocks is configurable.

* Blocks do not have :term:`dependent objects`.

* Blocks are not automatically :term:`reclaimed`.

* Blocks are not :term:`scanned <scan>`.

* Blocks are not protected by :term:`barriers (1)`.

* Blocks do not :term:`move <moving garbage collector>`.

* Blocks may not be registered for :term:`finalization`.

* Blocks must not belong to an :term:`object format`.

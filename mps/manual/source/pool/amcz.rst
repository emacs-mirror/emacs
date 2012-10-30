.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/manual/wiki/pool_classes.html>`_

.. index::
   single: AMCZ
   single: pool class; AMCZ

.. _pool-amcz:

AMCZ (Automatic Mostly-Copying Zero-rank)
=========================================

**AMCZ** is a general-purpose :term:`automatically managed <automatic
memory management>` :term:`pool class` for :term:`leaf objects`
("zero-rank" objects that contain no references).

It is otherwise indentical to :ref:`pool-amc`.

AMCZ is intended for "simple" objects like numbers, characters, and
strings. Segregating these objects into one or more AMCZ pools avoids
the cost of scanning them that would be incurred if they were
interleaved in a pool with objects containing references.


.. index::
   single: AMCZ; properties

AMCZ properties
---------------

AMCZ is indentical to :ref:`pool-amc`, except that:

* Blocks may not contain :term:`references`.

* Blocks are not :term:`scanned <scan>`. A consequence of this is that
  the pool's :term:`object format` need not provide a :term:`scan
  method`.

* Blocks are not protected by :term:`barriers (1)`.


.. index::
   single: AMCZ; interface

AMCZ interface
--------------

::

   #include "mpscamc.h"

.. c:function:: mps_class_t mps_class_amcz(void)

    Return the :term:`pool class` for an AMCZ (Automatic
    Mostly-Copying Zero-rank) :term:`pool`.

    When creating an AMCZ pool, :c:func:`mps_pool_create` takes two
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_amcz(),
                                  mps_fmt_t fmt,
                                  mps_chain_t chain)

    ``fmt`` specifies the :term:`object format` for the objects
    allocated in the pool.

    ``chain`` specifies the :term:`generation chain` for the pool.

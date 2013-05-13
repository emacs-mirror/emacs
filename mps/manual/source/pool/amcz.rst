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

It is otherwise identical to :ref:`pool-amc`.

AMCZ is intended for "simple" objects like numbers, characters, and
strings. Segregating these objects into one or more AMCZ pools avoids
the cost of scanning them that would be incurred if they were
interleaved in a pool with objects containing references. It may also
simplify the scanning of the objects that are left behind.

See :ref:`guide-advanced-segregation` for an example.


.. index::
   single: AMCZ; properties

AMCZ properties
---------------

AMCZ is identical to :ref:`pool-amc`, except that:

* Blocks may not contain :term:`references` to blocks in automatically
  managed pools.

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

    When creating an AMCZ pool, :c:func:`mps_pool_create_k` requires
    two :term:`keyword arguments`:

    * :c:macro:`MPS_KEY_FORMAT` (type :c:type:`mps_fmt_t`) specifies
      the :term:`object format` for the objects allocated in the pool.
      The format must provide a :term:`skip method`, a :term:`forward
      method`, an :term:`is-forwarded method` and a :term:`padding
      method`.

    * :c:macro:`MPS_KEY_CHAIN` (type :c:type:`mps_chain_t`) specifies
      the :term:`generation chain` for the pool.

    For example::

        MPS_ARGS_BEGIN(args) {
            MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
            MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
            MPS_ARGS_DONE(args);
            res = mps_pool_create_k(&pool, arena, mps_class_amcz(), args);
        } MPS_ARGS_END(args);

    .. deprecated:: starting with version 1.112.

        When using :c:func:`mps_pool_create`, pass the format and
        chain like this::

          mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                    mps_class_t mps_class_amcz(),
                                    mps_fmt_t fmt,
                                    mps_chain_t chain)

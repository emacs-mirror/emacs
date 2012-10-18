.. _pool-mv:

====================
MV (Manual Variable)
====================

The :term:`pool class` MV (Manual Variable) is a general-purpose class
of :term:`manually managed <manual memory management>` pools that
manage :term:`blocks <block>` of variable size.

It supports allocation via :c:func:`mps_alloc` and deallocation via
:c:func:`mps_free`.

It does not support :term:`allocation points <allocation point>`.


-------------------
MV symbol reference
-------------------

::

   #include "mpscmv.h"


.. c:function:: mps_class_t mps_class_mv(void)

    Return the :term:`pool class` for an MV (Manual Variable)
    :term:`pool`.

    When creating an MV pool, :c:func:`mps_pool_create` takes three
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_mvff(),
                                  mps_size_t extend_size,
                                  mps_size_t average_size,
                                  mps_size_t maximum_size)


    ``extend_size`` is the :term:`size` of :term:`segment` that the
    pool will request from the :term:`arena`.

    ``average_size`` and ``maximum size`` are the predicted average
    and maximum size of blocks that will be allocated from the pool.


------------
Undocumented
------------

.. c:function:: size_t mps_mv_free_size(mps_pool_t pool)
.. c:function:: size_t mps_mv_size(mps_pool_t pool)
.. c:function:: mps_class_t mps_class_mv_debug(void)

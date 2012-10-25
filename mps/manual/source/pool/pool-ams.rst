.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/manual/wiki/pool_classes.html>`_
    `<https://info.ravenbrook.com/project/mps/master/design/poolams/>`_

.. _pool-ams:

==============================
AMS (Automatic Mark and Sweep)
==============================

Non-moving automatic (collecting) pool class.

AMS is "Automatic Mark & Sweep". Not generational.

Chain: specify capacity and mortality of 'generation' 0. [Why? Perhaps to trigger nursery-like collections? New 2001-03-02. RHSK 2006-11-27]



--------------------
AMS symbol reference
--------------------

::

   #include "mpscams.h"


.. c:function:: mps_class_t mps_class_ams(void)

    Return the :term:`pool class` for an AMC (Automatic
    Mostly-Copying) :term:`pool`.

    When creating an AMC pool, :c:func:`mps_pool_create` takes two
    extra arguments::

        mps_res_t mps_pool_create(mps_pool_t *pool_o, mps_arena_t arena, 
                                  mps_class_t mps_class_amc(),
                                  mps_fmt_t fmt,
                                  mps_chain_t chain)

    ``fmt`` specifies the :term:`object format` for the objects
    allocated in the pool.

    ``chain`` specifies the :term:`generation chain` for the pool. It
    must have a single generation.


------------
Undocumented
------------

.. c:function:: mps_class_t mps_class_ams_debug(void)

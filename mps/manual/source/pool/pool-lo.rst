.. Sources:

    `<https://info.ravenbrook.com/project/mps/master/manual/wiki/pool_classes.html>`_
    `<https://info.ravenbrook.com/project/mps/master/design/poollo/>`_

.. _pool-lo:

==============
LO (Leaf Only)
==============

Non-moving, non-protecting, automatic (collecting), Leaf Only pool class (for objects that contain no references). Unit test: locv.c. RefMan: -undocumented-.

This pool class should really be called "flat data that needs to be accessed by foreign code" -- it's ideal for allocating a buffer to pass to an operating system I/O call. It is not very fast. [Conversation with DRJ, 2006-07].

(For a general-purpose leaf-only pool see AMCZ).


-------------------
LO symbol reference
-------------------

::

   #include "mpsclo.h"


------------
Undocumented
------------

.. c:function:: mps_class_t mps_class_lo(void)

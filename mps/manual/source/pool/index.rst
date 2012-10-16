.. _pool:

**************
Pool reference
**************

.. toctree::
   :maxdepth: 2

   pool-amc
   pool-amcz
   pool-ams
   pool-awl
   pool-lo
   pool-mv
   pool-mvff
   pool-mvt
   pool-snc


=====================
Pool class comparison
=====================

This table summarizes the properties of each :term:`pool class`
provided by the MPS. For "block" properties, "yes" means that the
property holds for *all* blocks allocated from the pool. An entry "--"
indicates that a property makes no sense for a pool class: for
example, if blocks in a pool may not contain :term:`references
<reference>`, it makes no sense to ask whether they may contain
:term:`weak references <weak reference (1)>`.


+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Property                                     | AMC  | AMCZ | AMS  | AWL  | LO   | MV   | MVFF | MVT  | SNC  |
+==============================================+======+======+======+======+======+======+======+======+======+
| Supports :c:func:`mps_alloc`?                | no   | no   | ??   | no   | no   | yes  | yes  | no   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Supports :c:func:`mps_free`?                 | no   | no   | ??   | no   | no   | yes  | yes  | yes  | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Supports allocation points?                  | yes  | yes  | ??   | yes  | yes  | no   | no   | yes  | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Timing of collections? [2]_                  | auto | auto | ??   | auto | auto | --   | --   | --   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| May contain references? [7]_                 | yes  | no   | ??   | yes  | no   | no   | no   | no   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| May contain exact references? [11]_          | yes  | --   | ??   | yes  | --   | --   | --   | --   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| May contain ambiguous references? [11]_      | no   | --   | ??   | no   | --   | --   | --   | --   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| May contain weak references? [11]_           | no   | --   | ??   | yes  | --   | --   | --   | --   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Allocations fixed or variable in size?       | var  | var  | ??   | var  | var  | var  | var  | var  | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Alignment? [5]_                              | conf | conf | ??   | conf | conf | [4]_ | [6]_ | [4]_ | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Associated objects? [10]_                    | no   | --   | ??   | yes  | --   | --   | --   | --   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| May use remote references? [3]_              | no   | --   | ??   | no   | --   | --   | --   | --   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Ambiguous references keep blocks alive?      | no   | no   | ??   | no   | no   | --   | --   | --   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Blocks are automatically managed? [1]_       | yes  | yes  | ??   | yes  | yes  | no   | no   | no   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Blocks are manually managed? [1]_            | no   | no   | ??   | no   | no   | yes  | yes  | yes  | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Blocks are scanned? [9]_                     | yes  | no   | ??   | yes  | no   | no   | no   | no   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Blocks support base references only? [8]_    | yes  | yes  | ??   | yes  | yes  | --   | --   | --   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Blocks support internal references? [8]_     | no   | no   | ??   | no   | no   | --   | --   | --   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Blocks may be protected by barriers?         | yes  | no   | ??   | yes  | no   | no   | no   | no   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Blocks may move?                             | yes  | yes  | ??   | no   | no   | no   | no   | no   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Blocks may be finalized?                     | yes  | yes  | ??   | yes  | yes  | no   | no   | no   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
| Blocks must be formatted? [9]_               | yes  | yes  | ??   | yes  | yes  | no   | no   | no   | ??   |
+----------------------------------------------+------+------+------+------+------+------+------+------+------+
                                             

-----
Notes
-----

.. [1] Blocks are "automatically managed" if they may be automatically
       discarded when the MPS determines that they are dead; they are
       "manually managed" if they can be discarded when the
       :term:`client program` requests it. Note that these properties
       are not mutually exclusive, although the MPS does not provide a
       pool class that satisfies both.

.. [2] "Timing of collections" is "auto" if :term:`garbage collection`
       is under the control of the MPS, which decides when collection
       should take place and performs it :term:`automatically
       <automatic memory management>` and :term:`incrementally
       <incremental garbage collection>`.

.. [3] "Remote references" are references that are stored outside the
       block to which they logically belong (for example, in some kind
       of auxiliary table). A pool containing remote references cannot
       rely on a :term:`write barrier` to detect changed references.

.. [4] The alignment of blocks allocated from :ref:`pool-mv` and
       :ref:`pool-mvt` pools is platform-dependent.

.. [5] "Alignment" is "conf" if the client program may specify
       :term:`alignment` for each pool.

.. [6] :ref:`pool-mvff` pools have conifgurable alignment, but it may
       not be smaller than :c:macro:`MPS_PF_ALIGN` (the :term:`natural
       alignment` for the :term:`platform`).

.. [7] Pools that may not contain references are suitable for storing
       :term:`leaf objects <leaf object>` only.

.. [8] A block "supports internal references" if a reference to any
       location within the block is considered to be a reference to
       the block. It "supports base references only" if only a
       reference to the base of the block is considered to be a
       reference to the block.

.. [9] Blocks "are scanned" if the MPS :term:`scans <scan>` them for
       references; blocks "must be formatted" if they are described to
       the MPS by an :term:`object format`. At present, the MPS only
       knows how to scan blocks using the :term:`scan method` from an
       object format, but it is in theory possible to scan unformatted
       blocks.

.. [10] In pools with this property, each object may specify an
       :term:`associated object` which the client program guarantees
       will be accessible during the scanning of the first
       object. This may be used in the implementation of :term:`weak
       hash tables <weak hash table>`.

.. [11] Pools "may contain :term:`ambiguous <ambiguous reference>` /
       :term:`exact <exact reference>` / :term:`weak <weak
       reference (1)>` references" if the references that the client
       program fixes during scanning may include references of the
       indicated :term:`rank`.

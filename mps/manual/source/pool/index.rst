.. _pool:

Pool reference
**************

.. toctree::
   :maxdepth: 1
   :glob:

   pool-*


.. _pool-choose:

Choosing a pool class
=====================

This section contains a simple procedure for choosing a :term:`pool
class` based on the properties of the data you plan to store in
it. The MPS works well if you can segregate your data into a variety
of pools, choosing the most appropriate pool class for each.

.. note::

    Pool classes can differ in many ways not considered here: speed,
    vulnerability to fragmentation, control overhead, and so on. This
    procedure gives you a decent recommendation, but an expert in the
    MPS might be able to make a better one.

First, answer these questions about your data:

1. Do you need the MPS to :term:`automatically <automatic memory
   management>` :term:`reclaim` :term:`unreachable` blocks?

2. Is it acceptable for the MPS to :term:`move <moving memory
   manager>` blocks in memory and to place :term:`barriers <barrier
   (1)>` on blocks? (For example, it might not be acceptable to move a
   block if it has been passed to :term:`foreign code` that remembered
   its location.)

3. Do your blocks contain :term:`references <reference>` to blocks
   stored in automatically managed pools (including references to
   other blocks in the same pool, if it's automatically managed)? And
   if so, are these references :term:`exact <exact reference>`,
   :term:`ambiguous <ambiguous reference>` or :term:`weak <weak
   reference (1)>`?

Second, look up your answers in this table to find the recommended
pool class to use:

==========  ======================  ===========  ====================================
Automatic?  Movable & protectable?  References?  Use this pool class
==========  ======================  ===========  ====================================
yes         yes                     none         :ref:`pool-amcz`
yes         yes                     exact        :ref:`pool-amc`
yes         yes                     ambiguous    nothing suitable
yes         yes                     weak         :ref:`pool-awl`
yes         no                      none         :ref:`pool-lo`
yes         no                      exact        :ref:`pool-ams`
yes         no                      ambiguous    nothing suitable
yes         no                      weak         nothing suitable
no          *any*                   none         :ref:`pool-mvt`
no          *any*                   ambiguous    :ref:`pool-mvt` [1]_
no          *any*                   exact        :ref:`pool-mvt` [1]_
no          *any*                   weak         :ref:`pool-mvt` [1]_
==========  ======================  ===========  ====================================

.. note::

    .. [1] :ref:`pool-mvt` doesn't scan for references, but you can
           work around this by registering your blocks as :term:`roots
           <root>` (with the appropriate :term:`rank`) just after they
           are allocated, and deregistering them just before freeing
           them.


.. _pool-properties:

Pool class properties
=====================

This table summarizes the properties of each :term:`pool class`
provided by the MPS. For "block" properties, "yes" means that the
property holds for *all* blocks allocated from the pool. An entry
"---" indicates that a property makes no sense for a pool class: for
example, if blocks in a pool may not contain :term:`references
<reference>`, it makes no sense to ask whether they may contain
:term:`weak references <weak reference (1)>`.


=============================================  =====  =====  =====  =====  =====  =====  =====  =====  =====
Property                                       AMC    AMCZ   AMS    AWL    LO     MV     MVFF   MVT    SNC
=============================================  =====  =====  =====  =====  =====  =====  =====  =====  =====
Supports :c:func:`mps_alloc`?                  no     no     no     no     no     yes    yes    no     ??
Supports :c:func:`mps_free`?                   no     no     no     no     no     yes    yes    yes    ??
Supports allocation points?                    yes    yes    yes    yes    yes    no     no     yes    ??
Timing of collections? [2]_                    auto   auto   yes    auto   auto   ---    ---    ---    ??
May contain references? [3]_                   yes    no     yes    yes    no     no     no     no     ??
May contain exact references? [4]_             yes    ---    yes    yes    ---    ---    ---    ---    ??
May contain ambiguous references? [4]_         no     ---    no     no     ---    ---    ---    ---    ??
May contain weak references? [4]_              no     ---    no     yes    ---    ---    ---    ---    ??
Allocations fixed or variable in size?         var    var    var    var    var    var    var    var    ??
Alignment? [5]_                                conf   conf   conf   conf   conf   [6]_   [7]_   [6]_   ??
Dependent objects? [8]_                        no     ---    no     yes    ---    ---    ---    ---    ??
May use remote references? [9]_                no     ---    ??     no     ---    ---    ---    ---    ??
Ambiguous references keep blocks alive?        no     no     ??     no     no     ---    ---    ---    ??
Blocks are automatically managed? [10]_        yes    yes    yes    yes    yes    no     no     no     ??
Blocks are manually managed? [10]_             no     no     no     no     no     yes    yes    yes    ??
Blocks are scanned? [11]_                      yes    no     yes    yes    no     no     no     no     ??
Blocks support base references only? [12]_     yes    yes    ??     yes    yes    ---    ---    ---    ??
Blocks support internal references? [12]_      no     no     ??     no     no     ---    ---    ---    ??
Blocks may be protected by barriers?           yes    no     ??     yes    no     no     no     no     ??
Blocks may move?                               yes    yes    no     no     no     no     no     no     ??
Blocks may be finalized?                       yes    yes    ??     yes    yes    no     no     no     ??
Blocks must be formatted? [11]_                yes    yes    yes    yes    yes    no     no     no     ??
=============================================  =====  =====  =====  =====  =====  =====  =====  =====  =====

.. note::

    .. [2] "Timing of collections" is "auto" if :term:`garbage collection`
           is under the control of the MPS, which decides when collection
           should take place and performs it :term:`automatically
           <automatic memory management>` and :term:`incrementally
           <incremental garbage collection>`.

    .. [3] Pools that may not contain references are suitable for storing
           :term:`leaf objects <leaf object>` only.

    .. [4] Pools "may contain :term:`ambiguous <ambiguous reference>` /
           :term:`exact <exact reference>` / :term:`weak <weak
           reference (1)>` references" if the references that the client
           program fixes during scanning may include references of the
           indicated :term:`rank`.

    .. [5] "Alignment" is "conf" if the client program may specify
           :term:`alignment` for each pool.

    .. [6] The alignment of blocks allocated from :ref:`pool-mv` and
           :ref:`pool-mvt` pools is platform-dependent.

    .. [7] :ref:`pool-mvff` pools have configurable alignment, but it may
           not be smaller than :c:macro:`MPS_PF_ALIGN` (the :term:`natural
           alignment` for the :term:`platform`).

    .. [8] In pools with this property, each object may specify an
           :term:`dependent object` which the client program guarantees
           will be accessible during the scanning of the first
           object. This may be used in the implementation of :term:`weak
           hash tables <weak hash table>`.

    .. [9] "Remote references" are references that are stored outside the
           block to which they logically belong (for example, in some kind
           of auxiliary table). A pool containing remote references cannot
           rely on a :term:`write barrier` to detect changed references.

    .. [10] Blocks are "automatically managed" if they may be
           automatically discarded when the MPS determines that they
           are unreachable; they are "manually managed" if they can be
           discarded when the :term:`client program` requests it. Note
           that these properties are not mutually exclusive, although
           the MPS does not provide a pool class that satisfies both.

    .. [11] Blocks "are scanned" if the MPS :term:`scans <scan>` them
           for references; blocks "must be formatted" if they are
           described to the MPS by an :term:`object format`. At
           present, the MPS only knows how to scan blocks using the
           :term:`scan method` from an object format, but the MPS
           design does not preclude pools that scan unformatted
           blocks.

    .. [12] A block "supports internal references" if a reference to any
           location within the block is considered to be a reference to
           the block. It "supports base references only" if only a
           reference to the base of the block is considered to be a
           reference to the block.

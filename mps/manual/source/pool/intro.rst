.. index::
   single: pool class; choosing

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
    MPS might be able to make a better recommendation. And if no pool
    class in the open source MPS exactly matches your needs, then it
    is possible to develop new pool classes. See :ref:`pool-writing`.

First, answer these questions about your data:

1. Do you need the MPS to :term:`automatically <automatic memory
   management>` :term:`reclaim` :term:`unreachable` blocks?

2. Is it acceptable for the MPS to :term:`move <moving memory
   manager>` blocks in memory and to place :term:`barriers (1)` on
   blocks? (For example, it might not be acceptable to move a block if
   it has been passed to :term:`foreign code` that remembered its
   location.)

3. Do your blocks contain :term:`references` to blocks stored in
   automatically managed pools (including references to other blocks
   in the same pool, if it's automatically managed)? And if so, are
   these references :term:`exact <exact reference>`, :term:`ambiguous
   <ambiguous reference>` or :term:`weak <weak reference (1)>`?

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
no          *any*                   none         :ref:`pool-mvff`
no          *any*                   exact        :ref:`pool-mvff` [1]_
no          *any*                   ambiguous    :ref:`pool-mvff` [1]_
no          *any*                   weak         :ref:`pool-mvff` [1]_
==========  ======================  ===========  ====================================

.. note::

    .. [1] :ref:`pool-mvff` doesn't scan for references, but you can
           work around this by registering your blocks as
           :term:`roots` (with the appropriate :term:`rank`) just
           after they are allocated, and deregistering them just
           before freeing them.


.. Sources:

     `<https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/guide/pool-classes/>`_

.. index::
   single: pool class; table of properties

.. _pool-properties:

Pool class properties
=====================

This table summarizes the properties of each :term:`pool class`
provided by the open source MPS. For "block" properties, "yes" means
that the property holds for *all* blocks allocated from the pool. An
entry "---" indicates that a property makes no sense for a pool class:
for example, if blocks in a pool may not contain :term:`references`,
it makes no sense to ask whether they may contain :term:`weak
references (1)`.


=============================================  =====  =====  =====  =====  =====  =====  =====  =====  =====  =====
Property                                       AMC    AMCZ   AMS    AWL    LO     MFS    MV     MVFF   MVT    SNC
=============================================  =====  =====  =====  =====  =====  =====  =====  =====  =====  =====
Supports :c:func:`mps_alloc`?                  no     no     no     no     no     yes    yes    yes    no     no
Supports :c:func:`mps_free`?                   no     no     no     no     no     yes    yes    yes    yes    no
Supports allocation points?                    yes    yes    yes    yes    yes    no     no     yes    yes    yes
Supports allocation frames?                    yes    yes    yes    yes    yes    no     no     yes    yes    yes
Supports segregated allocation caches?         no     no     no     no     no     yes    yes    yes    no     no
Timing of collections? [2]_                    auto   auto   auto   auto   auto   ---    ---    ---    ---    ---
May contain references? [3]_                   yes    no     yes    yes    no     no     no     no     no     yes
May contain exact references? [4]_             yes    ---    yes    yes    ---    ---    ---    ---    ---    yes
May contain ambiguous references? [4]_         no     ---    no     no     ---    ---    ---    ---    ---    no
May contain weak references? [4]_              no     ---    no     yes    ---    ---    ---    ---    ---    no
Allocations fixed or variable in size?         var    var    var    var    var    fixed  var    var    var    var
Alignment? [5]_                                conf   conf   conf   conf   conf   [6]_   [6]_   [7]_   [6]_   conf
Dependent objects? [8]_                        no     ---    no     yes    ---    ---    ---    ---    ---    no
May use remote references? [9]_                no     ---    no     no     ---    ---    ---    ---    ---    no
Blocks are automatically managed? [10]_        yes    yes    yes    yes    yes    no     no     no     no     no
Blocks are manually managed? [10]_             no     no     no     no     no     yes    yes    yes    yes    yes
Blocks are scanned? [11]_                      yes    no     yes    yes    no     no     no     no     no     yes
Blocks support base pointers only? [12]_       yes    yes    yes    yes    yes    ---    ---    ---    ---    yes
Blocks support internal pointers? [12]_        no     no     no     no     no     ---    ---    ---    ---    no
Blocks may be protected by barriers?           yes    no     yes    yes    yes    no     no     no     no     yes
Blocks may move?                               yes    yes    no     no     no     no     no     no     no     no
Blocks may be finalized?                       yes    yes    yes    yes    yes    no     no     no     no     no
Blocks must be formatted? [11]_                yes    yes    yes    yes    yes    no     no     no     no     yes
Blocks may belong to format auto-header?       yes    yes    yes    yes    yes    ---    ---    ---    ---    no
=============================================  =====  =====  =====  =====  =====  =====  =====  =====  =====  =====

.. note::

    .. [2] "Timing of collections" is "auto" if :term:`garbage collection`
           is under the control of the MPS, which decides when collection
           should take place and performs it :term:`automatically
           <automatic memory management>` and :term:`incrementally
           <incremental garbage collection>`.

    .. [3] The references in question are references to blocks in
           :term:`automatically managed <automatic memory management>`
           :term:`pools`.

    .. [4] Pools "may contain :term:`ambiguous <ambiguous reference>` /
           :term:`exact <exact reference>` / :term:`weak <weak
           reference (1)>` references" if the references that the client
           program fixes during scanning may include references of the
           indicated :term:`rank`.

    .. [5] "Alignment" is "conf" if the client program may specify
           :term:`alignment` for each pool.

    .. [6] The alignment of blocks allocated from :ref:`pool-mv` and
           :ref:`pool-mvt` pools is platform-dependent.

    .. [7] :ref:`pool-mvff` pools have configurable alignment, but it
           may not be smaller than the :term:`natural alignment` for
           the :term:`platform` (see :c:macro:`MPS_PF_ALIGN`).

    .. [8] In pools with this property, each object may specify an
           :term:`dependent object` which the client program
           guarantees will be accessible during the scanning of the
           first object. This may be used in the implementation of
           :term:`weak hash tables`.

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

    .. [11] Blocks "are scanned" if the MPS :term:`scans` them for
           references; blocks "must be formatted" if they are
           described to the MPS by an :term:`object format`. At
           present, the MPS only knows how to scan blocks using the
           :term:`scan method` from an object format, but the MPS
           design does not preclude pools that scan unformatted
           blocks.

    .. [12] A block "supports internal pointers" if a pointer to any
           location within the block is considered to be a reference
           to the block. It "supports base pointers only" if only a
           pointer to the base of the block (or, if the block belongs
           to an object format of variant auto-header, a pointer just
           past the end of the header) is considered to be a reference
           to the block.


.. index::
   single: pool class; writing

.. _pool-writing:

Writing a new pool class
========================

If none of the pool classes supplied with the MPS are quite right for
your application, don't despair: the MPS is designed to be extensible
with new pool classes, and designed so that the properties of pools
are as orthogonal as possible. So if you need a pool containing
objects that are scannable but unformatted, or movable objects which
are manually managed, or a pool all of whose objects are roots, there
is no technical reason why it should not be possible to write it.

If you'd be interested in our developing new pool classes for your
requirements, or if you've started writing a new pool class
yourself, :ref:`we'd love to hear from you <contact>`.

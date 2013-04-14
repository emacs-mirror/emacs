.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/cbs/>`_


.. mps:prefix:: design.mps.cbs

Coalescing block structure
==========================


Introduction
------------

:mps:tag:`intro` This is the design for :mps:ref:`impl.c.cbs`, which
implements a data structure for the management of non-intersecting
memory ranges, with eager coalescence.

:mps:tag:`readership` This document is intended for any MM developer.

:mps:tag:`source` :mps:ref:`design.mps.poolmv2`, :mps:ref:`design.mps.poolmvff`.

:mps:tag:`overview` The "coalescing block structure" is a set of
addresses (or a subset of address space), with provision for efficient
management of contiguous ranges, including insertion and deletion,
high level communication with the client about the size of contiguous
ranges, and detection of protocol violations.


History
-------

:mps:tag:`hist.0` This document was derived from the outline in
:mps:ref:`design.mps.poolmv2(2)`. Written by Gavin Matthews
1998-05-01.

:mps:tag:`hist.1` Updated by Gavin Matthews 1998-07-22 in response to
approval comments in :mps:ref:`change.epcore.anchovy.160040` There is
too much fragmentation in trapping memory.

:mps:tag:`hist.2` Updated by Gavin Matthews (as part of
:mps:ref:`change.epcore.brisling.160158`: MVFF cannot be instantiated
with 4-byte alignment) to document new alignment restrictions.

:mps:tag:`hist.3` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.4` Converted to reStructuredText. Gareth Rees,
2013-04-14.


Definitions
-----------

:mps:tag:`def.range` A (contiguous) range of addresses is a semi-open
interval on address space.

:mps:tag:`def.isolated` A contiguous range is isolated with respect to
some property it has, if adjacent elements do not have that property.

:mps:tag:`def.interesting` A block is interesting if it is of at least
the minimum interesting size specified by the client.


Requirements
------------

:mps:tag:`req.set` Must maintain a set of addresses.

:mps:tag:`req.fast` Common operations must have a low amortized cost.

:mps:tag:`req.add` Must be able to add address ranges to the set.

:mps:tag:`req.remove` Must be able to remove address ranges from the set.

:mps:tag:`req.size` Must report concisely to the client when isolated
contiguous ranges of at least a certain size appear and disappear.

:mps:tag:`req.iterate` Must support the iteration of all isolated
contiguous ranges. This will not be a common operation.

:mps:tag:`req.protocol` Must detect protocol violations.

:mps:tag:`req.debug` Must support debugging of client code.

:mps:tag:`req.small` Must have a small space overhead for the storage
of typical subsets of address space and not have abysmal overhead for
the storage of any subset of address space.

:mps:tag:`req.align` Must support an alignment (the alignment of all
addresses specifying ranges) of down to ``sizeof(void *)`` without
losing memory.


Interface
---------

:mps:tag:`header` CBS is used through :mps:ref:`impl.h.cbs`.


External types
..............

.. c:type:: typedef struct CBSStruct CBSStruct, *CBS;

:mps:tag:`type.cbs` :c:type:`CBS` is the main datastructure for
manipulating a CBS. It is intended that a :c:type:`CBSStruct` be
embedded in another structure. No convenience functions are provided
for the allocation or deallocation of the CBS.

.. c:type:: typedef struct CBSBlockStruct CBSBlockStruct, *CBSBlock;

:mps:tag:`type.cbs.block` :c:type:`CBSBlock` is the data-structure
that represents an isolated contiguous range held by the CBS. It is
returned by the new and delete methods described below.

:mps:tag:`type.cbs.method` The following methods are provided as
callbacks to advise the client of certain events. The implementation
of these functions should not cause any CBS function to be called on
the same CBS. In this respect, the CBS module is not re-entrant.

.. c:type:: typedef void (*CBSChangeSizeMethod)(CBS cbs, CBSBlock block, Size oldSize, SizeNewSize);

:mps:tag:`type.cbs.change.size.method` :c:type:`CBSChangeSizeMethod`
is the function pointer type, four instances of which are optionally
registered via CBSInit.

These callbacks are invoked under :c:func:`CBSInsert`,
:c:func:`CBSDelete`, or :c:func:`CBSSetMinSize` in certain
circumstances. Unless otherwise stated, ``oldSize`` and ``newSize``
will both be non-zero, and different. The accessors
:c:func:`CBSBlockBase`, :c:func:`CBSBlockLimit`, and
:c:func:`CBSBlockSize` may be called from within these callbacks,
except within the delete callback when ``newSize`` is zero. See
:mps:ref:`.impl.callback` for implementation details.

.. c:type:: typedef Bool (*CBSIterateMethod)(CBS cbs, CBSBlock block, void *closureP, unsigned long closureS);

:mps:tag:`type.cbs.iterate.method` :c:type:`CBSIterateMethod` is a
function pointer type for a client method invoked by the CBS module
for every isolated contiguous range in address order, when passed to
the :c:func:`CBSIterate` or :c:func:`CBSIterateLarge` functions. The
function returns a boolean indicating whether to continue with the
iteration.


External functions
..................

.. c:function::  Res CBSInit(Arena arena, CBS cbs, CBSChangeSizeMethod new, CBSChangeSizeMethod delete, CBSChangeSizeMethod grow, CBSChangeSizeMethod shrink, Size minSize, Align alignment, Bool mayUseInline)

:mps:tag:`function.cbs.init` :c:func:`CBSInit` is the function that
initialises the CBS structure. It performs allocation in the supplied
arena. Four methods are passed in as function pointers (see
:mps:ref:`.type.*` above), any of which may be ``NULL``. It receives a
minimum size, which is used when determining whether to call the
optional methods. The ``mayUseInline`` Boolean indicates whether the
CBS may use the memory in the ranges as a low-memory fallback (see
:mps:ref:`.impl.low-mem`). The alignment indicates the alignment of
ranges to be maintained. An initialised CBS contains no ranges.

:mps:tag:`function.cbs.init.may-use-inline` If ``mayUseInline`` is
set, then ``alignment`` must be at least ``sizeof(void *)``. In this
mode, the CBS will never fail to insert or delete ranges, even if
memory for control structures becomes short. Note that, in such cases,
the CBS may defer notification of new/grow events, but will report
available blocks in :c:func:`CBSFindFirst` and :c:func:`CBSFindLast`.
Such low memory conditions will be rare and transitory. See
:mps:ref:`.align` for more details.

.. c:function:: void CBSFinish(CBS cbs)

:mps:tag:`function.cbs.finish` :c:func:`CBSFinish` is the function
that finishes the CBS structure and discards any other resources
associated with the CBS.

.. c:function:: Res CBSInsert(CBS cbs, Addr base, Addr limit)

:mps:tag:`function.cbs.insert` :c:func:`CBSInsert` is the function
used to add a contiguous range specified by ``[base,limit)`` to the
CBS. If any part of the range is already in the CBS, then
:c:macro:`ResFAIL` is returned, and the CBS is unchanged. This
function may cause allocation; if this allocation fails, and any
contingency mechanism fails, then :c:macro:`ResMEMORY` is returned,
and the CBS is unchanged.

:mps:tag:`function.cbs.insert.callback` :c:func:`CBSInsert` will invoke callbacks as follows:

* ``new``: when a new block is created that is interesting. ``oldSize == 0; newSize >= minSize``.

* ``new``: when an uninteresting block coalesces to become interesting. ``0 < oldSize < minSize <= newSize``.

* ``delete``: when two interesting blocks are coalesced.  ``grow`` will also be invoked in this case on the larger of the two blocks.  ``newSize == 0; oldSize >= minSize``.

* ``grow``: when an interesting block grows in size.  ``minSize <= oldSize < newSize``.
 
.. c:function::  Res CBSDelete(CBS cbs, Addr base, Addr limit)

:mps:tag:`function.cbs.delete` :c:func:`CBSDelete` is the function
used to remove a contiguous range specified by ``[base,limit)`` from
the CBS. If any part of the range is not in the CBS, then
:c:macro:`ResFAIL` is returned, and the CBS is unchanged. This
function may cause allocation; if this allocation fails, and any
contingency mechanism fails, then :c:macro:`ResMEMORY` is returned,
and the CBS is unchanged.

:mps:tag:`function.cbs.delete.callback` :c:func:`CBSDelete` will
invoke callbacks as follows:

* ``delete``: when an interesting block is entirely removed.  ``newSize == 0; oldSize >= minSize``.
* ``delete``: when an interesting block becomes uninteresting. ``0 < newSize < minSize <= oldSize``.
* ``new``: when a block is split into two blocks, both of which are interesting. ``shrink`` will also be invoked in this case on the larger of the two blocks. ``oldSize == 0; newSize >= minSize``.
* ``shrink``: when an interesting block shrinks in size, but remains interesting. ``minSize <= newSize < oldSize``.

.. c:function:: void CBSIterate(CBS cbs, CBSIterateMethod iterate, void *closureP, unsigned long closureS)

:mps:tag:`function.cbs.iterate` :c:func:`CBSIterate` is the function
used to iterate all isolated contiguous ranges in a CBS. It receives a
pointer, unsigned long closure pair to pass on to the iterator method,
and an iterator method to invoke on every range in address order. If
the iterator method returns ``FALSE``, then the iteration is
terminated.

.. c:function:: void CBSIterateLarge(CBS cbs, CBSIterateMethod iterate, void *closureP, unsigned long closureS)

:mps:tag:`function.cbs.iterate.large` :c:func:`CBSIterateLarge` is the
function used to iterate all isolated contiguous ranges of size
greater than or equal to the client indicated minimum size in a CBS.
It receives a pointer, unsigned long closure pair to pass on to the
iterator method, and an iterator method to invoke on every large range
in address order. If the iterator method returns ``FALSE``, then the
iteration is terminated.

.. c:function:: void CBSSetMinSize(CBS cbs, Size minSize)

:mps:tag:`function.cbs.set.min-size` :c:func:`CBSSetMinSize` is the
function used to change the minimum size of interest in a CBS. This
minimum size is used to determine whether to invoke the client
callbacks from :c:func:`CBSInsert` and :c:func:`CBSDelete`. This
function will invoke either the ``new`` or ``delete`` callback for all
blocks that are (in the semi-open interval) between the old and new
values. ``oldSize`` and ``newSize`` will be the same in these cases.

.. c:function:: Res CBSDescribe(CBS cbs, mps_lib_FILE *stream)

:mps:tag:`function.cbs.describe` :c:func:`CBSDescribe` is a function
that prints a textual representation of the CBS to the given stream,
indicating the contiguous ranges in order, as well as the structure of
the underlying splay tree implementation. It is provided for debugging
purposes only.

.. c:function:: Addr CBSBlockBase(CBSBlock block)

:mps:tag:`function.cbs.block.base` The :c:func:`CBSBlockBase` function
returns the base of the range represented by the :c:type:`CBSBlock`.
This function may not be called from the delete callback when the
block is being deleted entirely.

.. note::

    The value of the base of a particular :c:type:`CBSBlock` is not
    guaranteed to remain constant across calls to :c:func:`CBSDelete`
    and :c:func:`CBSInsert`, regardless of whether a callback is
    invoked.

.. c:function:: Addr CBSBlockLimit(CBSBlock block)

:mps:tag:`function.cbs.block.limit` The :c:func:`CBSBlockLimit`
function returns the limit of the range represented by the
:c:type:`CBSBlock`. This function may not be called from the delete
callback when the block is being deleted entirely.

.. note::

    The value of the limit of a particular :c:type:`CBSBlock` is not
    guaranteed to remain constant across calls to :c:func:`CBSDelete`
    and :c:func:`CBSInsert`, regardless of whether a callback is
    invoked.

.. c:function:: Size CBSBlockSize(CBSBlock block)

:mps:tag:`function.cbs.block.size` The :c:func:`CBSBlockSize` function
returns the size of the range represented by the :c:type:`CBSBlock`.
This function may not be called from the ``delete`` callback when the
block is being deleted entirely.

.. note::

    The value of the size of a particular :c:type:`CBSBlock` is not
    guaranteed to remain constant across calls to :c:func:`CBSDelete`
    and :c:func:`CBSInsert`, regardless of whether a callback is
    invoked.

.. c:function:: Res CBSBlockDescribe(CBSBlock block, mps_lib_FILE *stream)

:mps:tag:`function.cbs.block.describe` The :c:func:`CBSBlockDescribe`
function prints a textual representation of the :c:type:`CBSBlock` to
the given stream. It is provided for debugging purposes only.

.. c:function:: Bool CBSFindFirst(Addr *baseReturn, Addr *limitReturn, CBS cbs, Size size, CBSFindDelete findDelete)

:mps:tag:`function.cbs.find.first` The :c:func:`CBSFindFirst` function
locates the first block (in address order) within the CBS of at least
the specified size, and returns its range. If there are no such
blocks, it returns ``FALSE``. It optionally deletes the top, bottom,
or all of the found range, depending on the ``findDelete`` argument
(this saves a separate call to :c:func:`CBSDelete`, and uses the
knowledge of exactly where we found the range), which must come from
this enumeration::

    enum {
        CBSFindDeleteNONE,    /* don't delete after finding */
        CBSFindDeleteLOW,     /* delete precise size from low end */
        CBSFindDeleteHIGH,    /* delete precise size from high end */
        CBSFindDeleteENTIRE   /* delete entire range */
    };

.. c:function:: Bool CBSFindLast(Addr *baseReturn, Addr *limitReturn, CBS cbs, Size size, CBSFindDelete findDelete)

:mps:tag:`function.cbs.find.last` The :c:func:`CBSFindLast` function
locates the last block (in address order) within the CBS of at least
the specified size, and returns its range. If there are no such
blocks, it returns ``FALSE``. Like :c:func:`CBSFindFirst`, it
optionally deletes the range.

.. c:function:: Bool CBSFindLargest(Addr *baseReturn, Addr *limitReturn, CBS cbs, CBSFindDelete findDelete)

:mps:tag:`function.cbs.find.largest` The :c:func:`CBSFindLargest`
function locates the largest block within the CBS, and returns its
range. If there are no blocks, it returns ``FALSE``. Like
:c:func:`CBSFindFirst`, it optionally deletes the range (specifying
``CBSFindDeleteLOW`` or ``CBSFindDeleteHIGH`` has the same effect as
``CBSFindDeleteENTIRE``).


Alignment
---------

:mps:tag:`align` When ``mayUseInline`` is specified to permit inline
data structures and hence avoid losing memory in low memory
situations, the alignments that the CBS supports are constrained by
three requirements:

- The smallest possible range (namely one that is the alignment in
  size) must be large enough to contain a single ``void *`` pointer (see
  :mps:ref:`.impl.low-mem.inline.grain`);

- Any larger range (namely one that is at least twice the alignment in
  size) must be large enough to contain two ``void *`` pointers (see
  :mps:ref:`.impl.low-mem.inline.block`);

- It must be valid on all platforms to access a ``void *`` pointer
  stored at the start of an aligned range.

All alignments that meet these requirements are aligned to
``sizeof(void *)``, so we take that as the minimum alignment.


Implementation
--------------

:mps:tag:`impl` Note that this section is concerned with describing
various aspects of the implementation. It does not form part of the
interface definition.


Size change callback protocol
.............................

:mps:tag:`impl.callback` The size change callback protocol concerns
the mechanism for informing the client of the appearance and
disappearance of interesting ranges. The intention is that each range
has an identity (represented by the :c:type:`CBSBlock`). When blocks
are split, the larger fragment retains the identity. When blocks are
merged, the new block has the identity of the larger fragment.

:mps:tag:`impl.callback.delete` Consider the case when the minimum
size is ``minSize``, and :c:func:`CBSDelete` is called to remove a
range of size ``middle``. The two (possibly non-existant) neighbouring
ranges have (possibly zero) sizes ``left`` and ``right``. ``middle`` is part
of the :c:type:`CBSBlock` ``middleBlock``.

:mps:tag:`impl.callback.delete.delete` The ``delete`` callback will be
called in this case if and only if::

    left + middle + right >= minSize  &&  left < minSize  &&  right < minSize

That is, the combined range is interesting, but neither remaining
fragment is. It will be called with the following parameters:

* ``block``: ``middleBlock``
* ``oldSize``: ``left + middle + right``
* ``newSize``: ``left >= right ? left : right``
 
:mps:tag:`impl.callback.delete.new` The ``new`` callback will be
called in this case if and only if::

    left >= minSize  &&  right >= minSize

That is, both remaining fragments are interesting. It will be called
with the following parameters:

* ``block``: a new block
* ``oldSize``: ``0``
* ``newSize``: ``left >= right ? right : left``

:mps:tag:`impl.callback.delete.shrink` The shrink callback will be
called in this case if and only if::

    left + middle + right >= minSize && (left >= minSize || right >= minSize)

That is, at least one of the remaining fragments is still interesting.  It will be called with the following parameters:

* ``block``: ``middleBlock``
* ``oldSize``: ``left + middle + right``
* ``newSize``: ``left >= right ? left : right``

:mps:tag:`impl.callback.insert` Consider the case when the minimum
size is ``minSize``, and :c:func:`CBSInsert` is called to add a range
of size ``middle``. The two (possibly non-existant) neighbouring
blocks are ``leftBlock`` and ``rightBlock``, and have (possibly zero)
sizes ``left`` and ``right``.

:mps:tag:`impl.callback.insert.delete` The ``delete`` callback will be
called in this case if and only if:

    left >= minSize  &&  right >= minSize

That is, both neighbours were interesting. It will be called with the
following parameters:

* ``block``: ``left >= right ? rightBlock : leftBlock``
* ``oldSize``: ``left >= right ? right : left``
* ``newSize``: ``0``

:mps:tag:`impl.callback.insert.new` The ``new`` callback will be
called in this case if and only if:

    left + middle + right >= minSize  &&  left < minSize  &&  right < minSize

That is, the combined block is interesting, but neither neighbour was.
It will be called with the following parameters:

* ``block``: ``left >= right ? leftBlock : rightBlock``
* ``oldSize``: ``left >= right ? left : right``
* ``newSize``: ``left + middle + right``

:mps:tag:`impl.callback.insert.grow` The ``grow`` callback will be
called in this case if and only if::

    left + middle + right >= minSize && (left >= minSize || right >= minSize)

That is, at least one of the neighbours was interesting. It will be
called with the following parameters:

* ``block``: ``left >= right ? leftBlock : rightBlock``
* ``oldSize``: ``left >= right ? left : right``
* ``newSize``: ``left + middle + right``


Splay tree
..........

:mps:tag:`impl.splay` The CBS is principally implemented using a splay
tree (see :mps:ref:`design.mps.splay`). Each splay tree node is
embedded in a CBSBlock that represents a semi-open address range. The
key passed for comparison is the base of another range.

:mps:tag:`impl.splay.fast-find` :c:func:`CBSFindFirst` and
:c:func:`CBSFindLast` use the update/refresh facility of splay trees
to store, in each :c:type:`CBSBlock`, an accurate summary of the
maximum block size in the tree rooted at the corresponding splay node.
This allows rapid location of the first or last suitable block, and
very rapid failure if there is no suitable block.

:mps:tag:`impl.find-largest` :c:func:`CBSFindLargest` simply finds out
the size of the largest block in the CBS from the root of the tree
(using :c:func:`SplayRoot`), and does :c:func:`SplayFindFirst` for a
block of that size. This is O(log(*n*)) in the size of the free list,
so it's about the best you can do without maintaining a separate
priority queue, just to do :c:func:`CBSFindLargest`. Except when the
emergency lists (see :mps:ref:`.impl.low-mem`) are in use, they are
also searched.


Low memory behaviour
....................

:mps:tag:`impl.low-mem` Low memory situations cause problems when the
CBS tries to allocate a new :c:type:`CBSBlock` structure for a new
isolated range as a result of either :c:func:`CBSInsert` or
:c:func:`CBSDelete`, and there is insufficient memory to allocation
the :c:type:`CBSBlock` structure:

:mps:tag:`impl.low-mem.no-inline` If ``mayUseInline`` is ``FALSE``,
then the range is not added to the CBS, and the call to
:c:func:`CBSInsert` or :c:func:`CBSDelete` returns ``ResMEMORY``.

:mps:tag:`impl.low-mem.inline` If ``mayUseInline`` is ``TRUE``:

:mps:tag:`impl.low-mem.inline.block` If the range is large enough to
contain an inline block descriptor consisting of two pointers, then it
is kept on an emergency block list. The CBS will eagerly attempt to
add this block back into the splay tree during subsequent calls to
:c:func:`CBSInsert` and :c:func:`CBSDelete`. The CBS will also keep
its emergency block list in address order, and will coalesce this list
eagerly. Some performance degradation will be seen when the emergency
block list is in use. Ranges on this emergency block list will not be
made available to the CBS's client via callbacks. :c:func:`CBSIterate`
and :c:func:`CBSIterateLarge` will not iterate over ranges on this
list.

:mps:tag:`impl.low-mem.inline.block.structure` The two pointers stored
are to the next such block (or ``NULL``), and to the limit of the
block, in that order.

:mps:tag:`impl.low-mem.inline.grain` Otherwise, the range must be
large enough to contain an inline grain descriptor consisting of one
pointer, then it is kept on an emergency grain list. The CBS will
eagerly attempt to add this grain back into either the splay tree or
the emergency block list during subsequent calls to
:c:func:`CBSInsert` and :c:func:`CBSDelete`. The CBS will also keep
its emergency grain list in address order. Some performance
degradation will be seen when the emergency grain list is in use.
Ranges on this emergency grain list will not be made available to the
CBS's client via callbacks. :c:func:`CBSIterate` and
:c:func:`CBSIterateLarge` will not iterate over ranges on this list.

:mps:tag:`impl.low-mem.inline.grain.structure` The pointer stored is
to the next such grain, or ``NULL``.


The CBS block
.............

:mps:tag:`impl.cbs.block` The block contains a base-limit pair and a
splay tree node.

:mps:tag:`impl.cbs.block.special` The base and limit may be equal if
the block is halfway through being deleted.

:mps:tag:`impl.cbs.block.special.just` This conflates values and
status, but is justified because block size is very important.


Testing
-------

:mps:tag:`test` The following testing will be performed on this module:

:mps:tag:`test.cbstest` There is a stress test for this module in
:mps:ref:`impl.c.cbstest`. This allocates a large block of memory and
then simulates the allocation and deallocation of ranges within this
block using both a :c:type:`CBS` and a :c:type:`BT`. It makes both
valid and invalid requests, and compares the :c:type:`CBS` response to
the correct behaviour as determined by the :c:type:`BT`. It also
iterates the ranges in the :c:type:`CBS`, comparing them to the
:c:type:`BT`. It also invokes the :c:func:`CBSDescribe` method, but
makes no automatic test of the resulting output. It does not currently
test the callbacks.

:mps:tag:`test.pool` Several pools (currently :ref:`pool-mvt` and
:ref:`pool-mvff`) are implemented on top of a CBS. These pool are
subject to testing in development, QA, and are/will be heavily
exercised by customers.


Notes for future development
----------------------------

:mps:tag:`future.not-splay` The initial implementation of CBSs is
based on splay trees. It could be revised to use any other data
structure that meets the requirements (especially
:mps:ref:`.req.fast`).

:mps:tag:`future.hybrid` It would be possible to attenuate the problem
of :mps:ref:`.risk.overhead` (below) by using a single word bit set to
represent the membership in a (possibly aligned) word-width of grains.
This might be used for block sizes less than a word-width of grains,
converting them when they reach all free in the bit set. Note that
this would make coalescence slightly less eager, by up to
``(word-width - 1)``.


Risks
-----

:mps:tag:`risk.overhead` Clients should note that the current
implementation of CBSs has a space overhead proportional to the number
of isolated contiguous ranges. [Four words per range.] If the CBS
contains every other grain in an area, then the overhead will be large
compared to the size of that area. [Four words per two grains.] See
:mps:ref:`.future.hybrid` for a suggestion to solve this problem. An
alternative solution is to use CBSs only for managing long ranges.


Proposed hybrid implementation
------------------------------

.. note::

    The following relates to a pending re-design and does not yet
    relate to any working source version. GavinM 1998-09-25

The CBS system provides its services by combining the services
provided by three subsidiary CBS modules:

- ``CBSST`` -- Splay Tree: Based on out-of-line splay trees; must
  allocate to insert isolated, which may therefore fail.

- ``CBSBL`` -- Block List: Based on a singly-linked list of variable
  sized ranges with inline descriptors; ranges must be at least large
  enough to store the inline descriptor.

- ``CBSGL`` -- Grain List: Based on a singly-linked list of fixed size
  ranges with inline descriptors; the ranges must be the alignment of
  the CBS.

The three sub-modules have a lot in common. Although their methods are
not invoked via a dispatcher, they have been given consistent
interfaces, and consistent internal appearance, to aid maintenance.

Methods supported by sub-modules (not all sub-modules support all
methods):

- ``MergeRange`` -- Finds any ranges in the specific CBS adjacent to
  the supplied one. If there are any, it extends the ranges, possibly
  deleting one of them. This cannot fail, but should return ``FALSE``
  if there is an intersection between the supplied range and a range
  in the specific CBS.

- ``InsertIsolatedRange`` -- Adds a range to the specific CBS that is
  not adjacent to any range already in there. Depending on the
  specific CBS, this may be able to fail for allocation reasons, in
  which case it should return ``FALSE``. It should :c:func:`AVER` if
  the range is adjacent to or intersects with a range already there.

- ``RemoveAdjacentRanges`` -- Finds and removes from the specific CBS
  any ranges that are adjacent to the supplied range. Should return
  ``FALSE`` if the supplied range intersects with any ranges already
  there.

- ``DeleteRange`` -- Finds and deletes the supplied range from the
  specific CBS. Returns a tri-state result:

  - ``Success`` -- The range was successfully deleted. This may have
    involved the creation of a new range, which should be done via
    ``CBSInsertIsolatedRange``.

  - ``ProtocolError`` -- Either some non-trivial strict subset of the
    supplied range was in the specific CBS, or a range adjacent to the
    supplied range was in the specific CBS. Either of these indicates
    a protocol error.

  - ``NoIntersection`` -- The supplied range was not found in the CBS.
    This may or not be a protocol error, depending on the invocation
    context.

- ``FindFirst`` -- Returns the first (in address order) range in the
  specific CBS that is at least as large as the supplied size, or
  ``FALSE`` if there is no such range.

- ``FindFirstBefore`` -- As ``FindFirst``, but only finds ranges prior
  to the supplied address.

- ``FindLast`` -- As ``FindFirst``, but finds the last such range in
  address order.

- ``FindLastAfter`` -- ``FindLast`` equivalent of ``FindFirstBefore``.

- ``Init`` -- Initialise the control structure embedded in the CBS.

- ``Finish`` -- Finish the control structure embedded in the CBS.

- ``InlineDescriptorSize`` -- Returns the aligned size of the inline descriptor.

- ``Check`` -- Checks the control structure embedded in the CBS.

The CBS supplies the following utilities:

- ``CBSAlignment`` -- Returns the alignment of the CBS.

- ``CBSMayUseInline`` -- Returns whether the CBS may use the memory in
  the ranges stored.

- ``CBSInsertIsolatedRange`` -- Wrapper for ``CBS*InsertIsolatedRange``.

Internally, the ``CBS*`` sub-modules each have an internal structure
``CBS*Block`` that represents an isolated range within the module. It
supports the following methods (for sub-module internal use):

- ``BlockBase`` -- Returns the base of the associated range;
- ``BlockLimit`` 
- ``BlockRange``
- ``BlockSize``

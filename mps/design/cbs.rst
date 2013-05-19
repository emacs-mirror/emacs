Coalescing block structure
==========================

:Tag: design.mps.cbs
:Author: Gavin Matthews
:Data: 1998-05-01
:Status: incomplete design
:Revision: $Id$
:Copyright: See section `C. Copyright and License`_.


Introduction
------------

_`intro`: This is the design for impl.c.cbs, which implements a data
structure for the management of non-intersecting memory ranges, with
eager coalescence.

_`readership`: This document is intended for any MM developer.

_`source`: design.mps.poolmv2, design.mps.poolmvff.

_`overview`: The "coalescing block structure" is a set of addresses
(or a subset of address space), with provision for efficient
management of contiguous ranges, including insertion and deletion,
high level communication with the client about the size of contiguous
ranges, and detection of protocol violations.


Definitions
-----------

_`def.range`: A (contiguous) *range* of addresses is a semi-open
interval on address space.

_`def.isolated`: A contiguous range is *isolated* with respect to
some property it has, if adjacent elements do not have that property.

_`def.interesting`: A block is *interesting* if it is of at least
the minimum interesting size specified by the client.


Requirements
------------

_`req.set`: Must maintain a set of addresses.

_`req.fast`: Common operations must have a low amortized cost.

_`req.add`: Must be able to add address ranges to the set.

_`req.remove`: Must be able to remove address ranges from the set.

_`req.size`: Must report concisely to the client when isolated
contiguous ranges of at least a certain size appear and disappear.

_`req.iterate`: Must support the iteration of all isolated
contiguous ranges. This will not be a common operation.

_`req.protocol`: Must detect protocol violations.

_`req.debug`: Must support debugging of client code.

_`req.small`: Must have a small space overhead for the storage of
typical subsets of address space and not have abysmal overhead for the
storage of any subset of address space.

_`req.align`: Must support an alignment (the alignment of all
addresses specifying ranges) of down to ``sizeof(void *)`` without
losing memory.


Interface
---------

_`header`: CBS is used through impl.h.cbs.


External types
..............

.. c:type:: typedef struct CBSStruct CBSStruct, *CBS;

_`type.cbs`: ``CBS`` is the main datastructure for manipulating a CBS.
It is intended that a ``CBSStruct`` be embedded in another structure.
No convenience functions are provided for the allocation or
deallocation of the CBS.

.. c:type:: typedef struct CBSBlockStruct CBSBlockStruct, *CBSBlock;

_`type.cbs.block`: ``CBSBlock`` is the data structure that represents
an isolated contiguous range held by the CBS. It is returned by the
new and delete methods described below.

_`type.cbs.method`: The following methods are provided as callbacks to
advise the client of certain events. The implementation of these
functions should not cause any CBS function to be called on the same
CBS. In this respect, the CBS module is not re-entrant.

.. c:type:: typedef void (*CBSChangeSizeMethod)(CBS cbs, CBSBlock block, Size oldSize, SizeNewSize);

_`type.cbs.change.size.method`: ``CBSChangeSizeMethod`` is the
function pointer type, four instances of which are optionally
registered via CBSInit.

These callbacks are invoked under ``CBSInsert``, ``CBSDelete``, or
``CBSSetMinSize`` in certain circumstances. Unless otherwise stated,
``oldSize`` and ``newSize`` will both be non-zero, and different. The
accessors ``CBSBlockBase``, ``CBSBlockLimit``, and ``CBSBlockSize``
may be called from within these callbacks, except within the delete
callback when ``newSize`` is zero. See `.impl.callback`_ for
implementation details.

.. c:type:: typedef Bool (*CBSIterateMethod)(CBS cbs, CBSBlock block, void *closureP, unsigned long closureS);

_`type.cbs.iterate.method`: ``CBSIterateMethod`` is a function pointer
type for a client method invoked by the CBS module for every isolated
contiguous range in address order, when passed to the ``CBSIterate``
or ``CBSIterateLarge`` functions. The function returns a boolean
indicating whether to continue with the iteration.


External functions
..................

.. c:function::  Res CBSInit(Arena arena, CBS cbs, CBSChangeSizeMethod new, CBSChangeSizeMethod delete, CBSChangeSizeMethod grow, CBSChangeSizeMethod shrink, Size minSize, Align alignment, Bool mayUseInline)

_`function.cbs.init`: ``CBSInit`` is the function that initialises the
CBS structure. It performs allocation in the supplied arena. Four
methods are passed in as function pointers (see `.type.*`_ above), any
of which may be ``NULL``. It receives a minimum size, which is used
when determining whether to call the optional methods. The
``mayUseInline`` Boolean indicates whether the CBS may use the memory
in the ranges as a low-memory fallback (see `.impl.low-mem`_). The
alignment indicates the alignment of ranges to be maintained. An
initialised CBS contains no ranges.

_`function.cbs.init.may-use-inline`: If ``mayUseInline`` is set, then
``alignment`` must be at least ``sizeof(void *)``. In this mode, the
CBS will never fail to insert or delete ranges, even if memory for
control structures becomes short. Note that, in such cases, the CBS
may defer notification of new/grow events, but will report available
blocks in ``CBSFindFirst`` and ``CBSFindLast``. Such low memory
conditions will be rare and transitory. See `.align`_ for more
details.

.. c:function:: void CBSFinish(CBS cbs)

_`function.cbs.finish`: ``CBSFinish`` is the function that finishes
the CBS structure and discards any other resources associated with the
CBS.

.. c:function:: Res CBSInsert(CBS cbs, Addr base, Addr limit)

_`function.cbs.insert`: ``CBSInsert`` is the function used to add a
contiguous range specified by ``[base,limit)`` to the CBS. If any part
of the range is already in the CBS, then ``ResFAIL`` is returned, and
the CBS is unchanged. This function may cause allocation; if this
allocation fails, and any contingency mechanism fails, then
``ResMEMORY`` is returned, and the CBS is unchanged.

_`function.cbs.insert.callback`: ``CBSInsert`` will invoke callbacks as follows:

* ``new``: when a new block is created that is interesting. ``oldSize
  == 0; newSize >= minSize``.

* ``new``: when an uninteresting block coalesces to become
  interesting. ``0 < oldSize < minSize <= newSize``.

* ``delete``: when two interesting blocks are coalesced. ``grow`` will
  also be invoked in this case on the larger of the two blocks.
  ``newSize == 0; oldSize >= minSize``.

* ``grow``: when an interesting block grows in size. ``minSize <=
  oldSize < newSize``.
 
.. c:function::  Res CBSDelete(CBS cbs, Addr base, Addr limit)

_`function.cbs.delete`: ``CBSDelete`` is the function used to remove a
contiguous range specified by ``[base,limit)`` from the CBS. If any
part of the range is not in the CBS, then ``ResFAIL`` is returned, and
the CBS is unchanged. This function may cause allocation; if this
allocation fails, and any contingency mechanism fails, then
``ResMEMORY`` is returned, and the CBS is unchanged.

_`function.cbs.delete.callback`: ``CBSDelete`` will
invoke callbacks as follows:

* ``delete``: when an interesting block is entirely removed. ``newSize
  == 0; oldSize >= minSize``.
* ``delete``: when an interesting block becomes uninteresting. ``0 <
  newSize < minSize <= oldSize``.
* ``new``: when a block is split into two blocks, both of which are
  interesting. ``shrink`` will also be invoked in this case on the
  larger of the two blocks. ``oldSize == 0; newSize >= minSize``.
* ``shrink``: when an interesting block shrinks in size, but remains
  interesting. ``minSize <= newSize < oldSize``.

.. c:function:: void CBSIterate(CBS cbs, CBSIterateMethod iterate, void *closureP, unsigned long closureS)

_`function.cbs.iterate`: ``CBSIterate`` is the function used to
iterate all isolated contiguous ranges in a CBS. It receives a
pointer, unsigned long closure pair to pass on to the iterator method,
and an iterator method to invoke on every range in address order. If
the iterator method returns ``FALSE``, then the iteration is
terminated.

.. c:function:: void CBSIterateLarge(CBS cbs, CBSIterateMethod iterate, void *closureP, unsigned long closureS)

_`function.cbs.iterate.large`: ``CBSIterateLarge`` is the function
used to iterate all isolated contiguous ranges of size greater than or
equal to the client indicated minimum size in a CBS. It receives a
pointer, unsigned long closure pair to pass on to the iterator method,
and an iterator method to invoke on every large range in address
order. If the iterator method returns ``FALSE``, then the iteration is
terminated.

.. c:function:: void CBSSetMinSize(CBS cbs, Size minSize)

_`function.cbs.set.min-size`: ``CBSSetMinSize`` is the function used
to change the minimum size of interest in a CBS. This minimum size is
used to determine whether to invoke the client callbacks from
``CBSInsert`` and ``CBSDelete``. This function will invoke either the
``new`` or ``delete`` callback for all blocks that are (in the
semi-open interval) between the old and new values. ``oldSize`` and
``newSize`` will be the same in these cases.

.. c:function:: Res CBSDescribe(CBS cbs, mps_lib_FILE *stream)

_`function.cbs.describe`: ``CBSDescribe`` is a function that prints a
textual representation of the CBS to the given stream, indicating the
contiguous ranges in order, as well as the structure of the underlying
splay tree implementation. It is provided for debugging purposes only.

.. c:function:: Addr CBSBlockBase(CBSBlock block)

_`function.cbs.block.base`: The ``CBSBlockBase`` function returns the
base of the range represented by the ``CBSBlock``. This function may
not be called from the delete callback when the block is being deleted
entirely.

.. note::

    The value of the base of a particular ``CBSBlock`` is not
    guaranteed to remain constant across calls to ``CBSDelete`` and
    ``CBSInsert``, regardless of whether a callback is invoked.

.. c:function:: Addr CBSBlockLimit(CBSBlock block)

_`function.cbs.block.limit`: The ``CBSBlockLimit`` function returns
the limit of the range represented by the ``CBSBlock``. This function
may not be called from the delete callback when the block is being
deleted entirely.

.. note::

    The value of the limit of a particular ``CBSBlock`` is not
    guaranteed to remain constant across calls to ``CBSDelete`` and
    ``CBSInsert``, regardless of whether a callback is invoked.

.. c:function:: Size CBSBlockSize(CBSBlock block)

_`function.cbs.block.size`: The ``CBSBlockSize`` function returns the
size of the range represented by the ``CBSBlock``. This function may
not be called from the ``delete`` callback when the block is being
deleted entirely.

.. note::

    The value of the size of a particular ``CBSBlock`` is not
    guaranteed to remain constant across calls to ``CBSDelete`` and
    ``CBSInsert``, regardless of whether a callback is invoked.

.. c:function:: Res CBSBlockDescribe(CBSBlock block, mps_lib_FILE *stream)

_`function.cbs.block.describe`: The ``CBSBlockDescribe`` function
prints a textual representation of the ``CBSBlock`` to the given
stream. It is provided for debugging purposes only.

.. c:function:: Bool CBSFindFirst(Addr *baseReturn, Addr *limitReturn, CBS cbs, Size size, CBSFindDelete findDelete)

_`function.cbs.find.first`: The ``CBSFindFirst`` function locates the
first block (in address order) within the CBS of at least the
specified size, and returns its range. If there are no such blocks, it
returns ``FALSE``. It optionally deletes the top, bottom, or all of
the found range, depending on the ``findDelete`` argument (this saves
a separate call to ``CBSDelete``, and uses the knowledge of exactly
where we found the range), which must come from this enumeration::

    enum {
        CBSFindDeleteNONE,    /* don't delete after finding */
        CBSFindDeleteLOW,     /* delete precise size from low end */
        CBSFindDeleteHIGH,    /* delete precise size from high end */
        CBSFindDeleteENTIRE   /* delete entire range */
    };

.. c:function:: Bool CBSFindLast(Addr *baseReturn, Addr *limitReturn, CBS cbs, Size size, CBSFindDelete findDelete)

_`function.cbs.find.last`: The ``CBSFindLast`` function locates the
last block (in address order) within the CBS of at least the specified
size, and returns its range. If there are no such blocks, it returns
``FALSE``. Like ``CBSFindFirst``, it optionally deletes the range.

.. c:function:: Bool CBSFindLargest(Addr *baseReturn, Addr *limitReturn, CBS cbs, CBSFindDelete findDelete)

_`function.cbs.find.largest`: The ``CBSFindLargest`` function locates
the largest block within the CBS, and returns its range. If there are
no blocks, it returns ``FALSE``. Like ``CBSFindFirst``, it optionally
deletes the range (specifying ``CBSFindDeleteLOW`` or
``CBSFindDeleteHIGH`` has the same effect as ``CBSFindDeleteENTIRE``).


Alignment
---------

_`align`: When ``mayUseInline`` is specified to permit inline data
structures and hence avoid losing memory in low memory situations, the
alignments that the CBS supports are constrained by three
requirements:

- The smallest possible range (namely one that is the alignment in
  size) must be large enough to contain a single ``void *`` pointer
  (see `.impl.low-mem.inline.grain`_);

- Any larger range (namely one that is at least twice the alignment in
  size) must be large enough to contain two ``void *`` pointers (see
  `.impl.low-mem.inline.block`_);

- It must be valid on all platforms to access a ``void *`` pointer
  stored at the start of an aligned range.

All alignments that meet these requirements are aligned to
``sizeof(void *)``, so we take that as the minimum alignment.


Implementation
--------------

_`impl`: Note that this section is concerned with describing various
aspects of the implementation. It does not form part of the interface
definition.


Size change callback protocol
.............................

_`impl.callback`: The size change callback protocol concerns the
mechanism for informing the client of the appearance and disappearance
of interesting ranges. The intention is that each range has an
identity (represented by the ``CBSBlock``). When blocks are split, the
larger fragment retains the identity. When blocks are merged, the new
block has the identity of the larger fragment.

_`impl.callback.delete`: Consider the case when the minimum size is
``minSize``, and ``CBSDelete`` is called to remove a range of size
``middle``. The two (possibly non-existant) neighbouring ranges have
(possibly zero) sizes ``left`` and ``right``. ``middle`` is part of
the ``CBSBlock`` ``middleBlock``.

_`impl.callback.delete.delete`: The ``delete`` callback will be called
in this case if and only if::

    left + middle + right >= minSize  &&  left < minSize  &&  right < minSize

That is, the combined range is interesting, but neither remaining
fragment is. It will be called with the following parameters:

* ``block``: ``middleBlock``
* ``oldSize``: ``left + middle + right``
* ``newSize``: ``left >= right ? left : right``
 
_`impl.callback.delete.new`: The ``new`` callback will be called in
this case if and only if::

    left >= minSize  &&  right >= minSize

That is, both remaining fragments are interesting. It will be called
with the following parameters:

* ``block``: a new block
* ``oldSize``: ``0``
* ``newSize``: ``left >= right ? right : left``

_`impl.callback.delete.shrink`: The shrink callback will be called in
this case if and only if::

    left + middle + right >= minSize && (left >= minSize || right >= minSize)

That is, at least one of the remaining fragments is still interesting.
It will be called with the following parameters:

* ``block``: ``middleBlock``
* ``oldSize``: ``left + middle + right``
* ``newSize``: ``left >= right ? left : right``

_`impl.callback.insert`: Consider the case when the minimum size is
``minSize``, and ``CBSInsert`` is called to add a range of size
``middle``. The two (possibly non-existant) neighbouring blocks are
``leftBlock`` and ``rightBlock``, and have (possibly zero) sizes
``left`` and ``right``.

_`impl.callback.insert.delete`: The ``delete`` callback will be called
in this case if and only if:

    left >= minSize  &&  right >= minSize

That is, both neighbours were interesting. It will be called with the
following parameters:

* ``block``: ``left >= right ? rightBlock : leftBlock``
* ``oldSize``: ``left >= right ? right : left``
* ``newSize``: ``0``

_`impl.callback.insert.new`: The ``new`` callback will be called in
this case if and only if:

    left + middle + right >= minSize  &&  left < minSize  &&  right < minSize

That is, the combined block is interesting, but neither neighbour was.
It will be called with the following parameters:

* ``block``: ``left >= right ? leftBlock : rightBlock``
* ``oldSize``: ``left >= right ? left : right``
* ``newSize``: ``left + middle + right``

_`impl.callback.insert.grow`: The ``grow`` callback will be called in
this case if and only if::

    left + middle + right >= minSize && (left >= minSize || right >= minSize)

That is, at least one of the neighbours was interesting. It will be
called with the following parameters:

* ``block``: ``left >= right ? leftBlock : rightBlock``
* ``oldSize``: ``left >= right ? left : right``
* ``newSize``: ``left + middle + right``


Splay tree
..........

_`impl.splay`: The CBS is principally implemented using a splay tree
(see design.mps.splay). Each splay tree node is embedded in a CBSBlock
that represents a semi-open address range. The key passed for
comparison is the base of another range.

_`impl.splay.fast-find`: ``CBSFindFirst`` and ``CBSFindLast`` use the
update/refresh facility of splay trees to store, in each ``CBSBlock``,
an accurate summary of the maximum block size in the tree rooted at
the corresponding splay node. This allows rapid location of the first
or last suitable block, and very rapid failure if there is no suitable
block.

_`impl.find-largest`: ``CBSFindLargest`` simply finds out the size of
the largest block in the CBS from the root of the tree (using
``SplayRoot``), and does ``SplayFindFirst`` for a block of that size.
This is O(log(*n*)) in the size of the free list, so it's about the
best you can do without maintaining a separate priority queue, just to
do ``CBSFindLargest``.


Low memory behaviour
....................

_`impl.low-mem`: When the CBS tries to allocate a new ``CBSBlock``
structure for a new isolated range as a result of either ``CBSInsert``
or ``CBSDelete``, and there is insufficient memory to allocation the
``CBSBlock`` structure, then the range is not added to the CBS or
deleted from it, and the call to ``CBSInsert`` or ``CBSDelete``
returns ``ResMEMORY``.


The CBS block
.............

_`impl.cbs.block`: The block contains a base-limit pair and a splay
tree node.

_`impl.cbs.block.special`: The base and limit may be equal if the
block is halfway through being deleted.

_`impl.cbs.block.special.just`: This conflates values and status, but
is justified because block size is very important.


Testing
-------

_`test`: The following testing will be performed on this module:

_`test.cbstest`: There is a stress test for this module in
impl.c.cbstest. This allocates a large block of memory and then
simulates the allocation and deallocation of ranges within this block
using both a ``CBS`` and a ``BT``. It makes both valid and invalid
requests, and compares the ``CBS`` response to the correct behaviour
as determined by the ``BT``. It also iterates the ranges in the
``CBS``, comparing them to the ``BT``. It also invokes the
``CBSDescribe`` method, but makes no automatic test of the resulting
output. It does not currently test the callbacks.

_`test.pool`: Several pools (currently :ref:`pool-mvt` and
:ref:`pool-mvff`) are implemented on top of a CBS. These pool are
subject to testing in development, QA, and are/will be heavily
exercised by customers.


Notes for future development
----------------------------

_`future.not-splay`: The initial implementation of CBSs is based on
splay trees. It could be revised to use any other data structure that
meets the requirements (especially `.req.fast`_).

_`future.hybrid`: It would be possible to attenuate the problem of
`.risk.overhead`_ (below) by using a single word bit set to represent
the membership in a (possibly aligned) word-width of grains. This
might be used for block sizes less than a word-width of grains,
converting them when they reach all free in the bit set. Note that
this would make coalescence slightly less eager, by up to
``(word-width - 1)``.


Risks
-----

_`risk.overhead`: Clients should note that the current implementation
of CBSs has a space overhead proportional to the number of isolated
contiguous ranges. [Four words per range.] If the CBS contains every
other grain in an area, then the overhead will be large compared to
the size of that area. [Four words per two grains.] The CBS structure
is thus suitable only for managing large enough ranges.



B. Document history
-------------------

- 1998-05-01 GRM This document was derived from the outline in
  design.mps.poolmv2(2).

- 1998-07-22 GRM Updated in response to approval comments in
  change.epcore.anchovy.160040. There is too much fragmentation in
  trapping memory.

- GRM Updated (as part of change.epcore.brisling.160158: MVFF cannot
  be instantiated with 4-byte alignment) to document new alignment
  restrictions.

- 2002-06-07 RB_ Converted from MMInfo database design document.

- 2013-04-14 GDR_ Converted to reStructuredText.

- 2013-05-19 GDR_ Remove the "emergency" free list allocator and the
  design notes on an unimplemented "future hybrid" scheme.

.. _RB: http://www.ravenbrook.com/consultants/rb/
.. _GDR: http://www.ravenbrook.com/consultants/gdr/


C. Copyright and License
------------------------

Copyright (C) 1998-2013 Ravenbrook Limited. All rights reserved. 
<http://www.ravenbrook.com/>. This is an open source license. Contact
Ravenbrook for commercial licensing options.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Redistributions in any form must be accompanied by information on how
   to obtain complete source code for this software and any
   accompanying software that uses this software.  The source code must
   either be included in the distribution or be available for no more than
   the cost of distribution plus a nominal fee, and must be freely
   redistributable under reasonable conditions.  For an executable file,
   complete source code means the source code for all modules it contains.
   It does not include source code for modules or files that typically
   accompany the major components of the operating system on which the
   executable file runs.

**This software is provided by the copyright holders and contributors
"as is" and any express or implied warranties, including, but not
limited to, the implied warranties of merchantability, fitness for a
particular purpose, or non-infringement, are disclaimed.  In no event
shall the copyright holders and contributors be liable for any direct,
indirect, incidental, special, exemplary, or consequential damages
(including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption)
however caused and on any theory of liability, whether in contract,
strict liability, or tort (including negligence or otherwise) arising in
any way out of the use of this software, even if advised of the
possibility of such damage.**

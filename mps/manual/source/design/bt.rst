.. sources:

    `<https://info.ravenbrook.com/project/mps/master/design/bt/>`_

.. mps:prefix:: design.mps.bt

Bit tables
==========

Introduction
------------

:mps:tag:`readership` Any MPS developer.

:mps:tag:`intro` This is the design of the Bit Tables module. A Bit
Table is a linear array of bits. A Bit Table of length *n* is indexed
using an integer from 0 up to (but not including) *n*. Each bit in a
Bit Table can hold either the value 0 (aka ``FALSE``) or 1 (aka
``TRUE``). A variety of operations are provided including: set, reset,
and retrieve, individual bits; set and reset a contiguous range of
bits; search for a contiguous range of reset bits; making a "negative
image" copy of a range.


History
-------

:mps:tag:`hist.0-3` The history for versions 0-3 is lost pending
possible reconstruction. David Jones, 1997-03-04.

:mps:tag:`hist.4` Prepared for review. Added full requirements
section. Made notation more consistent throughout. Documented all
functions. David Jones, 1999-04-29.

:mps:tag:`hist.5` Converted from MMInfo database design document.
Richard Brooksby, 2002-06-07.

:mps:tag:`hist.6` Converted to reStructuredText. Gareth Rees,
2013-03-12.


Definitions
-----------

:mps:tag:`def.set` **Set**

    Used as a verb meaning to assign the value 1 or ``TRUE`` to a bit.
    Used descriptively to denote a bit containing the value 1. Note 1
    and ``TRUE`` are synonyms in MPS C code (see
    :mps:ref:`design.mps.type(0).bool.value`).

:mps:tag:`def.reset` **Reset**

    Used as a verb meaning to assign the value 0 or ``FALSE`` to a
    bit. Used descriptively to denote a bit containing the value 0.
    Note 0 and ``FALSE`` are synonyms in MPS C code (see
    :mps:ref:`design.mps.type(0).bool.value`).

.. note::

    Consider using "fill/empty" or "mark/clear" instead of
    "set/reset", set/reset is probably a hangover from drj's z80
    hacking days -- drj 1999-04-26

:mps:tag:`def.bt` **Bit Table**

    A Bit Table is a mapping from [0, *n*) to {0,1} for some *n*,
    represented as a linear array of bits.

    :mps:tag:`.def.bt.justify` They are called *bit tables* because a
    single bit is used to encode whether the image of a particular
    integer under the map is 0 or 1.

:mps:tag:`def.range` **Range**

    A contiguous sequence of bits in a Bit Table. Ranges are typically
    specified as a *base*--*limit* pair where the range includes the
    position specified by the base, but excludes that specified by the
    limit. The mathematical interval notation for half-open intervals,
    [*base*, *limit*), is used.



Requirements
------------

:mps:tag:`req.bit` The storage for a Bit Table of *n* bits shall take
no more than a small constant addition to the storage required for *n*
bits. :mps:tag:`.req.bit.why` This is so that clients can make some
predictions about how much storage their algorithms use. A small
constant is allowed over the minimal for two reasons: inevitable
implementation overheads (such as only being able to allocate storage
in multiples of 32 bits), extra storage for robustness or speed (such
as signature and length fields).

:mps:tag:`req.create` A means to create Bit Tables.
:mps:tag:`req.create.why` Obvious.

:mps:tag:`req.destroy` A means to destroy Bit Tables.
:mps:tag:`req.destroy.why` Obvious.

:mps:tag:`req.ops` The following operations shall be supported:

* :mps:tag:`req.ops.get` **Get**.  Get the value of a bit at a specified
  index.

* :mps:tag:`req.ops.set` **Set**.  Set a bit at a specified index.

* :mps:tag:`req.ops.reset` **Reset**.  Reset a bit at a specified index.

:mps:tag:`req.ops.minimal.why` Get, Set, Reset, are the minimal
operations. All possible mappings can be created and inspected using
these operations.

* :mps:tag:`req.ops.set.range` **SetRange**. Set a range of bits.
  :mps:tag:`req.ops.set.range.why` It's expected that clients will
  often want to set a range of bits; providing this operation allows
  the implementation of the BT module to make the operation efficient.

* :mps:tag:`req.ops.reset.range` **ResetRange**. Reset a range of
  bits. :mps:tag:`req.ops.reset.range.why` as for SetRange, see
  :mps:ref:`.req.ops.set.range.why`.

* :mps:tag:`req.ops.test.range.set` **IsSetRange**. Test whether a range
  of bits are all set. :mps:tag:`req.ops.test.range.set.why` Mostly
  for checking. For example, often clients will know that a range they
  are about to reset is currently all set, they can use this operation
  to assert that fact.

* :mps:tag:`req.ops.test.range.reset` **IsResetRange**. Test whether a
  range of bits are all reset. :mps:tag:`req.ops.test.range.reset.why`
  As for IsSetRange, see :mps:ref:`.req.ops.test.range.set.why`.

* :mps:tag:`req.ops.find` Find a range (which we'll denote [*i*, *j*))
  of at least *L* reset bits that lies in a specified subrange of the
  entire Bit Table. Various find operations are required according to
  the (additional) properties of the required range:

  * :mps:tag:`req.ops.find.short.low` **FindShortResetRange**. Of all
    candidate ranges, find the range with least *j* (find the leftmost
    range that has at least *L* reset bits and return just enough of
    that). :mps:tag:`req.ops.find.short.low.why` Required by client
    and VM arenas to allocate segments. The arenas implement definite
    placement policies (such as lowest addressed segment first) so
    they need the lowest (or highest) range that will do. It's not
    currently useful to allocate segments larger than the requested
    size, so finding a short range is sufficient.

  * :mps:tag:`req.ops.find.short.high` **FindShortResetRangeHigh**. Of
    all candidate ranges, find the range with greatest *i* (find the
    rightmost range that has at least *L* reset bits and return just
    enough of that). :mps:tag:`req.ops.find.short.high.why` Required
    by arenas to implement a specific segment placement policy
    (highest addressed segment first).

  * :mps:tag:`req.ops.find.long.low` **FindLongResetRange**. Of all
    candidate ranges, identify the ranges with least *i* and of those
    find the one with greatest *j* (find the leftmost range that has
    at least *L* reset bits and return all of it).
    :mps:tag:`req.ops.find.long.low.why` Required by the mark and
    sweep Pool Classes (AMS, AWL, LO) for allocating objects (filling
    a buffer). It's more efficient to fill a buffer with as much
    memory as is conveniently possible. There's no strong reason to
    find the lowest range but it's bound to have some beneficial
    (small) cache effect and makes the algorithm more predictable.

  * :mps:tag:`req.ops.find.long.high` **FindLongResetRangeHigh**.
    Provided, but not required, see
    :mps:ref:`.non-req.ops.find.long.high`.

* :mps:tag:`req.ops.copy` Copy a range of bits from one Bit Table to another Bit Table. Various copy operations are required:

  * :mps:tag:`req.ops.copy.simple` Copy a range of bits from one Bit
    Table to the same position in another Bit Table.
    :mps:tag:`req.ops.copy.why` Required to support copying of the
    tables for the "low" segment during segment merging and splitting,
    for pools using tables (for example, :c:type:`PoolClassAMS`).

  * :mps:tag:`req.ops.copy.offset` Copy a range of bits from one Bit
    Table to an offset position in another Bit Table.
    :mps:tag:`req.ops.copy.why` Required to support copying of the
    tables for the "high" segment during segment merging and
    splitting, for pools which support this (currently none, as of
    2000-01-17).

  * :mps:tag:`req.ops.copy.invert` Copy a range of bits from one Bit
    Table to the same position in another Bit Table inverting all the
    bits in the target copy. :mps:tag:`req.ops.copy.invert.why`
    Required by colour manipulation code in :c:type:`PoolClassAMS` and
    :c:type:`PoolClassLO`.

:mps:tag:`req.speed` Operations shall take no more than a few memory
operations per bit manipulated. :mps:tag:`req.speed.why` Any slower
would be gratuitous.

:mps:tag:`req.speed.fast` The following operations shall be very fast:

* :mps:tag:`req.speed.fast.find.short` FindShortResRange (the
  operation used to meet :mps:ref:`.req.ops.find.short.low`)
  FindShortResRangeHigh (the operation used to meet
  :mps:ref:`.req.ops.find.short.high`).

  :mps:tag:`req.speed.fast.find.short.why` These two are used by the
  client arena (design.mps.arena.client) and the VM arena
  (design.mps.arena.vm) for finding segments in page tables. The
  operation will be used sufficiently often that its speed will
  noticeably affect the overall speed of the MPS. They will be called
  with a length equal to the number of pages in a segment. Typical
  values of this length depend on the pool classes used and their
  configuration, but we can expect length to be small (1 to 16)
  usually. We can expect the Bit Table to be populated densely where
  it is populated at all, that is set bits will tend to be clustered
  together in subranges.

* :mps:tag:`req.speed.fast.find.long` FindLongResRange (the operation
  used to meet :mps:ref:`.req.ops.find.long.low`)

  :mps:tag:`req.speed.fast.find.long.why` Used in the allocator for
  :c:type:`PoolClassAWL` (:mps:ref:`design.mps.poolawl(1)`),
  :c:type:`PoolClassAMS` (:mps:ref:`design.mps.poolams(2)`),
  :c:type:`PoolClassEPVM` (:mps:ref:`design.mps.poolepvm(0)`). Of
  these AWL and EPVM have speed requirements. For AWL the length of
  range to be found will be the length of a Dylan table in words.
  According to mail.tony.1999-05-05.11-36(0), only <entry-vector>
  objects are allocated in AWL (though not all <entry-vector> objects
  are allocated in AWL), and the mean length of an <entry-vector>
  object is 486 Words. No data for EPVM alas.

:mps:tag:`req.speed.fast.other.why` We might expect mark and sweep
pools to make use of Bit Tables, the MPS has general requirements to
support efficient mark and sweep pools, so that imposes general speed
requirements on Bit Tables.


Non requirements
----------------

The following are not requirements but the current design could
support them with little modification or does support them. Often they
used to be requirements, but are no longer, or were added
speculatively or experimentally but aren't currently used.

* :mps:tag:`non-req.ops.test.range.same` **RangesSame**. Test whether
  two ranges that occupy the same positions in different Bit Tables
  are the same. This used to be required by :c:type:`PoolClassAMS`,
  but is no longer. Currently (1999-05-04) the functionality still
  exists.

* :mps:tag:`non-req.ops.find.long.high` **FindLongResetRangeHigh**.
  (see :mps:ref:`.req.ops.find`) Of all candidate ranges, identify the
  ranges with greatest *j* and of those find the one with least *i*
  (find the rightmost range that has at least *L* reset bits and
  return all of it). Provided for symmetry but only currently used by
  the BT tests and ``cbstest.c``.


Background
----------

:mps:tag:`background` Originally Bit Tables were used and implemented
by :c:type:`PoolClassLO` (:mps:ref:`design.mps.poollo`). It was
decided to lift them out into a separate module when designing the
Pool to manage Dylan Weak Tables which is also a mark and sweep pool
and will make use of Bit Tables (see :mps:ref:`design.mps.poolawl`).
:mps:tag:`background.analysis` :mps:ref:`analysis.mps.bt(0)` contains
some of the analysis of the design decisions that were and were not
made in this document.


Clients
-------

:mps:tag:`clients` Bit Tables are used throughout the MPS but the
important uses are in the client and VM arenas
(:mps:ref:`design.mps.arena.client(0)` and
:mps:ref:`design.mps.arena.vm(1)`) a bit table is used to record
whether each page is free or not; several pool classes
(:c:type:`PoolClassLO`, :c:type:`PoolClassEPVM`,
:c:type:`PoolClassAMS`) use bit tables to record which locations are
free and also to store colour.


Overview
--------

:mps:tag:`over` Mostly, the design is as simple as possible. The
significant complications are iteration (see :mps:ref:`.iteration`
below) and searching (see :mps:ref:`.fun.find-res-range` below)
because both of these are required to be fast.


Interface
---------

:mps:tag:`if.representation.abstract` A Bit Table is represented by
the type :c:type:`BT`.

:mps:tag:`if.declare` The module declares a type :c:type:`BT` and a
prototype for each of the functions below. The type is declared in
:mps:ref:`impl.h.mpmtypes`, the prototypes are declared in
:mps:ref:`impl.h.mpm`. Some of the functions are in fact implemented
as macros in the usual way
(:mps:ref:`doc.mps.ref-man.if-conv(0).macro.std`).

:mps:tag:`if.general.index` Many of the functions specified below take
indexes. If otherwise unspecified an index must be in the interval
[0, *n*) (note, up to, but not including, *n*) where *n* is the number
of bits in the relevant Bit Table (as passed to the :c:func:`BTCreate`
function).

:mps:tag:`if.general.range` Where a range is specified by two indexes
(*base* and *limit*), the index *base*, which specifies the beginning
of the range, must be in the interval [0, *n*), and the index *limit*,
which specifies the end of the range, must be in the interval [1, *n*]
(note can be *n*), and *base* must be strictly less than *limit*
(empty ranges are not allowed). Sometimes *i* and *j* are used instead
of *base* and *limit*.

.. c:function:: Res BTCreate(BT *btReturn, Arena arena, Count n)

:mps:tag:`if.create` Attempts to create a table of length ``n`` in the
arena control pool, putting the table in ``*btReturn``. Returns
:c:macro:`ResOK` if and only if the table is created OK. The initial
values of the bits in the table are undefined (so the client should
probably call :c:func:`BTResRange` on the entire range before using
the :c:type:`BT`). Meets :mps:ref:`.req.create`.

.. c:function:: void BTDestroy(BT t, Arena arena, Count n)

:mps:tag:`if.destroy` Destroys the table ``t``, which must have been
created with :c:func:`BTCreate`. The value of argument ``n`` must be
same as the value of the argument passed to :c:func:`BTCreate`. Meets
mps:ref:`.req.destroy`.

.. c:function:: size_t BTSize(Count n)

:mps:tag:`if.size` ``BTSize(n)`` returns the number of bytes needed
for a Bit Table of ``n`` bits. :c:func:`BTSize` is a macro, but
``(BTSize)(n)`` will assert if ``n`` exceeds `COUNT_MAX -
MPS_WORD_WIDTH + 1``. This is used by clients that allocate storage
for the :c:type:`BT` themselves. Before :c:func:`BTCreate` and
:c:func:`BTDestroy` were implemented that was the only way to allocate
a Bit Table, but is now deprecated.

.. c:function:: int BTGet(BT t, Index i)

:mps:tag:`if.get` ``BTGet(t, i)`` returns the ``i``-th bit of the table
``t`` (that is, the image of ``i`` under the mapping). Meets
:mps:ref:`.req.ops.get`.

.. c:function:: void BTSet(BT t, Index i)

:mps:tag:`if.set` ``BTSet(t, i)`` sets the ``i``-th bit of the table
``t`` (to 1). ``BTGet(t, i)`` will now return 1. Meets
:mps:ref:`.req.ops.set`.

.. c:function:: void BTRes(BT t, Index i)

:mps:tag:`if.res` ``BTRes(t, i)`` resets the ``i``-th bit of the table
``t`` (to 0). ``BTGet(t, i)`` will now return 0. Meets
:mps:ref:`.req.ops.res`.

.. c:function:: void BTSetRange(BT t, Index base, Index limit)

:mps:tag:`if.set-range` ``BTSetRange(t, base, limit)`` sets the range
of bits [``base``, ``limit``) in the table ``t``. ``BTGet(t, x)`` will
now return 1 for ``base`` ≤ ``x`` < ``limit``. Meets
:mps:ref:`.req.ops.range.set`.

.. c:function:: void BTResRange(BT t, Index base, Index limit)

:mps:tag:`if.res-range` ``BTResRange(t, base, limit)`` resets the
range of bits [``base``, ``limit``) in the table ``t``. ``BTGet(t,
x)`` will now return 0 for ``base`` ≤ ``x`` < ``limit``. Meets :mps:ref:`.req.ops.range.res`.

.. c:function:: Bool BTIsSetRange(BT bt, Index base, Index limit)

:mps:tag:`if.test.range.set` Returns ``TRUE`` if all the bits in the
range [``base``, ``limit``) are set, ``FALSE`` otherwise. Meets
:mps:ref:`.req.ops.test.range.set`.

.. c:function:: Bool BTIsResRange(BT bt, Index base, Index limit)

:mps:tag:`if.test.range.reset` Returns ``TRUE`` if all the bits in the
range [``base``, ``limit``) are reset, ``FALSE`` otherwise. Meets
:mps:ref:`.req.ops.test.range.reset`.

.. c:function:: Bool BTRangesSame(BT BTx, BT BTy, Index base, Index limit);

:mps:tag:`if.test.range.same` returns ``TRUE`` if ``BTGet(BTx,i)``
equals ``BTGet(BTy,i)`` for ``i`` in [``base``, ``limit``), and
``FALSE`` otherwise. Meets :mps:ref:`.non-req.ops.test.range.same`.

:mps:tag:`if.find.general` There are four functions (below) to find
reset ranges. All the functions have the same prototype (for
symmetry)::

    Bool find(Index *baseReturn, Index *limitReturn,
              BT bt,
              Index searchBase, Index searchLimit,
              Count length);

where ``bt`` is the Bit Table in which to search. ``searchBase`` and
``searchLimit`` specify a subset of the Bit Table to use, the
functions will only find ranges that are subsets of [``searchBase``,
``searchLimit``) (when set, ``*baseReturn`` will never be less than
``searchBase`` and ``*limitReturn`` will never be greater than
``searchLimit``). ``searchBase`` and ``searchLimit`` specify a range
that must conform to the general range requirements for a range
[*i*, *j*), as per :mps:ref:`.if.general.range` modified appropriately.
``length`` is the number of contiguous reset bits to find; it must not
be bigger than ``searchLimit - searchBase`` (that would be silly). If
a suitable range cannot be found the function returns ``FALSE`` (0)
and leaves ``*baseReturn`` and ``*limitReturn`` untouched. If a
suitable range is found then the function returns the range's base in
``*baseReturn`` and its limit in ``*limitReturn`` and returns ``TRUE``
(1).

.. c:function:: Bool BTFindShortResRange(Index *baseReturn, Index *limitReturn, BT bt, Index searchBase, Index searchLimit, Count length)

:mps:tag:`if.find-short-res-range` Finds a range of reset bits in the
table, starting at ``searchBase`` and working upwards. This function
is intended to meet :mps:ref:`.req.ops.find.short.low` so it will find
the leftmost range that will do, and never finds a range longer than
the requested length (the intention is that it will not waste time
looking).

.. c:function:: Bool BTFindShortResRangeHigh(Index *baseReturn, Index *limitReturn, BT bt, Index searchBase, Index searchLimit, Count length)

:mps:tag:`if.find-short-res-range-high` Finds a range of reset bits in
the table, starting at ``searchLimit`` and working downwards. This
function is intended to meet :mps:ref:`.req.ops.find.short.high` so it
will find the rightmost range that will do, and never finds a range
longer than the requested length.

.. c:function:: Bool BTFindLongResRange(Index *baseReturn, Index *limitReturn, BT bt, Index searchBase, Index searchLimit, Count length)

:mps:tag:`if.find-long-res-range` Finds a range of reset bits in the
table, starting at ``searchBase`` and working upwards. This function
is intended to meet :mps:ref:`.req.ops.find.long.low` so it will find
the leftmost range that will do and returns all of that range (which
can be longer than the requested length).

.. c:function:: Bool BTFindLongResRangeHigh(Index *baseReturn, Index *limitReturn, BT bt, Index searchBase, Index searchLimit, Count length)

:mps:tag:`if.find-long-res-range-high` Finds a range of reset bits in
the table, starting at ``searchLimit`` and working downwards. This
function is intended to meet :mps:ref:`.req.ops.find.long.high` so it
will find the rightmost range that will do and returns all that range
(which can be longer than the requested length).

.. c:function:: void BTCopyRange(BT fromBT, BT toBT, Index base, Index limit)

:mps:tag:`if.copy-range` Overwrites the ``i``-th bit of ``toBT`` with
the ``i``-th bit of ``fromBT``, for all ``i`` in [``base``, ``limit``).
Meets :mps:ref:`.req.ops.copy.simple`.

.. c:function:: void BTCopyOffsetRange(BT fromBT, BT toBT, Index fromBase, Index fromLimit, Index toBase, Index toLimit)

:mps:tag:`if.copy-offset-range` Overwrites the ``i``-th bit of ``toBT``
with the ``j``-th bit of ``fromBT``, for all ``i`` in [``toBase``,
``toLimit``) and corresponding ``j`` in [``fromBase``, ``fromLimit``).
Each of these ranges must be the same size. This might be
significantly less efficient than :c:func:`BTCopyRange`. Meets
:mps:ref:`.req.ops.copy.offset`.

.. c:function:: void BTCopyInvertRange(BT fromBT, BT toBT, Index base, Index limit)

:mps:tag:`if.copy-invert-range` Overwrites the ``i``-th bit of ``toBT`` with the inverse of the ``i``-th bit of ``fromBT``, for 
all ``i`` in [``base``, ``limit``).  Meets :mps:ref:`.req.ops.copy.invert`.


Detailed design
---------------


Data structures
...............

:mps:tag:`datastructure` Bit Tables will be represented as (a pointer
to) an array of :c:type:`Word`. A plain array is used instead of the
more usual design convention of implementing an abstract data type as
a structure with a signature (see :mps:ref:`guide.impl.c.adt(0)`).
:mps:tag:`datastructure.words.justify` The type :c:type:`Word` is used
as it will probably map to the object that can be most efficiently
accessed on any particular platform.
:mps:tag:`datastructure.non-adt.justify` The usual abstract data type
convention was not followed because (i) The initial design (drj) was
lazy, (ii) Bit Tables are more likely to come in convenient powers of
two with the extra one or two words overhead. However, the loss of
checking is severe. Perhaps it would be better to use the usual
abstract data type style.


Functions
.........

:mps:tag:`fun.size` :c:func:`BTSize`. Since a Bit Table is an array of
:c:type:`Word`, the size of a Bit Table of *n* bits is simply the
number of words that it takes to store *n* bits times the number of
bytes in a word. This is ceiling(n/MPS_WORD_WIDTH)*sizeof(Word).
:mps:tag:`fun.size.justify` Since there can be at most
`MPS_WORD_WIDTH - 1` unused bits in the entire table, this satisfies
:mps:ref:`.req.bit`.

:mps:tag:`index` The designs for the following functions use a
decomposition of a bit-index, ``i``, into two parts, ``iw``, ``ib``.

* :mps:tag:`index.word` ``iw`` is the "word-index" which is the index
  into the word array of the word that contains the bit referred to by
  the bit-index. ``iw = i / MPS_WORD_WIDTH``. Since ``MPS_WORD_WIDTH``
  is a power of two, this is the same as ``iw = i >> MPS_WORD_SHIFT``.
  The latter expression is used in the code.
  :mps:tag:`index.word.justify` The compiler is more likely to
  generate good code without the divide.

* :mps:tag:`index.sub-word` ``ib`` is the "sub-word-index" which is
  the index of the bit referred to by the bit-index in the above word.
  ``ib = i % MPS_WORD_WIDTH``. Since ``MPS_WORD_WIDTH`` is a power of
  two, this is the same as ``ib = i & ~((Word)-1<<MPS_WORD_SHIFT)``.
  The latter expression is used in the code.
  :mps:tag:`index.sub-word.justify` The compiler is more likely to
  generate good code without the modulus.

:mps:tag:`index.justify.dubious` The above justifications are dubious;
gcc 2.7.2 (with -O2) running on a sparc (zaphod) produces identical
code for the following two functions::

    unsigned long f(unsigned long i) {
        return i/32 + i%32;
    }

    unsigned long g(unsigned long i) {
       return (i>>5) + (i&31);
    }

.. c:function:: ACT_ON_RANGE(Index base, Index limit, single_action, bits_action, word_action)
.. c:function:: ACT_ON_RANGE_HIGH(Index base, Index limit, single_action, bits_action, word_action)

:mps:tag:`iteration` Many of the following functions involve iteration
over ranges in a Bit Table. This is performed on whole words rather
than individual bits, whenever possible (to improve speed). This is
implemented internally by the macros :c:func:`ACT_ON_RANGE` and
:c:func:`ACT_ON_RANGE_HIGH` for iterating over the range forwards and
backwards respectively. These macros do not form part of the interface
of the module, but are used extensively in the implementation. The
macros are often used even when speed is not an issue because it
simplifies the implementation and makes it more uniform. The iteration
macros take the parameters ``base``, ``limit``, ``single_action``, ``bits_action``, and ``word_action``:

* ``base`` and ``limit`` are of type :c:type:`Index` and define the
  range of the iteration.

* ``single_action`` is the name of a macro which will be used for
  iterating over bits in the table individually. This macro must take
  a single :c:type:`Index` parameter corresponding to the index for
  the bit. The expansion of the macro must not contain ``break`` or
  ``continue`` because it will be called from within a loop from the
  expansion of :c:func:`ACT_ON_RANGE`.

* ``bits_action`` is the name of a macro which will be used for
  iterating over part-words. This macro must take parameters
  ``wordIndex``, ``base``, ``limit`` where ``wordIndex`` is the index
  into the array of words, and ``base`` and ``limit`` define a range
  of bits within the indexed word.

* ``word_action`` is the name of a macro which will be used for
  iterating over whole-words. This macro must take the single
  parameter ``wordIndex`` which is the index of the whole-word in the
  array. The expansion of the macro must not contain ``break`` or
  ``continue`` because it will be called from within a loop from the
  expansion of :c:func:`ACT_ON_RANGE`.

:mps:tag:`iteration.exit` The expansion of the ``single_action``,
``bits_action``, and ``word_action`` macros is allowed to contain
``return`` or ``goto`` to terminate the iteration early. This is used
by the test (:mps:ref:`.fun.test.range.set`) and find
(:mps:ref:`.fun.find`) operations.

:mps:tag:`iteration.small` If the range is sufficiently small only the
``single_action`` macro will be used, as this is more efficient in
practice. The choice of what constitutes a small range is made
entirely on the basis of experimental performance results (and
currently, 1999-04-27, a "small range" is 6 bits or fewer. See
change.mps.epcore.brisling.160181 for some justification). Otherwise
(for a bigger range) ``bits_action`` is used on the part words at
either end of the range (or the whole of the range it if it fits in a
single word), and ``word_action`` is used on the words that comprise
the inner portion of the range.

The implementation of :c:func:`ACT_ON_RANGE` (and
:c:func:`ACT_ON_RANGE_HIGH`) is simple enough. It decides which macros
it should invoke and invokes them. ``single_action`` and
``word_action`` are invoked inside loops.

:mps:tag:`fun.get` :c:func:`BTGet`. The bit-index will be converted in
the usual way, see :mps:ref:`.index`. The relevant :c:type:`Word` will
be read out of the Bit Table and shifted right by the
sub-:c:type:`Word` index (this brings the relevant bit down to the
least significant bit of the :c:type:`Word`), the :c:type:`Word` will
then be masked with 1, producing the answer.

:mps:tag:`fun.set` :c:func:`BTSet`.

:mps:tag:`fun.res` :c:func:`BTRes`.

In both :c:func:`BTSet` and :c:func:`BTRes` a mask is constructed by
shifting 1 left by the sub-word-index (see :mps:ref:`.index`). For
:c:func:`BTSet` the mask is or-ed into the relevant word (thereby
setting a single bit). For :c:func:`BTRes` the mask is inverted and
and-ed into the relevant word (thereby resetting a single bit).

:mps:tag:`fun.set-range` :c:func:`BTSetRange`. :c:func:`ACT_ON_RANGE`
(see :mps:ref:`.iteration` above) is used with macros that set a
single bit (using :c:func:`BTSet`), set a range of bits in a word, and
set a whole word.

:mps:tag:`fun.res-range` :c:func:`BTResRange` This is implemented
similarly to :c:func:`BTSetRange` (:mps:ref:`.fun.set-range`) except
using :c:func:`BTRes` and reverse bit-masking logic.

:mps:tag:`fun.test.range.set` :c:func:`BTIsSetRange`.
:c:func:`ACT_ON_RANGE` (see :mps:ref:`.iteration` above) is used with
macros that test whether all the relevant bits are set; if some of the
relevant bits are not set then ``return FALSE`` is used to terminate
the iteration early and return from the :c:func:`BTIsSetRange`
function. If the iteration completes then ``TRUE`` is returned.

:mps:tag:`fun.test.range.reset` :c:func:`BTIsResRange`. As for
:c:func:`BTIsSetRange` (:mps:ref:`.fun.test.range.set` above) but
testing whether the bits are reset.

:mps:tag:`fun.test.range.same` :c:func:`BTRangesSame`. As for
:c:func:`BTIsSetRange` (:mps:ref:`.fun.test.range.set` above) but
testing whether corresponding ranges in the two Bit Tables are the
same. Note there are no speed requirements, but :c:func:`ACT_ON_RANGE`
is used for simplicity and uniformity.

:mps:tag:`fun.find` The four external find functions
(:c:func:`BTFindShortResRange`, :c:func:`BTFindShortResRangeHigh`,
:c:func:`BTFindLongResRange`, :c:func:`BTFindLongResRangeHigh`) simply
call through to one of the two internal functions:
:c:func:`BTFindResRange` and :c:func:`BTFindResRangeHigh`.

.. c:function:: Bool BTFindResRange(Index *baseReturn, Index *limitReturn, BT bt, Index searchBase, Index searchLimit, Count minLength, Count maxLength)
.. c:function:: Bool BTFindResRangeHigh(Index *baseReturn, Index *limitReturn, BT bt, Index searchBase, Index searchLimit, Count minLength, Count maxLength)

There are two length parameters, one specifying the minimum length of
the range to be found, the other the maximum length. For
:c:func:`BTFindShort` and :c:func:`BTFindShortHigh`, ``maxLength`` is
equal to ``minLength`` when passed; for :c:func:`BTFindLong` and
:c:func:`BTFindLongHigh`, ``maxLength` is equal to the maximum
possible range, namely ``searchLimit - searchBase``.

:mps:tag:`fun.find-res-range` :c:func:`BTFindResRange`. Iterate within
the search boundaries, identifying candidate ranges by searching for a
reset bit. The :ref:`Boyer–Moore algorithm <BM77>` is used (it's
particularly easy to implement when there are only two symbols, 0 and
1, in the alphabet). For each candidate range, iterate backwards over
the bits from the end of the range towards the beginning. If a set bit
is found, this candidate has failed and a new candidate range is
selected. If when scanning for the set bit a range of reset bits was
found before finding the set bit, then this (small) range of reset
bits is used as the start of the next candidate. Additionally the end
of this small range of reset bits (the end of the failed candidate
range) is remembered so that we don't have to iterate over this range
again. But if no reset bits were found in the candidate range, then
iterate again (starting from the end of the failed candidate) to look
for one. If during the backwards search no set bit is found, then we
have found a sufficiently large range of reset bits; now extend the
valid range as far as possible up to the maximum length by iterating
forwards up to the maximum limit looking for a set bit. The iterations
make use of the :c:func:`ACT_ON_RANGE` and :c:func:`ACT_ON_RANGE_HIGH`
macros, which can use ``goto`` to effect an early termination of the
iteration when a set/reset (as appropriate) bit is found. The macro
:c:func:`ACTION_FIND_SET_BIT` is used in the iterations. It
efficiently finds the first (that is, with lowest index or weight) set
bit in a word or subword.

:mps:tag:`fun.find-res-range.improve` Various other performance
improvements have been suggested in the past, including some from
request.epcore.170534. Here is a list of potential improvements which
all sound plausible, but which have not led to performance
improvements in practice:

* :mps:tag:`fun.find-res-range.improve.step.partial` When the top
  index in a candidate range fails, skip partial words as well as
  whole words, using, for example, lookup tables.

* :mps:tag:`fun.find-res-range.improve.lookup` When testing a
  candidate run, examine multiple bits at once (for example, 8), using
  lookup tables for (for example) index of first set bit, index of
  last set bit, number of reset bits, length of maximum run of reset
  bits.

:mps:tag:`fun.find-res-range-high` :c:func:`BTFindResRangeHigh`.
Exactly the same algorithm as in :c:func:`BTFindResRange` (see
:mps:ref:`.fun.find-res-range` above), but moving over the table in
the opposite direction.

:mps:tag:`fun.copy-simple-range` :c:func:`BTCopyRange`. Uses
:c:func:`ACT_ON_RANGE` (see :mps:ref:`.iteration` above) with the
obvious implementation. Should be fast.

:mps:tag:`fun.copy-offset-range` :c:func:`BTCopyOffsetRange`. Uses a
simple iteration loop, reading bits with :c:func:`BTGet` and setting
them with :c:func:`BTSet`. Doesn't use :c:func:`ACT_ON_RANGE` because
the two ranges will not, in general, be similarly word-aligned.

:mps:tag:`fun.copy-invert-range` :c:func:`BTCopyInvertRange`. Uses
:c:func:`ACT_ON_RANGE` (see :mps:ref:`.iteration` above) with the
obvious implementation. Should be fast---although there are no speed
requirements.


Testing
-------

:mps:tag:`test` The following tests are available or have been used
during development.

:mps:tag:`test.btcv` ``btcv.c``. This is supposed to be a coverage
test, intended to execute all of the module's code in at least some
minimal way.

:mps:tag:`test.cbstest` ``cbstest.c``. This was written as a test of
the :c:type:`CBS` module (:mps:ref:`design.mps.cbs(2)`). It compares
the functional operation of a :c:type:`CBS` with that of a
:c:type:`BT` so is a good functional test of either module.

:mps:tag:`test.mmqa.120` MMQA_test_function!210.c. This is used
because it has a fair amount of segment allocation and freeing so
exercises the arena code that uses Bit Tables.

:mps:tag:`test.bttest` ``bttest.c``. This is an interactive test that
can be used to exercise some of the :c:type:`BT` functionality by hand.

:mps:tag:`test.dylan` It is possible to modify Dylan so that it uses
Bit Tables more extensively. See change.mps.epcore.brisling.160181
TEST1 and TEST2.

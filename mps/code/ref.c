/* ref.c: REFERENCES
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Implement operations on Ref, RefSet, ZoneSet, and Rank.
 *
 * .design: <design/ref> and <design/refset>.
 */

#include "mpm.h"

SRCID(ref, "$Id$");


/* RankCheck -- check a rank value */

Bool RankCheck(Rank rank)
{
  CHECKL(rank < RankLIMIT);
  UNUSED(rank); /* <code/mpm.c#check.unused> */
  return TRUE;
}


/* RankSetCheck -- check a rank set */

Bool RankSetCheck(RankSet rankSet)
{
  CHECKL(rankSet < ((ULongest)1 << RankLIMIT));
  UNUSED(rankSet); /* <code/mpm.c#check.unused> */
  return TRUE;
}


/* ZoneSetOfRange -- calculate the zone set of a range of addresses */

ZoneSet ZoneSetOfRange(Arena arena, Addr base, Addr limit)
{
  Word zbase, zlimit;

  AVERT(Arena, arena);
  AVER(limit > base);

  /* The base and limit zones of the range are calculated.  The limit */
  /* zone is the zone after the last zone of the range, not the zone of */
  /* the limit address. */
  zbase = (Word)base >> arena->zoneShift;
  zlimit = (((Word)limit-1) >> arena->zoneShift) + 1;


  /* If the range is large enough to span all zones, its zone set is */
  /* universal. */
  if (zlimit - zbase >= MPS_WORD_WIDTH)
    return ZoneSetUNIV;

  zbase  &= MPS_WORD_WIDTH - 1;
  zlimit &= MPS_WORD_WIDTH - 1;

  /* If the base zone is less than the limit zone, the zone set looks */
  /* like 000111100, otherwise it looks like 111000011. */
  if (zbase < zlimit)
    return ((ZoneSet)1<<zlimit) - ((ZoneSet)1<<zbase);
  else
    return ~(((ZoneSet)1<<zbase) - ((ZoneSet)1<<zlimit));
}


/* ZoneSetOfSeg -- calculate the zone set of segment addresses
 *
 * .rsor.def: The zone set of a segment is the union of the zones the
 * segment occupies.
 */

ZoneSet ZoneSetOfSeg(Arena arena, Seg seg)
{
  /* arena is checked by ZoneSetOfRange */
  AVERT(Seg, seg);

  return ZoneSetOfRange(arena, SegBase(seg), SegLimit(seg));
}


/* RangeInZoneSetFirst -- find an area of address space within a zone set
 *
 * Given a range of addresses, find the first sub-range of at least size that
 * is also within a zone set.  i.e. ZoneSetOfRange is a subset of the zone set.
 * Returns FALSE if no range satisfying the conditions could be found.
 */

static Addr nextStripe(Addr base, Addr limit, Arena arena)
{
  Addr next = AddrAlignUp(AddrAdd(base, 1), ArenaStripeSize(arena));
  AVER(next > base || next == (Addr)0);
  if (next >= limit || next < base)
    next = limit;
  return next;
}

Bool RangeInZoneSetFirst(Addr *baseReturn, Addr *limitReturn,
                         Addr base, Addr limit,
                         Arena arena, ZoneSet zoneSet, Size size)
{
  Size zebra;
  Addr searchLimit;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(base < limit);
  AVERT(Arena, arena);
  AVER(size > 0);
  AVER(zoneSet != ZoneSetEMPTY);

  /* TODO: Consider whether this search is better done by bit twiddling
     zone sets, e.g. by constructing a mask of zone bits as wide as the
     size and rotating the zoneSet. */

  if (AddrOffset(base, limit) < size)
    return FALSE;

  if (zoneSet == ZoneSetUNIV) {
    *baseReturn = base;
    *limitReturn = limit;
    return TRUE;
  }

  /* A "zebra" is the size of a complete set of stripes. */
  zebra = (sizeof(ZoneSet) * CHAR_BIT) << ArenaZoneShift(arena);
  if (size >= zebra) {
    AVER(zoneSet != ZoneSetUNIV);
    return FALSE;
  }

  /* There's no point searching through the zoneSet more than once. */
  searchLimit = AddrAdd(AddrAlignUp(base, ArenaStripeSize(arena)), zebra);
  if (searchLimit > base && limit > searchLimit)
    limit = searchLimit;

  do {
    Addr next;

    /* Search for a stripe in the zoneSet and within the block. */
    /* (Find the first set bit in the zoneSet not below the base zone.) */
    while (!ZoneSetHasAddr(arena, zoneSet, base)) {
      base = nextStripe(base, limit, arena);
      if (base >= limit)
        return FALSE;
    }

    /* Search for a run stripes in the zoneSet and within the block. */
    /* (Find a run of set bits in the zoneSet.) */
    next = base;
    do
      next = nextStripe(next, limit, arena);
    while (next < limit && ZoneSetHasAddr(arena, zoneSet, next));

    /* Is the run big enough to satisfy the size? */
    if (AddrOffset(base, next) >= size) {
      *baseReturn = base;
      *limitReturn = next;
      return TRUE;
    }

    base = next;
  } while (base < limit);

  return FALSE;
}


/* RangeInZoneSetLast -- find an area of address space within a zone set
 *
 * Given a range of addresses, find the last sub-range of at least size that
 * is also within a zone set.  i.e. ZoneSetOfRange is a subset of the zone set.
 * Returns FALSE if no range satisfying the conditions could be found.
 */

static Addr prevStripe(Addr base, Addr limit, Arena arena)
{
  Addr prev;
  AVER(limit != (Addr)0);
  prev = AddrAlignDown(AddrSub(limit, 1), ArenaStripeSize(arena));
  AVER(prev < limit);
  if (prev < base)
    prev = base;
  return prev;
}

Bool RangeInZoneSetLast(Addr *baseReturn, Addr *limitReturn,
                        Addr base, Addr limit,
                        Arena arena, ZoneSet zoneSet, Size size)
{
  Size zebra;
  Addr searchBase;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(base < limit);
  AVERT(Arena, arena);
  AVER(size > 0);
  AVER(zoneSet != ZoneSetEMPTY);

  /* TODO: Consider whether this search is better done by bit twiddling
     zone sets, e.g. by constructing a mask of zone bits as wide as the
     size and rotating the zoneSet. */

  if (AddrOffset(base, limit) < size)
    return FALSE;

  if (zoneSet == ZoneSetUNIV) {
    *baseReturn = base;
    *limitReturn = limit;
    return TRUE;
  }

  /* A "zebra" is the size of a complete set of stripes. */
  zebra = (sizeof(ZoneSet) * CHAR_BIT) << ArenaZoneShift(arena);
  if (size >= zebra) {
    AVER(zoneSet != ZoneSetUNIV);
    return FALSE;
  }

  /* There's no point searching through the zoneSet more than once. */
  searchBase = AddrSub(AddrAlignDown(limit, ArenaStripeSize(arena)), zebra);
  if (searchBase < limit && base < searchBase)
    base = searchBase;

  do {
    Addr prev;

    /* Search for a stripe in the zoneSet and within the block. */
    /* (Find the last set bit in the zoneSet below the limit zone.) */
    while (!ZoneSetHasAddr(arena, zoneSet, AddrSub(limit, 1))) {
      limit = prevStripe(base, limit, arena);
      if (base >= limit)
        return FALSE;
    }

    /* Search for a run stripes in the zoneSet and within the block. */
    /* (Find a run of set bits in the zoneSet.) */
    prev = limit;
    do
      prev = prevStripe(base, prev, arena);
    while (prev > base && ZoneSetHasAddr(arena, zoneSet, AddrSub(prev, 1)));

    /* Is the run big enough to satisfy the size? */
    if (AddrOffset(prev, limit) >= size) {
      *baseReturn = prev;
      *limitReturn = limit;
      return TRUE;
    }

    limit = prev;
  } while (base < limit);

  return FALSE;
}


/* ZoneSetBlacklist() -- calculate a zone set of likely false positives
 *
 * We blacklist the zones that could be referenced by values likely to be
 * found in ambiguous roots (such as the stack) and misinterpreted as
 * references, in order to avoid nailing down objects.  This isn't a
 * perfect simulation, but it should catch the common cases.
 */

ZoneSet ZoneSetBlacklist(Arena arena)
{
  ZoneSet blacklist;
  union {
    mps_word_t word;
    mps_addr_t addr;
    int i;
    long l;
  } nono;

  AVERT(Arena, arena);

  blacklist = ZoneSetEMPTY;
  nono.word = 0;
  nono.i = 1;
  blacklist = ZoneSetAddAddr(arena, blacklist, nono.addr);
  nono.i = -1;
  blacklist = ZoneSetAddAddr(arena, blacklist, nono.addr);
  nono.l = 1;
  blacklist = ZoneSetAddAddr(arena, blacklist, nono.addr);
  nono.l = -1;
  blacklist = ZoneSetAddAddr(arena, blacklist, nono.addr);

  return blacklist;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

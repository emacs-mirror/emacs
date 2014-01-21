/* ref.c: REFERENCES
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Implement operations on Ref, RefSet, ZoneSet, and Rank.
 *
 * .design: See design.mps.ref and design.mps.refset.
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

RefSet ZoneSetOfRange(Arena arena, Addr base, Addr limit)
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


/* RangeInZoneSet -- find an area of address space within a zone set
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

Bool RangeInZoneSet(Addr *baseReturn, Addr *limitReturn,
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
    if (AddrOffset(base, limit) >= size) {
      *baseReturn = base;
      *limitReturn = limit;
      return TRUE;
    }
    return FALSE;
  }
  
  /* A "zebra" is the size of a complete set of stripes. */
  zebra = sizeof(ZoneSet) * CHAR_BIT << arena->zoneShift;
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
    while (!ZoneSetIsMember(arena, zoneSet, base)) {
      base = nextStripe(base, limit, arena);
      if (base >= limit)
        return FALSE;
    }

    /* Search for a run stripes in the zoneSet and within the block. */
    next = base;
    do
      next = nextStripe(next, limit, arena);
    while(next < limit && ZoneSetIsMember(arena, zoneSet, next));
    
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




/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

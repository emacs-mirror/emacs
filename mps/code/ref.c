/* impl.c.ref: REFERENCES
 *
 * $HopeName: MMsrc!ref.c(trunk.12) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 *
 * .purpose: Implement operations on Ref, RefSet, ZoneSet, and Rank.
 *
 * .design: See design.mps.ref and design.mps.refset.
 */

#include "mpm.h"

SRCID(ref, "$HopeName: MMsrc!ref.c(trunk.12) $");


/* RankCheck -- check a rank value */

Bool RankCheck(Rank rank)
{
  CHECKL(rank < RankLIMIT);
  UNUSED(rank); /* impl.c.mpm.check.unused */
  return TRUE;
}


/* RankSetCheck -- check a rank set */

Bool RankSetCheck(RankSet rankSet)
{
  CHECKL(rankSet < (1uL << RankLIMIT));
  UNUSED(rankSet); /* impl.c.mpm.check.unused */
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

/* impl.c.ref: REFERENCES
 *
 * $HopeName: MMsrc!ref.c(trunk.9) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * .def.ref: Ref is an alias for Addr which can be used to document
 * where references are being passed.
 *
 * .def.refset: RefSet is a conservative approximation to a set of
 * references.  It is conservative in the sense that if isMember returns
 * FALSE for a reference then it is guaranteed that the reference was
 * not added into the RefSet (i.e., accumulating a pointer guarantees
 * that isMember will return TRUE for that reference), but the converse
 * is not true (i.e., isMember can return TRUE for references that have
 * not been accumulated).
 *
 * RefSets are designed to provide a very fast method for Add and
 * for IsMember.  Add is used to implement reference summaries,
 * which provide a remembered set.  IsMember is used to inline part
 * of the Fix function, and provide good discrimination of the
 * white set.  It is expected that the discrimination provided
 * will be useful for distinguishing segments and groups of segments.
 */

#include "mpm.h"

SRCID(ref, "$HopeName: MMsrc!ref.c(trunk.9) $");

Bool RankCheck(Rank rank)
{
  CHECKL(rank < RankMAX);
  UNUSED(rank); /* impl.c.mpm.check.unused */
  return TRUE;
}


Bool RankSetCheck(RankSet rankSet)
{
  CHECKL(rankSet < (1uL << RankMAX));
  UNUSED(rankSet); /* impl.c.mpm.check.unused */
  return TRUE;
}


/* RefSetOfRange -- calculate the reference set of a range of addresses
 *
 * .rsor.def: The reference set of a segment is the union of the
 * set of potential references _to_ that segment, i.e. of all the
 * addresses the segment occupies.
 *
 * .rsor.zones: The base and limit zones of the segment
 * are calculated.  The limit zone is one plus the zone of the last
 * address in the segment, not the zone of the limit address.
 *
 * .rsor.univ: If the segment is large enough to span all zones,
 * its reference set is universal.
 *
 * .rsor.swap: If the base zone is less than the limit zone,
 * then the reference set looks like 000111100, otherwise it looks like
 * 111000011.
 */

RefSet RefSetOfRange(Arena arena, Addr base, Addr limit)
{
  Word zbase, zlimit;

  AVERT(Arena, arena);
  AVER(limit > base);

  /* .rsor.zones */
  zbase = (Word)base >> arena->zoneShift;
  zlimit = (((Word)limit-1) >> arena->zoneShift) + 1;

  if(zlimit - zbase >= MPS_WORD_WIDTH)        /* .rsor.univ */
    return RefSetUNIV;

  zbase  &= MPS_WORD_WIDTH - 1;
  zlimit &= MPS_WORD_WIDTH - 1;

  if(zbase < zlimit)                      /* .rsor.swap */
    return ((RefSet)1<<zlimit) - ((RefSet)1<<zbase);
  else
    return ~(((RefSet)1<<zbase) - ((RefSet)1<<zlimit));
}


/* RefSetOfSeg -- calculate the reference set of segment addresses
 *
 */

RefSet RefSetOfSeg(Arena arena, Seg seg)
{
  /* arena is checked by RefSetOfRange */
  AVERT(Seg, seg);

  return RefSetOfRange(arena, SegBase(seg), SegLimit(seg));
}

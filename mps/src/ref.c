/* impl.c.ref: REFERENCES
 *
 * $HopeName: MMsrc!ref.c(MMdevel_restr.4) $
 * Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 * Ref is an alias for Addr which can be used to document where
 * references are being passed.
 *
 * RefSet is a conservative approximation to a set of references.
 * It is conservative in the sense that if isMember returns FALSE
 * for a reference then it is guaranteed that the reference was
 * not added into the RefSet (ie, accumulating a pointer guarantees
 * that isMember will return TRUE for that reference), but the
 * converse is not true (ie isMember can return TRUE for references
 * that have not been accumulated).
 *
 * RefSets are designed to provide a very fast method for Add and
 * for IsMember.  Add is used to implement reference summaries,
 * which provide a remembered set.  IsMember is used to inline part
 * of the Fix function, and provide good discrimination of the
 * condemned set.  It is expected that the discrimination provided
 * will be useful for distinguishing segments and groups of segments.
 */

#include "mpm.h"

SRCID(ref, "$HopeName: MMsrc!ref.c(MMdevel_restr.4) $");

Bool RankCheck(Rank rank)
{
  CHECKL(rank < RankMAX);
  return TRUE;
}


/* RefSetOfSeg -- calculate the reference set of segment addresses
 *
 * .rsos.def: The reference set of a segment is the union of the
 * set of potential references _to_ that segment, i.e. of all the
 * addresses the segment occupies.
 *
 * .rsos.zones: The base and limit zones of the segment
 * are calculated.  The limit zone is one plus the zone of the last
 * address in the segment, not the zone of the limit address.
 *
 * .rsos.univ: If the segment is large enough to span all zones,
 * its reference set is universal.
 *
 * .rsos.swap: If the base zone is less than the limit zone,
 * then the reference set looks like 000111100, otherwise it looks like
 * 111000011.
 */

RefSet RefSetOfSeg(Space space, Seg seg)
{
  Word base, limit;

  AVERT(Space, space);
  AVERT(Seg, seg);

  /* .rsos.zones */
  base = (Word)SegBase(space, seg) >> space->zoneShift;
  limit = (((Word)SegLimit(space, seg)-1) >> space->zoneShift) + 1;

  if(limit - base >= WORD_WIDTH)        /* .rsos.univ */
    return RefSetUniv;

  base  &= WORD_WIDTH - 1;
  limit &= WORD_WIDTH - 1;

  if(base < limit)                      /* .rsos.swap */
    return ((RefSet)1<<limit) - ((RefSet)1<<base);
  else
    return ~(((RefSet)1<<base) - ((RefSet)1<<limit));
}

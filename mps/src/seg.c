/* impl.c.seg: SEGMENTS
 *
 * $HopeName: MMsrc!seg.c(trunk.4) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * .design: The design for this module is design.mps.seg.
 *
 * TRANSGRESSIONS
 *
 * .check.shield: The "pm", "sm", and "depth" fields are not checked by
 * SegCheck, because I haven't spent time working out the invariants.
 * We should certainly work them out, by studying impl.c.shield, and
 * assert things about shielding, protection, shield cache consistency,
 * etc. richard 1997-04-03
 */

#include "mpm.h"

SRCID(seg, "$HopeName: MMsrc!seg.c(trunk.4) $");


/* SegCheck -- check the integrity of a segment */

Bool SegCheck(Seg seg)
{
  CHECKU(Pool, SegPool(seg));
  CHECKL(TraceSetCheck(SegWhite(seg)));
  CHECKL(TraceSetCheck(SegGrey(seg)));
  if(SegBuffer(seg) != NULL) {
    CHECKU(Buffer, SegBuffer(seg));
    /* design.mps.seg.field.buffer.owner */
    CHECKL(BufferPool(SegBuffer(seg)) == SegPool(seg));
  }
  CHECKL(RingCheck(SegPoolRing(seg)));
  CHECKL(RankSetCheck(SegRankSet(seg)));
  if(SegRankSet(seg) == RankSetEMPTY) {
    /* design.mps.seg.field.rankSet.empty: If there are no refs */
    /* in the segment then it cannot contain black or grey refs. */
    CHECKL(SegGrey(seg) == TraceSetEMPTY);
    CHECKL(SegSummary(seg) == RefSetEMPTY);
    CHECKL(SegSM(seg) == AccessSetEMPTY);
    CHECKL(SegPM(seg) == AccessSetEMPTY);
  } else {
    /* design.mps.seg.field.rankSet.single: The Tracer only permits */
    /* one rank per segment [ref?] so this field is either empty or a */
    /* singleton. */
    CHECKL(RankSetIsSingle(SegRankSet(seg)));
    /* .check.wb: If summary isn't universal then it must be Write shielded */
    CHECKL(SegSummary(seg) == RefSetUNIV || (SegSM(seg) & AccessWRITE));
  }
  /* "pm", "sm", and "depth" not checked.  See .check.shield. */
  CHECKL(BoolCheck(SegSingle(seg)));
  return TRUE;
}


/* SegInit -- initialize the generic part of a segment */

void SegInit(Seg seg, Pool pool)
{
  SegSetPool(seg, pool);
  SegSetP(seg, NULL);
  SegSetRankSet(seg, RankSetEMPTY);
  SegSetWhite(seg, TraceSetEMPTY);
  SegSetGrey(seg, TraceSetEMPTY);
  SegSetSummary(seg, RefSetEMPTY);
  SegSetBuffer(seg, NULL);
  RingInit(SegPoolRing(seg));
  SegSetPM(seg, AccessSetEMPTY);
  SegSetSM(seg, AccessSetEMPTY);
  SegSetDepth(seg, 0);
  SegSetSingle(seg, FALSE);

  AVERT(Seg, seg);
}


/* SegFinish -- finish the generic part of a segment */

void SegFinish(Seg seg)
{
  AVERT(Seg, seg);

  /* Check that the segment is not exposed, or in the shield */
  /* cache (see impl.c.shield.def.depth). */
  AVER(SegDepth(seg) == 0);
  
  /* Don't leave a dangling buffer allocating into hyperspace. */
  AVER(SegBuffer(seg) == NULL);

  RingFinish(SegPoolRing(seg));
}

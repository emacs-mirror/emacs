/* impl.c.seg: SEGMENTS
 *
 * $HopeName: MMsrc!seg.c(trunk.12) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
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

SRCID(seg, "$HopeName: MMsrc!seg.c(trunk.12) $");


/* SegCheck -- check the integrity of a segment */

Bool SegCheck(Seg seg)
{
  CHECKU(Pool, seg->_pool);
  CHECKL(TraceSetCheck(seg->_white));
  CHECKL(TraceSetSub(seg->_nailed, seg->_white));
  CHECKL(TraceSetCheck(seg->_grey));

  if(seg->_buffer != NULL) {
    CHECKU(Buffer, seg->_buffer);
    /* design.mps.seg.field.buffer.owner */
    CHECKL(BufferPool(seg->_buffer) == seg->_pool);
  }

  CHECKL(RingCheck(&seg->_poolRing));

  /* The segment must belong to some pool, so it should be on a */
  /* pool's segment ring.  (Actually, this isn't true just after */
  /* the segment is initialized.) */
  /*  CHECKL(RingNext(&seg->_poolRing) != &seg->_poolRing); */

  /* The segment should be on a grey ring if and only if it is grey. */
  CHECKL(RingCheck(&seg->_greyRing));
  CHECKL((seg->_grey == TraceSetEMPTY) ==
         RingIsSingle(&seg->_greyRing));

  CHECKL(RankSetCheck(seg->_rankSet));
  if(seg->_rankSet == RankSetEMPTY) {
    /* design.mps.seg.field.rankSet.empty: If there are no refs */
    /* in the segment then it cannot contain black or grey refs. */
    CHECKL(seg->_grey == TraceSetEMPTY);
    CHECKL(seg->_summary == RefSetEMPTY);
    CHECKL(seg->_sm == AccessSetEMPTY);
    CHECKL(seg->_pm == AccessSetEMPTY);
  } else {
    /* design.mps.seg.field.rankSet.single: The Tracer only permits */
    /* one rank per segment [ref?] so this field is either empty or a */
    /* singleton. */
    CHECKL(RankSetIsSingle(seg->_rankSet));
    /* Can't check barrier invariants because SegCheck is called */
    /* when raising or lowering the barrier. */
    /* .check.wb: If summary isn't universal then it must be */
    /* write shielded. */
    /* CHECKL(seg->_summary == RefSetUNIV || (seg->_sm & AccessWRITE)); */
    /* @@@@ What can be checked about the read barrier? */
  }

  /* "pm", "sm", and "depth" not checked.  See .check.shield. */

  CHECKL(BoolCheck(seg->_single));

  return TRUE;
}


/* SegInit -- initialize the generic part of a segment */

void SegInit(Seg seg, Pool pool)
{
  AVER(seg != NULL);
  AVERT(Pool, pool);

  seg->_pool = pool;
  seg->_p = NULL;
  seg->_rankSet = RankSetEMPTY;
  seg->_white = TraceSetEMPTY;
  seg->_nailed = TraceSetEMPTY;
  seg->_grey = TraceSetEMPTY;
  seg->_summary = RefSetEMPTY;
  seg->_buffer = NULL;
  RingInit(&seg->_poolRing);
  RingInit(&seg->_greyRing);
  seg->_pm = AccessSetEMPTY;
  seg->_sm = AccessSetEMPTY;
  seg->_depth = 0;
  seg->_single = TRUE;

  AVERT(Seg, seg);

  RingAppend(&pool->segRing, SegPoolRing(seg));
}


/* SegFinish -- finish the generic part of a segment */

void SegFinish(Seg seg)
{
  AVERT(Seg, seg);

  /* See impl.c.shield.shield.flush */
  ShieldFlush(PoolArena(seg->_pool));

  /* Check that the segment is not exposed, or in the shield */
  /* cache (see impl.c.shield.def.depth). */
  AVER(seg->_depth == 0);
  
  /* Don't leave a dangling buffer allocating into hyperspace. */
  AVER(seg->_buffer == NULL);

  RingRemove(SegPoolRing(seg));

  /* Detach the segment from the grey list if it is grey.  It is OK */
  /* to delete a grey segment provided the objects in it have been */
  /* proven to be unreachable by another trace. */
  if(seg->_grey != TraceSetEMPTY)
    RingRemove(&seg->_greyRing);

  RingFinish(&seg->_poolRing);
  RingFinish(&seg->_greyRing);
}


/* SegSetSummary -- change the summary on a segment
 *
 * In fact, we only need to raise the write barrier if the
 * segment contains references, and its summary is strictly smaller 
 * than the summary of the unprotectable data (i.e. the mutator).
 * We don't maintain such a summary, assuming that the mutator can 
 * access all references, so its summary is RefSetUNIV.
 */

void SegSetSummary(Seg seg, RefSet summary)
{
  RefSet oldSummary;
  Arena arena;

  AVERT(Seg, seg);

  arena = PoolArena(seg->_pool);
  oldSummary = seg->_summary;
  seg->_summary = summary;

  AVER(seg->_rankSet != RankSetEMPTY);

  /* Note: !RefSetSuper is a test for a strict subset */
  if(!RefSetSuper(summary, RefSetUNIV)) {
    if(RefSetSuper(oldSummary, RefSetUNIV))
      ShieldRaise(arena, seg, AccessWRITE);
  } else {
    if(!RefSetSuper(oldSummary, RefSetUNIV))
      ShieldLower(arena, seg, AccessWRITE);
  }
}


/* SegSetGrey -- change the greyness of a segment
 *
 * Sets the segment greyness to the trace set ts and adjusts
 * the shielding on the segment appropriately.
 */

void SegSetGrey(Seg seg, TraceSet grey)
{
  Arena arena;
  TraceSet oldGrey, flippedTraces;
  Rank rank;
  
  AVERT(Seg, seg);
  AVER(TraceSetCheck(grey));
  AVER(seg->_rankSet != RankSetEMPTY);

  arena = PoolArena(seg->_pool);
  oldGrey = seg->_grey;
  seg->_grey = grey;

  /* If the segment is now grey and wasn't before, add it to the */
  /* appropriate grey list so that TraceFindGrey can locate it */
  /* quickly later.  If it is no longer grey and was before, */
  /* remove it from the list. */
  if(oldGrey == TraceSetEMPTY) {
    if(grey != TraceSetEMPTY) {
      AVER(RankSetIsSingle(seg->_rankSet));
      for(rank = 0; rank < RankMAX; ++rank)
	if(RankSetIsMember(seg->_rankSet, rank)) {
	  RingInsert(ArenaGreyRing(arena, rank), &seg->_greyRing);
	  break;
	}
      AVER(rank != RankMAX); /* there should've been a match */
    }
  } else {
    if(grey == TraceSetEMPTY)
      RingRemove(&seg->_greyRing);
  }

  /* The read barrier is raised when the segment is grey for */
  /* some _flipped_ trace, i.e. is grey for a trace for which */
  /* the mutator is black. */
  flippedTraces = arena->flippedTraces;
  if(TraceSetInter(oldGrey, flippedTraces) == TraceSetEMPTY) {
    if(TraceSetInter(grey, flippedTraces) != TraceSetEMPTY)
      ShieldRaise(arena, seg, AccessREAD);
  } else {
    if(TraceSetInter(grey, flippedTraces) == TraceSetEMPTY)
      ShieldLower(arena, seg, AccessREAD);
  }

  EVENT_PPU(SegSetGrey, arena, seg, grey);
}


/* SegSetRankSet -- set the rank set of a segment
 *
 * If the rank set is made non-empty then the segment's summary is
 * now a subset of the mutator's (which is assumed to be RefSetUNIV)
 * so the write barrier must be imposed on the segment.  If the
 * rank set is made empty then there are no longer any references
 * on the segment so the barrier is removed.
 *
 * The caller must set the summary to empty before setting the rank
 * set to empty.  The caller must set the rank set to non-empty before
 * setting the summary to non-empty.
 */

void SegSetRankSet(Seg seg, RankSet rankSet)
{
  RankSet oldRankSet;
  Arena arena;

  AVERT(Seg, seg);
  AVER(RankSetCheck(rankSet));
  AVER(rankSet == RankSetEMPTY || RankSetIsSingle(rankSet));

  arena = PoolArena(seg->_pool);
  oldRankSet = seg->_rankSet;
  seg->_rankSet = rankSet;

  if(oldRankSet == RankSetEMPTY) {
    if(rankSet != RankSetEMPTY) {
      AVER(seg->_summary == RefSetEMPTY);
      ShieldRaise(arena, seg, AccessWRITE);
    }
  } else {
    if(rankSet == RankSetEMPTY) {
      AVER(seg->_summary == RefSetEMPTY);
      ShieldLower(arena, seg, AccessWRITE);
    }
  }
}

void SegSetRankAndSummary(Seg seg, RankSet rankSet, RefSet summary)
{
  Bool wasShielded, willbeShielded;
  Arena arena;

  AVERT(Seg, seg);
  AVER(RankSetCheck(rankSet));
  AVER(rankSet == RankSetEMPTY || RankSetIsSingle(rankSet));

  /* rankSet == RankSetEMPTY implies summary == RefSetEMPTY */
  AVER(rankSet != RankSetEMPTY || summary == RefSetEMPTY);

  arena = PoolArena(seg->_pool);

  if(seg->_rankSet != RankSetEMPTY && seg->_summary != RefSetUNIV) {
    wasShielded = TRUE;
  } else {
    wasShielded = FALSE;
  }

  if(rankSet != RankSetEMPTY && summary != RefSetUNIV) {
    willbeShielded = TRUE;
  } else {
    willbeShielded = FALSE;
  }

  seg->_rankSet = rankSet;
  seg->_summary = summary;

  if(willbeShielded && !wasShielded) {
    ShieldRaise(arena, seg, AccessWRITE);
  } else if(wasShielded && !willbeShielded) {
    ShieldLower(arena, seg, AccessWRITE);
  }
}


/* SegDescribe -- the description method */

Res SegDescribe(Seg seg, mps_lib_FILE *stream)
{
  Res res;

  /* Can't check seg, because Seg has no signature. */

  res = WriteF(stream,
               "Segment $P [$A,$A) {\n", (WriteFP)seg,
               (WriteFA)SegBase(seg), (WriteFA)SegLimit(seg),
               "  pool $P ($U)\n",
               (WriteFP)seg->_pool, (WriteFU)seg->_pool->serial,
               "  p $P\n", (WriteFP)seg->_p,
               NULL);
  if(res != ResOK)
    return res;
  if(seg->_buffer != NULL) {
    res = BufferDescribe(seg->_buffer, stream);
    if(res != ResOK)
      return res;
  }
  res = WriteF(stream,
               "  summary $W\n", (WriteFW)seg->_summary,
               "  shield depth $U\n", (WriteFU)seg->_depth,
               "  protection mode:",
               NULL);
  if(res != ResOK)
    return res;
  if(AccessSetIsMember(seg->_pm, AccessREAD)) {
     res = WriteF(stream, " read", NULL);
     if(res != ResOK)
       return res;
  }
  if(AccessSetIsMember(seg->_pm, AccessWRITE)) {
     res = WriteF(stream, " write", NULL);
     if(res != ResOK)
       return res;
  }
  res = WriteF(stream, "\n  shield mode:", NULL);
  if(res != ResOK)
    return res;
  if(AccessSetIsMember(seg->_sm, AccessREAD)) {
     res = WriteF(stream, " read", NULL);
     if(res != ResOK)
       return res;
  }
  if(AccessSetIsMember(seg->_sm, AccessWRITE)) {
     res = WriteF(stream, " write", NULL);
     if(res != ResOK)
       return res;
  }
  res = WriteF(stream, "\n  ranks:", NULL);
  /* This bit ought to be in a RankSetDescribe in ref.c. */
  if(RankSetIsMember(seg->_rankSet, RankAMBIG)) {
     res = WriteF(stream, " ambiguous", NULL);
     if(res != ResOK)
       return res;
  }
  if(RankSetIsMember(seg->_rankSet, RankEXACT)) {
     res = WriteF(stream, " exact", NULL);
     if(res != ResOK)
       return res;
  }
  if(RankSetIsMember(seg->_rankSet, RankFINAL)) {
     res = WriteF(stream, " final", NULL);
     if(res != ResOK)
       return res;
  }
  if(RankSetIsMember(seg->_rankSet, RankWEAK)) {
     res = WriteF(stream, " weak", NULL);
     if(res != ResOK)
       return res;
  }
  res = WriteF(stream, "\n",
               "  white  $B\n", (WriteFB)seg->_white,
               "  grey   $B\n", (WriteFB)seg->_grey,
               "  nailed $B\n", (WriteFB)seg->_nailed,
               /* _single is covered by base&limit, _rank is above */
               "} Segment $P\n", (WriteFP)seg, NULL);
  return res;
}

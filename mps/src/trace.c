/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 * $HopeName: MMsrc!trace.c(trunk.36) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 */

#include "mpm.h"

SRCID(trace, "$HopeName: MMsrc!trace.c(trunk.36) $");


/* ScanStateCheck -- check consistency of a ScanState object */

Bool ScanStateCheck(ScanState ss)
{
  TraceId ti;
  RefSet white;
  CHECKS(ScanState, ss);
  CHECKL(FUNCHECK(ss->fix));
  CHECKU(Arena, ss->arena);
  CHECKL(TraceSetCheck(ss->traces));
  CHECKL(TraceSetSuper(ss->arena->busyTraces, ss->traces));
  white = RefSetEMPTY;
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetIsMember(ss->traces, ti))
      white = RefSetUnion(white, ss->arena->trace[ti].white);
  CHECKL(ss->white == white);
  CHECKL(ss->zoneShift == ss->arena->zoneShift);
  CHECKL(RankCheck(ss->rank));
  CHECKL(BoolCheck(ss->wasMarked));
  return TRUE;
}


/* TraceIdCheck -- check that a TraceId is valid */

Bool TraceIdCheck(TraceId ti)
{
  CHECKL(ti == TraceIdNONE || ti < TRACE_MAX);
  return TRUE;
}


/* TraceSetCheck -- check that a TraceSet is valid */

Bool TraceSetCheck(TraceSet ts)
{
  CHECKL(ts < (1uL << TRACE_MAX));
  return TRUE;
}


/* TraceCheck -- check consistency of Trace object */

Bool TraceCheck(Trace trace)
{
  CHECKS(Trace, trace);
  CHECKU(Arena, trace->arena);
  CHECKL(TraceIdCheck(trace->ti));
  CHECKL(trace == &trace->arena->trace[trace->ti]);
  CHECKL(TraceSetIsMember(trace->arena->busyTraces, trace->ti));
  CHECKL(RankSetCheck(trace->grey));
  /* Can't check trace->white -- not in O(1) anyway. */
  /* Use trace->state to check more invariants. */
  switch(trace->state) {
    case TraceINIT:
    /* @@@@ What can be checked here? */
    break;

    case TraceUNFLIPPED:
    CHECKL(!TraceSetIsMember(trace->arena->flippedTraces, trace->ti));
    /* @@@@ Assert that mutator is grey for trace. */
    break;

    case TraceFLIPPED:
    CHECKL(TraceSetIsMember(trace->arena->flippedTraces, trace->ti));
    /* @@@@ Assert that mutator is black for trace. */
    break;

    case TraceRECLAIM:
    CHECKL(TraceSetIsMember(trace->arena->flippedTraces, trace->ti));
    /* @@@@ Assert that grey set is empty for trace. */
    break;

    case TraceFINISHED:
    CHECKL(TraceSetIsMember(trace->arena->flippedTraces, trace->ti));
    /* @@@@ Assert that grey and white sets is empty for trace. */
    break;

    default:
    NOTREACHED;
  }
  /* @@@@ Check trace->interval? */
  return TRUE;
}


/* TraceStart -- condemn a set of objects and start collection
 *
 * TraceStart should be passed a trace with state TraceINIT, i.e.
 * recently returned from TraceCreate.
 *
 * .start.black: All segments are black w.r.t. a newly allocated trace.
 * However, if TraceStart initialized segments to black when it
 * calculated the grey set then this condition could be relaxed, making
 * it easy to destroy traces half-way through.
 */

static Res TraceStart(Trace trace, Action action)
{
  Res res;
  Ring ring, node;
  Arena arena;
  Seg seg;
  Pool pool;

  AVERT(Trace, trace);
  AVERT(Action, action);
  AVER((action->pool->class->attr & AttrGC) != 0);
  AVER(trace->state == TraceINIT);
  AVER(trace->white == RefSetEMPTY);

  /* Identify the condemned set and turn it white. */
  arena = trace->arena;
  pool = action->pool;

  EVENT_PPP(TraceStart, trace, pool, action);
  ring = PoolSegRing(pool);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    seg = SegOfPoolRing(node);

    AVER(!TraceSetIsMember(SegWhite(seg), trace->ti)); /* .start.black */

    /* Give the pool the opportunity to turn the segment white. */
    /* If it fails, unwind. */
    res = PoolCondemn(pool, trace, seg, action);
    if(res != ResOK) goto failCondemn;

    /* Add the segment to the approximation of the white set the */
    /* pool made it white. */
    if(TraceSetIsMember(SegWhite(seg), trace->ti))
      trace->white = RefSetUnion(trace->white, RefSetOfSeg(arena, seg));

    node = next;
  }

  /* If there is nothing white then there can be nothing grey, */
  /* so everything is black and we can proceed straight to */
  /* reclaim.  We have to reclaim because we want to guarantee */
  /* to the pool that for every condemn there will be a reclaim. */
  /* @@@@ We can also shortcut if there is nothing grey. */
  /* @@@@ This should be in design. */
  if(trace->white == RefSetEMPTY) {
    arena->flippedTraces = TraceSetAdd(arena->flippedTraces, trace->ti);
    trace->state = TraceRECLAIM;
    return ResOK;
  }

  /* Turn everything else grey. */

  /* @@@@ Instead of iterating over all the segments, we could */
  /* iterate over all pools which are scannable and thence over */
  /* all their segments.  This might be better if the minority */
  /* of segments are scannable.  Perhaps we should choose */
  /* dynamically which method to use. */

  if(SegFirst(&seg, arena)) {
    Addr base;
    do {
      base = SegBase(arena, seg);
      /* Segment should be either black or white by now. */
      AVER(!TraceSetIsMember(SegGrey(seg), trace->ti));

      /* A segment can only be grey if it contains some references. */
      /* This is indicated by the rankSet begin non-empty.  Such */
      /* segments may only belong to scannable pools. */
      if(SegRankSet(seg) != RankSetEMPTY) {
        /* Segments with ranks may only belong to scannable pools. */
        AVER((SegPool(seg)->class->attr & AttrSCAN) != 0);

        /* Turn the segment grey if there might be a reference in it */
        /* to the white set.  This is done by seeing if the summary */
        /* of references in the segment intersects with the approximation */
        /* to the white set. */
        if(RefSetInter(SegSummary(seg), trace->white) != RefSetEMPTY)
          PoolGrey(SegPool(seg), trace, seg);
      }
    } while(SegNext(&seg, arena, base));
  }

  ring = ArenaRootRing(arena);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Root root = RING_ELT(Root, arenaRing, node);

    if(RefSetInter(root->summary, trace->white) != RefSetEMPTY)
      RootGrey(root, trace);

    node = next;
  }

  trace->state = TraceUNFLIPPED;

  return ResOK;

  /* PoolCodemn failed, possibly half-way through whitening the condemned */
  /* set.  This loop empties the white set again. */ 
failCondemn:
  ring = PoolSegRing(pool);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    seg = SegOfPoolRing(node);
    SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace->ti));
    node = next;
  }

  return res;
}


/* TraceCreate -- create a Trace object
 *
 * Allocates and initializes a new Trace object with a TraceId
 * which is not currently active.
 *
 * Returns ResLIMIT if there aren't any available trace IDs.
 *
 * Trace objects are allocated directly from a small array in the
 * arena structure which is indexed by the TraceId.  This is so
 * that it's always possible to start a trace (provided there's
 * a free TraceId) even if there's no available memory.
 *
 * This code is written to be adaptable to allocating Trace
 * objects dynamically.
 */

Res TraceCreate(Trace *traceReturn, Arena arena, Action action)
{
  TraceId ti;
  Trace trace;
  Res res;

  AVER(TRACE_MAX == 1);         /* .single-collection */

  AVER(traceReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Action, action);

  /* Find a free trace ID */
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(!TraceSetIsMember(arena->busyTraces, ti))
      goto found;

  return ResLIMIT;              /* no trace IDs available */

found:
  trace = ArenaTrace(arena, ti);
  AVER(trace->sig == SigInvalid);       /* design.mps.arena.trace.invalid */
  arena->busyTraces = TraceSetAdd(arena->busyTraces, ti);

  trace->arena = arena;
  trace->action = action;
  trace->white = RefSetEMPTY;
  trace->ti = ti;
  trace->state = TraceINIT;
  trace->interval = (Size)4096; /* @@@@ should be progress control */
  /* We conservatively assume that there may be grey segments at all */
  /* ranks when we create the trace.  (almost certainly we could do */
  /* better) */
  trace->grey = RankSetUNIV;

  trace->sig = TraceSig;
  AVERT(Trace, trace);

  res = PoolTraceBegin(action->pool, trace, action);
  if(res != ResOK) goto failBegin;
  
  res = TraceStart(trace, action);
  if(res != ResOK) goto failStart;

  *traceReturn = trace;
  EVENT_PPPU(TraceCreate, arena, action, trace, ti);
  return ResOK;

failStart:
  PoolTraceEnd(action->pool, trace, action);
failBegin:
  trace->sig = SigInvalid;              /* design.mps.arena.trace.invalid */
  arena->busyTraces = TraceSetDel(arena->busyTraces, ti);
  return res;
}


/* TraceDestroy -- destroy a trace object
 *
 * Finish and deallocate a Trace object, freeing up a TraceId.
 *
 * This code does not allow a Trace to be destroyed while it is
 * active.  It would be possible to allow this, but the colours
 * of segments etc. would need to be reset to black.
 */

void TraceDestroy(Trace trace)
{
  AVERT(Trace, trace);
  AVER(trace->state == TraceFINISHED);
#if 0
  /* removed AVER for now as it is not true for the first trace */
  AVER(trace->grey == RankSetEMPTY);
#endif
  
  PoolTraceEnd(trace->action->pool, trace, trace->action);
  
  trace->sig = SigInvalid;              /* design.mps.arena.trace.invalid */
  trace->arena->busyTraces =
    TraceSetDel(trace->arena->busyTraces, trace->ti);
  trace->arena->flippedTraces =
    TraceSetDel(trace->arena->flippedTraces, trace->ti);
  EVENT_P(TraceDestroy, trace);
}


/* TraceSetGreyen -- turn a segment more grey
 *
 * Adds the trace set ts to the greyness of the segment and adjusts
 * the shielding on the segment appropriately.  (If it causes the
 * segment to become grey for a flipped trace the shield is raised.)
 * @@@@ Why does it seem to be write and a read barrier?
 */

void TraceSegGreyen(Arena arena, Seg seg, TraceSet ts)
{
  TraceSet grey;
  
  AVERT(Arena, arena);
  AVERT(Seg, seg);
  AVER(TraceSetCheck(ts));

  grey = SegGrey(seg);
  grey = TraceSetUnion(grey, ts);
  if(grey != SegGrey(seg)) {
    /* Currently we assume that there is only one trace.  */
    /* This makes it simpler to greyen each trace. */
    AVER(ts == 1); /* @@@@ Hack */
    ArenaTrace(arena, 0)->grey =
      RankSetUnion(ArenaTrace(arena, 0)->grey, SegRankSet(seg));
    if(TraceSetInter(grey, arena->flippedTraces) != TraceSetEMPTY)
      ShieldRaise(arena, seg, AccessREAD);
  }
  SegSetGrey(seg, grey);
  EVENT_PPU(TraceSegGreyen, arena, seg, ts);
}


/* TraceFlipBuffers -- flip all buffers in the arena */

static void TraceFlipBuffers(Arena arena)
{
  Ring poolRing, poolNode, bufferRing, bufferNode;
  
  AVERT(Arena, arena);
  
  poolRing = ArenaPoolRing(arena);
  poolNode = RingNext(poolRing);
  while(poolNode != poolRing) {
    Ring poolNext = RingNext(poolNode);
    Pool pool = RING_ELT(Pool, arenaRing, poolNode);
    
    AVERT(Pool, pool);
    
    bufferRing = &pool->bufferRing;
    bufferNode = RingNext(bufferRing);
    while(bufferNode != bufferRing) {
      Ring bufferNext = RingNext(bufferNode);
      Buffer buffer = RING_ELT(Buffer, poolRing, bufferNode);
      
      AVERT(Buffer, buffer);
      
      BufferFlip(buffer);
      
      bufferNode = bufferNext;
    }
    
    poolNode = poolNext;
  }
}


/* TraceSetSummary -- change the summary on a segment
 *
 * The order of setting summary and lowering shield is important.
 * This code preserves the invariant that the segment is write-
 * shielded whenever the summary is not universal.
 * See impl.c.seg.check.wb.
 *
 * @@@@ In fact, we only need to raise the write barrier if the
 * summary is strictly smaller than the summary of the unprotectable
 * data (i.e. the mutator).  We don't maintain such a summary at the
 * moment, and assume that the mutator's summary is RefSetUNIV.
 */

void TraceSetSummary(Arena arena, Seg seg, RefSet summary)
{
  AVERT(Arena, arena);
  AVERT(Seg, seg);

  if(summary == RefSetUNIV) {
    SegSetSummary(seg, summary);             /* NB summary == RefSetUNIV */
    if(SegSM(seg) & AccessWRITE)
      ShieldLower(arena, seg, AccessWRITE);
  } else {
    if(!(SegSM(seg) & AccessWRITE))
      ShieldRaise(arena, seg, AccessWRITE);
    SegSetSummary(seg, summary);
  }
}


/* TraceFlip -- blacken the mutator */

static Res TraceFlip(Trace trace)
{
  Ring ring;
  Ring node;
  Arena arena;
  ScanStateStruct ss;
  Res res;

  AVERT(Trace, trace);

  arena = trace->arena;
  ShieldSuspend(arena);

  AVER(trace->state == TraceUNFLIPPED);

  EVENT_PP(TraceFlipBegin, trace, arena);

  TraceFlipBuffers(arena);
 
  /* Update location dependency structures.  white is */
  /* a conservative approximation of the refset of refs which */
  /* may move during this collection. */
  /* @@@@ It is too conservative.  Not everything white will */
  /* necessarily move. */
  LDAge(arena, trace->white);

  /* The trace is marked as flipped here, apparently prematurely, */
  /* so that TraceSegGreyen will DTRT when things are scanned below. */
  /* @@@@ This isn't right.  When flippedTraces is changed _all_ */
  /* grey segments should have their shield modes fixed up anyway. */
  trace->state = TraceFLIPPED;
  arena->flippedTraces = TraceSetAdd(arena->flippedTraces, trace->ti);

  /* At the moment we must scan all roots, because we don't have */
  /* a mechanism for shielding them.  There can't be any weak or */
  /* final roots either, since we must protect these in order to */
  /* avoid scanning them too early, before the pool contents. */

  /* @@@@ This isn't correct if there are higher ranking roots than */
  /* data in pools. */

  ss.fix = TraceFix;
  ss.zoneShift = ArenaZoneShift(arena);
  ss.white = trace->white;
  ss.summary = RefSetEMPTY;
  ss.arena = arena;
  ss.traces = TraceSetSingle(trace->ti);
  ss.wasMarked = TRUE;
  ss.sig = ScanStateSig;

  for(ss.rank = RankAMBIG; ss.rank <= RankEXACT; ++ss.rank) {
    ring = ArenaRootRing(arena);
    node = RingNext(ring);

    AVERT(ScanState, &ss);

    while(node != ring) {
      Ring next = RingNext(node);
      Root root = RING_ELT(Root, arenaRing, node);

      AVER(RootRank(root) <= RankEXACT); /* see above */

      if(RootRank(root) == ss.rank) {
        res = RootScan(&ss, root);
        if(res != ResOK) {
          return res;
        }
      }

      node = next;
    }
  }

  ss.sig = SigInvalid;  /* just in case */

  EVENT_PP(TraceFlipEnd, trace, arena);

  ShieldResume(arena);

  return ResOK;
}


static void TraceReclaim(Trace trace)
{
  Arena arena;
  Seg seg;

  AVERT(Trace, trace);
  AVER(trace->state == TraceRECLAIM);


  EVENT_P(TraceReclaim, trace);
  arena = trace->arena;
  if(SegFirst(&seg, arena)) {
    Addr base;
    do {
      base = SegBase(arena, seg);

      /* There shouldn't be any grey stuff left for this trace. */
      AVER(!TraceSetIsMember(SegGrey(seg), trace->ti));

      if(TraceSetIsMember(SegWhite(seg), trace->ti)) {
        AVER((SegPool(seg)->class->attr & AttrGC) != 0);

        PoolReclaim(SegPool(seg), trace, seg);

        /* If the segment still exists, it should no longer be white. */
        AVER(!(SegOfAddr(&seg, arena, base) &&
               TraceSetIsMember(SegWhite(seg), trace->ti)));
      }
    } while(SegNext(&seg, arena, base));
  }

  trace->state = TraceFINISHED;
}


/* FindGrey -- find a grey segment
 *
 * This function finds a segment which is grey for any of the traces
 * in ts and which does not have a higher rank than any other such
 * segment (i.e. a next segment to scan).
 *
 * This is equivalent to choosing a grey node from the grey set
 * of a partition.
 *
 * @@@@ This must be optimised by using better data structures at
 * the cost of some bookkeeping elsewhere, esp. during fix.
 */

static Bool FindGrey(Seg *segReturn, Rank *rankReturn,
                     Arena arena, TraceId ti)
{
  Rank rank;
  Trace trace;
  Seg seg;

  AVER(segReturn != NULL);
  AVERT(Arena, arena);
  AVER(TraceIdCheck(ti));

  trace = ArenaTrace(arena, ti);
  
  for(rank = 0; rank < RankMAX; ++rank) {
    if(RankSetIsMember(trace->grey, rank)) {
      if(SegFirst(&seg, arena)) {
	Addr base;
	do {
	  base = SegBase(arena, seg);
	  if(RankSetIsMember(SegRankSet(seg), rank) &&
	     TraceSetIsMember(SegGrey(seg), ti)) {
	    *segReturn = seg;
	    *rankReturn = rank;
	    return TRUE;
	  }
	} while(SegNext(&seg, arena, base));
      }
      trace->grey = RankSetDel(trace->grey, rank);
    }
  }

  AVER(trace->grey == RankSetEMPTY);

  return FALSE;
}


/* TraceScan -- scan a segment to remove greyness
 *
 * @@@@ During scanning, the segment should be write-shielded to
 * prevent any other threads from updating it while fix is being
 * applied to it (because fix is not atomic).  At the moment, we
 * don't bother, because we know that all threads are suspended.
 */

static Res TraceScan(TraceSet ts, Rank rank,
                     Arena arena, Seg seg)
{
  Res res;
  ScanStateStruct ss;
  TraceId ti;

  AVER(TraceSetCheck(ts));
  AVER(RankCheck(rank));
  AVERT(Seg, seg);
  
  /* The reason for scanning a segment is that it's grey. */
  AVER(TraceSetInter(ts, SegGrey(seg)) != TraceSetEMPTY);
  EVENT_UUPPP(TraceScan, ts, rank, arena, seg, &ss);

  ss.rank = rank;
  ss.traces = ts;
  ss.fix = TraceFix;
  ss.zoneShift = arena->zoneShift;
  ss.summary = RefSetEMPTY;
  ss.fixed = RefSetEMPTY;
  ss.arena = arena;
  ss.wasMarked = TRUE;
  ss.white = RefSetEMPTY;
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetIsMember(ss.traces, ti))
      ss.white = RefSetUnion(ss.white, ArenaTrace(arena, ti)->white);
  ss.sig = ScanStateSig;
  AVERT(ScanState, &ss);

  /* Expose the segment to make sure we can scan it. */
  ShieldExpose(arena, seg);

  res = PoolScan(&ss, SegPool(seg), seg);
  if(res != ResOK) {
    ShieldCover(arena, seg);
    return res;
  }

  /* .scan.post-condition: */ 
  /* The summary of reference seens by scan (ss.summary) is a subset */
  /* of the summary previously computed (SegSummary).  There are two */
  /* reasons that it is not an equality relation: */

  /* 1. if the segment has had objects forwarded onto it then its summary */
  /* will get unioned with the summary of the segment that the object was */
  /* forwarded from.  This may increase the summary.  The forwarded object */
  /* of course may have a smaller summary (if such a thing were to be */
  /* computed) and so subsequent scanning of the segment may reduce the */
  /* summmary.  (The forwarding process may erroneously introduce zones */
  /* into the destination's summary). */

  /* 2. A write barrier hit will set the summary to RefSetUNIV. */

  /* The reason that ss.summary is always a subset of the previous summary */
  /* is due to an "optimization" which has not been made in TraceFix.  See */
  /* .fix.fixed.all */

  AVER(RefSetSub(ss.summary, SegSummary(seg)));
  TraceSetSummary(arena, seg,
                  TraceSetUnion(ss.fixed,
                                TraceSetDiff(ss.summary, ss.white)));

  ss.sig = SigInvalid;                  /* just in case */

  /* The segment has been scanned, so remove the greyness from it. */
  SegSetGrey(seg, TraceSetDiff(SegGrey(seg), ts));

  /* If the segment is no longer grey for any flipped trace it */
  /* doesn't need to be behind the read barrier. */  
  if(TraceSetInter(SegGrey(seg), arena->flippedTraces) == TraceSetEMPTY)
    ShieldLower(arena, seg, AccessREAD);

  /* Cover the segment again, now it's been scanned. */
  ShieldCover(arena, seg);

  return res;
}


void TraceAccess(Arena arena, Seg seg, AccessSet mode)
{
  Res res;

  AVERT(Arena, arena);
  AVERT(Seg, seg);
  UNUSED(mode);

  /* If it's a read access, then the segment must be grey for a trace */
  /* which is flipped. */
  AVER((mode & SegSM(seg) & AccessREAD) == 0 ||
       TraceSetInter(SegGrey(seg), space->flippedTraces) !=
       TraceSetEMPTY);

  /* If it's a write acess, then the segment must have a summary that */
  /* is smaller than the mutator's summary (which is assumed to be */
  /* RefSetUNIV). */
  AVER((mode & SegSM(seg) & AccessWRITE) == 0 ||
       SegSummary(seg) != RefSetUNIV);

  EVENT_PPU(TraceAccess, arena, seg, mode);

  if((mode & SegSM(seg) & AccessREAD) != 0) {     /* read barrier? */
    /* scan.conservative: At the moment we scan at RankEXACT.  Really */
    /* we should be scanning at the "phase" of the trace, which is the */
    /* minimum rank of all grey segments. */
    /* design.mps.poolamc.access.multi @@@@ tag correct?? */
    res = TraceScan(arena->busyTraces,  /* @@@@ Should just be flipped traces? */
                    RankEXACT,
                    arena, seg);
    AVER(res == ResOK);                 /* design.mps.poolamc.access.error */

    /* The pool should've done the job of removing the greyness that */
    /* was causing the segment to be protected, so that the mutator */
    /* can go ahead and access it. */
    AVER(TraceSetInter(SegGrey(seg), arena->flippedTraces) == TraceSetEMPTY);
  }

  /* The write barrier handling must come after the read barrier, */
  /* because the latter may set the summary and raise the write barrier. */
  
  if((mode & SegSM(seg) & AccessWRITE) != 0)      /* write barrier? */
    TraceSetSummary(space, seg, RefSetUNIV);

  /* The segment must now be accessible. */
  AVER((mode & SegSM(seg)) == AccessSetEMPTY);
}


static Res TraceRun(Trace trace)
{
  Res res;
  Arena arena;
  Seg seg;
  Rank rank;

  AVERT(Trace, trace);
  AVER(trace->state == TraceFLIPPED);

  arena = trace->arena;

  if(FindGrey(&seg, &rank, arena, trace->ti)) {
    AVER((SegPool(seg)->class->attr & AttrSCAN) != 0);
    res = TraceScan(TraceSetSingle(trace->ti), rank,
                    arena, seg);
    if(res != ResOK) return res;
  } else
    trace->state = TraceRECLAIM;

  return ResOK;
}


/* TracePoll -- make some progress in tracing
 *
 * @@@@ This should accept some sort of progress control.
 */

Res TracePoll(Trace trace)
{
  Arena arena;
  Res res;

  AVERT(Trace, trace);

  arena = trace->arena;

  EVENT_PP(TracePoll, trace, arena);

  switch(trace->state) {
    case TraceUNFLIPPED: {
      res = TraceFlip(trace);
      if(res != ResOK) return res;
    } break;

    case TraceFLIPPED: {
      res = TraceRun(trace);
      if(res != ResOK) return res;
    } break;

    case TraceRECLAIM: {
      TraceReclaim(trace);
    } break;

    case TraceFINISHED:
    case TraceINIT:
    NOOP;
    break;

    default:
    NOTREACHED;
    break;
  }

  return ResOK;
}


/* TraceGreyEstimate -- estimate amount of grey stuff
 *
 * This function returns an estimate of the total size (in bytes)
 * of objects which would need to be scanned in order to find
 * all references to a certain RefSet.
 *
 * @@@@ This currently assumes that it's everything in the world.
 * @@@@ Should factor in the size of the roots, especially if the stack
 * is currently very deep.
 */

Size TraceGreyEstimate(Arena arena, RefSet refSet)
{
  return ArenaCommitted(arena);
}


Res TraceFix(ScanState ss, Ref *refIO)
{
  Ref ref;
  Seg seg;
  Pool pool;

  AVERT(ScanState, ss);
  AVER(refIO != NULL);

  ref = *refIO;

  EVENT_PPAU(TraceFix, ss, refIO, ref, ss->rank);
  if(SegOfAddr(&seg, ss->arena, ref)) {
    EVENT_P(TraceFixSeg, seg);
    if(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY) {
      Res res;
      EVENT_0(TraceFixWhite);
      pool = SegPool(seg);
      res = PoolFix(pool, ss, seg, refIO);
      if (res != ResOK)
        return res;
    }
  }

  /* .fix.fixed.all: */
  /* ss->fixed is accumulated for all the pointers whether or not they are */
  /* genuine references.  We could accumulate fewer pointers here, if a */
  /* pointer fails the SegOfAddr test then we know it isn't a reference, so */
  /* we needn't accumulate it into the fixed summary.  The design allows */
  /* this, but it breaks a useful post-condition on scanning.  See */
  /* .scan.post-condition.  (if the accumulation of ss->fixed was moved the */
  /* accuracy of ss->fixed would vary according to the "width" of the white */
  /* summary). */

  ss->fixed = RefSetAdd(ss->arena, ss->fixed, *refIO);

  return ResOK;
}


/* TraceScanArea -- scan contiguous area of references
 *
 * This is a convenience function for scanning the contiguous area
 * [base, limit).  i.e. it calls fix on all words from base up
 * to limit, inclusive of base and exclusive of limit.
 */

Res TraceScanArea(ScanState ss, Addr *base, Addr *limit)
{
  Res res;
  Addr *p;
  Ref ref;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  EVENT_PPP(TraceScanArea, ss, base, limit);

  TRACE_SCAN_BEGIN(ss) {
    p = base;
  loop:
    if(p >= limit) goto out;
    ref = *p++;
    if(!TRACE_FIX1(ss, ref)) goto loop;
    res = TRACE_FIX2(ss, p-1);
    if(res == ResOK) goto loop;
    return res;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(ss);

  return ResOK;
}


/* TraceScanAreaTagged -- scan contiguous area of tagged references
 *
 * This is as TraceScanArea except words are only fixed if they are
 * tagged as Dylan references (i.e. bottom two bits are zero).
 * @@@@ This Dylan-specificness should be generalized in some way.
 */

Res TraceScanAreaTagged(ScanState ss, Addr *base, Addr *limit)
{
  return TraceScanAreaMasked(ss, base, limit, (Word)3);
}


/* TraceScanAreaMasked -- scan contiguous area of filtered references
 *
 * This is as TraceScanArea except words are only fixed if they
 * are zero when masked with a mask.
 */

Res TraceScanAreaMasked(ScanState ss, Addr *base, Addr *limit, Word mask)
{
  Res res;
  Addr *p;
  Ref ref;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  EVENT_PPP(TraceScanAreaTagged, ss, base, limit);

  TRACE_SCAN_BEGIN(ss) {
    p = base;
  loop:
    if(p >= limit) goto out;
    ref = *p++;
    if(((Word)ref & mask) != 0) goto loop;
    if(!TRACE_FIX1(ss, ref)) goto loop;
    res = TRACE_FIX2(ss, p-1);
    if(res == ResOK) goto loop;
    return res;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(ss);

  return ResOK;
}

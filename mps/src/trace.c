/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 * $HopeName: MMsrc!trace.c(trunk.29) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 */

#include "mpm.h"

SRCID(trace, "$HopeName: MMsrc!trace.c(trunk.29) $");


/* ScanStateCheck -- check consistency of a ScanState object */

Bool ScanStateCheck(ScanState ss)
{
  TraceId ti;
  RefSet white;
  CHECKS(ScanState, ss);
  CHECKL(FUNCHECK(ss->fix));
  CHECKU(Space, ss->space);
  CHECKL(TraceSetCheck(ss->traces));
  CHECKL(TraceSetSuper(ss->space->busyTraces, ss->traces));
  white = RefSetEMPTY;
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetIsMember(ss->traces, ti))
      white = RefSetUnion(white, ss->space->trace[ti].white);
  CHECKL(ss->white == white);
  CHECKL(ss->zoneShift == ss->space->zoneShift);
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
  CHECKU(Space, trace->space);
  CHECKL(TraceIdCheck(trace->ti));
  CHECKL(trace == &trace->space->trace[trace->ti]);
  CHECKL(TraceSetIsMember(trace->space->busyTraces, trace->ti));
  /* Can't check trace->white -- not in O(1) anyway. */
  /* Use trace->state to check more invariants. */
  switch(trace->state) {
    case TraceINIT:
    /* @@@@ What can be checked here? */
    break;

    case TraceUNFLIPPED:
    CHECKL(!TraceSetIsMember(trace->space->flippedTraces, trace->ti));
    /* @@@@ Assert that mutator is grey for trace. */
    break;

    case TraceFLIPPED:
    CHECKL(TraceSetIsMember(trace->space->flippedTraces, trace->ti));
    /* @@@@ Assert that mutator is black for trace. */
    break;

    case TraceRECLAIM:
    CHECKL(TraceSetIsMember(trace->space->flippedTraces, trace->ti));
    /* @@@@ Assert that grey set is empty for trace. */
    break;

    case TraceFINISHED:
    CHECKL(TraceSetIsMember(trace->space->flippedTraces, trace->ti));
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
  Space space;
  Seg seg;
  Pool pool;

  AVERT(Trace, trace);
  AVERT(Action, action);
  AVER((action->pool->class->attr & AttrGC) != 0);
  AVER(trace->state == TraceINIT);
  AVER(trace->white == RefSetEMPTY);

  /* Identify the condemned set and turn it white. */
  space = trace->space;
  pool = action->pool;

  EVENT3(TraceStart, trace, pool, action);
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
      trace->white = RefSetUnion(trace->white, RefSetOfSeg(space, seg));

    node = next;
  }

  /* If there is nothing white then there can be nothing grey, */
  /* so everything is black and we can proceed straight to */
  /* reclaim.  We have to reclaim because we want to guarantee */
  /* to the pool that for every condemn there will be a reclaim. */
  /* @@@@ We can also shortcut if there is nothing grey. */
  /* @@@@ This should be in design. */
  if(trace->white == RefSetEMPTY) {
    space->flippedTraces = TraceSetAdd(space->flippedTraces, trace->ti);
    trace->state = TraceRECLAIM;
    return ResOK;
  }

  /* Turn everything else grey. */

  /* @@@@ Instead of iterating over all the segments, we could */
  /* iterate over all pools which are scannable and thence over */
  /* all their segments.  This might be better if the minority */
  /* of segments are scannable.  Perhaps we should choose */
  /* dynamically which method to use. */

  if(SegFirst(&seg, space)) {
    Addr base;
    do {
      base = SegBase(space, seg);
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
    } while(SegNext(&seg, space, base));
  }

  ring = SpaceRootRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Root root = RING_ELT(Root, spaceRing, node);

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
 * space structure which is indexed by the TraceId.  This is so
 * that it's always possible to start a trace (provided there's
 * a free TraceId) even if there's no available memory.
 *
 * This code is written to be adaptable to allocating Trace
 * objects dynamically.
 */

Res TraceCreate(Trace *traceReturn, Space space, Action action)
{
  TraceId ti;
  Trace trace;
  Res res;

  AVER(TRACE_MAX == 1);		/* .single-collection */

  AVER(traceReturn != NULL);
  AVERT(Space, space);
  AVERT(Action, action);

  /* Find a free trace ID */
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(!TraceSetIsMember(space->busyTraces, ti))
      goto found;

  return ResLIMIT;		/* no trace IDs available */

found:
  trace = SpaceTrace(space, ti);
  space->busyTraces = TraceSetAdd(space->busyTraces, ti);

  trace->space = space;
  trace->action = action;
  trace->white = RefSetEMPTY;
  trace->ti = ti;
  trace->state = TraceINIT;
  trace->interval = (Size)4096; /* @@@@ should be progress control */

  trace->sig = TraceSig;
  AVERT(Trace, trace);

  res = PoolTraceBegin(action->pool, trace, action);
  if(res != ResOK) goto failBegin;
  
  res = TraceStart(trace, action);
  if(res != ResOK) goto failStart;

  *traceReturn = trace;
  EVENT4(TraceCreate, space, action, trace, ti);
  return ResOK;

failStart:
  PoolTraceEnd(action->pool, trace, action);
failBegin:
  trace->sig = SigInvalid;
  space->busyTraces = TraceSetDel(space->busyTraces, ti);
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
  
  PoolTraceEnd(trace->action->pool, trace, trace->action);
  
  trace->sig = SigInvalid;
  trace->space->busyTraces =
    TraceSetDel(trace->space->busyTraces, trace->ti);
  trace->space->flippedTraces =
    TraceSetDel(trace->space->flippedTraces, trace->ti);
  EVENT1(TraceDestroy, trace);
}


/* TraceSetGreyen -- turn a segment more grey
 *
 * Adds the trace set ts to the greyness of the segment and adjusts
 * the shielding on the segment appropriately.  (If it causes the
 * segment to become grey for a flipped trace the shield is raised.)
 * @@@@ Why does it seem to be write and a read barrier?
 */

void TraceSegGreyen(Space space, Seg seg, TraceSet ts)
{
  TraceSet grey;
  
  AVERT(Space, space);
  AVERT(Seg, seg);
  AVER(TraceSetCheck(ts));

  grey = SegGrey(seg);
  grey = TraceSetUnion(grey, ts);
  if(grey != SegGrey(seg) &&
     TraceSetInter(grey, space->flippedTraces) != TraceSetEMPTY)
    ShieldRaise(space, seg, AccessREAD);
  SegSetGrey(seg, grey);
  EVENT3(TraceSegGreyen, space, seg, ts);
}


/* TraceFlipBuffers -- flip all buffers in the space */

static void TraceFlipBuffers(Space space)
{
  Ring poolRing, poolNode, bufferRing, bufferNode;
  
  AVERT(Space, space);
  
  poolRing = SpacePoolRing(space);
  poolNode = RingNext(poolRing);
  while(poolNode != poolRing) {
    Ring poolNext = RingNext(poolNode);
    Pool pool = RING_ELT(Pool, spaceRing, poolNode);
    
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

void TraceSetSummary(Space space, Seg seg, RefSet summary)
{
  AVERT(Space, space);
  AVERT(Seg, seg);

  if(summary == RefSetUNIV) {
    SegSetSummary(seg, summary);             /* NB summary == RefSetUNIV */
    if(SegSM(seg) & AccessWRITE)
      ShieldLower(space, seg, AccessWRITE);
  } else {
    if(!(SegSM(seg) & AccessWRITE))
      ShieldRaise(space, seg, AccessWRITE);
    SegSetSummary(seg, summary);
  }
}


/* TraceFlip -- blacken the mutator */

static Res TraceFlip(Trace trace)
{
  Ring ring;
  Ring node;
  Space space;
  ScanStateStruct ss;
  Res res;

  AVERT(Trace, trace);

  space = trace->space;
  ShieldSuspend(space);

  AVER(trace->state == TraceUNFLIPPED);

  EVENT2(TraceFlipBegin, trace, space);

  TraceFlipBuffers(space);
 
  /* Update location dependency structures.  white is */
  /* a conservative approximation of the refset of refs which */
  /* may move during this collection. */
  /* @@@@ It is too conservative.  Not everything white will */
  /* necessarily move. */
  LDAge(space, trace->white);

  /* The trace is marked as flipped here, apparently prematurely, */
  /* so that TraceSegGreyen will DTRT when things are scanned below. */
  /* @@@@ This isn't right.  When flippedTraces is changed _all_ */
  /* grey segments should have their shield modes fixed up anyway. */
  trace->state = TraceFLIPPED;
  space->flippedTraces = TraceSetAdd(space->flippedTraces, trace->ti);

  /* At the moment we must scan all roots, because we don't have */
  /* a mechanism for shielding them.  There can't be any weak or */
  /* final roots either, since we must protect these in order to */
  /* avoid scanning them too early, before the pool contents. */

  /* @@@@ This isn't correct if there are higher ranking roots than */
  /* data in pools. */

  ss.fix = TraceFix;
  ss.zoneShift = SpaceZoneShift(space);
  ss.white = trace->white;
  ss.summary = RefSetEMPTY;
  ss.space = space;
  ss.traces = TraceSetSingle(trace->ti);
  ss.wasMarked = TRUE;
  ss.sig = ScanStateSig;

  for(ss.rank = RankAMBIG; ss.rank <= RankEXACT; ++ss.rank) {
    ring = SpaceRootRing(space);
    node = RingNext(ring);

    AVERT(ScanState, &ss);

    while(node != ring) {
      Ring next = RingNext(node);
      Root root = RING_ELT(Root, spaceRing, node);

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

  EVENT2(TraceFlipEnd, trace, space);

  ShieldResume(space);

  return ResOK;
}


static void TraceReclaim(Trace trace)
{
  Space space;
  Seg seg;

  AVERT(Trace, trace);
  AVER(trace->state == TraceRECLAIM);


  EVENT1(TraceReclaim, trace);
  space = trace->space;
  if(SegFirst(&seg, space)) {
    Addr base;
    do {
      base = SegBase(space, seg);

      /* There shouldn't be any grey stuff left for this trace. */
      AVER(!TraceSetIsMember(SegGrey(seg), trace->ti));

      if(TraceSetIsMember(SegWhite(seg), trace->ti)) {
	AVER((SegPool(seg)->class->attr & AttrGC) != 0);

	PoolReclaim(SegPool(seg), trace, seg);
      }
    } while(SegNext(&seg, space, base));
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
                     Space space, TraceId ti)
{
  Rank rank;
  Seg seg;

  AVER(segReturn != NULL);
  AVERT(Space, space);
  AVER(TraceIdCheck(ti));
  
  for(rank = 0; rank < RankMAX; ++rank) {
    if(SegFirst(&seg, space)) {
      Addr base;
      do {
	base = SegBase(space, seg);
	if(RankSetIsMember(SegRankSet(seg), rank) &&
	   TraceSetIsMember(SegGrey(seg), ti)) {
	  *segReturn = seg;
	  *rankReturn = rank;
	  return TRUE;
	}
      } while(SegNext(&seg, space, base));
    }
  }

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
                     Space space, Seg seg)
{
  Res res;
  ScanStateStruct ss;
  TraceId ti;

  AVER(TraceSetCheck(ts));
  AVER(RankCheck(rank));
  AVERT(Seg, seg);
  
  /* The reason for scanning a segment is that it's grey. */
  AVER(TraceSetInter(ts, SegGrey(seg)) != TraceSetEMPTY);
  EVENT5(TraceScan, ts, rank, space, seg, &ss);

  ss.rank = rank;
  ss.traces = ts;
  ss.fix = TraceFix;
  ss.zoneShift = space->zoneShift;
  ss.summary = RefSetEMPTY;
  ss.fixed = RefSetEMPTY;
  ss.space = space;
  ss.wasMarked = TRUE;
  ss.white = RefSetEMPTY;
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetIsMember(ss.traces, ti))
      ss.white = RefSetUnion(ss.white, SpaceTrace(space, ti)->white);
  ss.sig = ScanStateSig;
  AVERT(ScanState, &ss);

  /* Expose the segment to make sure we can scan it. */
  ShieldExpose(space, seg);

  res = PoolScan(&ss, SegPool(seg), seg);
  if(res != ResOK) {
    ShieldCover(space, seg);
    return res;
  }

  /* The summary of references seen by scan must be a subset of */
  /* the ones we thought were there before. */
  AVER(RefSetSub(ss.summary, SegSummary(seg)));
  TraceSetSummary(space, seg,
                  TraceSetUnion(ss.fixed,
                                TraceSetDiff(ss.summary, ss.white)));

  ss.sig = SigInvalid;			/* just in case */

  /* The segment has been scanned, so remove the greyness from it. */
  SegSetGrey(seg, TraceSetDiff(SegGrey(seg), ts));

  /* If the segment is no longer grey for any flipped trace it */
  /* doesn't need to be behind the read barrier. */  
  if(TraceSetInter(SegGrey(seg), space->flippedTraces) == TraceSetEMPTY)
    ShieldLower(space, seg, AccessREAD);

  /* Cover the segment again, now it's been scanned. */
  ShieldCover(space, seg);

  return res;
}


void TraceAccess(Space space, Seg seg, AccessSet mode)
{
  Res res;

  AVERT(Space, space);
  AVERT(Seg, seg);
  UNUSED(mode);

  EVENT3(TraceAccess, space, seg, mode);

  if((mode & SegSM(seg) & AccessREAD) != 0) {     /* read barrier? */
    /* In this case, the segment must be grey for a trace which is */
    /* flipped. */
    AVER(TraceSetInter(SegGrey(seg), space->flippedTraces) != TraceSetEMPTY);

    /* scan.conservative: At the moment we scan at RankEXACT.  Really */
    /* we should be scanning at the "phase" of the trace, which is the */
    /* minimum rank of all grey segments. */
    /* design.mps.poolamc.access.multi @@@@ tag correct?? */
    res = TraceScan(space->busyTraces,  /* @@@@ Should just be flipped traces? */
                    RankEXACT,
                    space, seg);
    AVER(res == ResOK);                 /* design.mps.poolamc.access.error */

    /* The pool should've done the job of removing the greyness that */
    /* was causing the segment to be protected, so that the mutator */
    /* can go ahead and access it. */
    AVER(TraceSetInter(SegGrey(seg), space->flippedTraces) == TraceSetEMPTY);
  }

  if((mode & SegSM(seg) & AccessWRITE) != 0) {    /* write barrier? */
    AVER(SegSummary(seg) != RefSetUNIV);
    TraceSetSummary(space, seg, RefSetUNIV);
  }

  AVER((mode & SegSM(seg)) == AccessSetEMPTY);
}


static Res TraceRun(Trace trace)
{
  Res res;
  Space space;
  Seg seg;
  Rank rank;

  AVERT(Trace, trace);
  AVER(trace->state == TraceFLIPPED);

  space = trace->space;

  if(FindGrey(&seg, &rank, space, trace->ti)) {
    AVER((SegPool(seg)->class->attr & AttrSCAN) != 0);
    res = TraceScan(TraceSetSingle(trace->ti), rank,
                    space, seg);
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
  Space space;
  Res res;

  AVERT(Trace, trace);

  space = trace->space;

  EVENT2(TracePoll, trace, space);

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

Size TraceGreyEstimate(Space space, RefSet refSet)
{
  return ArenaCommitted(space);
}


Res TraceFix(ScanState ss, Ref *refIO)
{
  Ref ref;
  Seg seg;
  Pool pool;

  AVERT(ScanState, ss);
  AVER(refIO != NULL);

  ref = *refIO;

  EVENT4(TraceFix, ss, refIO, ref, ss->rank);
  if(SegOfAddr(&seg, ss->space, ref)) {
    EVENT1(TraceFixSeg, seg);
    if(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY) {
      Res res;
      EVENT0(TraceFixWhite);
      pool = SegPool(seg);
      res = PoolFix(pool, ss, seg, refIO);
      if (res != ResOK)
	return res;
    }
  }
  ss->fixed = RefSetAdd(ss->space, ss->fixed, *refIO);

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

  EVENT3(TraceScanArea, ss, base, limit);

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

  EVENT3(TraceScanAreaTagged, ss, base, limit);

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

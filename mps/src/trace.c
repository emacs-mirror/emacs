/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 * $HopeName: MMsrc!trace.c(trunk.56) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * .sources: design.mps.tracer.
 */

#include "mpm.h"

SRCID(trace, "$HopeName: MMsrc!trace.c(trunk.56) $");


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
  UNUSED(ti); /* impl.c.mpm.check.unused */
  return TRUE;
}


/* TraceSetCheck -- check that a TraceSet is valid */

Bool TraceSetCheck(TraceSet ts)
{
  CHECKL(ts < (1uL << TRACE_MAX));
  UNUSED(ts); /* impl.c.mpm.check.unused */
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
  /* Can't check trace->white -- not in O(1) anyway. */
  CHECKL(RefSetSub(trace->mayMove, trace->white));
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
  return TRUE;
}


/* TraceAddWhite -- add a segment to the white set of a trace */

Res TraceAddWhite(Trace trace, Seg seg)
{
  Res res;
  Pool pool;

  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(!TraceSetIsMember(SegWhite(seg), trace->ti)); /* .start.black */

  pool = SegPool(seg);
  AVERT(Pool, pool);

  /* Give the pool the opportunity to turn the segment white. */
  /* If it fails, unwind. */
  res = PoolWhiten(pool, trace, seg);
  if(res != ResOK)
    return res;

  /* Add the segment to the approximation of the white set the */
  /* pool made it white. */
  if(TraceSetIsMember(SegWhite(seg), trace->ti)) {
    trace->white = RefSetUnion(trace->white,
                               RefSetOfSeg(trace->arena, seg));
    trace->condemned += SegSize(seg);
    /* if the pool is a moving GC, then condemned objects may move */
    if(pool->class->attr & AttrMOVINGGC) {
      trace->mayMove = RefSetUnion(trace->mayMove, 
                                   RefSetOfSeg(PoolArena(pool), seg));
    }
  }

  return ResOK;
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

Res TraceStart(Trace trace)
{
  Ring ring, node;
  Arena arena;
  Seg seg;
  Res res;

  AVERT(Trace, trace);
  AVER(trace->state == TraceINIT);

  arena = trace->arena;

  /* If there is nothing white then there can be nothing grey, */
  /* so everything is black and we can finish the trace immediately. */
  if(trace->white == RefSetEMPTY) {
    arena->flippedTraces = TraceSetAdd(arena->flippedTraces, trace->ti);
    trace->state = TraceFINISHED;
    trace->rate = (Size)1;
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
      base = SegBase(seg);
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
        if(RefSetInter(SegSummary(seg), trace->white) != RefSetEMPTY) {
          PoolGrey(SegPool(seg), trace, seg);
	  if(TraceSetIsMember(SegGrey(seg), trace->ti))
	    trace->foundation += SegSize(seg);
        }
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

  /* Calculate the rate of working.  Assumes that half the condemned */
  /* set will survive, and calculates a rate of work which will */
  /* finish the collection by the time that a megabyte has been */
  /* allocated.  The 4096 is the number of bytes scanned by each */
  /* TracePoll (approximately) and should be replaced by a parameter. */
  /* This is a temporary measure for change.dylan.honeybee.170466. */
  {
    double surviving = trace->condemned / 2;
    double scan = trace->foundation + surviving;
    /* double reclaim = trace->condemned - surviving; */
    double alloc = 1024*1024L; /* reclaim / 2; */
    /* if(alloc > 0) */
      trace->rate = 1 + (Size)(scan * ARENA_POLL_MAX / (4096 * alloc));
    /* else */
      /* trace->rate = 1 + (Size)(scan / 4096); */
  }

  trace->state = TraceUNFLIPPED;

  /* All traces must flip at beginning at the moment. */
  res = TraceFlip(trace);
  if(res != ResOK)
    return res;

  return ResOK;
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

Res TraceCreate(Trace *traceReturn, Arena arena)
{
  TraceId ti;
  Trace trace;

  AVER(TRACE_MAX == 1);         /* .single-collection */

  AVER(traceReturn != NULL);
  AVERT(Arena, arena);

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
  trace->white = RefSetEMPTY;
  trace->mayMove = RefSetEMPTY;
  trace->ti = ti;
  trace->state = TraceINIT;
  trace->condemned = (Size)0;   /* nothing condemned yet */
  trace->foundation = (Size)0;  /* nothing grey yet */
  trace->rate = (Size)0;        /* no scanning to be done yet */
  trace->rootScanCount = (Count)0;
  trace->rootScanSize = (Size)0;
  trace->rootCopiedSize = (Size)0;
  trace->segScanCount = (Count)0;
  trace->segScanSize = (Size)0;
  trace->segCopiedSize = (Size)0;
  trace->fixRefCount = (Count)0;
  trace->segRefCount = (Count)0;
  trace->whiteSegRefCount = (Count)0;
  trace->nailCount = (Count)0;
  trace->snapCount = (Count)0;
  trace->forwardCount = (Count)0;
  trace->faultCount = (Count)0;
  trace->reclaimCount = (Count)0;
  trace->reclaimSize = (Size)0;
  trace->sig = TraceSig;
  AVERT(Trace, trace);

  *traceReturn = trace;
  return ResOK;
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
  
  trace->sig = SigInvalid;
  trace->arena->busyTraces =
    TraceSetDel(trace->arena->busyTraces, trace->ti);
  trace->arena->flippedTraces =
    TraceSetDel(trace->arena->flippedTraces, trace->ti);
  EVENT_P(TraceDestroy, trace);
}


/* TraceSegGreyen -- turn a segment more grey
 *
 * Adds the trace set ts to the greyness of the segment and adjusts
 * the shielding on the segment appropriately.  (If it causes the
 * segment to become grey for a flipped trace the shield is raised.)
 * @@@@ Why does it seem to be write and a read barrier?
 */

void TraceSegGreyen(Arena arena, Seg seg, TraceSet ts)
{
  TraceSet segGrey, newGrey;
 
  AVERT(Arena, arena);
  AVERT(Seg, seg);
  AVER(TraceSetCheck(ts));
 
  segGrey = SegGrey(seg);
  newGrey = TraceSetUnion(segGrey, ts);
  if(newGrey != segGrey) {
    /* The read barrier should only really be raised when the */
    /* segment is grey for some flipped trace, i.e. */
    /* if(TraceSetInter(grey, space->flippedTraces) != TraceSetEMPTY) */
    /* But this requires Flip to raise it when flippedTraces changes, */
    /* which it does not do at present. */
    ShieldRaise(arena, seg, AccessREAD);
 
    /* Temporary hack to add to grey list for */
    /* change.dylan.sunflower.7.170421. */
    AVER(RankSetIsSingle(SegRankSet(seg)));
    if(segGrey == TraceSetEMPTY) {
      switch(SegRankSet(seg)) {
      case RankSetSingle(RankAMBIG):
        RingInsert(&arena->greyRing[RankAMBIG], SegGreyRing(seg));
        break;
      case RankSetSingle(RankEXACT):
        RingInsert(&arena->greyRing[RankEXACT], SegGreyRing(seg));
        break;
      case RankSetSingle(RankFINAL):
        RingInsert(&arena->greyRing[RankFINAL], SegGreyRing(seg));
        break;
      case RankSetSingle(RankWEAK):
        RingInsert(&arena->greyRing[RankWEAK], SegGreyRing(seg));
        break;
      default:
        NOTREACHED;
        break;
      }
    }
  }
  SegSetGrey(seg, newGrey);
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


/* TraceFlip -- blacken the mutator */

Res TraceFlip(Trace trace)
{
  Ring ring;
  Ring node, nextNode;
  Arena arena;
  ScanStateStruct ss;
  Rank rank;
  Res res;

  AVERT(Trace, trace);

  arena = trace->arena;
  ShieldSuspend(arena);

  AVER(trace->state == TraceUNFLIPPED);
  AVER(!TraceSetIsMember(arena->flippedTraces, trace->ti));

  EVENT_PP(TraceFlipBegin, trace, arena);

  TraceFlipBuffers(arena);
 
  /* Update location dependency structures. */
  /* mayMove is a conservative approximation of the refset of refs */
  /* which may move during this collection. */
  if(trace->mayMove != RefSetEMPTY) {
    LDAge(arena, trace->mayMove);
  }

  /* At the moment we must scan all roots, because we don't have */
  /* a mechanism for shielding them.  There can't be any weak or */
  /* final roots either, since we must protect these in order to */
  /* avoid scanning them too early, before the pool contents. */

  /* @@@@ This isn't correct if there are higher ranking roots than */
  /* data in pools. */

  ss.fix = TraceFix;
  ss.zoneShift = ArenaZoneShift(arena);
  ss.white = trace->white;
  ss.arena = arena;
  ss.traces = TraceSetSingle(trace->ti);
  ss.wasMarked = TRUE;
  ss.fixRefCount = (Count)0;
  ss.segRefCount = (Count)0;
  ss.whiteSegRefCount = (Count)0;
  ss.nailCount = (Count)0;
  ss.snapCount = (Count)0;
  ss.forwardCount = (Count)0;
  ss.copiedSize = (Size)0;
  ss.scannedSize = (Size)0;
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
        ScanStateSetSummary(&ss, RefSetEMPTY);
        res = RootScan(&ss, root);
        ++trace->rootScanCount;
        if(res != ResOK) {
          return res;
        }
      }

      node = next;
    }
  }
  trace->rootScanSize += ss.scannedSize;
  trace->rootCopiedSize += ss.copiedSize;
  trace->fixRefCount += ss.fixRefCount;
  trace->segRefCount += ss.segRefCount;
  trace->whiteSegRefCount += ss.whiteSegRefCount;
  trace->nailCount += ss.nailCount;
  trace->snapCount += ss.snapCount;
  trace->forwardCount += ss.forwardCount;

  ss.sig = SigInvalid;  /* just in case */

  /* .flip.alloc: Allocation needs to become black now. While we flip */
  /* at the start, we can get away with always allocating black. This */
  /* needs to change when we flip later (i.e. have a read-barrier     */
  /* collector), so that we allocate grey or white before the flip    */
  /* and black afterwards. For instance, see                          */
  /* design.mps.poolams.invariant.alloc.                              */

  /* Now that the mutator is black we must prevent it from reading */
  /* grey objects so that it can't obtain white pointers.  This is */
  /* achieved by read protecting all segments containing objects */
  /* which are grey for any of the flipped traces. */
  for(rank = 0; rank < RankMAX; ++rank)
    RING_FOR(node, ArenaGreyRing(arena, rank), nextNode) {
      Seg seg = SegOfGreyRing(node);
      if(TraceSetInter(SegGrey(seg),
                       arena->flippedTraces) == TraceSetEMPTY &&
         TraceSetIsMember(SegGrey(seg), trace->ti))
        ShieldRaise(arena, seg, AccessREAD);
    }

  /* @@@@ When write barrier collection is implemented, this is where */
  /* write protection should be removed for all segments which are */
  /* no longer blacker than the mutator.  Possibly this can be done */
  /* lazily as they are touched. */

  /* Mark the trace as flipped. */
  trace->state = TraceFLIPPED;
  arena->flippedTraces = TraceSetAdd(arena->flippedTraces, trace->ti);

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
      base = SegBase(seg);

      /* There shouldn't be any grey stuff left for this trace. */
      AVER(!TraceSetIsMember(SegGrey(seg), trace->ti));

      if(TraceSetIsMember(SegWhite(seg), trace->ti)) {
        AVER((SegPool(seg)->class->attr & AttrGC) != 0);

        PoolReclaim(SegPool(seg), trace, seg);

        /* If the segment still exists, it should no longer be white. */
        /* Note that the seg returned by this SegOfAddr may not be */
        /* the same as the one above, but in that case it's new and */
        /* still shouldn't be white for this trace. */

	/* The code from the class-specific reclaim methods to */
	/* unwhiten the segment could in fact be moved here.   */
        {
          Seg nonWhiteSeg = NULL;	/* prevents compiler warning */
	  AVER(!(SegOfAddr(&nonWhiteSeg, arena, base) &&
		 TraceSetIsMember(SegWhite(nonWhiteSeg), trace->ti)));
        }
      }
    } while(SegNext(&seg, arena, base));
  }

  trace->state = TraceFINISHED;
}


/* traceFindGrey -- find a grey segment
 *
 * This function finds a segment which is grey for any of the traces
 * in ts and which does not have a higher rank than any other such
 * segment (i.e. a next segment to scan).
 *
 * This is equivalent to choosing a grey node from the grey set
 * of a partition.
 */

static Bool traceFindGrey(Seg *segReturn, Rank *rankReturn,
                          Arena arena, TraceId ti)
{
  Rank rank;
  Trace trace;
  Ring node, nextNode;

  AVER(segReturn != NULL);
  AVERT(Arena, arena);
  AVER(TraceIdCheck(ti));

  trace = ArenaTrace(arena, ti);
  
  for(rank = 0; rank < RankMAX; ++rank) {
    RING_FOR(node, ArenaGreyRing(arena, rank), nextNode) {
      Seg seg = SegOfGreyRing(node);
      AVERT(Seg, seg);
      AVER(SegGrey(seg) != TraceSetEMPTY);
      AVER(RankSetIsMember(SegRankSet(seg), rank));
      if(TraceSetIsMember(SegGrey(seg), ti)) {
        *segReturn = seg;
        *rankReturn = rank;
        return TRUE;
      }
    }
  }

  /* There are no grey segments for this trace. */

  return FALSE;
}


/* ScanStateSetSummary -- set the summary of scanned references
 *
 * This function sets unfixedSummary and fixedSummary such that
 * ScanStateSummary will return the summary passed.  Subsequently
 * fixed references are accumulated into this result.
 */

void ScanStateSetSummary(ScanState ss, RefSet summary)
{
  AVERT(ScanState, ss);

  ss->unfixedSummary = RefSetEMPTY;
  ss->fixedSummary = summary;
  AVER(ScanStateSummary(ss) == summary);
}

/* ScanStateSummary -- calculate the summary of scanned references
 *
 * The summary of the scanned references is the summary of the
 * unfixed references, minus the white set, plus the summary of the
 * fixed references.  This is because TraceFix is called for all
 * references in the white set, and accumulates a summary of
 * references after they have been fixed.
 */

RefSet ScanStateSummary(ScanState ss)
{
  AVERT(ScanState, ss);

  return TraceSetUnion(ss->fixedSummary,
                       TraceSetDiff(ss->unfixedSummary, ss->white));
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
  TraceId ti;
  RefSet white;
  ScanStateStruct ss;

  AVER(TraceSetCheck(ts));
  AVER(RankCheck(rank));
  AVERT(Seg, seg);
  
  /* The reason for scanning a segment is that it's grey. */
  AVER(TraceSetInter(ts, SegGrey(seg)) != TraceSetEMPTY);
  EVENT_UUPPP(TraceScan, ts, rank, arena, seg, &ss);

  white = RefSetEMPTY;
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(TraceSetIsMember(ts, ti))
      white = RefSetUnion(white, ArenaTrace(arena, ti)->white);

  /* only scan a segment if it refers to the white set */
  if (RefSetInter(white, SegSummary(seg)) == RefSetEMPTY) { /* blacken it */
    PoolBlacken(SegPool(seg), ts, seg);
  } else {  /* scan it */
    ss.rank = rank;
    ss.traces = ts;
    ss.fix = TraceFix;
    ss.zoneShift = arena->zoneShift;
    ss.unfixedSummary = RefSetEMPTY;
    ss.fixedSummary = RefSetEMPTY;
    ss.arena = arena;
    ss.wasMarked = TRUE;
    ss.white = white;
    ss.fixRefCount = (Count)0;
    ss.segRefCount = (Count)0;
    ss.whiteSegRefCount = (Count)0;
    ss.nailCount = (Count)0;
    ss.snapCount = (Count)0;
    ss.forwardCount = (Count)0;
    ss.copiedSize = (Size)0;
    ss.scannedSize = (Size)0;
    ss.sig = ScanStateSig;
    AVERT(ScanState, &ss);
    
    /* Expose the segment to make sure we can scan it. */
    ShieldExpose(arena, seg);
    
    res = PoolScan(&ss, SegPool(seg), seg);

    if(res != ResOK) {
      ShieldCover(arena, seg);
      return res;
    }
    
    /* Cover the segment again, now it's been scanned. */
    ShieldCover(arena, seg);

    /* See design.mps.scan.summary.subset. */
    AVER(RefSetSub(ss.unfixedSummary, SegSummary(seg)));
    
    /* All objects on the segment have been scanned, so the scanned */
    /* summary should replace the segment summary. */
    SegSetSummary(seg, ScanStateSummary(&ss));

    for(ti = 0; ti < TRACE_MAX; ++ti)
      if(TraceSetIsMember(ts, ti)) {
        Trace trace = ArenaTrace(arena, ti);

        ++trace->segScanCount;
        trace->segScanSize += ss.scannedSize;
        trace->segCopiedSize += ss.copiedSize;
        trace->fixRefCount += ss.fixRefCount;
        trace->segRefCount += ss.segRefCount;
        trace->whiteSegRefCount += ss.whiteSegRefCount;
        trace->nailCount += ss.nailCount;
        trace->snapCount += ss.snapCount;
        trace->forwardCount += ss.forwardCount;
      }
    
    ss.sig = SigInvalid;                  /* just in case */
  }

  /* The segment is now black, so remove the greyness from it. */
  SegSetGrey(seg, TraceSetDiff(SegGrey(seg), ts));

  return ResOK;

}


void TraceAccess(Arena arena, Seg seg, AccessSet mode)
{
  Res res;
  TraceId ti;

  AVERT(Arena, arena);
  AVERT(Seg, seg);
  UNUSED(mode);

  /* If it's a read access, then the segment must be grey for a trace */
  /* which is flipped. */
  AVER((mode & SegSM(seg) & AccessREAD) == 0 ||
       TraceSetInter(SegGrey(seg), arena->flippedTraces) !=
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

    for(ti = 0; ti < TRACE_MAX; ++ti)
      if(TraceSetIsMember(arena->busyTraces, ti))
        ++ArenaTrace(arena, ti)->faultCount;
  }

  /* The write barrier handling must come after the read barrier, */
  /* because the latter may set the summary and raise the write barrier. */
  
  if((mode & SegSM(seg) & AccessWRITE) != 0)      /* write barrier? */
    SegSetSummary(seg, RefSetUNIV);

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

  if(traceFindGrey(&seg, &rank, arena, trace->ti)) {
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
  UNUSED(refSet);
  return ArenaCommitted(arena);
}


Res TraceFix(ScanState ss, Ref *refIO)
{
  Ref ref;
  Seg seg;
  Pool pool;

  /* See design.mps.trace.fix.noaver */
  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(refIO != NULL);

  ref = *refIO;

  ++ss->fixRefCount;

  EVENT_PPAU(TraceFix, ss, refIO, ref, ss->rank);

  /* SegOfAddr is inlined, see design.mps.trace.fix.segofaddr */
  if(SEG_OF_ADDR(&seg, ss->arena, ref)) {
    ++ss->segRefCount;
    EVENT_P(TraceFixSeg, seg);
    if(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY) {
      Res res;

      ++ss->whiteSegRefCount;
      EVENT_0(TraceFixWhite);
      pool = SegPool(seg);
      /* Could move the rank switch here from the class-specific */
      /* fix methods. */
      res = PoolFix(pool, ss, seg, refIO);
      if(res != ResOK)
        return res;
    }
  } else {
    /* Address is not in segment. */
    /* It is illegal for exact references to point to an */
    /* address that is currently reserved by the arena, but */
    /* not in use.  There can't possibly be any objects at */
    /* those addresses.  We check that here.
    /* This AVER might be a candidate for making CRITICAL in */
    /* some configurations */
    AVER(ss->rank < RankEXACT ||
	 !ArenaIsReservedAddr(ss->arena, ref));
  }


  /* .fix.fixed.all: */
  /* ss->fixedSummary is accumulated for all the pointers whether */
  /* or not they are genuine references.  We could accumulate fewer */
  /* pointers here, if a pointer fails the SegOfAddr test then we */
  /* know it isn't a reference, so we needn't accumulate it into the */
  /* fixed summary.  The design allows this, but it breaks a useful */
  /* post-condition on scanning.  See .scan.post-condition.  (if */
  /* the accumulation of ss->fixedSummary was moved the accuracy */
  /* of ss->fixedSummary would vary according to the "width" of the */
  /* white summary). */
  ss->fixedSummary = RefSetAdd(ss->arena, ss->fixedSummary, *refIO);

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
    if(res == ResOK)
      goto loop;
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
    if(res == ResOK)
      goto loop;
    return res;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(ss);

  return ResOK;
}

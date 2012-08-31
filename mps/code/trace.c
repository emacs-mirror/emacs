/* trace.c: GENERIC TRACER IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2003, 2006, 2007 Ravenbrook Limited.
 * See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .design: <design/trace/>.  */

#include "chain.h"
#include "mpm.h"
#include <limits.h> /* for LONG_MAX */

SRCID(trace, "$Id$");

/* Forward declarations */
Rank traceBand(Trace);
Bool traceBandAdvance(Trace);
Bool traceBandFirstStretch(Trace);
void traceBandFirstStretchDone(Trace);
DIAG_DECL( static void traceFindGrey_diag(Bool found, Rank rank); )

/* Types */

enum {
  traceAccountingPhaseRootScan = 1,
  traceAccountingPhaseSegScan,
  traceAccountingPhaseSingleScan
};
typedef int traceAccountingPhase;

/* ScanStateCheck -- check consistency of a ScanState object */

Bool ScanStateCheck(ScanState ss)
{
  TraceId ti;
  Trace trace;
  ZoneSet white;

  CHECKS(ScanState, ss);
  CHECKL(FUNCHECK(ss->fix));
  CHECKL(ss->zoneShift == ss->arena->zoneShift);
  white = ZoneSetEMPTY;
  TRACE_SET_ITER(ti, trace, ss->traces, ss->arena)
    white = ZoneSetUnion(white, ss->arena->trace[ti].white);
  TRACE_SET_ITER_END(ti, trace, ss->traces, ss->arena);
  CHECKL(ss->white == white);
  CHECKU(Arena, ss->arena);
  /* Summaries could be anything, and can't be checked. */
  CHECKL(TraceSetCheck(ss->traces));
  CHECKL(TraceSetSuper(ss->arena->busyTraces, ss->traces));
  CHECKL(RankCheck(ss->rank));
  CHECKL(BoolCheck(ss->wasMarked));
  /* @@@@ checks for counts missing */
  return TRUE;
}


/* ScanStateInit -- Initialize a ScanState object */

void ScanStateInit(ScanState ss, TraceSet ts, Arena arena,
                   Rank rank, ZoneSet white)
{
  TraceId ti;
  Trace trace;

  AVER(TraceSetCheck(ts));
  AVERT(Arena, arena);
  AVER(RankCheck(rank));
  /* white is arbitrary and can't be checked */

  ss->fix = TraceFix;
  TRACE_SET_ITER(ti, trace, ts, arena)
    if(trace->emergency) {
      ss->fix = TraceFixEmergency;
    }
  TRACE_SET_ITER_END(ti, trace, ts, arena);
  ss->rank = rank;
  ss->traces = ts;
  ss->zoneShift = arena->zoneShift;
  ss->unfixedSummary = RefSetEMPTY;
  ss->fixedSummary = RefSetEMPTY;
  ss->arena = arena;
  ss->wasMarked = TRUE;
  ss->white = white;
  STATISTIC(ss->fixRefCount = (Count)0);
  STATISTIC(ss->segRefCount = (Count)0);
  STATISTIC(ss->whiteSegRefCount = (Count)0);
  STATISTIC(ss->nailCount = (Count)0);
  STATISTIC(ss->snapCount = (Count)0);
  STATISTIC(ss->forwardedCount = (Count)0);
  ss->forwardedSize = (Size)0; /* see .message.data */
  STATISTIC(ss->preservedInPlaceCount = (Count)0);
  ss->preservedInPlaceSize = (Size)0; /* see .message.data */
  STATISTIC(ss->copiedSize = (Size)0);
  ss->scannedSize = (Size)0; /* see .workclock */
  ss->sig = ScanStateSig;

  AVERT(ScanState, ss);
}


/* ScanStateFinish -- Finish a ScanState object */

void ScanStateFinish(ScanState ss)
{
  AVERT(ScanState, ss);
  ss->sig = SigInvalid;
}


/* TraceIdCheck -- check that a TraceId is valid */

Bool TraceIdCheck(TraceId ti)
{
  CHECKL(ti < TraceLIMIT);
  UNUSED(ti); /* <code/mpm.c#check.unused> */
  return TRUE;
}


/* TraceSetCheck -- check that a TraceSet is valid */

Bool TraceSetCheck(TraceSet ts)
{
  CHECKL(ts < ((ULongest)1 << TraceLIMIT));
  UNUSED(ts); /* <code/mpm.c#check.unused> */
  return TRUE;
}


/* TraceCheck -- check consistency of Trace object */

Bool TraceCheck(Trace trace)
{
  CHECKS(Trace, trace);
  CHECKU(Arena, trace->arena);
  CHECKL(TraceIdCheck(trace->ti));
  CHECKL(trace == &trace->arena->trace[trace->ti]);
  CHECKL(TraceSetIsMember(trace->arena->busyTraces, trace));
  CHECKL(ZoneSetSub(trace->mayMove, trace->white));
  /* Use trace->state to check more invariants. */
  switch(trace->state) {
    case TraceINIT:
      /* @@@@ What can be checked here? */
      break;

    case TraceUNFLIPPED:
      CHECKL(!TraceSetIsMember(trace->arena->flippedTraces, trace));
      /* @@@@ Assert that mutator is grey for trace. */
      break;

    case TraceFLIPPED:
      CHECKL(TraceSetIsMember(trace->arena->flippedTraces, trace));
      /* @@@@ Assert that mutator is black for trace. */
      break;

    case TraceRECLAIM:
      CHECKL(TraceSetIsMember(trace->arena->flippedTraces, trace));
      /* @@@@ Assert that grey set is empty for trace. */
      break;

    case TraceFINISHED:
      CHECKL(TraceSetIsMember(trace->arena->flippedTraces, trace));
      /* @@@@ Assert that grey and white sets is empty for trace. */
      break;

    default:
      NOTREACHED;
      break;
  }
  /* Valid values for band depend on state. */
  if(trace->state == TraceFLIPPED) {
    CHECKL(RankCheck(trace->band));
  }
  CHECKL(BoolCheck(trace->emergency));
  if(trace->chain != NULL) {
    CHECKU(Chain, trace->chain);
  }

  /* @@@@ checks for counts missing */

  /* check pre-allocated messages for this traceid */
  CHECKL(TraceIdMessagesCheck(trace->arena, trace->ti));

  return TRUE;
}

/* traceBand - current band of the trace.
 *
 * The current band is the band currently being discovered.  Each band
 * corresponds to a rank.  The R band is all objects that are reachable
 * only by tracing references of rank R or earlier _and_ are not in some
 * earlier band (thus, the bands are disjoint).  Whilst a particular
 * band is current all the objects that become marked are the objects in
 * that band.
 */
Rank traceBand(Trace trace)
{
  AVERT(Trace, trace);

  return trace->band;
}

/* traceBandAdvance - advance to next band.
 *
 * Advances (increments) the current band to the next band and returns TRUE
 * if possible;
 * otherwise, there are no more bands, so resets the band state and
 * returns FALSE.
 */
Bool traceBandAdvance(Trace trace)
{
  AVER(trace->state == TraceFLIPPED);

  ++trace->band;
  trace->firstStretch = TRUE;
  if(trace->band >= RankLIMIT) {
    trace->band = RankAMBIG;
    return FALSE;
  }
  return TRUE;
}

/* traceBandFirstStretch - whether in first stretch or not.
 *
 * For a band R (see traceBand) the first stretch is defined as all the
 * scanning work done up until the first point where we run out of grey
 * rank R segments (and either scan something of an earlier rank or
 * change bands).
 *
 * This function returns TRUE whilst we are in the first stretch, FALSE
 * otherwise.
 *
 * Entering the first stretch is automatically performed by
 * traceBandAdvance, but finishing it is detected in traceFindGrey.
 */
Bool traceBandFirstStretch(Trace trace)
{
  return trace->firstStretch;
}

void traceBandFirstStretchDone(Trace trace)
{
  trace->firstStretch = FALSE;
}

/* traceUpdateCounts - dumps the counts from a ScanState into the Trace */

static void traceUpdateCounts(Trace trace, ScanState ss,
                              traceAccountingPhase phase)
{
  switch(phase) {
    case traceAccountingPhaseRootScan: {
      trace->rootScanSize += ss->scannedSize;
      trace->rootCopiedSize += ss->copiedSize;
      STATISTIC(++trace->rootScanCount);
      break;
    }
    case traceAccountingPhaseSegScan: {
      trace->segScanSize += ss->scannedSize; /* see .workclock */
      trace->segCopiedSize += ss->copiedSize;
      STATISTIC(++trace->segScanCount);
      break;
    }
    case traceAccountingPhaseSingleScan: {
      STATISTIC(trace->singleScanSize += ss->scannedSize);
      STATISTIC(trace->singleCopiedSize += ss->copiedSize);
      break;
    }
    default:
      NOTREACHED;
  }
  STATISTIC(trace->fixRefCount += ss->fixRefCount);
  STATISTIC(trace->segRefCount += ss->segRefCount);
  STATISTIC(trace->whiteSegRefCount += ss->whiteSegRefCount);
  STATISTIC(trace->nailCount += ss->nailCount);
  STATISTIC(trace->snapCount += ss->snapCount);
  STATISTIC(trace->forwardedCount += ss->forwardedCount);
  trace->forwardedSize += ss->forwardedSize;  /* see .message.data */
  STATISTIC(trace->preservedInPlaceCount += ss->preservedInPlaceCount);
  trace->preservedInPlaceSize += ss->preservedInPlaceSize;

  return;
}


/* traceSetUpdateCounts -- update counts for a set of traces */

static void traceSetUpdateCounts(TraceSet ts, Arena arena, ScanState ss,
                                 traceAccountingPhase phase)
{
  TraceId ti; Trace trace;

  AVERT(ScanState, ss); /* check that we're not copying garbage */

  TRACE_SET_ITER(ti, trace, ts, arena)
    traceUpdateCounts(trace, ss, phase);
  TRACE_SET_ITER_END(ti, trace, ts, arena);
  return;
}


/* traceSetSignalEmergency -- move a set of traces into emergency mode. */

static void traceSetSignalEmergency(TraceSet ts, Arena arena)
{
  TraceId ti;
  Trace trace;

  DIAG_SINGLEF(( "traceSetSignalEmergency",
    "traceSet: $B", (WriteFB)ts, NULL ));

  TRACE_SET_ITER(ti, trace, ts, arena)
    trace->emergency = TRUE;
  TRACE_SET_ITER_END(ti, trace, ts, arena);

  return;
}


/* traceSetWhiteUnion
 *
 * Returns a ZoneSet describing the union of the white sets of all the
 * specified traces.  */

static ZoneSet traceSetWhiteUnion(TraceSet ts, Arena arena)
{
  TraceId ti;
  Trace trace;
  ZoneSet white = ZoneSetEMPTY;

  TRACE_SET_ITER(ti, trace, ts, arena)
    white = ZoneSetUnion(white, trace->white);
  TRACE_SET_ITER_END(ti, trace, ts, arena);

  return white;
}


/* TraceAddWhite -- add a segment to the white set of a trace */

Res TraceAddWhite(Trace trace, Seg seg)
{
  Res res;
  Pool pool;

  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(!TraceSetIsMember(SegWhite(seg), trace)); /* .start.black */

  pool = SegPool(seg);
  AVERT(Pool, pool);

  /* Give the pool the opportunity to turn the segment white. */
  /* If it fails, unwind. */
  res = PoolWhiten(pool, trace, seg);
  if(res != ResOK)
    return res;

  /* Add the segment to the approximation of the white set the */
  /* pool made it white. */
  if(TraceSetIsMember(SegWhite(seg), trace)) {
    trace->white = ZoneSetUnion(trace->white, ZoneSetOfSeg(trace->arena, seg));
    /* if the pool is a moving GC, then condemned objects may move */
    if(pool->class->attr & AttrMOVINGGC) {
      trace->mayMove = ZoneSetUnion(trace->mayMove,
                                    ZoneSetOfSeg(trace->arena, seg));
    }
  }

  return ResOK;
}


/* TraceCondemnZones -- condemn all objects in the given zones
 *
 * TraceCondemnZones is passed a trace in state TraceINIT, and a set of
 * objects to condemn.
 *
 * @@@@ For efficiency, we ought to find the condemned set and the
 * foundation in one search of the segment ring.  This hasn't been done
 * because some pools still use TraceAddWhite for the condemned set.
 *
 * @@@@ This function would be more efficient if there were a cheaper
 * way to select the segments in a particular zone set.  */

Res TraceCondemnZones(Trace trace, ZoneSet condemnedSet)
{
  Seg seg;
  Arena arena;
  Res res;

  AVERT(Trace, trace);
  AVER(condemnedSet != ZoneSetEMPTY);
  AVER(trace->state == TraceINIT);
  AVER(trace->white == ZoneSetEMPTY);

  arena = trace->arena;

  if(SegFirst(&seg, arena)) {
    Addr base;
    do {
      base = SegBase(seg);
      /* Segment should be black now. */
      AVER(!TraceSetIsMember(SegGrey(seg), trace));
      AVER(!TraceSetIsMember(SegWhite(seg), trace));

      /* A segment can only be white if it is GC-able. */
      /* This is indicated by the pool having the GC attribute */
      /* We only condemn segments that fall entirely within */
      /* the requested zone set.  Otherwise, we would bloat the */
      /* foundation to no gain.  Note that this doesn't exclude */
      /* any segments from which the condemned set was derived, */
      if((SegPool(seg)->class->attr & AttrGC) != 0
          && ZoneSetSuper(condemnedSet, ZoneSetOfSeg(arena, seg))) {
        res = TraceAddWhite(trace, seg);
        if(res != ResOK)
          return res;
      }
    } while (SegNext(&seg, arena, base));
  }

  /* The trace's white set must be a subset of the condemned set */
  AVER(ZoneSetSuper(condemnedSet, trace->white));

  return ResOK;
}


/* traceFlipBuffers -- flip all buffers in the arena */

static void traceFlipBuffers(Globals arena)
{
  Ring nodep, nextp;

  RING_FOR(nodep, &arena->poolRing, nextp) {
    Pool pool = RING_ELT(Pool, arenaRing, nodep);
    Ring nodeb, nextb;

    AVERT(Pool, pool);
    RING_FOR(nodeb, &pool->bufferRing, nextb) {
      BufferFlip(RING_ELT(Buffer, poolRing, nodeb));
    }
  }
}


/* traceScanRootRes -- scan a root, with result code */

static Res traceScanRootRes(TraceSet ts, Rank rank, Arena arena, Root root)
{
  ZoneSet white;
  Res res;
  ScanStateStruct ss;

  white = traceSetWhiteUnion(ts, arena);

  ScanStateInit(&ss, ts, arena, rank, white);

  res = RootScan(&ss, root);

  traceSetUpdateCounts(ts, arena, &ss, traceAccountingPhaseRootScan);
  ScanStateFinish(&ss);
  return res;
}


/* traceScanRoot
 *
 * Scan a root without fail.  The traces may enter emergency mode to
 * ensure this.  */

static void traceScanRoot(TraceSet ts, Rank rank, Arena arena, Root root)
{
  Res res;

  res = traceScanRootRes(ts, rank, arena, root);
  if(res != ResOK) {
    AVER(ResIsAllocFailure(res));
    traceSetSignalEmergency(ts, arena);
    res = traceScanRootRes(ts, rank, arena, root);
    /* Should be OK in emergency mode */
  }
  AVER(ResOK == res);

  return;
}


/* traceFlip -- blacken the mutator */

struct rootFlipClosureStruct {
  TraceSet ts;
  Arena arena;
  Rank rank;
};

static Res rootFlip(Root root, void *p)
{
  struct rootFlipClosureStruct *rf = (struct rootFlipClosureStruct *)p;

  AVERT(Root, root);
  AVER(p != NULL);
  AVER(TraceSetCheck(rf->ts));
  AVERT(Arena, rf->arena);
  AVER(RankCheck(rf->rank));

  AVER(RootRank(root) <= RankEXACT); /* see .root.rank */

  if(RootRank(root) == rf->rank)
    traceScanRoot(rf->ts, rf->rank, rf->arena, root);

  return ResOK;
}

static void traceFlip(Trace trace)
{
  Ring node, nextNode;
  Arena arena;
  Rank rank;
  struct rootFlipClosureStruct rfc;

  AVERT(Trace, trace);
  rfc.ts = TraceSetSingle(trace);

  arena = trace->arena;
  rfc.arena = arena;
  ShieldSuspend(arena);

  AVER(trace->state == TraceUNFLIPPED);
  AVER(!TraceSetIsMember(arena->flippedTraces, trace));

  EVENT2(TraceFlipBegin, trace, arena);

  traceFlipBuffers(ArenaGlobals(arena));

  /* Update location dependency structures. */
  /* mayMove is a conservative approximation of the zones of objects */
  /* which may move during this collection. */
  if(trace->mayMove != ZoneSetEMPTY) {
    LDAge(arena, trace->mayMove);
  }

  /* .root.rank: At the moment we must scan all roots, because we don't have */
  /* a mechanism for shielding them.  There can't be any weak or final roots */
  /* either, since we must protect these in order to avoid scanning them too */
  /* early, before the pool contents.  @@@@ This isn't correct if there are */
  /* higher ranking roots than data in pools. */

  for(rank = RankAMBIG; rank <= RankEXACT; ++rank) {
    Res res;

    rfc.rank = rank;
    res = RootsIterate(ArenaGlobals(arena), rootFlip, (void *)&rfc);
    AVER(res == ResOK);
  }

  /* .flip.alloc: Allocation needs to become black now. While we flip */
  /* at the start, we can get away with always allocating black. This */
  /* needs to change when we flip later (i.e. have a read-barrier     */
  /* collector), so that we allocate grey or white before the flip    */
  /* and black afterwards. For instance, see                          */
  /* <design/poolams/#invariant.alloc>.                              */
  /* (surely we mean "write-barrier" not "read-barrier" above? */
  /* drj 2003-02-19) */

  /* Now that the mutator is black we must prevent it from reading */
  /* grey objects so that it can't obtain white pointers.  This is */
  /* achieved by read protecting all segments containing objects */
  /* which are grey for any of the flipped traces. */
  for(rank = 0; rank < RankLIMIT; ++rank)
    RING_FOR(node, ArenaGreyRing(arena, rank), nextNode) {
      Seg seg = SegOfGreyRing(node);
      if(TraceSetInter(SegGrey(seg), arena->flippedTraces) == TraceSetEMPTY
          && TraceSetIsMember(SegGrey(seg), trace))
        ShieldRaise(arena, seg, AccessREAD);
    }

  /* @@@@ When write barrier collection is implemented, this is where */
  /* write protection should be removed for all segments which are */
  /* no longer blacker than the mutator.  Possibly this can be done */
  /* lazily as they are touched. */

  /* Mark the trace as flipped. */
  trace->state = TraceFLIPPED;
  arena->flippedTraces = TraceSetAdd(arena->flippedTraces, trace);

  EVENT2(TraceFlipEnd, trace, arena);

  ShieldResume(arena);

  return;
}

/* traceCopySizes -- preserve size information for later use
 *
 * A PoolGen's newSize is important information that we want to emit in
 * a diagnostic message at TraceStart.  In order to do that we must copy
 * the information before Whiten changes it.  This function does that.
 */

static void traceCopySizes(Trace trace)
{
  Ring node, nextNode;
  Index i;
  Arena arena = trace->arena;

  RING_FOR(node, &arena->chainRing, nextNode) {
    Chain chain = RING_ELT(Chain, chainRing, node);

    for(i = 0; i < chain->genCount; ++i) {
      Ring n, nn;
      GenDesc desc = &chain->gens[i];
      RING_FOR(n, &desc->locusRing, nn) {
        PoolGen gen = RING_ELT(PoolGen, genRing, n);
        gen->newSizeAtCreate = gen->newSize;
      }
    }
  }
  return;
}

/* TraceCreate -- create a Trace object
 *
 * Allocates and initializes a new Trace object with a TraceId which is
 * not currently active.
 *
 * Returns ResLIMIT if there aren't any available trace IDs.
 *
 * Trace objects are allocated directly from a small array in the arena
 * structure which is indexed by the TraceId.  This is so that it's
 * always possible to start a trace (provided there's a free TraceId)
 * even if there's no available memory.
 *
 * This code is written to be adaptable to allocating Trace objects
 * dynamically.  */

Res TraceCreate(Trace *traceReturn, Arena arena, int why)
{
  TraceId ti;
  Trace trace;

  AVER(traceReturn != NULL);
  AVERT(Arena, arena);

  /* Find a free trace ID */
  TRACE_SET_ITER(ti, trace, TraceSetComp(arena->busyTraces), arena)
    goto found;
  TRACE_SET_ITER_END(ti, trace, TraceSetComp(arena->busyTraces), arena);
  return ResLIMIT;              /* no trace IDs available */

found:
  trace = ArenaTrace(arena, ti);
  AVER(trace->sig == SigInvalid);       /* <design/arena/#trace.invalid> */

  trace->arena = arena;
  trace->why = why;
  trace->white = ZoneSetEMPTY;
  trace->mayMove = ZoneSetEMPTY;
  trace->ti = ti;
  trace->state = TraceINIT;
  trace->band = RankAMBIG;      /* Required to be the earliest rank. */
  trace->emergency = FALSE;
  trace->chain = NULL;
  STATISTIC(trace->preTraceArenaReserved = ArenaReserved(arena));
  trace->condemned = (Size)0;   /* nothing condemned yet */
  trace->notCondemned = (Size)0;
  trace->foundation = (Size)0;  /* nothing grey yet */
  trace->rate = (Size)0;        /* no scanning to be done yet */
  STATISTIC(trace->greySegCount = (Count)0);
  STATISTIC(trace->greySegMax = (Count)0);
  STATISTIC(trace->rootScanCount = (Count)0);
  trace->rootScanSize = (Size)0;
  trace->rootCopiedSize = (Size)0;
  STATISTIC(trace->segScanCount = (Count)0);
  trace->segScanSize = (Size)0; /* see .workclock */
  trace->segCopiedSize = (Size)0;
  STATISTIC(trace->singleScanCount = (Count)0);
  STATISTIC(trace->singleScanSize = (Size)0);
  STATISTIC(trace->singleCopiedSize = (Size)0);
  STATISTIC(trace->fixRefCount = (Count)0);
  STATISTIC(trace->segRefCount = (Count)0);
  STATISTIC(trace->whiteSegRefCount = (Count)0);
  STATISTIC(trace->nailCount = (Count)0);
  STATISTIC(trace->snapCount = (Count)0);
  STATISTIC(trace->readBarrierHitCount = (Count)0);
  STATISTIC(trace->pointlessScanCount = (Count)0);
  STATISTIC(trace->forwardedCount = (Count)0);
  trace->forwardedSize = (Size)0; /* see .message.data */
  STATISTIC(trace->preservedInPlaceCount = (Count)0);
  trace->preservedInPlaceSize = (Size)0;  /* see .message.data */
  STATISTIC(trace->reclaimCount = (Count)0);
  STATISTIC(trace->reclaimSize = (Size)0);
  trace->sig = TraceSig;
  arena->busyTraces = TraceSetAdd(arena->busyTraces, trace);
  AVERT(Trace, trace);

  /* We suspend the mutator threads so that the PoolWhiten methods */
  /* can calculate white sets without the mutator allocating in */
  /* buffers under our feet. */
  /* @@@@ This is a short-term fix for request.dylan.160098. */
  ShieldSuspend(arena);

  traceCopySizes(trace);

  *traceReturn = trace;
  return ResOK;
}


/* TraceDestroy -- destroy a trace object
 *
 * Finish and deallocate a Trace object, freeing up a TraceId.
 *
 * This code does not allow a Trace to be destroyed while it is active.
 * It would be possible to allow this, but the colours of segments
 * etc. would need to be reset to black.  This also means the error
 * paths in this file don't work.  @@@@ */

void TraceDestroy(Trace trace)
{
  AVERT(Trace, trace);
  AVER(trace->state == TraceFINISHED);

  if(trace->chain == NULL) {
    Ring chainNode, nextChainNode;

    /* Notify all the chains. */
    RING_FOR(chainNode, &trace->arena->chainRing, nextChainNode) {
      Chain chain = RING_ELT(Chain, chainRing, chainNode);

      ChainEndGC(chain, trace);
    }
  } else {
    ChainEndGC(trace->chain, trace);
  }

  STATISTIC_STAT(EVENT13
                  (TraceStatScan, trace,
                   trace->rootScanCount, trace->rootScanSize,
                   trace->rootCopiedSize,
                   trace->segScanCount, trace->segScanSize,
                   trace->segCopiedSize,
                   trace->singleScanCount, trace->singleScanSize,
                   trace->singleCopiedSize,
                   trace->readBarrierHitCount, trace->greySegMax,
                   trace->pointlessScanCount));
  STATISTIC_STAT(EVENT10
                  (TraceStatFix, trace,
                   trace->fixRefCount, trace->segRefCount,
                   trace->whiteSegRefCount,
                   trace->nailCount, trace->snapCount,
                   trace->forwardedCount, trace->forwardedSize,
                   trace->preservedInPlaceCount,
                   trace->preservedInPlaceSize));
  STATISTIC_STAT(EVENT3
                  (TraceStatReclaim, trace,
                   trace->reclaimCount, trace->reclaimSize));

  trace->sig = SigInvalid;
  trace->arena->busyTraces = TraceSetDel(trace->arena->busyTraces, trace);
  trace->arena->flippedTraces = TraceSetDel(trace->arena->flippedTraces, trace);
  EVENT1(TraceDestroy, trace);
}


/* traceReclaim -- reclaim the remaining objects white for this trace */

static void traceReclaim(Trace trace)
{
  Arena arena;
  Seg seg;
  Ring node, nextNode;

  AVER(trace->state == TraceRECLAIM);

  EVENT1(TraceReclaim, trace);
  arena = trace->arena;
  if(SegFirst(&seg, arena)) {
    Addr base;
    do {
      base = SegBase(seg);
      /* There shouldn't be any grey stuff left for this trace. */
      AVER_CRITICAL(!TraceSetIsMember(SegGrey(seg), trace));

      if(TraceSetIsMember(SegWhite(seg), trace)) {
        AVER_CRITICAL((SegPool(seg)->class->attr & AttrGC) != 0);
        STATISTIC(++trace->reclaimCount);
        PoolReclaim(SegPool(seg), trace, seg);

        /* If the segment still exists, it should no longer be white. */
        /* Note that the seg returned by this SegOfAddr may not be */
        /* the same as the one above, but in that case it's new and */
        /* still shouldn't be white for this trace. */

        /* The code from the class-specific reclaim methods to */
        /* unwhiten the segment could in fact be moved here.   */
        {
          Seg nonWhiteSeg = NULL;       /* prevents compiler warning */
          AVER_CRITICAL(!(SegOfAddr(&nonWhiteSeg, arena, base)
                          && TraceSetIsMember(SegWhite(nonWhiteSeg), trace)));
          UNUSED(nonWhiteSeg); /* <code/mpm.c#check.unused> */
        }
      }
    } while(SegNext(&seg, arena, base));
  }

  trace->state = TraceFINISHED;

  /* Call each pool's TraceEnd method -- do end-of-trace work */
  RING_FOR(node, &ArenaGlobals(arena)->poolRing, nextNode) {
    Pool pool = RING_ELT(Pool, arenaRing, node);
    PoolTraceEnd(pool, trace);
  }

NOTREACHED;

  ArenaCompact(arena, trace);  /* let arenavm drop chunks */

  TracePostMessage(trace);  /* trace end */
  /* Immediately pre-allocate messages for next time; failure is okay */
  (void)TraceIdMessagesCreate(arena, trace->ti);
}


/* traceFindGrey -- find a grey segment
 *
 * This function finds the next segment to scan.  It does this according
 * to the current band of the trace.  See design/trace/
 *
 * This code also performs various checks about the ranks of the object
 * graph.  Explanations of the checks would litter the code, so the
 * explanations are here, and the code references these.
 *
 * .check.ambig.not: RankAMBIG segments never appear on the grey ring.
 * The current tracer cannot support ambiguous reference except as
 * roots, so it's a buf if we ever find any.  This behaviour is not set
 * in stone, it's possible to imagine changing the tracer so that we can
 * support ambiguous objects one day.  For example, a fully conservative
 * non-moving mode.
 *
 * .check.band.begin: At the point where we start working on a new band
 * of Rank R, there are no grey objects at earlier ranks.  If there
 * were, we would've found them whilst the current band was the previous
 * band.  We don't check this, but I rely on this fact in the next
 * check, .check.weak.no-preserve.
 *
 * .check.weak.band: Weak references cannot cause objects to be
 * newly preserved (marked).  Because of .check.band.begin all the
 * scanning work performed when the current band is a weak rank will be
 * scanning objects at that rank.  There is currently only one weak
 * rank, RankWEAK.
 *
 * .check.final.one-pass: Because all the RankFINAL references are
 * allocated in PoolMRG and effectively treated as roots, all the
 * RankFINAL references will be scanned in one push (possibly split up,
 * incrementally).  Once they have been scanned, no new RankFINAL
 * references will be discovered (the mutator is not permitted to
 * allocate RankFINAL references wherever they like).  In fact because
 * of various coincidences (no Ambig segments so band Exact never
 * discovers an Ambig segment and then more Exact segments; the only
 * other rank is weak so never discovers any new segments) it is the
 * case that for any band R there is an initial burst of scanning
 * segments at rank R then after that we see no more rank R segments
 * whilst working in this band.  That's what we check, although we
 * expect to have to change the check if we introduce more ranks, or
 * start changing the semantics of them.  A flag is used to implement
 * this check.
 */

static Bool traceFindGrey(Seg *segReturn, Rank *rankReturn,
                          Arena arena, TraceId ti)
{
  Rank rank;
  Trace trace;
  Ring node, nextNode;

  AVER(segReturn != NULL);
  AVER(TraceIdCheck(ti));

  trace = ArenaTrace(arena, ti);

  while(1) {
    Rank band = traceBand(trace);

    /* Within the R band we look for segments of rank R first,  */
    /* then succesively earlier ones.  Slight hack: We never    */
    /* expect to find any segments of RankAMBIG, so we use      */
    /* this as a terminating condition for the loop.            */
    for(rank = band; rank > RankAMBIG; --rank) {
      RING_FOR(node, ArenaGreyRing(arena, rank), nextNode) {
        Seg seg = SegOfGreyRing(node);

        AVERT(Seg, seg);
        AVER(SegGrey(seg) != TraceSetEMPTY);
        AVER(RankSetIsMember(SegRankSet(seg), rank));

        if(TraceSetIsMember(SegGrey(seg), trace)) {
          /* .check.band.weak */
          AVER(band != RankWEAK || rank == band);
          if(rank != band) {
            traceBandFirstStretchDone(trace);
          } else {
            /* .check.final.one-pass */
            AVER(traceBandFirstStretch(trace));
          }
          *segReturn = seg;
          *rankReturn = rank;
          DIAG( traceFindGrey_diag(TRUE, rank); );
          return TRUE;
        }
      }
    }
    /* .check.ambig.not */
    AVER(RingIsSingle(ArenaGreyRing(arena, RankAMBIG)));
    if(!traceBandAdvance(trace)) {
      /* No grey segments for this trace. */
      DIAG( traceFindGrey_diag(FALSE, rank); );
      return FALSE;
    }
  }
}


/* diagnostic output for traceFindGrey */
DIAG_DECL( 
static void traceFindGrey_diag(Bool found, Rank rank)
{
  char this;
  static char prev = '.';
  static int segcount;
  static char report_array[20];
  static char *report_lim;
  int report_maxchars = sizeof(report_array) - 2; /* '.' + '\0' */

  this = (char)(!found ? '.'
                : (rank == RankAMBIG) ? 'A'
                : (rank == RankEXACT) ? 'E'
                : (rank == RankFINAL) ? 'F'
                : (rank == RankWEAK) ? 'W'
                : '?');

  if(prev == '.') {
    /* First seg of new trace */
    prev = this;
    segcount = 0;
    report_lim = report_array;
  }

  if(this == prev) {
    segcount += 1;
  } else {
    /* Change of rank: add prev rank and segcount to report */
    if((report_lim - report_array) + 2 > report_maxchars) {
      /* no space to add 2 chars */
      report_array[0] = '!';
    } else {
      /* prev rank */
      *report_lim++ = prev;
      /* prev rank's segcount [0..9, a..z (x10), or *] */
      if(segcount < 10) {
        *report_lim++ = (char)('0' + segcount);
      } else if(segcount < 260) {
        *report_lim++ = (char)(('a' - 1) + (segcount / 10));
      } else {
        *report_lim++ = '*';
      }
    }
    /* begin new rank */
    prev = this;
    segcount = 1;
  }
  
  if(!found) {
    /* No more grey in this trace: output report */
    AVER(this == '.');
    AVER(segcount == 1);  /* single failed attempt to find a seg */
    *report_lim++ = this;
    *report_lim++ = '\0';
    DIAG_SINGLEF(( "traceFindGrey",
                   "rank sequence: $S",
                   (WriteFS)report_array,
                   NULL ));
  }
  return;
}
)

/* ScanStateSetSummary -- set the summary of scanned references
 *
 * This function sets unfixedSummary and fixedSummary such that
 * ScanStateSummary will return the summary passed.  Subsequently fixed
 * references are accumulated into this result.  */

void ScanStateSetSummary(ScanState ss, RefSet summary)
{
  AVERT(ScanState, ss);
  /* Can't check summary, as it can be anything. */

  ss->unfixedSummary = RefSetEMPTY;
  ss->fixedSummary = summary;
  AVER(ScanStateSummary(ss) == summary);
}


/* ScanStateSummary -- calculate the summary of scanned references
 *
 * The summary of the scanned references is the summary of the unfixed
 * references, minus the white set, plus the summary of the fixed
 * references.  This is because TraceFix is called for all references in
 * the white set, and accumulates a summary of references after they
 * have been fixed.  */

RefSet ScanStateSummary(ScanState ss)
{
  AVERT(ScanState, ss);

  return RefSetUnion(ss->fixedSummary,
                     RefSetDiff(ss->unfixedSummary, ss->white));
}


/* traceScanSegRes -- scan a segment to remove greyness
 *
 * @@@@ During scanning, the segment should be write-shielded to prevent
 * any other threads from updating it while fix is being applied to it
 * (because fix is not atomic).  At the moment, we don't bother, because
 * we know that all threads are suspended.  */

static Res traceScanSegRes(TraceSet ts, Rank rank, Arena arena, Seg seg)
{
  Bool wasTotal;
  ZoneSet white;
  Res res;

  /* The reason for scanning a segment is that it's grey. */
  AVER(TraceSetInter(ts, SegGrey(seg)) != TraceSetEMPTY);
  EVENT4(TraceScanSeg, ts, rank, arena, seg);

  white = traceSetWhiteUnion(ts, arena);

  /* Only scan a segment if it refers to the white set. */
  if(ZoneSetInter(white, SegSummary(seg)) == ZoneSetEMPTY) {
    PoolBlacken(SegPool(seg), ts, seg);
    /* Setup result code to return later. */
    res = ResOK;
  } else {      /* scan it */
    ScanStateStruct ss;
    ScanStateInit(&ss, ts, arena, rank, white);

    /* Expose the segment to make sure we can scan it. */
    ShieldExpose(arena, seg);
    res = PoolScan(&wasTotal, &ss, SegPool(seg), seg);
    /* Cover, regardless of result */
    ShieldCover(arena, seg);

    traceSetUpdateCounts(ts, arena, &ss, traceAccountingPhaseSegScan);
    /* Count segments scanned pointlessly */
    STATISTIC_STAT
      ({
         TraceId ti; Trace trace;
         Count whiteSegRefCount = 0;

         TRACE_SET_ITER(ti, trace, ts, arena)
           whiteSegRefCount += trace->whiteSegRefCount;
         TRACE_SET_ITER_END(ti, trace, ts, arena);
         if(whiteSegRefCount == 0)
           TRACE_SET_ITER(ti, trace, ts, arena)
             ++trace->pointlessScanCount;
           TRACE_SET_ITER_END(ti, trace, ts, arena);
      });

    /* Following is true whether or not scan was total. */
    /* See <design/scan/#summary.subset>. */
    /* .verify.segsummary: were the seg contents, as found by this 
     * scan, consistent with the recorded SegSummary?
     */
    AVER(RefSetSub(ss.unfixedSummary, SegSummary(seg)));

    if(res != ResOK || !wasTotal) {
      /* scan was partial, so... */
      /* scanned summary should be ORed into segment summary. */
      SegSetSummary(seg, RefSetUnion(SegSummary(seg), ScanStateSummary(&ss)));
    } else {
      /* all objects on segment have been scanned, so... */
      /* scanned summary should replace the segment summary. */
      SegSetSummary(seg, ScanStateSummary(&ss));
    }

    ScanStateFinish(&ss);
  }

  if(res == ResOK) {
    /* The segment is now black only if scan was successful. */
    /* Remove the greyness from it. */
    SegSetGrey(seg, TraceSetDiff(SegGrey(seg), ts));
  }

  return res;
}


/* traceScanSeg
 *
 * Scans a segment without fail.  May put the traces into emergency mode
 * to ensure this.  */

static void traceScanSeg(TraceSet ts, Rank rank, Arena arena, Seg seg)
{
  Res res;

  res = traceScanSegRes(ts, rank, arena, seg);
  if(res != ResOK) {
    AVER(ResIsAllocFailure(res));
    traceSetSignalEmergency(ts, arena);
    res = traceScanSegRes(ts, rank, arena, seg);
    /* Should be OK in emergency mode. */
  }
  AVER(ResOK == res);

  return;
}


/* TraceSegAccess -- handle barrier hit on a segment */

void TraceSegAccess(Arena arena, Seg seg, AccessSet mode)
{
  TraceId ti;

  AVERT(Arena, arena);
  AVERT(Seg, seg);

  /* If it's a read access, then the segment must be grey for a trace */
  /* which is flipped. */
  AVER((mode & SegSM(seg) & AccessREAD) == 0
       || TraceSetInter(SegGrey(seg), arena->flippedTraces) != TraceSetEMPTY);

  /* If it's a write acess, then the segment must have a summary that */
  /* is smaller than the mutator's summary (which is assumed to be */
  /* RefSetUNIV). */
  AVER((mode & SegSM(seg) & AccessWRITE) == 0 || SegSummary(seg) != RefSetUNIV);

  EVENT3(TraceAccess, arena, seg, mode);

  if((mode & SegSM(seg) & AccessREAD) != 0) {   /* read barrier? */
    /* Pick set of traces to scan for: */
    TraceSet traces = arena->flippedTraces;

    /* .scan.conservative: At the moment we scan at RankEXACT.  Really */
    /* we should be scanning at the "phase" of the trace, which is the */
    /* minimum rank of all grey segments. (see request.mps.170160) */
    traceScanSeg(traces, RankEXACT, arena, seg);

    /* The pool should've done the job of removing the greyness that */
    /* was causing the segment to be protected, so that the mutator */
    /* can go ahead and access it. */
    AVER(TraceSetInter(SegGrey(seg), traces) == TraceSetEMPTY);

    STATISTIC_STAT({
      Trace trace;

      TRACE_SET_ITER(ti, trace, traces, arena)
        ++trace->readBarrierHitCount;
      TRACE_SET_ITER_END(ti, trace, traces, arena);
    });
  } else {              /* write barrier */
    STATISTIC(++arena->writeBarrierHitCount);
  }

  /* The write barrier handling must come after the read barrier, */
  /* because the latter may set the summary and raise the write barrier. */
  if((mode & SegSM(seg) & AccessWRITE) != 0)      /* write barrier? */
    SegSetSummary(seg, RefSetUNIV);

  /* The segment must now be accessible. */
  AVER((mode & SegSM(seg)) == AccessSetEMPTY);
}


/* TraceFix -- fix a reference */

Res TraceFix(ScanState ss, Ref *refIO)
{
  Ref ref;
  Tract tract;
  Pool pool;

  /* See <design/trace/#fix.noaver> */
  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(refIO != NULL);

  ref = *refIO;

  STATISTIC(++ss->fixRefCount);
  EVENT4(TraceFix, ss, refIO, ref, ss->rank);

  TRACT_OF_ADDR(&tract, ss->arena, ref);
  if(tract) {
    if(TraceSetInter(TractWhite(tract), ss->traces) != TraceSetEMPTY) {
      Seg seg;
      if(TRACT_SEG(&seg, tract)) {
        Res res;
        STATISTIC(++ss->segRefCount);
        STATISTIC(++ss->whiteSegRefCount);
        EVENT1(TraceFixSeg, seg);
        EVENT0(TraceFixWhite);
        pool = TractPool(tract);
        /* Could move the rank switch here from the class-specific */
        /* fix methods. */
        res = PoolFix(pool, ss, seg, refIO);
        if(res != ResOK) {
          /* Fix protocol (de facto): if Fix fails, ref must be unchanged */
          /* Justification for this restriction:
           * A: it simplifies;
           * B: it's reasonable (given what may cause Fix to fail);
           * C: the code (here) already assumes this: it returns without 
           *    updating ss->fixedSummary.  RHSK 2007-03-21.
           */
          AVER(*refIO == ref);
          return res;
        }
      }
    } else {
      /* Tract isn't white. Don't compute seg for non-statistical */
      /* variety. See <design/trace/#fix.tractofaddr> */
      STATISTIC_STAT
        ({
          Seg seg;
          if(TRACT_SEG(&seg, tract)) {
            ++ss->segRefCount;
            EVENT1(TraceFixSeg, seg);
          }
        });
    }
  } else {
    /* See <design/trace/#exact.legal> */
    AVER(ss->rank < RankEXACT
         || !ArenaIsReservedAddr(ss->arena, ref));
  }

  /* See <design/trace/#fix.fixed.all> */
  ss->fixedSummary = RefSetAdd(ss->arena, ss->fixedSummary, *refIO);

  return ResOK;
}


/* TraceFixEmergency -- fix a reference in emergency mode */

Res TraceFixEmergency(ScanState ss, Ref *refIO)
{
  Ref ref;
  Tract tract;
  Pool pool;

  AVERT(ScanState, ss);
  AVER(refIO != NULL);

  ref = *refIO;

  STATISTIC(++ss->fixRefCount);
  EVENT4(TraceFix, ss, refIO, ref, ss->rank);

  TRACT_OF_ADDR(&tract, ss->arena, ref);
  if(tract) {
    if(TraceSetInter(TractWhite(tract), ss->traces) != TraceSetEMPTY) {
      Seg seg;
      if(TRACT_SEG(&seg, tract)) {
        STATISTIC(++ss->segRefCount);
        STATISTIC(++ss->whiteSegRefCount);
        EVENT1(TraceFixSeg, seg);
        EVENT0(TraceFixWhite);
        pool = TractPool(tract);
        PoolFixEmergency(pool, ss, seg, refIO);
      }
    } else {
      /* Tract isn't white. Don't compute seg for non-statistical */
      /* variety. See <design/trace/#fix.tractofaddr> */
      STATISTIC_STAT
        ({
          Seg seg;
          if(TRACT_SEG(&seg, tract)) {
            ++ss->segRefCount;
            EVENT1(TraceFixSeg, seg);
          }
        });
    }
  } else {
    /* See <design/trace/#exact.legal> */
    AVER(ss->rank < RankEXACT ||
         !ArenaIsReservedAddr(ss->arena, ref));
  }

  /* See <design/trace/#fix.fixed.all> */
  ss->fixedSummary = RefSetAdd(ss->arena, ss->fixedSummary, *refIO);

  return ResOK;
}


/* traceScanSingleRefRes -- scan a single reference, with result code */

static Res traceScanSingleRefRes(TraceSet ts, Rank rank, Arena arena,
                                 Seg seg, Ref *refIO)
{
  RefSet summary;
  ZoneSet white;
  Res res;
  ScanStateStruct ss;

  EVENT4(TraceScanSingleRef, ts, rank, arena, (Addr)refIO);

  white = traceSetWhiteUnion(ts, arena);
  if(ZoneSetInter(SegSummary(seg), white) == ZoneSetEMPTY) {
    return ResOK;
  }

  ScanStateInit(&ss, ts, arena, rank, white);
  ShieldExpose(arena, seg);

  TRACE_SCAN_BEGIN(&ss) {
    res = TRACE_FIX(&ss, refIO);
  } TRACE_SCAN_END(&ss);
  ss.scannedSize = sizeof *refIO;

  summary = SegSummary(seg);
  summary = RefSetAdd(arena, summary, *refIO);
  SegSetSummary(seg, summary);
  ShieldCover(arena, seg);

  traceSetUpdateCounts(ts, arena, &ss, traceAccountingPhaseSingleScan);
  ScanStateFinish(&ss);

  return res;
}


/* TraceScanSingleRef -- scan a single reference
 *
 * This one can't fail.  It may put the traces into emergency mode in
 * order to achieve this.  */

void TraceScanSingleRef(TraceSet ts, Rank rank, Arena arena,
                        Seg seg, Ref *refIO)
{
  Res res;

  AVER(TraceSetCheck(ts));
  AVER(RankCheck(rank));
  AVERT(Arena, arena);
  AVER(SegCheck(seg));
  AVER(refIO != NULL);

  res = traceScanSingleRefRes(ts, rank, arena, seg, refIO);
  if(res != ResOK) {
    traceSetSignalEmergency(ts, arena);
    res = traceScanSingleRefRes(ts, rank, arena, seg, refIO);
    /* Ought to be OK in emergency mode now. */
  }
  AVER(ResOK == res);

  return;
}


/* TraceScanArea -- scan contiguous area of references
 *
 * This is a convenience function for scanning the contiguous area
 * [base, limit).  I.e., it calls Fix on all words from base up to
 * limit, inclusive of base and exclusive of limit.  */

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
    if(!TRACE_FIX1(ss, ref))
      goto loop;
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
 * tagged as Dylan references (i.e., bottom two bits are zero).  @@@@
 * This Dylan-specificness should be generalized in some way.  */

Res TraceScanAreaTagged(ScanState ss, Addr *base, Addr *limit)
{
  return TraceScanAreaMasked(ss, base, limit, (Word)3);
}


/* TraceScanAreaMasked -- scan contiguous area of filtered references
 *
 * This is as TraceScanArea except words are only fixed if they are zero
 * when masked with a mask.  */

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
    if(res == ResOK)
      goto loop;
    return res;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(ss);

  return ResOK;
}


/* traceCondemnAll -- condemn everything and notify all the chains */

static Res traceCondemnAll(Trace trace)
{
  Res res;
  Arena arena;
  Ring chainNode, nextChainNode;
  Bool haveWhiteSegs = FALSE;

  arena = trace->arena;
  AVERT(Arena, arena);
  /* Condemn all the chains. */
  RING_FOR(chainNode, &arena->chainRing, nextChainNode) {
    Chain chain = RING_ELT(Chain, chainRing, chainNode);

    AVERT(Chain, chain);
    res = ChainCondemnAll(chain, trace);
    if(res != ResOK)
      goto failBegin;
    haveWhiteSegs = TRUE;
  }
  /* Notify all the chains. */
  RING_FOR(chainNode, &arena->chainRing, nextChainNode) {
    Chain chain = RING_ELT(Chain, chainRing, chainNode);

    ChainStartGC(chain, trace);
  }
  return ResOK;

failBegin:
  AVER(!haveWhiteSegs); /* Would leave white sets inconsistent. */
  return res;
}


/* Collection control parameters */

double TraceTopGenMortality = 0.51;
double TraceWorkFactor = 0.25;


/* TraceStart -- condemn a set of objects and start collection
 *
 * TraceStart should be passed a trace with state TraceINIT, i.e.,
 * recently returned from TraceCreate, with some condemned segments
 * added.  mortality is the fraction of the condemned set expected to
 * survive.  finishingTime is relative to the current polling clock, see
 * <design/arena/#poll.clock>.
 *
 * .start.black: All segments are black w.r.t. a newly allocated trace.
 * However, if TraceStart initialized segments to black when it
 * calculated the grey set then this condition could be relaxed, making
 * it easy to destroy traces half-way through.  */

static Res rootGrey(Root root, void *p)
{
  Trace trace = (Trace)p;

  AVERT(Root, root);
  AVERT(Trace, trace);

  if(ZoneSetInter(RootSummary(root), trace->white) != ZoneSetEMPTY) {
    RootGrey(root, trace);
  }

  return ResOK;
}


static void TraceStartGenDesc_diag(GenDesc desc, Bool top, Index i)
{
  Ring n, nn;
  
#if !defined(DIAG_WITH_STREAM_AND_WRITEF)
  UNUSED(i);
#endif

  if(top) {
    DIAG_WRITEF(( DIAG_STREAM,
      "         GenDesc [top]",
      NULL ));
  } else {
    DIAG_WRITEF(( DIAG_STREAM,
      "         GenDesc [$U]", (WriteFU)i,
      NULL ));
  }
  DIAG_WRITEF(( DIAG_STREAM,
    " $P capacity: $U KiB, mortality $D\n",
    (WriteFP)desc, (WriteFU)desc->capacity, (WriteFD)desc->mortality,
    "         ZoneSet:$B\n", (WriteFB)desc->zones,
    NULL ));
  RING_FOR(n, &desc->locusRing, nn) {
    DIAG_DECL( PoolGen gen = RING_ELT(PoolGen, genRing, n); )
    DIAG_WRITEF(( DIAG_STREAM,
      "           PoolGen $U ($S)", 
      (WriteFU)gen->nr, (WriteFS)gen->pool->class->name,
      " totalSize $U", (WriteFU)gen->totalSize,
      " newSize $U\n", (WriteFU)gen->newSizeAtCreate,
      NULL ));
  }
}

void TraceStart(Trace trace, double mortality, double finishingTime)
{
  Arena arena;
  Res res;
  Seg seg;
  Size size;

  AVERT(Trace, trace);
  AVER(trace->state == TraceINIT);
  AVER(0.0 <= mortality);
  AVER(mortality <= 1.0);
  AVER(finishingTime >= 0.0);

  arena = trace->arena;

  /* From the already set up white set, derive a grey set. */

  /* @@@@ Instead of iterating over all the segments, we could */
  /* iterate over all pools which are scannable and thence over */
  /* all their segments.  This might be better if the minority */
  /* of segments are scannable.  Perhaps we should choose */
  /* dynamically which method to use. */

  if(SegFirst(&seg, arena)) {
    Addr base;
    do {
      base = SegBase(seg);
      size = SegSize(seg);
      AVER(!TraceSetIsMember(SegGrey(seg), trace));

      /* A segment can only be grey if it contains some references. */
      /* This is indicated by the rankSet begin non-empty.  Such */
      /* segments may only belong to scannable pools. */
      if(SegRankSet(seg) != RankSetEMPTY) {
        /* Segments with ranks may only belong to scannable pools. */
        AVER((SegPool(seg)->class->attr & AttrSCAN) != 0);

        /* Turn the segment grey if there might be a reference in it */
        /* to the white set.  This is done by seeing if the summary */
        /* of references in the segment intersects with the */
        /* approximation to the white set. */
        if(ZoneSetInter(SegSummary(seg), trace->white) != ZoneSetEMPTY) {
          /* Note: can a white seg get greyed as well?  At this point */
          /* we still assume it may.  (This assumption runs out in */
          /* PoolTrivGrey). */
          PoolGrey(SegPool(seg), trace, seg);
          if(TraceSetIsMember(SegGrey(seg), trace)) {
            trace->foundation += size;
          }
        }

        if((SegPool(seg)->class->attr & AttrGC)
            && !TraceSetIsMember(SegWhite(seg), trace)) {
          trace->notCondemned += size;
        }
      }
    } while (SegNext(&seg, arena, base));
  }

  DIAG_FIRSTF(( "TraceStart",
    "because code $U: $S\n",
    (WriteFU)trace->why, (WriteFS)TraceStartWhyToString(trace->why),
    NULL ));

  DIAG( ArenaDescribe(arena, DIAG_STREAM); );

  DIAG_MOREF((
    "       white set:$B\n",
    (WriteFB)trace->white,
    NULL ));

  {
    /* @@ */
    /* Iterate over all chains, all GenDescs within a chain, */
    /* (and all PoolGens within a GenDesc).  */
    Ring node, nextNode;
    Index i;

    RING_FOR(node, &arena->chainRing, nextNode) {
      Chain chain = RING_ELT(Chain, chainRing, node);
      DIAG_WRITEF(( DIAG_STREAM,
        "       Chain $P\n", (WriteFP)chain,
        NULL ));

      for(i = 0; i < chain->genCount; ++i) {
        GenDesc desc = &chain->gens[i];
        TraceStartGenDesc_diag(desc, FALSE, i);
      }
    }

    /* Now do topgen GenDesc (and all PoolGens within it). */
    DIAG_WRITEF(( DIAG_STREAM,
      "       topGen\n",
      NULL ));
    TraceStartGenDesc_diag(&arena->topGen, TRUE, 0);
  }
  
  DIAG_END( "TraceStart" );

  res = RootsIterate(ArenaGlobals(arena), rootGrey, (void *)trace);
  AVER(res == ResOK);

  STATISTIC_STAT(EVENT2(ArenaWriteFaults, arena, arena->writeBarrierHitCount));

  /* Calculate the rate of scanning. */
  {
    Size sSurvivors = (Size)(trace->condemned * (1.0 - mortality));
    double nPolls = finishingTime / ArenaPollALLOCTIME;

    /* There must be at least one poll. */
    if(nPolls < 1.0)
      nPolls = 1.0;
    /* We use casting to long to truncate nPolls down to the nearest */
    /* integer, so try to make sure it fits. */
    if(nPolls >= (double)LONG_MAX)
      nPolls = (double)LONG_MAX;
    /* rate equals scanning work per number of polls available */
    trace->rate = (trace->foundation + sSurvivors) / (unsigned long)nPolls + 1;
  }

  /* @@ DIAG for rate of scanning here. */

  STATISTIC_STAT(EVENT7(TraceStatCondemn, trace,
                               trace->condemned, trace->notCondemned,
                               trace->foundation, trace->rate,
                               mortality, finishingTime));

  trace->state = TraceUNFLIPPED;
  TracePostStartMessage(trace);

  /* All traces must flip at beginning at the moment. */
  traceFlip(trace);

  return;
}


/* traceWorkClock -- a measure of the work done for this trace
 *
 * .workclock: Segment and root scanning work is the regulator.  */

#define traceWorkClock(trace) ((trace)->segScanSize + (trace)->rootScanSize)


/* TraceQuantum -- progresses a trace by one quantum */

void TraceQuantum(Trace trace)
{
  Size pollEnd;

  pollEnd = traceWorkClock(trace) + trace->rate;
  do {
    switch(trace->state) {
      case TraceUNFLIPPED:
        /* all traces are flipped in TraceStart at the moment */
        NOTREACHED;
        break;
      case TraceFLIPPED: {
        Arena arena = trace->arena;
        Seg seg;
        Rank rank;

        if(traceFindGrey(&seg, &rank, arena, trace->ti)) {
          AVER((SegPool(seg)->class->attr & AttrSCAN) != 0);
          traceScanSeg(TraceSetSingle(trace), rank, arena, seg);
        } else {
          trace->state = TraceRECLAIM;
        }
        break;
      }
      case TraceRECLAIM:
        traceReclaim(trace);
        break;
      default:
        NOTREACHED;
        break;
    }
  } while(trace->state != TraceFINISHED
          && (trace->emergency || traceWorkClock(trace) < pollEnd));
}

/* TraceStartCollectAll: start a trace which condemns everything in
 * the arena.
 *
 * "why" is a TraceStartWhy* enum member that specifies why the
 * collection is starting. */

Res TraceStartCollectAll(Trace *traceReturn, Arena arena, int why)
{
  Trace trace = NULL;
  Res res;
  double finishingTime;

  AVERT(Arena, arena);
  AVER(arena->busyTraces == TraceSetEMPTY);

  res = TraceCreate(&trace, arena, why);
  AVER(res == ResOK); /* succeeds because no other trace is busy */
  res = traceCondemnAll(trace);
  if(res != ResOK) /* should try some other trace, really @@@@ */
    goto failCondemn;
  finishingTime = ArenaAvail(arena)
                  - trace->condemned * (1.0 - TraceTopGenMortality);
  if(finishingTime < 0) {
    /* Run out of time, should really try a smaller collection. @@@@ */
    finishingTime = 0.0;
  }
  TraceStart(trace, TraceTopGenMortality, finishingTime);
  *traceReturn = trace;
  return ResOK;

failCondemn:
  TraceDestroy(trace);
  return res;
}


/* TracePoll -- Check if there's any tracing work to be done */

Size TracePoll(Globals globals)
{
  Trace trace;
  Res res;
  Arena arena;
  Size scannedSize;

  AVERT(Globals, globals);
  arena = GlobalsArena(globals);

  scannedSize = (Size)0;
  if(arena->busyTraces == TraceSetEMPTY) {
    /* If no traces are going on, see if we need to start one. */
    Size sFoundation, sCondemned, sSurvivors, sConsTrace;
    double tTracePerScan; /* tTrace/cScan */
    double dynamicDeferral;

    /* Compute dynamic criterion.  See strategy.lisp-machine. */
    AVER(TraceTopGenMortality >= 0.0);
    AVER(TraceTopGenMortality <= 1.0);
    sFoundation = (Size)0; /* condemning everything, only roots @@@@ */
    /* @@@@ sCondemned should be scannable only */
    sCondemned = ArenaCommitted(arena) - ArenaSpareCommitted(arena);
    sSurvivors = (Size)(sCondemned * (1 - TraceTopGenMortality));
    tTracePerScan = sFoundation + (sSurvivors * (1 + TraceCopyScanRATIO));
    AVER(TraceWorkFactor >= 0);
    AVER(sSurvivors + tTracePerScan * TraceWorkFactor <= (double)SizeMAX);
    sConsTrace = (Size)(sSurvivors + tTracePerScan * TraceWorkFactor);
    dynamicDeferral = (double)ArenaAvail(arena) - (double)sConsTrace;

    if(dynamicDeferral < 0.0) { /* start full GC */
      res = TraceStartCollectAll(&trace, arena, TraceStartWhyDYNAMICCRITERION);
      if(res != ResOK)
        goto failStart;
      scannedSize = traceWorkClock(trace);
    } else { /* Find the nursery most over its capacity. */
      Ring node, nextNode;
      double firstTime = 0.0;
      Chain firstChain = NULL;

      RING_FOR(node, &arena->chainRing, nextNode) {
        Chain chain = RING_ELT(Chain, chainRing, node);
        double time;

        AVERT(Chain, chain);
        time = ChainDeferral(chain);
        if(time < firstTime) {
          firstTime = time; firstChain = chain;
        }
      }

      /* If one was found, start collection on that chain. */
      if(firstTime < 0) {
        double mortality;

        res = TraceCreate(&trace, arena, TraceStartWhyCHAIN_GEN0CAP);
        AVER(res == ResOK);
        res = ChainCondemnAuto(&mortality, firstChain, trace);
        if(res != ResOK) /* should try some other trace, really @@@@ */
          goto failCondemn;
        trace->chain = firstChain;
        ChainStartGC(firstChain, trace);
        TraceStart(trace, mortality, trace->condemned * TraceWorkFactor);
        scannedSize = traceWorkClock(trace);
      }
    } /* (dynamicDeferral > 0.0) */
  } /* (arena->busyTraces == TraceSetEMPTY) */

  /* If there is a trace, do one quantum of work. */
  if(arena->busyTraces != TraceSetEMPTY) {
    Size oldScanned;

    trace = ArenaTrace(arena, (TraceId)0);
    AVER(arena->busyTraces == TraceSetSingle(trace));
    oldScanned = traceWorkClock(trace);
    TraceQuantum(trace);
    scannedSize = traceWorkClock(trace) - oldScanned;
    if(trace->state == TraceFINISHED) {
      TraceDestroy(trace);
    }
  }
  return scannedSize;

failCondemn:
  TraceDestroy(trace);
failStart:
  return (Size)0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2003, 2006, 2007 Ravenbrook Limited
 * <http://www.ravenbrook.com/>.
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

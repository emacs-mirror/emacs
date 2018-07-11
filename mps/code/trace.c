/* trace.c: GENERIC TRACER IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2018 Ravenbrook Limited.
 * See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .design: <design/trace/>.  */

#include "locus.h"
#include "mpm.h"
#include <limits.h> /* for LONG_MAX */

SRCID(trace, "$Id$");

/* Forward declarations */
Rank traceBand(Trace);
Bool traceBandAdvance(Trace);
Bool traceBandFirstStretch(Trace);
void traceBandFirstStretchDone(Trace);

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
  /* Can't check ss->fixClosure. */
  CHECKL(ScanStateZoneShift(ss) == ss->arena->zoneShift);
  white = ZoneSetEMPTY;
  TRACE_SET_ITER(ti, trace, ss->traces, ss->arena)
    white = ZoneSetUnion(white, ss->arena->trace[ti].white);
  TRACE_SET_ITER_END(ti, trace, ss->traces, ss->arena);
  CHECKL(ScanStateWhite(ss) == white);
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

  AVERT(TraceSet, ts);
  AVERT(Arena, arena);
  AVERT(Rank, rank);
  /* white is arbitrary and can't be checked */

  /* NOTE: We can only currently support scanning for a set of traces
     with the same fix method. To remove this restriction, it would be
     necessary to dispatch to the fix methods of sets of traces in
     TraceFix. */
  ss->fix = NULL;
  ss->fixClosure = NULL;
  TRACE_SET_ITER(ti, trace, ts, arena) {
    if (ss->fix == NULL) {
      ss->fix = trace->fix;
      ss->fixClosure = trace->fixClosure;
    } else {
      AVER(ss->fix == trace->fix);
      AVER(ss->fixClosure == trace->fixClosure);
    }
  } TRACE_SET_ITER_END(ti, trace, ts, arena);
  AVER(ss->fix != NULL);

  /* If the fix method is the normal GC fix, then we optimise the test for
     whether it's an emergency or not by updating the dispatch here, once. */
  if (ss->fix == SegFix && ArenaEmergency(arena))
        ss->fix = SegFixEmergency;

  ss->rank = rank;
  ss->traces = ts;
  ScanStateSetZoneShift(ss, arena->zoneShift);
  ScanStateSetUnfixedSummary(ss, RefSetEMPTY);
  ss->fixedSummary = RefSetEMPTY;
  ss->arena = arena;
  ss->wasMarked = TRUE;
  ScanStateSetWhite(ss, white);
  STATISTIC(ss->fixRefCount = (Count)0);
  STATISTIC(ss->segRefCount = (Count)0);
  STATISTIC(ss->whiteSegRefCount = (Count)0);
  STATISTIC(ss->nailCount = (Count)0);
  STATISTIC(ss->snapCount = (Count)0);
  STATISTIC(ss->forwardedCount = (Count)0);
  STATISTIC(ss->preservedInPlaceCount = (Count)0);
  STATISTIC(ss->copiedSize = (Size)0);
  ss->scannedSize = (Size)0; /* see .work */
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
  CHECKD_NOSIG(Ring, &trace->genRing);
  /* Use trace->state to check more invariants. */
  switch(trace->state) {
    case TraceINIT:
      CHECKL(!TraceSetIsMember(trace->arena->flippedTraces, trace));
      /* @@@@ What can be checked here? */
      break;

    case TraceUNFLIPPED:
      CHECKL(!RingIsSingle(&trace->genRing));
      CHECKL(!TraceSetIsMember(trace->arena->flippedTraces, trace));
      /* @@@@ Assert that mutator is grey for trace. */
      break;

    case TraceFLIPPED:
      CHECKL(!RingIsSingle(&trace->genRing));
      CHECKL(TraceSetIsMember(trace->arena->flippedTraces, trace));
      CHECKL(RankCheck(trace->band));
      /* @@@@ Assert that mutator is black for trace. */
      break;

    case TraceRECLAIM:
      CHECKL(!RingIsSingle(&trace->genRing));
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
  CHECKL(FUNCHECK(trace->fix));
  /* Can't check trace->fixClosure. */

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
    trace->band = RankMIN;
    return FALSE;
  }
  EVENT3(TraceBandAdvance, trace->arena, trace->ti, trace->band);
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
      STATISTIC(trace->rootCopiedSize += ss->copiedSize);
      STATISTIC(++trace->rootScanCount);
      break;
    }
    case traceAccountingPhaseSegScan: {
      trace->segScanSize += ss->scannedSize; /* see .work */
      STATISTIC(trace->segCopiedSize += ss->copiedSize);
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
  STATISTIC(trace->preservedInPlaceCount += ss->preservedInPlaceCount);
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


/* TraceIsEmpty -- return TRUE if trace has no condemned segments
 *
 * .empty.size: If the trace has a condemned size of zero, then it has
 * no white segments, because we don't allow pools to whiten segments
 * with no white objects in.
 */

Bool TraceIsEmpty(Trace trace)
{
  AVERT(Trace, trace);
  return trace->condemned == 0;
}


/* TraceAddWhite -- add a segment to the white set of a trace */

Res TraceAddWhite(Trace trace, Seg seg)
{
  Res res;
  Pool pool;
  Size condemnedBefore;

  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(!TraceSetIsMember(SegWhite(seg), trace)); /* .start.black */

  pool = SegPool(seg);
  AVERT(Pool, pool);

  condemnedBefore = trace->condemned;

  /* Give the pool the opportunity to turn the segment white. */
  /* If it fails, unwind. */
  res = SegWhiten(seg, trace);
  if(res != ResOK)
    return res;

  if (TraceSetIsMember(SegWhite(seg), trace)) {
    /* Pools must not condemn empty segments, otherwise we can't tell
       when a trace is empty and safe to destroy.  See .empty.size. */
    AVER(trace->condemned > condemnedBefore);
    
    /* Add the segment to the approximation of the white set if the
       pool made it white. */
    trace->white = ZoneSetUnion(trace->white, ZoneSetOfSeg(trace->arena, seg));

    /* if the pool is a moving GC, then condemned objects may move */
    if (PoolHasAttr(pool, AttrMOVINGGC)) {
      trace->mayMove = ZoneSetUnion(trace->mayMove,
                                    ZoneSetOfSeg(trace->arena, seg));
    }
  }

  return ResOK;
}


/* TraceCondemnStart -- start selecting generations to condemn for a trace */

void TraceCondemnStart(Trace trace)
{
  AVERT(Trace, trace);
  AVER(trace->state == TraceINIT);
  AVER(trace->white == ZoneSetEMPTY);
  AVER(RingIsSingle(&trace->genRing));
}


/* TraceCondemnEnd -- condemn segments for trace
 *
 * Condemn the segments in the generations that were selected since
 * TraceCondemnStart, and compute the predicted mortality of the
 * condemned memory. If successful, update *mortalityReturn and return
 * ResOK.
 *
 * We suspend the mutator threads so that the PoolWhiten methods can
 * calculate white sets without the mutator allocating in buffers
 * under our feet. See request.dylan.160098
 * <https://info.ravenbrook.com/project/mps/import/2001-11-05/mmprevol/request/dylan/160098>.
 *
 * TODO: Consider how to avoid this suspend in order to implement
 * incremental condemn.
 */

Res TraceCondemnEnd(double *mortalityReturn, Trace trace)
{
  Size condemnedSize = 0, survivorSize = 0;
  Ring genNode, genNext;
  Res res;

  AVER(mortalityReturn != NULL);
  AVERT(Trace, trace);
  AVER(trace->state == TraceINIT);
  AVER(trace->white == ZoneSetEMPTY);

  ShieldHold(trace->arena);
  RING_FOR(genNode, &trace->genRing, genNext) {
    Size genNewSize, genTotalSize;
    Ring segNode, segNext;
    GenDesc gen = GenDescOfTraceRing(genNode, trace);
    AVERT(GenDesc, gen);
    RING_FOR(segNode, &gen->segRing, segNext) {
      GCSeg gcseg = RING_ELT(GCSeg, genRing, segNode);
      res = TraceAddWhite(trace, &gcseg->segStruct);
      if (res != ResOK)
        goto failBegin;
    }
    genTotalSize = GenDescTotalSize(gen);
    genNewSize = GenDescNewSize(gen);
    condemnedSize += genTotalSize;
    survivorSize += (Size)(genNewSize * (1.0 - gen->mortality))
                    /* predict survivors will survive again */
                    + (genTotalSize - genNewSize);
  }
  ShieldRelease(trace->arena);

  if (TraceIsEmpty(trace))
    return ResFAIL;

  *mortalityReturn = 1.0 - (double)survivorSize / condemnedSize;
  return ResOK;

failBegin:
  /* If we successfully whitened one or more segments, but failed to
     whiten them all, then the white sets would now be inconsistent.
     This can't happen in practice (at time of writing) because all
     PoolWhiten methods always succeed. If we ever have a pool class
     that fails to whiten a segment, then this assertion will be
     triggered. In that case, we'll have to recover here by blackening
     the segments again. */
  AVER(TraceIsEmpty(trace));
  ShieldRelease(trace->arena);
  return res;
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
 * Scan a root, entering emergency mode on allocation failure.
 */

static Res traceScanRoot(TraceSet ts, Rank rank, Arena arena, Root root)
{
  Res res;

  res = traceScanRootRes(ts, rank, arena, root);

  if (ResIsAllocFailure(res)) {
    ArenaSetEmergency(arena, TRUE);
    res = traceScanRootRes(ts, rank, arena, root);
    /* Should be OK in emergency mode */
    AVER(!ResIsAllocFailure(res));
  }

  return res;
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
  Res res;

  AVERT(Root, root);
  AVER(p != NULL);
  AVERT(TraceSet, rf->ts);
  AVERT(Arena, rf->arena);
  AVERT(Rank, rf->rank);

  AVER(RootRank(root) <= RankEXACT); /* see .root.rank */

  if(RootRank(root) == rf->rank) {
    res = traceScanRoot(rf->ts, rf->rank, rf->arena, root);
    if (res != ResOK)
      return res;
  }

  return ResOK;
}


/* traceFlip -- flip the mutator from grey to black w.r.t. a trace
 *
 * The main job of traceFlip is to scan references which can't be protected
 * from the mutator, changing the colour of the mutator from grey to black
 * with respect to a trace.  The mutator threads are suspended while this
 * is happening, and the mutator perceives an instantaneous change in all
 * the references, enforced by the shield (barrier) system.
 *
 * NOTE: We don't have a way to shield the roots, so they are all scanned
 * here.  This is a coincidence.  There is no theoretical reason that the
 * roots have to be scanned at flip time, provided we could protect them
 * from the mutator.  (The thread registers are unlikely ever to be
 * protectable on stock hardware, however, as they were -- kind of -- on
 * Lisp machines.)
 *
 * NOTE: Ambiguous references may only exist in roots, because we can't
 * shield the exact roots and defer them for later scanning (after ambiguous
 * heap references).
 *
 * NOTE: We don't support weak or final roots because we can't shield them
 * and defer scanning until later.  See above.
 *
 * If roots and segments were more similar, we could melt a lot of these
 * problems.
 */

static Res traceFlip(Trace trace)
{
  Ring node, nextNode;
  Arena arena;
  Rank rank;
  struct rootFlipClosureStruct rfc;
  Res res;

  AVERT(Trace, trace);
  rfc.ts = TraceSetSingle(trace);

  arena = trace->arena;
  rfc.arena = arena;
  ShieldHold(arena);

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

  for(rank = RankMIN; rank <= RankEXACT; ++rank) {
    rfc.rank = rank;
    res = RootsIterate(ArenaGlobals(arena), rootFlip, (void *)&rfc);
    if (res != ResOK)
      goto failRootFlip;
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
  /* grey objects so that it can't obtain white pointers. */
  for(rank = RankMIN; rank < RankLIMIT; ++rank)
    RING_FOR(node, ArenaGreyRing(arena, rank), nextNode) {
      Seg seg = SegOfGreyRing(node);
      SegFlip(seg, trace);
    }

  /* @@@@ When write barrier collection is implemented, this is where */
  /* write protection should be removed for all segments which are */
  /* no longer blacker than the mutator.  Possibly this can be done */
  /* lazily as they are touched. */

  /* Mark the trace as flipped. */
  trace->state = TraceFLIPPED;
  arena->flippedTraces = TraceSetAdd(arena->flippedTraces, trace);

  EVENT2(TraceFlipEnd, trace, arena);

  ShieldRelease(arena);
  return ResOK;

failRootFlip:
  ShieldRelease(arena);
  return res;
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

ATTRIBUTE_UNUSED
static void traceCreatePoolGen(GenDesc gen)
{
  Ring n, nn;
  RING_FOR(n, &gen->locusRing, nn) {
    PoolGen pgen = RING_ELT(PoolGen, genRing, n);
    EVENT11(TraceCreatePoolGen, gen, gen->capacity, gen->mortality, gen->zones,
            pgen->pool, pgen->totalSize, pgen->freeSize, pgen->newSize,
            pgen->oldSize, pgen->newDeferredSize, pgen->oldDeferredSize);
  }
}

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
  trace->band = RankMIN;
  trace->fix = SegFix;
  trace->fixClosure = NULL;
  RingInit(&trace->genRing);
  STATISTIC(trace->preTraceArenaReserved = ArenaReserved(arena));
  trace->condemned = (Size)0;   /* nothing condemned yet */
  trace->notCondemned = (Size)0;
  trace->foundation = (Size)0;  /* nothing grey yet */
  trace->quantumWork = (Work)0; /* computed in TraceStart */
  STATISTIC(trace->greySegCount = (Count)0);
  STATISTIC(trace->greySegMax = (Count)0);
  STATISTIC(trace->rootScanCount = (Count)0);
  trace->rootScanSize = (Size)0;
  STATISTIC(trace->rootCopiedSize = (Size)0);
  STATISTIC(trace->segScanCount = (Count)0);
  trace->segScanSize = (Size)0; /* see .work */
  STATISTIC(trace->segCopiedSize = (Size)0);
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

  EVENT3(TraceCreate, trace, arena, (EventFU)why);

  STATISTIC({
    /* Iterate over all chains, all GenDescs within a chain, and all
     * PoolGens within a GenDesc. */
    Ring node;
    Ring nextNode;

    RING_FOR(node, &arena->chainRing, nextNode) {
      Chain chain = RING_ELT(Chain, chainRing, node);
      Index i;
      for (i = 0; i < chain->genCount; ++i) {
        GenDesc gen = &chain->gens[i];
        traceCreatePoolGen(gen);
      }
    }

    /* Now do topgen GenDesc, and all PoolGens within it. */
    traceCreatePoolGen(&arena->topGen);
  });

 *traceReturn = trace;
  return ResOK;
}


/* traceDestroyCommon -- common functionality for TraceDestroy*  */

static void traceDestroyCommon(Trace trace)
{
  Ring node, nextNode;

  RING_FOR(node, &trace->genRing, nextNode) {
    GenDesc gen = GenDescOfTraceRing(node, trace);
    GenDescEndTrace(gen, trace);
  }
  RingFinish(&trace->genRing);

  /* Ensure that address space is returned to the operating system for
   * traces that don't have any condemned objects (there might be
   * manually allocated objects that were freed). See job003999. */
  ArenaCompact(trace->arena, trace);

  EVENT1(TraceDestroy, trace);

  /* Hopefully the trace reclaimed some memory, so clear any emergency.
   * Do this before removing the trace from busyTraces, to avoid
   * violating <code/global.c#emergency.invariant>. */
  ArenaSetEmergency(trace->arena, FALSE);

  trace->sig = SigInvalid;
  trace->arena->busyTraces = TraceSetDel(trace->arena->busyTraces, trace);
  trace->arena->flippedTraces = TraceSetDel(trace->arena->flippedTraces, trace);
}


/* TraceDestroyInit -- destroy a trace object in state INIT */

void TraceDestroyInit(Trace trace)
{
  AVERT(Trace, trace);
  AVER(trace->state == TraceINIT);
  AVER(trace->condemned == 0);
  AVER(!TraceSetIsMember(trace->arena->flippedTraces, trace));

  traceDestroyCommon(trace);
}


/* TraceDestroyFinished -- destroy a trace object in state FINISHED
 *
 * Finish and deallocate a Trace object, freeing up a TraceId.
 *
 * This code does not allow a Trace to be destroyed while it is active.
 * It would be possible to allow this, but the colours of segments
 * etc. would need to be reset to black.  This also means the error
 * paths in this file don't work.  @@@@ */

void TraceDestroyFinished(Trace trace)
{
  AVERT(Trace, trace);
  AVER(trace->state == TraceFINISHED);

  STATISTIC(EVENT13(TraceStatScan, trace,
                    trace->rootScanCount, trace->rootScanSize,
                    trace->rootCopiedSize,
                    trace->segScanCount, trace->segScanSize,
                    trace->segCopiedSize,
                    trace->singleScanCount, trace->singleScanSize,
                    trace->singleCopiedSize,
                    trace->readBarrierHitCount, trace->greySegMax,
                    trace->pointlessScanCount));
  STATISTIC(EVENT10(TraceStatFix, trace,
                    trace->fixRefCount, trace->segRefCount,
                    trace->whiteSegRefCount,
                    trace->nailCount, trace->snapCount,
                    trace->forwardedCount, trace->forwardedSize,
                    trace->preservedInPlaceCount,
                    trace->preservedInPlaceSize));
  STATISTIC(EVENT3(TraceStatReclaim, trace,
                   trace->reclaimCount, trace->reclaimSize));

  traceDestroyCommon(trace);
}


/* traceReclaim -- reclaim the remaining objects white for this trace */

static void traceReclaim(Trace trace)
{
  Arena arena;
  Seg seg;

  AVER(trace->state == TraceRECLAIM);

  EVENT1(TraceReclaim, trace);
  arena = trace->arena;
  if(SegFirst(&seg, arena)) {
    Pool pool;
    Ring next;
    do {
      Addr base = SegBase(seg);
      pool = SegPool(seg);
      next = RingNext(SegPoolRing(seg));

      /* There shouldn't be any grey stuff left for this trace. */
      AVER_CRITICAL(!TraceSetIsMember(SegGrey(seg), trace));

      if(TraceSetIsMember(SegWhite(seg), trace)) {
        AVER_CRITICAL(PoolHasAttr(pool, AttrGC));
        STATISTIC(++trace->reclaimCount);
        SegReclaim(seg, trace);

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
    } while(SegNextOfRing(&seg, arena, pool, next));
  }

  trace->state = TraceFINISHED;

  ArenaCompact(arena, trace);  /* let arenavm drop chunks */

  TracePostMessage(trace);  /* trace end */
  /* Immediately pre-allocate messages for next time; failure is okay */
  (void)TraceIdMessagesCreate(arena, trace->ti);
}

/* TraceRankForAccess -- Returns rank to scan at if we hit a barrier.
 * 
 * We assume a single trace as otherwise we need to implement rank
 * filters on scanning.
 *
 * .scan.conservative: It's safe to scan at EXACT unless the band is
 * WEAK and in that case the segment should be weak.
 * 
 * If the trace band is EXACT then we scan EXACT. This might prevent
 * finalisation messages and may preserve objects pointed to only by weak
 * references but tough luck -- the mutator wants to look.
 * 
 * If the trace band is FINAL and the segment is FINAL, we scan it FINAL.
 * Any objects not yet preserved deserve to die, and we're only giving
 * them a temporary reprieve.  All the objects on the segment should be FINAL,
 * otherwise they might get sent finalization messages.
 *
 * If the trace band is FINAL, and the segment is not FINAL, we scan at EXACT.
 * This is safe to do for FINAL and WEAK references.
 * 
 * If the trace band is WEAK then the segment must be weak only, and we 
 * scan at WEAK.  All other segments for this trace should be scanned by now.
 * We must scan at WEAK to avoid bringing any objects back to life.
 * 
 * See the message <http://info.ravenbrook.com/mail/2012/08/30/16-46-42/0.txt>
 * for a description of these semantics.
 */
Rank TraceRankForAccess(Arena arena, Seg seg)
{
  TraceSet ts;
  Trace trace;
  TraceId ti;
  Rank band;
  RankSet rankSet;

  AVERT(Arena, arena);
  AVERT(Seg, seg);

  band = RankLIMIT; /* initialize with invalid rank */
  ts = arena->flippedTraces;    
  AVER(TraceSetIsSingle(ts));
  TRACE_SET_ITER(ti, trace, ts, arena)
    band = traceBand(trace);
  TRACE_SET_ITER_END(ti, trace, ts, arena);
  rankSet = SegRankSet(seg);
  switch(band) {
  case RankAMBIG:
    NOTREACHED;
    break;
  case RankEXACT:
    return RankEXACT;
  case RankFINAL:
    if(rankSet == RankSetSingle(RankFINAL)) {
      return RankFINAL;
    }
    /* It's safe to scan at exact in the final band so do so if there are
     * any non-final references. */
    return RankEXACT;
  case RankWEAK:
    AVER(rankSet == RankSetSingle(RankWEAK));
    return RankWEAK;
  default:
    NOTREACHED;
    break;
  }
  NOTREACHED;
  return RankEXACT;
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
 * roots, so it's a bug if we ever find any.  This behaviour is not set
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
 * this check.  See <http://info.ravenbrook.com/project/mps/issue/job001658/>.
 * 
 * For further discussion on the semantics of rank based tracing see
 * <http://info.ravenbrook.com/mail/2007/06/25/11-35-57/0.txt>
 */

static Bool traceFindGrey(Seg *segReturn, Rank *rankReturn,
                          Arena arena, TraceId ti)
{
  Rank rank;
  Trace trace;
  Ring node, nextNode;

  AVER(segReturn != NULL);
  AVERT(TraceId, ti);

  trace = ArenaTrace(arena, ti);

  while(1) {
    Rank band = traceBand(trace);

    /* Within the R band we look for segments of rank R first,  */
    /* then successively earlier ones.  Slight hack: We never    */
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
          EVENT4(TraceFindGrey, arena, ti, seg, rank);
          return TRUE;
        }
      }
    }
    /* .check.ambig.not */
    AVER(RingIsSingle(ArenaGreyRing(arena, RankAMBIG)));
    if(!traceBandAdvance(trace)) {
      /* No grey segments for this trace. */
      return FALSE;
    }
  }
}


/* ScanStateSetSummary -- set the summary of scanned references
 *
 * This function sets unfixedSummary and fixedSummary such that
 * ScanStateSummary will return the summary passed.  Subsequently fixed
 * references are accumulated into this result.  */

void ScanStateSetSummary(ScanState ss, RefSet summary)
{
  AVERT(ScanState, ss);
  /* Can't check summary, as it can be anything. */

  ScanStateSetUnfixedSummary(ss, RefSetEMPTY);
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
                     RefSetDiff(ScanStateUnfixedSummary(ss),
                                ScanStateWhite(ss)));
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
  RefSet summary;

  /* The reason for scanning a segment is that it's grey. */
  AVER(TraceSetInter(ts, SegGrey(seg)) != TraceSetEMPTY);
  EVENT4(TraceScanSeg, ts, rank, arena, seg);

  white = traceSetWhiteUnion(ts, arena);

  /* Only scan a segment if it refers to the white set. */
  if(ZoneSetInter(white, SegSummary(seg)) == ZoneSetEMPTY) {
    SegBlacken(seg, ts);
    /* Setup result code to return later. */
    res = ResOK;
  } else {      /* scan it */
    ScanStateStruct ssStruct;
    ScanState ss = &ssStruct;
    ScanStateInit(ss, ts, arena, rank, white);

    /* Expose the segment to make sure we can scan it. */
    ShieldExpose(arena, seg);
    res = SegScan(&wasTotal, seg, ss);
    /* Cover, regardless of result */
    ShieldCover(arena, seg);

    traceSetUpdateCounts(ts, arena, ss, traceAccountingPhaseSegScan);
    /* Count segments scanned pointlessly */
    STATISTIC({
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
    AVER(RefSetSub(ScanStateUnfixedSummary(ss), SegSummary(seg))); /* <design/check/#.common> */

    /* Write barrier deferral -- see design.mps.write-barrier.deferral. */
    /* Did the segment refer to the white set? */
    if (ZoneSetInter(ScanStateUnfixedSummary(ss), white) == ZoneSetEMPTY) {
      /* Boring scan.  One step closer to raising the write barrier. */
      if (seg->defer > 0)
        --seg->defer;
    } else {
      /* Interesting scan. Defer raising the write barrier. */
      if (seg->defer < WB_DEFER_DELAY)
        seg->defer = WB_DEFER_DELAY;
    }

    /* Only apply the write barrier if it is not deferred. */
    if (seg->defer == 0) {
      /* If we scanned every reference in the segment then we have a
         complete summary we can set. Otherwise, we just have
         information about more zones that the segment refers to. */
      if (res == ResOK && wasTotal)
        summary = ScanStateSummary(ss);
      else
        summary = RefSetUnion(SegSummary(seg), ScanStateSummary(ss));
    } else {
      summary = RefSetUNIV;
    }
    SegSetSummary(seg, summary);

    ScanStateFinish(ss);
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
 * Scans a segment, switching to emergency mode if there is an allocation
 * failure.
 */

static Res traceScanSeg(TraceSet ts, Rank rank, Arena arena, Seg seg)
{
  Res res;

  res = traceScanSegRes(ts, rank, arena, seg);
  if(ResIsAllocFailure(res)) {
    ArenaSetEmergency(arena, TRUE);
    res = traceScanSegRes(ts, rank, arena, seg);
    /* Should be OK in emergency mode. */
    AVER(!ResIsAllocFailure(res));
  }

  return res;
}


/* TraceSegAccess -- handle barrier hit on a segment */

void TraceSegAccess(Arena arena, Seg seg, AccessSet mode)
{
  Res res;
  AccessSet shieldHit;
  Bool readHit, writeHit;

  AVERT(Arena, arena);
  AVERT(Seg, seg);
  AVERT(AccessSet, mode);

  shieldHit = BS_INTER(mode, SegSM(seg));
  readHit = BS_INTER(shieldHit, AccessREAD) != AccessSetEMPTY;
  writeHit = BS_INTER(shieldHit, AccessWRITE) != AccessSetEMPTY;

  /* If it's a read access, then the segment must be grey for a trace */
  /* which is flipped. */
  AVER(!readHit ||
       TraceSetInter(SegGrey(seg), arena->flippedTraces) != TraceSetEMPTY);

  /* If it's a write access, then the segment must have a summary that */
  /* is smaller than the mutator's summary (which is assumed to be */
  /* RefSetUNIV). */
  AVER(!writeHit || SegSummary(seg) != RefSetUNIV);

  EVENT3(TraceAccess, arena, seg, mode);

  /* Write barrier deferral -- see design.mps.write-barrier.deferral. */
  if (writeHit)
    seg->defer = WB_DEFER_HIT;

  if (readHit) {
    Rank rank;
    TraceSet traces;

    AVER(SegRankSet(seg) != RankSetEMPTY);
    
    /* Pick set of traces to scan for: */
    traces = arena->flippedTraces;
    rank = TraceRankForAccess(arena, seg);
    res = traceScanSeg(traces, rank, arena, seg);      

    /* Allocation failures should be handled my emergency mode, and we don't
       expect any other kind of failure in a normal GC that causes access
       faults. */
    AVER(res == ResOK);

    /* The pool should've done the job of removing the greyness that */
    /* was causing the segment to be protected, so that the mutator */
    /* can go ahead and access it. */
    AVER(TraceSetInter(SegGrey(seg), traces) == TraceSetEMPTY);

    STATISTIC({
      Trace trace;
      TraceId ti;
      TRACE_SET_ITER(ti, trace, traces, arena)
        ++trace->readBarrierHitCount;
      TRACE_SET_ITER_END(ti, trace, traces, arena);
    });
  } else {              /* write barrier */
    STATISTIC(++arena->writeBarrierHitCount);
  }

  /* The write barrier handling must come after the read barrier, */
  /* because the latter may set the summary and raise the write barrier. */
  if (writeHit)
    SegSetSummary(seg, RefSetUNIV);

  /* The segment must now be accessible. */
  AVER(BS_INTER(mode, SegSM(seg)) == AccessSetEMPTY);
}


/* _mps_fix2 (a.k.a. "TraceFix") -- second stage of fixing a reference
 *
 * _mps_fix2 is on the [critical path](../design/critical-path.txt).  A
 * one-instruction difference in the early parts of this code will have a
 * significant impact on overall run time.  The priority is to eliminate
 * irrelevant references early and fast using the colour information stored
 * in the tract table.
 *
 * The name "TraceFix" is pervasive in the MPS and its documents to describe
 * this function.  Optimisation and strict aliasing rules have meant that we
 * need to use the external name for it here.
 */

mps_res_t _mps_fix2(mps_ss_t mps_ss, mps_addr_t *mps_ref_io)
{
  ScanState ss = PARENT(ScanStateStruct, ss_s, mps_ss);
  Ref ref;
  Chunk chunk;
  Index i;
  Tract tract;
  Seg seg;
  Res res;

  /* Special AVER macros are used on the critical path. */
  /* See <design/trace/#fix.noaver> */
  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(mps_ref_io != NULL);

  ref = (Ref)*mps_ref_io;

  /* The zone test should already have been passed by MPS_FIX1 in mps.h. */
  AVER_CRITICAL(ZoneSetInter(ScanStateWhite(ss),
                             ZoneSetAddAddr(ss->arena, ZoneSetEMPTY, ref)) !=
                ZoneSetEMPTY);

  STATISTIC(++ss->fixRefCount);
  EVENT4(TraceFix, ss, mps_ref_io, ref, ss->rank);

  /* This sequence of tests is equivalent to calling TractOfAddr(),
   * but inlined so that we can distinguish between "not pointing to
   * chunk" and "pointing to chunk but not to tract" so that we can
   * check the rank in the latter case. See
   * <design/trace/#fix.tractofaddr.inline>
   *
   * If compilers fail to do a good job of inlining ChunkOfAddr and
   * TreeFind then it may become necessary to inline at least the
   * comparison against the root of the tree. See
   * <https://info.ravenbrook.com/mail/2014/06/11/13-32-08/0/>
   */
  if (!ChunkOfAddr(&chunk, ss->arena, ref))
    /* Reference points outside MPS-managed address space: ignore. */
    goto done;

  i = INDEX_OF_ADDR(chunk, ref);
  if (!BTGet(chunk->allocTable, i)) {
    /* Reference points into a chunk but not to an allocated tract.
     * See <design/trace/#exact.legal> */
    AVER_CRITICAL(ss->rank < RankEXACT); /* <design/check/#.common> */
    goto done;
  }

  tract = PageTract(&chunk->pageTable[i]);
  if (TraceSetInter(TractWhite(tract), ss->traces) == TraceSetEMPTY) {
    /* Reference points to a tract that is not white for any of the
     * active traces. See <design/trace/#fix.tractofaddr> */
    STATISTIC({
      if (TRACT_SEG(&seg, tract)) {
        ++ss->segRefCount;
        EVENT1(TraceFixSeg, seg);
      }
    });
    goto done;
  }

  if (!TRACT_SEG(&seg, tract)) {
    /* Tracts without segments must not be condemned. */
    NOTREACHED;
    goto done;
  }

  STATISTIC(++ss->segRefCount);
  STATISTIC(++ss->whiteSegRefCount);
  EVENT1(TraceFixSeg, seg);
  EVENT0(TraceFixWhite);
  res = (*ss->fix)(seg, ss, &ref);
  if (res != ResOK) {
    /* SegFixEmergency must not fail. */
    AVER_CRITICAL(ss->fix != SegFixEmergency);
    /* Fix protocol (de facto): if Fix fails, ref must be unchanged
     * Justification for this restriction:
     * A: it simplifies;
     * B: it's reasonable (given what may cause Fix to fail);
     * C: the code (here) already assumes this: it returns without 
     *    updating ss->fixedSummary.  RHSK 2007-03-21.
     */
    AVER_CRITICAL(ref == (Ref)*mps_ref_io);
    return res;
  }

done:
  /* See <design/trace/#fix.fixed.all> */
  ss->fixedSummary = RefSetAdd(ss->arena, ss->fixedSummary, ref);
  
  *mps_ref_io = (mps_addr_t)ref;
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

  AVERT(TraceSet, ts);
  AVERT(Rank, rank);
  AVERT(Arena, arena);
  AVERT(Seg, seg);
  AVER(refIO != NULL);

  res = traceScanSingleRefRes(ts, rank, arena, seg, refIO);
  if(res != ResOK) {
    ArenaSetEmergency(arena, TRUE);
    res = traceScanSingleRefRes(ts, rank, arena, seg, refIO);
    /* Ought to be OK in emergency mode now. */
  }
  AVER(ResOK == res);
}


/* TraceScanArea -- scan an area of memory for references
 *
 * This is a wrapper for area scanning functions, which should not
 * otherwise be called directly from within the MPS.  This function
 * checks arguments and takes care of accounting for the scanned
 * memory.
 *
 * c.f. FormatScan()
 */

Res TraceScanArea(ScanState ss, Word *base, Word *limit,
                  mps_area_scan_t scan_area,
                  void *closure)
{
  AVERT(ScanState, ss);
  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  EVENT3(TraceScanArea, ss, base, limit);

  /* scannedSize is accumulated whether or not scan_area succeeds, so
     it's safe to accumulate now so that we can tail-call
     scan_area. */
  ss->scannedSize += AddrOffset(base, limit);

  return scan_area(&ss->ss_s, base, limit, closure);
}


/* TraceStart -- condemn a set of objects and start collection
 *
 * TraceStart should be passed a trace with state TraceINIT, i.e.,
 * recently returned from TraceCreate, with some condemned segments
 * added. mortality is the fraction of the condemned set expected not
 * to survive. finishingTime is relative to the current polling clock,
 * see <design/arena/#poll.clock>.
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


/* TraceStart -- start a trace whose white set has been established
 *
 * The main job of TraceStart is to set up the grey list for a trace.  The
 * trace is first created with TraceCreate, objects are whitened, then
 * TraceStart is called to initialise the tracing process.
 *
 * NOTE: At present, TraceStart also flips the mutator, so there is no
 * grey-mutator tracing.
 */

Res TraceStart(Trace trace, double mortality, double finishingTime)
{
  Arena arena;
  Res res;
  Seg seg;

  AVERT(Trace, trace);
  AVER(trace->state == TraceINIT);
  AVER(0.0 <= mortality);
  AVER(mortality <= 1.0);
  AVER(finishingTime >= 0.0);
  AVER(trace->condemned > 0);

  arena = trace->arena;
  
  /* From the already set up white set, derive a grey set. */

  /* @@@@ Instead of iterating over all the segments, we could */
  /* iterate over all pools which are scannable and thence over */
  /* all their segments.  This might be better if the minority */
  /* of segments are scannable.  Perhaps we should choose */
  /* dynamically which method to use. */

  if(SegFirst(&seg, arena)) {
    do {
      Size size = SegSize(seg);
      AVER(!TraceSetIsMember(SegGrey(seg), trace));

      /* A segment can only be grey if it contains some references. */
      /* This is indicated by the rankSet begin non-empty.  Such */
      /* segments may only belong to scannable pools. */
      if(SegRankSet(seg) != RankSetEMPTY) {
        /* Turn the segment grey if there might be a reference in it */
        /* to the white set.  This is done by seeing if the summary */
        /* of references in the segment intersects with the */
        /* approximation to the white set. */
        if(ZoneSetInter(SegSummary(seg), trace->white) != ZoneSetEMPTY) {
          /* Note: can a white seg get greyed as well?  At this point */
          /* we still assume it may.  (This assumption runs out in */
          /* PoolTrivGrey). */
          SegGreyen(seg, trace);
          if(TraceSetIsMember(SegGrey(seg), trace)) {
            trace->foundation += size;
          }
        }

        if(PoolHasAttr(SegPool(seg), AttrGC)
           && !TraceSetIsMember(SegWhite(seg), trace))
        {
          trace->notCondemned += size;
        }
      }
    } while (SegNext(&seg, arena, seg));
  }

  res = RootsIterate(ArenaGlobals(arena), rootGrey, (void *)trace);
  AVER(res == ResOK);

  STATISTIC(EVENT2(ArenaWriteFaults, arena, arena->writeBarrierHitCount));

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
    /* One quantum of work equals total tracing work divided by number
     * of polls, plus one to ensure it's not zero. */
    trace->quantumWork
      = (trace->foundation + sSurvivors) / (unsigned long)nPolls + 1;
  }

  /* TODO: compute rate of scanning here. */

  EVENT8(TraceStart, trace, mortality, finishingTime,
         trace->condemned, trace->notCondemned,
         trace->foundation, trace->white,
         trace->quantumWork);

  trace->state = TraceUNFLIPPED;
  TracePostStartMessage(trace);

  /* All traces must flip at beginning at the moment. */
  return traceFlip(trace);
}


/* traceWork -- a measure of the work done for this trace.
 *
 * See design.mps.type.work.
 */

#define traceWork(trace) ((Work)((trace)->segScanSize + (trace)->rootScanSize))


/* TraceAdvance -- progress a trace by one step */

void TraceAdvance(Trace trace)
{
  Arena arena;
  Work oldWork, newWork;

  AVERT(Trace, trace);
  arena = trace->arena;
  oldWork = traceWork(trace);

  switch (trace->state) {
  case TraceUNFLIPPED:
    /* all traces are flipped in TraceStart at the moment */
    NOTREACHED;
    break;
  case TraceFLIPPED: {
    Seg seg;
    Rank rank;

    if (traceFindGrey(&seg, &rank, arena, trace->ti)) {
      Res res;
      res = traceScanSeg(TraceSetSingle(trace), rank, arena, seg);
      /* Allocation failures should be handled by emergency mode, and we
       * don't expect any other error in a normal GC trace. */
      AVER(res == ResOK);
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

  newWork = traceWork(trace);
  AVER(newWork >= oldWork);
  arena->tracedWork += newWork - oldWork;
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
  double mortality, finishingTime;
  Ring chainNode, chainNext;

  AVERT(Arena, arena);
  AVER(arena->busyTraces == TraceSetEMPTY);

  res = TraceCreate(&trace, arena, why);
  AVER(res == ResOK); /* succeeds because no other trace is busy */

  TraceCondemnStart(trace);

  /* Condemn all generations in all chains, plus the top generation. */
  RING_FOR(chainNode, &arena->chainRing, chainNext) {
    size_t i;
    Chain chain = RING_ELT(Chain, chainRing, chainNode);
    AVERT(Chain, chain);
    for (i = 0; i < chain->genCount; ++i) {
      GenDesc gen = &chain->gens[i];
      AVERT(GenDesc, gen);
      GenDescStartTrace(gen, trace);
    }
  }
  GenDescStartTrace(&arena->topGen, trace);

  res = TraceCondemnEnd(&mortality, trace);
  if(res != ResOK) /* should try some other trace, really @@@@ */
    goto failCondemn;
  finishingTime = ArenaAvail(arena) - trace->condemned * (1.0 - mortality);
  if(finishingTime < 0) {
    /* Run out of time, should really try a smaller collection. @@@@ */
    finishingTime = 0.0;
  }
  res = TraceStart(trace, mortality, finishingTime);
  if (res != ResOK)
    goto failStart;
  *traceReturn = trace;
  return ResOK;

failStart:
  /* TODO: We can't back-out from a failed TraceStart that has
     already done some scanning, so this error path is somewhat bogus if it
     destroys the trace.  In the current system, TraceStartCollectAll is
     only used for a normal GC, so TraceStart should not fail and this case
     should never be reached.  There's a chance the mutator will survive
     if the assertion isn't hit, so drop through anyway. */
  NOTREACHED;
failCondemn:
  TraceDestroyInit(trace);
  return res;
}


/* TracePoll -- Check if there's any tracing work to be done
 *
 * Consider starting a trace if none is running; advance the running
 * trace (if any) by one quantum.
 *
 * The collectWorldReturn and collectWorldAllowed arguments are as for
 * PolicyStartTrace.
 *
 * If there may be more work to do, update *workReturn with a measure
 * of the work done and return TRUE. Otherwise return FALSE.
 */

Bool TracePoll(Work *workReturn, Bool *collectWorldReturn, Globals globals,
               Bool collectWorldAllowed)
{
  Trace trace;
  Arena arena;
  Work oldWork, newWork, work, endWork;

  AVERT(Globals, globals);
  arena = GlobalsArena(globals);

  if (arena->busyTraces != TraceSetEMPTY) {
    trace = ArenaTrace(arena, (TraceId)0);
  } else {
    /* No traces are running: consider starting one now. */
    if (!PolicyStartTrace(&trace, collectWorldReturn, arena,
                          collectWorldAllowed))
      return FALSE;
  }

  AVER(arena->busyTraces == TraceSetSingle(trace));
  oldWork = traceWork(trace);
  endWork = oldWork + trace->quantumWork;
  do {
    TraceAdvance(trace);
  } while (trace->state != TraceFINISHED && traceWork(trace) < endWork);
  newWork = traceWork(trace);
  AVER(newWork >= oldWork);
  work = newWork - oldWork;
  if (trace->state == TraceFINISHED)
    TraceDestroyFinished(trace);
  *workReturn = work;
  return TRUE;
}


/* TraceDescribe -- describe a trace */

Res TraceDescribe(Trace trace, mps_lib_FILE *stream, Count depth)
{
  Ring node, next;
  Res res;
  const char *state;

  if (!TESTT(Trace, trace))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  switch (trace->state) {
  case TraceINIT:      state = "INIT";      break;
  case TraceUNFLIPPED: state = "UNFLIPPED"; break;
  case TraceFLIPPED:   state = "FLIPPED";   break;
  case TraceRECLAIM:   state = "RECLAIM";   break;
  case TraceFINISHED:  state = "FINISHED";  break;
  default:             state = "unknown";   break;
  }

  res = WriteF(stream, depth,
               "Trace $P ($U) {\n", (WriteFP)trace, (WriteFU)trace->ti,
               "  arena $P ($U)\n", (WriteFP)trace->arena,
               (WriteFU)trace->arena->serial,
               "  why \"$S\"\n", (WriteFS)TraceStartWhyToString(trace->why),
               "  state $S\n", (WriteFS)state,
               "  band $U\n", (WriteFU)trace->band,
               "  white   $B\n", (WriteFB)trace->white,
               "  mayMove $B\n", (WriteFB)trace->mayMove,
               "  condemned $U\n", (WriteFU)trace->condemned,
               "  notCondemned $U\n", (WriteFU)trace->notCondemned,
               "  foundation $U\n", (WriteFU)trace->foundation,
               "  quantumWork $U\n", (WriteFU)trace->quantumWork,
               "  rootScanSize $U\n", (WriteFU)trace->rootScanSize,
               STATISTIC_WRITE("  rootCopiedSize $U\n",
                               (WriteFU)trace->rootCopiedSize)
               "  segScanSize $U\n", (WriteFU)trace->segScanSize,
               STATISTIC_WRITE("  segCopiedSize $U\n",
                               (WriteFU)trace->segCopiedSize)
               "  forwardedSize $U\n", (WriteFU)trace->forwardedSize,
               "  preservedInPlaceSize $U\n", (WriteFU)trace->preservedInPlaceSize,
               "} Trace $P\n", (WriteFP)trace,
               NULL);

  RING_FOR(node, &trace->genRing, next) {
    GenDesc gen = GenDescOfTraceRing(node, trace);
    res = GenDescDescribe(gen, stream, depth + 2);
    if (res != ResOK)
      return res;
  }

  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2018 Ravenbrook Limited
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

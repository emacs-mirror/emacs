/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 * $HopeName: MMsrc!trace.c(MMdevel_restr2.2) $
 */

#include "mpm.h"

SRCID(trace, "$HopeName: MMsrc!trace.c(MMdevel_restr2.2) $");

Bool ScanStateCheck(ScanState ss)
{
  CHECKS(ScanState, ss);
  CHECKU(Space, ss->space);
  CHECKL(ss->zoneShift == ss->space->zoneShift);
  CHECKL(RankCheck(ss->rank));
  CHECKL(ss->condemned == ss->space->trace[ss->traceId].condemned);
  return TRUE;
}

Bool TraceIdCheck(TraceId ti)
{
  CHECKL(ti == TraceIdNONE || ti < TRACE_MAX);
  return TRUE;
}

Bool TraceSetCheck(TraceSet ts)
{
  CHECKL(ts < (1uL << TRACE_MAX));
  return TRUE;
}

Res TraceCreate(TraceId *tiReturn, Space space)
{
  TraceId ti;

  /* .single-collection */
  AVER(TRACE_MAX == 1);

  AVER(tiReturn != NULL);
  AVERT(Space, space);

  /* allocate free TraceId */
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(!TraceSetIsMember(space->busyTraces, ti))
      goto found;
  return ResLIMIT;

found:
  space->trace[ti].condemned = RefSetEmpty;
  space->busyTraces = TraceSetAdd(space->busyTraces, ti);

  *tiReturn = ti;
  return ResOK;
}

void TraceDestroy(Space space, TraceId ti)
{
  AVERT(Space, space);
  space->busyTraces = TraceSetDelete(space->busyTraces, ti);
}

Res TraceFlip(Space space, TraceId ti, RefSet condemned)
{
  Ring ring;
  Ring node;
  Trace trace;
  ScanStateStruct ss;
  Res res;

  AVERT(Space, space);

  ShieldSuspend(space);

  trace = &space->trace[ti];
  AVER(trace->condemned == RefSetEmpty);
  trace->condemned = condemned;

  /* Update location dependency structures.  condemned is
   * a conservative approximation of the refset of refs which
   * may move during this collection.
   * @@@@ It is too conservative.  Not everything condemned will
   * necessarily move.
   */
  LDAge(space, condemned);

  /* Grey all the roots and pools. */

  ring = SpacePoolRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Pool pool = RING_ELT(Pool, spaceRing, node);

    if((pool->class->attr & AttrSCAN) != 0)
      PoolGrey(pool, space, ti);  /* implicitly excludes condemned set */

    node = next;
  }

  ring = SpaceRootRing(space);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Root root = RING_ELT(Root, spaceRing, node);

    RootGrey(root, space, ti);

    node = next;
  }

  ss.fix = TraceFix;
  ss.zoneShift = space->zoneShift;
  ss.condemned = space->trace[ti].condemned;
  ss.summary = RefSetEmpty;
  ss.space = space;
  ss.traceId = ti;
  ss.sig = ScanStateSig;

  /* At the moment we must scan all roots, because we don't have */
  /* a mechanism for shielding them.  There can't be any weak or */
  /* final roots either, since we must protect these in order to */
  /* avoid scanning them too early, before the pool contents. */

  /* @@@@ This isn't correct if there are higher ranking roots than */
  /* data in pools. */

  for(ss.rank = RankAMBIG; ss.rank <= RankEXACT; ++ss.rank) {
    ring = SpaceRootRing(space);
    node = RingNext(ring);
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

  ShieldResume(space);

  return ResOK;
}

static void TraceReclaim(Space space, TraceId ti)
{
  Ring node;

  node = RingNext(&space->poolRing);
  while(node != &space->poolRing) {
    Ring next = RingNext(node);
    Pool pool = RING_ELT(Pool, spaceRing, node);

    if((pool->class->attr & AttrGC) != 0)
      PoolReclaim(pool, space, ti);

    node = next;
  }
}

Size TracePoll(Space space, TraceId ti)
{
  Res res;
  Bool finished;
  Trace trace;

  trace = &space->trace[ti];

  if(trace->condemned != RefSetEmpty) {
    res = TraceRun(space, ti, &finished);
    AVER(res == ResOK); /* @@@@ */
    if(finished) {
      TraceReclaim(space, ti);
      TraceDestroy(space, ti);
      return SPACE_POLL_MAX;
    }
  }

  /* We need to calculate a rate depending on the amount of work */
  /* remaining and the deadline for the collection to finish. */
  return (Size)4096;            /* @@@@ */
}

Res TraceFix(ScanState ss, Ref *refIO)
{
  Ref ref;
  Seg seg;
  Pool pool;

  AVERT(ScanState, ss);
  AVER(refIO != NULL);

  ref = *refIO;
  if(SegOfAddr(&seg, ss->space, ref))
    if(ss->traceId == seg->condemned) {
      pool = seg->pool;
      return PoolFix(pool, ss, seg, refIO);
    }

  return ResOK;
}

/*  == Scan Area ==
 *
 *  This is a convenience function for scanning the contiguous area
 *  [base, limit).  i.e. it calls fix on all words from base up
 *  to limit, inclusive of base and exclusive of limit.
 */

Res TraceScanArea(ScanState ss, Addr *base, Addr *limit)
{
  Res res;
  Addr *p;
  Ref ref;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

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

/*  == Scan Area Tagged ==
 *
 *  This is as TraceScanArea except words are only fixed if they
 *  are multiples of four. i.e. look like 4-byte aligned pointers.
 */

Res TraceScanAreaTagged(ScanState ss, Addr *base, Addr *limit)
{
  Res res;
  Addr *p;
  Ref ref;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  TRACE_SCAN_BEGIN(ss) {
    p = base;
  loop:
    if(p >= limit) goto out;
    ref = *p++;
    if(((Word)ref&3) != 0)   /* only fix 4-aligned pointers */
      goto loop;             /* not a pointer */
    if(!TRACE_FIX1(ss, ref)) goto loop;
    res = TRACE_FIX2(ss, p-1);
    if(res == ResOK) goto loop;
    return res;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(ss);

  return ResOK;
}

Res TraceRun(Space space, TraceId ti, Bool *finishedReturn)
{
  Res res;
  ScanStateStruct ss;

  AVERT(Space, space);
  AVER(finishedReturn != NULL);

  ss.fix = TraceFix;
  ss.zoneShift = space->zoneShift;
  ss.condemned = space->trace[ti].condemned;
  ss.summary = RefSetEmpty;
  ss.space = space;
  ss.traceId = ti;
  ss.sig = ScanStateSig;

  for(ss.rank = 0; ss.rank < RankMAX; ++ss.rank) {
    Ring ring;
    Ring node;

    ring = SpacePoolRing(space);
    node = RingNext(ring);

    while(node != ring) {
      Ring next = RingNext(node);
      Pool pool = RING_ELT(Pool, spaceRing, node);
      Bool finished;

      if((pool->class->attr & AttrSCAN) != 0) {
        res = PoolScan(&ss, pool, &finished);
        if(res != ResOK) return res;

        if(!finished) {
          *finishedReturn = FALSE;
          return ResOK;
        }
      }

      node = next;
    }
  }

  ss.sig = SigInvalid;  /* just in case */

  *finishedReturn = TRUE;
  return ResOK;
}

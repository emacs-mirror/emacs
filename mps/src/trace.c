/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 * $HopeName: MMsrc!trace.c(trunk.12) $
 */

#include "std.h"
#include "lib.h"
#include "mpmconf.h"
#include "ref.h"
#include "space.h"
#include "trace.h"
#include "pool.h"
#include "root.h"
#include "rootst.h"
#include "ld.h"
#include <limits.h>

SRCID("$HopeName: MMsrc!trace.c(trunk.12) $");

Bool ScanStateIsValid(ScanState ss, ValidationType validParam)
{
  AVER(ss != NULL);
  AVER(ss->sig == ScanStateSig);
  AVER(ISVALIDNESTED(Space, ss->space));
  AVER(ss->zoneShift == ss->space->zoneShift);
  AVER(ISVALIDNESTED(RefRank, ss->rank));
  AVER(ss->condemned == ss->space->trace[ss->traceId].condemned);
  return TRUE;
}

Error TraceCreate(TraceId *tiReturn, Space space)
{
  TraceId ti;

  /* .single-collection */
  AVER(TRACE_MAX == 1);

  AVER(tiReturn != NULL);
  AVER(ISVALID(Space, space));

  /* allocate free TraceId */
  for(ti = 0; ti < TRACE_MAX; ++ti)
    if(!TraceSetIsMember(space->busyTraces, ti))
      goto found;
  return ErrLIMIT;

found:
  space->trace[ti].condemned = RefSetEmpty;
  space->busyTraces = TraceSetAdd(space->busyTraces, ti);

  *tiReturn = ti;
  return ErrSUCCESS;
}

void TraceDestroy(Space space, TraceId ti)
{
  AVER(ISVALID(Space, space));
  space->busyTraces = TraceSetDelete(space->busyTraces, ti);
}  

Error TraceFlip(Space space, TraceId ti, RefSet condemned)
{
  Deque deque;
  DequeNode node;
  Trace trace;
  ScanStateStruct ss;
  Error e;

  AVER(ISVALID(Space, space));

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

  deque = SpacePoolDeque(space);
  node = DequeFirst(deque);
  while(node != DequeSentinel(deque)) {
    DequeNode next = DequeNodeNext(node);
    Pool pool = DEQUENODEELEMENT(Pool, spaceDeque, node);

    PoolGrey(pool, space, ti);	/* implicitly excludes condemned set */

    node = next;
  }

  deque = SpaceRootDeque(space);
  node = DequeFirst(deque);
  while(node != DequeSentinel(deque)) {
    DequeNode next = DequeNodeNext(node);
    Root root = DEQUENODEELEMENT(Root, spaceDeque, node);

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

  ShieldEnter(space);

  for(ss.rank = RefRankAMBIG; ss.rank <= RefRankEXACT; ++ss.rank) {
    deque = SpaceRootDeque(space);
    node = DequeFirst(deque);
    while(node != DequeSentinel(deque)) {
      DequeNode next = DequeNodeNext(node);
      Root root = DEQUENODEELEMENT(Root, spaceDeque, node);

      AVER(RootRank(root) <= RefRankEXACT); /* see above */

      if(RootRank(root) == ss.rank) {
        e = RootScan(&ss, root);
        if(e != ErrSUCCESS) {
          ShieldLeave(space);
          return e;
        }
      }

      node = next;
    }
  }

  ShieldLeave(space);

  ss.sig = SigInvalid;	/* just in case */

  return ErrSUCCESS;
}

static void TraceReclaim(Space space, TraceId ti)
{
  DequeNode node;

  node = DequeFirst(&space->poolDeque);
  while(node != DequeSentinel(&space->poolDeque)) {
    DequeNode next = DequeNodeNext(node);
    Pool pool = DEQUENODEELEMENT(Pool, spaceDeque, node);

    PoolReclaim(pool, space, ti);

    node = next;
  }
}

Size TracePoll(Space space, TraceId ti)
{
  Error e;
  Bool finished;
  Trace trace;

  trace = &space->trace[ti];

  if(trace->condemned != RefSetEmpty) {
    e = TraceRun(space, ti, &finished);
    AVER(e == ErrSUCCESS);	/* @@@@ */
    if(finished) {
      TraceReclaim(space, ti);
      TraceDestroy(space, ti);
      return SPACE_POLL_MAX;
    }
  }

  /* We need to calculate a rate depending on the amount of work */
  /* remaining and the deadline for the collection to finish. */
  return (Size)4096;		/* @@@@ */
}

Error TraceFix(ScanState ss, Ref *refIO)
{
  Arena arena;
  Pool pool;
  Ref ref;

  AVER(ISVALID(ScanState, ss));
  AVER(refIO != NULL);

  arena = SpaceArena(ss->space);
  ref = *refIO;
  if(PoolOfAddr(&pool, arena, ref))
    return PoolFix(pool, ss, arena, refIO);

  return ErrSUCCESS;
}

Error TraceScanArea(ScanState ss, Addr *base, Addr *limit)
{
  Error e;
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
    e = TRACE_FIX2(ss, p-1);
    if(e == ErrSUCCESS) goto loop;
    return e;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(ss);
  
  return ErrSUCCESS;
}

Error TraceScanAreaTagged(ScanState ss, Addr *base, Addr *limit)
{
  Error e;
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
    if(((Addr)ref&3) != 0)   /* only fix 4-aligned pointers */
      goto loop;             /* not a pointer */
    if(!TRACE_FIX1(ss, ref)) goto loop;
    e = TRACE_FIX2(ss, p-1);
    if(e == ErrSUCCESS) goto loop;
    return e;
  out:
    AVER(p == limit);
  } TRACE_SCAN_END(ss);
  
  return ErrSUCCESS;
}

Error TraceRun(Space space, TraceId ti, Bool *finishedReturn)
{
  Error e;
  ScanStateStruct ss;
  
  AVER(ISVALID(Space, space));
  AVER(finishedReturn != NULL);
  
  ss.fix = TraceFix;
  ss.zoneShift = space->zoneShift;
  ss.condemned = space->trace[ti].condemned;
  ss.summary = RefSetEmpty;
  ss.space = space;
  ss.traceId = ti;
  ss.sig = ScanStateSig;

  ShieldEnter(space);

  for(ss.rank = 0; ss.rank < RefRankMAX; ++ss.rank) {
    Deque deque;
    DequeNode node;

    deque = SpacePoolDeque(space);
    node = DequeFirst(deque);

    while(node != DequeSentinel(deque)) {
      DequeNode next = DequeNodeNext(node);
      Pool pool = DEQUENODEELEMENT(Pool, spaceDeque, node);
      Bool finished;
    
      e = PoolScan(&ss, pool, &finished); 
      if(e != ErrSUCCESS) {
	ShieldLeave(space);
	return e;
      }

      if(!finished) {
	*finishedReturn = FALSE;
	ShieldLeave(space);
	return ErrSUCCESS;
      }

      node = next;
    }
  }

  ShieldLeave(space);

  ss.sig = SigInvalid;	/* just in case */

  *finishedReturn = TRUE;
  return ErrSUCCESS;
}

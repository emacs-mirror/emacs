/* impl.c.trace: GENERIC TRACER IMPLEMENTATION
 *
 *  $HopeName: MMsrc!trace.c(trunk.9) $
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
#include <limits.h>

SRCID("$HopeName: MMsrc!trace.c(trunk.9) $");



#ifdef DEBUG

Bool TraceIsValid(Trace trace, ValidationType validParam)
{
  RefRank rank;
  AVER(trace != NULL);
  AVER(trace->sig == TraceSig);
  AVER(ISVALIDNESTED(Space, trace->space));
  for(rank = 0; rank < RefRankMAX; ++rank)
    AVER(trace->work[rank].marked >= trace->work[rank].scanned);
  AVER(ISVALIDNESTED(RefRank, trace->rank));
  AVER(TraceSetIsMember(trace->space->busyTraces,
                        trace - trace->space->trace));
  return TRUE;
}

#endif /* DEBUG */


Error TraceCreate(Trace *traceReturn, Space space)
{
  RefRank rank;
  TraceId id;
  Trace trace;

  /* .single-collection */
  AVER(TRACE_MAX == 1);

  AVER(traceReturn != NULL);
  AVER(ISVALID(Space, space));

  /* allocate free TraceId */
  for(id = 0; id < TRACE_MAX; ++id)
    if(!TraceSetIsMember(space->busyTraces, id))
      goto found;
  return ErrLIMIT;

found:
  trace = &space->trace[id];
  trace->space = space;
  for(rank = 0; rank < RefRankMAX; ++rank) {
    trace->work[rank].scanned = 0;
    trace->work[rank].marked = 0;
  }
  trace->ss.fix = TraceFix;
  trace->ss.zoneShift = space->zoneShift;
  trace->ss.condemned = RefSetEmpty;
  trace->ss.summary = RefSetEmpty;
  trace->rank = 0;                /* current rank */

  trace->sig = TraceSig;
  
  space->busyTraces = TraceSetAdd(space->busyTraces, id);

  AVER(ISVALID(Trace, trace));
  
  return ErrSUCCESS;
}

void TraceDestroy(Trace trace)
{
  Space space;
  TraceId id;

  AVER(ISVALID(Trace, trace));
  
  space = trace->space;
  id = TraceTraceId(trace);

#ifdef DEBUG
  {
    RefRank rank;

    /* Check that all scanning has been done. */
    for(rank = 0; rank < RefRankMAX; ++rank)  
      AVER(trace->work[rank].scanned == trace->work[rank].marked);
  }
#endif

  space->busyTraces = TraceSetDelete(space->busyTraces, id);

  trace->sig = SigInvalid;
}  


Error TraceDescribe(Trace trace, LibStream stream)
{
  RefRank rank;

  AVER(ISVALID(Trace, trace));
  
  LibFormat(stream,
            "Trace %p {\n"
            "  space = %p\n"
            "  condemned refset = %lX\n",
            (void *)trace,
            (unsigned long)TraceTraceId(trace),
            (void *)trace->space,
            (unsigned long)trace->ss.condemned);
  
  LibFormat(stream, "  rank    marked   scanned\n");
  for(rank = 0; rank < RefRankMAX; ++rank)
    LibFormat(stream, "  %4d  %8lX  %8lX\n",
              rank,
              (unsigned long)trace->work[rank].marked,
              (unsigned long)trace->work[rank].scanned);

  LibFormat(stream, "} Trace %p\n", (void *)trace);

  return ErrSUCCESS;
}


TraceId TraceTraceId(Trace trace)
{
  AVER(ISVALID(Trace, trace));
  return trace - trace->space->trace;
}

Space TraceSpace(Trace trace)
{
  AVER(ISVALID(Trace, trace));
  return trace->space;
}

RefRank TraceRank(Trace trace)
{
  AVER(ISVALID(Trace, trace));
  return trace->rank;
}

ScanState TraceScanState(Trace trace)
{
  AVER(ISVALID(Trace, trace));
  return &trace->ss;
}


void TraceCondemn(Trace trace, RefSet rs)
{
  Arena arena;

  AVER(ISVALID(Trace, trace));

  arena = SpaceArena(trace->space);
  trace->ss.condemned = RefSetUnion(trace->ss.condemned, rs);
}


void TraceNoteMarked(Trace trace, RefRank rank, Addr count)
{
  AVER(ISVALID(Trace, trace));
  AVER(ISVALID(RefRank, rank));
  
  trace->work[rank].marked += count;
}

void TraceNoteScanned(Trace trace, Addr count)
{
  AVER(ISVALID(Trace, trace));
  AVER(count > 0);
  
  trace->work[trace->rank].scanned += count;
}

Size TracePoll(Trace trace)
{
  return SPACE_POLL_MAX;
}

Error TraceFix(ScanState ss, Ref *refIO)
{
  Arena arena;
  Pool pool;
  Ref ref;
  Trace trace = PARENT(TraceStruct, ss, ss);

  AVER(ISVALID(Trace, trace));
  AVER(refIO != NULL);

  arena = SpaceArena(trace->space);
  ref = *refIO;
  if(PoolOfAddr(&pool, arena, ref))
    return PoolFix(pool, trace, arena, refIO);

  return ErrSUCCESS;
}


Error TraceScanArea(ScanState ss, Addr *base, Addr *limit)
{
  Error e;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  while(base < limit) {
    e = TraceFix(ss, base);
    if(e != ErrSUCCESS)
      return e;
    ++base;
  }
  
  return ErrSUCCESS;
}


Error TraceRunAtomic(Trace trace)
{
  Error e;
  RefRank rank;
  Space space;

  AVER(ISVALID(Trace, trace));
  
  space = trace->space;

  /* At the moment we must scan all roots, because we don't have */
  /* a mechanism for shielding them.  There can't be any weak or */
  /* final roots either, since we must protect these in order to */
  /* avoid scanning them too early, before the pool contents. */

  for(rank = RefRankAMBIG; rank <= RefRankEXACT; ++rank) {
    Deque deque;
    DequeNode node;

    ShieldEnter(space);

    trace->rank = rank;

    deque = SpaceRootDeque(space);
    node = DequeFirst(deque);
    while(node != DequeSentinel(deque)) {
      DequeNode next = DequeNodeNext(node);
      Root root = DEQUENODEELEMENT(Root, spaceDeque, node);

      AVER(RootRank(root) <= RefRankEXACT); /* see above */

      if(RootRank(root) == rank) {
        e = RootScan(root, trace);
        if(e != ErrSUCCESS) return e;
      }

      node = next;
    }

    ShieldLeave(space);
  }

  return ErrSUCCESS;
}
 

Error TraceRun(Trace trace, Bool *finishedReturn)
{
  Error e;
  RefRank rank;
  Space space;
  
  AVER(ISVALID(Trace, trace));
  AVER(finishedReturn != NULL);
  
  space = trace->space;

  for(rank = 0; rank < RefRankMAX; ++rank) {

    trace->rank = rank;

    if(trace->work[rank].scanned < trace->work[rank].marked) {
      Deque deque;
      DequeNode node;

      ShieldEnter(space);

      deque = SpacePoolDeque(space);
      node = DequeFirst(deque);
      while(node != DequeSentinel(deque)) {
        DequeNode next = DequeNodeNext(node);
        Pool pool = DEQUENODEELEMENT(Pool, spaceDeque, node);
      
        e = PoolScan(pool, trace);
        if(e != ErrSUCCESS) {
          ShieldLeave(space);
          return e;
        }

        node = next;
      }

      ShieldLeave(space);

      *finishedReturn = FALSE;
      return ErrSUCCESS;
    }
  }

  *finishedReturn = TRUE;
  return ErrSUCCESS;
}

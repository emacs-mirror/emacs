/*  impl.c.trace
 *
 *                GENERIC TRACER IMPLEMENTATION
 *
 *  $HopeName: MMsrc!trace.c(trunk.5) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This implements the Generic Tracer defined in impl.h.trace
 *
 *  .single-collection: This implementation only supports a single
 *  concurrent collection.  See issue.single-collection
 */

#include "std.h"
#include "lib.h"
#include "mpmconf.h"
#include "ref.h"
#include "refsig.h"
#include "trace.h"
#include "space.h"
#include "pool.h"
#include "root.h"
#include "rootst.h"
#include <limits.h>


#ifdef DEBUG_SIGN
static SigStruct TraceSigStruct;
static SigStruct TraceSetSigStruct;
#endif


#define TRACEBIT(id)    ((Addr)((Addr)1 << (id)))


#ifdef DEBUG_ASSERT

Bool TraceIdIsValid(TraceId id, ValidationType validParam)
{
  AVER(id < TRACE_MAX);
  return TRUE;
}

Bool TraceSetIsValid(TraceSet set, ValidationType validParam)
{
  AVER(set != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &TraceSetSigStruct));
  AVER(set->sig == &TraceSetSigStruct);
#endif
  AVER(TRACE_MAX == ADDRWIDTH || set->bits < TRACEBIT(TRACE_MAX));
  return TRUE;
}

Bool TraceIsValid(Trace trace, ValidationType validParam)
{
  RefRank rank;
  AVER(trace != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &TraceSigStruct));
  AVER(trace->sig == &TraceSigStruct);
#endif
  AVER(ISVALIDNESTED(TraceId, trace->id));
  AVER(ISVALIDNESTED(DequeNode, &trace->spaceDeque));
  AVER(ISVALIDNESTED(Space, trace->space));
  AVER(ISVALIDNESTED(RefSig, trace->ss.condemned));
  for(rank = 0; rank < RefRankMAX; ++rank)
    AVER(trace->work[rank].marked >= trace->work[rank].scanned);
  AVER(ISVALIDNESTED(RefRank, trace->rank));
  return TRUE;
}

#endif /* DEBUG_ASSERT */


Error TraceSetInit(TraceSet set)
{
  AVER(set != NULL);

  /* Addr is used to implement the TraceSet as a bitset. */
  /* Check that it is big enough. */
  AVER(TRACE_MAX <= ADDRWIDTH);

  set->bits = (Addr)0;

#ifdef DEBUG_SIGN
  SigInit(&TraceSetSigStruct, "TraceSet");
  set->sig = &TraceSetSigStruct;
#endif

  AVER(ISVALID(TraceSet, set));

  return ErrSUCCESS;
}

void TraceSetFinish(TraceSet set)
{
  AVER(ISVALID(TraceSet, set));
#ifdef DEBUG_SIGN
  set->sig = SigInvalid;
#endif
}

void TraceSetAdd(TraceSet set, TraceId id)
{
  AVER(ISVALID(TraceSet, set));
  AVER(ISVALID(TraceId, id));
  
  set->bits |= TRACEBIT(id);
}

void TraceSetDelete(TraceSet set, TraceId id)
{
  AVER(ISVALID(TraceSet, set));
  AVER(ISVALID(TraceId, id));
  
  set->bits &= ~TRACEBIT(id);
}

Bool TraceSetIsMember(TraceSet set, TraceId id)
{
  AVER(ISVALID(TraceSet, set));
  AVER(ISVALID(TraceId, id));
  
  return (set->bits & TRACEBIT(id)) != 0;
}

/* If there is a TraceId not in set, then find
 * returns TRUE and puts this id in *idReturn.  Otherwise
 * FALSE is returned.
 */
static Bool find(TraceSet set, TraceId *idReturn)
{
  TraceId id;
  for(id = 0; id < TRACE_MAX; ++id)
    if(!TraceSetIsMember(set, id)) {
      *idReturn = id;
      return TRUE;
    }
  return FALSE;
}

Error TraceInit(Trace trace, Space space)
{
  RefRank rank;
  TraceId id;

  /* .single-collection */
  AVER(TRACE_MAX == 1);

  AVER(trace != NULL);
  AVER(ISVALID(Space, space));

  /* allocate free TraceId */
  if(!find(SpaceTraceSet(space), &id))
    return ErrLIMIT;

  DequeNodeInit(&trace->spaceDeque);
  trace->id = id;
  trace->space = space;
  for(rank = 0; rank < RefRankMAX; ++rank) {
    trace->work[rank].scanned = 0;
    trace->work[rank].marked = 0;
  }
  trace->ss.fix = TraceFix;
  trace->ss.zoneShift = 0;        /* zoneShift */
  /* trace->ss.condemned = RefSigEmpty(SpaceArena(space)); */
  trace->ss.condemned = (Addr)-1; /* condemned */
  trace->ss.summary = 0;          /* summary   */
  trace->rank = 0;                /* current rank */

#ifdef DEBUG_SIGN
  SigInit(&TraceSigStruct, "Trace");
  trace->sig = &TraceSigStruct;
#endif
  
  AVER(ISVALID(Trace, trace));

  TraceSetAdd(SpaceTraceSet(space), id);
  DequeAppend(SpaceTraceDeque(space), &trace->spaceDeque);
  
  return ErrSUCCESS;
}

void TraceFinish(Trace trace)
{
  AVER(ISVALID(Trace, trace));

#ifdef DEBUG_ASSERT
  {
    RefRank rank;

    /* Check that all scanning has been done. */
    for(rank = 0; rank < RefRankMAX; ++rank)  
      AVER(trace->work[rank].scanned == trace->work[rank].marked);
  }
#endif

  DequeNodeRemove(&trace->spaceDeque);
  TraceSetDelete(SpaceTraceSet(trace->space), trace->id);

#ifdef DEBUG_SIGN
  trace->sig = SigInvalid;
#endif
}  


Error TraceCreate(Trace *traceReturn, Space space)
{
  Error e;
  Trace trace;
  Pool controlPool;

  AVER(traceReturn != NULL);
  AVER(ISVALID(Space, space));
  
  controlPool = SpaceControlPool(space);

  e = PoolAlloc((Addr *)&trace, controlPool, sizeof(TraceStruct));
  if(e != ErrSUCCESS) return e;
  
  e = TraceInit(trace, space);
  if(e != ErrSUCCESS) {
    PoolFree(controlPool, (Addr)trace, sizeof(TraceStruct));
    return e;
  }
  
  *traceReturn = trace;
  return ErrSUCCESS;
}


void TraceDestroy(Trace trace)
{
  Pool controlPool;
  AVER(ISVALID(Trace, trace));
  controlPool = SpaceControlPool(trace->space);;
  TraceFinish(trace);
  PoolFree(controlPool, (Addr)trace, sizeof(TraceStruct));
}


Error TraceDescribe(Trace trace, LibStream stream)
{
  RefRank rank;

  AVER(ISVALID(Trace, trace));
  
  LibFormat(stream,
            "Trace %p {\n"
            "  space = %p\n"
            "  condemned refsig = %lX\n",
            (void *)trace,
            (unsigned long)trace->id,
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
  return trace->id;
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

/* thread safe */
Trace TraceOfScanState(ScanState ss)
{
  return PARENT(TraceStruct, ss, ss);
}

void TraceCondemn(Trace trace, RefSig refsig)
{
  Arena arena;

  AVER(ISVALID(Trace, trace));

  arena = SpaceArena(trace->space);
  /* trace->ss.condemned = RefSigUnion(trace->ss.condemned, refsig, arena); */
  trace->ss.condemned = (Addr)-1;
}


void TraceNoteMarked(Trace trace, RefRank rank, Addr count)
{
  AVER(ISVALID(Trace, trace));
  AVER(ISVALID(RefRank, rank));
  
  trace->work[rank].marked += count;
}

void TraceNoteScanned(Trace trace, RefRank rank, Addr count)
{
  AVER(ISVALID(Trace, trace));
  AVER(ISVALID(RefRank, rank));
  AVER(count > 0);
  
  trace->work[rank].scanned += count;
}


Error TraceFix(ScanState ss, Ref *refIO)
{
  Arena arena;
  Pool pool;
  Ref ref;
  Trace trace = PARENT(TraceStruct, ss, ss);
  RefRank rank;

  AVER(ISVALID(Trace, trace));
  AVER(refIO != NULL);
  rank = trace->rank;
  AVER(ISVALID(RefRank, rank));

  arena = SpaceArena(trace->space);
  ref = *refIO;
  if(PoolOfAddr(&pool, arena, ref))
    return PoolFix(pool, trace, rank, arena, refIO);

  return ErrSUCCESS;
}


Error TraceScanArea(Addr *base, Addr *limit, Trace trace, RefRank rank)
{
  Error e;

  AVER(base != NULL);
  AVER(limit != NULL);
  AVER(base < limit);

  while(base < limit) {
    e = TraceFix(TraceScanState(trace), (Ref *)base);
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
  Shield shield;

  AVER(ISVALID(Trace, trace));

  shield = SpaceShield(trace->space);

  /* At the moment we must scan all roots, because we don't have */
  /* a mechanism for shielding them.  There can't be any weak or */
  /* final roots either, since we must protect these in order to */
  /* avoid scanning them too early, before the pool contents. */

  for(rank = RefRankAMBIG; rank <= RefRankEXACT; ++rank) {
    Deque deque;
    DequeNode node;

    ShieldEnter(shield);

    trace->rank = rank;

    deque = SpaceRootDeque(trace->space);
    node = DequeFirst(deque);
    while(node != DequeSentinel(deque)) {
      DequeNode next = DequeNodeNext(node);
      Root root = DEQUENODEELEMENT(Root, spaceDeque, node);

      AVER(RootRank(root) <= RefRankEXACT); /* see above */

      if(RootRank(root) == rank) {
        e = RootScan(root, trace, rank);
        if(e != ErrSUCCESS) return e;
      }

      node = next;
    }

    ShieldLeave(shield);
  }

  return ErrSUCCESS;
}
 

Error TraceRun(Trace trace, Bool *finishedReturn)
{
  Error e;
  RefRank rank;
  Shield shield;
  
  AVER(ISVALID(Trace, trace));
  AVER(finishedReturn != NULL);

  shield = SpaceShield(trace->space);

  for(rank = 0; rank < RefRankMAX; ++rank) {

    trace->rank = rank;

    if(trace->work[rank].scanned < trace->work[rank].marked) {
      Deque deque;
      DequeNode node;

      ShieldEnter(shield);

      deque = SpacePoolDeque(trace->space);
      node = DequeFirst(deque);
      while(node != DequeSentinel(deque)) {
        DequeNode next = DequeNodeNext(node);
        Pool pool = DEQUENODEELEMENT(Pool, spaceDeque, node);
      
        e = PoolScan(pool, trace, rank);
        if(e != ErrSUCCESS) {
          ShieldLeave(shield);
          return e;
        }

        node = next;
      }

      ShieldLeave(shield);

      *finishedReturn = FALSE;
      return ErrSUCCESS;
    }
  }

  *finishedReturn = TRUE;
  return ErrSUCCESS;
}

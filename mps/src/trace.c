/*  impl.c.trace
 *
 *                GENERIC TRACER IMPLEMENTATION
 *
 *  $HopeName: MMsrc/!trace.c(trunk.2)$
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


#define TRACEBIT(id)    ((Addr)(1uL << (id)))


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
  AVER(ISVALIDNESTED(RefSig, trace->condemned));
  for(rank = 0; rank < RefRankMAX; ++rank)
    AVER(trace->work[rank].marked >= trace->work[rank].scanned);
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
  trace->condemned = RefSigEmpty(SpaceArena(space));
  for(rank = 0; rank < RefRankMAX; ++rank) {
    trace->work[rank].scanned = 0;
    trace->work[rank].marked = 0;
  }

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

  e = PoolAllocP((void **)&trace, controlPool, sizeof(TraceStruct));
  if(e != ErrSUCCESS) return e;
  
  e = TraceInit(trace, space);
  if(e != ErrSUCCESS) {
    PoolFreeP(controlPool, (void *)trace, sizeof(TraceStruct));
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
  PoolFreeP(controlPool, (void *)trace, sizeof(TraceStruct));
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
            (unsigned long)trace->condemned);
  
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

void TraceCondemn(Trace trace, RefSig refsig)
{
  Arena arena;

  AVER(ISVALID(Trace, trace));

  arena = SpaceArena(trace->space);
  trace->condemned = RefSigUnion(trace->condemned, refsig, arena);
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


Error TraceFix(Trace trace, RefRank rank, Ref *refIO)
{
  Arena arena;
  Pool pool;
  Ref ref;

  AVER(ISVALID(Trace, trace));
  AVER(ISVALID(RefRank, rank));
  AVER(refIO != NULL);

  arena = SpaceArena(trace->space);
  ref = *refIO;
  if(RefSigIsMember(trace->condemned, arena, ref))
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
    e = TraceFix(trace, rank, (Ref *)base);
    if(e != ErrSUCCESS)
      return e;
    ++base;
  }
  
  return ErrSUCCESS;
}


Error TraceRun(Trace trace)
{
  Error e;
  RefRank rank;
  
  AVER(ISVALID(Trace, trace));
  
  rank = 0;
  while(rank < RefRankMAX) {

    if(trace->work[rank].scanned < trace->work[rank].marked) {
      Deque deque;
      DequeNode node;

      /* It would be correct to scan the roots and pools
       * in a different order.  Roots will create
       * work for pools to scan, but not the other way
       * round, so it makes sense to scan them in this
       * order.  It is safe to scan roots multiple
       * times but they are only do scanning work the
       * first time through
       */
      
      deque = SpaceRootDeque(trace->space);
      node = DequeFirst(deque);
      while(node != DequeSentinel(deque)) {
        DequeNode next = DequeNodeNext(node);
        Root root = DEQUENODEELEMENT(Root, spaceDeque, node);
        
        e = RootScan(root, trace, rank);
        if(e != ErrSUCCESS)
          return e;
        
        node = next;
      }

      deque = SpacePoolDeque(trace->space);
      node = DequeFirst(deque);
      while(node != DequeSentinel(deque)) {
        DequeNode next = DequeNodeNext(node);
        Pool pool = DEQUENODEELEMENT(Pool, spaceDeque, node);
      
        e = PoolScan(pool, trace, rank);
        if(e != ErrSUCCESS)
          return e;

        node = next;
      }
      
      /*  If there was something to scan at RefRank rank
       *  then this may generate more work at RefRanks 
       *  less than rank.  rank is set to 0 so that we
       *  do this work, if generated, before we proceed.
       */
      rank = 0;
    }
    else
      ++rank;
  }

#ifdef DEBUG_ASSERT
  for(rank = 0; rank < RefRankMAX; ++rank)
    AVER(trace->work[rank].scanned == trace->work[rank].marked);
#endif
  
  return ErrSUCCESS;
}

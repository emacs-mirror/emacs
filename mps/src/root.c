/*  impl.c.root
 *
 *                   ROOT IMPLEMENTATION
 *
 *  $HopeName: MMsrc!root.c(trunk.5) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of roots.
 */

#include "std.h"
#include "lib.h"
#include "deque.h"
#include "root.h"
#include "rootst.h"
#include "pool.h"
#include "mpmconf.h"
#include "ref.h"
#include "trace.h"
#include "spacest.h"


#ifdef DEBUG_SIGN
static SigStruct RootSigStruct;
#endif


#ifdef DEBUG_ASSERT

static Bool modeIsValid(RootMode mode)
{
  AVER((mode &~(RootFIXABLE | RootMUTABLE | RootATOMIC)) == 0);
  /* Need to check for legal combinations of modes. */
  return TRUE;
}

#endif


Bool RootIsValid(Root root, ValidationType validParam)
{
  AVER(root != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &RootSigStruct));
  AVER(root->sig == &RootSigStruct);
#endif
  AVER(ISVALIDNESTED(DequeNode, &root->spaceDeque));
  AVER(modeIsValid(root->mode));
  AVER(ISVALIDNESTED(RefRank, root->rank));
  AVER(ISVALIDNESTED(TraceSet, &root->marked));
  AVER(root->type == RootTABLE || root->type == RootFUN);
  switch(root->type)
  {
    case RootTABLE:
    AVER(root->the.table.base != 0);
    AVER(root->the.table.base < root->the.table.limit);
    break;
    
    case RootFUN:
    AVER(root->the.fun.scan != NULL);
    break;

    default:
    NOTREACHED;
  }
  return TRUE;
}


static Error create(Root *rootReturn, Space space,
                    RefRank rank, RootMode mode,
                    RootType type, RootUnion theUnion)
{
  Root root;
  Error e;

  AVER(rootReturn != NULL);
  AVER(ISVALID(Space, space));
  AVER(modeIsValid(mode));
  AVER(ISVALID(RefRank, rank));

  e = PoolAlloc((Addr *)&root, SpaceControlPool(space),
                sizeof(RootStruct));
  if(e != ErrSUCCESS)
    return e;

  root->rank = rank;
  root->mode = mode;
  root->type = type;
  root->the  = theUnion;
  TraceSetInit(&root->marked);

  DequeNodeInit(&root->spaceDeque);

#ifdef DEBUG_SIGN
  SigInit(&RootSigStruct, "Root");
  root->sig = &RootSigStruct;
#endif

  AVER(ISVALID(Root, root));

  DequeAppend(SpaceRootDeque(space), &root->spaceDeque);

  *rootReturn = root;
  return ErrSUCCESS;
}


Error RootCreateTable(Root *rootReturn, Space space,
                      RefRank rank, RootMode mode,
                      Addr *base, Addr *limit)
{
  RootUnion theUnion;

  AVER(base != 0);
  AVER(base < limit);

  theUnion.table.base = base;
  theUnion.table.limit = limit;
  
  return create(rootReturn, space, rank, mode, RootTABLE, theUnion);
}


Error RootCreate(Root *rootReturn, Space space,
                 RefRank rank, RootMode mode,
                 Error (*scan)(ScanState ss, void *p, size_t s),
                 void *p, size_t s)
{
  RootUnion theUnion;
  
  AVER(scan != NULL);

  theUnion.fun.scan = scan;
  theUnion.fun.p = p;
  theUnion.fun.s = s;

  return create(rootReturn, space, rank, mode, RootFUN, theUnion);
}


void RootDestroy(Root root)
{
  Space space;

  AVER(ISVALID(Root, root));

  space = RootSpace(root);

  AVER(ISVALID(Space, space));

  DequeNodeRemove(&root->spaceDeque);
  DequeNodeFinish(&root->spaceDeque);
  
  TraceSetFinish(&root->marked);

#ifdef DEBUG_SIGN
  root->sig = SigInvalid;
#endif

  PoolFree(SpaceControlPool(space), (Addr)root, sizeof(RootStruct));
}


Bool RootIsAtomic(Root root)
{
  AVER(ISVALID(Root, root));

  return (root->mode & RootATOMIC) != 0;
}


RefRank RootRank(Root root)
{
  AVER(ISVALID(Root, root));

  return root->rank;
}


void RootMark(Root root, Trace trace)
{
  AVER(ISVALID(Root, root));
  AVER(ISVALID(Trace, trace));
  
  TraceSetAdd(&root->marked, TraceTraceId(trace));
  TraceNoteMarked(trace, root->rank, (Addr)1);
}


Error RootScan(Root root, Trace trace, RefRank rank)
{
  Error e;

  AVER(ISVALID(Root, root));
  AVER(ISVALID(Trace, trace));
  AVER(rank == TraceRank(trace));
  
  if(rank != root->rank)
    return ErrSUCCESS;

  if(!TraceSetIsMember(&root->marked, TraceTraceId(trace)))
    return ErrSUCCESS;

  switch(root->type) {
    case RootTABLE: {
      Addr *base, *what, *limit;
      base = (Addr *)root->the.table.base;
      what = base;
      limit = (Addr *)root->the.table.limit;
      /* doesn't accumulate summary or do zone test */
      while(what < limit) {
        e = TraceFix(TraceScanState(trace), what);
        if(e != ErrSUCCESS)
          return e;
        ++what;
      }
    }
    break;

    case RootFUN:
    e = (*root->the.fun.scan)(TraceScanState(trace),
        root->the.fun.p, root->the.fun.s);
    if(e != ErrSUCCESS)
      return e;
    break;

    default:
    NOTREACHED;
  }

  TraceNoteScanned(trace, rank, (Addr)1);
  TraceSetDelete(&root->marked, TraceTraceId(trace));
  
  return ErrSUCCESS;
}

/* Thread safe */
Space RootSpace(Root root)
{
  return PARENT(SpaceStruct, rootDeque, root->spaceDeque.deque);
}

Error RootDescribe(Root root, LibStream stream)
{
  TraceId id;

  AVER(ISVALID(Root, root));
  AVER(stream != NULL);
  
  LibFormat(stream,
            "Root %lX {\n"
            "  rank %d\n"
            "  mode%s%s%s\n",
            (unsigned long)root,
            root->rank,
            root->mode & RootATOMIC  ? " ATOMIC"  : "",
            root->mode & RootFIXABLE ? " FIXABLE" : "",
            root->mode & RootMUTABLE ? " MUTABLE" : "");

  LibFormat(stream, "  Trace status\n");
  for(id = 0; id < TRACE_MAX; ++id)
    LibFormat(stream, "    %2lu %s\n",
              (unsigned long)id,
              TraceSetIsMember(&root->marked, id) ? "marked" : "not marked");
  
  switch(root->type)
  {
    case RootTABLE:
    LibFormat(stream, "  table base 0x%lX limit 0x%lX\n",
            (unsigned long)root->the.table.base,
            (unsigned long)root->the.table.limit);
    break;
    
    case RootFUN:
    LibFormat(stream,
            "  scan function 0x%lX\n"
            "  environment p 0x%lX s 0x%lX\n",
            (unsigned long)root->the.fun.scan,
            (unsigned long)root->the.fun.p,
            root->the.fun.s);
    break;

    default:
    NOTREACHED;
  }
  
  LibFormat(stream, "} Root 0x%lX\n", (unsigned long)root);
  
  return ErrSUCCESS;
}

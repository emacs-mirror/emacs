/* impl.c.walk: OBJECT WALKER
 *
 * $HopeName: MMsrc!walk.c(trunk.2) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 */


#include "mpm.h"
#include "mps.h"

SRCID(walk, "$HopeName: MMsrc!walk.c(trunk.2) $");



/* Heap Walking
 *
 */


#define FormattedObjectsStepClosureSig ((Sig)0x519F05C1)

typedef struct FormattedObjectsStepClosureStruct *FormattedObjectsStepClosure;

typedef struct FormattedObjectsStepClosureStruct {
  Sig sig;
  mps_formatted_objects_stepper_t f;
  void *p;
  size_t s;
} FormattedObjectsStepClosureStruct;


static Bool FormattedObjectsStepClosureCheck(FormattedObjectsStepClosure c)
{
  CHECKS(FormattedObjectsStepClosure, c);
  CHECKL(FUNCHECK(c->f));
  /* p and s fields are arbitrary closures which cannot be checked */
  return TRUE;
}

static void ArenaFormattedObjectsStep(Addr object, Format format, Pool pool,
                                      void *p, Size s)
{
  FormattedObjectsStepClosure c;
  /* Can't check object */
  AVERT(Format, format);
  AVERT(Pool, pool);
  c = p;
  AVERT(FormattedObjectsStepClosure, c);
  AVER(s == 0);

  (*c->f)((mps_addr_t)object, (mps_fmt_t)format, (mps_pool_t)pool, 
          c->p, c->s);
}


/* ArenaFormattedObjectsWalk -- iterate over all objects
 *
 * so called because it walks all formatted objects in an arena 
 */

static void ArenaFormattedObjectsWalk(Arena arena,
                                      FormattedObjectsStepMethod f,
                                          void *p, Size s)
{
  Seg seg;
  FormattedObjectsStepClosure c;

  AVERT(Arena, arena);
  AVER(FUNCHECK(f));
  AVER(f == ArenaFormattedObjectsStep);
  /* p and s are arbitrary closures. */
  /* Know that p is a FormattedObjectsStepClosure  */
  /* Know that s is 0 */
  AVER(p != NULL);
  AVER(s == 0);

  c = p;
  AVERT(FormattedObjectsStepClosure, c);

  if(SegFirst(&seg, arena)) {
    Addr base;
    do {
      Pool pool;
      base = SegBase(seg);
      pool = SegPool(seg);
      if(pool->class->attr & AttrFMT) {
        ShieldExpose(arena, seg);
        PoolWalk(pool, seg, f, p, s);
        ShieldCover(arena, seg);
      }
    } while(SegNext(&seg, arena, base));
  }
}


/* mps_arena_formatted_objects_walk -- iterate over all objects
 *
 * Client interface to ArenaFormattedObjectsWalk
 */

void mps_arena_formatted_objects_walk(mps_arena_t mps_arena,
                                      mps_formatted_objects_stepper_t f,
                                      void *p,
                                      size_t s)
{
  Arena arena = (Arena)mps_arena;
  FormattedObjectsStepClosureStruct c;

  ArenaEnter(arena);
  AVERT(Arena, arena);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures, hence can't be checked */
  c.sig = FormattedObjectsStepClosureSig;
  c.f = f;
  c.p = p;
  c.s = s;
  ArenaFormattedObjectsWalk(arena, ArenaFormattedObjectsStep, &c, 0);
  ArenaLeave(arena);
}



/* Root Walking
 *
 * This involves more code than it should. The roots are walked 
 * by scanning them. But there's no direct support for
 * invoking the scanner without there being a trace, and there's
 * no direct support for creating a trace without also condemning
 * part of the heap. (@@@@ This looks like a useful canditate for
 * inclusion in the future). For now, the root walker contains 
 * its own code for creating a minimal trace and scan state.
 *
 * ASSUMPTIONS
 *
 * .assume.parked: The root walker must be invoked with a parked
 * arena. It's only strictly necessary for there to be no current 
 * trace, but the client has no way to ensure this apart from
 * parking the arena.
 *
 * .assume.rootaddr: The client closure is called with a parameter
 * which is the address of a reference to an object referenced from 
 * a root. The client may desire this address to be the address of
 * the actual reference in the root (so that the debugger can be 
 * used to determine details about the root). This is not always 
 * possible, since the root might actually be a register, or the 
 * format scan method might not pass this address directly to the 
 * fix method. If the format code does pass on the address, the 
 * client can be sure to be passed the address of any root other
 * than a register or stack.
 */


/* RootsStepClosure -- closure environment for root walker
 *
 * Defined as a subclass of ScanState 
 */

/* SIGnature Roots Step CLOsure */
#define RootsStepClosureSig ((Sig)0x51965C10)  

typedef struct RootsStepClosureStruct *RootsStepClosure;
typedef struct RootsStepClosureStruct {
  ScanStateStruct ssStruct;          /* generic scan state object */
  mps_roots_stepper_t f;             /* client closure function */
  void *p;                           /* client closure data */
  size_t s;                          /* client closure data */
  Root root;                         /* current root, or NULL */
  Sig sig;                           /* impl.h.misc.sig */
} RootsStepClosureStruct;

static Bool RootsStepClosureCheck(RootsStepClosure rsc)
{
  CHECKS(RootsStepClosure, rsc);
  CHECKD(ScanState, &rsc->ssStruct);
  CHECKL(FUNCHECK(rsc->f));
  /* p and s fields are arbitrary closures which cannot be checked */
  if (rsc->root != NULL) {
    CHECKL(RootCheck(rsc->root));
  }
  return TRUE;
}

static ScanState RootsStepClosureScanState(RootsStepClosure rsc)
{
  AVERT(RootsStepClosure, rsc);

  return &rsc->ssStruct;
}

static RootsStepClosure ScanStateRootsStepClosure(ScanState ss)
{
  AVERT(ScanState, ss);

  return PARENT(RootsStepClosureStruct, ssStruct, ss);
}


/* RootsStepClosureInit -- Initialize a RootsStepClosure
 *
 * Initialize the parent ScanState too.
 */

static void RootsStepClosureInit(RootsStepClosure rsc, 
                                 Arena arena,
                                 Trace trace,
                                 TraceFixMethod rootFix,
                                 mps_roots_stepper_t f,
                                 void *p, Size s)
{
  ScanState ss;

  /* we are initing it, so we can't check rsc */
  AVERT(Arena, arena);
  AVERT(Trace, trace);
  AVER(FUNCHECK(rootFix));
  AVER(FUNCHECK(f));
  /* p and s are arbitrary client-provided closure data. */

  /* First initialize the ScanState superclass */
  ss = &rsc->ssStruct;
  ScanStateInit(ss, TraceSetSingle(trace), arena, RankAMBIG, trace->white);

  /* Initialize the fix method in the ScanState */
  ss->fix = rootFix;

  /* Initialize subclass specific data */
  rsc->f = f;
  rsc->p = p;
  rsc->s = s;
  rsc->root = NULL;

  rsc->sig = RootsStepClosureSig;

  AVERT(RootsStepClosure, rsc);
}


/* RootsStepClosureFinish -- Finish a RootsStepClosure
 *
 * Finish the parent ScanState too.
 */ 

static void RootsStepClosureFinish(RootsStepClosure rsc)
{
  ScanState ss;

  AVERT(RootsStepClosure, rsc);

  ss = RootsStepClosureScanState(rsc);
  rsc->sig = SigInvalid;
  ScanStateFinish(ss);
}

/* RootsWalkTraceStart -- Initialize a minimal trace for root walking */

static Res RootsWalkTraceStart(Trace trace)
{
  Ring ring, node, next;
  Arena arena;

  AVERT(Trace, trace);
  arena = trace->arena;

  /* Set the white ref set to universal so that the scanner */
  /* doesn't filter out any references from roots into the arena */
  trace->white = RefSetUNIV; 

  /* Make the roots grey so that they are scanned */
  ring = ArenaRootRing(arena);
  RING_FOR(node, ring, next) {
    Root root = RING_ELT(Root, arenaRing, node);
    RootGrey(root, trace);
  }

  return ResOK;
} 


/* RootsWalkTraceFinish -- Finish a minimal trace for root walking */

static void RootsWalkTraceFinish(Trace trace)
{
  Arena arena;

  AVERT(Trace, trace);

  /* Make this trace look like any other finished trace. */
  /* Need to set the state of the trace, and add it to the  */
  /* arena's set of flipped traces */
  arena = trace->arena;
  arena->flippedTraces = TraceSetAdd(arena->flippedTraces, trace);
  trace->state = TraceFINISHED;
  TraceDestroy(trace);
}


/* RootsWalkFix -- the fix method used during root walking
 *
 * This doesn't cause further scanning of transitive references, 
 * it just calls the client closure.
 */

static Res RootsWalkFix(ScanState ss, Ref *refIO)
{
  RootsStepClosure rsc;
  Root root;
  Ref ref;
  Seg seg;
  Arena arena;
  
  AVERT(ScanState, ss);
  AVER(refIO != NULL);

  rsc = ScanStateRootsStepClosure(ss);
  AVERT(RootsStepClosure, rsc);

  root = rsc->root;
  AVERT(Root, root);

  arena = ss->arena;
  ref = *refIO;

  /* Check that the reference is to a valid segment */
  if(SegOfAddr(&seg, arena, ref)) {
    /* Test if the segment belongs to a GCable pool */
    /* If it isn't then it's not in the heap, and the reference */
    /* shouldn't be passed to the client */
    if ((SegPool(seg)->class->attr & AttrGC) != 0) {
      /* Call the client closure - .assume.rootaddr */
      rsc->f((mps_addr_t*)refIO, 
             (mps_root_t)root, 
             rsc->p, rsc->s);
    }
  } else {
    /* See design.mps.trace.exact.legal */
    AVER(ss->rank < RankEXACT
         || !ArenaIsReservedAddr(arena, ref));
  }

  /* See design.mps.trace.fix.fixed.all */
  ss->fixedSummary = RefSetAdd(ss->arena, ss->fixedSummary, *refIO);

  AVER(ref == *refIO);  /* can walk object graph - but not modify it */

  return ResOK;
}


/* ArenaRootsWalk -- starts the trace and scans the roots 
 */

static Res ArenaRootsWalk(Arena arena, 
                          mps_roots_stepper_t f,
                          void *p, size_t s)
{
  RootsStepClosureStruct rscStruct;
  RootsStepClosure rsc = &rscStruct;
  Trace trace;
  ScanState ss;
  Rank rank;
  Res res;

  AVERT(Arena, arena);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary client-provided closure data. */

  /* Scan all the roots with a minimal trace. */
  /* Invoke the scanner with a RootsStepClosure, which */
  /* is a subclass of ScanState and contains the client */
  /* provided closure. Supply a special fix method */
  /* in order to call the client closure. This fix method */
  /* must perform no tracing operations of its own */

  res = TraceCreate(&trace, arena);
  /* Have to fail if no trace available. Unlikely due to .assume.parked */
  if(res != ResOK)
    return res;

  res = RootsWalkTraceStart(trace);
  if(res != ResOK)
    return res;
 
  RootsStepClosureInit(rsc, arena, trace, RootsWalkFix, f, p, s);
  ss = RootsStepClosureScanState(rsc);

  for(rank = RankAMBIG; rank < RankMAX; ++rank) {
    Ring ring = ArenaRootRing(arena);
    Ring node, next;
    ss->rank = rank;

    AVERT(ScanState, ss);

    RING_FOR(node, ring, next) {
      Root root = RING_ELT(Root, arenaRing, node);

      if(RootRank(root) == ss->rank) {
        /* set the root for the benefit of the fix method */
        rsc->root = root;
        /* Scan it */
        ScanStateSetSummary(ss, RefSetEMPTY);
        res = RootScan(ss, root);
        if(res != ResOK) {
          return res;
        }
      }
    }
  }

  RootsStepClosureFinish(rsc);
  RootsWalkTraceFinish(trace);

  return ResOK;
}


/* mps_arena_roots_walk -- Client interface for walking */

void mps_arena_roots_walk(mps_arena_t mps_arena,
                          mps_roots_stepper_t f,
                          void *p, size_t s)
{
  Arena arena = (Arena)mps_arena;
  Res res;

  ArenaEnter(arena);
  AVERT(Arena, arena);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures, hence can't be checked */

  AVER(TRUE == arena->clamped);                /* .assume.parked */
  AVER(arena->busyTraces == TraceSetEMPTY);    /* .assume.parked */

  res = ArenaRootsWalk(arena, f, p, s);
  AVER(res == ResOK);
  ArenaLeave(arena);
}

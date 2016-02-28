/* walk.c: OBJECT WALKER
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
#include "mps.h"

SRCID(walk, "$Id$");


/* mps_arena_formatted_objects_walk -- iterate over all objects */

typedef struct ObjectsWalkClosureStruct {
  Arena arena;
  mps_formatted_objects_stepper_t stepper;
  void *closureP;
  size_t closureS;
} ObjectsWalkClosureStruct, *ObjectsWalkClosure;

static void objectsWalkStep(Addr addr, Format format, Pool pool,
                            void *closureP, Size closureS)
{
  ObjectsWalkClosure ow = closureP;
  AVER(closureS == sizeof(*ow));
  ow->stepper(addr, format, pool, ow->closureP, ow->closureS);
}

static Bool objectsWalkVisit(Seg seg, void *closure)
{
  ObjectsWalkClosure ow = closure;
  Pool pool;
  
  pool = SegPool(seg);
  if (PoolHasAttr(pool, AttrFMT)) {
    ShieldExpose(ow->arena, seg);
    PoolWalk(pool, seg, objectsWalkStep, ow, sizeof(*ow));
    ShieldCover(ow->arena, seg);
  }

  return TRUE;
}

void mps_arena_formatted_objects_walk(mps_arena_t mps_arena,
                                      mps_formatted_objects_stepper_t stepper,
                                      void *closureP, size_t closureS)
{
  Arena arena = (Arena)mps_arena;
  ObjectsWalkClosureStruct owStruct;

  ArenaEnter(arena);

  AVERT(Arena, arena);
  AVER(FUNCHECK(stepper));
  /* closureP and closureS are arbitrary closures, hence can't be checked */

  owStruct.arena = arena;
  owStruct.stepper = stepper;
  owStruct.closureP = closureP;
  owStruct.closureS = closureS;
  SegTraverse(arena, objectsWalkVisit, &owStruct);

  ArenaLeave(arena);
}



/* Root Walking
 *
 * This involves more code than it should. The roots are walked by
 * scanning them. But there's no direct support for invoking the scanner
 * without there being a trace, and there's no direct support for
 * creating a trace without also condemning part of the heap. (@@@@ This
 * looks like a useful candidate for inclusion in the future). For now,
 * the root walker contains its own code for creating a minimal trace
 * and scan state.
 *
 * ASSUMPTIONS
 *
 * .assume.parked: The root walker must be invoked with a parked
 * arena. It's only strictly necessary for there to be no current trace,
 * but the client has no way to ensure this apart from parking the
 * arena.
 *
 * .assume.rootaddr: The client closure is called with a parameter which
 * is the address of a reference to an object referenced from a
 * root. The client may desire this address to be the address of the
 * actual reference in the root (so that the debugger can be used to
 * determine details about the root). This is not always possible, since
 * the root might actually be a register, or the format scan method
 * might not pass this address directly to the fix method. If the format
 * code does pass on the address, the client can be sure to be passed
 * the address of any root other than a register or stack.  */


/* rootsStepClosure -- closure environment for root walker
 *
 * Defined as a subclass of ScanState.  */

/* SIGnature Roots Step CLOsure */
#define rootsStepClosureSig ((Sig)0x51965C10) 

typedef struct rootsStepClosureStruct *rootsStepClosure;
typedef struct rootsStepClosureStruct {
  ScanStateStruct ssStruct;          /* generic scan state object */
  mps_roots_stepper_t f;             /* client closure function */
  void *p;                           /* client closure data */
  size_t s;                          /* client closure data */
  Root root;                         /* current root, or NULL */
  Sig sig;                           /* <code/misc.h#sig> */
} rootsStepClosureStruct;

#define rootsStepClosure2ScanState(rsc) (&(rsc)->ssStruct)
#define ScanState2rootsStepClosure(ss) \
  PARENT(rootsStepClosureStruct, ssStruct, ss)


/* rootsStepClosureCheck -- check a rootsStepClosure */

ATTRIBUTE_UNUSED
static Bool rootsStepClosureCheck(rootsStepClosure rsc)
{
  CHECKS(rootsStepClosure, rsc);
  CHECKD(ScanState, &rsc->ssStruct);
  CHECKL(FUNCHECK(rsc->f));
  /* p and s fields are arbitrary closures which cannot be checked */
  if (rsc->root != NULL) {
    CHECKD_NOSIG(Root, rsc->root); /* <design/check/#.hidden-type> */
  }
  return TRUE;
}


/* rootsStepClosureInit -- Initialize a rootsStepClosure
 *
 * Initialize the parent ScanState too.  */

static void rootsStepClosureInit(rootsStepClosure rsc,
                                 Globals arena, Trace trace,
                                 PoolFixMethod rootFix,
                                 mps_roots_stepper_t f, void *p, size_t s)
{
  ScanState ss;

  /* First initialize the ScanState superclass */
  ss = &rsc->ssStruct;
  ScanStateInit(ss, TraceSetSingle(trace), GlobalsArena(arena), RankMIN,
                trace->white);

  /* Initialize the fix method in the ScanState */
  ss->fix = rootFix;

  /* Initialize subclass specific data */
  rsc->f = f;
  rsc->p = p;
  rsc->s = s;
  rsc->root = NULL;

  rsc->sig = rootsStepClosureSig;

  AVERT(rootsStepClosure, rsc);
}


/* rootsStepClosureFinish -- Finish a rootsStepClosure
 *
 * Finish the parent ScanState too.  */

static void rootsStepClosureFinish(rootsStepClosure rsc)
{
  ScanState ss;

  ss = rootsStepClosure2ScanState(rsc);
  rsc->sig = SigInvalid;
  ScanStateFinish(ss);
}


/* RootsWalkFix -- the fix method used during root walking
 *
 * This doesn't cause further scanning of transitive references, it just
 * calls the client closure.  */

static Res RootsWalkFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  rootsStepClosure rsc;
  Ref ref;
        
  UNUSED(pool);

  AVERT(ScanState, ss);
  AVER(refIO != NULL);
  rsc = ScanState2rootsStepClosure(ss);
  AVERT(rootsStepClosure, rsc);

  ref = *refIO;

  /* If the segment isn't GCable then the ref is not to the heap and */
  /* shouldn't be passed to the client. */
  AVER(PoolHasAttr(SegPool(seg), AttrGC));

  /* Call the client closure - .assume.rootaddr */
  rsc->f((mps_addr_t*)refIO, (mps_root_t)rsc->root, rsc->p, rsc->s);

  AVER(ref == *refIO);  /* can walk object graph - but not modify it */

  return ResOK;
}


/* rootWalk -- the step function for ArenaRootsWalk */

static Res rootWalk(Root root, void *p)
{
  ScanState ss = (ScanState)p;

  AVERT(ScanState, ss);

  if (RootRank(root) == ss->rank) {
    /* set the root for the benefit of the fix method */
    ScanState2rootsStepClosure(ss)->root = root;
    /* Scan it */
    ScanStateSetSummary(ss, RefSetEMPTY);
    return RootScan(ss, root);
  } else
    return ResOK;
}


/* rootWalkGrey -- make the root grey for the trace passed as p */

static Res rootWalkGrey(Root root, void *p)
{
  Trace trace = p;

  AVERT(Root, root);
  AVERT(Trace, trace);

  RootGrey(root, trace);
  return ResOK;
}


/* ArenaRootsWalk -- walks all the root in the arena */

static Bool rootsWalkWhiteVisit(Seg seg, void *closure)
{
  Trace trace = closure;
  /* TODO: Should use segment class test here? */
  if (PoolHasAttr(SegPool(seg), AttrGC)) {
    Res res = TraceAddWhite(trace, seg);
    AVER(res == ResOK);
  }
  return TRUE;
}

static Bool rootsWalkBlackVisit(Seg seg, void *closure)
{
  Trace trace = closure;
  /* TODO: Should use segment class test here? */
  if (PoolHasAttr(SegPool(seg), AttrGC)) {
    SegSetGrey(seg, TraceSetDel(SegGrey(seg), trace));
    SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));
  }
  return TRUE;
}

static Res ArenaRootsWalk(Globals arenaGlobals, mps_roots_stepper_t f,
                          void *p, size_t s)
{
  Arena arena;
  rootsStepClosureStruct rscStruct;
  rootsStepClosure rsc = &rscStruct;
  Trace trace;
  ScanState ss;
  Rank rank;
  Res res;

  AVERT(Globals, arenaGlobals);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary client-provided closure data. */
  arena = GlobalsArena(arenaGlobals);

  /* Scan all the roots with a minimal trace.  Invoke the scanner with a */
  /* rootsStepClosure, which is a subclass of ScanState and contains the */
  /* client-provided closure.  Supply a special fix method in order to */
  /* call the client closure.  This fix method must perform no tracing */
  /* operations of its own. */

  res = TraceCreate(&trace, arena, TraceStartWhyWALK);
  /* Have to fail if no trace available.  Unlikely due to .assume.parked. */
  if (res != ResOK)
    return res;

  /* ArenaRootsWalk only passes references to GCable pools to the client. */
  /* NOTE: I'm not sure why this is. RB 2012-07-24 */
  SegTraverse(arena, rootsWalkWhiteVisit, trace);

  /* Make the roots grey so that they are scanned */
  res = RootsIterate(arenaGlobals, rootWalkGrey, trace);
  /* Make this trace look like any other trace. */
  arena->flippedTraces = TraceSetAdd(arena->flippedTraces, trace);

  rootsStepClosureInit(rsc, arenaGlobals, trace, RootsWalkFix, f, p, s);
  ss = rootsStepClosure2ScanState(rsc);

  for(rank = RankMIN; rank < RankLIMIT; ++rank) {
    ss->rank = rank;
    AVERT(ScanState, ss);
    res = RootsIterate(arenaGlobals, rootWalk, (void *)ss);
    if (res != ResOK)
      break;
  }

  /* Turn segments black again. */
  SegTraverse(arena, rootsWalkBlackVisit, trace);

  rootsStepClosureFinish(rsc);
  /* Make this trace look like any other finished trace. */
  trace->state = TraceFINISHED;
  TraceDestroyFinished(trace);
  AVER(!ArenaEmergency(arena)); /* There was no allocation. */

  return res;
}


/* mps_arena_roots_walk -- Client interface for walking */

void mps_arena_roots_walk(mps_arena_t mps_arena, mps_roots_stepper_t f,
                          void *p, size_t s)
{
  Arena arena = (Arena)mps_arena;
  Res res;

  ArenaEnter(arena);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures, hence can't be checked */

  AVER(ArenaGlobals(arena)->clamped);          /* .assume.parked */
  AVER(arena->busyTraces == TraceSetEMPTY);    /* .assume.parked */

  res = ArenaRootsWalk(ArenaGlobals(arena), f, p, s);
  AVER(res == ResOK);
  ArenaLeave(arena);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

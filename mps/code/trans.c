/* trans.c: TRANSFORMS IMPLEMENTATION
 *
 * $Id$
 * Copyright 2011-2023 Ravenbrook Limited.  See end of file for license.
 *
 * A transform is a special kind of garbage collection that replaces
 * references to a set of objects.  The transform is piggybacked onto
 * a garbage collection by overriding the fix method for a trace
 * (design.mps.trace.fix).  The mapping used to replace the references
 * is built up in a hash table by the client.  See
 * design.mps.transform.
 */

#include "trans.h"
#include "table.h"


#define TransformSig         ((Sig)0x51926A45) /* SIGnature TRANSform */

typedef struct mps_transform_s {
  Sig sig;                      /* <design/sig/> */
  Arena arena;                  /* owning arena */
  Table oldToNew;               /* map to apply to refs */
  Epoch epoch;                  /* epoch in which transform was created */
  Bool aborted;                 /* no longer transforming, just GCing */
} TransformStruct;


Bool TransformCheck(Transform transform)
{
  CHECKS(Transform, transform);
  CHECKU(Arena, transform->arena);
  /* .check.boot: avoid bootstrap problem in transformTableAlloc where
     transformTableFree checks the transform while the table is being
     destroyed */
  if (transform->oldToNew != NULL)
    CHECKD(Table, transform->oldToNew);
  CHECKL(BoolCheck(transform->aborted));
  CHECKL(transform->epoch <= ArenaEpoch(transform->arena));
  return TRUE;
}


/* Allocator functions for the Table oldToNew */

static void *transformTableAlloc(void *closure, size_t size)
{
  Transform transform = (Transform)closure;
  Res res;
  void *p;

  AVERT(Transform, transform);

  res = ControlAlloc(&p, transform->arena, size);
  if (res != ResOK)
    return NULL;

  return p;
}

static void transformTableFree(void *closure, void *p, size_t size)
{
  Transform transform = (Transform)closure;
  AVERT(Transform, transform);
  ControlFree(transform->arena, p, size);
}


Res TransformCreate(Transform *transformReturn, Arena arena)
{
  Transform transform;
  Res res;
  void *p;

  AVER(transformReturn != NULL);
  AVERT(Arena, arena);

  res = ControlAlloc(&p, arena, sizeof(TransformStruct));
  if (res != ResOK)
    goto failAlloc;
  transform = (Transform)p;
  
  transform->oldToNew = NULL;
  transform->arena = arena;
  transform->epoch = ArenaEpoch(arena);
  transform->aborted = FALSE;

  transform->sig = TransformSig;

  AVERT(Transform, transform);

  res = TableCreate(&transform->oldToNew,
                    0, /* no point guessing size before TransformAddOldNew */
                    transformTableAlloc,
                    transformTableFree,
                    transform,
                    0, 1); /* use invalid refs as special keys */
  if (res != ResOK)
    goto failTable;

  *transformReturn = transform;
  return ResOK;

failTable:
  ControlFree(arena, transform, sizeof(TransformStruct));
failAlloc:
  return res;
}


void TransformDestroy(Transform transform)
{
  Arena arena;
  Table oldToNew;

  AVERT(Transform, transform);

  /* TODO: Log some transform statistics. */

  /* Workaround bootstrap problem, see .check.boot */
  oldToNew = transform->oldToNew;
  transform->oldToNew = NULL;
  TableDestroy(oldToNew);

  arena = TransformArena(transform);
  transform->sig = SigInvalid;
  ControlFree(arena, transform, sizeof(TransformStruct));
}


/* TransformArena -- return transform's arena
 * 
 * Must be thread-safe as it is called outside the arena lock. See
 * <design/thread-safety/#sol.check>
 */

Arena TransformArena(Transform transform)
{
  Arena arena;
  AVER(TESTT(Transform, transform));
  arena = transform->arena;
  AVER(TESTT(Arena, arena));
  return arena;
}


Res TransformAddOldNew(Transform transform,
                       Ref old_list[],
                       Ref new_list[],
                       Count count)
{
  Res res;
  Index i;
  Count added = 0;

  AVERT(Transform, transform);
  AVER(old_list != NULL);
  AVER(new_list != NULL);
  /* count: cannot check */
  
  res = TableGrow(transform->oldToNew, count);
  if (res != ResOK)
    return res;

  for (i = 0; i < count; ++i) {
    /* NOTE: If the mutator isn't adding references while the arena is parked,
       we might need to access the client-provided lists, using ArenaRead. */
    if (old_list[i] == NULL)
      continue;  /* permitted, but no transform to do */
    if (old_list[i] == new_list[i])
      continue;  /* ignore identity-transforms */

    /* .old-white: Old refs must be in managed memory, because
       transformFix is only reached when a reference is to something
       in the condemned set.  Other referenes are eliminated by
       TraceFix, and we can't (currently) transformation of them. */
    {
      Seg seg;
      AVER(SegOfAddr(&seg, transform->arena, old_list[i]));
    }

    res = TableDefine(transform->oldToNew, (Word)old_list[i], new_list[i]);
    AVER(res != ResFAIL); /* It's a static error to add the same old twice. */
    if (res != ResOK)
      return res;
    
    ++added;
  }

  AVERT(Transform, transform);
  
  return ResOK;
}


/* TransformApply -- transform references on the heap */

static Res transformFix(Seg seg, ScanState ss, Ref *refIO)
{
  Ref ref;
  Transform transform;
  Res res;

  AVERT_CRITICAL(Seg, seg);
  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(refIO != NULL);

  transform = ss->fixClosure;
  AVERT_CRITICAL(Transform, transform);

  if (!transform->aborted) {
    void *refNew;

    ref = *refIO;

    if (TableLookup(&refNew, transform->oldToNew, (Word)ref)) {
      if (ss->rank == RankAMBIG) {
        /* We rely on the fact that ambiguous references are fixed
           first, so that no exact references have been transformed
           yet. */
        transform->aborted = TRUE;
      } else {
        /* NOTE: We could fix refNew in the table before copying it,
           since any summaries etc. collected in the scan state will still
           apply when it's copied.  That could save a few snap-outs. */
        *refIO = refNew;
      }
    }
  }

  /* Now progress to a normal GC fix. */
  /* TODO: Make a clean interface to this kind of dynamic binding. */
  ss->fix = ss->arena->emergency ? SegFixEmergency : SegFix;
  TRACE_SCAN_BEGIN(ss) {
    res = TRACE_FIX12(ss, refIO);
  } TRACE_SCAN_END(ss);
  ss->fix = transformFix;

  return res;
}


static void transformCondemn(void *closure, Word old, void *value)
{
  Seg seg = NULL; /* suppress "may be used uninitialized" from GCC 11.3.0 */
  GenDesc gen;
  Bool b;
  Trace trace = closure;

  AVERT(Trace, trace);
  UNUSED(value);

  /* Find segment containing old address. */
  b = SegOfAddr(&seg, trace->arena, (Ref)old);
  AVER(b); /* old refs must be in managed memory, else client param error */

  /* Condemn generation containing seg if not already condemned. */
  gen = PoolSegPoolGen(SegPool(seg), seg)->gen;
  AVERT(GenDesc, gen);
  if (RingIsSingle(&gen->trace[trace->ti].traceRing))
    GenDescStartTrace(gen, trace);
}


Res TransformApply(Bool *appliedReturn, Transform transform)
{
  Res res;
  Arena arena;
  Globals globals;
  Trace trace;
  double mortality;

  AVER(appliedReturn != NULL);
  AVERT(Transform, transform);

  arena = TransformArena(transform);

  /* If there have been any flips since the transform was created, the old
     and new pointers will be invalid, since they are not scanned as roots.
     The client program must park the arena before applying the transform. */
  if (transform->epoch != ArenaEpoch(arena))
    return ResPARAM;

  globals = ArenaGlobals(arena);
  AVERT(Globals, globals);

  /* .park: Parking the arena ensures that there is a trace available
     and that no other traces are running, so that the tracer will
     dispatch to transformFix correctly.  See
     impl.c.trace.fix.single. */
  ArenaPark(globals);
  
  res = TraceCreate(&trace, arena, TraceStartWhyEXTENSION);
  AVER(res == ResOK); /* parking should make a trace available */
  if (res != ResOK)
    return res;

  /* Condemn the generations containing the transform's old objects,
     so that all references to them are scanned. */
  TraceCondemnStart(trace);
  TableMap(transform->oldToNew, transformCondemn, trace);
  res = TraceCondemnEnd(&mortality, trace);
  if (res != ResOK) {
    /* Nothing to transform. */
    TraceDestroyInit(trace);
    goto done;
  }

  trace->fix = transformFix;
  trace->fixClosure = transform;

  res = TraceStart(trace, 1.0, 0.0);
  AVER(res == ResOK); /* transformFix can't fail */
  
  /* If transformFix during traceFlip found ambiguous references and
     aborted the transform then the rest of the trace is just a normal GC. 
     Note that aborting a trace part-way through is pretty much impossible
     without corrupting the mutator graph.  We could safely
         if (transform->aborted) {
           trace->fix = PoolFix;
           trace->fixClosure = NULL;
         }
   */
  
  /* Force the trace to complete now. */
  ArenaPark(globals);

done:
  *appliedReturn = !transform->aborted;
  
  return ResOK;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2011-2023 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

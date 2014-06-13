/* pool.c: POOL IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2001 Global Graphics Software.
 *
 * DESIGN
 *
 * .design: See <design/class-interface/> and <design/pool/>.
 *
 * PURPOSE
 *
 * .purpose: This is the implementation of the generic pool interface.
 * There are three sorts of functions provided:
 * .purpose.support: Support functions for manipulating and accessing
 * Pool and PoolClass objects (create, destroy, check, various
 * accessors, and other miscellaneous functions).
 * .purpose.dispatch: Dispatch functions that implement the generic
 * function dispatch mechanism for Pool Classes (PoolAlloc, PoolFix,
 * etc.).
 *
 * SOURCES
 *
 * .source: See .design also.  PoolStruct and PoolClassStruct, the
 * central types for this module, are defined in <code/mpmst.h>, the
 * corresponding abstract types in <code/mpmtypes.h>.  Declarations and
 * prototypes are in <code/mpm.h>.  Several functions have macro versions
 * defined in <code/mpm.h>.
 */

#include "mpm.h"

SRCID(pool, "$Id$");


/* PoolClassCheck -- check a pool class */

Bool PoolClassCheck(PoolClass class)
{
  CHECKD(ProtocolClass, &class->protocol);
  CHECKL(class->name != NULL); /* Should be <=6 char C identifier */
  CHECKL(class->size >= sizeof(PoolStruct));
  /* Offset of generic Pool within class-specific instance cannot be */
  /* greater than the size of the class-specific portion of the instance */
  CHECKL(class->offset <= (size_t)(class->size - sizeof(PoolStruct)));
  CHECKL(AttrCheck(class->attr));
  CHECKL(!(class->attr & AttrMOVINGGC) || (class->attr & AttrGC));
  CHECKL(FUNCHECK(class->varargs));
  CHECKL(FUNCHECK(class->init));
  CHECKL(FUNCHECK(class->finish));
  CHECKL(FUNCHECK(class->alloc));
  CHECKL(FUNCHECK(class->free));
  CHECKL(FUNCHECK(class->bufferFill));
  CHECKL(FUNCHECK(class->bufferEmpty));
  CHECKL(FUNCHECK(class->access));
  CHECKL(FUNCHECK(class->whiten));
  CHECKL(FUNCHECK(class->grey));
  CHECKL(FUNCHECK(class->blacken));
  CHECKL(FUNCHECK(class->scan));
  CHECKL(FUNCHECK(class->fix));
  CHECKL(FUNCHECK(class->fixEmergency));
  CHECKL(FUNCHECK(class->reclaim));
  CHECKL(FUNCHECK(class->traceEnd));
  CHECKL(FUNCHECK(class->rampBegin));
  CHECKL(FUNCHECK(class->rampEnd));
  CHECKL(FUNCHECK(class->framePush));
  CHECKL(FUNCHECK(class->framePop));
  CHECKL(FUNCHECK(class->framePopPending));
  CHECKL(FUNCHECK(class->addrObject));
  CHECKL(FUNCHECK(class->walk));
  CHECKL(FUNCHECK(class->freewalk));
  CHECKL(FUNCHECK(class->bufferClass));
  CHECKL(FUNCHECK(class->describe));
  CHECKL(FUNCHECK(class->debugMixin));
  CHECKL(FUNCHECK(class->totalSize));
  CHECKL(FUNCHECK(class->freeSize));
  CHECKS(PoolClass, class);
  return TRUE;
}


/* PoolCheck -- check the generic part of a pool */

Bool PoolCheck(Pool pool)
{
  /* Checks ordered as per struct decl in <code/mpmst.h#pool> */
  CHECKS(Pool, pool);
  /* Break modularity for checking efficiency */
  CHECKL(pool->serial < ArenaGlobals(pool->arena)->poolSerial);
  CHECKD(PoolClass, pool->class);
  CHECKU(Arena, pool->arena);
  CHECKD_NOSIG(Ring, &pool->arenaRing);
  CHECKD_NOSIG(Ring, &pool->bufferRing);
  /* Cannot check pool->bufferSerial */
  CHECKD_NOSIG(Ring, &pool->segRing);
  CHECKL(AlignCheck(pool->alignment));
  /* normally pool->format iff PoolHasAttr(pool, AttrFMT), but during
   * pool initialization pool->format may not yet be set. */
  CHECKL(pool->format == NULL || PoolHasAttr(pool, AttrFMT));
  CHECKL(pool->fillMutatorSize >= 0.0);
  CHECKL(pool->emptyMutatorSize >= 0.0);
  CHECKL(pool->fillInternalSize >= 0.0);
  CHECKL(pool->emptyInternalSize >= 0.0);
  return TRUE;
}


/* Common keywords to PoolInit */

ARG_DEFINE_KEY(format, Format);
ARG_DEFINE_KEY(chain, Chain);
ARG_DEFINE_KEY(gen, Cant);
ARG_DEFINE_KEY(rank, Rank);
ARG_DEFINE_KEY(extend_by, Size);
ARG_DEFINE_KEY(min_size, Size);
ARG_DEFINE_KEY(mean_size, Size);
ARG_DEFINE_KEY(max_size, Size);
ARG_DEFINE_KEY(align, Align);
ARG_DEFINE_KEY(spare, double);
ARG_DEFINE_KEY(interior, Bool);


/* PoolInit -- initialize a pool
 *
 * Initialize the generic fields of the pool and calls class-specific
 * init.  See <design/pool/#align>.  */

Res PoolInit(Pool pool, Arena arena, PoolClass class, ArgList args)
{
  Res res;
  Word classId;
  Globals globals;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(PoolClass, class);
  globals = ArenaGlobals(arena);

  pool->class = class;
  /* label the pool class with its name */
  if (!class->labelled) {
    /* We could still get multiple labelling if multiple instances of */
    /* the pool class get created simultaneously, but it's not worth */
    /* putting another lock in the code. */
    class->labelled = TRUE;
    classId = EventInternString(class->name);
    /* NOTE: this breaks <design/type/#addr.use> */
    EventLabelAddr((Addr)class, classId);
  }

  pool->arena = arena;
  RingInit(&pool->arenaRing);
  RingInit(&pool->bufferRing);
  RingInit(&pool->segRing);
  pool->bufferSerial = (Serial)0;
  pool->alignment = MPS_PF_ALIGN;
  pool->format = NULL;
  pool->fix = class->fix;
  pool->fillMutatorSize = 0.0;
  pool->emptyMutatorSize = 0.0;
  pool->fillInternalSize = 0.0;
  pool->emptyInternalSize = 0.0;

  /* Initialise signature last; see <design/sig/> */
  pool->sig = PoolSig;
  pool->serial = globals->poolSerial;
  ++(globals->poolSerial);

  AVERT(Pool, pool);

  /* Do class-specific initialization. */
  res = (*class->init)(pool, args);
  if (res != ResOK)
    goto failInit;

  /* Add initialized pool to list of pools in arena. */
  RingAppend(&globals->poolRing, &pool->arenaRing);

  return ResOK;

failInit:
  pool->sig = SigInvalid;      /* Leave arena->poolSerial incremented */
  RingFinish(&pool->segRing);
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->arenaRing);
  return res;
}


/* PoolCreate: Allocate and initialise pool */

Res PoolCreate(Pool *poolReturn, Arena arena,
               PoolClass class, ArgList args)
{
  Res res;
  Pool pool;
  void *base;

  AVER(poolReturn != NULL);
  AVERT(Arena, arena);
  AVERT(PoolClass, class);

  /* .space.alloc: Allocate the pool instance structure with the size */
  /* requested  in the pool class.  See .space.free */
  res = ControlAlloc(&base, arena, class->size,
                     /* withReservoirPermit */ FALSE);
  if (res != ResOK)
    goto failControlAlloc;

  /* base is the address of the class-specific pool structure. */
  /* We calculate the address of the generic pool structure within the */
  /* instance by using the offset information from the class. */
  pool = (Pool)PointerAdd(base, class->offset);

  /* Initialize the pool. */ 
  res = PoolInit(pool, arena, class, args);
  if (res != ResOK)
    goto failPoolInit;
 
  *poolReturn = pool; 
  return ResOK;

failPoolInit:
  ControlFree(arena, base, class->size);
failControlAlloc:
  return res;
}


/* PoolFinish -- Finish pool including class-specific and generic fields. */

void PoolFinish(Pool pool)
{
  AVERT(Pool, pool); 
 
  /* Do any class-specific finishing. */
  (*pool->class->finish)(pool);

  /* Detach the pool from the arena, and unsig it. */
  RingRemove(&pool->arenaRing);
  pool->sig = SigInvalid;
 
  RingFinish(&pool->segRing);
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->arenaRing);
 
  EVENT1(PoolFinish, pool);
}


/* PoolDestroy -- Finish and free pool. */

void PoolDestroy(Pool pool)
{
  PoolClass class;
  Arena arena;
  Addr base;

  AVERT(Pool, pool); 
 
  class = pool->class; /* } In case PoolFinish changes these */
  arena = pool->arena; /* } */

  /* Finish the pool instance structure. */
  PoolFinish(pool);

  /* .space.free: Free the pool instance structure.  See .space.alloc */
  base = AddrSub((Addr)pool, (Size)(class->offset));
  ControlFree(arena, base, (Size)(class->size));
}


/* PoolDefaultBufferClass -- return the buffer class used by the pool */

BufferClass PoolDefaultBufferClass(Pool pool)
{
  AVERT(Pool, pool);
  return (*pool->class->bufferClass)();
}


/* PoolAlloc -- allocate a block of memory from a pool */

Res PoolAlloc(Addr *pReturn, Pool pool, Size size,
              Bool withReservoirPermit)
{
  Res res;

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(Bool, withReservoirPermit);

  res = (*pool->class->alloc)(pReturn, pool, size, withReservoirPermit);
  if (res != ResOK)
    return res;
  /* Make sure that the allocated address was in the pool's memory. */
  /* .hasaddr.critical: The PoolHasAddr check is expensive, and in */
  /* allocation-bound programs this is on the critical path. */
  AVER_CRITICAL(PoolHasAddr(pool, *pReturn));
  /* All allocations should be aligned to the pool's alignment */
  AVER_CRITICAL(AddrIsAligned(*pReturn, pool->alignment));

  /* All PoolAllocs should advance the allocation clock, so we count */
  /* it all in the fillMutatorSize field. */
  pool->fillMutatorSize += size;
  ArenaGlobals(PoolArena(pool))->fillMutatorSize += size;

  EVENT3(PoolAlloc, pool, *pReturn, size);

  return ResOK;
}


/* PoolFree -- deallocate a block of memory allocated from the pool */

void PoolFree(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER(old != NULL);
  /* The pool methods should check that old is in pool. */
  AVER(size > 0);
  AVER(PoolHasRange(pool, old, AddrAdd(old, size)));

  (*pool->class->free)(pool, old, size);
 
  EVENT3(PoolFree, pool, old, size);
}


Res PoolAccess(Pool pool, Seg seg, Addr addr,
               AccessSet mode, MutatorFaultContext context)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  /* Can't check mode as there is no check method */
  /* Can't check MutatorFaultContext as there is no check method */

  return (*pool->class->access)(pool, seg, addr, mode, context);
}


/* PoolWhiten, PoolGrey, PoolBlacken -- change color of a segment in the pool */

Res PoolWhiten(Pool pool, Trace trace, Seg seg)
{ 
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(PoolArena(pool) == trace->arena);
  AVER(SegPool(seg) == pool);
  return (*pool->class->whiten)(pool, trace, seg);
}

void PoolGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(pool->arena == trace->arena);
  AVER(SegPool(seg) == pool);
  (*pool->class->grey)(pool, trace, seg);
}

void PoolBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);
  AVER(SegPool(seg) == pool);
  (*pool->class->blacken)(pool, traceSet, seg);
}


/* PoolScan -- scan a segment in the pool */

Res PoolScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(ss->arena == pool->arena);

  /* The segment must belong to the pool. */
  AVER(pool == SegPool(seg));

  /* We check that either ss->rank is in the segment's
   * ranks, or that ss->rank is exact.  The check is more complicated if
   * we actually have multiple ranks in a seg.
   * See <code/trace.c#scan.conservative> */
  AVER(ss->rank == RankEXACT || RankSetIsMember(SegRankSet(seg), ss->rank));

  /* Should only scan segments which contain grey objects. */
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);

  return (*pool->class->scan)(totalReturn, ss, pool, seg);
}


/* PoolFix* -- fix a reference to an object in this pool
 *
 * See <code/mpm.h> for macro version; see <design/pool/#req.fix>.
 */

Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO)
{
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(pool == SegPool(seg));
  AVER_CRITICAL(refIO != NULL);

  /* Should only be fixing references to white segments. */
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);

  return PoolFix(pool, ss, seg, refIO);
}

Res PoolFixEmergency(Pool pool, ScanState ss, Seg seg, Addr *refIO)
{
  Res res;

  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(pool == SegPool(seg));
  AVER_CRITICAL(refIO != NULL);

  /* Should only be fixing references to white segments. */
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);

  res = (pool->class->fixEmergency)(pool, ss, seg, refIO);
  AVER_CRITICAL(res == ResOK);
  return res;
}


/* PoolReclaim -- reclaim a segment in the pool */

void PoolReclaim(Pool pool, Trace trace, Seg seg)
{
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(Trace, trace);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(pool->arena == trace->arena);
  AVER_CRITICAL(SegPool(seg) == pool);

  /* There shouldn't be any grey things left for this trace. */
  AVER_CRITICAL(!TraceSetIsMember(SegGrey(seg), trace));
  /* Should only be reclaiming segments which are still white. */
  AVER_CRITICAL(TraceSetIsMember(SegWhite(seg), trace));

  (*pool->class->reclaim)(pool, trace, seg);
}


/* PoolTraceEnd -- do end-of-trace work
 *
 * This method is for a pool class to do final end-of-trace work, 
 * after all reclaiming is complete.  For example, emitting 
 * diagnostics about what happened during the trace.
 */

void PoolTraceEnd(Pool pool, Trace trace)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVER(pool->arena == trace->arena);

  (*pool->class->traceEnd)(pool, trace);
}


/* PoolAddrObject -- find client pointer to object containing addr
 * See user documentation for mps_addr_object.
 * addr is known to belong to seg, which belongs to pool.
 * See job003589.
 */

Res PoolAddrObject(Addr *pReturn, Pool pool, Seg seg, Addr addr)
{
  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(pool == SegPool(seg));
  AVER(SegBase(seg) <= addr && addr < SegLimit(seg));
  return (*pool->class->addrObject)(pReturn, pool, seg, addr);
}


/* PoolWalk -- walk objects in this segment */

void PoolWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
              void *p, size_t s)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary values, hence can't be checked. */

  (*pool->class->walk)(pool, seg, f, p, s);
}


/* PoolFreeWalk -- walk free blocks in this pool
 *
 * PoolFreeWalk is not required to find all free blocks.
 */

void PoolFreeWalk(Pool pool, FreeBlockStepMethod f, void *p)
{
  AVERT(Pool, pool);
  AVER(FUNCHECK(f));
  /* p is arbitrary, hence can't be checked. */

  (*pool->class->freewalk)(pool, f, p);
}


/* PoolTotalSize -- return total memory allocated from arena */

Size PoolTotalSize(Pool pool)
{
  AVERT(Pool, pool);

  return (*pool->class->totalSize)(pool);
}


/* PoolFreeSize -- return free memory (unused by client program) */

Size PoolFreeSize(Pool pool)
{
  AVERT(Pool, pool);

  return (*pool->class->freeSize)(pool);
}


/* PoolDescribe -- describe a pool */

Res PoolDescribe(Pool pool, mps_lib_FILE *stream, Count depth)
{
  Res res;
  Ring node, nextNode;

  if (!TESTT(Pool, pool)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
 
  res = WriteF(stream, depth,
               "Pool $P ($U) {\n", (WriteFP)pool, (WriteFU)pool->serial,
               "  class $P (\"$S\")\n",
               (WriteFP)pool->class, pool->class->name,
               "  arena $P ($U)\n",
               (WriteFP)pool->arena, (WriteFU)pool->arena->serial,
               "  alignment $W\n", (WriteFW)pool->alignment,
               NULL);
  if (res != ResOK) return res;
  if (NULL != pool->format) {
    res = FormatDescribe(pool->format, stream, depth + 2);
    if (res != ResOK) return res;
  }
  res = WriteF(stream, depth + 2,
               "fillMutatorSize $UKb\n",
               (WriteFU)(pool->fillMutatorSize / 1024),
               "emptyMutatorSize $UKb\n",
               (WriteFU)(pool->emptyMutatorSize / 1024),
               "fillInternalSize $UKb\n",
               (WriteFU)(pool->fillInternalSize / 1024),
               "emptyInternalSize $UKb\n",
               (WriteFU)(pool->emptyInternalSize / 1024),
               NULL);
  if (res != ResOK) return res;

  res = (*pool->class->describe)(pool, stream, depth + 2);
  if (res != ResOK) return res;

  RING_FOR(node, &pool->bufferRing, nextNode) {
    Buffer buffer = RING_ELT(Buffer, poolRing, node);
    res = BufferDescribe(buffer, stream, depth + 2);
    if (res != ResOK) return res;
  }

  res = WriteF(stream, depth,
               "} Pool $P ($U)\n", (WriteFP)pool, (WriteFU)pool->serial,
               NULL);
  if (res != ResOK) return res;

  return ResOK;
}


/* PoolFormat
 *
 * Returns the format of the pool (the format of objects in the pool).
 * If the pool is unformatted or doesn't declare a format then this
 * function returns FALSE and does not update *formatReturn.  Otherwise
 * this function returns TRUE and *formatReturn is updated to be the
 * pool's format.  */

Bool PoolFormat(Format *formatReturn, Pool pool)
{
  AVER(formatReturn != NULL);
  AVERT(Pool, pool);

  if (pool->format) {
    *formatReturn = pool->format;
    return TRUE;
  }
  return FALSE;
}


/* PoolOfAddr -- return the pool containing the given address
 *
 * If the address points to a tract assigned to a pool, this returns TRUE
 * and sets *poolReturn to that pool.  Otherwise, it returns FALSE, and
 * *poolReturn is unchanged.
 */

Bool PoolOfAddr(Pool *poolReturn, Arena arena, Addr addr)
{
  Tract tract;

  AVER(poolReturn != NULL);
  AVERT(Arena, arena);

  if (TractOfAddr(&tract, arena, addr)) {
    *poolReturn = TractPool(tract);
    return TRUE;
  }

  return FALSE;
}


/* PoolOfRange -- return the pool containing a given range
 *
 * If all addresses in the range [base, limit) are owned by a single
 * pool, update *poolReturn to that pool and return TRUE. Otherwise,
 * leave *poolReturn unchanged and return FALSE.
 */
Bool PoolOfRange(Pool *poolReturn, Arena arena, Addr base, Addr limit)
{
  Bool havePool = FALSE;
  Pool pool = NULL;
  Tract tract;
  Addr addr, alignedBase, alignedLimit;

  AVER(poolReturn != NULL);
  AVERT(Arena, arena);
  AVER(base < limit);

  alignedBase = AddrArenaGrainDown(base, arena);
  alignedLimit = AddrArenaGrainUp(limit, arena);

  TRACT_FOR(tract, addr, arena, alignedBase, alignedLimit) {
    Pool p = TractPool(tract);
    if (havePool && pool != p)
      return FALSE;
    pool = p;
    havePool = TRUE;
  }

  if (havePool) {
    *poolReturn = pool;
    return TRUE;
  } else {
    return FALSE;
  }
}


Bool PoolHasAddr(Pool pool, Addr addr)
{
  Pool addrPool;
  Arena arena;
  Bool managed;

  AVERT(Pool, pool);

  arena = PoolArena(pool);
  managed = PoolOfAddr(&addrPool, arena, addr);
  return (managed && addrPool == pool);
}


Bool PoolHasRange(Pool pool, Addr base, Addr limit)
{
  Pool rangePool;
  Arena arena;
  Bool managed;

  AVERT(Pool, pool);
  AVER(base < limit);

  arena = PoolArena(pool);
  managed = PoolOfRange(&rangePool, arena, base, limit);
  return (managed && rangePool == pool);
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

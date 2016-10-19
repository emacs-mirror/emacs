/* pool.c: POOL IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
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

Bool PoolClassCheck(PoolClass klass)
{
  CHECKD(InstClass, &klass->instClassStruct);
  CHECKL(klass->size >= sizeof(PoolStruct));
  CHECKL(AttrCheck(klass->attr));
  CHECKL(!(klass->attr & AttrMOVINGGC) || (klass->attr & AttrGC));
  CHECKL(FUNCHECK(klass->varargs));
  CHECKL(FUNCHECK(klass->init));
  CHECKL(FUNCHECK(klass->alloc));
  CHECKL(FUNCHECK(klass->free));
  CHECKL(FUNCHECK(klass->bufferFill));
  CHECKL(FUNCHECK(klass->bufferEmpty));
  CHECKL(FUNCHECK(klass->access));
  CHECKL(FUNCHECK(klass->whiten));
  CHECKL(FUNCHECK(klass->grey));
  CHECKL(FUNCHECK(klass->blacken));
  CHECKL(FUNCHECK(klass->scan));
  CHECKL(FUNCHECK(klass->fix));
  CHECKL(FUNCHECK(klass->fixEmergency));
  CHECKL(FUNCHECK(klass->reclaim));
  CHECKL(FUNCHECK(klass->rampBegin));
  CHECKL(FUNCHECK(klass->rampEnd));
  CHECKL(FUNCHECK(klass->framePush));
  CHECKL(FUNCHECK(klass->framePop));
  CHECKL(FUNCHECK(klass->walk));
  CHECKL(FUNCHECK(klass->freewalk));
  CHECKL(FUNCHECK(klass->bufferClass));
  CHECKL(FUNCHECK(klass->debugMixin));
  CHECKL(FUNCHECK(klass->totalSize));
  CHECKL(FUNCHECK(klass->freeSize));

  /* Check that pool classes overide sets of related methods. */
  CHECKL((klass->init == PoolAbsInit) ==
         (klass->instClassStruct.finish == PoolAbsFinish));
  CHECKL((klass->bufferFill == PoolNoBufferFill) ==
         (klass->bufferEmpty == PoolNoBufferEmpty));
  CHECKL((klass->framePush == PoolNoFramePush) ==
         (klass->framePop == PoolNoFramePop));
  CHECKL((klass->rampBegin == PoolNoRampBegin) ==
         (klass->rampEnd == PoolNoRampEnd));

  /* Check that pool classes that set attributes also override the
     methods they imply. */
  CHECKL(((klass->attr & AttrFMT) == 0) == (klass->walk == PoolNoWalk));
  if (klass != &CLASS_STATIC(AbstractCollectPool)) {
    CHECKL(((klass->attr & AttrGC) == 0) == (klass->fix == PoolNoFix));
    CHECKL(((klass->attr & AttrGC) == 0) == (klass->fixEmergency == PoolNoFix));
    CHECKL(((klass->attr & AttrGC) == 0) == (klass->reclaim == PoolNoReclaim));
  }
  
  CHECKS(PoolClass, klass);
  return TRUE;
}


/* PoolCheck -- check the generic part of a pool */

Bool PoolCheck(Pool pool)
{
  PoolClass klass;
  /* Checks ordered as per struct decl in <code/mpmst.h#pool> */
  CHECKS(Pool, pool);
  CHECKC(AbstractPool, pool);
  /* Break modularity for checking efficiency */
  CHECKL(pool->serial < ArenaGlobals(pool->arena)->poolSerial);
  klass = ClassOfPoly(Pool, pool);
  CHECKD(PoolClass, klass);
  CHECKU(Arena, pool->arena);
  CHECKD_NOSIG(Ring, &pool->arenaRing);
  CHECKD_NOSIG(Ring, &pool->bufferRing);
  /* Cannot check pool->bufferSerial */
  CHECKD_NOSIG(Ring, &pool->segRing);
  CHECKL(AlignCheck(pool->alignment));
  /* Normally pool->format iff PoolHasAttr(pool, AttrFMT), but during
     pool initialization the class may not yet be set. */
  CHECKL(!PoolHasAttr(pool, AttrFMT) || pool->format != NULL);
  return TRUE;
}


/* Common keywords to PoolInit */

ARG_DEFINE_KEY(FORMAT, Format);
ARG_DEFINE_KEY(CHAIN, Chain);
ARG_DEFINE_KEY(GEN, Cant);
ARG_DEFINE_KEY(RANK, Rank);
ARG_DEFINE_KEY(EXTEND_BY, Size);
ARG_DEFINE_KEY(LARGE_SIZE, Size);
ARG_DEFINE_KEY(MIN_SIZE, Size);
ARG_DEFINE_KEY(MEAN_SIZE, Size);
ARG_DEFINE_KEY(MAX_SIZE, Size);
ARG_DEFINE_KEY(ALIGN, Align);
ARG_DEFINE_KEY(SPARE, double);
ARG_DEFINE_KEY(INTERIOR, Bool);


/* PoolInit -- initialize a pool
 *
 * Initialize the generic fields of the pool and calls class-specific
 * init.  See <design/pool/#align>.
 */

Res PoolInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  Res res;

  AVERT(PoolClass, klass);

  res = klass->init(pool, arena, klass, args);
  if (res != ResOK)
    return res;

  EVENT3(PoolInit, pool, PoolArena(pool), ClassOfPoly(Pool, pool));

  return ResOK;
}


/* PoolCreate: Allocate and initialise pool */

Res PoolCreate(Pool *poolReturn, Arena arena,
               PoolClass klass, ArgList args)
{
  Res res;
  Pool pool;
  void *base;

  AVER(poolReturn != NULL);
  AVERT(Arena, arena);
  AVERT(PoolClass, klass);

  /* .space.alloc: Allocate the pool instance structure with the size */
  /* requested  in the pool class.  See .space.free */
  res = ControlAlloc(&base, arena, klass->size);
  if (res != ResOK)
    goto failControlAlloc;
  pool = (Pool)base;

  /* Initialize the pool. */ 
  res = PoolInit(pool, arena, klass, args);
  if (res != ResOK)
    goto failPoolInit;
 
  *poolReturn = pool; 
  return ResOK;

failPoolInit:
  ControlFree(arena, base, klass->size);
failControlAlloc:
  return res;
}


/* PoolFinish -- Finish pool including class-specific and generic fields. */

void PoolFinish(Pool pool)
{
  AVERT(Pool, pool); 
  Method(Inst, pool, finish)(MustBeA(Inst, pool));
}


/* PoolDestroy -- Finish and free pool. */

void PoolDestroy(Pool pool)
{
  Arena arena;
  Size size;

  AVERT(Pool, pool); 
  arena = pool->arena;
  size = ClassOfPoly(Pool, pool)->size;
  PoolFinish(pool);

  /* .space.free: Free the pool instance structure.  See .space.alloc */
  ControlFree(arena, pool, size);
}


/* PoolDefaultBufferClass -- return the buffer class used by the pool */

BufferClass PoolDefaultBufferClass(Pool pool)
{
  AVERT(Pool, pool);
  return Method(Pool, pool, bufferClass)();
}


/* PoolAlloc -- allocate a block of memory from a pool
 *
 * .alloc.critical: In manual-allocation-bound programs this is on the
 * critical path.
 */

Res PoolAlloc(Addr *pReturn, Pool pool, Size size)
{
  Res res;

  AVER_CRITICAL(pReturn != NULL);
  AVERT_CRITICAL(Pool, pool);
  AVER_CRITICAL(size > 0);

  res = Method(Pool, pool, alloc)(pReturn, pool, size);
  if (res != ResOK)
    return res;
  /* Make sure that the allocated address was in the pool's memory. */
  AVER_CRITICAL(PoolHasAddr(pool, *pReturn));
  /* All allocations should be aligned to the pool's alignment */
  AVER_CRITICAL(AddrIsAligned(*pReturn, pool->alignment));

  /* All PoolAllocs should advance the allocation clock, so we count */
  /* it all in the fillMutatorSize field. */
  ArenaGlobals(PoolArena(pool))->fillMutatorSize += size;

  EVENT3(PoolAlloc, pool, *pReturn, size);

  return ResOK;
}


/* PoolFree -- deallocate a block of memory allocated from the pool
 *
 * .free.critical: In manual-allocation-bound programs this is on the
 * critical path.
 */

void PoolFree(Pool pool, Addr old, Size size)
{
  AVERT_CRITICAL(Pool, pool);
  AVER_CRITICAL(old != NULL);
  /* The pool methods should check that old is in pool. */
  AVER_CRITICAL(size > 0);
  AVER_CRITICAL(AddrIsAligned(old, pool->alignment));
  AVER_CRITICAL(PoolHasRange(pool, old, AddrAdd(old, size)));

  Method(Pool, pool, free)(pool, old, size);
 
  EVENT3(PoolFree, pool, old, size);
}


Res PoolAccess(Pool pool, Seg seg, Addr addr,
               AccessSet mode, MutatorContext context)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVERT(AccessSet, mode);
  AVERT(MutatorContext, context);

  return Method(Pool, pool, access)(pool, seg, addr, mode, context);
}


/* PoolWhiten, PoolGrey, PoolBlacken -- change color of a segment in the pool */

Res PoolWhiten(Pool pool, Trace trace, Seg seg)
{ 
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(PoolArena(pool) == trace->arena);
  AVER(SegPool(seg) == pool);
  return Method(Pool, pool, whiten)(pool, trace, seg);
}

void PoolGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(pool->arena == trace->arena);
  AVER(SegPool(seg) == pool);
  Method(Pool, pool, grey)(pool, trace, seg);
}

void PoolBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);
  AVER(SegPool(seg) == pool);
  Method(Pool, pool, blacken)(pool, traceSet, seg);
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

  return Method(Pool, pool, scan)(totalReturn, ss, pool, seg);
}


/* PoolFix* -- fix a reference to an object in this pool
 *
 * See <design/pool/#req.fix>.
 */

Res PoolFix(Pool pool, ScanState ss, Seg seg, Addr *refIO)
{
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(pool == SegPool(seg));
  AVER_CRITICAL(refIO != NULL);

  /* Should only be fixing references to white segments. */
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);

  return pool->fix(pool, ss, seg, refIO);
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

  res = Method(Pool, pool, fixEmergency)(pool, ss, seg, refIO);
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

  Method(Pool, pool, reclaim)(pool, trace, seg);
}


/* PoolWalk -- walk objects in this segment */

void PoolWalk(Pool pool, Seg seg, FormattedObjectsVisitor f, void *p, size_t s)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary values, hence can't be checked. */

  Method(Pool, pool, walk)(pool, seg, f, p, s);
}


/* PoolFreeWalk -- walk free blocks in this pool
 *
 * PoolFreeWalk is not required to find all free blocks.
 */

void PoolFreeWalk(Pool pool, FreeBlockVisitor f, void *p)
{
  AVERT(Pool, pool);
  AVER(FUNCHECK(f));
  /* p is arbitrary, hence can't be checked. */

  Method(Pool, pool, freewalk)(pool, f, p);
}


/* PoolTotalSize -- return total memory allocated from arena */

Size PoolTotalSize(Pool pool)
{
  AVERT(Pool, pool);

  return Method(Pool, pool, totalSize)(pool);
}


/* PoolFreeSize -- return free memory (unused by client program) */

Size PoolFreeSize(Pool pool)
{
  AVERT(Pool, pool);

  return Method(Pool, pool, freeSize)(pool);
}


/* PoolDescribe -- describe a pool */

Res PoolDescribe(Pool pool, mps_lib_FILE *stream, Count depth)
{
  return Method(Inst, pool, describe)(MustBeA(Inst, pool), stream, depth);
}


/* PoolFormat -- get the format of a pool, if any
 *
 * Returns the format of the pool (the format of objects in the pool).
 * If the pool is unformatted or doesn't declare a format then this
 * function returns FALSE and does not update *formatReturn.  Otherwise
 * this function returns TRUE and *formatReturn is updated to be the
 * pool's format.
 */

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

  AVERT_CRITICAL(Pool, pool);
  AVER_CRITICAL(base < limit);

  arena = PoolArena(pool);
  managed = PoolOfRange(&rangePool, arena, base, limit);
  return (managed && rangePool == pool);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

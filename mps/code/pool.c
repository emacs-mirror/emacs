/* pool.c: POOL IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2001 Global Graphics Software.
 *
 * DESIGN
 *
 * .design: See <design/pool/>.
 *
 * PURPOSE
 *
 * .purpose: This is the implementation of the generic pool interface.
 * There are three sorts of functions provided:
 * .purpose.support: Support functions for manipulating and accessing
 * Pool and PoolClass objects (create, destroy, check, various
 * accessors, and other miscellaneous functions).
 * .purpose.dispatch: Dispatch functions that implement the generic
 * function dispatch mechanism for Pool Classes (PoolAlloc, PoolFree,
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
  CHECKL(FUNCHECK(klass->segPoolGen));
  CHECKL(FUNCHECK(klass->bufferFill));
  CHECKL(FUNCHECK(klass->bufferEmpty));
  CHECKL(FUNCHECK(klass->rampBegin));
  CHECKL(FUNCHECK(klass->rampEnd));
  CHECKL(FUNCHECK(klass->framePush));
  CHECKL(FUNCHECK(klass->framePop));
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
  CHECKL(ShiftCheck(pool->alignShift));
  CHECKL(pool->alignment == PoolGrainsSize(pool, (Align)1));
  if (pool->format != NULL)
    CHECKD(Format, pool->format);
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

  EVENT4(PoolInit, pool, PoolArena(pool), ClassOfPoly(Pool, pool),
         pool->serial);

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
 * critical path via mps_alloc.
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


/* PoolFree -- deallocate a block of memory allocated from the pool */

void (PoolFree)(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER(old != NULL);
  /* The pool methods should check that old is in pool. */
  AVER(size > 0);
  AVER(AddrIsAligned(old, pool->alignment));
  AVER(PoolHasRange(pool, old, AddrAdd(old, size)));

  PoolFreeMacro(pool, old, size);
 
  EVENT3(PoolFree, pool, old, size);
}


/* PoolSegPoolGen -- get pool generation for a segment */

PoolGen PoolSegPoolGen(Pool pool, Seg seg)
{ 
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(pool == SegPool(seg));
  return Method(Pool, pool, segPoolGen)(pool, seg);
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

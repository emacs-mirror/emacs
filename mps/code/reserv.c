/* reserv.c: ARENA RESERVOIR
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * IMPROVEMENTS
 *
 * .improve.contiguous: There should be a means of grouping contiguous
 * tracts together so that there's a likelihood of being able to meet
 * requests for regions larger than the arena alignment.  */

#include "mpm.h"

SRCID(reserv, "$Id$");


/* The reservoir pool is defined here. See <design/reservoir/> */

#define PoolReservoir(pool) PARENT(ReservoirStruct, poolStruct, pool)


/* Management of tracts
 *
 * The reservoir maintains a linked list of tracts in arbitrary order.
 * (see .improve.contiguous)
 *
 * Tracts are chained using the TractP field.  */

#define resTractNext(tract) ((Tract)TractP((tract)))
#define resTractSetNext(tract, next) (TractSetP((tract), (void*)(next)))


#define reservoirArena(reservoir) (PoolArena(ReservoirPool(reservoir)))


/* ResPoolInit -- Reservoir pool init method */

static Res ResPoolInit(Pool pool, ArgList arg)
{
  AVER(pool != NULL);

  UNUSED(arg);
  /* Caller will set sig and AVERT. */
  EVENT3(PoolInit, pool, PoolArena(pool), ClassOfPool(pool));
  return ResOK;
}


/* ResPoolFinish -- Reservoir pool finish method
 *
 * .reservoir.finish: This might be called from ArenaFinish, so the
 * arena cannot be checked at this time.  In order to avoid the check,
 * insist that the reservoir is empty, by AVERing that the reserve list
 * is NULL.  */

static void ResPoolFinish(Pool pool)
{
  Reservoir reservoir;

  AVERT(Pool, pool);
  reservoir = PoolReservoir(pool);
  AVERT(Reservoir, reservoir);
  AVER(reservoir->reserve == NULL);  /* .reservoir.finish */
}


/* ReservoirPoolClass -- Class definition */

DEFINE_POOL_CLASS(ReservoirPoolClass, this)
{
  INHERIT_CLASS(this, AbstractPoolClass);
  this->name = "Reservoir";
  this->size = sizeof(ReservoirStruct);
  this->offset = offsetof(ReservoirStruct, poolStruct);
  this->init = ResPoolInit;
  this->finish = ResPoolFinish;
  AVERT(PoolClass, this);
}


/* ReservoirCheck -- Reservoir check method */

Bool ReservoirCheck(Reservoir reservoir)
{
  ReservoirPoolClass reservoircl = EnsureReservoirPoolClass();
  Arena arena;
  Tract tract;

  CHECKS(Reservoir, reservoir);
  CHECKD(Pool, ReservoirPool(reservoir));
  CHECKL(ReservoirPool(reservoir)->class == reservoircl);
  UNUSED(reservoircl); /* <code/mpm.c#check.unused> */
  arena = reservoirArena(reservoir);
  CHECKU(Arena, arena);
  /* could call ReservoirIsConsistent, but it's costly. */
  tract = reservoir->reserve;
  if (tract != NULL) {
    CHECKD_NOSIG(Tract, tract);
    CHECKL(TractPool(tract) == ReservoirPool(reservoir));
  }
  CHECKL(SizeIsAligned(reservoir->reservoirLimit, ArenaAlign(arena)));
  CHECKL(SizeIsAligned(reservoir->reservoirSize, ArenaAlign(arena)));

  return TRUE;
}


/* reservoirIsConsistent -- returns FALSE if the reservoir is corrupt */

ATTRIBUTE_UNUSED
static Bool reservoirIsConsistent(Reservoir reservoir)
{
  Size alignment, size = 0;
  Tract tract;
  Pool pool;
  Arena arena;

  arena = reservoirArena(reservoir);
  pool = ReservoirPool(reservoir);

  /* Check that the size of the tracts matches reservoirSize */
  alignment = ArenaAlign(arena);
  tract = reservoir->reserve;
  while (tract != NULL) {
    AVERT(Tract, tract);
    AVER(TractPool(tract) == pool);
    tract = resTractNext(tract);
    size += alignment;
  }

  if (size != reservoir->reservoirSize)
    return FALSE;

  /* <design/reservoir/#align> */
  return SizeIsAligned(reservoir->reservoirLimit, alignment)
         && SizeIsAligned(reservoir->reservoirSize, alignment)
         && (reservoir->reservoirLimit >= reservoir->reservoirSize);
}


/* ReservoirEnsureFull 
 *
 * Ensures that the reservoir is the right size, by topping it up with
 * fresh memory from the arena if possible.  */

Res ReservoirEnsureFull(Reservoir reservoir)
{
  Size limit, size;
  Pool pool;
  Arena arena;
  AVERT(Reservoir, reservoir);
  arena = reservoirArena(reservoir);

  AVERT(Arena, arena);
  size = ArenaAlign(arena);
  limit = reservoir->reservoirLimit;

  /* optimize the common case of a full reservoir */
  if (reservoir->reservoirSize == limit)
    return ResOK;

  pool = ReservoirPool(reservoir);

  /* really ought to try hard to allocate contiguous tracts */
  /* see .improve.contiguous */
  while (reservoir->reservoirSize < limit) {
    Res res;
    Addr base;
    Tract tract;
    res = ArenaAlloc(&base, SegPrefDefault(), size, pool, FALSE);
    if (res != ResOK) {
      AVER(reservoirIsConsistent(reservoir));
      return res;
    }
    tract = TractOfBaseAddr(arena, base);
    reservoir->reservoirSize += size;
    resTractSetNext(tract, reservoir->reserve);
    reservoir->reserve = tract;
  }
  AVER(reservoirIsConsistent(reservoir));
  return ResOK;
}


/* reservoirShrink -- Reduce the size of the reservoir */

static void reservoirShrink(Reservoir reservoir, Size want)
{
  Arena arena;
  Pool pool;
  Size size;

  pool = ReservoirPool(reservoir);
  arena = reservoirArena(reservoir);
  AVER(SizeIsAligned(want, ArenaAlign(arena)));
  AVER(reservoir->reservoirSize >= want);

  if (reservoir->reservoirSize == want)
    return;

  /* Iterate over tracts, freeing them while reservoir is too big */
  size = ArenaAlign(arena);
  while (reservoir->reservoirSize > want) {
    Tract tract = reservoir->reserve;
    AVER(tract != NULL);
    reservoir->reserve = resTractNext(tract);
    ArenaFree(TractBase(tract), size, pool);
    reservoir->reservoirSize -= size;
  }
  AVER(reservoir->reservoirSize == want);
  AVER(reservoirIsConsistent(reservoir));
}


/* ReservoirWithdraw -- Attempt to supply memory from the reservoir */

Res ReservoirWithdraw(Addr *baseReturn, Tract *baseTractReturn,
                      Reservoir reservoir, Size size, Pool pool)
{
  Arena arena;
 
  AVER(baseReturn != NULL);
  AVER(baseTractReturn != NULL);
  AVERT(Reservoir, reservoir);
  arena = reservoirArena(reservoir);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(size, ArenaAlign(arena)));
  AVER(size > 0);
  AVERT(Pool, pool);

  /* @@@@ As a short-term measure, we only permit the reservoir to */
  /* allocate single-page regions. */
  /* See .improve.contiguous &  change.dylan.jackdaw.160125 */
  if (size != ArenaAlign(arena))
    return ResMEMORY;
 
  if (size <= reservoir->reservoirSize) {
    /* Return the first tract  */
    Tract tract = reservoir->reserve;
    Addr base;
    AVER(tract != NULL);
    base  = TractBase(tract);
    reservoir->reserve = resTractNext(tract);
    reservoir->reservoirSize -= ArenaAlign(arena);
    TractFinish(tract);
    TractInit(tract, pool, base);
    AVER(reservoirIsConsistent(reservoir));
    *baseReturn = base;
    *baseTractReturn = tract;
    return ResOK;
  }

  AVER(reservoirIsConsistent(reservoir)); 
  return ResMEMORY; /* no suitable region in the reservoir */
}


/* ReservoirDeposit -- Top up the reservoir */

Bool ReservoirDeposit(Reservoir reservoir, Addr *baseIO, Size *sizeIO)
{
  Pool respool;
  Addr addr, limit;
  Size reslimit, alignment;
  Arena arena;
  Tract tract;
  Addr base;
  Size size;

  AVERT(Reservoir, reservoir);
  arena = reservoirArena(reservoir);
  AVERT(Arena, arena);
  respool = ReservoirPool(reservoir);
  alignment = ArenaAlign(arena);
  AVER(baseIO != NULL);
  AVER(sizeIO != NULL);
  base = *baseIO;
  size = *sizeIO;
  AVER(AddrIsAligned(base, alignment));
  AVER(SizeIsAligned(size, alignment));
  limit = AddrAdd(base, size);
  reslimit = reservoir->reservoirLimit;

  /* put as many pages as necessary into the reserve & free the rest */
  TRACT_FOR(tract, addr, arena, base, limit) {
    AVERT(Tract, tract);
    if (reservoir->reservoirSize < reslimit) {
      /* Reassign the tract to the reservoir pool */
      TractFinish(tract);
      TractInit(tract, respool, addr);
      reservoir->reservoirSize += alignment;
      resTractSetNext(tract, reservoir->reserve);
      reservoir->reserve = tract;
    } else {
      *baseIO = addr;
      *sizeIO = AddrOffset(base, limit);
      AVER(reservoirIsConsistent(reservoir));
      return TRUE;
    }
  }
  AVER(addr == limit);
  AVER(reservoirIsConsistent(reservoir));
  return FALSE;
}


/* mutatorBufferCount -- returns the number of mutator buffers for the arena
 *
 * This should probably be in the pool module, but it's only used here.  */

static Count mutatorBufferCount(Globals arena)
{
  Ring nodep, nextp;
  Count count = 0;
 
  /* Iterate over all pools, and count the mutator buffers in each */
  RING_FOR(nodep, &arena->poolRing, nextp) {
    Pool pool = RING_ELT(Pool, arenaRing, nodep);
    Ring nodeb, nextb;

    AVERT(Pool, pool);
    RING_FOR(nodeb, &pool->bufferRing, nextb) {
      Buffer buff = RING_ELT(Buffer, poolRing, nodeb);
      if (buff->isMutator)
        count++;
    }
  }
  return count;
}


/* ReservoirSetLimit -- Set the reservoir limit */

void ReservoirSetLimit(Reservoir reservoir, Size size)
{
  Size needed;
  Arena arena;
  AVERT(Reservoir, reservoir);
  arena = reservoirArena(reservoir);
  AVERT(Arena, arena);

  if (size > 0) {
    Size wastage;
    /* <design/reservoir/#wastage> */
    wastage = ArenaAlign(arena) * mutatorBufferCount(ArenaGlobals(arena));
    /* <design/reservoir/#align> */
    needed = SizeAlignUp(size, ArenaAlign(arena)) + wastage;
  } else {
    needed = 0; /* <design/reservoir/#really-empty> */
  }

  AVER(SizeIsAligned(needed, ArenaAlign(arena)));
  /* Emit event now, so subsequent change can be ascribed to it. */
  EVENT2(ReservoirLimitSet, arena, size);

  if (needed > reservoir->reservoirSize) {
    /* Try to grow the reservoir */
    reservoir->reservoirLimit = needed;
    (void)ReservoirEnsureFull(reservoir);
  } else {
    /* Shrink the reservoir */
    reservoirShrink(reservoir, needed);
    reservoir->reservoirLimit = needed;
    AVER(reservoirIsConsistent(reservoir)); 
  }
}


/* ReservoirLimit -- Return the reservoir limit */

Size ReservoirLimit(Reservoir reservoir)
{
  AVERT(Reservoir, reservoir);
  AVER(reservoirIsConsistent(reservoir)); 
  return reservoir->reservoirLimit;
}


/* ReservoirAvailable -- Return the amount in the reservoir */

Size ReservoirAvailable(Reservoir reservoir)
{
  AVERT(Reservoir, reservoir);
  (void)ReservoirEnsureFull(reservoir);
  return reservoir->reservoirSize;
}


/* ReservoirInit -- Initialize a reservoir */

Res ReservoirInit(Reservoir reservoir, Arena arena)
{
  Res res;

  /* reservoir and arena are not initialized and can't be checked */
  reservoir->reservoirLimit = (Size)0;
  reservoir->reservoirSize = (Size)0;
  reservoir->reserve = NULL;
  reservoir->sig = ReservoirSig;
  /* initialize the reservoir pool, <design/reservoir/> */
  res = PoolInit(ReservoirPool(reservoir),
                 arena, EnsureReservoirPoolClass(), argsNone);
  if (res == ResOK) {
    AVERT(Reservoir, reservoir);
  }
  return res;
}


/* ReservoirFinish -- Finish a reservoir */

void ReservoirFinish (Reservoir reservoir)
{
  PoolFinish(ReservoirPool(reservoir));
  reservoir->sig = SigInvalid;
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

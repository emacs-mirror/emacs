/* impl.c.reserv: ARENA RESERVOIR
 *
 * $HopeName: MMsrc!reserv.c(trunk.3) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 *
 * IMPROVEMENTS
 *
 * .improve.contiguous: There should be a means of grouping contiguous
 * tracts together so that there's a likelihood of being able to meet
 * requests for regions larger than the arena alignment.
 */


#include "mpm.h"

SRCID(reserv, "$HopeName: MMsrc!reserv.c(trunk.3) $");


/* The reservoir pool is defined here. See design.mps.reservoir */

#define PoolPoolReservoir(pool) PARENT(ReservoirStruct, poolStruct, pool)


/* Management of tracts
 *
 * The reservoir maintains a linked list of tracts in arbitrary order.
 * (see .improve.contiguous)
 * 
 * Tracts are chained using the TractP field.
 */

#define resTractNext(tract) ((Tract)TractP((tract)))
#define resTractSetNext(tract, next) (TractSetP((tract), (void*)(next)))


#define reservoirArena(reservoir) ((reservoir)->poolStruct.arena)


/* ResPoolInit -- Reservoir pool init method */

static Res ResPoolInit(Pool pool, va_list arg)
{
  AVER(pool != NULL);
  UNUSED(arg);
  /* Caller will set sig and AVERT. */
  EVENT_PPP(PoolInit, pool, PoolArena(pool), ClassOfPool(pool));
  return ResOK;
}


/* ResPoolFinish -- Reservoir pool finish method 
 *
 * .reservoir.finish: This might be called from ArenaFinish, so the 
 * arena cannot be checked at this time.  In order to avoid the 
 * check, insist that the reservoir is empty, by AVERing that
 * the reserve list is NULL.
 */

static void ResPoolFinish(Pool pool)
{
  Reservoir reservoir;

  AVERT(Pool, pool);
  reservoir = PoolPoolReservoir(pool);
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
}


/* ReservoirCheck -- Reservoir check method */

Bool ReservoirCheck(Reservoir reservoir)
{
  ReservoirPoolClass reservoircl = EnsureReservoirPoolClass();
  Arena arena;
  Tract tract;
  CHECKS(Reservoir, reservoir);
  CHECKD(Pool, &reservoir->poolStruct);
  CHECKL(reservoir->poolStruct.class == reservoircl);
  arena = reservoirArena(reservoir);
  CHECKU(Arena, arena);
  /* could call ReservoirIsConsistent, but it's costly. */
  tract = reservoir->reserve;
  if (tract != NULL) {
    CHECKL(TractCheck(tract));
    CHECKL(TractPool(tract) == &reservoir->poolStruct);
  }
  CHECKL(SizeIsAligned(reservoir->reservoirLimit, ArenaAlign(arena)));
  CHECKL(SizeIsAligned(reservoir->reservoirSize, ArenaAlign(arena)));

  return TRUE;
}


/* ReservoirIsConsistent
 *
 * Returns FALSE if the reservoir is corrupt.
 */

static Bool ReservoirIsConsistent(Reservoir reservoir)
{
  Bool res;
  Size alignment, size = 0;
  Tract tract;
  Pool pool;
  Arena arena;
  AVERT(Reservoir, reservoir);
  arena = reservoirArena(reservoir);
  AVERT(Arena, arena);
  pool = &reservoir->poolStruct;
  AVERT(Pool, pool);

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

  /* design.mps.reservoir.align */
  res = SizeIsAligned(reservoir->reservoirLimit, alignment) &&
        SizeIsAligned(reservoir->reservoirSize, alignment) &&
        (reservoir->reservoirLimit >= reservoir->reservoirSize);

  return res;
}


/* ReservoirEnsureFull  
 * 
 * Ensures that the reservoir is the right size, by topping it up 
 * with fresh memory from the arena if possible.
 */

Res ReservoirEnsureFull(Reservoir reservoir)
{
  Size limit, alignment;
  Pool pool;
  Arena arena;
  AVERT(Reservoir, reservoir);
  arena = reservoirArena(reservoir);

  AVERT(Arena, arena);
  alignment = ArenaAlign(arena);
  limit = reservoir->reservoirLimit;

  /* optimize the common case of a full reservoir */
  if (reservoir->reservoirSize == limit)
    return ResOK; 

  pool = &reservoir->poolStruct;
  AVERT(Pool, pool);

  /* really ought to try hard to allocate contiguous tracts */
  /* see .improve.contiguous */
  while (reservoir->reservoirSize < limit) {
    Res res;
    Addr base;
    Tract tract;
    res = (*arena->class->alloc)(&base, &tract, SegPrefDefault(),
                                 alignment, pool);
    if (res != ResOK) {
      AVER(ReservoirIsConsistent(reservoir));
      return res;
    }
    reservoir->reservoirSize += alignment;
    resTractSetNext(tract, reservoir->reserve);
    reservoir->reserve = tract;
  }
  AVER(ReservoirIsConsistent(reservoir));
  return ResOK;
}


/* ReservoirShrink -- Reduce the size of the reservoir */

static void ReservoirShrink(Reservoir reservoir, Size want)
{
  Arena arena;
  Pool pool;
  Size alignment;
  AVERT(Reservoir, reservoir);
  pool = &reservoir->poolStruct;
  AVERT(Pool, pool);
  arena = reservoirArena(reservoir);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(want, ArenaAlign(arena)));
  AVER(reservoir->reservoirSize >= want);

  if (reservoir->reservoirSize == want)
    return;

  /* Iterate over tracts, freeing them while reservoir is too big */
  alignment = ArenaAlign(arena);
  while (reservoir->reservoirSize > want) {
    Tract tract = reservoir->reserve;
    AVER(tract != NULL);
    reservoir->reserve = resTractNext(tract);
    (*arena->class->free)(TractBase(tract), alignment, pool);
    reservoir->reservoirSize -= alignment;
  }
  AVER(reservoir->reservoirSize == want);
  AVER(ReservoirIsConsistent(reservoir));
}


/* ReservoirWithdraw -- Attempt to supply memory from the reservoir */

Res ReservoirWithdraw(Addr *baseReturn, Tract *baseTractReturn,
                      Reservoir reservoir, Size size, Pool pool)
{
  Pool respool;
  Arena arena;
  
  AVER(baseReturn != NULL);
  AVER(baseTractReturn != NULL);
  AVERT(Reservoir, reservoir);
  arena = reservoirArena(reservoir);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(size, ArenaAlign(arena)));
  AVER(size > 0);
  AVERT(Pool, pool);
  respool = &reservoir->poolStruct;
  AVERT(Pool, respool);

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
    AVER(ReservoirIsConsistent(reservoir));
    *baseReturn = base;
    *baseTractReturn = tract;
    return ResOK;
  }

  AVER(ReservoirIsConsistent(reservoir));  
  return ResMEMORY; /* no suitable region in the reservoir */
}


/* ReservoirDeposit -- Top up the reservoir */

void ReservoirDeposit(Reservoir reservoir, Addr base, Size size)
{
  Pool respool;
  Addr addr, limit;
  Size reslimit, alignment;
  Arena arena;
  Tract tract;
  AVERT(Reservoir, reservoir);
  arena = reservoirArena(reservoir);
  AVERT(Arena, arena);
  respool = &reservoir->poolStruct;
  AVERT(Pool, respool);
  alignment = ArenaAlign(arena);
  AVER(AddrIsAligned(base, alignment));
  AVER(SizeIsAligned(size, alignment));
  limit = AddrAdd(base, size);
  reslimit = reservoir->reservoirLimit;

  /* put as many pages as necessary into the reserve & free the rest */
  TRACT_FOR(tract, addr, arena, base, limit) {
    AVER(TractCheck(tract));
    if (reservoir->reservoirSize < reslimit) {
      /* Reassign the tract to the reservoir pool */
      TractFinish(tract);
      TractInit(tract, respool, addr);
      reservoir->reservoirSize += alignment; 
      resTractSetNext(tract, reservoir->reserve);
      reservoir->reserve = tract;
    } else {
      /* free the tract */
      (*arena->class->free)(addr, alignment, TractPool(tract));
    }
  }
  AVER(addr == limit);
  AVER(ReservoirIsConsistent(reservoir));
}


/* MutatorBufferCount
 *
 * Returns the number of mutator buffers for the arena. 
 */

static Count MutatorBufferCount(Arena arena)
{
  Ring nodep, nextp;
  Count count = 0;

  AVERT(Arena, arena);
  
  /* Iterate over all pools, and count the mutator buffers in each */
  RING_FOR(nodep, &arena->poolRing, nextp) {
    Pool pool = RING_ELT(Pool, arenaRing, nodep);
    Ring nodeb, nextb;
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
    /* design.mps.reservoir.wastage */
    wastage = ArenaAlign(arena) * MutatorBufferCount(arena);
    /* design.mps.reservoir.align */
    needed = SizeAlignUp(size, ArenaAlign(arena)) + wastage;
  } else {
    needed = 0; /* design.mps.reservoir.really-empty */
  }

  AVER(SizeIsAligned(needed, ArenaAlign(arena)));
  /* Emit event now, so subsequent change can be ascribed to it. */
  EVENT_PW(ReservoirLimitSet, arena, size);

  if (needed > reservoir->reservoirSize) {
    /* Try to grow the reservoir */
    reservoir->reservoirLimit = needed;
    ReservoirEnsureFull(reservoir);
  } else {
    /* Shrink the reservoir */
    ReservoirShrink(reservoir, needed);
    reservoir->reservoirLimit = needed;
    AVER(ReservoirIsConsistent(reservoir));  
  }
}


/* ReservoirLimit -- Return the reservoir limit */

Size ReservoirLimit(Reservoir reservoir)
{
  AVERT(Reservoir, reservoir);
  AVER(ReservoirIsConsistent(reservoir));  
  return reservoir->reservoirLimit;
}


/* ReservoirAvailable -- Return the amount in the reservoir */

Size ReservoirAvailable(Reservoir reservoir)
{
  AVERT(Reservoir, reservoir);
  ReservoirEnsureFull(reservoir);
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
  /* initialize the reservoir pool, design.mps.reservoir */
  res = PoolInit(&reservoir->poolStruct, 
                 arena, EnsureReservoirPoolClass());
  if (res == ResOK) {
    AVERT(Reservoir, reservoir);
  }
  return res;
}


/* ReservoirFinish -- Finish a reservoir */

void ReservoirFinish (Reservoir reservoir)
{
  PoolFinish(&reservoir->poolStruct);
  reservoir->sig = SigInvalid;
}

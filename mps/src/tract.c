/* impl.c.tract: TRACTS
 *
 * $HopeName: !tract.c(trunk.2) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * .readership: Any MPS developer
 */

#include "mpm.h"

SRCID(tract, "$HopeName: !tract.c(trunk.2) $");



#define TractArena(seg) PoolArena(TractPool(tract))


/* TractCheck -- check the integrity of a tract */

Bool TractCheck(Tract tract)
{
  CHECKU(Pool, TractPool(tract));
  CHECKL(AddrIsAligned(TractBase(tract), 
                       ArenaAlign(PoolArena(TractPool(tract)))));
  if (TractHasSeg(tract)) {
    CHECKL(TraceSetCheck(TractWhite(tract)));
    CHECKU(Seg, (Seg)TractP(tract)); 
  } else {
    CHECKL(TractWhite(tract) == TraceSetEMPTY);
  }
  return TRUE;
}


/* TractInit -- initialize a tract */

void TractInit(Tract tract, Pool pool, Addr base)
{
  AVER(tract != NULL);
  AVERT(Pool, pool);

  tract->pool = pool;
  tract->base = base;
  tract->p = NULL;
  tract->white = TraceSetEMPTY;
  tract->hasSeg = FALSE;

  AVERT(Tract, tract);

}


/* TractFinish -- finish a tract */

void TractFinish(Tract tract)
{
  AVERT(Tract, tract);

  /* Check that there's no segment - and hence no shielding */
  AVER(!TractHasSeg(tract));
  tract->pool = NULL;
}



/* .tract.critical: These tract functions are low-level and used 
 * throughout. They are therefore on the critical path and their 
 * AVERs are so-marked.
 */


/* TractBase -- return the base address of a tract */

Addr (TractBase)(Tract tract)
{
  Addr base;
  AVERT_CRITICAL(Tract, tract); /* .tract.critical */

  base = tract->base;
  return base;
}


/* TractLimit -- return the limit address of a segment */

Addr TractLimit(Tract tract)
{
  Arena arena;
  AVERT_CRITICAL(Tract, tract); /* .tract.critical */
  arena = TractArena(tract);
  AVERT_CRITICAL(Arena, arena);
  return AddrAdd(TractBase(tract), arena->alignment);
}


/* TractOfAddr -- return the tract the given address is in, if any */

Bool TractOfAddr(Tract *tractReturn, Arena arena, Addr addr)
{
  AVER_CRITICAL(tractReturn != NULL);
  AVERT_CRITICAL(Arena, arena);

  return (*arena->class->tractOfAddr)(tractReturn, arena, addr);
}


/* TractOfBaseAddr -- return a tract given a base address
 * 
 * The address must have been allocated to some pool
 */

Tract TractOfBaseAddr(Arena arena, Addr addr)
{
  Tract tract;
  Bool found;
  AVERT_CRITICAL(Arena, arena);
  AVER_CRITICAL(AddrIsAligned(addr, arena->alignment));

  /* check first in the cache - design.mps.arena.tract.cache */
  if (arena->lastTractBase == addr) {
    tract = arena->lastTract;
  } else {
    found = (*arena->class->tractOfAddr)(&tract, arena, addr);
    AVER_CRITICAL(found);
  }

  AVER_CRITICAL(TractBase(tract) == addr);
  return tract;
}


/* TractFirst -- return the first tract in the arena
 *
 * This is used to start an iteration over all tracts in the arena.
 */

Bool TractFirst(Tract *tractReturn, Arena arena)
{
  AVER(tractReturn != NULL);
  AVERT(Arena, arena);

  return (*arena->class->tractFirst)(tractReturn, arena);
}


/* TractNext -- return the "next" tract in the arena
 *
 * This is used as the iteration step when iterating over all
 * tracts in the arena.
 *
 * TractNext finds the tract with the lowest base address which is
 * greater than a specified address.  The address must be (or once
 * have been) the base address of a tract.
 */

Bool TractNext(Tract *tractReturn, Arena arena, Addr addr)
{
  AVER_CRITICAL(tractReturn != NULL); /* .tract.critical */
  AVERT_CRITICAL(Arena, arena);

  return (*arena->class->tractNext)(tractReturn, arena, addr);
}


/* TractNextContig -- return the contiguously following tract
 *
 * This is used as the iteration step when iterating over all
 * tracts in a contiguous area belonging to a pool.
 */

Tract TractNextContig(Arena arena, Tract tract)
{
  Tract next;

  AVERT_CRITICAL(Tract, tract);
  AVER_CRITICAL(NULL != TractPool(tract));

  next = (*arena->class->tractNextContig)(arena, tract);

  AVER_CRITICAL(TractPool(next) == TractPool(tract));
  AVER_CRITICAL(TractBase(next) == 
                AddrAdd(TractBase(tract), arena->alignment));
  return next;
}






/* impl.c.arena: ARENA ALLOCATION FEATURES
 *
 * $HopeName: MMsrc!arena.c(trunk.75) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 * 
 * .sources: design.mps.arena is the main design document.
 *
 * .improve.modular: The allocation stuff should be in a separate
 * structure.  We don't need to AVERT the whole Arena here (or
 * anywhere else, for that matter), but if everything's in the same
 * structure, then that's the rule.
 */

#include "tract.h"
#include "poolmv.h"
#include "mpm.h"

SRCID(arena, "$HopeName: MMsrc!arena.c(trunk.75) $");


/* ArenaControlPool -- get the control pool */

#define ArenaControlPool(arena) MVPool(&(arena)->controlPoolStruct)


/* ArenaReservoir - return the reservoir for the arena */

Reservoir ArenaReservoir(Arena arena)
{
  AVERT(Arena, arena);
  return &arena->reservoirStruct;
}


/* AbstractArenaClass  -- The abstact arena class definition
 *
 * .null: Most abstract class methods are set to NULL.
 * See design.mps.arena.class.abstract.null.
 */

typedef ArenaClassStruct AbstractArenaClassStruct;

DEFINE_CLASS(AbstractArenaClass, class)
{
  INHERIT_CLASS(&class->protocol, ProtocolClass);
  class->name = "ABSARENA";
  class->size = 0;
  class->offset = 0;
  class->init = NULL;
  class->finish = NULL;
  class->reserved = NULL;
  class->spareCommitExceeded = ArenaNoSpareCommitExceeded;
  class->extend = ArenaNoExtend;
  class->alloc = NULL;
  class->free = NULL;
  class->chunkInit = NULL;
  class->chunkFinish = NULL;
  class->describe = ArenaTrivDescribe;
  class->sig = ArenaClassSig;
}


/* ArenaClassCheck -- check the consistency of an arena class */

Bool ArenaClassCheck(ArenaClass class)
{
  CHECKL(ProtocolClassCheck(&class->protocol));
  CHECKL(class->name != NULL); /* Should be <=6 char C identifier */
  CHECKL(class->size >= sizeof(ArenaStruct));
  /* Offset of generic Pool within class-specific instance cannot be */
  /* greater than the size of the class-specific portion of the */
  /* instance. */
  CHECKL(class->offset <= (size_t)(class->size - sizeof(ArenaStruct)));
  CHECKL(FUNCHECK(class->init));
  CHECKL(FUNCHECK(class->finish));
  CHECKL(FUNCHECK(class->reserved));
  CHECKL(FUNCHECK(class->spareCommitExceeded));
  CHECKL(FUNCHECK(class->extend));
  CHECKL(FUNCHECK(class->alloc));
  CHECKL(FUNCHECK(class->free));
  CHECKL(FUNCHECK(class->chunkInit));
  CHECKL(FUNCHECK(class->chunkFinish));
  CHECKL(FUNCHECK(class->describe));
  CHECKS(ArenaClass, class);
  return TRUE;
}


/* ArenaAllocCheck -- check the consistency of the allocation fields */

Bool ArenaAllocCheck(Arena arena)
{
  CHECKS(Arena, arena);
  CHECKD(ArenaClass, arena->class);

  CHECKL(BoolCheck(arena->poolReady));
  if (arena->poolReady) { /* design.mps.arena.pool.ready */
    CHECKD(MV, &arena->controlPoolStruct);
    CHECKD(Reservoir, &arena->reservoirStruct);
  }
  /* Can't check that limit>=size because we may call ArenaCheck */
  /* while the size is being adjusted. */

  CHECKL(arena->fillMutatorSize >= 0.0);
  CHECKL(arena->emptyMutatorSize >= 0.0);
  CHECKL(arena->allocMutatorSize >= 0.0);
  CHECKL(arena->fillMutatorSize - arena->emptyMutatorSize >=
         arena->allocMutatorSize);
  CHECKL(arena->fillInternalSize >= 0.0);
  CHECKL(arena->emptyInternalSize >= 0.0);
  CHECKL(arena->committed <= arena->commitLimit);
  CHECKL(arena->spareCommitted <= arena->committed);
  CHECKL(arena->spareCommitted <= arena->spareCommitLimit);

  CHECKL(ShiftCheck(arena->zoneShift));
  CHECKL(AlignCheck(arena->alignment));
  /* Tract allocation must be platform-aligned. */
  CHECKL(arena->alignment >= MPS_PF_ALIGN);
  /* Stripes can't be smaller than pages. */
  CHECKL(((Size)1 << arena->zoneShift) >= arena->alignment);

  if (arena->lastTract == NULL) {
    CHECKL(arena->lastTractBase == (Addr)0);
  } else {
    CHECKL(TractBase(arena->lastTract) == arena->lastTractBase);
  }

  if (arena->primary != NULL) {
    CHECKD(Chunk, arena->primary);
  }
  CHECKL(RingCheck(&arena->chunkRing));
  /* nothing to check for chunkSerial */
  CHECKD(ChunkCacheEntry, &arena->chunkCache);

  CHECKL(LocusCheck(arena));

  return TRUE;
}


/* ArenaAllocInit -- initialize the allocation fields */

void ArenaAllocInit(Arena arena, ArenaClass class)
{
  /* We do not check the arena argument, because it's _supposed_ to */
  /* point to an uninitialized block of memory. */
  AVERT(ArenaClass, class);

  arena->class = class;

  arena->fillMutatorSize = 0.0;
  arena->emptyMutatorSize = 0.0;
  arena->allocMutatorSize = 0.0;
  arena->fillInternalSize = 0.0;
  arena->emptyInternalSize = 0.0;
  arena->committed = (Size)0;
  /* commitLimit may be overridden by init (but probably not */
  /* as there's not much point) */
  arena->commitLimit = (Size)-1;
  arena->spareCommitted = (Size)0;
  arena->spareCommitLimit = ARENA_INIT_SPARE_COMMIT_LIMIT;
  /* alignment is usually overridden by init */
  arena->alignment = 1 << ARENA_ZONESHIFT;
  /* zoneShift is usually overridden by init */
  arena->zoneShift = ARENA_ZONESHIFT;
  arena->poolReady = FALSE;     /* design.mps.arena.pool.ready */
  arena->lastTract = NULL;
  arena->lastTractBase = NULL;

  arena->primary = NULL;
  RingInit(&arena->chunkRing);
  arena->chunkSerial = (Serial)0;
  ChunkCacheEntryInit(&arena->chunkCache);

  LocusInit(arena);
}


/* ArenaAllocFinish -- finish the allocation fields */

void ArenaAllocFinish(Arena arena)
{
  LocusFinish(arena);
  RingFinish(&arena->chunkRing);
}


/* ArenaAllocCreate -- create the arena and call initializers */

Res ArenaAllocCreate(Arena *arenaReturn, ArenaClass class, va_list args)
{
  Arena arena;
  Res res;

  /* Do initialization.  This will call ArenaInit (see .init.caller). */
  res = (*class->init)(&arena, class, args);
  if (res != ResOK)
    goto failInit;

  arena->alignment = ChunkPageSize(arena->primary);
  if (arena->alignment > ((Size)1 << arena->zoneShift)) {
    res = ResMEMORY; /* size was too small */
    goto failStripeSize;
  }

  /* load cache */
  ChunkEncache(arena, arena->primary);

  AVERT(Arena, arena);
  *arenaReturn = arena;
  return ResOK;

failStripeSize:
  (*class->finish)(arena);
failInit:
  return res;
}


/* ArenaAllocDestroy -- destroy the arena */

void ArenaAllocDestroy(Arena arena)
{
  /* Call class-specific finishing.  This will call ArenaFinish. */
  (*arena->class->finish)(arena);
}


/* ControlInit -- initialize the control pool */

Res ControlInit(Arena arena)
{
  Res res;

  AVERT(Arena, arena);
  res = PoolInit(&arena->controlPoolStruct.poolStruct, 
                 arena, PoolClassMV(),
                 ARENA_CONTROL_EXTENDBY, ARENA_CONTROL_AVGSIZE,
                 ARENA_CONTROL_MAXSIZE);
  if (res != ResOK) 
    return res;
  arena->poolReady = TRUE;      /* design.mps.arena.pool.ready */
  return ResOK;
}


/* ControlFinish -- finish the control pool */

void ControlFinish(Arena arena)
{
  AVERT(Arena, arena);
  arena->poolReady = FALSE;
  PoolFinish(&arena->controlPoolStruct.poolStruct);
}


/* ArenaAllocDescribe -- describe the allocation fields */

Res ArenaAllocDescribe(Arena arena, mps_lib_FILE *stream)
{
  Res res;

  if (!CHECKT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "  poolReady $S\n",     arena->poolReady ? "YES" : "NO",
               "  controlPool $P\n",   
               (WriteFP)&arena->controlPoolStruct,
               "  fillMutatorSize $U KB\n",
                 (WriteFU)(arena->fillMutatorSize / 1024),
               "  emptyMutatorSize $U KB\n",
                 (WriteFU)(arena->emptyMutatorSize / 1024),
               "  allocMutatorSize $U KB\n",
                 (WriteFU)(arena->allocMutatorSize / 1024),
               "  fillInternalSize $U KB\n",
                 (WriteFU)(arena->fillInternalSize / 1024),
               "  emptyInternalSize $U KB\n",
                 (WriteFU)(arena->emptyInternalSize / 1024),
               "  commitLimit $W\n", (WriteFW)arena->commitLimit,
	       "  spareCommitted $W\n", (WriteFW)arena->spareCommitted,
	       "  spareCommitLimit $W\n", (WriteFW)arena->spareCommitLimit,
               "  zoneShift $U\n", (WriteFU)arena->zoneShift,
               "  alignment $W\n", (WriteFW)arena->alignment,
               NULL);
  if (res != ResOK) return res;

  res = (*arena->class->describe)(arena, stream);
  return res;
}


/* ArenaDescribeTracts -- describe all the tracts in the arena */

Res ArenaDescribeTracts(Arena arena, mps_lib_FILE *stream)
{
  Res res;
  Tract tract;
  Bool b;
  Addr oldLimit, base, limit;
  Size size;

  if (!CHECKT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  b = TractFirst(&tract, arena); 
  oldLimit = TractBase(tract);
  while (b) {
    base = TractBase(tract);
    limit = TractLimit(tract);
    size = ArenaAlign(arena);

    if (TractBase(tract) > oldLimit) {
      res = WriteF(stream,
                   "[$P, $P) $W $U   ---\n",
                   (WriteFP)oldLimit,
                   (WriteFP)base,
                   (WriteFW)AddrOffset(oldLimit, base),
                   (WriteFU)AddrOffset(oldLimit, base),
                   NULL);
      if (res != ResOK) return res;
    }

    res = WriteF(stream,
                 "[$P, $P) $W $U   $P ($S)\n",
                 (WriteFP)base,
                 (WriteFP)limit,
                 (WriteFW)size,
                 (WriteFW)size,
                 (WriteFP)TractPool(tract),
                 (WriteFS)(TractPool(tract)->class->name),
                 NULL);
    if (res != ResOK) return res;
    b = TractNext(&tract, arena, TractBase(tract));
    oldLimit = limit;
  }
  return ResOK;
}


/* ControlAlloc -- allocate a small block directly from the control pool
 *
 * .arena.control-pool: Actually the block will be allocated from the
 * control pool, which is an MV pool embedded in the arena itself.
 *
 * .controlalloc.addr: In implementations where Addr is not compatible
 * with void* (design.mps.type.addr.use), ControlAlloc must take care of
 * allocating so that the block can be addressed with a void*.
 */

Res ControlAlloc(void **baseReturn, Arena arena, size_t size, 
                 Bool withReservoirPermit)
{
  Addr base;
  Res res;

  AVERT(Arena, arena);
  AVER(baseReturn != NULL);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));
  AVER(arena->poolReady);

  res = PoolAlloc(&base, ArenaControlPool(arena), (Size)size,
                  withReservoirPermit);
  if (res != ResOK) 
    return res;

  *baseReturn = (void *)base; /* see .controlalloc.addr */
  return ResOK;
}


/* ControlFree -- free a block allocated using ControlAlloc */

void ControlFree(Arena arena, void* base, size_t size)
{
  AVERT(Arena, arena);
  AVER(base != NULL);
  AVER(size > 0);
  AVER(arena->poolReady);

  PoolFree(ArenaControlPool(arena), (Addr)base, (Size)size);
}


/* ArenaAlloc -- allocate some tracts from the arena */

Res ArenaAlloc(Addr *baseReturn, SegPref pref, Size size, Pool pool,
               Bool withReservoirPermit)
{
  Res res;
  Arena arena;
  Addr base;
  Tract baseTract;
  Reservoir reservoir;

  AVER(baseReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  AVER(BoolCheck(withReservoirPermit));

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(size, arena->alignment));
  reservoir = ArenaReservoir(arena);
  AVERT(Reservoir, reservoir);

  res = ReservoirEnsureFull(reservoir);
  if (res != ResOK) {
    AVER(ResIsAllocFailure(res));
    if (!withReservoirPermit)
      return res;
  }

  res = (*arena->class->alloc)(&base, &baseTract, pref, size, pool);
  if (res == ResOK) {
    goto goodAlloc;
  } else if (withReservoirPermit) {
    AVER(ResIsAllocFailure(res));
    res = ReservoirWithdraw(&base, &baseTract, reservoir, size, pool);
    if (res == ResOK)
      goto goodAlloc;
  }
  EVENT_PWP(ArenaAllocFail, arena, size, pool);
  return res;

goodAlloc:
  /* cache the tract - design.mps.arena.tract.cache */
  arena->lastTract = baseTract;
  arena->lastTractBase = base;

  EVENT_PPAWP(ArenaAlloc, arena, baseTract, base, size, pool);
  *baseReturn = base;
  return ResOK;
}


/* ArenaFree -- free some tracts to the arena */

void ArenaFree(Addr base, Size size, Pool pool)
{
  Arena arena;
  Addr limit;
  Reservoir reservoir;
  Res res;

  AVERT(Pool, pool);
  AVER(base != NULL);
  AVER(size > (Size)0);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  reservoir = ArenaReservoir(arena);
  AVERT(Reservoir, reservoir);
  AVER(AddrIsAligned(base, arena->alignment));
  AVER(SizeIsAligned(size, arena->alignment));

  /* uncache the tract if in range - design.mps.arena.tract.uncache */
  limit = AddrAdd(base, size);
  if ((arena->lastTractBase >= base) && (arena->lastTractBase < limit)) {
    arena->lastTract = NULL;
    arena->lastTractBase = (Addr)0;
  }

  res = ReservoirEnsureFull(reservoir);
  if (res == ResOK) {
    (*arena->class->free)(base, size, pool);
  } else {
    AVER(ResIsAllocFailure(res));
    ReservoirDeposit(reservoir, base, size);
  }

  EVENT_PAW(ArenaFree, arena, base, size);
  return;
}


Size ArenaReserved(Arena arena)
{
  AVERT(Arena, arena);
  return (*arena->class->reserved)(arena);
}

Size ArenaCommitted(Arena arena)
{
  AVERT(Arena, arena);
  return arena->committed;
}

Size ArenaSpareCommitted(Arena arena)
{
  AVERT(Arena, arena);
  return arena->spareCommitted;
}

Size ArenaSpareCommitLimit(Arena arena)
{
  AVERT(Arena, arena);
  return arena->spareCommitLimit;
}

void ArenaSetSpareCommitLimit(Arena arena, Size limit)
{
  AVERT(Arena, arena);
  /* Can't check limit, as all possible values are allowed. */

  arena->spareCommitLimit = limit;
  if (arena->spareCommitLimit < arena->spareCommitted) {
    arena->class->spareCommitExceeded(arena);
  }

  EVENT_PW(SpareCommitLimitSet, arena, limit);
  return;
}

/* Used by arenas which don't use spare committed memory */
void ArenaNoSpareCommitExceeded(Arena arena)
{
  AVERT(Arena, arena);
  return;
}


Size ArenaCommitLimit(Arena arena)
{
  AVERT(Arena, arena);
  return arena->commitLimit;
}

Res ArenaSetCommitLimit(Arena arena, Size limit)
{
  Size committed;
  Res res;

  AVERT(Arena, arena);
  AVER(ArenaCommitted(arena) <= arena->commitLimit);

  committed = ArenaCommitted(arena);
  if (limit < committed) {
    /* Attempt to set the limit below current committed */
    if (limit >= committed - arena->spareCommitted) {
      /* could set the limit by flushing any spare committed memory */
      arena->class->spareCommitExceeded(arena);
      AVER(limit >= ArenaCommitted(arena));
      arena->commitLimit = limit;
      res = ResOK;
    } else {
      res = ResFAIL;
    }
  } else {
    arena->commitLimit = limit;
    res = ResOK;
  }
  EVENT_PWU(CommitLimitSet, arena, limit, (res == ResOK));
  return res;
}


/* ArenaMutatorAllocSize -- total amount allocated by the mutator */

double ArenaMutatorAllocSize(Arena arena)
{
  AVERT(Arena, arena);
  return arena->fillMutatorSize - arena->emptyMutatorSize;
}


/* ArenaExtend -- Add a new chunk in the arena */

Res ArenaExtend(Arena arena, Addr base, Size size)
{
  Res res;

  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > 0);

  res = (*arena->class->extend)(arena, base, size);
  if (res != ResOK) 
    return res;

  EVENT_PAW(ArenaExtend, arena, base, size);
  return ResOK;
}


/* ArenaNoExtend -- fail to extend the arena by a chunk */

Res ArenaNoExtend(Arena arena, Addr base, Size size)
{
  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > (Size)0);

  NOTREACHED;
  return ResUNIMPL;
}


/* ArenaTrivDescribe -- produce trivial description of an arena */

Res ArenaTrivDescribe(Arena arena, mps_lib_FILE *stream)
{
  AVERT(Arena, arena);
  AVER(stream != NULL);

  return WriteF(stream, 
    "  No class-specific description available.\n", NULL);
}

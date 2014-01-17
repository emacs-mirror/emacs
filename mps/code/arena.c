/* arena.c: ARENA ALLOCATION FEATURES
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .sources: <design/arena/> is the main design document.  */

#include "tract.h"
#include "poolmv.h"
#include "mpm.h"
#include "cbs.h"


SRCID(arena, "$Id$");


/* ArenaControlPool -- get the control pool */

#define ArenaControlPool(arena) MV2Pool(&(arena)->controlPoolStruct)


/* Forward declarations */

static void ArenaTrivCompact(Arena arena, Trace trace);


/* ArenaTrivDescribe -- produce trivial description of an arena */

static Res ArenaTrivDescribe(Arena arena, mps_lib_FILE *stream)
{
  if (!TESTT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  /* .describe.triv.never-called-from-subclass-method:
   * This Triv method seems to assume that it will never get called
   * from a subclass-method invoking ARENA_SUPERCLASS()->describe.
   * It assumes that it only gets called if the describe method has
   * not been subclassed.  (That's the only reason for printing the
   * "No class-specific description available" message).
   * This is bogus, but that's the status quo.  RHSK 2007-04-27.
   */
  /* .describe.triv.dont-upcall: Therefore (for now) the last 
   * subclass describe method should avoid invoking 
   * ARENA_SUPERCLASS()->describe.  RHSK 2007-04-27.
   */
  return WriteF(stream,
    "  No class-specific description available.\n", NULL);
}


/* AbstractArenaClass  -- The abstract arena class definition
 *
 * .null: Most abstract class methods are set to NULL.  See
 * <design/arena/#class.abstract.null>.  */

typedef ArenaClassStruct AbstractArenaClassStruct;

DEFINE_CLASS(AbstractArenaClass, class)
{
  INHERIT_CLASS(&class->protocol, ProtocolClass);
  class->name = "ABSARENA";
  class->size = 0;
  class->offset = 0;
  class->varargs = ArgTrivVarargs;
  class->init = NULL;
  class->finish = NULL;
  class->reserved = NULL;
  class->spareCommitExceeded = ArenaNoSpareCommitExceeded;
  class->extend = ArenaNoExtend;
  class->alloc = NULL;
  class->free = NULL;
  class->chunkInit = NULL;
  class->chunkFinish = NULL;
  class->compact = ArenaTrivCompact;
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
  CHECKL(FUNCHECK(class->varargs));
  CHECKL(FUNCHECK(class->init));
  CHECKL(FUNCHECK(class->finish));
  CHECKL(FUNCHECK(class->reserved));
  CHECKL(FUNCHECK(class->spareCommitExceeded));
  CHECKL(FUNCHECK(class->extend));
  CHECKL(FUNCHECK(class->alloc));
  CHECKL(FUNCHECK(class->free));
  CHECKL(FUNCHECK(class->chunkInit));
  CHECKL(FUNCHECK(class->chunkFinish));
  CHECKL(FUNCHECK(class->compact));
  CHECKL(FUNCHECK(class->describe));
  CHECKS(ArenaClass, class);
  return TRUE;
}


/* ArenaCheck -- check the arena */

Bool ArenaCheck(Arena arena)
{
  CHECKS(Arena, arena);
  CHECKD(Globals, ArenaGlobals(arena));
  CHECKD(ArenaClass, arena->class);

  CHECKL(BoolCheck(arena->poolReady));
  if (arena->poolReady) { /* <design/arena/#pool.ready> */
    CHECKD(MV, &arena->controlPoolStruct);
    CHECKD(Reservoir, &arena->reservoirStruct);
  }
  /* Can't check that limit>=size because we may call ArenaCheck */
  /* while the size is being adjusted. */

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
  
  /* FIXME: Can't check freeCBS until it's initialised */
  /* CHECKD(CBS, &arena->freeCBS); */

  CHECKL(LocusCheck(arena));
  
  return TRUE;
}


/* ArenaInit -- initialize the generic part of the arena
 *
 * .init.caller: Unlike PoolInit, this is called by the class init
 * methods, not the generic Create.  This is because the class is
 * responsible for allocating the descriptor.  */

Res ArenaInit(Arena arena, ArenaClass class)
{
  Res res;

  /* We do not check the arena argument, because it's _supposed_ to */
  /* point to an uninitialized block of memory. */
  AVERT(ArenaClass, class);

  arena->class = class;

  arena->committed = (Size)0;
  /* commitLimit may be overridden by init (but probably not */
  /* as there's not much point) */
  arena->commitLimit = (Size)-1;
  arena->spareCommitted = (Size)0;
  arena->spareCommitLimit = ARENA_INIT_SPARE_COMMIT_LIMIT;
  /* alignment is usually overridden by init */
  arena->alignment = (Align)1 << ARENA_ZONESHIFT;
  /* zoneShift is usually overridden by init */
  arena->zoneShift = ARENA_ZONESHIFT;
  arena->poolReady = FALSE;     /* <design/arena/#pool.ready> */
  arena->lastTract = NULL;
  arena->lastTractBase = NULL;

  arena->primary = NULL;
  RingInit(&arena->chunkRing);
  arena->chunkSerial = (Serial)0;
  ChunkCacheEntryInit(&arena->chunkCache);
  
  LocusInit(arena);
  
  res = GlobalsInit(ArenaGlobals(arena));
  if (res != ResOK)
    goto failGlobalsInit;

  arena->sig = ArenaSig;

  MPS_ARGS_BEGIN(cbsiArgs) {
    MPS_ARGS_ADD(cbsiArgs, MPS_KEY_CBS_EXTEND_BY, 0); /* FIXME: explain why we never extend */
    MPS_ARGS_DONE(cbsiArgs);
    res = CBSInit(arena, &arena->freeCBS, arena, arena->alignment, TRUE, cbsiArgs);
  } MPS_ARGS_END(cbsiArgs);
  if (res != ResOK)
    goto failCBSInit;

  /* initialize the reservoir, <design/reservoir/> */
  res = ReservoirInit(&arena->reservoirStruct, arena);
  if (res != ResOK)
    goto failReservoirInit;

  AVERT(Arena, arena);
  return ResOK;

failReservoirInit:
  CBSFinish(&arena->freeCBS);
failCBSInit:
  GlobalsFinish(ArenaGlobals(arena));
failGlobalsInit:
  return res;
}


/* VM keys are defined here even though the code they apply to might
 * not be linked.  For example, MPS_KEY_VMW3_TOP_DOWN only applies to
 * vmw3.c.  The reason is that we want these keywords to be optional
 * even on the wrong platform, so that clients can write simple portable
 * code.  They should be free to pass MPS_KEY_VMW3_TOP_DOWN on other
 * platforms, knowing that it has no effect.  To do that, the key must
 * exist on all platforms. */

ARG_DEFINE_KEY(vmw3_top_down, Bool);


/* ArenaCreate -- create the arena and call initializers */

ARG_DEFINE_KEY(arena_size, Size);

Res ArenaCreate(Arena *arenaReturn, ArenaClass class, ArgList args)
{
  Arena arena;
  Res res;

  AVER(arenaReturn != NULL);
  AVERT(ArenaClass, class);
  AVER(ArgListCheck(args));

  /* We must initialise the event subsystem very early, because event logging
     will start as soon as anything interesting happens and expect to write
     to the EventLast pointers. */
  EventInit();

  /* Do initialization.  This will call ArenaInit (see .init.caller). */
  res = (*class->init)(&arena, class, args);
  if (res != ResOK)
    goto failInit;

  /* arena->alignment must have been set up by *class->init() */
  if (arena->alignment > ((Size)1 << arena->zoneShift)) {
    res = ResMEMORY; /* size was too small */
    goto failStripeSize;
  }

  res = ControlInit(arena);
  if (res != ResOK)
    goto failControlInit;

  res = GlobalsCompleteCreate(ArenaGlobals(arena));
  if (res != ResOK)
    goto failGlobalsCompleteCreate;

  AVERT(Arena, arena);
  *arenaReturn = arena;
  return ResOK;

failGlobalsCompleteCreate:
  ControlFinish(arena);
failControlInit:
failStripeSize:
  (*class->finish)(arena);
failInit:
  return res;
}


/* ArenaFinish -- finish the generic part of the arena
 *
 * .finish.caller: Unlike PoolFinish, this is called by the class finish
 * methods, not the generic Destroy.  This is because the class is
 * responsible for deallocating the descriptor.  */

void ArenaFinish(Arena arena)
{
  ReservoirFinish(ArenaReservoir(arena));
  CBSFinish(&arena->freeCBS);
  arena->sig = SigInvalid;
  GlobalsFinish(ArenaGlobals(arena));
  LocusFinish(arena);
  RingFinish(&arena->chunkRing);
}


/* ArenaDestroy -- destroy the arena */

void ArenaDestroy(Arena arena)
{
  AVERT(Arena, arena);

  GlobalsPrepareToDestroy(ArenaGlobals(arena));

  /* Empty the reservoir - see <code/reserv.c#reservoir.finish> */
  ReservoirSetLimit(ArenaReservoir(arena), 0);

  arena->poolReady = FALSE;
  ControlFinish(arena);

  /* Call class-specific finishing.  This will call ArenaFinish. */
  (*arena->class->finish)(arena);

  EventFinish();
}


/* ControlInit -- initialize the control pool */

Res ControlInit(Arena arena)
{
  Res res;

  AVERT(Arena, arena);
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, CONTROL_EXTEND_BY);
    MPS_ARGS_DONE(args);
    res = PoolInit(&arena->controlPoolStruct.poolStruct, arena,
                   PoolClassMV(), args);
  } MPS_ARGS_END(args);
  if (res != ResOK)
    return res;
  arena->poolReady = TRUE;      /* <design/arena/#pool.ready> */
  return ResOK;
}


/* ControlFinish -- finish the control pool */

void ControlFinish(Arena arena)
{
  AVERT(Arena, arena);
  arena->poolReady = FALSE;
  PoolFinish(&arena->controlPoolStruct.poolStruct);
}


/* ArenaDescribe -- describe the arena */

Res ArenaDescribe(Arena arena, mps_lib_FILE *stream)
{
  Res res;
  Size reserved;

  if (!TESTT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream, "Arena $P {\n", (WriteFP)arena,
               "  class $P (\"$S\")\n",
               (WriteFP)arena->class, arena->class->name,
               NULL);
  if (res != ResOK) return res;

  if (arena->poolReady) {
    res = WriteF(stream,
                 "  controlPool $P\n", (WriteFP)&arena->controlPoolStruct,
                 NULL);
    if (res != ResOK) return res;
  }

  /* Note: this Describe clause calls a function */
  reserved = ArenaReserved(arena);
  res = WriteF(stream,
               "  reserved         $W  <-- "
               "total size of address-space reserved\n",
               (WriteFW)reserved,
               NULL);
  if (res != ResOK) return res;

  res = WriteF(stream,
               "  committed        $W  <-- "
               "total bytes currently stored (in RAM or swap)\n",
               (WriteFW)arena->committed,
               "  commitLimit      $W\n", (WriteFW)arena->commitLimit,
               "  spareCommitted   $W\n", (WriteFW)arena->spareCommitted,
               "  spareCommitLimit $W\n", (WriteFW)arena->spareCommitLimit,
               "  zoneShift $U\n", (WriteFU)arena->zoneShift,
               "  alignment $W\n", (WriteFW)arena->alignment,
               NULL);
  if (res != ResOK) return res;

  res = WriteF(stream,
               "  droppedMessages $U$S\n", (WriteFU)arena->droppedMessages,
               (arena->droppedMessages == 0 ? "" : "  -- MESSAGES DROPPED!"),
               NULL);
  if (res != ResOK) return res;

  res = (*arena->class->describe)(arena, stream);
  if (res != ResOK) return res;

  /* Do not call GlobalsDescribe: it makes too much output, thanks.
   * RHSK 2007-04-27
   */
#if 0
  res = GlobalsDescribe(ArenaGlobals(arena), stream);
  if (res != ResOK) return res;
#endif

  res = WriteF(stream,
               "} Arena $P ($U)\n", (WriteFP)arena,
               (WriteFU)arena->serial,
               NULL);
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

  if (!TESTT(Arena, arena)) return ResFAIL;
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
                   (WriteFP)oldLimit, (WriteFP)base,
                   (WriteFW)AddrOffset(oldLimit, base),
                   (WriteFU)AddrOffset(oldLimit, base),
                   NULL);
      if (res != ResOK) return res;
    }

    res = WriteF(stream,
                 "[$P, $P) $W $U   $P ($S)\n",
                 (WriteFP)base, (WriteFP)limit,
                 (WriteFW)size, (WriteFW)size,
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
 * with void* (<design/type/#addr.use>), ControlAlloc must take care of
 * allocating so that the block can be addressed with a void*.  */

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


/* ControlDescribe -- describe the arena's control pool */

Res ControlDescribe(Arena arena, mps_lib_FILE *stream)
{
  Res res;

  if (!TESTT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = PoolDescribe(ArenaControlPool(arena), stream);

  return res;
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
  EVENT3(ArenaAllocFail, arena, size, pool);
  return res;

goodAlloc:
  /* cache the tract - <design/arena/#tract.cache> */
  arena->lastTract = baseTract;
  arena->lastTractBase = base;

  EVENT5(ArenaAlloc, arena, baseTract, base, size, pool);
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

  /* uncache the tract if in range - <design/arena/#tract.uncache> */
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

  EVENT3(ArenaFree, arena, base, size);
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

  EVENT2(SpareCommitLimitSet, arena, limit);
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
  EVENT3(CommitLimitSet, arena, limit, (res == ResOK));
  return res;
}


/* ArenaAvail -- return available memory in the arena */

Size ArenaAvail(Arena arena)
{
  Size sSwap;

  sSwap = ArenaReserved(arena);
  if (sSwap > arena->commitLimit)
    sSwap = arena->commitLimit;

  /* TODO: sSwap should take into account the amount of backing store
     available to supply the arena with memory.  This would be the amount
     available in the paging file, which is possibly the amount of free
     disk space in some circumstances.  We'd have to see whether we can get
     this information from the operating system.  It also depends on the
     arena class, of course. */

  return sSwap - arena->committed + arena->spareCommitted;
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

  EVENT3(ArenaExtend, arena, base, size);
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


/* ArenaCompact -- respond (or not) to trace reclaim */

void ArenaCompact(Arena arena, Trace trace)
{
  AVERT(Arena, arena);
  AVERT(Trace, trace);
  (*arena->class->compact)(arena, trace);
}

static void ArenaTrivCompact(Arena arena, Trace trace)
{
  UNUSED(arena);
  UNUSED(trace);
  return;
}


/* Has Addr */

Bool ArenaHasAddr(Arena arena, Addr addr)
{
  Seg seg;

  AVERT(Arena, arena);
  return SegOfAddr(&seg, arena, addr);
}


/* ArenaAddrObject -- find client pointer to object containing addr
 * See job003589.
 */

Res ArenaAddrObject(Addr *pReturn, Arena arena, Addr addr)
{
  Seg seg;
  Pool pool;

  AVER(pReturn != NULL);
  AVERT(Arena, arena);
  
  if (!SegOfAddr(&seg, arena, addr)) {
    return ResFAIL;
  }
  pool = SegPool(seg);
  return PoolAddrObject(pReturn, pool, seg, addr);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

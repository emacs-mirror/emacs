/* arena.c: ARENA ALLOCATION FEATURES
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .sources: <design/arena/> is the main design document.  */

#include "tract.h"
#include "poolmv.h"
#include "mpm.h"
#include "cbs.h"
#include "bt.h"
#include "poolmfs.h"
#include "mpscmfs.h"


SRCID(arena, "$Id$");


#define ArenaControlPool(arena) MV2Pool(&(arena)->controlPoolStruct)
#define ArenaCBSBlockPool(arena)  (&(arena)->freeCBSBlockPoolStruct.poolStruct)
#define ArenaFreeCBS(arena)    (&(arena)->freeCBSStruct)


/* Forward declarations */

static void ArenaTrivCompact(Arena arena, Trace trace);
static void arenaFreePage(Arena arena, Addr base, Pool pool);


/* ArenaTrivDescribe -- produce trivial description of an arena */

static Res ArenaTrivDescribe(Arena arena, mps_lib_FILE *stream, Count depth)
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
  return WriteF(stream, depth,
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
  class->purgeSpare = ArenaNoPurgeSpare;
  class->extend = ArenaNoExtend;
  class->grow = ArenaNoGrow;
  class->free = NULL;
  class->chunkInit = NULL;
  class->chunkFinish = NULL;
  class->compact = ArenaTrivCompact;
  class->describe = ArenaTrivDescribe;
  class->pagesMarkAllocated = NULL;
  class->sig = ArenaClassSig;
}


/* ArenaClassCheck -- check the consistency of an arena class */

Bool ArenaClassCheck(ArenaClass class)
{
  CHECKD(ProtocolClass, &class->protocol);
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
  CHECKL(FUNCHECK(class->purgeSpare));
  CHECKL(FUNCHECK(class->extend));
  CHECKL(FUNCHECK(class->grow));
  CHECKL(FUNCHECK(class->free));
  CHECKL(FUNCHECK(class->chunkInit));
  CHECKL(FUNCHECK(class->chunkFinish));
  CHECKL(FUNCHECK(class->compact));
  CHECKL(FUNCHECK(class->describe));
  CHECKL(FUNCHECK(class->pagesMarkAllocated));
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
  CHECKD_NOSIG(Ring, &arena->chunkRing);
  /* nothing to check for chunkSerial */
  CHECKD(ChunkCacheEntry, &arena->chunkCache);
  
  CHECKL(LocusCheck(arena));

  CHECKL(BoolCheck(arena->hasFreeCBS));
  if (arena->hasFreeCBS)
    CHECKD(CBS, ArenaFreeCBS(arena));

  CHECKL(BoolCheck(arena->zoned));

  return TRUE;
}


/* ArenaInit -- initialize the generic part of the arena
 *
 * .init.caller: ArenaInit is called by class->init (which is called
 * by ArenaCreate). The initialization must proceed in this order, as
 * opposed to class->init being called by ArenaInit, which would
 * correspond to the initialization order for pools and other objects,
 * because the memory for the arena structure is not available until
 * it has been allocated by the arena class.
 */

Res ArenaInit(Arena arena, ArenaClass class, Align alignment, ArgList args)
{
  Res res;
  Bool zoned = ARENA_DEFAULT_ZONED;
  mps_arg_s arg;

  AVER(arena != NULL);
  AVERT(ArenaClass, class);
  AVERT(Align, alignment);
  
  if (ArgPick(&arg, args, MPS_KEY_ARENA_ZONED))
    zoned = arg.val.b;

  arena->class = class;

  arena->committed = (Size)0;
  /* commitLimit may be overridden by init (but probably not */
  /* as there's not much point) */
  arena->commitLimit = (Size)-1;
  arena->spareCommitted = (Size)0;
  arena->spareCommitLimit = ARENA_INIT_SPARE_COMMIT_LIMIT;
  arena->alignment = alignment;
  /* zoneShift is usually overridden by init */
  arena->zoneShift = ARENA_ZONESHIFT;
  arena->poolReady = FALSE;     /* <design/arena/#pool.ready> */
  arena->lastTract = NULL;
  arena->lastTractBase = NULL;
  arena->hasFreeCBS = FALSE;
  arena->freeZones = ZoneSetUNIV;
  arena->zoned = zoned;

  arena->primary = NULL;
  RingInit(&arena->chunkRing);
  arena->chunkSerial = (Serial)0;
  ChunkCacheEntryInit(&arena->chunkCache);
  
  LocusInit(arena);
  
  res = GlobalsInit(ArenaGlobals(arena));
  if (res != ResOK)
    goto failGlobalsInit;

  arena->sig = ArenaSig;
  
  /* Initialise a pool to hold the arena's CBS blocks.  This pool can't be
     allowed to extend itself using ArenaAlloc because it is used during
     ArenaAlloc, so MFSExtendSelf is set to FALSE.  Failures to extend are
     handled where the CBS is used. */

  MPS_ARGS_BEGIN(piArgs) {
    MPS_ARGS_ADD(piArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(CBSBlockStruct));
    MPS_ARGS_ADD(piArgs, MPS_KEY_EXTEND_BY, arena->alignment);
    MPS_ARGS_ADD(piArgs, MFSExtendSelf, FALSE);
    res = PoolInit(ArenaCBSBlockPool(arena), arena, PoolClassMFS(), piArgs);
  } MPS_ARGS_END(piArgs);
  AVER(res == ResOK); /* no allocation, no failure expected */
  if (res != ResOK)
    goto failMFSInit;

  /* Initialise the freeCBS. */
  MPS_ARGS_BEGIN(cbsiArgs) {
    MPS_ARGS_ADD(cbsiArgs, CBSBlockPool, ArenaCBSBlockPool(arena));
    res = CBSInit(ArenaFreeCBS(arena), arena, arena, alignment,
                  /* fastFind */ TRUE, arena->zoned, cbsiArgs);
  } MPS_ARGS_END(cbsiArgs);
  AVER(res == ResOK); /* no allocation, no failure expected */
  if (res != ResOK)
    goto failCBSInit;
  /* Note that although freeCBS is initialised, it doesn't have any memory
     for its blocks, so hasFreeCBS remains FALSE until later. */

  /* initialize the reservoir, <design/reservoir/> */
  res = ReservoirInit(&arena->reservoirStruct, arena);
  if (res != ResOK)
    goto failReservoirInit;

  AVERT(Arena, arena);
  return ResOK;

failReservoirInit:
  CBSFinish(ArenaFreeCBS(arena));
failCBSInit:
  PoolFinish(ArenaCBSBlockPool(arena));
failMFSInit:
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
ARG_DEFINE_KEY(arena_zoned, Bool);

Res ArenaCreate(Arena *arenaReturn, ArenaClass class, ArgList args)
{
  Arena arena;
  Res res;

  AVER(arenaReturn != NULL);
  AVERT(ArenaClass, class);
  AVERT(ArgList, args);

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

  /* With the primary chunk initialised we can add page memory to the freeCBS
     that describes the free address space in the primary chunk. */
  arena->hasFreeCBS = TRUE;
  res = ArenaFreeCBSInsert(arena,
                           PageIndexBase(arena->primary,
                                         arena->primary->allocBase),
                           arena->primary->limit);
  if (res != ResOK)
    goto failPrimaryCBS;
  
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
failPrimaryCBS:
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
  arena->sig = SigInvalid;
  GlobalsFinish(ArenaGlobals(arena));
  LocusFinish(arena);
  RingFinish(&arena->chunkRing);
}


/* ArenaDestroy -- destroy the arena */

static void arenaMFSPageFreeVisitor(Pool pool, Addr base, Size size,
                                    void *closureP, Size closureS)
{
  AVERT(Pool, pool);
  UNUSED(closureP);
  UNUSED(closureS);
  UNUSED(size);
  AVER(size == ArenaAlign(PoolArena(pool)));
  arenaFreePage(PoolArena(pool), base, pool);
}

void ArenaDestroy(Arena arena)
{
  AVERT(Arena, arena);

  GlobalsPrepareToDestroy(ArenaGlobals(arena));

  /* Empty the reservoir - see <code/reserv.c#reservoir.finish> */
  ReservoirSetLimit(ArenaReservoir(arena), 0);

  arena->poolReady = FALSE;
  ControlFinish(arena);

  /* We must tear down the freeCBS before the chunks, because pages
     containing CBS blocks might be allocated in those chunks. */
  AVER(arena->hasFreeCBS);
  arena->hasFreeCBS = FALSE;
  CBSFinish(ArenaFreeCBS(arena));

  /* The CBS block pool can't free its own memory via ArenaFree because
     that would use the ZonedCBS. */
  MFSFinishTracts(ArenaCBSBlockPool(arena),
                  arenaMFSPageFreeVisitor, NULL, 0);
  PoolFinish(ArenaCBSBlockPool(arena));

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

Res ArenaDescribe(Arena arena, mps_lib_FILE *stream, Count depth)
{
  Res res;
  Size reserved;

  if (!TESTT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream, depth, "Arena $P {\n", (WriteFP)arena,
               "  class $P (\"$S\")\n",
               (WriteFP)arena->class, arena->class->name,
               NULL);
  if (res != ResOK) return res;

  if (arena->poolReady) {
    res = WriteF(stream, depth + 2,
                 "controlPool $P\n", (WriteFP)&arena->controlPoolStruct,
                 NULL);
    if (res != ResOK) return res;
  }

  /* Note: this Describe clause calls a function */
  reserved = ArenaReserved(arena);
  res = WriteF(stream, depth + 2,
               "reserved         $W  <-- "
               "total size of address-space reserved\n",
               (WriteFW)reserved,
               NULL);
  if (res != ResOK) return res;

  res = WriteF(stream, depth + 2,
               "committed        $W  <-- "
               "total bytes currently stored (in RAM or swap)\n",
               (WriteFW)arena->committed,
               "commitLimit      $W\n", (WriteFW)arena->commitLimit,
               "spareCommitted   $W\n", (WriteFW)arena->spareCommitted,
               "spareCommitLimit $W\n", (WriteFW)arena->spareCommitLimit,
               "zoneShift $U\n", (WriteFU)arena->zoneShift,
               "alignment $W\n", (WriteFW)arena->alignment,
               NULL);
  if (res != ResOK) return res;

  res = WriteF(stream, depth + 2,
               "droppedMessages $U$S\n", (WriteFU)arena->droppedMessages,
               (arena->droppedMessages == 0 ? "" : "  -- MESSAGES DROPPED!"),
               NULL);
  if (res != ResOK) return res;

  res = (*arena->class->describe)(arena, stream, depth);
  if (res != ResOK) return res;

  res = WriteF(stream, depth + 2, "Globals {\n", NULL);
  if (res != ResOK) return res;  
  res = GlobalsDescribe(ArenaGlobals(arena), stream, depth + 4);
  if (res != ResOK) return res;
  res = WriteF(stream, depth + 2, "} Globals\n", NULL);
  if (res != ResOK) return res;  

  res = WriteF(stream, depth,
               "} Arena $P ($U)\n", (WriteFP)arena,
               (WriteFU)arena->serial,
               NULL);
  return res;
}


/* ArenaDescribeTracts -- describe all the tracts in the arena */

Res ArenaDescribeTracts(Arena arena, mps_lib_FILE *stream, Count depth)
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
      res = WriteF(stream, depth,
                   "[$P, $P) $W $U   ---\n",
                   (WriteFP)oldLimit, (WriteFP)base,
                   (WriteFW)AddrOffset(oldLimit, base),
                   (WriteFU)AddrOffset(oldLimit, base),
                   NULL);
      if (res != ResOK) return res;
    }

    res = WriteF(stream, depth,
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
  AVERT(Bool, withReservoirPermit);
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

Res ControlDescribe(Arena arena, mps_lib_FILE *stream, Count depth)
{
  Res res;

  if (!TESTT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = PoolDescribe(ArenaControlPool(arena), stream, depth);

  return res;
}


/* arenaAllocPage -- allocate one page from the arena
 *
 * This is a primitive allocator used to allocate pages for the arena CBS.
 * It is called rarely and can use a simple search.  It may not use the
 * CBS or any pool, because it is used as part of the bootstrap.
 */

static Res arenaAllocPageInChunk(Addr *baseReturn, Chunk chunk, Pool pool)
{
  Res res;
  Index basePageIndex, limitPageIndex;
  Arena arena;

  AVER(baseReturn != NULL);
  AVERT(Chunk, chunk);
  AVERT(Pool, pool);
  arena = ChunkArena(chunk);
  
  if (!BTFindShortResRange(&basePageIndex, &limitPageIndex,
                           chunk->allocTable,
                           chunk->allocBase, chunk->pages, 1))
    return ResRESOURCE;
  
  res = (*arena->class->pagesMarkAllocated)(arena, chunk,
                                            basePageIndex, 1,
                                            pool);
  if (res != ResOK)
    return res;
  
  *baseReturn = PageIndexBase(chunk, basePageIndex);
  return ResOK;
}

static Res arenaAllocPage(Addr *baseReturn, Arena arena, Pool pool)
{
  Res res;
  
  /* Favour the primary chunk, because pages allocated this way aren't
     currently freed, and we don't want to prevent chunks being destroyed. */
  /* TODO: Consider how the ArenaCBSBlockPool might free pages. */
  res = arenaAllocPageInChunk(baseReturn, arena->primary, pool);
  if (res != ResOK) {
    Ring node, next;
    RING_FOR(node, &arena->chunkRing, next) {
      Chunk chunk = RING_ELT(Chunk, chunkRing, node);
      if (chunk != arena->primary) {
        res = arenaAllocPageInChunk(baseReturn, chunk, pool);
        if (res == ResOK)
          break;
      }
    }
  }
  return res;
}


/* arenaFreePage -- free page allocated by arenaAllocPage */

static void arenaFreePage(Arena arena, Addr base, Pool pool)
{
  AVERT(Arena, arena);
  AVERT(Pool, pool);
  (*arena->class->free)(base, ArenaAlign(arena), pool);
}


/* arenaExtendCBSBlockPool -- add a page of memory to the CBS block pool
 *
 * IMPORTANT: Must be followed by arenaExcludePage to ensure that the
 * page doesn't get allocated by ArenaAlloc.  See .insert.exclude.
 */

static Res arenaExtendCBSBlockPool(Range pageRangeReturn, Arena arena)
{
  Addr pageBase;
  Res res;

  res = arenaAllocPage(&pageBase, arena, ArenaCBSBlockPool(arena));
  if (res != ResOK)
    return res;
  MFSExtend(ArenaCBSBlockPool(arena), pageBase, ArenaAlign(arena));

  RangeInit(pageRangeReturn, pageBase, AddrAdd(pageBase, ArenaAlign(arena)));
  return ResOK;
}

/* arenaExcludePage -- exclude CBS block pool's page from CBSs
 *
 * Exclude the page we specially allocated for the CBS block pool
 * so that it doesn't get reallocated.
 */

static void arenaExcludePage(Arena arena, Range pageRange)
{
  RangeStruct oldRange;
  Res res;

  res = CBSDelete(&oldRange, ArenaFreeCBS(arena), pageRange);
  AVER(res == ResOK); /* we just gave memory to the CBSs */
}


/* arenaCBSInsert -- add a block to an arena CBS, extending pool if necessary
 *
 * The arena's CBSs can't get memory in the usual way because they are used
 * in the basic allocator, so we allocate pages specially.
 *
 * Only fails if it can't get a page for the block pool.
 */

static Res arenaCBSInsert(Range rangeReturn, Arena arena, Range range)
{
  Res res;
  
  AVER(rangeReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Range, range);

  res = CBSInsert(rangeReturn, ArenaFreeCBS(arena), range);

  if (res == ResLIMIT) { /* freeCBS MFS pool ran out of blocks */
    RangeStruct pageRange;
    res = arenaExtendCBSBlockPool(&pageRange, arena);
    if (res != ResOK)
      return res;
    /* .insert.exclude: Must insert before exclude so that we can
       bootstrap when the zoned CBS is empty. */
    res = CBSInsert(rangeReturn, ArenaFreeCBS(arena), range);
    AVER(res == ResOK); /* we just gave memory to the CBSs */
    arenaExcludePage(arena, &pageRange);
  }
  
  return ResOK;
}


/* ArenaFreeCBSInsert -- add a block to arena CBS, maybe stealing memory
 *
 * See arenaCBSInsert.  This function may only be applied to mapped pages
 * and may steal them to store CBS nodes if it's unable to allocate
 * space for CBS nodes.
 *
 * IMPORTANT: May update rangeIO.
 */

static void arenaCBSInsertSteal(Range rangeReturn, Arena arena, Range rangeIO)
{
  Res res;
  
  AVER(rangeReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Range, rangeIO);

  res = arenaCBSInsert(rangeReturn, arena, rangeIO);
  
  if (res != ResOK) {
    Addr pageBase;
    Tract tract;
    AVER(ResIsAllocFailure(res));

    /* Steal a page from the memory we're about to free. */
    AVER(RangeSize(rangeIO) >= ArenaAlign(arena));
    pageBase = RangeBase(rangeIO);
    RangeInit(rangeIO, AddrAdd(pageBase, ArenaAlign(arena)),
                       RangeLimit(rangeIO));

    /* Steal the tract from its owning pool. */
    tract = TractOfBaseAddr(arena, pageBase);
    TractFinish(tract);
    TractInit(tract, ArenaCBSBlockPool(arena), pageBase);
  
    MFSExtend(ArenaCBSBlockPool(arena), pageBase, ArenaAlign(arena));

    /* Try again. */
    res = CBSInsert(rangeReturn, ArenaFreeCBS(arena), rangeIO);
    AVER(res == ResOK); /* we just gave memory to the CBS */
  }

  AVER(res == ResOK); /* not expecting other kinds of error from the CBS */
}


/* ArenaFreeCBSInsert -- add block to free CBS, extending pool if necessary
 *
 * The inserted block of address space may not abut any existing block.
 * This restriction ensures that we don't coalesce chunks and allocate
 * object across the boundary, preventing chunk deletion.
 */

Res ArenaFreeCBSInsert(Arena arena, Addr base, Addr limit)
{
  RangeStruct range, oldRange;
  Res res;

  AVERT(Arena, arena);

  RangeInit(&range, base, limit);
  res = arenaCBSInsert(&oldRange, arena, &range);
  if (res != ResOK)
    return res;

  /* .chunk.no-coalesce: Make sure it didn't coalesce.  We don't want
     chunks to coalesce so that there are no chunk-crossing allocations
     that would prevent chunks being destroyed. */
  AVER(RangesEqual(&oldRange, &range));

  return ResOK;
}


/* ArenaFreeCBSDelete -- remove a block from free CBS, extending pool if necessary
 *
 * This is called from ChunkFinish in order to remove address space from
 * the arena.
 *
 * IMPORTANT: May only be called on whole chunk ranges, because we don't
 * deal with the case where the range is coalesced.  This restriction would
 * be easy to lift by extending the block pool on error, but doesn't happen,
 * so we can't test that path.
 */

void ArenaFreeCBSDelete(Arena arena, Addr base, Addr limit)
{
  RangeStruct range, oldRange;
  Res res;

  RangeInit(&range, base, limit);
  res = CBSDelete(&oldRange, ArenaFreeCBS(arena), &range);
  
  /* Shouldn't be any other kind of failure because we were only deleting
     a non-coalesced block.  See .chunk.no-coalesce and
     <code/cbs.c#.delete.alloc>. */
  AVER(res == ResOK);
}


static Res arenaAllocFromCBS(Tract *tractReturn, ZoneSet zones, Bool high,
                             Size size, Pool pool)
{
  Arena arena;
  RangeStruct range, oldRange;
  Chunk chunk;
  Bool b;
  Index baseIndex;
  Count pages;
  Res res;
  
  AVER(tractReturn != NULL);
  /* ZoneSet is arbitrary */
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  AVER(SizeIsAligned(size, arena->alignment));
  
  if (!arena->zoned)
    zones = ZoneSetUNIV;

  /* Step 1. Find a range of address space. */
  
  res = CBSFindInZones(&range, &oldRange, ArenaFreeCBS(arena),
                       size, zones, high);

  if (res == ResLIMIT) { /* found block, but couldn't store info */
    RangeStruct pageRange;
    res = arenaExtendCBSBlockPool(&pageRange, arena);
    if (res != ResOK) /* disastrously short on memory */
      return res;
    arenaExcludePage(arena, &pageRange);
    res = CBSFindInZones(&range, &oldRange, ArenaFreeCBS(arena),
                         size, zones, high);
    AVER(res != ResLIMIT);
  }

  if (res == ResFAIL) /* out of address space */
    return ResRESOURCE;

  AVER(res == ResOK); /* unexpected error from ZoneCBS */
  if (res != ResOK) /* defensive return */
    return res;
  
  /* Step 2. Make memory available in the address space range. */

  b = CHUNK_OF_ADDR(&chunk, arena, RangeBase(&range));
  AVER(b);
  AVER(RangeIsAligned(&range, ChunkPageSize(chunk)));
  baseIndex = INDEX_OF_ADDR(chunk, RangeBase(&range));
  pages = ChunkSizeToPages(chunk, RangeSize(&range));

  res = (*arena->class->pagesMarkAllocated)(arena, chunk, baseIndex, pages, pool);
  if (res != ResOK)
    goto failMark;

  arena->freeZones = ZoneSetDiff(arena->freeZones,
                                 ZoneSetOfRange(arena,
                                                RangeBase(&range),
                                                RangeLimit(&range)));

  *tractReturn = PageTract(ChunkPage(chunk, baseIndex));
  return ResOK;

failMark:
   {
     Res insertRes = arenaCBSInsert(&oldRange, arena, &range);
     AVER(insertRes == ResOK); /* We only just deleted it. */
     /* If the insert does fail, we lose some address space permanently. */
   }
   return res;
}


/* arenaAllocPolicy -- arena allocation policy implementation
 *
 * This is the code responsible for making decisions about where to allocate
 * memory.  Avoid distributing code for doing this elsewhere, so that policy
 * can be maintained and adjusted.
 *
 * TODO: This currently duplicates the policy from VMAllocPolicy in
 * //info.ravenbrook.com/project/mps/master/code/arenavm.c#36 in order
 * to avoid disruption to clients, but needs revision.
 */

static Res arenaAllocPolicy(Tract *tractReturn, Arena arena, SegPref pref,
                            Size size, Pool pool)
{
  Res res;
  Tract tract;
  ZoneSet zones, moreZones, evenMoreZones;

  AVER(tractReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  
  /* Don't attempt to allocate if doing so would definitely exceed the
     commit limit. */
  if (arena->spareCommitted < size) {
    Size necessaryCommitIncrease = size - arena->spareCommitted;
    if (arena->committed + necessaryCommitIncrease > arena->commitLimit
        || arena->committed + necessaryCommitIncrease < arena->committed) {
      return ResCOMMIT_LIMIT;
    }
  }

  /* Plan A: allocate from the free CBS in the requested zones */
  zones = ZoneSetDiff(pref->zones, pref->avoid);
  if (zones != ZoneSetEMPTY) {
    res = arenaAllocFromCBS(&tract, zones, pref->high, size, pool);
    if (res == ResOK)
      goto found;
  }

  /* Plan B: add free zones that aren't blacklisted */
  /* TODO: Pools without ambiguous roots might not care about the blacklist. */
  /* TODO: zones are precious and (currently) never deallocated, so we
     should consider extending the arena first if address space is plentiful.
     See also job003384. */
  moreZones = ZoneSetUnion(pref->zones, ZoneSetDiff(arena->freeZones, pref->avoid));
  if (moreZones != zones) {
    res = arenaAllocFromCBS(&tract, moreZones, pref->high, size, pool);
    if (res == ResOK)
      goto found;
  }
  
  /* Plan C: Extend the arena, then try A and B again. */
  if (moreZones != ZoneSetEMPTY) {
    res = arena->class->grow(arena, pref, size);
    if (res != ResOK)
      return res;
    if (zones != ZoneSetEMPTY) {
      res = arenaAllocFromCBS(&tract, zones, pref->high, size, pool);
      if (res == ResOK)
        goto found;
    }
    if (moreZones != zones) {
      zones = ZoneSetUnion(zones, ZoneSetDiff(arena->freeZones, pref->avoid));
      res = arenaAllocFromCBS(&tract, moreZones, pref->high, size, pool);
      if (res == ResOK)
        goto found;
    }
  }

  /* Plan D: add every zone that isn't blacklisted.  This might mix GC'd
     objects with those from other generations, causing the zone check
     to give false positives and slowing down the collector. */
  /* TODO: log an event for this */
  evenMoreZones = ZoneSetDiff(ZoneSetUNIV, pref->avoid);
  if (evenMoreZones != moreZones) {
    res = arenaAllocFromCBS(&tract, evenMoreZones, pref->high, size, pool);
    if (res == ResOK)
      goto found;
  }

  /* Last resort: try anywhere.  This might put GC'd objects in zones where
     common ambiguous bit patterns pin them down, causing the zone check
     to give even more false positives permanently, and possibly retaining
     garbage indefinitely. */
  res = arenaAllocFromCBS(&tract, ZoneSetUNIV, pref->high, size, pool);
  if (res == ResOK)
    goto found;

  /* Uh oh. */
  return res;

found:
  *tractReturn = tract;
  return ResOK;
}


/* ArenaAlloc -- allocate some tracts from the arena */

Res ArenaAlloc(Addr *baseReturn, SegPref pref, Size size, Pool pool,
               Bool withReservoirPermit)
{
  Res res;
  Arena arena;
  Addr base;
  Tract tract;
  Reservoir reservoir;

  AVER(baseReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  AVERT(Bool, withReservoirPermit);

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(size, arena->alignment));
  reservoir = ArenaReservoir(arena);
  AVERT(Reservoir, reservoir);

  if (pool != ReservoirPool(reservoir)) {
    res = ReservoirEnsureFull(reservoir);
    if (res != ResOK) {
      AVER(ResIsAllocFailure(res));
      if (!withReservoirPermit)
        return res;
    }
  }

  res = arenaAllocPolicy(&tract, arena, pref, size, pool);
  if (res != ResOK) {
    if (withReservoirPermit) {
      Res resRes = ReservoirWithdraw(&base, &tract, reservoir, size, pool);
      if (resRes != ResOK)
        goto allocFail;
    } else
      goto allocFail;
  }
  
  base = TractBase(tract);

  /* cache the tract - <design/arena/#tract.cache> */
  arena->lastTract = tract;
  arena->lastTractBase = base;

  EVENT5(ArenaAlloc, arena, tract, base, size, pool);

  *baseReturn = base;
  return ResOK;

allocFail:
   EVENT3(ArenaAllocFail, arena, size, pool); /* TODO: Should have res? */
   return res;
}


/* ArenaFree -- free some tracts to the arena */

void ArenaFree(Addr base, Size size, Pool pool)
{
  Arena arena;
  Addr limit;
  Reservoir reservoir;
  Res res;
  Addr wholeBase;
  Size wholeSize;
  RangeStruct range, oldRange;

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
  
  wholeBase = base;
  wholeSize = size;

  if (pool != ReservoirPool(reservoir)) {
    res = ReservoirEnsureFull(reservoir);
    if (res != ResOK) {
      AVER(ResIsAllocFailure(res));
      if (!ReservoirDeposit(reservoir, &base, &size))
        goto allDeposited;
    }
  }

  /* Just in case the shenanigans with the reservoir mucked this up. */
  AVER(limit == AddrAdd(base, size));

  RangeInit(&range, base, limit);

  arenaCBSInsertSteal(&oldRange, arena, &range); /* may update range */

  (*arena->class->free)(RangeBase(&range), RangeSize(&range), pool);

  /* Freeing memory might create spare pages, but not more than this. */
  CHECKL(arena->spareCommitted <= arena->spareCommitLimit);

allDeposited:
  EVENT3(ArenaFree, arena, wholeBase, wholeSize);
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
    Size excess = arena->spareCommitted - arena->spareCommitLimit;
    (void)arena->class->purgeSpare(arena, excess);
  }

  EVENT2(SpareCommitLimitSet, arena, limit);
  return;
}

/* Used by arenas which don't use spare committed memory */
Size ArenaNoPurgeSpare(Arena arena, Size size)
{
  AVERT(Arena, arena);
  UNUSED(size);
  return 0;
}


Res ArenaNoGrow(Arena arena, SegPref pref, Size size)
{
  AVERT(Arena, arena);
  AVERT(SegPref, pref);
  UNUSED(size);
  return ResRESOURCE;
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
      Size excess = committed - limit;
      (void)arena->class->purgeSpare(arena, excess);
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

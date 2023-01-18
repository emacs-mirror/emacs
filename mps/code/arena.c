/* arena.c: ARENA ALLOCATION FEATURES
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .sources: <design/arena> is the main design document.  */

#include "tract.h"
#include "poolmvff.h"
#include "mpm.h"
#include "cbs.h"
#include "bt.h"
#include "poolmfs.h"
#include "mpscmfs.h"


SRCID(arena, "$Id$");


#define ArenaControlPool(arena) MVFFPool(&(arena)->controlPoolStruct)
#define ArenaCBSBlockPool(arena) MFSPool(&(arena)->freeCBSBlockPoolStruct)
#define ArenaFreeLand(arena) CBSLand(&(arena)->freeLandStruct)


/* ArenaGrainSizeCheck -- check that size is a valid arena grain size */

Bool ArenaGrainSizeCheck(Size size)
{
  CHECKL(size > 0);
  /* <design/arena#.req.attr.block.align.min> */
  CHECKL(SizeIsAligned(size, MPS_PF_ALIGN));
  /* Grain size must be a power of 2 for the tract lookup and the
   * zones to work. */
  CHECKL(SizeIsP2(size));

  return TRUE;
}


/* Forward declarations */

static void ArenaTrivCompact(Arena arena, Trace trace);
static void arenaFreePage(Arena arena, Addr base, Pool pool);
static void arenaFreeLandFinish(Arena arena);
static Res ArenaAbsInit(Arena arena, Size grainSize, ArgList args);
static void ArenaAbsFinish(Inst inst);
static Res ArenaAbsDescribe(Inst inst, mps_lib_FILE *stream, Count depth);


static void ArenaNoFree(Addr base, Size size, Pool pool)
{
  UNUSED(base);
  UNUSED(size);
  UNUSED(pool);
  NOTREACHED;
}

static Res ArenaNoChunkInit(Chunk chunk, BootBlock boot)
{
  UNUSED(chunk);
  UNUSED(boot);
  NOTREACHED;
  return ResUNIMPL;
}

static void ArenaNoChunkFinish(Chunk chunk)
{
  UNUSED(chunk);
  NOTREACHED;
}

static Res ArenaNoPagesMarkAllocated(Arena arena, Chunk chunk,
                                     Index baseIndex, Count pages,
                                     Pool pool)
{
  UNUSED(arena);
  UNUSED(chunk);
  UNUSED(baseIndex);
  UNUSED(pages);
  UNUSED(pool);
  NOTREACHED;
  return ResUNIMPL;
}

static Bool ArenaNoChunkPageMapped(Chunk chunk, Index index)
{
  UNUSED(chunk);
  UNUSED(index);
  NOTREACHED;
  return FALSE;
}

static Res ArenaNoCreate(Arena *arenaReturn, ArgList args)
{
  UNUSED(arenaReturn);
  UNUSED(args);
  NOTREACHED;
  return ResUNIMPL;
}

static void ArenaNoDestroy(Arena arena)
{
  UNUSED(arena);
  NOTREACHED;
}

DEFINE_CLASS(Inst, ArenaClass, klass)
{
  INHERIT_CLASS(klass, ArenaClass, InstClass);
  AVERT(InstClass, klass);
}


/* AbstractArenaClass  -- The abstract arena class definition */

DEFINE_CLASS(Arena, AbstractArena, klass)
{
  INHERIT_CLASS(&klass->instClassStruct, AbstractArena, Inst);
  klass->instClassStruct.finish = ArenaAbsFinish;
  klass->instClassStruct.describe = ArenaAbsDescribe;
  klass->size = sizeof(ArenaStruct);
  klass->varargs = ArgTrivVarargs;
  klass->init = ArenaAbsInit;
  klass->create = ArenaNoCreate;
  klass->destroy = ArenaNoDestroy;
  klass->purgeSpare = ArenaNoPurgeSpare;
  klass->extend = ArenaNoExtend;
  klass->grow = ArenaNoGrow;
  klass->free = ArenaNoFree;
  klass->chunkInit = ArenaNoChunkInit;
  klass->chunkFinish = ArenaNoChunkFinish;
  klass->compact = ArenaTrivCompact;
  klass->pagesMarkAllocated = ArenaNoPagesMarkAllocated;
  klass->chunkPageMapped = ArenaNoChunkPageMapped;
  klass->sig = ArenaClassSig;
  AVERT(ArenaClass, klass);
}


/* ArenaClassCheck -- check the consistency of an arena class */

Bool ArenaClassCheck(ArenaClass klass)
{
  CHECKD(InstClass, &klass->instClassStruct);
  CHECKL(klass->size >= sizeof(ArenaStruct));
  CHECKL(FUNCHECK(klass->varargs));
  CHECKL(FUNCHECK(klass->init));
  CHECKL(FUNCHECK(klass->create));
  CHECKL(FUNCHECK(klass->destroy));
  CHECKL(FUNCHECK(klass->purgeSpare));
  CHECKL(FUNCHECK(klass->extend));
  CHECKL(FUNCHECK(klass->grow));
  CHECKL(FUNCHECK(klass->free));
  CHECKL(FUNCHECK(klass->chunkInit));
  CHECKL(FUNCHECK(klass->chunkFinish));
  CHECKL(FUNCHECK(klass->compact));
  CHECKL(FUNCHECK(klass->pagesMarkAllocated));
  CHECKL(FUNCHECK(klass->chunkPageMapped));

  /* Check that arena classes override sets of related methods. */
  CHECKL((klass->init == ArenaAbsInit)
         == (klass->instClassStruct.finish == ArenaAbsFinish));
  CHECKL((klass->create == ArenaNoCreate)
         == (klass->destroy == ArenaNoDestroy));
  CHECKL((klass->chunkInit == ArenaNoChunkInit)
         == (klass->chunkFinish == ArenaNoChunkFinish));

  CHECKS(ArenaClass, klass);
  return TRUE;
}


/* ArenaCheck -- check the arena */

Bool ArenaCheck(Arena arena)
{
  CHECKC(AbstractArena, arena);
  CHECKD(Globals, ArenaGlobals(arena));

  CHECKL(BoolCheck(arena->poolReady));
  if (arena->poolReady) { /* <design/arena#.pool.ready> */
    CHECKD(MVFF, &arena->controlPoolStruct);
  }

  /* .reserved.check: Would like to check that arena->committed <=
   * arena->reserved, but that isn't always true in the VM arena.
   * Memory is committed early on when VMChunkCreate calls vmArenaMap
   * (to provide a place for the chunk struct) but is not recorded as
   * reserved until ChunkInit calls ArenaChunkInsert.
   */
  CHECKL(arena->committed <= arena->commitLimit);
  CHECKL(arena->spareCommitted <= arena->committed);
  CHECKL(0.0 <= arena->spare);
  CHECKL(arena->spare <= 1.0);
  CHECKL(0.0 <= arena->pauseTime);

  CHECKL(arena->zoneShift == ZoneShiftUNSET
         || ShiftCheck(arena->zoneShift));
  CHECKL(ArenaGrainSizeCheck(arena->grainSize));

  /* Stripes can't be smaller than grains. */
  CHECKL(arena->zoneShift == ZoneShiftUNSET
         || ((Size)1 << arena->zoneShift) >= arena->grainSize);

  if (arena->lastTract == NULL) {
    CHECKL(arena->lastTractBase == (Addr)0);
  } else {
    CHECKL(TractBase(arena->lastTract) == arena->lastTractBase);
  }

  if (arena->primary != NULL) {
    CHECKD(Chunk, arena->primary);
  }
  CHECKD_NOSIG(Ring, ArenaChunkRing(arena));
  /* Can't use CHECKD_NOSIG because TreeEMPTY is NULL. */
  CHECKL(TreeCheck(ArenaChunkTree(arena)));
  /* TODO: check that the chunkRing and chunkTree have identical members */
  /* nothing to check for chunkSerial */

  CHECKL(LocusCheck(arena));

  CHECKL(BoolCheck(arena->hasFreeLand));
  if (arena->hasFreeLand)
    CHECKD(Land, ArenaFreeLand(arena));

  CHECKL(BoolCheck(arena->zoned));

  return TRUE;
}


/* ArenaAbsInit -- initialize the generic part of the arena */

static Res ArenaAbsInit(Arena arena, Size grainSize, ArgList args)
{
  Res res;
  Bool zoned = ARENA_DEFAULT_ZONED;
  Size commitLimit = ARENA_DEFAULT_COMMIT_LIMIT;
  double spare = ARENA_SPARE_DEFAULT;
  double pauseTime = ARENA_DEFAULT_PAUSE_TIME;
  mps_arg_s arg;

  AVER(arena != NULL);
  AVERT(ArenaGrainSize, grainSize);

  if (ArgPick(&arg, args, MPS_KEY_ARENA_ZONED))
    zoned = arg.val.b;
  if (ArgPick(&arg, args, MPS_KEY_COMMIT_LIMIT))
    commitLimit = arg.val.size;
  /* MPS_KEY_SPARE_COMMIT_LIMIT is deprecated */
  if (ArgPick(&arg, args, MPS_KEY_SPARE_COMMIT_LIMIT)) {
    if (0 < commitLimit && commitLimit <= arg.val.size)
      spare = (double)arg.val.size / (double)commitLimit;
    else
      spare = 1.0;
  }
  if (ArgPick(&arg, args, MPS_KEY_SPARE))
    spare = arg.val.d;
  if (ArgPick(&arg, args, MPS_KEY_PAUSE_TIME))
    pauseTime = arg.val.d;

  /* Superclass init */
  InstInit(CouldBeA(Inst, arena));

  arena->reserved = (Size)0;
  arena->committed = (Size)0;
  arena->commitLimit = commitLimit;
  arena->spareCommitted = (Size)0;
  arena->spare = spare;
  arena->pauseTime = pauseTime;
  arena->grainSize = grainSize;
  /* zoneShift must be overridden by arena class init */
  arena->zoneShift = ZoneShiftUNSET;
  arena->poolReady = FALSE;     /* <design/arena#.pool.ready> */
  arena->lastTract = NULL;
  arena->lastTractBase = NULL;
  arena->hasFreeLand = FALSE;
  arena->freeZones = ZoneSetUNIV;
  arena->zoned = zoned;

  arena->primary = NULL;
  RingInit(ArenaChunkRing(arena));
  arena->chunkTree = TreeEMPTY;
  arena->chunkSerial = (Serial)0;

  LocusInit(arena);

  res = GlobalsInit(ArenaGlobals(arena));
  if (res != ResOK)
    goto failGlobalsInit;

  SetClassOfPoly(arena, CLASS(AbstractArena));
  arena->sig = ArenaSig;
  AVERC(Arena, arena);

  /* Initialise a pool to hold the CBS blocks for the arena's free
   * land. This pool can't be allowed to extend itself using
   * ArenaAlloc because it is used to implement ArenaAlloc, so
   * MFSExtendSelf is set to FALSE. Failures to extend are handled
   * where the free land is used: see arenaFreeLandInsertExtend. */

  MPS_ARGS_BEGIN(piArgs) {
    MPS_ARGS_ADD(piArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(CBSZonedBlockStruct));
    MPS_ARGS_ADD(piArgs, MPS_KEY_EXTEND_BY, ArenaGrainSize(arena));
    MPS_ARGS_ADD(piArgs, MFSExtendSelf, FALSE);
    res = PoolInit(ArenaCBSBlockPool(arena), arena, PoolClassMFS(), piArgs);
  } MPS_ARGS_END(piArgs);
  AVER(res == ResOK); /* no allocation, no failure expected */
  if (res != ResOK)
    goto failMFSInit;

  EventLabelPointer(ArenaCBSBlockPool(arena), EventInternString("CBSBlock"));
  return ResOK;

failMFSInit:
  GlobalsFinish(ArenaGlobals(arena));
failGlobalsInit:
  InstFinish(MustBeA(Inst, arena));
  return res;
}


/* VM keys are defined here even though the code they apply to might
 * not be linked.  For example, MPS_KEY_VMW3_TOP_DOWN only applies to
 * vmw3.c.  The reason is that we want these keywords to be optional
 * even on the wrong platform, so that clients can write simple portable
 * code.  They should be free to pass MPS_KEY_VMW3_TOP_DOWN on other
 * platforms, knowing that it has no effect.  To do that, the key must
 * exist on all platforms. */

ARG_DEFINE_KEY(VMW3_TOP_DOWN, Bool);


/* ArenaCreate -- create the arena and call initializers */

ARG_DEFINE_KEY(ARENA_GRAIN_SIZE, Size);
ARG_DEFINE_KEY(ARENA_SIZE, Size);
ARG_DEFINE_KEY(ARENA_ZONED, Bool);
ARG_DEFINE_KEY(COMMIT_LIMIT, Size);
ARG_DEFINE_KEY(SPARE_COMMIT_LIMIT, Size);
ARG_DEFINE_KEY(PAUSE_TIME, double);

static Res arenaFreeLandInit(Arena arena)
{
  Res res;

  AVERT(Arena, arena);
  AVER(!arena->hasFreeLand);
  AVER(arena->primary != NULL);

  /* Initialise the free land. */
  MPS_ARGS_BEGIN(liArgs) {
    MPS_ARGS_ADD(liArgs, CBSBlockPool, ArenaCBSBlockPool(arena));
    res = LandInit(ArenaFreeLand(arena), CLASS(CBSZoned), arena,
                   ArenaGrainSize(arena), arena, liArgs);
  } MPS_ARGS_END(liArgs);
  AVER(res == ResOK); /* no allocation, no failure expected */
  if (res != ResOK)
    goto failLandInit;

  /* With the primary chunk initialised we can add page memory to the
   * free land that describes the free address space in the primary
   * chunk. */
  res = ArenaFreeLandInsert(arena,
                            PageIndexBase(arena->primary,
                                          arena->primary->allocBase),
                            arena->primary->limit);
  if (res != ResOK)
    goto failFreeLandInsert;

  arena->hasFreeLand = TRUE;
  return ResOK;

failFreeLandInsert:
  LandFinish(ArenaFreeLand(arena));
failLandInit:
  return res;
}

Res ArenaCreate(Arena *arenaReturn, ArenaClass klass, ArgList args)
{
  Arena arena;
  Res res;

  AVER(arenaReturn != NULL);
  AVERT(ArenaClass, klass);
  AVERT(ArgList, args);

  /* We must initialise the event subsystem very early, because event logging
     will start as soon as anything interesting happens and expect to write
     to the EventLast pointers. */
  EventInit();

  res = klass->create(&arena, args);
  if (res != ResOK)
    goto failInit;

  /* Zone shift must have been set up by klass->create() */
  AVER(ShiftCheck(arena->zoneShift));

  /* TODO: Consider how each of the stages below could be incorporated
     into arena initialization, rather than tacked on here. */

  /* Grain size must have been set up by klass->create() */
  if (ArenaGrainSize(arena) > ((Size)1 << arena->zoneShift)) {
    res = ResMEMORY; /* size was too small */
    goto failStripeSize;
  }

  res = arenaFreeLandInit(arena);
  if (res != ResOK)
    goto failFreeLandInit;

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
  arenaFreeLandFinish(arena);
failFreeLandInit:
failStripeSize:
  klass->destroy(arena);
failInit:
  return res;
}


/* ArenaAbsFinish -- finish the generic part of the arena */

static void ArenaAbsFinish(Inst inst)
{
  Arena arena = MustBeA(AbstractArena, inst);
  AVERC(Arena, arena);
  PoolFinish(ArenaCBSBlockPool(arena));
  arena->sig = SigInvalid;
  NextMethod(Inst, AbstractArena, finish)(inst);
  GlobalsFinish(ArenaGlobals(arena));
  LocusFinish(arena);
  RingFinish(ArenaChunkRing(arena));
  AVER(ArenaChunkTree(arena) == TreeEMPTY);
}


/* ArenaDestroy -- destroy the arena */

static void arenaMFSPageFreeVisitor(Pool pool, Addr base, Size size,
                                    void *closure)
{
  AVERT(Pool, pool);
  AVER(closure == UNUSED_POINTER);
  UNUSED(closure);
  UNUSED(size);
  AVER(size == ArenaGrainSize(PoolArena(pool)));
  arenaFreePage(PoolArena(pool), base, pool);
}

static void arenaFreeLandFinish(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->hasFreeLand);

  /* We're about to free the memory occupied by the free land, which
     contains a CBS.  We want to make sure that LandFinish doesn't try
     to check the CBS, so nuke it here.  TODO: LandReset? */
  arena->freeLandStruct.splayTreeStruct.root = TreeEMPTY;

  /* The CBS block pool can't free its own memory via ArenaFree because
   * that would use the free land. */
  MFSFinishExtents(ArenaCBSBlockPool(arena), arenaMFSPageFreeVisitor,
                   UNUSED_POINTER);

  arena->hasFreeLand = FALSE;
  LandFinish(ArenaFreeLand(arena));
}

void ArenaDestroy(Arena arena)
{
  AVERT(Arena, arena);

  GlobalsPrepareToDestroy(ArenaGlobals(arena));

  ControlFinish(arena);

  /* We must tear down the free land before the chunks, because pages
   * containing CBS blocks might be allocated in those chunks. */
  arenaFreeLandFinish(arena);

  /* Call class-specific destruction.  This will call ArenaAbsFinish. */
  Method(Arena, arena, destroy)(arena);

  EventFinish();
}


/* ControlInit -- initialize the control pool */

Res ControlInit(Arena arena)
{
  Res res;

  AVERT(Arena, arena);
  AVER(!arena->poolReady);
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, CONTROL_EXTEND_BY);
    res = PoolInit(ArenaControlPool(arena), arena,
                   PoolClassMVFF(), args);
  } MPS_ARGS_END(args);
  if (res != ResOK)
    return res;
  arena->poolReady = TRUE;      /* <design/arena#.pool.ready> */
  EventLabelPointer(&arena->controlPoolStruct, EventInternString("Control"));
  return ResOK;
}


/* ControlFinish -- finish the control pool */

void ControlFinish(Arena arena)
{
  AVERT(Arena, arena);
  AVER(arena->poolReady);
  arena->poolReady = FALSE;
  PoolFinish(ArenaControlPool(arena));
}


/* ArenaDescribe -- describe the arena */

static Res ArenaAbsDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  Arena arena = CouldBeA(AbstractArena, inst);
  Res res;

  if (!TESTC(AbstractArena, arena))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = InstDescribe(CouldBeA(Inst, arena), stream, depth);
  if (res != ResOK)
    return res;

  if (arena->poolReady) {
    res = WriteF(stream, depth + 2,
                 "controlPool $P\n", (WriteFP)&arena->controlPoolStruct,
                 NULL);
    if (res != ResOK)
      return res;
  }

  res = WriteF(stream, depth + 2,
               "reserved         $W\n", (WriteFW)arena->reserved,
               "committed        $W\n", (WriteFW)arena->committed,
               "commitLimit      $W\n", (WriteFW)arena->commitLimit,
               "spareCommitted   $W\n", (WriteFW)arena->spareCommitted,
               "spare            $D\n", (WriteFD)arena->spare,
               "zoneShift        $U\n", (WriteFU)arena->zoneShift,
               "grainSize        $W\n", (WriteFW)arena->grainSize,
               "lastTract        $P\n", (WriteFP)arena->lastTract,
               "lastTractBase    $P\n", (WriteFP)arena->lastTractBase,
               "primary          $P\n", (WriteFP)arena->primary,
               "hasFreeLand      $S\n", WriteFYesNo(arena->hasFreeLand),
               "freeZones        $B\n", (WriteFB)arena->freeZones,
               "zoned            $S\n", WriteFYesNo(arena->zoned),
               NULL);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth + 2,
               "droppedMessages $U$S\n", (WriteFU)arena->droppedMessages,
               (arena->droppedMessages == 0 ? "" : "  -- MESSAGES DROPPED!"),
               NULL);
  if (res != ResOK)
    return res;

  res = GlobalsDescribe(ArenaGlobals(arena), stream, depth + 2);
  if (res != ResOK)
    return res;

  return res;
}

Res ArenaDescribe(Arena arena, mps_lib_FILE *stream, Count depth)
{
  return Method(Inst, arena, describe)(MustBeA(Inst, arena), stream, depth);
}


/* arenaDescribeTractsInChunk -- describe the tracts in a chunk */

static Res arenaDescribeTractsInChunk(Chunk chunk, mps_lib_FILE *stream, Count depth)
{
  Res res;
  Index pi;

  if (!TESTT(Chunk, chunk))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream, depth, "Chunk [$P, $P) ($U) {\n",
               (WriteFP)chunk->base, (WriteFP)chunk->limit,
               (WriteFU)chunk->serial,
               NULL);
  if (res != ResOK)
    return res;

  for (pi = chunk->allocBase; pi < chunk->pages; ++pi) {
    if (BTGet(chunk->allocTable, pi)) {
      Tract tract = PageTract(ChunkPage(chunk, pi));
      res = WriteF(stream, depth + 2, "[$P, $P)",
                   (WriteFP)TractBase(tract),
                   (WriteFP)TractLimit(tract, ChunkArena(chunk)),
                   NULL);
      if (res != ResOK)
        return res;
      if (TractHasPool(tract)) {
        Pool pool = TractPool(tract);
        PoolClass poolClass = ClassOfPoly(Pool, pool);
        res = WriteF(stream, 0, " $P $U ($S)",
                     (WriteFP)pool,
                     (WriteFU)(pool->serial),
                     (WriteFS)ClassName(poolClass),
                     NULL);
        if (res != ResOK)
          return res;
      }
      res = WriteF(stream, 0, "\n", NULL);
      if (res != ResOK)
        return res;
    }
  }

  res = WriteF(stream, depth, "} Chunk [$P, $P)\n",
               (WriteFP)chunk->base, (WriteFP)chunk->limit,
               NULL);
  return res;
}


/* ArenaDescribeTracts -- describe all the tracts in the arena */

Res ArenaDescribeTracts(Arena arena, mps_lib_FILE *stream, Count depth)
{
  Ring node, next;
  Res res;

  if (!TESTT(Arena, arena))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  RING_FOR(node, ArenaChunkRing(arena), next) {
    Chunk chunk = RING_ELT(Chunk, arenaRing, node);
    res = arenaDescribeTractsInChunk(chunk, stream, depth);
    if (res != ResOK)
      return res;
  }

  return ResOK;
}


/* ControlAlloc -- allocate a small block directly from the control pool
 *
 * .arena.control-pool: Actually the block will be allocated from the
 * control pool, which is an MV pool embedded in the arena itself.
 *
 * .controlalloc.addr: In implementations where Addr is not compatible
 * with void* <design/type#.addr.use>, ControlAlloc must take care of
 * allocating so that the block can be addressed with a void*.  */

Res ControlAlloc(void **baseReturn, Arena arena, size_t size)
{
  Addr base;
  Res res;

  AVERT(Arena, arena);
  AVER(baseReturn != NULL);
  AVER(size > 0);
  AVER(arena->poolReady);

  res = PoolAlloc(&base, ArenaControlPool(arena), (Size)size);
  if (res != ResOK)
    return res;

  *baseReturn = (void *)base; /* see .controlalloc.addr */
  return ResOK;
}


/* ControlFree -- free a block allocated using ControlAlloc */

void ControlFree(Arena arena, void* base, size_t size)
{
  Pool pool;

  AVERT(Arena, arena);
  AVER(base != NULL);
  AVER(size > 0);
  AVER(arena->poolReady);

  pool = ArenaControlPool(arena);
  PoolFree(pool, (Addr)base, (Size)size);
}


/* ControlDescribe -- describe the arena's control pool */

Res ControlDescribe(Arena arena, mps_lib_FILE *stream, Count depth)
{
  Res res;

  if (!TESTT(Arena, arena))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = PoolDescribe(ArenaControlPool(arena), stream, depth);

  return res;
}


/* ArenaChunkInsert -- insert chunk into arena's chunk tree and ring,
 * update the total reserved address space, and set the primary chunk
 * if not already set.
 */

void ArenaChunkInsert(Arena arena, Chunk chunk)
{
  Bool inserted;
  Tree tree, updatedTree = NULL;

  AVERT(Arena, arena);
  AVERT(Chunk, chunk);
  tree = &chunk->chunkTree;

  inserted = TreeInsert(&updatedTree, ArenaChunkTree(arena),
                        tree, ChunkKey(tree), ChunkCompare);
  AVER(inserted);
  AVER(updatedTree);
  TreeBalance(&updatedTree);
  arena->chunkTree = updatedTree;
  RingAppend(ArenaChunkRing(arena), &chunk->arenaRing);

  arena->reserved += ChunkReserved(chunk);

  /* As part of the bootstrap, the first created chunk becomes the primary
     chunk.  This step allows ArenaFreeLandInsert to allocate pages. */
  if (arena->primary == NULL)
    arena->primary = chunk;
}


/* ArenaChunkRemoved -- chunk was removed from the arena and is being
 * finished, so update the total reserved address space, and unset the
 * primary chunk if necessary.
 */

void ArenaChunkRemoved(Arena arena, Chunk chunk)
{
  Size size;

  AVERT(Arena, arena);
  AVERT(Chunk, chunk);

  size = ChunkReserved(chunk);
  AVER(arena->reserved >= size);
  arena->reserved -= size;

  if (chunk == arena->primary) {
    /* The primary chunk must be the last chunk to be removed. */
    AVER(RingIsSingle(ArenaChunkRing(arena)));
    AVER(arena->reserved == 0);
    arena->primary = NULL;
  }
}


/* arenaAllocPage -- allocate one page from the arena
 *
 * This is a primitive allocator used to allocate pages for the arena
 * Land. It is called rarely and can use a simple search. It may not
 * use the Land or any pool, because it is used as part of the
 * bootstrap.  <design/bootstrap#.land.sol.alloc>.
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

  res = Method(Arena, arena, pagesMarkAllocated)(arena, chunk,
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

  AVER(baseReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Pool, pool);

  /* Favour the primary chunk, because pages allocated this way aren't
     currently freed, and we don't want to prevent chunks being destroyed. */
  /* TODO: Consider how the ArenaCBSBlockPool might free pages. */
  res = arenaAllocPageInChunk(baseReturn, arena->primary, pool);
  if (res != ResOK) {
    Ring node, next;
    RING_FOR(node, ArenaChunkRing(arena), next) {
      Chunk chunk = RING_ELT(Chunk, arenaRing, node);
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
  Method(Arena, arena, free)(base, ArenaGrainSize(arena), pool);
}


/* arenaExtendCBSBlockPool -- add a page of memory to the CBS block pool
 *
 * IMPORTANT: Must be followed by arenaExcludePage to ensure that the
 * page doesn't get allocated by ArenaAlloc.  See .insert.exclude.
 */

static Res arenaExtendCBSBlockPool(Range pageRangeReturn, Arena arena)
{
  Addr pageBase, pageLimit;
  Res res;

  res = arenaAllocPage(&pageBase, arena, ArenaCBSBlockPool(arena));
  if (res != ResOK)
    return res;
  pageLimit = AddrAdd(pageBase, ArenaGrainSize(arena));
  MFSExtend(ArenaCBSBlockPool(arena), pageBase, pageLimit);

  RangeInit(pageRangeReturn, pageBase, pageLimit);
  return ResOK;
}

/* arenaExcludePage -- exclude CBS block pool's page from free land
 *
 * Exclude the page we specially allocated for the CBS block pool
 * so that it doesn't get reallocated.
 */

static void arenaExcludePage(Arena arena, Range pageRange)
{
  RangeStruct oldRange;
  Res res;
  Land land = ArenaFreeLand(arena);

  res = LandDelete(&oldRange, land, pageRange);
  AVER(res == ResOK); /* we just gave memory to the Land */
}


/* arenaFreeLandInsertExtend -- add range to arena's free land, maybe
 * extending block pool
 *
 * The arena's free land can't get memory for its block pool in the
 * usual way (via ArenaAlloc), because it is the mechanism behind
 * ArenaAlloc! So we extend the block pool via a back door (see
 * arenaExtendCBSBlockPool).  <design/bootstrap#.land.sol.pool>.
 *
 * Only fails if it can't get a page for the block pool.
 */

static Res arenaFreeLandInsertExtend(Range rangeReturn, Arena arena,
                                     Range range)
{
  Res res;
  Land land;

  AVER(rangeReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Range, range);

  land = ArenaFreeLand(arena);
  res = LandInsert(rangeReturn, land, range);

  if (res == ResLIMIT) { /* CBS block pool ran out of blocks */
    RangeStruct pageRange;
    res = arenaExtendCBSBlockPool(&pageRange, arena);
    if (res != ResOK)
      return res;
    /* .insert.exclude: Must insert before exclude so that we can
       bootstrap when the zoned CBS is empty. */
    res = LandInsert(rangeReturn, land, range);
    AVER(res == ResOK); /* we just gave memory to the CBS block pool */
    arenaExcludePage(arena, &pageRange);
  }

  return ResOK;
}


/* ArenaFreeLandInsert -- add range to arena's free land, maybe extending
 * block pool
 *
 * The inserted block of address space may not abut any existing block.
 * This restriction ensures that we don't coalesce chunks and allocate
 * object across the boundary, preventing chunk deletion.
 */

Res ArenaFreeLandInsert(Arena arena, Addr base, Addr limit)
{
  RangeStruct range, oldRange;
  Res res;

  AVERT(Arena, arena);
  AVER(base < limit);

  RangeInit(&range, base, limit);
  res = arenaFreeLandInsertExtend(&oldRange, arena, &range);
  if (res != ResOK)
    return res;

  /* .chunk.no-coalesce: Make sure it didn't coalesce. We don't want
     chunks to coalesce so that there are no chunk-crossing
     allocations that would prevent chunks being destroyed. See
     <code/tract.c#chunk.at.base> for the mechanism that ensures that
     chunks never coalesce. */
  AVER(RangesEqual(&oldRange, &range));

  return ResOK;
}


/* ArenaFreeLandDelete -- remove range from arena's free land if
 * possible without extending the block pool
 */

Res ArenaFreeLandDelete(Arena arena, Addr base, Addr limit)
{
  RangeStruct range, oldRange;
  Res res;
  Land land;

  RangeInit(&range, base, limit);
  land = ArenaFreeLand(arena);
  res = LandDelete(&oldRange, land, &range);

  return res;
}


/* ArenaFreeLandAlloc -- allocate a continguous range of tracts of
 * size bytes from the arena's free land.
 *
 * size, zones, and high are as for LandFindInZones.
 *
 * If successful, mark the allocated tracts as belonging to pool, set
 * *tractReturn to point to the first tract in the range, and return
 * ResOK.
 */

Res ArenaFreeLandAlloc(Tract *tractReturn, Arena arena, ZoneSet zones,
                       Bool high, Size size, Pool pool)
{
  RangeStruct range, oldRange;
  Chunk chunk = NULL; /* suppress uninit warning */
  Bool found, b;
  Index baseIndex;
  Count pages;
  Res res;
  Land land;

  AVER(tractReturn != NULL);
  AVERT(Arena, arena);
  /* ZoneSet is arbitrary */
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  AVER(arena == PoolArena(pool));
  AVER(SizeIsArenaGrains(size, arena));

  if (!arena->zoned)
    zones = ZoneSetUNIV;

  /* Step 1. Find a range of address space. */

  land = ArenaFreeLand(arena);
  res = LandFindInZones(&found, &range, &oldRange, land, size, zones, high);

  if (res == ResLIMIT) { /* found block, but couldn't store info */
    RangeStruct pageRange;
    res = arenaExtendCBSBlockPool(&pageRange, arena);
    if (res != ResOK) /* disastrously short on memory */
      return res;
    arenaExcludePage(arena, &pageRange);
    res = LandFindInZones(&found, &range, &oldRange, land, size, zones, high);
    AVER(res != ResLIMIT);
  }

  AVER(res == ResOK); /* unexpected error from ZoneCBS */
  if (res != ResOK) /* defensive return */
    return res;

  if (!found) /* out of address space */
    return ResRESOURCE;

  /* Step 2. Make memory available in the address space range. */

  b = ChunkOfAddr(&chunk, arena, RangeBase(&range));
  AVER(b);
  AVER(RangeIsAligned(&range, ChunkPageSize(chunk)));
  baseIndex = INDEX_OF_ADDR(chunk, RangeBase(&range));
  pages = ChunkSizeToPages(chunk, RangeSize(&range));

  res = Method(Arena, arena, pagesMarkAllocated)(arena, chunk, baseIndex, pages, pool);
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
     Res insertRes = arenaFreeLandInsertExtend(&oldRange, arena, &range);
     AVER(insertRes == ResOK); /* We only just deleted it. */
     /* If the insert does fail, we lose some address space permanently. */
   }
   return res;
}


/* ArenaAlloc -- allocate some tracts from the arena */

Res ArenaAlloc(Addr *baseReturn, LocusPref pref, Size size, Pool pool)
{
  Res res;
  Arena arena;
  Addr base;
  Tract tract;

  AVER(baseReturn != NULL);
  AVERT(LocusPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  AVER(SizeIsArenaGrains(size, arena));

  res = PolicyAlloc(&tract, arena, pref, size, pool);
  if (res != ResOK)
    goto allocFail;

  base = TractBase(tract);

  /* cache the tract - <design/arena#.tract.cache> */
  arena->lastTract = tract;
  arena->lastTractBase = base;

  EVENT5(ArenaAlloc, arena, tract, base, size, pool);

  *baseReturn = base;
  return ResOK;

allocFail:
   EVENT4(ArenaAllocFail, arena, size, pool, (unsigned)res);
   return res;
}


/* ArenaFree -- free some tracts to the arena */

void ArenaFree(Addr base, Size size, Pool pool)
{
  Arena arena;
  RangeStruct range, oldRange;
  Res res;

  AVERT(Pool, pool);
  AVER(base != NULL);
  AVER(size > (Size)0);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  AVER(AddrIsArenaGrain(base, arena));
  AVER(SizeIsArenaGrains(size, arena));

  RangeInitSize(&range, base, size);

  /* uncache the tract if in range - <design/arena#.tract.uncache> */
  if (base <= arena->lastTractBase && arena->lastTractBase < RangeLimit(&range))
  {
    arena->lastTract = NULL;
    arena->lastTractBase = (Addr)0;
  }

  res = arenaFreeLandInsertExtend(&oldRange, arena, &range);
  if (res != ResOK) {
    Land land = ArenaFreeLand(arena);
    res = LandInsertSteal(&oldRange, land, &range); /* may update range */
    AVER(res == ResOK);
    if (RangeIsEmpty(&range))
      goto done;
  }
  Method(Arena, arena, free)(RangeBase(&range), RangeSize(&range), pool);

done:
  /* Freeing memory might create spare pages, but not more than this. */
  AVER(arena->spareCommitted <= ArenaSpareCommitLimit(arena));

  EVENT4(ArenaFree, arena, base, size, pool);
}


Size ArenaReserved(Arena arena)
{
  AVERT(Arena, arena);
  return arena->reserved;
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

double ArenaSpare(Arena arena)
{
  AVERT(Arena, arena);
  return arena->spare;
}

void ArenaSetSpare(Arena arena, double spare)
{
  Size spareMax;

  AVERT(Arena, arena);
  AVER(0.0 <= spare);
  AVER(spare <= 1.0);

  arena->spare = spare;
  EVENT2(ArenaSetSpare, arena, spare);

  spareMax = ArenaSpareCommitLimit(arena);
  if (arena->spareCommitted > spareMax) {
    Size excess = arena->spareCommitted - spareMax;
    (void)Method(Arena, arena, purgeSpare)(arena, excess);
  }
}

double ArenaPauseTime(Arena arena)
{
  AVERT(Arena, arena);
  return arena->pauseTime;
}

void ArenaSetPauseTime(Arena arena, double pauseTime)
{
  AVERT(Arena, arena);
  AVER(0.0 <= pauseTime);
  arena->pauseTime = pauseTime;
  EVENT2(PauseTimeSet, arena, pauseTime);
}

/* Used by arenas which don't use spare committed memory */
Size ArenaNoPurgeSpare(Arena arena, Size size)
{
  AVERT(Arena, arena);
  UNUSED(size);
  return 0;
}


Res ArenaNoGrow(Arena arena, LocusPref pref, Size size)
{
  AVERT(Arena, arena);
  AVERT(LocusPref, pref);
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
      (void)Method(Arena, arena, purgeSpare)(arena, excess);
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
  EVENT3(CommitLimitSet, arena, limit, (unsigned)res);
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

  AVER(sSwap >= arena->committed);
  return sSwap - arena->committed + arena->spareCommitted;
}


/* ArenaCollectable -- return estimate of collectable memory in arena */

Size ArenaCollectable(Arena arena)
{
  /* Conservative estimate -- see job003929. */
  Size committed = ArenaCommitted(arena);
  Size spareCommitted = ArenaSpareCommitted(arena);
  AVER(committed >= spareCommitted);
  return committed - spareCommitted;
}


/* ArenaAccumulateTime -- accumulate time spent tracing */

void ArenaAccumulateTime(Arena arena, Clock start, Clock end)
{
  AVERT(Arena, arena);
  AVER(start <= end);
  arena->tracedTime += (double)(end - start) / (double)ClocksPerSec();
}


/* ArenaExtend -- Add a new chunk in the arena */

Res ArenaExtend(Arena arena, Addr base, Size size)
{
  Res res;

  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > 0);

  res = Method(Arena, arena, extend)(arena, base, size);
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
  Method(Arena, arena, compact)(arena, trace);
}

static void ArenaTrivCompact(Arena arena, Trace trace)
{
  UNUSED(arena);
  UNUSED(trace);
}


/* Has Addr */

Bool ArenaHasAddr(Arena arena, Addr addr)
{
  Tract tract;

  AVERT(Arena, arena);
  return TractOfAddr(&tract, arena, addr);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
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

/* poolmvff.c: First Fit Manual Variable Pool
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * **** RESTRICTION: This pool may not allocate from the arena control
 *                   pool, since it is used to implement that pool.
 *
 * .purpose: This is a pool class for manually managed objects of
 * variable size where address-ordered first (or last) fit is an
 * appropriate policy.
 *
 * .design: <design/poolmvff>
 *
 * .critical: In manual-allocation-bound programs using MVFF, many of
 * these functions are on the critical paths via mps_alloc (and then
 * PoolAlloc, MVFFAlloc) and mps_free (and then PoolFree, MVFFFree).
 */

#include "cbs.h"
#include "dbgpool.h"
#include "failover.h"
#include "freelist.h"
#include "mpm.h"
#include "mpscmvff.h"
#include "poolmvff.h"
#include "mpscmfs.h"
#include "mpscmv.h"
#include "poolmfs.h"

SRCID(poolmvff, "$Id$");


/* Note: MVFFStruct is declared in mpmst.h rather than here because it
   is the control pool and is inlined in the arena globals. */

typedef MVFF MVFFPool;
#define MVFFPoolCheck MVFFCheck
DECLARE_CLASS(Pool, MVFFPool, AbstractBufferPool);
DECLARE_CLASS(Pool, MVFFDebugPool, MVFFPool);


#define PoolMVFF(pool)     PARENT(MVFFStruct, poolStruct, pool)
#define MVFFTotalLand(mvff)  (&(mvff)->totalCBSStruct.landStruct)
#define MVFFFreePrimary(mvff)   (&(mvff)->freeCBSStruct.landStruct)
#define MVFFFreeSecondary(mvff)  FreelistLand(&(mvff)->flStruct)
#define MVFFFreeLand(mvff)  FailoverLand(&(mvff)->foStruct)
#define MVFFLocusPref(mvff) (&(mvff)->locusPrefStruct)
#define MVFFBlockPool(mvff) MFSPool(&(mvff)->cbsBlockPoolStruct)


/* MVFFDebug -- MVFFDebug class */

typedef struct MVFFDebugStruct {
  MVFFStruct mvffStruct;         /* MVFF structure */
  PoolDebugMixinStruct debug;    /* debug mixin */
} MVFFDebugStruct;

typedef MVFFDebugStruct *MVFFDebug;


#define MVFF2MVFFDebug(mvff) PARENT(MVFFDebugStruct, mvffStruct, mvff)
#define MVFFDebug2MVFF(mvffd) (&((mvffd)->mvffStruct))


/* MVFFReduce -- return memory to the arena
 *
 * This is usually called immediately after inserting a range into the
 * MVFFFreeLand. (But not in all cases: see MVFFExtend.)
 */
static void MVFFReduce(MVFF mvff)
{
  Arena arena;
  Size freeSize, freeLimit, targetFree;
  RangeStruct freeRange, oldFreeRange;
  Align grainSize;
  Land totalLand, freeLand;

  AVERT_CRITICAL(MVFF, mvff);
  arena = PoolArena(MVFFPool(mvff));

  /* Try to return memory when the amount of free memory exceeds a
     threshold fraction of the total memory. */

  totalLand = MVFFTotalLand(mvff);
  freeLimit = (Size)(LandSize(totalLand) * mvff->spare);
  freeLand = MVFFFreeLand(mvff);
  freeSize = LandSize(freeLand);
  if (freeSize < freeLimit)
    return;

  /* NOTE: Memory is returned to the arena in the smallest units
     possible (arena grains). There's a possibility that this could
     lead to fragmentation in the arena (because allocation is in
     multiples of mvff->extendBy). If so, try setting grainSize =
     mvff->extendBy here. */

  grainSize = ArenaGrainSize(arena);

  /* For hysteresis, return only a proportion of the free memory. */

  targetFree = freeLimit / 2;

  /* Each time around this loop we either break, or we free at least
     one grain back to the arena, thus ensuring that eventually the
     loop will terminate */

  /* NOTE: If this code becomes very hot, then the test of whether there's
     a large free block in the CBS could be inlined, since it's a property
     stored at the root node. */

  while (freeSize > targetFree
         && LandFindLargest(&freeRange, &oldFreeRange, freeLand,
                            grainSize, FindDeleteNONE))
  {
    RangeStruct grainRange, oldRange;
    Size size;
    Res res;
    Addr base, limit;

    AVER(RangesEqual(&freeRange, &oldFreeRange));

    base = AddrAlignUp(RangeBase(&freeRange), grainSize);
    limit = AddrAlignDown(RangeLimit(&freeRange), grainSize);
    
    /* Give up if this block doesn't contain a whole aligned grain,
       even though smaller better-aligned blocks might, because
       LandFindLargest won't be able to find those anyway. */
    if (base >= limit)
      break;

    size = AddrOffset(base, limit);

    /* Don't return (much) more than we need to. */
    if (size > freeSize - targetFree)
      size = SizeAlignUp(freeSize - targetFree, grainSize);

    /* Calculate the range of grains we can return to the arena near the
       top end of the free memory (because we're first fit). */
    RangeInit(&grainRange, AddrSub(limit, size), limit);
    AVER(!RangeIsEmpty(&grainRange));
    AVER(RangesNest(&freeRange, &grainRange));
    AVER(RangeIsAligned(&grainRange, grainSize));

    /* Delete the range from the free list before attempting to delete
       it from the total allocated memory, so that we don't have
       dangling blocks in the free list, even for a moment. If we fail
       to delete from the TotalCBS we add back to the free list, which
       can't fail. */

    res = LandDelete(&oldRange, freeLand, &grainRange);
    if (res != ResOK)
      break;
    freeSize -= RangeSize(&grainRange);
    AVER(freeSize == LandSize(freeLand));

    res = LandDelete(&oldRange, totalLand, &grainRange);
    if (res != ResOK) {
      RangeStruct coalescedRange;
      res = LandInsert(&coalescedRange, freeLand, &grainRange);
      AVER(res == ResOK);
      break;
    }

    ArenaFree(RangeBase(&grainRange), RangeSize(&grainRange), MVFFPool(mvff));
  }
}


/* MVFFExtend -- allocate a new range from the arena
 *
 * Allocate a new range from the arena of at least the specified
 * size. The specified size should be pool-aligned. Add it to the
 * allocated and free lists.
 */
static Res MVFFExtend(Range rangeReturn, MVFF mvff, Size size)
{
  Pool pool;
  Arena arena;
  Size allocSize;
  RangeStruct range, coalescedRange;
  Addr base;
  Res res;
  Land totalLand, freeLand;

  AVERT(MVFF, mvff);
  AVER(size > 0);

  pool = MVFFPool(mvff);
  arena = PoolArena(pool);

  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* Use extendBy unless it's too small (see */
  /* <design/poolmvff/#design.acquire-size>). */
  if (size <= mvff->extendBy)
    allocSize = mvff->extendBy;
  else
    allocSize = size;

  allocSize = SizeArenaGrains(allocSize, arena);

  res = ArenaAlloc(&base, MVFFLocusPref(mvff), allocSize, pool);
  if (res != ResOK) {
    /* try again with a range just large enough for object */
    /* see <design/poolmvff/#design.seg-fail> */
    allocSize = SizeArenaGrains(size, arena);
    res = ArenaAlloc(&base, MVFFLocusPref(mvff), allocSize, pool);
    if (res != ResOK)
      return res;
  }

  RangeInitSize(&range, base, allocSize);
  totalLand = MVFFTotalLand(mvff);
  res = LandInsert(&coalescedRange, totalLand, &range);
  if (res != ResOK) {
    /* Can't record this memory, so return it to the arena and fail. */
    ArenaFree(base, allocSize, pool);
    return res;
  }

  DebugPoolFreeSplat(pool, RangeBase(&range), RangeLimit(&range));
  freeLand = MVFFFreeLand(mvff);
  res = LandInsert(rangeReturn, freeLand, &range);
  /* Insertion must succeed because it fails over to a Freelist. */
  AVER(res == ResOK);

  /* Don't call MVFFReduce; that would be silly. */

  return ResOK;
}


/* mvffFindFree -- find a suitable free block or add one
 *
 * Finds a free block of the given (pool aligned) size, using the
 * policy (first fit, last fit, or worst fit) specified by findMethod
 * and findDelete.
 *
 * If there is no suitable free block, try extending the pool.
 */
static Res mvffFindFree(Range rangeReturn, MVFF mvff, Size size,
                        LandFindMethod findMethod, FindDelete findDelete)
{
  Bool found;
  RangeStruct oldRange;
  Land land;

  AVER_CRITICAL(rangeReturn != NULL);
  AVERT_CRITICAL(MVFF, mvff);
  AVER_CRITICAL(size > 0);
  AVER_CRITICAL(SizeIsAligned(size, PoolAlignment(MVFFPool(mvff))));
  AVER_CRITICAL(FUNCHECK(findMethod));
  AVERT_CRITICAL(FindDelete, findDelete);

  land = MVFFFreeLand(mvff);
  found = (*findMethod)(rangeReturn, &oldRange, land, size, findDelete);
  if (!found) {
    RangeStruct newRange;
    Res res;
    res = MVFFExtend(&newRange, mvff, size);
    if (res != ResOK)
      return res;
    found = (*findMethod)(rangeReturn, &oldRange, land, size, findDelete);

    /* We know that the found range must intersect the newly added
     * range. But it doesn't necessarily lie entirely within it. */
    AVER_CRITICAL(found);
    AVER_CRITICAL(RangesOverlap(rangeReturn, &newRange));
  }
  AVER_CRITICAL(found);

  return ResOK;
}


/* MVFFAlloc -- Allocate a block */

static Res MVFFAlloc(Addr *aReturn, Pool pool, Size size)
{
  Res res;
  MVFF mvff;
  RangeStruct range;
  LandFindMethod findMethod;
  FindDelete findDelete;

  AVER_CRITICAL(aReturn != NULL);
  AVERT_CRITICAL(Pool, pool);
  mvff = PoolMVFF(pool);
  AVERT_CRITICAL(MVFF, mvff);
  AVER_CRITICAL(size > 0);

  size = SizeAlignUp(size, PoolAlignment(pool));
  findMethod = mvff->firstFit ? LandFindFirst : LandFindLast;
  findDelete = mvff->slotHigh ? FindDeleteHIGH : FindDeleteLOW;

  res = mvffFindFree(&range, mvff, size, findMethod, findDelete);
  if (res != ResOK)
    return res;

  AVER_CRITICAL(RangeSize(&range) == size);
  *aReturn = RangeBase(&range);
  return ResOK;
}


/* MVFFFree -- free the given block */

static void MVFFFree(Pool pool, Addr old, Size size)
{
  Res res;
  RangeStruct range, coalescedRange;
  MVFF mvff;
  Land freeLand;

  AVERT_CRITICAL(Pool, pool);
  mvff = PoolMVFF(pool);
  AVERT_CRITICAL(MVFF, mvff);

  AVER_CRITICAL(old != (Addr)0);
  AVER_CRITICAL(AddrIsAligned(old, PoolAlignment(pool)));
  AVER_CRITICAL(size > 0);

  RangeInitSize(&range, old, SizeAlignUp(size, PoolAlignment(pool)));
  freeLand = MVFFFreeLand(mvff);
  res = LandInsert(&coalescedRange, freeLand, &range);
  /* Insertion must succeed because it fails over to a Freelist. */
  AVER_CRITICAL(res == ResOK);
  MVFFReduce(mvff);
}


/* MVFFBufferFill -- Fill the buffer
 *
 * Fill it with the largest block we can find. This is worst-fit
 * allocation policy; see <design/poolmvff/#over.buffer>.
 */
static Res MVFFBufferFill(Addr *baseReturn, Addr *limitReturn,
                          Pool pool, Buffer buffer, Size size)
{
  Res res;
  MVFF mvff;
  RangeStruct range;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  mvff = PoolMVFF(pool);
  AVERT(MVFF, mvff);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  res = mvffFindFree(&range, mvff, size, LandFindLargest, FindDeleteENTIRE);
  if (res != ResOK)
    return res;
  AVER(RangeSize(&range) >= size);

  *baseReturn = RangeBase(&range);
  *limitReturn = RangeLimit(&range);
  return ResOK;
}


/* MVFFVarargs -- decode obsolete varargs */

static void MVFFVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_EXTEND_BY;
  args[0].val.size = va_arg(varargs, Size);
  args[1].key = MPS_KEY_MEAN_SIZE;
  args[1].val.size = va_arg(varargs, Size);
  args[2].key = MPS_KEY_ALIGN;
  args[2].val.align = va_arg(varargs, Size); /* promoted type */
  args[3].key = MPS_KEY_MVFF_SLOT_HIGH;
  args[3].val.b = va_arg(varargs, Bool);
  args[4].key = MPS_KEY_MVFF_ARENA_HIGH;
  args[4].val.b = va_arg(varargs, Bool);
  args[5].key = MPS_KEY_MVFF_FIRST_FIT;
  args[5].val.b = va_arg(varargs, Bool);
  args[6].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}

static void MVFFDebugVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_POOL_DEBUG_OPTIONS;
  args[0].val.pool_debug_options = va_arg(varargs, mps_pool_debug_option_s *);
  MVFFVarargs(args + 1, varargs);
}


/* MVFFInit -- initialize method for MVFF */

ARG_DEFINE_KEY(MVFF_SLOT_HIGH, Bool);
ARG_DEFINE_KEY(MVFF_ARENA_HIGH, Bool);
ARG_DEFINE_KEY(MVFF_FIRST_FIT, Bool);

static Res MVFFInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  Size extendBy = MVFF_EXTEND_BY_DEFAULT;
  Size avgSize = MVFF_AVG_SIZE_DEFAULT;
  Align align = MVFF_ALIGN_DEFAULT;
  Bool slotHigh = MVFF_SLOT_HIGH_DEFAULT;
  Bool arenaHigh = MVFF_ARENA_HIGH_DEFAULT;
  Bool firstFit = MVFF_FIRST_FIT_DEFAULT;
  double spare = MVFF_SPARE_DEFAULT;
  MVFF mvff;
  Res res;
  ArgStruct arg;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);
  AVERC(PoolClass, klass);

  /* .arg: class-specific additional arguments; see */
  /* <design/poolmvff/#method.init> */
  /* .arg.check: we do the same checks here and in MVFFCheck */
  /* except for arenaHigh, which is stored only in the locusPref. */
  
  if (ArgPick(&arg, args, MPS_KEY_EXTEND_BY))
    extendBy = arg.val.size;
  
  if (ArgPick(&arg, args, MPS_KEY_MEAN_SIZE))
    avgSize = arg.val.size;
  
  if (ArgPick(&arg, args, MPS_KEY_ALIGN))
    align = arg.val.align;

  if (ArgPick(&arg, args, MPS_KEY_SPARE))
    spare = arg.val.d;

  if (ArgPick(&arg, args, MPS_KEY_MVFF_SLOT_HIGH))
    slotHigh = arg.val.b;
  
  if (ArgPick(&arg, args, MPS_KEY_MVFF_ARENA_HIGH))
    arenaHigh = arg.val.b;
  
  if (ArgPick(&arg, args, MPS_KEY_MVFF_FIRST_FIT))
    firstFit = arg.val.b;

  AVER(extendBy > 0);           /* .arg.check */
  AVER(avgSize > 0);            /* .arg.check */
  AVER(avgSize <= extendBy);    /* .arg.check */
  AVER(spare >= 0.0);           /* .arg.check */
  AVER(spare <= 1.0);           /* .arg.check */
  AVERT(Align, align);
  /* This restriction on the alignment is necessary because of the use
     of a Freelist to store the free address ranges in low-memory
     situations. <design/freelist/#impl.grain.align>. */
  AVER(AlignIsAligned(align, FreelistMinimumAlignment));
  AVER(align <= ArenaGrainSize(arena));
  AVERT(Bool, slotHigh);
  AVERT(Bool, arenaHigh);
  AVERT(Bool, firstFit);

  res = NextMethod(Pool, MVFFPool, init)(pool, arena, klass, args);
  if (res != ResOK)
    goto failNextInit;
  mvff = CouldBeA(MVFFPool, pool);

  mvff->extendBy = extendBy;
  if (extendBy < ArenaGrainSize(arena))
    mvff->extendBy = ArenaGrainSize(arena);
  mvff->avgSize = avgSize;
  pool->alignment = align;
  pool->alignShift = SizeLog2(pool->alignment);
  mvff->slotHigh = slotHigh;
  mvff->firstFit = firstFit;
  mvff->spare = spare;

  LocusPrefInit(MVFFLocusPref(mvff));
  LocusPrefExpress(MVFFLocusPref(mvff),
                   arenaHigh ? LocusPrefHIGH : LocusPrefLOW, NULL);

  /* An MFS pool is explicitly initialised for the two CBSs partly to
   * share space, but mostly to avoid a call to PoolCreate, so that
   * MVFF can be used during arena bootstrap as the control pool. */

  MPS_ARGS_BEGIN(piArgs) {
    MPS_ARGS_ADD(piArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(CBSFastBlockStruct));
    res = PoolInit(MVFFBlockPool(mvff), arena, PoolClassMFS(), piArgs);
  } MPS_ARGS_END(piArgs);
  if (res != ResOK)
    goto failBlockPoolInit;

  MPS_ARGS_BEGIN(liArgs) {
    MPS_ARGS_ADD(liArgs, CBSBlockPool, MVFFBlockPool(mvff));
    res = LandInit(MVFFTotalLand(mvff), CLASS(CBSFast), arena, align,
                   mvff, liArgs);
  } MPS_ARGS_END(liArgs);
  if (res != ResOK)
    goto failTotalLandInit;

  MPS_ARGS_BEGIN(liArgs) {
    MPS_ARGS_ADD(liArgs, CBSBlockPool, MVFFBlockPool(mvff));
    res = LandInit(MVFFFreePrimary(mvff), CLASS(CBSFast), arena, align,
                   mvff, liArgs);
  } MPS_ARGS_END(liArgs);
  if (res != ResOK)
    goto failFreePrimaryInit;

  res = LandInit(MVFFFreeSecondary(mvff), CLASS(Freelist), arena, align,
                 mvff, mps_args_none);
  if (res != ResOK)
    goto failFreeSecondaryInit;

  MPS_ARGS_BEGIN(foArgs) {
    MPS_ARGS_ADD(foArgs, FailoverPrimary, MVFFFreePrimary(mvff));
    MPS_ARGS_ADD(foArgs, FailoverSecondary, MVFFFreeSecondary(mvff));
    res = LandInit(MVFFFreeLand(mvff), CLASS(Failover), arena, align,
                   mvff, foArgs);
  } MPS_ARGS_END(foArgs);
  if (res != ResOK)
    goto failFreeLandInit;

  SetClassOfPoly(pool, CLASS(MVFFPool));
  mvff->sig = MVFFSig;
  AVERC(MVFFPool, mvff);
  
  EVENT8(PoolInitMVFF, pool, arena, extendBy, avgSize, align,
         BOOLOF(slotHigh), BOOLOF(arenaHigh), BOOLOF(firstFit));

  return ResOK;

failFreeLandInit:
  LandFinish(MVFFFreeSecondary(mvff));
failFreeSecondaryInit:
  LandFinish(MVFFFreePrimary(mvff));
failFreePrimaryInit:
  LandFinish(MVFFTotalLand(mvff));
failTotalLandInit:
  PoolFinish(MVFFBlockPool(mvff));
failBlockPoolInit:
  NextMethod(Inst, MVFFPool, finish)(MustBeA(Inst, pool));
failNextInit:
  AVER(res != ResOK);
  return res;
}


/* MVFFFinish -- finish method for MVFF */

static Bool mvffFinishVisitor(Bool *deleteReturn, Land land, Range range,
                              void *closure)
{
  Pool pool;

  AVER(deleteReturn != NULL);
  AVERT(Land, land);
  AVERT(Range, range);
  AVER(closure != NULL);
  pool = closure;
  AVERT(Pool, pool);

  ArenaFree(RangeBase(range), RangeSize(range), pool);
  *deleteReturn = TRUE;
  return TRUE;
}

static void MVFFFinish(Inst inst)
{
  Pool pool = MustBeA(AbstractPool, inst);
  MVFF mvff = MustBeA(MVFFPool, pool);
  Bool b;
  Land totalLand;

  AVERT(MVFF, mvff);
  mvff->sig = SigInvalid;

  totalLand = MVFFTotalLand(mvff);
  b = LandIterateAndDelete(totalLand, mvffFinishVisitor, pool);
  AVER(b);
  AVER(LandSize(totalLand) == 0);

  LandFinish(MVFFFreeLand(mvff));
  LandFinish(MVFFFreeSecondary(mvff));
  LandFinish(MVFFFreePrimary(mvff));
  LandFinish(totalLand);
  PoolFinish(MVFFBlockPool(mvff));
  NextMethod(Inst, MVFFPool, finish)(inst);
}


/* MVFFDebugMixin - find debug mixin in class MVFFDebug */

static PoolDebugMixin MVFFDebugMixin(Pool pool)
{
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = PoolMVFF(pool);
  AVERT(MVFF, mvff);
  /* Can't check MVFFDebug, because this is called during init */
  return &(MVFF2MVFFDebug(mvff)->debug);
}


/* MVFFTotalSize -- total memory allocated from the arena */

static Size MVFFTotalSize(Pool pool)
{
  MVFF mvff;
  Land totalLand;

  AVERT(Pool, pool);
  mvff = PoolMVFF(pool);
  AVERT(MVFF, mvff);

  totalLand = MVFFTotalLand(mvff);
  return LandSize(totalLand);
}


/* MVFFFreeSize -- free memory (unused by client program) */

static Size MVFFFreeSize(Pool pool)
{
  MVFF mvff;
  Land freeLand;

  AVERT(Pool, pool);
  mvff = PoolMVFF(pool);
  AVERT(MVFF, mvff);

  freeLand = MVFFFreeLand(mvff);
  return LandSize(freeLand);
}


/* MVFFDescribe -- describe an MVFF pool */

static Res MVFFDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  Pool pool = CouldBeA(AbstractPool, inst);
  MVFF mvff = CouldBeA(MVFFPool, pool);
  Res res;

  if (!TESTC(MVFFPool, mvff))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = NextMethod(Inst, MVFFPool, describe)(inst, stream, depth);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth + 2,
               "extendBy  $W\n",  (WriteFW)mvff->extendBy,
               "avgSize   $W\n",  (WriteFW)mvff->avgSize,
               "firstFit  $U\n",  (WriteFU)mvff->firstFit,
               "slotHigh  $U\n",  (WriteFU)mvff->slotHigh,
               "spare     $D\n",  (WriteFD)mvff->spare,
               NULL);
  if (res != ResOK)
    return res;

  res = LocusPrefDescribe(MVFFLocusPref(mvff), stream, depth + 2);
  if (res != ResOK)
    return res;

  /* Don't describe MVFFBlockPool(mvff) otherwise it'll appear twice
   * in the output of GlobalDescribe. */

  res = LandDescribe(MVFFTotalLand(mvff), stream, depth + 2);
  if (res != ResOK)
    return res;

  res = LandDescribe(MVFFFreePrimary(mvff), stream, depth + 2);
  if (res != ResOK)
    return res;

  res = LandDescribe(MVFFFreeSecondary(mvff), stream, depth + 2);
  if (res != ResOK)
    return res;

  return ResOK;
}


DEFINE_CLASS(Pool, MVFFPool, klass)
{
  INHERIT_CLASS(klass, MVFFPool, AbstractBufferPool);
  klass->instClassStruct.describe = MVFFDescribe;
  klass->instClassStruct.finish = MVFFFinish;
  klass->size = sizeof(MVFFStruct);
  klass->varargs = MVFFVarargs;
  klass->init = MVFFInit;
  klass->alloc = MVFFAlloc;
  klass->free = MVFFFree;
  klass->bufferFill = MVFFBufferFill;
  klass->totalSize = MVFFTotalSize;
  klass->freeSize = MVFFFreeSize;
  AVERT(PoolClass, klass);
}


PoolClass PoolClassMVFF(void)
{
  return CLASS(MVFFPool);
}


/* Pool class MVFFDebug */

DEFINE_CLASS(Pool, MVFFDebugPool, klass)
{
  INHERIT_CLASS(klass, MVFFDebugPool, MVFFPool);
  PoolClassMixInDebug(klass);
  klass->size = sizeof(MVFFDebugStruct);
  klass->varargs = MVFFDebugVarargs;
  klass->debugMixin = MVFFDebugMixin;
  AVERT(PoolClass, klass);
}



/* MPS Interface Extensions. */

mps_pool_class_t mps_class_mvff(void)
{
  return (mps_pool_class_t)(CLASS(MVFFPool));
}

mps_pool_class_t mps_class_mvff_debug(void)
{
  return (mps_pool_class_t)(CLASS(MVFFDebugPool));
}


/* MVFFCheck -- check the consistency of an MVFF structure */

Bool MVFFCheck(MVFF mvff)
{
  CHECKS(MVFF, mvff);
  CHECKC(MVFFPool, mvff);
  CHECKD(Pool, MVFFPool(mvff));
  CHECKD(LocusPref, MVFFLocusPref(mvff));
  CHECKL(mvff->extendBy >= ArenaGrainSize(PoolArena(MVFFPool(mvff))));
  CHECKL(mvff->avgSize > 0);                    /* see .arg.check */
  CHECKL(mvff->avgSize <= mvff->extendBy);      /* see .arg.check */
  CHECKL(mvff->spare >= 0.0);                   /* see .arg.check */
  CHECKL(mvff->spare <= 1.0);                   /* see .arg.check */
  CHECKD(MFS, &mvff->cbsBlockPoolStruct);
  CHECKD(CBS, &mvff->totalCBSStruct);
  CHECKD(CBS, &mvff->freeCBSStruct);
  CHECKD(Freelist, &mvff->flStruct);
  CHECKD(Failover, &mvff->foStruct);
  CHECKL((LandSize)(MVFFTotalLand(mvff)) >= (LandSize)(MVFFFreeLand(mvff)));
  CHECKL(SizeIsAligned((LandSize)(MVFFFreeLand(mvff)), PoolAlignment(MVFFPool(mvff))));
  CHECKL(SizeIsArenaGrains((LandSize)(MVFFTotalLand(mvff)), PoolArena(MVFFPool(mvff))));
  CHECKL(BoolCheck(mvff->slotHigh));
  CHECKL(BoolCheck(mvff->firstFit));
  return TRUE;
}


/* Replacement of the deprecated MV pool class.
 *
 * MVFF replaces MV, but these functions are provided for backward
 * compatibility.  TODO: Remove these sometime after MPS 1.117.
 */

DECLARE_CLASS(Pool, MVPool, MVFFPool);
DECLARE_CLASS(Pool, MVDebugPool, MVPool);

static Res mvInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  Index i;
  
  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERC(PoolClass, klass);
  AVERT(ArgList, args);

  /* MV allows arbitrary alignment but MVFF does not, so round up. */
  for (i = 0; args[i].key != MPS_KEY_ARGS_END; ++i)
    if (args[i].key == MPS_KEY_ALIGN
        && args[i].val.align < FreelistMinimumAlignment)
      args[i].val.align = FreelistMinimumAlignment;

  return NextMethod(Pool, MVPool, init)(pool, arena, klass, args);
}

static void mvVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_EXTEND_BY;
  args[0].val.size = va_arg(varargs, Size);
  args[1].key = MPS_KEY_MEAN_SIZE;
  args[1].val.size = va_arg(varargs, Size);
  args[2].key = MPS_KEY_MAX_SIZE;
  args[2].val.size = va_arg(varargs, Size);
  args[3].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}

static void mvDebugVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_POOL_DEBUG_OPTIONS;
  args[0].val.pool_debug_options = va_arg(varargs, mps_pool_debug_option_s *);
  mvVarargs(args + 1, varargs);
}

DEFINE_CLASS(Pool, MVPool, klass)
{
  INHERIT_CLASS(klass, MVPool, MVFFPool);
  klass->init = mvInit;
  klass->varargs = mvVarargs;
  AVERT(PoolClass, klass);
}

DEFINE_CLASS(Pool, MVDebugPool, klass)
{
  INHERIT_CLASS(klass, MVDebugPool, MVPool);
  PoolClassMixInDebug(klass);
  klass->size = sizeof(MVFFDebugStruct);
  klass->varargs = mvDebugVarargs;
  klass->debugMixin = MVFFDebugMixin;
  AVERT(PoolClass, klass);
}

mps_pool_class_t mps_class_mv(void)
{
  return CLASS(MVPool);
}

mps_pool_class_t mps_class_mv_debug(void)
{
  return CLASS(MVDebugPool);
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

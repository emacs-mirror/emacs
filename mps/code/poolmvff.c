/* poolmvff.c: First Fit Manual Variable Pool
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .purpose: This is a pool class for manually managed objects of
 * variable size where address-ordered first fit is an appropriate
 * policy.  Provision is made to allocate in reverse.
 *
 * .design: <design/poolmvff>
 *
 * TRANSGRESSIONS
 *
 * .trans.stat: mps_mvff_stat is a temporary hack for measurement purposes,
 * see .stat below.
 *
 * NOTE
 *
 * There's potential for up to 4% speed improvement by calling Land
 * methods statically instead of indirectly via the Land abstraction
 * (thus, cbsInsert instead of LandInsert, and so on).
 */

#include "cbs.h"
#include "dbgpool.h"
#include "failover.h"
#include "freelist.h"
#include "mpm.h"
#include "mpscmvff.h"
#include "mpscmfs.h"
#include "poolmfs.h"

SRCID(poolmvff, "$Id$");


/* Would go in poolmvff.h if the class had any MPS-internal clients. */
extern PoolClass PoolClassMVFF(void);


/* MVFFStruct -- MVFF (Manual Variable First Fit) pool outer structure
 *
 * The signature is placed at the end, see
 * <design/pool/#outer-structure.sig>
 */

#define MVFFSig           ((Sig)0x5193FFF9) /* SIGnature MVFF */

typedef struct MVFFStruct *MVFF;
typedef struct MVFFStruct {     /* MVFF pool outer structure */
  PoolStruct poolStruct;        /* generic structure */
  SegPrefStruct segPrefStruct;  /* the preferences for allocation */
  Size extendBy;                /* size to extend pool by */
  Size avgSize;                 /* client estimate of allocation size */
  double spare;                 /* spare space fraction, see MVFFReduce */
  MFSStruct cbsBlockPoolStruct; /* stores blocks for CBSs */
  CBSStruct totalCBSStruct;     /* all memory allocated from the arena */
  CBSStruct freeCBSStruct;      /* free list */
  FreelistStruct flStruct;      /* emergency free list */
  FailoverStruct foStruct;      /* fail-over mechanism */
  Bool firstFit;                /* as opposed to last fit */
  Bool slotHigh;                /* prefers high part of large block */
  Sig sig;                      /* <design/sig/> */
} MVFFStruct;


#define Pool2MVFF(pool)     PARENT(MVFFStruct, poolStruct, pool)
#define MVFF2Pool(mvff)     (&((mvff)->poolStruct))
#define MVFFTotalCBS(mvff)  (&((mvff)->totalCBSStruct.landStruct))
#define MVFFFreeCBS(mvff)   (&((mvff)->freeCBSStruct.landStruct))
#define MVFFFreelist(mvff)  (&((mvff)->flStruct.landStruct))
#define MVFFFailover(mvff)  (&((mvff)->foStruct.landStruct))
#define MVFFSegPref(mvff)   (&((mvff)->segPrefStruct))
#define MVFFBlockPool(mvff) (&((mvff)->cbsBlockPoolStruct.poolStruct))

static Bool MVFFCheck(MVFF mvff);


/* MVFFDebug -- MVFFDebug class */

typedef struct MVFFDebugStruct {
  MVFFStruct mvffStruct;         /* MVFF structure */
  PoolDebugMixinStruct debug;    /* debug mixin */
} MVFFDebugStruct;

typedef MVFFDebugStruct *MVFFDebug;


#define MVFF2MVFFDebug(mvff) PARENT(MVFFDebugStruct, mvffStruct, mvff)
#define MVFFDebug2MVFF(mvffd) (&((mvffd)->mvffStruct))


/* MVFFReduce -- free tracts from given range
 *
 * Given a free range, attempts to find entire tracts within it, and
 * returns them to the arena.
 *
 * This is usually called immediately after MVFFInsert. It is not
 * combined with MVFFInsert because the latter is also called when new
 * segments are added under MVFFAlloc.
 */
static void MVFFReduce(MVFF mvff)
{
  Arena arena;
  Size freeSize, freeLimit, targetFree;
  RangeStruct freeRange, oldFreeRange;
  Align align;

  AVERT(MVFF, mvff);
  arena = PoolArena(MVFF2Pool(mvff));
  align = ArenaAlign(arena);

  /* Try to return memory when the amount of free memory exceeds a
     threshold fraction of the total memory. */
  
  /* NOTE: If this code becomes very hot, then the test of whether there's
     a large free block in the CBS could be inlined, since it's a property
     stored at the root node. */

  freeLimit = (Size)(LandSize(MVFFTotalCBS(mvff)) * mvff->spare);
  freeSize = LandSize(MVFFFailover(mvff));
  if (freeSize < freeLimit)
    return;

  targetFree = freeLimit / 2;

  /* Each time around this loop we either break, or we free at least
     one page back to the arena, thus ensuring that eventually the
     loop will terminate */

  while (freeSize > targetFree
         && LandFindLargest(&freeRange, &oldFreeRange, MVFFFailover(mvff),
                            align, FindDeleteNONE))
  {
    RangeStruct pageRange, oldRange;
    Size size;
    Res res;
    Addr base, limit;

    AVER(RangesEqual(&freeRange, &oldFreeRange));

    base = AddrAlignUp(RangeBase(&freeRange), align);
    limit = AddrAlignDown(RangeLimit(&freeRange), align);
    
    /* Give up if the block is too small to contain a whole page when
       aligned, even though it might be masking smaller better aligned
       pages that we could return, because CBSFindLargest won't be able
       to find those. */
    if (base >= limit)
      break;

    size = AddrOffset(base, limit);

    /* Don't return (much) more than we need to. */
    if (size > freeSize - targetFree)
      size = SizeAlignUp(freeSize - targetFree, align);

    /* Calculate the range of pages we can return to the arena near the
       top end of the free memory (because we're first fit). */
    RangeInit(&pageRange, AddrSub(limit, size), limit);
    AVER(!RangeIsEmpty(&pageRange));
    AVER(RangesNest(&freeRange, &pageRange));
    AVER(RangeIsAligned(&pageRange, align));

    /* Delete the range from the free list before attempting to delete
       it from the total allocated memory, so that we don't have
       dangling blocks in the free list, even for a moment. If we fail
       to delete from the TotalCBS we add back to the free list, which
       can't fail. */

    res = LandDelete(&oldRange, MVFFFailover(mvff), &pageRange);
    if (res != ResOK)
      break;
    freeSize -= RangeSize(&pageRange);
    AVER(freeSize == LandSize(MVFFFailover(mvff)));

    res = LandDelete(&oldRange, MVFFTotalCBS(mvff), &pageRange);
    if (res != ResOK) {
      RangeStruct coalescedRange;
      res = LandInsert(&coalescedRange, MVFFFailover(mvff), &pageRange);
      AVER(res == ResOK);
      break;
    }

    ArenaFree(RangeBase(&pageRange), RangeSize(&pageRange), MVFF2Pool(mvff));
  }
}


/* MVFFExtend -- allocate a new range from the arena
 *
 * Allocate a new range from the arena (with the given
 * withReservoirPermit flag) of at least the specified size. The
 * specified size should be pool-aligned. Add it to the allocated and
 * free lists.
 */
static Res MVFFExtend(Range rangeReturn, MVFF mvff, Size size,
                      Bool withReservoirPermit)
{
  Pool pool;
  Arena arena;
  Size allocSize;
  Align align;
  RangeStruct range, coalescedRange;
  Addr base;
  Res res;

  AVERT(MVFF, mvff);
  AVER(size > 0);
  AVERT(Bool, withReservoirPermit);

  pool = MVFF2Pool(mvff);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);

  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* Use extendBy unless it's too small (see */
  /* <design/poolmvff/#design.seg-size>). */
  if (size <= mvff->extendBy)
    allocSize = mvff->extendBy;
  else
    allocSize = size;

  allocSize = SizeAlignUp(allocSize, align);

  res = ArenaAlloc(&base, MVFFSegPref(mvff), allocSize, pool, withReservoirPermit);
  if (res != ResOK) {
    /* try again with a range just large enough for object */
    /* see <design/poolmvff/#design.seg-fail> */
    allocSize = SizeAlignUp(size, align);
    res = ArenaAlloc(&base, MVFFSegPref(mvff), allocSize, pool,
                     withReservoirPermit);
    if (res != ResOK)
      return res;
  }

  RangeInitSize(&range, base, allocSize);
  res = LandInsert(&coalescedRange, MVFFTotalCBS(mvff), &range);
  if (res != ResOK) {
    /* Can't record this memory, so return it to the arena and fail. */
    ArenaFree(base, allocSize, pool);
    return res;
  }

  DebugPoolFreeSplat(pool, RangeBase(&range), RangeLimit(&range));
  res = LandInsert(rangeReturn, MVFFFailover(mvff), &range);
  AVER(res == ResOK);

  /* Don't call MVFFReduce; that would be silly. */

  return ResOK;
}


/* MVFFFindFree -- find the first (or last) suitable free block
 *
 * Finds a free block of the given (pool aligned) size, according
 * to a first (or last) fit policy controlled by the MVFF fields
 * firstFit, slotHigh (for whether to allocate the top or bottom
 * portion of a larger block).
 *
 * Will return FALSE if the free lists have no large enough block. In
 * particular, will not attempt to allocate a new segment.
 */
static Bool MVFFFindFree(Range rangeReturn, MVFF mvff, Size size)
{
  Bool foundBlock;
  FindDelete findDelete;
  RangeStruct oldRange;

  AVER(rangeReturn != NULL);
  AVERT(MVFF, mvff);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(MVFF2Pool(mvff))));

  findDelete = mvff->slotHigh ? FindDeleteHIGH : FindDeleteLOW;

  foundBlock =
    (mvff->firstFit ? LandFindFirst : LandFindLast)
    (rangeReturn, &oldRange, MVFFFailover(mvff), size, findDelete);

  return foundBlock;
}


/* MVFFAlloc -- Allocate a block */

static Res MVFFAlloc(Addr *aReturn, Pool pool, Size size,
                     Bool withReservoirPermit)
{
  Res res;
  MVFF mvff;
  RangeStruct range;
  Bool foundBlock;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  AVER(aReturn != NULL);
  AVER(size > 0);
  AVERT(Bool, withReservoirPermit);

  size = SizeAlignUp(size, PoolAlignment(pool));

  foundBlock = MVFFFindFree(&range, mvff, size);
  if (!foundBlock) {
    RangeStruct addRange;

    res = MVFFExtend(&addRange, mvff, size, withReservoirPermit);
    if (res != ResOK)
      return res;
    foundBlock = MVFFFindFree(&range, mvff, size);

    /* We know that the found range must intersect the new segment. */
    /* In particular, it doesn't necessarily lie entirely within it. */
    AVER(foundBlock && RangesOverlap(&range, &addRange));
  }
  AVER(foundBlock);
  AVER(RangeSize(&range) == size);

  *aReturn = RangeBase(&range);

  return ResOK;
}


/* MVFFFree -- free the given block */

static void MVFFFree(Pool pool, Addr old, Size size)
{
  Res res;
  RangeStruct range, coalescedRange;
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  AVER(old != (Addr)0);
  AVER(AddrIsAligned(old, PoolAlignment(pool)));
  AVER(size > 0);

  RangeInitSize(&range, old, SizeAlignUp(size, PoolAlignment(pool)));
  res = LandInsert(&coalescedRange, MVFFFailover(mvff), &range);
  AVER(res == ResOK);
  MVFFReduce(mvff);
}


/* MVFFBufferFill -- Fill the buffer
 *
 * Fill it with the largest block we can find. This is worst-fit
 * allocation policy; see <design/poolmvff/#over.buffer>.
 */
static Res MVFFBufferFill(Addr *baseReturn, Addr *limitReturn,
                          Pool pool, Buffer buffer, Size size,
                          Bool withReservoirPermit)
{
  Res res;
  MVFF mvff;
  RangeStruct range, oldRange, newRange;
  Bool found;
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));
  AVERT(Bool, withReservoirPermit);

  found = LandFindLargest(&range, &oldRange, MVFFFailover(mvff), size,
                          FindDeleteENTIRE);
  if (!found) {
    /* Add a new range to the free lists and try again. */
    res = MVFFExtend(&newRange, mvff, size, withReservoirPermit);
    if (res != ResOK)
      return res;
    found = LandFindLargest(&range, &oldRange, MVFFFailover(mvff), size,
                            FindDeleteENTIRE);
    AVER(found && RangesOverlap(&range, &newRange));
  }
  AVER(found);
  AVER(RangesEqual(&range, &oldRange));
  AVER(RangeSize(&range) >= size);

  *baseReturn = RangeBase(&range);
  *limitReturn = RangeLimit(&range);
  return ResOK;
}


/* MVFFBufferEmpty -- return unused portion of this buffer */

static void MVFFBufferEmpty(Pool pool, Buffer buffer,
                            Addr base, Addr limit)
{
  Res res;
  MVFF mvff;
  RangeStruct range, coalescedRange;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  RangeInit(&range, base, limit);

  if (RangeIsEmpty(&range))
    return;

  res = LandInsert(&coalescedRange, MVFFFailover(mvff), &range);
  AVER(res == ResOK);
  MVFFReduce(mvff);
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

ARG_DEFINE_KEY(mvff_slot_high, Bool);
ARG_DEFINE_KEY(mvff_arena_high, Bool);
ARG_DEFINE_KEY(mvff_first_fit, Bool);

static Res MVFFInit(Pool pool, ArgList args)
{
  Size extendBy = MVFF_EXTEND_BY_DEFAULT;
  Size avgSize = MVFF_AVG_SIZE_DEFAULT;
  Align align = MVFF_ALIGN_DEFAULT;
  Bool slotHigh = MVFF_SLOT_HIGH_DEFAULT;
  Bool arenaHigh = MVFF_ARENA_HIGH_DEFAULT;
  Bool firstFit = MVFF_FIRST_FIT_DEFAULT;
  double spare = MVFF_SPARE_DEFAULT;
  MVFF mvff;
  Arena arena;
  Res res;
  ArgStruct arg;

  AVERT(Pool, pool);
  arena = PoolArena(pool);

  /* .arg: class-specific additional arguments; see */
  /* <design/poolmvff/#method.init> */
  /* .arg.check: we do the same checks here and in MVFFCheck */
  /* except for arenaHigh, which is stored only in the segPref. */
  
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
   * of a Freelist to store the free address ranges in low-memory
   * situations. <design/freelist/#impl.grain.align>.
   */
  AVER(AlignIsAligned(align, FreelistMinimumAlignment));
  AVERT(Bool, slotHigh);
  AVERT(Bool, arenaHigh);
  AVERT(Bool, firstFit);

  mvff = Pool2MVFF(pool);

  mvff->extendBy = extendBy;
  if (extendBy < ArenaAlign(arena))
    mvff->extendBy = ArenaAlign(arena);
  mvff->avgSize = avgSize;
  pool->alignment = align;
  mvff->slotHigh = slotHigh;
  mvff->firstFit = firstFit;
  mvff->spare = spare;

  SegPrefInit(MVFFSegPref(mvff));
  SegPrefExpress(MVFFSegPref(mvff), arenaHigh ? SegPrefHigh : SegPrefLow, NULL);

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
    res = LandInit(MVFFTotalCBS(mvff), CBSFastLandClassGet(), arena, align,
                   mvff, liArgs);
  } MPS_ARGS_END(liArgs);
  if (res != ResOK)
    goto failTotalCBSInit;

  MPS_ARGS_BEGIN(liArgs) {
    MPS_ARGS_ADD(liArgs, CBSBlockPool, MVFFBlockPool(mvff));
    res = LandInit(MVFFFreeCBS(mvff), CBSFastLandClassGet(), arena, align,
                   mvff, liArgs);
  } MPS_ARGS_END(liArgs);
  if (res != ResOK)
    goto failFreeCBSInit;

  res = LandInit(MVFFFreelist(mvff), FreelistLandClassGet(), arena, align,
                 mvff, mps_args_none);
  if (res != ResOK)
    goto failFreelistInit;

  MPS_ARGS_BEGIN(foArgs) {
    MPS_ARGS_ADD(foArgs, FailoverPrimary, MVFFFreeCBS(mvff));
    MPS_ARGS_ADD(foArgs, FailoverSecondary, MVFFFreelist(mvff));
    res = LandInit(MVFFFailover(mvff), FailoverLandClassGet(), arena, align,
                   mvff, foArgs);
  } MPS_ARGS_END(foArgs);
  if (res != ResOK)
    goto failFailoverInit;

  mvff->sig = MVFFSig;
  AVERT(MVFF, mvff);
  EVENT8(PoolInitMVFF, pool, arena, extendBy, avgSize, align,
         BOOLOF(slotHigh), BOOLOF(arenaHigh), BOOLOF(firstFit));
  return ResOK;

failFailoverInit:
  LandFinish(MVFFFreelist(mvff));
failFreelistInit:
  LandFinish(MVFFFreeCBS(mvff));
failFreeCBSInit:
  LandFinish(MVFFTotalCBS(mvff));
failTotalCBSInit:
  PoolFinish(MVFFBlockPool(mvff));
failBlockPoolInit:
  return res;
}


/* MVFFFinish -- finish method for MVFF */

static Bool mvffFinishVisitor(Bool *deleteReturn, Land land, Range range,
                              void *closureP, Size closureS)
{
  Pool pool;
  AVER(deleteReturn != NULL);
  AVERT(Land, land);
  AVERT(Range, range);
  AVER(closureP != NULL);
  pool = closureP;
  AVERT(Pool, pool);
  UNUSED(closureS);

  ArenaFree(RangeBase(range), RangeSize(range), pool);
  *deleteReturn = FALSE;
  return TRUE;
}

static void MVFFFinish(Pool pool)
{
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);
  mvff->sig = SigInvalid;

  LandIterate(MVFFTotalCBS(mvff), mvffFinishVisitor, pool, 0);

  /* TODO: would like to check that LandSize(MVFFTotalCBS(mvff)) == 0
   * now, but CBS doesn't support deletion while iterating. */

  LandFinish(MVFFFailover(mvff));
  LandFinish(MVFFFreelist(mvff));
  LandFinish(MVFFFreeCBS(mvff));
  LandFinish(MVFFTotalCBS(mvff));
  PoolFinish(MVFFBlockPool(mvff));
}


/* MVFFDebugMixin - find debug mixin in class MVFFDebug */

static PoolDebugMixin MVFFDebugMixin(Pool pool)
{
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);
  /* Can't check MVFFDebug, because this is called during init */
  return &(MVFF2MVFFDebug(mvff)->debug);
}


/* MVFFDescribe -- describe an MVFF pool */

static Res MVFFDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  MVFF mvff;

  if (!TESTT(Pool, pool)) return ResFAIL;
  mvff = Pool2MVFF(pool);
  if (!TESTT(MVFF, mvff)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "MVFF $P {\n", (WriteFP)mvff,
               "  pool $P ($U)\n",
               (WriteFP)pool, (WriteFU)pool->serial,
               "  extendBy  $W\n",  (WriteFW)mvff->extendBy,
               "  avgSize   $W\n",  (WriteFW)mvff->avgSize,
               "  firstFit  $U\n",  (WriteFU)mvff->firstFit,
               "  slotHigh  $U\n",  (WriteFU)mvff->slotHigh,
               NULL);
  if (res != ResOK) return res;

  /* TODO: SegPrefDescribe(MVFFSegPref(mvff), stream); */

  res = PoolDescribe(MVFFBlockPool(mvff), stream);
  if (res != ResOK) return res;

  res = LandDescribe(MVFFTotalCBS(mvff), stream);
  if (res != ResOK) return res;

  res = LandDescribe(MVFFFreeCBS(mvff), stream);
  if (res != ResOK) return res;

  res = LandDescribe(MVFFFreelist(mvff), stream);
  if (res != ResOK) return res;

  res = WriteF(stream, "}\n", NULL);
  return res;
}


DEFINE_POOL_CLASS(MVFFPoolClass, this)
{
  INHERIT_CLASS(this, AbstractAllocFreePoolClass);
  PoolClassMixInBuffer(this);
  this->name = "MVFF";
  this->size = sizeof(MVFFStruct);
  this->offset = offsetof(MVFFStruct, poolStruct);
  this->varargs = MVFFVarargs;
  this->init = MVFFInit;
  this->finish = MVFFFinish;
  this->alloc = MVFFAlloc;
  this->free = MVFFFree;
  this->bufferFill = MVFFBufferFill;
  this->bufferEmpty = MVFFBufferEmpty;
  this->describe = MVFFDescribe;
  AVERT(PoolClass, this);
}


PoolClass PoolClassMVFF(void)
{
  return MVFFPoolClassGet();
}


/* Pool class MVFFDebug */

DEFINE_POOL_CLASS(MVFFDebugPoolClass, this)
{
  INHERIT_CLASS(this, MVFFPoolClass);
  PoolClassMixInDebug(this);
  this->name = "MVFFDBG";
  this->size = sizeof(MVFFDebugStruct);
  this->varargs = MVFFDebugVarargs;
  this->debugMixin = MVFFDebugMixin;
  AVERT(PoolClass, this);
}



/* MPS Interface Extensions. */

mps_class_t mps_class_mvff(void)
{
  return (mps_class_t)(MVFFPoolClassGet());
}

mps_class_t mps_class_mvff_debug(void)
{
  return (mps_class_t)(MVFFDebugPoolClassGet());
}


/* Total free bytes. See <design/poolmvff/#design.arena-enter> */

size_t mps_mvff_free_size(mps_pool_t mps_pool)
{
  Pool pool;
  MVFF mvff;

  pool = (Pool)mps_pool;
  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  return (size_t)LandSize(MVFFFailover(mvff));
}

/* Total owned bytes. See <design/poolmvff/#design.arena-enter> */

size_t mps_mvff_size(mps_pool_t mps_pool)
{
  Pool pool;
  MVFF mvff;

  pool = (Pool)mps_pool;
  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  return (size_t)LandSize(MVFFTotalCBS(mvff));
}


/* MVFFCheck -- check the consistency of an MVFF structure */

ATTRIBUTE_UNUSED
static Bool MVFFCheck(MVFF mvff)
{
  CHECKS(MVFF, mvff);
  CHECKD(Pool, MVFF2Pool(mvff));
  CHECKL(IsSubclassPoly(MVFF2Pool(mvff)->class, MVFFPoolClassGet()));
  CHECKD(SegPref, MVFFSegPref(mvff));
  CHECKL(mvff->extendBy >= ArenaAlign(PoolArena(MVFF2Pool(mvff))));
  CHECKL(mvff->avgSize > 0);                    /* see .arg.check */
  CHECKL(mvff->avgSize <= mvff->extendBy);      /* see .arg.check */
  CHECKL(mvff->spare >= 0.0);                   /* see .arg.check */
  CHECKL(mvff->spare <= 1.0);                   /* see .arg.check */
  CHECKD(Land, MVFFTotalCBS(mvff));
  CHECKD(Land, MVFFFreeCBS(mvff));
  CHECKD(Land, MVFFFreelist(mvff));
  CHECKD(Land, MVFFFailover(mvff));
  CHECKL(LandSize(MVFFTotalCBS(mvff)) >= LandSize(MVFFFailover(mvff)));
  CHECKL(SizeIsAligned(LandSize(MVFFFailover(mvff)), PoolAlignment(MVFF2Pool(mvff))));
  CHECKL(SizeIsAligned(LandSize(MVFFTotalCBS(mvff)), ArenaAlign(PoolArena(MVFF2Pool(mvff)))));
  CHECKL(BoolCheck(mvff->slotHigh));
  CHECKL(BoolCheck(mvff->firstFit));
  return TRUE;
}


/* Return the CBS of an MVFF pool for the benefit of fotest.c. */

extern Land _mps_mvff_cbs(Pool);
Land _mps_mvff_cbs(Pool pool) {
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  return MVFFFreeCBS(mvff);
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

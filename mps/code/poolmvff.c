/* poolmvff.c: First Fit Manual Variable Pool
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .purpose: This is a pool class for manually managed objects of
 * variable size where address-ordered first fit is an appropriate
 * policy.  Provision is made to allocate in reverse.  This pool
 * can allocate across segment boundaries.
 *
 * .design: <design/poolmvff>
 *
 *
 * TRANSGRESSIONS
 *
 * .trans.stat: mps_mvff_stat is a temporary hack for measurement purposes,
 * see .stat below.
 */

#include "mpscmvff.h"
#include "dbgpool.h"
#include "cbs.h"
#include "freelist.h"
#include "mpm.h"

SRCID(poolmvff, "$Id$");

/* #define MVFF_DEBUG */


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
  SegPrefStruct segPrefStruct;  /* the preferences for segments */
  Size extendBy;                /* size to extend pool by */
  Size avgSize;                 /* client estimate of allocation size */
  Size total;                   /* total bytes in pool */
  Size free;                    /* total free bytes in pool */
  double spare;                 /* spare space fraction, see MVFFReduce */
  MFSStruct cbsBlockPoolStruct; /* stores blocks for CBSs */
  CBSStruct totalCBSStruct;     /* all memory allocated from the arena */
  CBSStruct freeCBSStruct;      /* free list */
  FreelistStruct flStruct;      /* emergency free list */
  Bool firstFit;                /* as opposed to last fit */
  Bool slotHigh;                /* prefers high part of large block */
  Sig sig;                      /* <design/sig/> */
} MVFFStruct;


#define Pool2MVFF(pool)       PARENT(MVFFStruct, poolStruct, pool)
#define MVFF2Pool(mvff)       (&((mvff)->poolStruct))
#define MVFFFreeCBS(mvff)     (&((mvff)->freeCBSStruct))
#define MVFFTotalCBS(mvff)    (&((mvff)->totalCBSStruct))
#define MVFFOfCBS(cbs)        PARENT(MVFFStruct, cbsStruct, cbs)
#define MVFFFreelist(mvff)    (&((mvff)->flStruct))
#define MVFFOfFreelist(fl)    PARENT(MVFFStruct, flStruct, fl)
#define MVFFSegPref(mvff)     (&((mvff)->segPrefStruct))
#define MVFFBlockPool(mvff)   (&((mvff)->cbsBlockPoolStruct.poolStruct))

static Bool MVFFCheck(MVFF mvff);


/* MVFFDebug -- MVFFDebug class */

typedef struct MVFFDebugStruct {
  MVFFStruct mvffStruct;         /* MVFF structure */
  PoolDebugMixinStruct debug;    /* debug mixin */
} MVFFDebugStruct;

typedef MVFFDebugStruct *MVFFDebug;


#define MVFF2MVFFDebug(mvff) PARENT(MVFFDebugStruct, mvffStruct, mvff)
#define MVFFDebug2MVFF(mvffd) (&((mvffd)->mvffStruct))


/* MVFFAddToFree -- Add given range to free list
 *
 * Returns the maximally coalesced range containing the given range.
 * Updates MVFF counters for additional free space.
 */

static void MVFFAddToFree(Range coalesced, MVFF mvff, Range range) {
  Res res;

  AVERT(Range, range);
  AVERT(MVFF, mvff);
  AVER(coalesced != range);

  res = CBSInsert(coalesced, MVFFFreeCBS(mvff), range);
  if (res != ResOK) {
    /* CBS ran out of memory for splay nodes: add range to emergency
     * free list instead. */
    AVER(ResIsAllocFailure(res));
    FreelistInsert(coalesced, MVFFFreelist(mvff), range);
  }

  mvff->free += RangeSize(range);
}


/* MVFFDeleteFromFree -- delete range from free list
 *
 * This wraps CBSDelete so that it cannot fail due to allocation.
 *
 * If the CBS ran out of memory for splay nodes then there were fragments
 * on both sides: see <design/cbs/#function.cbs.delete.fail>. Handle
 * this by deleting the full range (which requires no allocation) and
 * re-inserting the fragments, which can't fail because it falls back
 * to the freelist.
 */

static void MVFFDeleteFromFree(MVFF mvff, Range range)
{
  RangeStruct fullRange, oldRange, fragRange;
  Res res;
  
  res = CBSDelete(&fullRange, MVFFFreeCBS(mvff), range);
  if (res == ResOK) {
    mvff->free -= RangeSize(range);
  } else if (res == ResFAIL) {
    /* Not found in the CBS: must be found in the Freelist. */
    res = FreelistDelete(&oldRange, MVFFFreelist(mvff), range);
    AVER(res == ResOK);
    mvff->free -= RangeSize(range);
  } else if (ResIsAllocFailure(res)) {
    res = CBSDelete(&oldRange, MVFFFreeCBS(mvff), &fullRange);
    AVER(res == ResOK);
    AVER(RangesEqual(&oldRange, &fullRange));
    mvff->free -= RangeSize(&fullRange);

    AVER(RangeBase(&fullRange) < RangeBase(range));
    RangeInit(&fragRange, RangeBase(&fullRange), RangeBase(range));
    MVFFAddToFree(&oldRange, mvff, &fragRange);
    
    AVER(RangeLimit(range) < RangeLimit(&fullRange));
    RangeInit(&fragRange, RangeLimit(range), RangeLimit(&fullRange));
    MVFFAddToFree(&oldRange, mvff, &fragRange);
  } else {
    NOTREACHED; /* unexpected kind of failure from CBSDelete */
  }
}


/* MVFFReduce -- free segments from given range
 *
 * Consider reducing the total size of the pool by returning memory
 * to the arena.
 *
 * This is usually called immediately after MVFFAddToFreeList.
 * It is not combined with MVFFAddToFreeList because the latter
 * is also called when new segments are added under MVFFAlloc.
 */

static void MVFFReduce(MVFF mvff)
{
  Arena arena;
  RangeStruct freeRange;
  Size freeLimit, targetFree;
  Align align;
  
  AVERT(MVFF, mvff);
  arena = PoolArena(MVFF2Pool(mvff));
  align = ArenaAlign(arena);

  /* Try to return memory when the amount of free memory exceeds a
     threshold fraction of the total memory. */
  
  /* NOTE: If this code becomes very hot, then the test of whether there's
     a large free block in the CBS could be inlined, since it's a property
     stored at the root node. */

  freeLimit = (Size)(mvff->total * mvff->spare);
  if (mvff->free < freeLimit)
    return;

  targetFree = freeLimit / 2;
  while (mvff->free > targetFree &&
         CBSFindLargest(&freeRange, &freeRange, MVFFFreeCBS(mvff),
                        0, FindDeleteNONE)) {
    RangeStruct pageRange, oldRange;
    Size size;
    Res res;
    Addr base, limit;
    
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
    if (size > mvff->free - targetFree)
      size = SizeAlignUp(mvff->free - targetFree, align);

    /* Calculate the range of pages we can return to the arena near the
       top end of the free memory (because we're first fit). */
    RangeInit(&pageRange, AddrSub(limit, size), limit);
    AVER(!RangeIsEmpty(&pageRange));
    AVER(RangesNest(&freeRange, &pageRange));
    AVER(RangeIsAligned(&pageRange, align));

    /* Delete the range from the free list before attempting to delete it
       from the total allocated memory, so that we don't have dangling blocks
       in the freelist, even for a moment.  If we fail to delete from the
       totalCBS we add back to the freelist, which can't fail. */
    
    MVFFDeleteFromFree(mvff, &pageRange);

    res = CBSDelete(&oldRange, MVFFTotalCBS(mvff), &pageRange);
    if (res != ResOK) {
      RangeStruct coalesced;
      MVFFAddToFree(&coalesced, mvff, &pageRange);
      return;
    }
    mvff->total -= RangeSize(&pageRange);

    ArenaFree(RangeBase(&pageRange), RangeSize(&pageRange), MVFF2Pool(mvff));
  }
}


/* MVFFExtend -- Allocates a new segment from the arena
 *
 * Allocates a new segment from the arena (with the given
 * withReservoirPermit flag) of at least the specified size.  The
 * specified size should be pool-aligned.  Adds it to the free list.
 */
static Res MVFFExtend(MVFF mvff, Size size, Bool withReservoirPermit)
{
  Pool pool;
  Arena arena;
  Size segSize;
  Res res;
  Align align;
  Addr base, limit;
  RangeStruct range, coalesced;

  AVERT(MVFF, mvff);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  pool = MVFF2Pool(mvff);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);

  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* Use extendBy unless it's too small (see */
  /* <design/poolmvff/#design.seg-size>). */
  if (size <= mvff->extendBy)
    segSize = mvff->extendBy;
  else
    segSize = size;

  segSize = SizeAlignUp(segSize, align);

  res = ArenaAlloc(&base, MVFFSegPref(mvff), segSize, pool, withReservoirPermit);
  if (res != ResOK) {
    /* try again for a seg just large enough for object */
    /* see <design/poolmvff/#design.seg-fail> */
    segSize = SizeAlignUp(size, align);
    res = ArenaAlloc(&base, MVFFSegPref(mvff), segSize, pool, withReservoirPermit);
    if (res != ResOK)
      return res;
  }
  
  limit = AddrAdd(base, segSize);
  RangeInit(&range, base, limit);

  res = CBSInsert(&coalesced, MVFFTotalCBS(mvff), &range);
  if (res != ResOK) {
    ArenaFree(base, segSize, pool);
    return res;
  }
  mvff->total += segSize;

  DebugPoolFreeSplat(pool, base, limit);
  MVFFAddToFree(&coalesced, mvff, &range);

  return ResOK;
}


/* MVFFFindFirstFree -- Finds the first (or last) suitable free block
 *
 * Finds a free block of the given (pool aligned) size, according
 * to a first (or last) fit policy controlled by the MVFF fields
 * firstFit, slotHigh (for whether to allocate the top or bottom
 * portion of a larger block).
 *
 * Will return FALSE if the free list has no large enough block.
 * In particular, will not attempt to allocate a new segment.
 */
static Bool MVFFFindFirstFree(Addr *baseReturn, Addr *limitReturn,
                              MVFF mvff, Size size)
{
  Bool foundBlock;
  FindDelete findDelete;
  RangeStruct range, oldRange;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(MVFF, mvff);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(MVFF2Pool(mvff))));

  FreelistFlushToCBS(MVFFFreelist(mvff), MVFFFreeCBS(mvff));

  findDelete = mvff->slotHigh ? FindDeleteHIGH : FindDeleteLOW;

  foundBlock =
    (mvff->firstFit ? CBSFindFirst : CBSFindLast)
    (&range, &oldRange, MVFFFreeCBS(mvff), size, findDelete);

  if (!foundBlock) {
    /* Failed to find a block in the CBS: try the emergency free list
     * as well. */
    foundBlock =
      (mvff->firstFit ? FreelistFindFirst : FreelistFindLast)
      (&range, &oldRange, MVFFFreelist(mvff), size, findDelete);
  }
  
  if (!foundBlock)
    return FALSE;

  AVER(RangeSize(&range) == size);
  mvff->free -= size;

  *baseReturn = RangeBase(&range);
  *limitReturn = RangeLimit(&range);
  return TRUE;
}


/* MVFFAlloc -- Allocate a block */

static Res MVFFAlloc(Addr *aReturn, Pool pool, Size size,
                     Bool withReservoirPermit)
{
  Res res;
  MVFF mvff;
  Addr base, limit;
  Bool foundBlock;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  AVER(aReturn != NULL);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  size = SizeAlignUp(size, PoolAlignment(pool));

  foundBlock = MVFFFindFirstFree(&base, &limit, mvff, size);
  if (!foundBlock) {
    res = MVFFExtend(mvff, size, withReservoirPermit);
    if (res != ResOK)
      return res;
    foundBlock = MVFFFindFirstFree(&base, &limit, mvff, size);
  }
  AVER(foundBlock);
  AVER(AddrOffset(base, limit) == size);

  *aReturn = base;
  return ResOK;
}


/* MVFFFree -- free the given block */

static void MVFFFree(Pool pool, Addr old, Size size)
{
  RangeStruct range, coalescedRange;
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  AVER(old != (Addr)0);
  AVER(AddrIsAligned(old, PoolAlignment(pool)));
  AVER(size > 0);

  size = SizeAlignUp(size, PoolAlignment(pool));
  RangeInit(&range, old, AddrAdd(old, size));
  MVFFAddToFree(&coalescedRange, mvff, &range);
  MVFFReduce(mvff);
}

/* MVFFFindLargest -- call CBSFindLargest and then fall back to
 * FreelistFindLargest if no block in the CBS was big enough. */

static Bool MVFFFindLargest(Range range, Range oldRange, MVFF mvff,
                            Size size, FindDelete findDelete)
{
  AVER(range != NULL);
  AVER(oldRange != NULL);
  AVERT(MVFF, mvff);
  AVER(size > 0);
  AVERT(FindDelete, findDelete);

  FreelistFlushToCBS(MVFFFreelist(mvff), MVFFFreeCBS(mvff));

  if (CBSFindLargest(range, oldRange, MVFFFreeCBS(mvff), size, findDelete))
    return TRUE;

  if (FreelistFindLargest(range, oldRange, MVFFFreelist(mvff),
                          size, findDelete))
    return TRUE;

  return FALSE;
}


/* MVFFBufferFill -- Fill the buffer
 *
 * Fill it with the largest block we can find.
 */
static Res MVFFBufferFill(Addr *baseReturn, Addr *limitReturn,
                          Pool pool, Buffer buffer, Size size,
                          Bool withReservoirPermit)
{
  Res res;
  MVFF mvff;
  RangeStruct range, oldRange;
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

  found = MVFFFindLargest(&range, &oldRange, mvff, size, FindDeleteENTIRE);
  if (!found) {
    /* Add a new segment to the free list and try again. */
    res = MVFFExtend(mvff, size, withReservoirPermit);
    if (res != ResOK)
      return res;
    found = MVFFFindLargest(&range, &oldRange, mvff, size, FindDeleteENTIRE);
  }
  AVER(found);
  AVER(RangesEqual(&range, &oldRange));

  AVER(RangeSize(&range) >= size);
  mvff->free -= RangeSize(&range);

  *baseReturn = RangeBase(&range);
  *limitReturn = RangeLimit(&range);
  return ResOK;
}


/* MVFFBufferEmpty -- return unused portion of this buffer */

static void MVFFBufferEmpty(Pool pool, Buffer buffer,
                            Addr base, Addr limit)
{
  MVFF mvff;
  RangeStruct range, coalescedRange;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  AVER(base <= limit);

  if (base == limit)
    return;

  RangeInit(&range, base, limit);
  MVFFAddToFree(&coalescedRange, mvff, &range);
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
  AVER(ArgListCheck(args));
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
  Size align = MVFF_ALIGN_DEFAULT;
  Bool slotHigh = MVFF_SLOT_HIGH_DEFAULT;
  Bool arenaHigh = MVFF_ARENA_HIGH_DEFAULT;
  Bool firstFit = MVFF_FIRST_FIT_DEFAULT;
  double spare = MVFF_SPARE_DEFAULT;
  MVFF mvff;
  Arena arena;
  Res res;
  ZoneSet zones;
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
  AVER(SizeIsAligned(align, MPS_PF_ALIGN));
  AVER(BoolCheck(slotHigh));
  AVER(BoolCheck(arenaHigh));
  AVER(BoolCheck(firstFit));

  mvff = Pool2MVFF(pool);

  mvff->extendBy = extendBy;
  mvff->avgSize = avgSize;
  pool->alignment = align;
  mvff->slotHigh = slotHigh;
  mvff->firstFit = firstFit;
  mvff->spare = spare;

  SegPrefInit(MVFFSegPref(mvff));
  SegPrefExpress(MVFFSegPref(mvff), arenaHigh ? SegPrefHigh : SegPrefLow, NULL);
  zones = ZoneSetComp(ArenaDefaultZONESET);
  SegPrefExpress(MVFFSegPref(mvff), SegPrefZoneSet, &zones);

  mvff->total = 0;
  mvff->free = 0;

  res = FreelistInit(MVFFFreelist(mvff), align);
  if (res != ResOK)
    goto failFreelistInit;

  /* An MFS pool is explicitly initialised for the two CBSs partly to share
     space, but mostly to avoid a call to PoolCreate, so that MVFF can be
     used during arena bootstrap as the control pool. */

  MPS_ARGS_BEGIN(piArgs) {
    MPS_ARGS_ADD(piArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(CBSBlockStruct));
    MPS_ARGS_DONE(piArgs);
    res = PoolInit(MVFFBlockPool(mvff), arena, PoolClassMFS(), piArgs);
  } MPS_ARGS_END(piArgs);
  if (res != ResOK)
    goto failBlockPoolInit;

  res = CBSInitWithPool(arena, MVFFTotalCBS(mvff), mvff, ArenaAlign(arena),
                        TRUE, MVFFBlockPool(mvff));
  if (res != ResOK)
    goto failTotalInit;

  res = CBSInitWithPool(arena, MVFFFreeCBS(mvff), mvff, align,
                        TRUE, MVFFBlockPool(mvff));
  if (res != ResOK)
    goto failFreeInit;

  mvff->sig = MVFFSig;
  AVERT(MVFF, mvff);
  EVENT8(PoolInitMVFF, pool, arena, extendBy, avgSize, align,
                 slotHigh, arenaHigh, firstFit);
  return ResOK;

failFreeInit:
  CBSFinish(MVFFTotalCBS(mvff));
failTotalInit:
  PoolFinish(MVFFBlockPool(mvff));
failBlockPoolInit:
  FreelistFinish(MVFFFreelist(mvff));
failFreelistInit:
  AVER(res != ResOK);
  return res;
}


/* MVFFFinish -- finish method for MVFF */

static Bool MVFFTotalFreeVisitor(CBS cbs, Range range,
                                 void *closureP, Size closureS)
{
  Pool pool = closureP;
  UNUSED(cbs);
  UNUSED(closureS);
  ArenaFree(RangeBase(range), RangeSize(range), pool);
  return TRUE;
}

static void MVFFFinish(Pool pool)
{
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  mvff->sig = SigInvalid;

  (void)CBSIterate(MVFFTotalCBS(mvff), MVFFTotalFreeVisitor, pool, 0);

  /* Could maintain mvff->total here and check it falls to zero, */
  /* but that would just make the function slow.  If only we had */
  /* a way to do operations only if AVERs are turned on. */

  CBSFinish(MVFFFreeCBS(mvff));
  CBSFinish(MVFFTotalCBS(mvff));
  PoolFinish(MVFFBlockPool(mvff));
  FreelistFinish(MVFFFreelist(mvff));
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
               "  total     $U\n",  (WriteFU)mvff->total,
               "  free      $U\n",  (WriteFU)mvff->free,
               NULL);
  if (res != ResOK)
    return res;

  res = CBSDescribe(MVFFTotalCBS(mvff), stream);
  if (res != ResOK)
    return res;

  res = CBSDescribe(MVFFFreeCBS(mvff), stream);
  if (res != ResOK)
    return res;

  res = FreelistDescribe(MVFFFreelist(mvff), stream);
  if (res != ResOK)
    return res;

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

  return (size_t)mvff->free;
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

  return (size_t)mvff->total;
}


/* MVFFCheck -- check the consistency of an MVFF structure */

static Bool MVFFCheck(MVFF mvff)
{
  CHECKS(MVFF, mvff);
  CHECKD(Pool, MVFF2Pool(mvff));
  CHECKL(IsSubclassPoly(MVFF2Pool(mvff)->class, MVFFPoolClassGet()));
  CHECKD(SegPref, MVFFSegPref(mvff));
  CHECKL(mvff->extendBy > 0);                   /* see .arg.check */
  CHECKL(mvff->avgSize > 0);                    /* see .arg.check */
  CHECKL(mvff->avgSize <= mvff->extendBy);      /* see .arg.check */
  CHECKL(mvff->spare >= 0.0);                   /* see .arg.check */
  CHECKL(mvff->spare <= 1.0);                   /* see .arg.check */
  CHECKL(mvff->total >= mvff->free);
  CHECKL(SizeIsAligned(mvff->free, PoolAlignment(MVFF2Pool(mvff))));
  CHECKL(SizeIsAligned(mvff->total, ArenaAlign(PoolArena(MVFF2Pool(mvff)))));
  CHECKD(CBS, MVFFFreeCBS(mvff));
  CHECKD(Freelist, MVFFFreelist(mvff));
  CHECKL(BoolCheck(mvff->slotHigh));
  CHECKL(BoolCheck(mvff->firstFit));
#ifdef MVFF_DEBUG /* FIXME: Consider using just "if" */
  CHECKL(mvff->free == CBSSize(MVFFFreeCBS(mvff)) +
                       FreelistSize(MVFFFreelist(mvff)));
  CHECKL(mvff->total == CBSSize(MVFFTotalCBS(mvff)));
#endif
  return TRUE;
}


/* Return the CBS of an MVFF pool for the benefit of fotest.c. */

extern CBS _mps_mvff_cbs(mps_pool_t);
CBS _mps_mvff_cbs(mps_pool_t mps_pool) {
  Pool pool;
  MVFF mvff;

  pool = (Pool)mps_pool;
  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  return MVFFFreeCBS(mvff);
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

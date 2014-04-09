/* poolmvff.c: First Fit Manual Variable Pool
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
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
  SegPref segPref;              /* the preferences for segments */
  Size extendBy;                /* segment size to extend pool by */
  Size minSegSize;              /* minimum size of segment */
  Size avgSize;                 /* client estimate of allocation size */
  Size total;                   /* total bytes in pool */
  Size free;                    /* total free bytes in pool */
  CBSStruct cbsStruct;          /* free list */
  FreelistStruct flStruct;      /* emergency free list */
  Bool firstFit;                /* as opposed to last fit */
  Bool slotHigh;                /* prefers high part of large block */
  Sig sig;                      /* <design/sig/> */
} MVFFStruct;


#define Pool2MVFF(pool)   PARENT(MVFFStruct, poolStruct, pool)
#define MVFF2Pool(mvff)       (&((mvff)->poolStruct))
#define CBSOfMVFF(mvff)      (&((mvff)->cbsStruct))
#define MVFFOfCBS(cbs)       PARENT(MVFFStruct, cbsStruct, cbs)
#define FreelistOfMVFF(mvff)      (&((mvff)->flStruct))
#define MVFFOfFreelist(fl)       PARENT(MVFFStruct, flStruct, fl)

static Bool MVFFCheck(MVFF mvff);


/* MVFFDebug -- MVFFDebug class */

typedef struct MVFFDebugStruct {
  MVFFStruct mvffStruct;         /* MVFF structure */
  PoolDebugMixinStruct debug;    /* debug mixin */
} MVFFDebugStruct;

typedef MVFFDebugStruct *MVFFDebug;


#define MVFF2MVFFDebug(mvff) PARENT(MVFFDebugStruct, mvffStruct, mvff)
#define MVFFDebug2MVFF(mvffd) (&((mvffd)->mvffStruct))


/* MVFFAddToFreeList -- Add given range to free list
 *
 * Updates MVFF counters for additional free space.  Returns maximally
 * coalesced range containing given range.  Does not attempt to free
 * segments (see MVFFFreeSegs).
 */
static Res MVFFAddToFreeList(Addr *baseIO, Addr *limitIO, MVFF mvff) {
  Res res;
  RangeStruct range, newRange;

  AVER(baseIO != NULL);
  AVER(limitIO != NULL);
  AVERT(MVFF, mvff);
  RangeInit(&range, *baseIO, *limitIO);

  res = CBSInsert(&newRange, CBSOfMVFF(mvff), &range);
  if (ResIsAllocFailure(res)) {
    /* CBS ran out of memory for splay nodes: add range to emergency
     * free list instead. */
    res = FreelistInsert(&newRange, FreelistOfMVFF(mvff), &range);
  }

  if (res == ResOK) {
    mvff->free += RangeSize(&range);
    *baseIO = RangeBase(&newRange);
    *limitIO = RangeLimit(&newRange);
  }

  return res;
}


/* MVFFFreeSegs -- Free segments from given range
 *
 * Given a free range, attempts to find entire segments within
 * it, and returns them to the arena, updating total size counter.
 *
 * This is usually called immediately after MVFFAddToFreeList.
 * It is not combined with MVFFAddToFreeList because the latter
 * is also called when new segments are added under MVFFAlloc.
 */
static void MVFFFreeSegs(MVFF mvff, Addr base, Addr limit)
{
  Seg seg = NULL;       /* suppress "may be used uninitialized" */
  Arena arena;
  Bool b;
  Addr segLimit;  /* limit of the current segment when iterating */
  Addr segBase;   /* base of the current segment when iterating */
  Res res;

  AVERT(MVFF, mvff);
  AVER(base < limit);
  /* Could profitably AVER that the given range is free, */
  /* but the CBS doesn't provide that facility. */

  if (AddrOffset(base, limit) < mvff->minSegSize)
    return; /* not large enough for entire segments */

  arena = PoolArena(MVFF2Pool(mvff));
  b = SegOfAddr(&seg, arena, base);
  AVER(b);

  segBase = SegBase(seg);
  segLimit = SegLimit(seg);

  while(segLimit <= limit) { /* segment ends in range */
    if (segBase >= base) { /* segment starts in range */
      RangeStruct range, oldRange;
      RangeInit(&range, segBase, segLimit);

      res = CBSDelete(&oldRange, CBSOfMVFF(mvff), &range);
      if (res == ResOK) {
        mvff->free -= RangeSize(&range);
      } else if (ResIsAllocFailure(res)) {
        /* CBS ran out of memory for splay nodes, which must mean that
         * there were fragments on both sides: see
         * <design/cbs/#function.cbs.delete.fail>. Handle this by
         * deleting the whole of oldRange (which requires no
         * allocation) and re-inserting the fragments. */
        RangeStruct oldRange2;
        res = CBSDelete(&oldRange2, CBSOfMVFF(mvff), &oldRange);
        AVER(res == ResOK);
        AVER(RangesEqual(&oldRange2, &oldRange));
        mvff->free -= RangeSize(&oldRange);
        AVER(RangeBase(&oldRange) != segBase);
        {
          Addr leftBase = RangeBase(&oldRange);
          Addr leftLimit = segBase;
          res = MVFFAddToFreeList(&leftBase, &leftLimit, mvff);
        }
        AVER(RangeLimit(&oldRange) != segLimit);
        {
          Addr rightBase = segLimit;
          Addr rightLimit = RangeLimit(&oldRange);
          res = MVFFAddToFreeList(&rightBase, &rightLimit, mvff);
        }
      } else if (res == ResFAIL) {
        /* Not found in the CBS: must be found in the Freelist. */
        res = FreelistDelete(&oldRange, FreelistOfMVFF(mvff), &range);
        AVER(res == ResOK);
        mvff->free -= RangeSize(&range);
      }

      AVER(res == ResOK);
      AVER(RangesNest(&oldRange, &range));

      /* Can't free the segment earlier, because if it was on the
       * Freelist rather than the CBS then it likely contains data
       * that needs to be read in order to update the Freelist. */
      SegFree(seg);
      mvff->total -= RangeSize(&range);
    }

    /* Avoid calling SegNext if the next segment would fail */
    /* the loop test, mainly because there might not be a */
    /* next segment. */
    if (segLimit == limit) /* segment ends at end of range */
      break;

    b = SegFindAboveAddr(&seg, arena, segBase);
    AVER(b);
    segBase = SegBase(seg);
    segLimit = SegLimit(seg);
  }

  return;
}


/* MVFFAddSeg -- Allocates a new segment from the arena
 *
 * Allocates a new segment from the arena (with the given
 * withReservoirPermit flag) of at least the specified size.  The
 * specified size should be pool-aligned.  Adds it to the free list.
 */
static Res MVFFAddSeg(Seg *segReturn,
                      MVFF mvff, Size size, Bool withReservoirPermit)
{
  Pool pool;
  Arena arena;
  Size segSize;
  Seg seg;
  Res res;
  Align align;
  Addr base, limit;

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
    segSize = mvff->extendBy;
  else
    segSize = size;

  segSize = SizeAlignUp(segSize, align);

  res = SegAlloc(&seg, SegClassGet(), mvff->segPref, segSize, pool,
                 withReservoirPermit, argsNone);
  if (res != ResOK) {
    /* try again for a seg just large enough for object */
    /* see <design/poolmvff/#design.seg-fail> */
    segSize = SizeAlignUp(size, align);
    res = SegAlloc(&seg, SegClassGet(), mvff->segPref, segSize, pool,
                   withReservoirPermit, argsNone);
    if (res != ResOK) {
      return res;
    }
  }

  mvff->total += segSize;
  base = SegBase(seg);
  limit = AddrAdd(base, segSize);
  DebugPoolFreeSplat(pool, base, limit);
  res = MVFFAddToFreeList(&base, &limit, mvff);
  AVER(res == ResOK);
  AVER(base <= SegBase(seg));
  if (mvff->minSegSize > segSize) mvff->minSegSize = segSize;

  /* Don't call MVFFFreeSegs; that would be silly. */

  *segReturn = seg;
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

  FreelistFlushToCBS(FreelistOfMVFF(mvff), CBSOfMVFF(mvff));

  findDelete = mvff->slotHigh ? FindDeleteHIGH : FindDeleteLOW;

  foundBlock =
    (mvff->firstFit ? CBSFindFirst : CBSFindLast)
    (&range, &oldRange, CBSOfMVFF(mvff), size, findDelete);

  if (!foundBlock) {
    /* Failed to find a block in the CBS: try the emergency free list
     * as well. */
    foundBlock =
      (mvff->firstFit ? FreelistFindFirst : FreelistFindLast)
      (&range, &oldRange, FreelistOfMVFF(mvff), size, findDelete);
  }

  if (foundBlock) {
    *baseReturn = RangeBase(&range);
    *limitReturn = RangeLimit(&range);
    mvff->free -= size;
  }

  return foundBlock;
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
  AVERT(Bool, withReservoirPermit);

  size = SizeAlignUp(size, PoolAlignment(pool));

  foundBlock = MVFFFindFirstFree(&base, &limit, mvff, size);
  if (!foundBlock) {
    Seg seg;

    res = MVFFAddSeg(&seg, mvff, size, withReservoirPermit);
    if (res != ResOK)
      return res;
    foundBlock = MVFFFindFirstFree(&base, &limit, mvff, size);

    /* We know that the found range must intersect the new segment. */
    /* In particular, it doesn't necessarily lie entirely within it. */
    /* The next three AVERs test for intersection of two intervals. */
    AVER(base >= SegBase(seg) || limit <= SegLimit(seg));
    AVER(base < SegLimit(seg));
    AVER(SegBase(seg) < limit);

    /* We also know that the found range is no larger than the segment. */
    AVER(SegSize(seg) >= AddrOffset(base, limit));
  }
  AVER(foundBlock);
  AVER(AddrOffset(base, limit) == size);

  *aReturn = base;

  return ResOK;
}


/* MVFFFree -- free the given block */

static void MVFFFree(Pool pool, Addr old, Size size)
{
  Res res;
  Addr base, limit;
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  AVER(old != (Addr)0);
  AVER(AddrIsAligned(old, PoolAlignment(pool)));
  AVER(size > 0);

  size = SizeAlignUp(size, PoolAlignment(pool));
  base = old;
  limit = AddrAdd(base, size);

  res = MVFFAddToFreeList(&base, &limit, mvff);
  AVER(res == ResOK);
  if (res == ResOK)
    MVFFFreeSegs(mvff, base, limit);

  return;
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

  FreelistFlushToCBS(FreelistOfMVFF(mvff), CBSOfMVFF(mvff));

  if (CBSFindLargest(range, oldRange, CBSOfMVFF(mvff), size, findDelete))
    return TRUE;

  if (FreelistFindLargest(range, oldRange, FreelistOfMVFF(mvff),
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
  Seg seg = NULL;
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
    res = MVFFAddSeg(&seg, mvff, size, withReservoirPermit);
    if (res != ResOK)
      return res;
    found = MVFFFindLargest(&range, &oldRange, mvff, size, FindDeleteENTIRE);
  }
  AVER(found);

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
  Res res;
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  AVER(base <= limit);

  if (base == limit)
    return;

  res = MVFFAddToFreeList(&base, &limit, mvff);
  AVER(res == ResOK);
  if (res == ResOK)
    MVFFFreeSegs(mvff, base, limit);

  return;
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
  MVFF mvff;
  Arena arena;
  Res res;
  void *p;
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

  if (ArgPick(&arg, args, MPS_KEY_MVFF_SLOT_HIGH))
    slotHigh = arg.val.b;
  
  if (ArgPick(&arg, args, MPS_KEY_MVFF_ARENA_HIGH))
    arenaHigh = arg.val.b;
  
  if (ArgPick(&arg, args, MPS_KEY_MVFF_FIRST_FIT))
    firstFit = arg.val.b;

  AVER(extendBy > 0);           /* .arg.check */
  AVER(avgSize > 0);            /* .arg.check */
  AVER(avgSize <= extendBy);    /* .arg.check */
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
    mvff->minSegSize = ArenaAlign(arena);
  else
    mvff->minSegSize = extendBy;
  mvff->avgSize = avgSize;
  pool->alignment = align;
  mvff->slotHigh = slotHigh;
  mvff->firstFit = firstFit;

  res = ControlAlloc(&p, arena, sizeof(SegPrefStruct), FALSE);
  if (res != ResOK)
    return res;

  mvff->segPref = (SegPref)p;
  SegPrefInit(mvff->segPref);
  SegPrefExpress(mvff->segPref, arenaHigh ? SegPrefHigh : SegPrefLow, NULL);

  mvff->total = 0;
  mvff->free = 0;

  res = FreelistInit(FreelistOfMVFF(mvff), align);
  if (res != ResOK)
    goto failInit;

  res = CBSInit(CBSOfMVFF(mvff), arena, (void *)mvff, align,
                /* fastFind */ TRUE, /* zoned */ FALSE, args);
  if (res != ResOK)
    goto failInit;

  mvff->sig = MVFFSig;
  AVERT(MVFF, mvff);
  EVENT8(PoolInitMVFF, pool, arena, extendBy, avgSize, align,
                 slotHigh, arenaHigh, firstFit);
  return ResOK;

failInit:
  ControlFree(arena, p, sizeof(SegPrefStruct));
  return res;
}


/* MVFFFinish -- finish method for MVFF */

static void MVFFFinish(Pool pool)
{
  MVFF mvff;
  Arena arena;
  Seg seg;
  Ring ring, node, nextNode;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  ring = PoolSegRing(pool);
  RING_FOR(node, ring, nextNode) {
    seg = SegOfPoolRing(node);
    AVER(SegPool(seg) == pool);
    SegFree(seg);
  }

  /* Could maintain mvff->total here and check it falls to zero, */
  /* but that would just make the function slow.  If only we had */
  /* a way to do operations only if AVERs are turned on. */

  arena = PoolArena(pool);
  ControlFree(arena, mvff->segPref, sizeof(SegPrefStruct));

  CBSFinish(CBSOfMVFF(mvff));
  FreelistFinish(FreelistOfMVFF(mvff));

  mvff->sig = SigInvalid;
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

  res = CBSDescribe(CBSOfMVFF(mvff), stream);
  if (res != ResOK)
    return res;

  res = FreelistDescribe(FreelistOfMVFF(mvff), stream);
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
  CHECKD(SegPref, mvff->segPref);
  CHECKL(mvff->extendBy > 0);                   /* see .arg.check */
  CHECKL(mvff->minSegSize >= ArenaAlign(PoolArena(MVFF2Pool(mvff))));
  CHECKL(mvff->avgSize > 0);                    /* see .arg.check */
  CHECKL(mvff->avgSize <= mvff->extendBy);      /* see .arg.check */
  CHECKL(mvff->total >= mvff->free);
  CHECKL(SizeIsAligned(mvff->free, PoolAlignment(MVFF2Pool(mvff))));
  CHECKL(SizeIsAligned(mvff->total, ArenaAlign(PoolArena(MVFF2Pool(mvff)))));
  CHECKD(CBS, CBSOfMVFF(mvff));
  CHECKD(Freelist, FreelistOfMVFF(mvff));
  CHECKL(BoolCheck(mvff->slotHigh));
  CHECKL(BoolCheck(mvff->firstFit));
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

  return CBSOfMVFF(mvff);
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

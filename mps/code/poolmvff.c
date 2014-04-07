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
#include "failover.h"
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
  FailoverStruct foStruct;      /* fail-over mechanism */
  Bool firstFit;                /* as opposed to last fit */
  Bool slotHigh;                /* prefers high part of large block */
  Sig sig;                      /* <design/sig/> */
} MVFFStruct;


#define Pool2MVFF(pool)   PARENT(MVFFStruct, poolStruct, pool)
#define MVFF2Pool(mvff)       (&((mvff)->poolStruct))
#define CBSOfMVFF(mvff)      (&((mvff)->cbsStruct.landStruct))
#define FreelistOfMVFF(mvff) (&((mvff)->flStruct.landStruct))
#define FailoverOfMVFF(mvff) (&((mvff)->foStruct.landStruct))

static Bool MVFFCheck(MVFF mvff);


/* MVFFDebug -- MVFFDebug class */

typedef struct MVFFDebugStruct {
  MVFFStruct mvffStruct;         /* MVFF structure */
  PoolDebugMixinStruct debug;    /* debug mixin */
} MVFFDebugStruct;

typedef MVFFDebugStruct *MVFFDebug;


#define MVFF2MVFFDebug(mvff) PARENT(MVFFDebugStruct, mvffStruct, mvff)
#define MVFFDebug2MVFF(mvffd) (&((mvffd)->mvffStruct))


/* MVFFInsert -- add given range to free lists
 *
 * Updates MVFF counters for additional free space. Returns maximally
 * coalesced range containing given range. Does not attempt to free
 * segments (see MVFFFreeSegs).
 */
static Res MVFFInsert(Range rangeIO, MVFF mvff) {
  Res res;
  Size size;

  AVER(rangeIO != NULL);
  AVERT(MVFF, mvff);

  size = RangeSize(rangeIO);
  res = LandInsert(rangeIO, FailoverOfMVFF(mvff), rangeIO);

  if (res == ResOK)
    mvff->free += size;

  return res;
}


/* MVFFFreeSegs -- free segments from given range
 *
 * Given a free range, attempts to find entire segments within it, and
 * returns them to the arena, updating total size counter.
 *
 * This is usually called immediately after MVFFInsert. It is not
 * combined with MVFFInsert because the latter is also called when new
 * segments are added under MVFFAlloc.
 */
static void MVFFFreeSegs(MVFF mvff, Range range)
{
  Seg seg = NULL;       /* suppress "may be used uninitialized" */
  Arena arena;
  Bool b;
  Addr segLimit;  /* limit of the current segment when iterating */
  Addr segBase;   /* base of the current segment when iterating */
  Res res;

  AVERT(MVFF, mvff);
  AVERT(Range, range);
  /* Could profitably AVER that the given range is free, */
  /* but the CBS doesn't provide that facility. */

  if (RangeSize(range) < mvff->minSegSize)
    return; /* not large enough for entire segments */

  arena = PoolArena(MVFF2Pool(mvff));
  b = SegOfAddr(&seg, arena, RangeBase(range));
  AVER(b);

  segBase = SegBase(seg);
  segLimit = SegLimit(seg);

  while(segLimit <= RangeLimit(range)) { /* segment ends in range */
    if (segBase >= RangeBase(range)) { /* segment starts in range */
      RangeStruct delRange, oldRange;
      RangeInit(&delRange, segBase, segLimit);

      res = LandDelete(&oldRange, FailoverOfMVFF(mvff), &delRange);
      AVER(res == ResOK);
      AVER(RangesNest(&oldRange, &delRange));

      /* Can't free the segment earlier, because if it was on the
       * Freelist rather than the CBS then it likely contains data
       * that needs to be read in order to update the Freelist. */
      SegFree(seg);

      mvff->free -= RangeSize(&delRange);
      mvff->total -= RangeSize(&delRange);
    }

    /* Avoid calling SegNext if the next segment would fail */
    /* the loop test, mainly because there might not be a */
    /* next segment. */
    if (segLimit == RangeLimit(range)) /* segment ends at end of range */
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
 * withReservoirPermit flag) of at least the specified size. The
 * specified size should be pool-aligned. Adds it to the free lists.
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
  RangeStruct range;

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
  RangeInitSize(&range, SegBase(seg), segSize);
  DebugPoolFreeSplat(pool, RangeBase(&range), RangeLimit(&range));
  res = MVFFInsert(&range, mvff);
  AVER(res == ResOK);
  AVER(RangeBase(&range) <= SegBase(seg));
  if (mvff->minSegSize > segSize) mvff->minSegSize = segSize;

  /* Don't call MVFFFreeSegs; that would be silly. */

  *segReturn = seg;
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
    (rangeReturn, &oldRange, FailoverOfMVFF(mvff), size, findDelete);

  if (foundBlock) {
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
    Seg seg;

    res = MVFFAddSeg(&seg, mvff, size, withReservoirPermit);
    if (res != ResOK)
      return res;
    foundBlock = MVFFFindFree(&range, mvff, size);

    /* We know that the found range must intersect the new segment. */
    /* In particular, it doesn't necessarily lie entirely within it. */
    /* The next two AVERs test for intersection of two intervals. */
    AVER(RangeBase(&range) < SegLimit(seg));
    AVER(SegBase(seg) < RangeLimit(&range));

    /* We also know that the found range is no larger than the segment. */
    AVER(SegSize(seg) >= RangeSize(&range));
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
  RangeStruct range;
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);

  AVER(old != (Addr)0);
  AVER(AddrIsAligned(old, PoolAlignment(pool)));
  AVER(size > 0);

  RangeInitSize(&range, old, SizeAlignUp(size, PoolAlignment(pool)));

  res = MVFFInsert(&range, mvff);
  AVER(res == ResOK);
  if (res == ResOK)
    MVFFFreeSegs(mvff, &range);

  return;
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

  found = LandFindLargest(&range, &oldRange, FailoverOfMVFF(mvff), size, FindDeleteENTIRE);
  if (!found) {
    /* Add a new segment to the free lists and try again. */
    res = MVFFAddSeg(&seg, mvff, size, withReservoirPermit);
    if (res != ResOK)
      return res;
    found = LandFindLargest(&range, &oldRange, FailoverOfMVFF(mvff), size, FindDeleteENTIRE);
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
  RangeStruct range;

  AVERT(Pool, pool);
  mvff = Pool2MVFF(pool);
  AVERT(MVFF, mvff);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  RangeInit(&range, base, limit);

  if (RangeIsEmpty(&range))
    return;

  res = MVFFInsert(&range, mvff);
  AVER(res == ResOK);
  if (res == ResOK)
    MVFFFreeSegs(mvff, &range);

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
  Size align = MVFF_ALIGN_DEFAULT;
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
  AVER(SizeIsAligned(align, MPS_PF_ALIGN));
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

  res = LandInit(FreelistOfMVFF(mvff), FreelistLandClassGet(), arena, align, mvff, mps_args_none);
  if (res != ResOK)
    goto failFreelistInit;

  res = LandInit(CBSOfMVFF(mvff), CBSFastLandClassGet(), arena, align, mvff,
                 mps_args_none);
  if (res != ResOK)
    goto failCBSInit;

  MPS_ARGS_BEGIN(foArgs) {
    MPS_ARGS_ADD(foArgs, FailoverPrimary, CBSOfMVFF(mvff));
    MPS_ARGS_ADD(foArgs, FailoverSecondary, FreelistOfMVFF(mvff));
    res = LandInit(FailoverOfMVFF(mvff), FailoverLandClassGet(), arena, align, mvff, foArgs);
  } MPS_ARGS_END(foArgs);
  if (res != ResOK)
    goto failFailoverInit;

  mvff->sig = MVFFSig;
  AVERT(MVFF, mvff);
  EVENT8(PoolInitMVFF, pool, arena, extendBy, avgSize, align,
                 slotHigh, arenaHigh, firstFit);
  return ResOK;

failFailoverInit:
  LandFinish(CBSOfMVFF(mvff));
failCBSInit:
  LandFinish(FreelistOfMVFF(mvff));
failFreelistInit:
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

  LandFinish(FailoverOfMVFF(mvff));
  LandFinish(FreelistOfMVFF(mvff));
  LandFinish(CBSOfMVFF(mvff));

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

  res = LandDescribe(CBSOfMVFF(mvff), stream);
  if (res != ResOK)
    return res;

  res = LandDescribe(FreelistOfMVFF(mvff), stream);
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
  CHECKD(CBS, &mvff->cbsStruct);
  CHECKD(Freelist, &mvff->flStruct);
  CHECKD(Failover, &mvff->foStruct);
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

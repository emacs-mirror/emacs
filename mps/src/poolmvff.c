/* impl.c.poolmvff: First Fit Manual Variable Pool
 * 
 * $HopeName: MMsrc!poolmvff.c(trunk.6) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .purpose: This is a pool class for manually managed objects of
 * variable size where address-ordered first fit is an appropriate
 * policy.  Provision is made to allocate in reverse.  This pool
 * can allocate across segment boundaries.
 * 
 * .readership: MM developers
 * 
 * .design: design.mps.poolmvff
 */

#include "mpm.h"
#include "mpscmvff.h"
#include "dbgpool.h"

SRCID(poolmvff, "$HopeName: MMsrc!poolmvff.c(trunk.6) $");


/* Would go in poolmvff.h if the class had any MPS-internal clients. */
extern PoolClass PoolClassMVFF(void);


/* MVFFStruct -- MVFF (Manual Variable First Fit) pool outer structure
 *
 * The signature is placed at the end, see
 * design.mps.pool.outer-structure.sig
 */

#define MVFFSig           ((Sig)0x5193FFF9) /* SIGnature MVFF */

typedef struct MVFFStruct *MVFF;
typedef struct MVFFStruct {     /* MVFF pool outer structure */
  PoolStruct poolStruct;        /* generic structure */
  SegPref segPref;              /* the preferences for segments */
  Size extendBy;                /* segment size to extend pool by */
  Size avgSize;                 /* client estimate of allocation size */
  Size total;                   /* total bytes in pool */
  Size free;                    /* total free bytes in pool */
  CBSStruct cbsStruct;          /* free list */
  Bool firstFit;                /* as opposed to last fit */
  Bool slotHigh;                /* prefers high part of large block */
  Sig sig;                      /* design.mps.sig */
} MVFFStruct;


#define PoolPoolMVFF(pool)   PARENT(MVFFStruct, poolStruct, pool)
#define MVFFPool(mvff)       (&((mvff)->poolStruct))
#define CBSOfMVFF(mvff)      (&((mvff)->cbsStruct))
#define MVFFOfCBS(cbs)       PARENT(MVFFStruct, cbsStruct, cbs)

static Bool MVFFCheck(MVFF mvff);


/* MVFFDebug -- MVFFDebug class */

typedef struct MVFFDebugStruct {
  MVFFStruct mvffStruct;         /* MVFF structure */
  PoolDebugMixinStruct debug;    /* debug mixin */
} MVFFDebugStruct;

typedef MVFFDebugStruct *MVFFDebug;


#define MVFFPoolMVFFDebug(mvff) PARENT(MVFFDebugStruct, mvffStruct, mvff)
#define MVFFDebugPoolMVFF(mvffd) (&((mvffd)->mvffStruct))


/* MVFFAddToFreeList -- Add given range to free list 
 *
 * Updates MVFF counters for additional free space.
 * Returns maximally coalesced range containing given range.
 * Does not attempt to free segments (see MVFFFreeSegs).
 * Cannot(!) fail.
 */

static void MVFFAddToFreeList(Addr *baseIO, Addr *limitIO, MVFF mvff) {
  Res res;
  Addr base, limit;

  AVER(baseIO != NULL);
  AVER(limitIO != NULL);
  AVERT(MVFF, mvff);
  base = *baseIO;
  limit = *limitIO;
  AVER(limit > base);

  res = CBSInsertReturningRange(baseIO, limitIO, CBSOfMVFF(mvff), base, limit);
  AVER(res == ResOK);
  mvff->free += AddrOffset(base, limit);

  return;
}


/* MVFFRemoveFromFreeList -- Remove given range from free list
 *
 * Updates MVFF counters for reduced free space.
 * Cannot(!) fail.
 */

static void MVFFRemoveFromFreeList(MVFF mvff, Addr base, Addr limit) {
  Res res;

  AVERT(MVFF, mvff);
  AVER(limit > base);

  res = CBSDelete(CBSOfMVFF(mvff), base, limit);
  AVER(res == ResOK);
  mvff->free -= AddrOffset(base, limit);

  return;
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
  Seg seg;
  Arena arena;
  Bool b;
  Addr segLimit;  /* limit of the current segment when iterating */
  Addr segBase;   /* base of the current segment when iterating */

  AVERT(MVFF, mvff);
  AVER(base < limit);
  /* Could profitably AVER that the given range is free, */
  /* but the CBS doesn't provide that facility. */

  arena = PoolArena(MVFFPool(mvff));
  b = SegOfAddr(&seg, arena, base);
  AVER(b);

  segBase = SegBase(seg);
  segLimit = SegLimit(seg);

  while(segLimit <= limit) { /* segment ends in range */
    if(segBase >= base) { /* segment starts in range */
      /* Must remove from free list first, in case free list */
      /* is using inline datastructures. */
      MVFFRemoveFromFreeList(mvff, segBase, segLimit);
      mvff->total -= AddrOffset(segBase, segLimit);
      SegFree(seg);
    }

    /* Avoid calling SegNext if the next segment would fail */
    /* the loop test, mainly because there might not be a */
    /* next segment. */
    if(segLimit == limit) /* segment ends at end of range */
      break;

    b = SegNext(&seg, arena, segBase);
    AVER(b);
    segBase = SegBase(seg);
    segLimit = SegLimit(seg);
  }

  return;
}


/* MVFFAddSeg -- Allocates a new segment from the arena
 *
 * Allocates a new segment from the arena (with the given 
 * withReservoirPermit flag) of at least the specified size.
 * The specified size should be pool aligned.  Adds it to
 * the free list.
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
  AVER(BoolCheck(withReservoirPermit));

  pool = MVFFPool(mvff);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);

  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* use extendBy unless it's too small.
   * see design.mps.poolmvff.design.seg-size */
  if(size <= mvff->extendBy)
    segSize = mvff->extendBy;
  else
    segSize = size;

  segSize = SizeAlignUp(segSize, align);

  res = SegAlloc(&seg, mvff->segPref, segSize, pool, 
                 withReservoirPermit);
  if(res != ResOK) {
    /* try again for a seg just large enough for object */
    /* see design.mps.poolmvff.design.seg-fail */
    segSize = SizeAlignUp(size, align);
    res = SegAlloc(&seg, mvff->segPref, segSize, pool, 
                   withReservoirPermit);
    if (res != ResOK) {
      return res;
    }
  }
  mvff->total += segSize;

  SegSetP(seg, (void*)0);

  base = SegBase(seg);
  limit = SegLimit(seg);
  MVFFAddToFreeList(&base, &limit, mvff);
  AVER(base <= SegBase(seg));
  AVER(limit >= SegLimit(seg));

  /* Don't call MVFFFreeSegs; that would be silly. */

  if(res == ResOK)
    *segReturn = seg;

  return res;
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
  Addr base, limit;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(MVFF, mvff);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(MVFFPool(mvff))));

  if(mvff->firstFit) {
    foundBlock = CBSFindFirst(&base, &limit, CBSOfMVFF(mvff), size);
  } else {
    foundBlock = CBSFindLast(&base, &limit, CBSOfMVFF(mvff), size);
  }

  /* CBSFind* returns a possibly larger block. */
  if(foundBlock) {
    if(mvff->slotHigh) { /* allocate in top of block */
      *limitReturn = limit;
      *baseReturn = AddrSub(limit, size);
    } else { /* allocate in bottom of block */
      *baseReturn = base;
      *limitReturn = AddrAdd(base, size);
    }
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
  mvff = PoolPoolMVFF(pool);
  AVERT(MVFF, mvff);

  AVER(aReturn != NULL);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  size = SizeAlignUp(size, PoolAlignment(pool));

  foundBlock = MVFFFindFirstFree(&base, &limit, mvff, size);
  if(!foundBlock) {
    Seg seg;

    res = MVFFAddSeg(&seg, mvff, size, withReservoirPermit);
    if(res != ResOK) 
      return res;
    foundBlock = MVFFFindFirstFree(&base, &limit, mvff, size);
    /* We know that the found range must intersect the new segment. */
    /* In particular, it doesn't necessarily lie entirely within it. */
    AVER(base >= SegBase(seg) || limit <= SegLimit(seg));
  }
  AVER(foundBlock);

  MVFFRemoveFromFreeList(mvff, base, limit);
  *aReturn = base;

  return ResOK;
}


/* MVFFFree -- free the given block */

static void MVFFFree(Pool pool, Addr old, Size size)
{
  Addr base, limit;
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = PoolPoolMVFF(pool);
  AVERT(MVFF, mvff);

  AVER(old != (Addr)0);
  AVER(AddrIsAligned(old, PoolAlignment(pool)));
  AVER(size > 0);

  size = SizeAlignUp(size, PoolAlignment(pool));
  base = old;
  limit = AddrAdd(base, size);


  MVFFAddToFreeList(&base, &limit, mvff);

  MVFFFreeSegs(mvff, base, limit);
}


/* == Initialize == */

static Res MVFFInit(Pool pool, va_list arg)
{
  Size extendBy, avgSize, align;
  Bool slotHigh, arenaHigh, firstFit;
  MVFF mvff;
  Arena arena;
  Res res;
  void *p;
  RefSet refset;

  AVERT(Pool, pool);

  /* .arg: class-specific additional arguments; see */
  /* design.mps.poolmvff.method.init */
  /* .arg.check: we do the same checks here and in MVFFCheck */ 
  /* except for arenaHigh, which is stored only in the segPref. */
  extendBy = va_arg(arg, Size);
  avgSize = va_arg(arg, Size);
  align = va_arg(arg, Size);
  slotHigh = va_arg(arg, Bool);
  arenaHigh = va_arg(arg, Bool);
  firstFit = va_arg(arg, Bool);
  AVER(extendBy > 0);           /* .arg.check */
  AVER(avgSize > 0);            /* .arg.check */
  AVER(avgSize <= extendBy);    /* .arg.check */
  AVER(BoolCheck(slotHigh));
  AVER(BoolCheck(arenaHigh));
  AVER(BoolCheck(firstFit));

  mvff = PoolPoolMVFF(pool);
  arena = PoolArena(pool);

  mvff->extendBy = extendBy;
  mvff->avgSize = avgSize;
  pool->alignment = align;
  mvff->slotHigh = slotHigh;
  mvff->firstFit = firstFit;

  res = ArenaAlloc(&p, arena, sizeof(SegPrefStruct));
  if (res != ResOK) {
    return res;
  }
  
  mvff->segPref = (SegPref)p;
  *mvff->segPref = *SegPrefDefault();
  SegPrefExpress(mvff->segPref, arenaHigh ? SegPrefHigh : SegPrefLow, NULL);
  /* If using refset placement, just put it apart from the others. */
  refset = RefSetComp(ARENA_DEFAULT_REFSET);
  SegPrefExpress(mvff->segPref, SegPrefRefSet, (void *)&refset);

  mvff->total = 0;
  mvff->free = 0;

  CBSInit(arena, CBSOfMVFF(mvff), NULL, NULL, NULL, NULL, 
          mvff->extendBy, align, TRUE, TRUE);

  mvff->sig = MVFFSig;
  AVERT(MVFF, mvff);
  return ResOK;
}


static void MVFFFinish(Pool pool)
{
  MVFF mvff;
  Arena arena;
  Seg seg;
  Ring ring, node, nextNode;

  AVERT(Pool, pool);
  mvff = PoolPoolMVFF(pool);
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
  ArenaFree(arena, mvff->segPref, sizeof(SegPrefStruct));

  CBSFinish(CBSOfMVFF(mvff));

  mvff->sig = SigInvalid;
}


/* MVFFDebugMixin - find debug mixin in class MVFFDebug */

static PoolDebugMixin MVFFDebugMixin(Pool pool)
{
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = PoolPoolMVFF(pool);
  AVERT(MVFF, mvff);
  /* Can't check MVFFDebug, because this is called during init */
  return &(MVFFPoolMVFFDebug(mvff)->debug);
}


static Res MVFFDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  MVFF mvff;

  AVERT(Pool, pool);
  mvff = PoolPoolMVFF(pool);
  AVERT(MVFF, mvff);
  AVER(stream != NULL);

  res = WriteF(stream,
               "  extendBy  $W\n",  (WriteFW)mvff->extendBy,
               "  avgSize   $W\n",  (WriteFW)mvff->avgSize,
               "  total     $U\n",  (WriteFU)mvff->total,
               "  free      $U\n",  (WriteFU)mvff->free,
               NULL);
  return res;               
}


static PoolClassStruct PoolClassMVFFStruct = {
  PoolClassSig,
  "MVFF",                               /* name */
  sizeof(MVFFStruct),                   /* size */
  offsetof(MVFFStruct, poolStruct),     /* offset */
  NULL,                                 /* superclass */
  AttrALLOC | AttrFREE,                 /* attr */
  MVFFInit,                             /* init */
  MVFFFinish,                           /* finish */
  MVFFAlloc,                            /* alloc */
  MVFFFree,                             /* free */
  PoolNoBufferInit,                     /* bufferInit */
  PoolNoBufferFill,                     /* bufferFill */
  PoolNoBufferEmpty,                    /* bufferEmpty */
  PoolNoBufferFinish,                   /* bufferFinish */
  PoolNoTraceBegin,                     /* traceBegin */
  PoolNoAccess,                         /* access */
  PoolNoWhiten,                         /* whiten */
  PoolNoGrey,                           /* mark */
  PoolNoBlacken,                        /* blacken */
  PoolNoScan,                           /* scan */
  PoolNoFix,                            /* fix */
  PoolNoFix,                            /* emergencyFix */
  PoolNoReclaim,                        /* relcaim */
  PoolNoBenefit,                        /* benefit */
  PoolNoAct,                            /* act */
  PoolNoRampBegin,
  PoolNoRampEnd,
  PoolNoWalk,                           /* walk */
  MVFFDescribe,                         /* describe */
  PoolNoDebugMixin, 
  PoolClassSig                          /* impl.h.mpmst.class.end-sig */
};

PoolClass PoolClassMVFF(void)
{
  return &PoolClassMVFFStruct;
}


/* Pool class MVFFDebug */

static PoolClassStruct poolClassMVFFDebugStruct;

static PoolClass poolClassMVFFDebug(void)
{
  /* This code has to be idempotent to avoid locking. */
  EnsureDebugClass(&poolClassMVFFDebugStruct, PoolClassMVFF());
  poolClassMVFFDebugStruct.name = "MVFFDBG";
  poolClassMVFFDebugStruct.size = sizeof(MVFFDebugStruct);
  poolClassMVFFDebugStruct.debugMixin = MVFFDebugMixin;
  return &poolClassMVFFDebugStruct;
}


/* MPS Interface Extensions. */

mps_class_t mps_class_mvff(void)
{
  return (mps_class_t)(PoolClassMVFF());
}

mps_class_t mps_class_mvff_debug(void)
{
  return (mps_class_t)(poolClassMVFFDebug());
}


/* Total free bytes. See design.mps.poolmvff.design.arena-enter */

size_t mps_mvff_free_size(mps_pool_t mps_pool)
{
  Pool pool;
  MVFF mvff;

  pool = (Pool)mps_pool;
  AVERT(Pool, pool);
  mvff = PoolPoolMVFF(pool);
  AVERT(MVFF, mvff);
  
  return (size_t)mvff->free;
}

/* Total owned bytes. See design.mps.poolmvff.design.arena-enter */

size_t mps_mvff_size(mps_pool_t mps_pool)
{
  Pool pool;
  MVFF mvff;

  pool = (Pool)mps_pool;
  AVERT(Pool, pool);
  mvff = PoolPoolMVFF(pool);
  AVERT(MVFF, mvff);

  return (size_t)mvff->total;
} 


/* MVFFCheck -- check the consistency of an MVFF structure */

static Bool MVFFCheck(MVFF mvff)
{
  CHECKS(MVFF, mvff);
  CHECKD(Pool, MVFFPool(mvff));
  CHECKL(MVFFPool(mvff)->class == &PoolClassMVFFStruct
         || MVFFPool(mvff)->class == &poolClassMVFFDebugStruct);
  CHECKD(SegPref, mvff->segPref);
  CHECKL(mvff->extendBy > 0);                   /* see .arg.check */
  CHECKL(mvff->avgSize > 0);                    /* see .arg.check */
  CHECKL(mvff->avgSize <= mvff->extendBy);      /* see .arg.check */
  /* the free and lost bytes are disjoint subsets of the total */
  CHECKL(BoolCheck(mvff->slotHigh));
  CHECKL(BoolCheck(mvff->firstFit));

  CHECKL(mvff->total >= mvff->free);
  CHECKL(SizeIsAligned(mvff->free, PoolAlignment(MVFFPool(mvff)))); 
  CHECKL(SizeIsAligned(mvff->total, ArenaAlign(PoolArena(MVFFPool(mvff)))));
  CHECKD(CBS, CBSOfMVFF(mvff));
  return TRUE;
}

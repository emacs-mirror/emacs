/* impl.c.poolmvff: First Fit Manual Variable Pool
 * 
 * $HopeName: MMsrc!poolmvff.c(trunk.3) $
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

SRCID(poolmvff, "$HopeName: MMsrc!poolmvff.c(trunk.3) $");


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
  Size lost;                    /* total lost bytes in pool */
  CBSStruct cbsStruct;          /* free list */
  Bool firstFit;                /* as opposed to last fit */
  Bool slotHigh;                /* prefers high part of large block */
  Bool freeRangeToCheck;        /* free range to check for entire segs */
  Addr freeRangeBase;           /* base of free range to check */
  Addr freeRangeLimit;          /* limit of free range to check */
  Sig sig;                      /* design.mps.sig */
} MVFFStruct;


#define PoolPoolMVFF(pool)   PARENT(MVFFStruct, poolStruct, pool)
#define MVFFPool(mvff)       (&((mvff)->poolStruct))
#define CBSOfMVFF(mvff)      (&((mvff)->cbsStruct))
#define MVFFOfCBS(cbs)       PARENT(MVFFStruct, cbsStruct, cbs)

static Bool MVFFCheck(MVFF mvff);

/* MVFFMinSegSize -- Minimum size of all MVFF segments */
static Size MVFFMinSegSize(MVFF mvff) {
  /* Used from MVFFCheck */

  return mvff->extendBy;
}

static Res MVFFAddToFreeList(MVFF mvff, Addr base, Addr limit) {
  Res res;

  AVERT(MVFF, mvff);
  AVER(limit > base);

  res = CBSInsert(CBSOfMVFF(mvff), base, limit);
  if(res == ResOK) {
    mvff->free += AddrOffset(base, limit);
  } else {
    mvff->lost += AddrOffset(base, limit);
  }

  return res;
}

static Res MVFFRemoveFromFreeList(MVFF mvff, Addr base, Addr limit) {
  Res res;

  AVERT(MVFF, mvff);
  AVER(limit > base);

  res = CBSDelete(CBSOfMVFF(mvff), base, limit);
  AVER(res == ResOK);
  if(res == ResOK) {
    mvff->free -= AddrOffset(base, limit);
  }
  if(mvff->freeRangeToCheck) {
    /* We know there can't be any whole segs. */
    mvff->freeRangeToCheck = FALSE;
    mvff->freeRangeBase = mvff->freeRangeLimit = (Addr)0;
  }

  return res;
}


/* MVFFNoteRange -- Note large free range as a candidiate for SegFree 
 *
 * This is called as a new or grow callback method from the CBS. */

static void MVFFNoteRange(CBS cbs, CBSBlock block, Size oldSize, Size newSize)
{
  MVFF mvff;

  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);
  AVER(newSize >= MVFFMinSegSize(MVFFOfCBS(cbs)));
  UNUSED(oldSize);

  mvff = MVFFOfCBS(cbs);
  AVER(!mvff->freeRangeToCheck);

  mvff->freeRangeToCheck = TRUE;
  mvff->freeRangeBase = CBSBlockBase(block);
  mvff->freeRangeLimit = CBSBlockLimit(block);

  return;
}

/* MVFFFreeSegs -- Free segs from range
 *
 * Given a large range recently freed that is cached in the MVFF,
 * Find entire segments and return them to the arena.
 */

static void MVFFFreeSegs(MVFF mvff) 
{
  Addr base, limit;
  Seg seg;
  Arena arena;
  Bool b;
  Res res;
  Pool pool;
  Addr segLimit;  /* limit of the current segment when iterating */
  Addr freeBase;  /* base of the area returned to the arena */
  Addr freeLimit; /* end of the area returned to the arena */

  AVERT(MVFF, mvff);
  AVER(mvff->freeRangeToCheck);

  base = mvff->freeRangeBase;
  limit = mvff->freeRangeLimit;

  mvff->freeRangeToCheck = FALSE;
  mvff->freeRangeBase = mvff->freeRangeLimit = (Addr)0;
  
  pool = MVFFPool(mvff);
  arena = PoolArena(pool);
  b = SegOfAddr(&seg, arena, base);
  AVER(b);

  segLimit = SegLimit(seg);
  if(segLimit > limit) /* then there are no segs to free */
    goto done;

  freeLimit = (Addr)0;  /* avoid compiler warning */

  freeBase = SegBase(seg);
  if(freeBase < base) { /* then we can't free this first seg */
    Bool bb;
    if(segLimit == limit) /* no segs to free */
      goto done;
    bb = SegNext(&seg, arena, freeBase); /* move on to the next seg */
    /* Can't have been the last segment, because the block */
    /* overlaps the end, so there must be another segment */
    AVER(bb);
    segLimit = SegLimit(seg);
    freeBase = SegBase(seg);
  }

  AVER(freeBase >= base);
  AVER(SegPool(seg) == pool);

  if (limit >= segLimit) { /* then we have some segs to free */
    while(limit > segLimit) { /* free a seg and move on */
      Addr segBase;
      Seg segNext;
      Bool bb;

      segBase = SegBase(seg);
      bb = SegNext(&segNext, arena, segBase);
      /* Can't have been the last segment, because the block */
      /* overlaps the end, so there must be another segment */
      AVER(bb);
      AVER(SegPool(segNext) == pool);
      SegFree(seg);
      freeLimit = segLimit;
      seg = segNext;
      segLimit = SegLimit(seg);
    }
    if (limit == segLimit) { /* then free the last seg */
      SegFree(seg);
      freeLimit = segLimit;
    }
    
    res = MVFFRemoveFromFreeList(mvff, freeBase, freeLimit);
    AVER(res == ResOK);
    mvff->total -= AddrOffset(freeBase, freeLimit);
  }

done:
  return;
}


/* MVFFAddSeg(mvff, size, withReservoirPermit) gets a new segment from */
/* the arena, large enough for an object of size 'size', and adds it */
/* to the pool by calling MVFFAddToFreeList(). */

static Res MVFFAddSeg(MVFF mvff, Size size, Bool withReservoirPermit)
{
  Pool pool;
  Arena arena;
  Size segSize;
  Seg seg;
  Res res;
  Align align;

  AVERT(MVFF, mvff);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  pool = MVFFPool(mvff);
  arena = PoolArena(pool);
  align = ArenaAlign(arena);

  AVER(SizeIsAligned(size, pool->alignment));

  /* use extendBy unless it's too small.
   * see design.mps.poolmvff.design.seg-size */
  if(size <= mvff->extendBy)
    segSize = mvff->extendBy;
  else
    segSize = size;

  segSize = SizeAlignUp(segSize, align);

  AVER(segSize >= MVFFMinSegSize(mvff));
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

  /* There shouldn't be any possible free segments hanging around */
  /* here. */
  AVER(!mvff->freeRangeToCheck);

  res = MVFFAddToFreeList(mvff, SegBase(seg), SegLimit(seg));
  AVER(res == ResOK);

  /* We don't want to remove the entirely free segment from the */
  /* free list. */
  if(mvff->freeRangeToCheck) {
    mvff->freeRangeToCheck = FALSE;
    mvff->freeRangeBase = mvff->freeRangeLimit = (Addr)0;
  }

  return res;
}

/* Finds the first free block on the free list that is big enough */
/* to accommodate the request.  The free list is extended by */
/* adding a segment if necessary. */

static Bool MVFFFindFirstFree(Addr *baseReturn, Addr *limitReturn,
                              MVFF mvff, Size size)
{
  Bool foundBlock;
  Addr base, limit;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  /* other parms checked by callers */

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


/*  == Allocate ==  */

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

  size = SizeAlignUp(size, pool->alignment);

  foundBlock = MVFFFindFirstFree(&base, &limit, mvff, size);
  if(!foundBlock) {
    res = MVFFAddSeg(mvff, size, withReservoirPermit);
    if(res != ResOK) 
      return res;
    foundBlock = MVFFFindFirstFree(&base, &limit, mvff, size);
  }
  AVER(foundBlock);

  res = MVFFRemoveFromFreeList(mvff, base, limit);
  AVER(res == ResOK);
  *aReturn = base;

  return ResOK;
}


/* MVFFFree -- free the given block */

static void MVFFFree(Pool pool, Addr old, Size size)
{
  Addr base, limit;
  MVFF mvff;
  Res res;

  AVERT(Pool, pool);
  mvff = PoolPoolMVFF(pool);
  AVERT(MVFF, mvff);

  AVER(old != (Addr)0);
  AVER(AddrIsAligned(old, pool->alignment));
  AVER(size > 0);

  size = SizeAlignUp(size, pool->alignment);
  base = old;
  limit = AddrAdd(base, size);

  res = MVFFAddToFreeList(mvff, base, limit);
  AVER(res == ResOK);
  if (res != ResOK)
    return;
  if(mvff->freeRangeToCheck) 
    MVFFFreeSegs(mvff);
  AVER(!mvff->freeRangeToCheck);
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
  
  mvff->freeRangeToCheck = FALSE;
  mvff->freeRangeBase = mvff->freeRangeLimit = (Addr)0;

  mvff->segPref = (SegPref)p;
  *mvff->segPref = *SegPrefDefault();
  SegPrefExpress(mvff->segPref, arenaHigh ? SegPrefHigh : SegPrefLow, NULL);
  /* If using refset placement, just put it apart from the others. */
  refset = RefSetComp(ARENA_DEFAULT_REFSET);
  SegPrefExpress(mvff->segPref, SegPrefRefSet, (void *)&refset);

  mvff->total = 0;
  mvff->free = 0;
  mvff->lost = 0;

  CBSInit(arena, CBSOfMVFF(mvff), &MVFFNoteRange, NULL,
          &MVFFNoteRange, NULL, mvff->extendBy, align, TRUE, TRUE);

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
               "  lost      $U\n",  (WriteFU)mvff->lost,
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


/* MPS Interface Extensions. */

mps_class_t mps_class_mvff(void)
{
  return (mps_class_t)(PoolClassMVFF());
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
  CHECKL(MVFFPool(mvff)->class == &PoolClassMVFFStruct);
  CHECKD(SegPref, mvff->segPref);
  CHECKL(mvff->extendBy > 0);                   /* see .arg.check */
  CHECKL(mvff->avgSize > 0);                    /* see .arg.check */
  CHECKL(mvff->avgSize <= mvff->extendBy);      /* see .arg.check */
  /* the free and lost bytes are disjoint subsets of the total */
  CHECKL(BoolCheck(mvff->slotHigh));
  CHECKL(BoolCheck(mvff->firstFit));

  CHECKL(BoolCheck(mvff->freeRangeToCheck));
  if(mvff->freeRangeToCheck) {
    CHECKL(mvff->freeRangeBase < mvff->freeRangeLimit);
    CHECKL(AddrOffset(mvff->freeRangeBase, mvff->freeRangeLimit) 
           >= MVFFMinSegSize(mvff));
  } else {
    CHECKL(mvff->freeRangeBase == (Addr)0);
    CHECKL(mvff->freeRangeLimit == (Addr)0);
  }

  CHECKL(mvff->total >= mvff->free + mvff->lost);
  CHECKL(SizeIsAligned(mvff->free, MVFFPool(mvff)->alignment)); 
  CHECKL(SizeIsAligned(mvff->lost, MVFFPool(mvff)->alignment));
  CHECKL(SizeIsAligned(mvff->total, ArenaAlign(PoolArena(MVFFPool(mvff)))));
  CHECKD(CBS, CBSOfMVFF(mvff));
  return TRUE;
}

/* impl.c.poolmv2: MANUAL VARIABLE-SIZED TEMPORAL POOL
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .purpose: A manual-variable pool designed to take advantage of
 * placement according to predicted deathtime.
 *
 * .design: See design.mps.poolmv2.
 */

#include "mpm.h"
#include "poolmv2.h"
#include "mpscmv2.h"
#include "abq.h"
#include "cbs.h"
#include "meter.h"

SRCID(poolmv2, "$Id$");


/* Signatures */

#define MVTSig ((Sig)0x5193F299) /* SIGnature MVT */


/* Private prototypes */

typedef struct MVTStruct *MVT;
static Res MVTInit(Pool pool, va_list arg);
static Bool MVTCheck(MVT mvt);
static void MVTFinish(Pool pool);
static Res MVTBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size minSize,
                         Bool withReservoirPermit);
static void MVTBufferEmpty(Pool pool, Buffer buffer, Addr base, Addr limit);
static void MVTFree(Pool pool, Addr base, Size size);
static Res MVTDescribe(Pool pool, mps_lib_FILE *stream);
static Res MVTSegAlloc(Seg *segReturn, MVT mvt, Size size, Pool pool,
                       Bool withReservoirPermit);

static void MVTSegFree(MVT mvt, Seg seg);
static Bool MVTReturnBlockSegs(MVT mvt, CBSBlock block, Arena arena);
static void MVTNoteNew(CBS cbs, CBSBlock block, Size oldSize, Size newSize);
static void MVTNoteDelete(CBS cbs, CBSBlock block, Size oldSize, Size newSize);
static void ABQRefillIfNecessary(MVT mvt, Size size);
static Bool ABQRefillCallback(CBS cbs, CBSBlock block, void *closureP);
static Res MVTContingencySearch(CBSBlock *blockReturn, CBS cbs, Size min);
static Bool MVTContingencyCallback(CBS cbs, CBSBlock block, void *closureP);
static Bool MVTCheckFit(CBSBlock block, Size min, Arena arena);
static ABQ MVTABQ(MVT mvt);
static CBS MVTCBS(MVT mvt);
static MVT CBSMVT(CBS cbs);
static SegPref MVTSegPref(MVT mvt);


/* Types */


typedef struct MVTStruct
{
  PoolStruct poolStruct;
  CBSStruct cbsStruct;          /* The coalescing block structure */
  ABQStruct abqStruct;          /* The available block queue */
  SegPrefStruct segPrefStruct;  /* The preferences for segments */
  /* design.mps.poolmvt:arch.parameters */
  Size minSize;                 /* Pool parameter */
  Size meanSize;                /* Pool parameter */
  Size maxSize;                 /* Pool parameter */
  Count fragLimit;              /* Pool parameter */
  /* design.mps.poolmvt:arch.overview.abq.reuse.size */
  Size reuseSize;               /* Size at which blocks are recycled */
  /* design.mps.poolmvt:arch.ap.fill.size */
  Size fillSize;                /* Size of pool segments */
  /* design.mps.poolmvt:arch.contingency */
  Size availLimit;              /* Limit on available */
  /* design.mps.poolmvt:impl.c.free.merge.segment.overflow */
  Bool abqOverflow;             /* ABQ dropped some candidates */
  /* design.mps.poolmvt:arch.ap.no-fit.* */
  Bool splinter;                /* Saved splinter */
  Seg splinterSeg;              /* Saved splinter seg */
  Addr splinterBase;            /* Saved splinter base */
  Addr splinterLimit;           /* Saved splinter size */

  /* pool accounting --- one of these first four is redundant, but
     size and available are used to implement fragmentation policy */
  Size size;                    /* size of segs in pool */
  Size allocated;               /* bytes allocated to mutator */
  Size available;               /* bytes available for allocation */
  Size unavailable;             /* bytes lost to fragmentation */
 
  /* pool meters*/
  METER_DECL(segAllocs);
  METER_DECL(segFrees);
  METER_DECL(bufferFills);
  METER_DECL(bufferEmpties);
  METER_DECL(poolFrees);
  METER_DECL(poolSize);
  METER_DECL(poolAllocated);
  METER_DECL(poolAvailable);
  METER_DECL(poolUnavailable);
  METER_DECL(poolUtilization);
  /* abq meters */
  METER_DECL(finds);
  METER_DECL(overflows);
  METER_DECL(underflows);
  METER_DECL(refills);
  METER_DECL(refillPushes);
  METER_DECL(refillOverflows);
  METER_DECL(refillReturns);
  /* fragmentation meters */
  METER_DECL(perfectFits);
  METER_DECL(firstFits);
  METER_DECL(secondFits);
  METER_DECL(failures);
  /* contingency meters */
  METER_DECL(emergencyContingencies);
  METER_DECL(fragLimitContingencies);
  METER_DECL(contingencySearches);
  METER_DECL(contingencyHardSearches);
  /* splinter meters */
  METER_DECL(splinters);
  METER_DECL(splintersUsed);
  METER_DECL(splintersDropped);
  METER_DECL(sawdust);
  /* exception meters */
  METER_DECL(exceptions);
  METER_DECL(exceptionSplinters);
  METER_DECL(exceptionReturns);
 
  Sig sig;
} MVTStruct;


DEFINE_POOL_CLASS(MVTPoolClass, this)
{
  INHERIT_CLASS(this, AbstractSegBufPoolClass);
  this->name = "MVT";
  this->size = sizeof(MVTStruct);
  this->offset = offsetof(MVTStruct, poolStruct);
  this->attr |= AttrFREE;
  this->init = MVTInit;
  this->finish = MVTFinish;
  this->free = MVTFree;
  this->bufferFill = MVTBufferFill;
  this->bufferEmpty = MVTBufferEmpty;
  this->describe = MVTDescribe;
}

/* Macros */


/* .trans.something: the C language sucks */
#define unless(cond) if (!(cond))
#define when(cond) if (cond)


#define Pool2MVT(pool) PARENT(MVTStruct, poolStruct, pool)
#define MVT2Pool(mvt) (&(mvt)->poolStruct)


/* Accessors */


static ABQ MVTABQ(MVT mvt)
{
  return &mvt->abqStruct;
}


static CBS MVTCBS(MVT mvt)
{
  return &mvt->cbsStruct;
}


static MVT CBSMVT(CBS cbs)
{
  return PARENT(MVTStruct, cbsStruct, cbs);
}


static SegPref MVTSegPref(MVT mvt)
{
  return &mvt->segPrefStruct;
}


/* Methods */


/* MVTInit -- initialize an MVT pool
 *
 * Parameters are:
 * minSize, meanSize, maxSize, reserveDepth, fragLimit
 */
static Res MVTInit(Pool pool, va_list arg)
{
  Arena arena;
  Size minSize, meanSize, maxSize, reuseSize, fillSize;
  Count reserveDepth, abqDepth, fragLimit;
  MVT mvt;
  Res res;

  AVERT(Pool, pool);
  mvt = Pool2MVT(pool);
  /* can't AVERT mvt, yet */
  arena = PoolArena(pool);
  AVERT(Arena, arena);
 
  /* --- Should there be a ResBADARG ? */
  minSize = va_arg(arg, Size);
  unless (minSize > 0)
    return ResLIMIT;
  meanSize = va_arg(arg, Size);
  unless (meanSize >= minSize)
    return ResLIMIT;
  maxSize = va_arg(arg, Size);
  unless (maxSize >= meanSize)
    return ResLIMIT;
  /* --- check that maxSize is not too large */
  reserveDepth = va_arg(arg, Count);
  unless (reserveDepth > 0)
    return ResLIMIT;
  /* --- check that reserveDepth is not too large or small */
  fragLimit = va_arg(arg, Count);
  unless (fragLimit <= 100)
    return ResLIMIT;

  /* see design.mps.poolmvt:arch.parameters */
  fillSize = SizeAlignUp(maxSize, ArenaAlign(arena));
  /* see design.mps.poolmvt:arch.fragmentation.internal */
  reuseSize = 2 * fillSize;
  abqDepth = (reserveDepth * meanSize + reuseSize - 1) / reuseSize;
  /* keep the abq from being useless */
  if (abqDepth < 3)
    abqDepth = 3;

  res = CBSInit(arena, MVTCBS(mvt), (void *)mvt, &MVTNoteNew, &MVTNoteDelete,
                NULL, NULL, reuseSize, MPS_PF_ALIGN, TRUE, FALSE);
  if (res != ResOK)
    goto failCBS;
 
  res = ABQInit(arena, MVTABQ(mvt), (void *)mvt, abqDepth);
  if (res != ResOK)
    goto failABQ;

  {
    ZoneSet zones;
    /* --- Loci needed here, what should the pref be? */
    *MVTSegPref(mvt) = *SegPrefDefault();
    zones = ZoneSetComp(ArenaDefaultZONESET);
    SegPrefExpress(MVTSegPref(mvt), SegPrefZoneSet, (void *)&zones);
  }

  mvt->reuseSize = reuseSize;
  mvt->fillSize = fillSize;
  mvt->abqOverflow = FALSE;
  mvt->minSize = minSize;
  mvt->meanSize = meanSize;
  mvt->maxSize = maxSize;
  mvt->fragLimit = fragLimit;
  mvt->splinter = FALSE;
  mvt->splinterSeg = NULL;
  mvt->splinterBase = (Addr)0;
  mvt->splinterLimit = (Addr)0;
 
  /* accounting */
  mvt->size = 0;
  mvt->allocated = 0;
  mvt->available = 0;
  mvt->availLimit = 0;
  mvt->unavailable = 0;
 
  /* meters*/
  METER_INIT(mvt->segAllocs, "segment allocations", (void *)mvt);
  METER_INIT(mvt->segFrees, "segment frees", (void *)mvt);
  METER_INIT(mvt->bufferFills, "buffer fills", (void *)mvt);
  METER_INIT(mvt->bufferEmpties, "buffer empties", (void *)mvt);
  METER_INIT(mvt->poolFrees, "pool frees", (void *)mvt);
  METER_INIT(mvt->poolSize, "pool size", (void *)mvt);
  METER_INIT(mvt->poolAllocated, "pool allocated", (void *)mvt);
  METER_INIT(mvt->poolAvailable, "pool available", (void *)mvt);
  METER_INIT(mvt->poolUnavailable, "pool unavailable", (void *)mvt);
  METER_INIT(mvt->poolUtilization, "pool utilization", (void *)mvt);
  METER_INIT(mvt->finds, "ABQ finds", (void *)mvt);
  METER_INIT(mvt->overflows, "ABQ overflows", (void *)mvt);
  METER_INIT(mvt->underflows, "ABQ underflows", (void *)mvt);
  METER_INIT(mvt->refills, "ABQ refills", (void *)mvt);
  METER_INIT(mvt->refillPushes, "ABQ refill pushes", (void *)mvt);
  METER_INIT(mvt->refillOverflows, "ABQ refill overflows", (void *)mvt);
  METER_INIT(mvt->refillReturns, "ABQ refill returns", (void *)mvt);
  METER_INIT(mvt->perfectFits, "perfect fits", (void *)mvt);
  METER_INIT(mvt->firstFits, "first fits", (void *)mvt);
  METER_INIT(mvt->secondFits, "second fits", (void *)mvt);
  METER_INIT(mvt->failures, "failures", (void *)mvt);
  METER_INIT(mvt->emergencyContingencies, "emergency contingencies",
             (void *)mvt);
  METER_INIT(mvt->fragLimitContingencies,
             "fragmentation limit contingencies", (void *)mvt);
  METER_INIT(mvt->contingencySearches, "contingency searches", (void *)mvt);
  METER_INIT(mvt->contingencyHardSearches,
             "contingency hard searches", (void *)mvt);
  METER_INIT(mvt->splinters, "splinters", (void *)mvt);
  METER_INIT(mvt->splintersUsed, "splinters used", (void *)mvt);
  METER_INIT(mvt->splintersDropped, "splinters dropped", (void *)mvt);
  METER_INIT(mvt->sawdust, "sawdust", (void *)mvt);
  METER_INIT(mvt->exceptions, "exceptions", (void *)mvt);
  METER_INIT(mvt->exceptionSplinters, "exception splinters", (void *)mvt);
  METER_INIT(mvt->exceptionReturns, "exception returns", (void *)mvt);

  mvt->sig = MVTSig;

  AVERT(MVT, mvt);
  EVENT_PWWWWW(PoolInitMVT, pool, minSize, meanSize, maxSize,
               reserveDepth, fragLimit);
  return ResOK;

failABQ:
  CBSFinish(MVTCBS(mvt));
failCBS:
  AVER(res != ResOK);
  return res;
}


/* MVTCheck -- validate an MVT Pool */

static Bool MVTCheck(MVT mvt)
{
  CHECKS(MVT, mvt);
  CHECKD(Pool, &mvt->poolStruct);
  CHECKL(mvt->poolStruct.class == MVTPoolClassGet());
  CHECKD(CBS, &mvt->cbsStruct);
  /* CHECKL(CBSCheck(MVTCBS(mvt))); */
  CHECKD(ABQ, &mvt->abqStruct);
  /* CHECKL(ABQCheck(MVTABQ(mvt))); */
  CHECKD(SegPref, &mvt->segPrefStruct);
  CHECKL(mvt->reuseSize >= 2 * mvt->fillSize);
  CHECKL(mvt->fillSize >= mvt->maxSize);
  CHECKL(mvt->maxSize >= mvt->meanSize);
  CHECKL(mvt->meanSize >= mvt->minSize);
  CHECKL(mvt->minSize > 0);
  CHECKL(mvt->fragLimit <= 100);
  CHECKL(mvt->availLimit == mvt->size * mvt->fragLimit / 100);
  CHECKL(BoolCheck(mvt->abqOverflow));
  CHECKL(BoolCheck(mvt->splinter));
  if (mvt->splinter) {
    CHECKL(AddrOffset(mvt->splinterBase, mvt->splinterLimit) >=
           mvt->minSize);
    /* CHECKD(Seg, mvt->splinterSeg); */
    CHECKL(SegCheck(mvt->splinterSeg));
    CHECKL(mvt->splinterBase >= SegBase(mvt->splinterSeg));
    CHECKL(mvt->splinterLimit <= SegLimit(mvt->splinterSeg));
  }
  CHECKL(mvt->size == mvt->allocated + mvt->available +
         mvt->unavailable);
  /* --- could check that sum of segment sizes == mvt->size */
  /* --- check meters? */

  return TRUE;
}


/* MVTFinish -- finish an MVT pool
 */
static void MVTFinish(Pool pool)
{
  MVT mvt;
  Arena arena;
  Ring ring;
  Ring node, nextNode;
 
  AVERT(Pool, pool);
  mvt = Pool2MVT(pool);
  AVERT(MVT, mvt);
  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* Free the segments in the pool */
  ring = PoolSegRing(pool);
  RING_FOR(node, ring, nextNode) {
    MVTSegFree(mvt, SegOfPoolRing(node));
  }

  /* Finish the ABQ and CBS structures */
  ABQFinish(arena, MVTABQ(mvt));
  CBSFinish(MVTCBS(mvt));

  mvt->sig = SigInvalid;
}


/* MVTBufferFill -- refill an allocation buffer from an MVT pool
 *
 * See design.mps.poolmvt:impl.c.ap.fill
 */
static Res MVTBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size minSize,
                         Bool withReservoirPermit)
{
  Seg seg;
  MVT mvt;
  Res res;
  Addr base, limit;
  Arena arena;
  Size alignedSize, fillSize;
  CBSBlock block;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  mvt = Pool2MVT(pool);
  AVERT(MVT, mvt);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(minSize > 0);
  AVER(SizeIsAligned(minSize, pool->alignment));
  AVER(BoolCheck(withReservoirPermit));

  arena = PoolArena(pool);
  fillSize = mvt->fillSize;
  alignedSize = SizeAlignUp(minSize, ArenaAlign(arena));

  /* design.mps.poolmvt:arch.ap.no-fit.oversize */
  /* Allocate oversize blocks exactly, directly from the arena */
  if (minSize > fillSize) {
    res = MVTSegAlloc(&seg, mvt, alignedSize, pool, withReservoirPermit);
    if (res == ResOK) {
      base = SegBase(seg);
      /* only allocate this block in the segment */
      limit = AddrAdd(base, minSize);
      mvt->available -= alignedSize - minSize;
      mvt->unavailable += alignedSize - minSize;
      AVER(mvt->size == mvt->allocated + mvt->available +
           mvt->unavailable);
      METER_ACC(mvt->exceptions, minSize);
      METER_ACC(mvt->exceptionSplinters, alignedSize - minSize);
      goto done;
    }
    /* --- There cannot be a segment big enough to hold this object in
       the free list, although there may be segments that could be
       coalesced to do so. */
    AVER(res != ResOK);
    return res;
  }

  /* design.mps.poolmvt:arch.ap.no-fit.return */
  /* Use any splinter, if available */
  if (mvt->splinter) {
    base = mvt->splinterBase;
    limit = mvt->splinterLimit;
    if(AddrOffset(base, limit) >= minSize) {
      seg = mvt->splinterSeg;
      mvt->splinter = FALSE;
      METER_ACC(mvt->splintersUsed, AddrOffset(base, limit));
      goto done;
    }
  }
 
  /* Attempt to retrieve a free block from the ABQ */
  ABQRefillIfNecessary(mvt, minSize);
  res = ABQPeek(MVTABQ(mvt), &block);
  if (res != ResOK) {
    METER_ACC(mvt->underflows, minSize);
    /* design.mps.poolmvt:arch.contingency.fragmentation-limit */
    if (mvt->available >=  mvt->availLimit) {
      METER_ACC(mvt->fragLimitContingencies, minSize);
      res = MVTContingencySearch(&block, MVTCBS(mvt), minSize);
    }
  } else {
    METER_ACC(mvt->finds, minSize);
  }
found:
  if (res == ResOK) {
    base = CBSBlockBase(block);
    limit = CBSBlockLimit(block);
    {
      Bool b = SegOfAddr(&seg, arena, base);
      AVER(b);
      UNUSED(b); /* impl.c.mpm.check.unused */
    }
    /* Only pass out segments - may not be the best long-term policy. */
    {
      Addr segLimit = SegLimit(seg);

      if (limit <= segLimit) {
        /* perfect fit */
        METER_ACC(mvt->perfectFits, AddrOffset(base, limit));
      } else if (AddrOffset(base, segLimit) >= minSize) {
        /* fit in 1st segment */
        limit = segLimit;
        METER_ACC(mvt->firstFits, AddrOffset(base, limit));
      } else {
        /* fit in 2nd second segment */
        base = segLimit;
        {
          Bool b = SegOfAddr(&seg, arena, base);
          AVER(b);
          UNUSED(b); /* impl.c.mpm.check.unused */
        }
        segLimit = SegLimit(seg);
        if (limit > segLimit)
          limit = segLimit;
        METER_ACC(mvt->secondFits, AddrOffset(base, limit));
      }
    }
    {
      Res r = CBSDelete(MVTCBS(mvt), base, limit);
      AVER(r == ResOK);
      UNUSED(r); /* impl.c.mpm.check.unused */
    }
    goto done;
  }
 
  /* Attempt to request a block from the arena */
  /* see design.mps.poolmvt:impl.c.free.merge.segment */
  res = MVTSegAlloc(&seg, mvt, fillSize, pool, withReservoirPermit);
  if (res == ResOK) {
    base = SegBase(seg);
    limit = SegLimit(seg);
    goto done;
  }
 
  /* Try contingency */
  METER_ACC(mvt->emergencyContingencies, minSize);
  res = MVTContingencySearch(&block, MVTCBS(mvt), minSize);
  if (res == ResOK)
    goto found;

  METER_ACC(mvt->failures, minSize);
  AVER(res != ResOK);
  return res;
 
done:
  *baseReturn = base;
  *limitReturn = limit;
  mvt->available -= AddrOffset(base, limit);
  mvt->allocated += AddrOffset(base, limit);
  AVER(mvt->size == mvt->allocated + mvt->available +
       mvt->unavailable);
  METER_ACC(mvt->poolUtilization, mvt->allocated * 100 / mvt->size);
  METER_ACC(mvt->poolUnavailable, mvt->unavailable);
  METER_ACC(mvt->poolAvailable, mvt->available);
  METER_ACC(mvt->poolAllocated, mvt->allocated);
  METER_ACC(mvt->poolSize, mvt->size);
  METER_ACC(mvt->bufferFills, AddrOffset(base, limit));
  AVER(AddrOffset(base, limit) >= minSize);
  return ResOK;
}


/* MVTBufferEmpty -- return an unusable portion of a buffer to the MVT
 * pool
 *
 * See design.mps.poolmvt:impl.c.ap.empty
 */
static void MVTBufferEmpty(Pool pool, Buffer buffer,
                           Addr base, Addr limit)
{
  MVT mvt;
  Size size;
  Res res;

  AVERT(Pool, pool);
  mvt = Pool2MVT(pool);
  AVERT(MVT, mvt);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  AVER(base <= limit);

  size = AddrOffset(base, limit);
  if (size == 0)
    return;

  mvt->available += size;
  mvt->allocated -= size;
  AVER(mvt->size == mvt->allocated + mvt->available +
       mvt->unavailable);
  METER_ACC(mvt->poolUtilization, mvt->allocated * 100 / mvt->size);
  METER_ACC(mvt->poolUnavailable, mvt->unavailable);
  METER_ACC(mvt->poolAvailable, mvt->available);
  METER_ACC(mvt->poolAllocated, mvt->allocated);
  METER_ACC(mvt->poolSize, mvt->size);
  METER_ACC(mvt->bufferEmpties, size);

  /* design.mps.poolmvt:arch.ap.no-fit.splinter */
  if (size < mvt->minSize) {
    res = CBSInsert(MVTCBS(mvt), base, limit);
    AVER(res == ResOK);
    METER_ACC(mvt->sawdust, size);
    return;
  }

  METER_ACC(mvt->splinters, size);
  /* design.mps.poolmvt:arch.ap.no-fit.return */
  if (mvt->splinter) {
    Size oldSize = AddrOffset(mvt->splinterBase, mvt->splinterLimit);

    /* Old better, drop new */
    if (size < oldSize) {
      res = CBSInsert(MVTCBS(mvt), base, limit);
      AVER(res == ResOK);
      METER_ACC(mvt->splintersDropped, size);
      return;
    } else {
      /* New better, drop old */
      res = CBSInsert(MVTCBS(mvt), mvt->splinterBase, mvt->splinterLimit);
      AVER(res == ResOK);
      METER_ACC(mvt->splintersDropped, oldSize);
    }
  }

  mvt->splinter = TRUE;
  mvt->splinterSeg = BufferSeg(buffer);
  mvt->splinterBase = base;
  mvt->splinterLimit = limit;
}


/* MVTFree -- free a block (previously allocated from a buffer) that
 * is no longer in use
 *
 * see design.poolmvt.impl.c.free
 */
static void MVTFree(Pool pool, Addr base, Size size)
{
  MVT mvt;
  Addr limit;

  AVERT(Pool, pool);
  mvt = Pool2MVT(pool);
  AVERT(MVT, mvt);
  AVER(base != (Addr)0);
  AVER(size > 0);


  /* We know the buffer observes pool->alignment  */
  size = SizeAlignUp(size, pool->alignment);
  limit = AddrAdd(base, size);
  METER_ACC(mvt->poolFrees, size);
  mvt->available += size;
  mvt->allocated -= size;
  AVER(mvt->size == mvt->allocated + mvt->available + mvt->unavailable);
  METER_ACC(mvt->poolUtilization, mvt->allocated * 100 / mvt->size);
  METER_ACC(mvt->poolUnavailable, mvt->unavailable);
  METER_ACC(mvt->poolAvailable, mvt->available);
  METER_ACC(mvt->poolAllocated, mvt->allocated);
  METER_ACC(mvt->poolSize, mvt->size);
 
  /* design.mps.poolmvt:arch.ap.no-fit.oversize.policy */
  /* Return exceptional blocks directly to arena */
  if (size > mvt->fillSize) {
    Seg seg;
    {
      Bool b = SegOfAddr(&seg, PoolArena(pool), base);
      AVER(b);
      UNUSED(b); /* impl.c.mpm.check.unused */
    }
    AVER(base == SegBase(seg));
    AVER(limit <= SegLimit(seg));
    mvt->available += SegSize(seg) - size;
    mvt->unavailable -= SegSize(seg) - size;
    AVER(mvt->size == mvt->allocated + mvt->available +
         mvt->unavailable);
    METER_ACC(mvt->exceptionReturns, SegSize(seg));
    if (SegBuffer(seg) != NULL)
      BufferDetach(SegBuffer(seg), MVT2Pool(mvt));
    MVTSegFree(mvt, seg);
    return;
  }
 
  {
    Res res = CBSInsert(MVTCBS(mvt), base, limit);
    AVER(res == ResOK);
    UNUSED(res); /* impl.c.mpm.check.unused */
  }
}


/* MVTDescribe -- describe an MVT pool */

static Res MVTDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  MVT mvt;

  if (!CHECKT(Pool, pool)) return ResFAIL;
  mvt = Pool2MVT(pool);
  if (!CHECKT(MVT, mvt)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
	       "MVT $P\n{\n", (WriteFP)mvt,
	       "  minSize: $U \n", (WriteFU)mvt->minSize,
	       "  meanSize: $U \n", (WriteFU)mvt->meanSize,
	       "  maxSize: $U \n", (WriteFU)mvt->maxSize,
	       "  fragLimit: $U \n", (WriteFU)mvt->fragLimit,
	       "  reuseSize: $U \n", (WriteFU)mvt->reuseSize,
	       "  fillSize: $U \n", (WriteFU)mvt->fillSize,
	       "  availLimit: $U \n", (WriteFU)mvt->availLimit,
	       "  abqOverflow: $S \n", mvt->abqOverflow?"TRUE":"FALSE",
	       "  splinter: $S \n", mvt->splinter?"TRUE":"FALSE",
	       "  splinterSeg: $P \n", (WriteFP)mvt->splinterSeg,
	       "  splinterBase: $A \n", (WriteFA)mvt->splinterBase,
	       "  splinterLimit: $A \n", (WriteFU)mvt->splinterLimit,
	       "  size: $U \n", (WriteFU)mvt->size,
	       "  allocated: $U \n", (WriteFU)mvt->allocated,
	       "  available: $U \n", (WriteFU)mvt->available,
	       "  unavailable: $U \n", (WriteFU)mvt->unavailable,
	       NULL);
  if(res != ResOK) return res;

  res = CBSDescribe(MVTCBS(mvt), stream);
  if(res != ResOK) return res;

  res = ABQDescribe(MVTABQ(mvt), stream);
  if(res != ResOK) return res;

  res = METER_WRITE(mvt->segAllocs, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->segFrees, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->bufferFills, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->bufferEmpties, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->poolFrees, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->poolSize, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->poolAllocated, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->poolAvailable, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->poolUnavailable, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->poolUtilization, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->finds, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->overflows, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->underflows, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->refills, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->refillPushes, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->refillOverflows, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->refillReturns, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->perfectFits, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->firstFits, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->secondFits, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->failures, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->emergencyContingencies, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->fragLimitContingencies, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->contingencySearches, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->contingencyHardSearches, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->splinters, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->splintersUsed, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->splintersDropped, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->sawdust, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->exceptions, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->exceptionSplinters, stream);
  if (res != ResOK) return res;
  res = METER_WRITE(mvt->exceptionReturns, stream);
  if (res != ResOK) return res;
 
  res = WriteF(stream, "}\n", NULL);
  return res;
}


/* Pool Interface */


/* PoolClassMVT -- the Pool (sub-)Class for an MVT pool */

PoolClass PoolClassMVT(void)
{
  return MVTPoolClassGet();
}


/* MPS Interface */


/* mps_class_mvt -- the class of an mvt pool */

mps_class_t mps_class_mvt(void)
{
  return (mps_class_t)(PoolClassMVT());
}


/* MPS Interface extensions --- should these be pool generics? */


/* mps_mvt_size -- number of bytes committed to the pool */

size_t mps_mvt_size(mps_pool_t mps_pool)
{
  Pool pool;
  MVT mvt;

  pool = (Pool)mps_pool;

  AVERT(Pool, pool);
  mvt = Pool2MVT(pool);
  AVERT(MVT, mvt);

  return (size_t)mvt->size;
}


/* mps_mvt_free_size -- number of bytes comitted to the pool that are
 * available for allocation
 */
size_t mps_mvt_free_size(mps_pool_t mps_pool)
{
  Pool pool;
  MVT mvt;

  pool = (Pool)mps_pool;

  AVERT(Pool, pool);
  mvt = Pool2MVT(pool);
  AVERT(MVT, mvt);

  return (size_t)mvt->available;
}


/* Internal methods */


/* MVTSegAlloc -- encapsulates SegAlloc with associated accounting and
 * metering
 */
static Res MVTSegAlloc(Seg *segReturn, MVT mvt, Size size,
                       Pool pool, Bool withReservoirPermit)
{
  Res res = SegAlloc(segReturn, GCSegClassGet(),
                     MVTSegPref(mvt), size, pool, withReservoirPermit);

  if (res == ResOK) {
    Size segSize = SegSize(*segReturn);
   
    /* see design.mps.poolmvt:arch.fragmentation.internal */
    AVER(segSize >= mvt->fillSize);
    mvt->size += segSize;
    mvt->available += segSize;
    mvt->availLimit = mvt->size * mvt->fragLimit / 100;
    AVER(mvt->size == mvt->allocated + mvt->available + mvt->unavailable);
    METER_ACC(mvt->segAllocs, segSize);
  }
  return res;
}
 

/* MVTSegFree -- encapsulates SegFree with associated accounting and
 * metering
 */
static void MVTSegFree(MVT mvt, Seg seg)
{
  Size size = SegSize(seg);

  mvt->available -= size;
  mvt->size -= size;
  mvt->availLimit = mvt->size * mvt->fragLimit / 100;
  AVER(mvt->size == mvt->allocated + mvt->available + mvt->unavailable);
  SegFree(seg);
  METER_ACC(mvt->segFrees, size);
}


/* MVTReturnBlockSegs -- return (interior) segments of a block to the
 * arena
 */
static Bool MVTReturnBlockSegs(MVT mvt, CBSBlock block, Arena arena)
{
  Addr base, limit;
  Bool success = FALSE;
   
  base = CBSBlockBase(block);
  limit = CBSBlockLimit(block);

  while (base < limit) {
    Seg seg;
    Addr segBase, segLimit;
     
    {
      Bool b = SegOfAddr(&seg, arena, base);
      AVER(b);
      UNUSED(b); /* impl.c.mpm.check.unused */
    }
    segBase = SegBase(seg);
    segLimit = SegLimit(seg);
    if (base <= segBase && limit >= segLimit) {
      Res r = CBSDelete(MVTCBS(mvt), segBase, segLimit);

      AVER(r == ResOK);
      UNUSED(r); /* impl.c.mpm.check.unused */
      MVTSegFree(mvt, seg);
      success = TRUE;
    }
    base = segLimit;
  }

  return success;
}


/* MVTNoteNew -- callback invoked when a block on the CBS >= reuseSize
 */
static void MVTNoteNew(CBS cbs, CBSBlock block, Size oldSize, Size newSize)
{
  Res res;
  MVT mvt;
 
  AVERT(CBS, cbs);
  mvt = CBSMVT(cbs);
  AVERT(MVT, mvt);
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) >= mvt->reuseSize);
  UNUSED(oldSize);
  UNUSED(newSize);

  res = ABQPush(MVTABQ(mvt), block);
  /* See design.mps.poolmvt:impl.c.free.merge */
  if (res != ResOK) {
    Arena arena = PoolArena(MVT2Pool(mvt));
    CBSBlock oldBlock;
    res = ABQPeek(MVTABQ(mvt), &oldBlock);
    AVER(res == ResOK);
    /* --- This should always succeed */
    (void)MVTReturnBlockSegs(mvt, oldBlock, arena);
    res = ABQPush(MVTABQ(CBSMVT(cbs)), block);
    if (res != ResOK) {
      unless(MVTReturnBlockSegs(mvt, block, arena)) {
        mvt->abqOverflow = TRUE;
        METER_ACC(mvt->overflows, CBSBlockSize(block));
      }
    }
  }
}


/* MVTNoteDelete -- callback invoked when a block on the CBS <= reuseSize */

static void MVTNoteDelete(CBS cbs, CBSBlock block, Size oldSize, Size newSize)
{
  Res res;

  AVERT(CBS, cbs);
  AVERT(MVT, CBSMVT(cbs));
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) < CBSMVT(cbs)->reuseSize);
  UNUSED(oldSize);
  UNUSED(newSize);
 
  res = ABQDelete(MVTABQ(CBSMVT(cbs)), block);
  AVER(res == ResOK || CBSMVT(cbs)->abqOverflow);
  UNUSED(res); /* impl.c.mpm.check.unused */
}


/* ABQRefillIfNecessary -- refill the ABQ from the CBS if it had
 * overflown and is now empty
 */
static void ABQRefillIfNecessary(MVT mvt, Size size)
{
  AVERT(MVT, mvt);
  AVER(size > 0);

  if (mvt->abqOverflow && ABQIsEmpty(MVTABQ(mvt))) {
    mvt->abqOverflow = FALSE;
    METER_ACC(mvt->refills, size);
    CBSIterateLarge(MVTCBS(mvt), &ABQRefillCallback, NULL);
  }
}


/* ABQRefillCallback -- called from CBSIterate at the behest of
 * ABQRefillIfNecessary
 */
static Bool ABQRefillCallback(CBS cbs, CBSBlock block, void *closureP)
{
  Res res;
  MVT mvt;
 
  AVERT(CBS, cbs);
  mvt = CBSMVT(cbs);
  AVERT(MVT, mvt);
  AVERT(ABQ, MVTABQ(mvt));
  AVERT(CBSBlock, block);
  AVER(CBSBlockSize(block) >= mvt->reuseSize);
  UNUSED(closureP);

  METER_ACC(mvt->refillPushes, ABQDepth(MVTABQ(mvt)));
  res = ABQPush(MVTABQ(mvt), block);
  if (res != ResOK) {
    if (MVTReturnBlockSegs(mvt, block, PoolArena(MVT2Pool(mvt)))) {
      METER_ACC(mvt->refillReturns, CBSBlockSize(block));
      return TRUE;
    } else {
      mvt->abqOverflow = TRUE;
      METER_ACC(mvt->refillOverflows, CBSBlockSize(block));
      return FALSE;
    }
  }

  return TRUE;
}
 

/* Closure for MVTContingencySearch */
typedef struct MVTContigencyStruct *MVTContigency;

typedef struct MVTContigencyStruct
{
  CBSBlock blockReturn;
  Arena arena;
  Size min;
  /* meters */
  Count steps;
  Count hardSteps;
} MVTContigencyStruct;


/* MVTContingencySearch -- search the CBS for a block of size min */

static Res MVTContingencySearch(CBSBlock *blockReturn, CBS cbs, Size min)
{
  MVTContigencyStruct cls;

  cls.blockReturn = NULL;
  cls.arena = PoolArena(MVT2Pool(CBSMVT(cbs)));
  cls.min = min;
  cls.steps = 0;
  cls.hardSteps = 0;
 
  CBSIterate(cbs, &MVTContingencyCallback, (void *)&cls);
  if (cls.blockReturn != NULL) {
    AVER(CBSBlockSize(cls.blockReturn) >= min);
    METER_ACC(CBSMVT(cbs)->contingencySearches, cls.steps);
    if (cls.hardSteps) {
      METER_ACC(CBSMVT(cbs)->contingencyHardSearches, cls.hardSteps);
    }
    *blockReturn = cls.blockReturn;
    return ResOK;
  }
   
  return ResFAIL;
}


/* MVTContingencyCallback -- called from CBSIterate at the behest of
 * MVTContingencySearch
 */
static Bool MVTContingencyCallback(CBS cbs, CBSBlock block, void *closureP)
{
  MVTContigency cl;
  Size size;
 
  AVERT(CBS, cbs);
  AVERT(CBSBlock, block);
  AVER(closureP != NULL);

  cl = (MVTContigency)closureP;
  size = CBSBlockSize(block);
 
  cl->steps++;
  if (size < cl->min)
    return TRUE;

  /* verify that min will fit when seg-aligned */
  if (size >= 2 * cl->min) {
    cl->blockReturn = block;
    return FALSE;
  }
 
  /* do it the hard way */
  cl->hardSteps++;
  if (MVTCheckFit(block, cl->min, cl->arena)) {
    cl->blockReturn = block;
    return FALSE;
  }
 
  /* keep looking */
  return TRUE;
}


/* MVTCheckFit -- verify that segment-aligned block of size min can
 * fit in a candidate CBSblock
 */
static Bool MVTCheckFit(CBSBlock block, Size min, Arena arena)
{
  Addr base = CBSBlockBase(block);
  Addr limit = CBSBlockLimit(block);
  Seg seg;
  Addr segLimit;

  {
    Bool b = SegOfAddr(&seg, arena, base);
    AVER(b);
    UNUSED(b); /* impl.c.mpm.check.unused */
  }
  segLimit = SegLimit(seg);

  if (limit <= segLimit) {
    if (AddrOffset(base, limit) >= min)
      return TRUE;
  }

  if (AddrOffset(base, segLimit) >= min)
    return TRUE;

  base = segLimit;
  {
    Bool b = SegOfAddr(&seg, arena, base);
    AVER(b);
    UNUSED(b); /* impl.c.mpm.check.unused */
  }
  segLimit = SegLimit(seg);

  if (AddrOffset(base, limit < segLimit ? limit : segLimit) >= min)
    return TRUE;

  return FALSE;
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

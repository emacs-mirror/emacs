/* impl.c.poolawl: AUTOMATIC WEAK LINKED POOL CLASS
 *
 * $HopeName: MMsrc!poolawl.c(trunk.73) $
 * Copyright (C) 2001 Harlequin Limited.  All rights reserved.
 *
 *
 * DESIGN
 *
 * .design: See design.mps.poolawl.  This is Dylan-specific pool.
 *
 *
 * ASSUMPTIONS (about when to scan single references on accesses)
 *
 * .assume.purpose: The purpose of scanning refs singly is to limit the 
 * amount of scanning of weak references which must be performed when
 * the mutator hits a barrier. Weak references which are scanned at this
 * time are not "weak splatted". Minimizing any loss of weak splats
 * potentially reduces conservatism in the collector.
 *
 * .assume.noweak: It follows (from .assume.purpose) that there is no
 * benefit from scanning single refs on barrier accesses for segments
 * which don't contain any weak references. However, if a segment 
 * contains either all weak refs or a mixture of weak and non-weak 
 * references then there is a potential benefit.
 *
 * .assume.mixedrank: If a segment contains a mixture of references 
 * at different ranks (e.g. weak and strong references), there is 
 * no way to determine whether or not references at a rank other than
 * the scan state rank will be  scanned as a result of normal 
 * (non-barrier) scanning  activity. (@@@@ This is a deficiency in MPS).
 * Assume that such references will, in fact, be scanned at the 
 * incorrect rank.
 *
 * .assume.samerank: The pool doesn't support segments with mixed 
 * rank segments in any case (despite .assume.mixedrank).
 *
 * .assume.alltraceable: The pool assumes that all objects are entirely
 * traceable. This must be documented elsewhere for the benefit of the 
 * client.
 */

#include "mpscawl.h"
#include "mpm.h"
#include "chain.h"


SRCID(poolawl, "$HopeName: MMsrc!poolawl.c(trunk.73) $");


#define AWLSig ((Sig)0x519B7A37) /* SIGnature PooL AWL */

#define AWLGen ((Serial)1) /* "generation" for AWL pools */
/* This and the dynamic criterion are the only ways AWL will get collected. */


/* awlStat* -- Statistics gathering about instruction emulation
 *
 * To support change.dylan.2.0.160044.
 */


/* Per-segment statistics maintained between segment scans */

typedef struct awlStatSegStruct {
  Count sameAccesses;  /* accesses involving same address as last access */
  Addr lastAccess;     /* the address of last access */
} awlStatSegStruct, *awlStatSeg;

/* Per-pool statistics updated at segment scans */

typedef struct awlStatTotalStruct {
  Count goodScans;     /* total times a segment scanned at proper rank */
  Count badScans;      /* total times a segment scanned at improper rank */
  Count savedScans;    /* total times an entire segment scan was avoided */
  Count savedAccesses; /* total single references leading to a saved scan */
  Count declined;      /* number of declined single accesses */
} awlStatTotalStruct, *awlStatTotal;


/* AWLStruct -- AWL pool structure
 *
 * See design.mps.poolawl.poolstruct
 */

typedef struct AWLStruct {
  PoolStruct poolStruct;
  Shift alignShift;
  Chain chain;              /* dummy chain */
  PoolGenStruct pgen;       /* generation representing the pool */
  Size size;                /* allocated size in bytes */
  Serial gen;               /* associated generation (for SegAlloc) */
  Count succAccesses;       /* number of successive single accesses */
  awlStatTotalStruct stats;
  Sig sig;
} AWLStruct, *AWL;

#define Pool2AWL(pool) PARENT(AWLStruct, poolStruct, pool)


static Bool AWLCheck(AWL awl);


/* Conversion between indexes and Addrs */
#define awlIndexOfAddr(base, awl, p) \
  (AddrOffset((base), (p)) >> (awl)->alignShift)


/* AWLSegStruct -- AWL segment subclass
 *
 * Subclass of GCSeg
 */

#define AWLSegSig ((Sig)0x519A3759) /* SIGnature AWL SeG */

/* design.mps.poolawl.seg */
typedef struct AWLSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  BT mark;
  BT scanned;
  BT alloc;
  Count grains;
  Count free; /* number of free grains */
  Count singleAccesses;   /* number of accesses processed singly */
  awlStatSegStruct stats;
  Sig sig;
} AWLSegStruct, *AWLSeg;

#define Seg2AWLSeg(seg)             ((AWLSeg)(seg))
#define AWLSeg2Seg(awlseg)          ((Seg)(awlseg))


static SegClass AWLSegClassGet(void);


static Bool AWLSegCheck(AWLSeg awlseg)
{
  CHECKS(AWLSeg, awlseg);
  CHECKD(GCSeg, &awlseg->gcSegStruct);
  CHECKL(awlseg->mark != NULL);
  CHECKL(awlseg->scanned != NULL);
  CHECKL(awlseg->alloc != NULL);
  /* Can't do any real check on ->grains */
  CHECKL(awlseg->grains > 0);
  CHECKL(awlseg->free <= awlseg->grains);
  return TRUE;
}


/* Management of statistics for monitoring protection-driven accesses */

static void awlStatSegInit(AWLSeg awlseg)
{
  awlseg->stats.sameAccesses = 0;
  awlseg->stats.lastAccess = NULL;
}

static void awlStatTotalInit(AWL awl)
{
  awl->stats.goodScans = 0;
  awl->stats.badScans = 0;
  awl->stats.savedAccesses = 0;
  awl->stats.savedScans = 0;
  awl->stats.declined = 0;
}


/* AWLSegInit -- Init method for AWL segments */

static Res AWLSegInit(Seg seg, Pool pool, Addr base, Size size, 
                      Bool reservoirPermit, va_list args)
{
  SegClass super;
  AWLSeg awlseg;
  AWL awl;
  Arena arena;
  RankSet rankSet;
  Count bits;        /* number of grains */
  Res res;
  Size tableSize;
  void *v;

  AVERT(Seg, seg);
  awlseg = Seg2AWLSeg(seg);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  /* no useful checks for base and size */
  AVER(BoolCheck(reservoirPermit));
  rankSet = va_arg(args, RankSet);
  /* .assume.samerank */
  /* AWL only accepts two ranks */
  AVER(RankSetSingle(RankEXACT) == rankSet
       || RankSetSingle(RankWEAK) == rankSet);
  awl = Pool2AWL(pool);
  AVERT(AWL, awl);

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(AWLSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if (res != ResOK)
    return res;

  bits = size >> awl->alignShift;
  tableSize = BTSize(bits);
  res = ControlAlloc(&v, arena, tableSize, reservoirPermit);
  if (res != ResOK)
    goto failControlAllocMark;
  awlseg->mark = v;
  res = ControlAlloc(&v, arena, tableSize, reservoirPermit);
  if (res != ResOK)
    goto failControlAllocScanned;
  awlseg->scanned = v;
  res = ControlAlloc(&v, arena, tableSize, reservoirPermit);
  if (res != ResOK)
    goto failControlAllocAlloc;
  awlseg->alloc = v;
  awlseg->grains = bits;
  BTResRange(awlseg->mark, 0, bits);
  BTResRange(awlseg->scanned, 0, bits);
  BTResRange(awlseg->alloc, 0, bits);
  SegSetRankAndSummary(seg, rankSet, RefSetUNIV);
  awlseg->free = bits;
  awlseg->sig = AWLSegSig;
  awlseg->singleAccesses = 0;
  awlStatSegInit(awlseg);
  AVERT(AWLSeg, awlseg);
  return ResOK;

failControlAllocAlloc:
  ControlFree(arena, awlseg->scanned, tableSize);
failControlAllocScanned:
  ControlFree(arena, awlseg->mark, tableSize);
failControlAllocMark:
  super->finish(seg);
  return res;
}


/* AWLSegFinish -- Finish method for AWL segments */

static void AWLSegFinish(Seg seg)
{
  AWL awl;
  AWLSeg awlseg;
  SegClass super;
  Pool pool;
  Size tableSize;
  Arena arena;
  Count segGrains;

  AVERT(Seg, seg);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);
  pool = SegPool(seg);
  AVERT(Pool, pool);
  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* This is one of the few places where it is easy to check */
  /* awlseg->grains, so we do */
  segGrains = SegSize(seg) >> awl->alignShift;
  AVER(segGrains == awlseg->grains);
  tableSize = BTSize(segGrains);
  ControlFree(arena, awlseg->alloc, tableSize);
  ControlFree(arena, awlseg->scanned, tableSize);
  ControlFree(arena, awlseg->mark, tableSize);
  awlseg->sig = SigInvalid;

  /* finish the superclass fields last */
  super = SEG_SUPERCLASS(AWLSegClass);
  super->finish(seg);
}
  

/* AWLSegClass -- Class definition for AWL segments */

DEFINE_SEG_CLASS(AWLSegClass, class)
{
  INHERIT_CLASS(class, GCSegClass);
  SegClassMixInNoSplitMerge(class);  /* no support for this (yet) */
  class->name = "AWLSEG";
  class->size = sizeof(AWLSegStruct);
  class->init = AWLSegInit;
  class->finish = AWLSegFinish;
}


/* Single access permission control parameters */

Count AWLSegSALimit = 0; /* Number of single accesses permitted per segment */
Bool AWLHaveSegSALimit = FALSE;  /* When TRUE, AWLSegSALimit applies */

Count AWLTotalSALimit = 0; /* Number of single accesses permitted in a row */
Bool AWLHaveTotalSALimit = FALSE;  /* When TRUE, AWLTotalSALimit applies */


/* Determine whether to permit scanning a single ref. */

static Bool AWLCanTrySingleAccess(AWL awl, Seg seg, Addr addr)
{
  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(addr != NULL);

  /* .assume.noweak */
  /* .assume.alltraceable */
  if (RankSetIsMember(SegRankSet(seg), RankWEAK)) {
    AWLSeg awlseg;

    awlseg = Seg2AWLSeg(seg);
    AVERT(AWLSeg, awlseg);
    
    if (AWLHaveTotalSALimit) {
      if (AWLTotalSALimit < awl->succAccesses) {
        STATISTIC(awl->stats.declined++);
        return FALSE; /* decline single access because of total limit */
      }
    }

    if (AWLHaveSegSALimit) {
      if (AWLSegSALimit < awlseg->singleAccesses) {
        STATISTIC(awl->stats.declined++);
        return FALSE; /* decline single access because of segment limit */
      }
    }

    return TRUE;

  } else {
    return FALSE; /* Single access only for weak segs (.assume.noweak) */
  }
}


/* Record an access to a segment which required scanning a single ref */

static void AWLNoteRefAccess(AWL awl, Seg seg, Addr addr)
{
  AWLSeg awlseg;

  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(addr != NULL);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  awlseg->singleAccesses++; /* increment seg count of ref accesses */
  if (addr == awlseg->stats.lastAccess) {
    /* If this is a repeated access, increment count  */
    STATISTIC(awlseg->stats.sameAccesses++);
  }
  STATISTIC(awlseg->stats.lastAccess = addr);
  awl->succAccesses++;  /* Note a new successive access */
}


/* Record an access to a segment which required scanning the entire seg */

static void AWLNoteSegAccess(AWL awl, Seg seg, Addr addr)
{
  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(addr != NULL);

  awl->succAccesses = 0; /* reset count of successive accesses */
}


/* Record a scan of a segment which wasn't provoked by an access */

static void AWLNoteScan(AWL awl, Seg seg, ScanState ss)
{
  AWLSeg awlseg;

  AVERT(AWL, awl);
  AVERT(Seg, seg);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  /* .assume.mixedrank */
  /* .assume.samerank */
  /* If this segment has any RankWEAK references, then  */
  /* record statistics about whether weak splatting is being lost. */
  if (RankSetIsMember(SegRankSet(seg), RankWEAK)) {
    if (RankWEAK == ss->rank) {
      /* This is "successful" scan at proper rank. */
      STATISTIC(awl->stats.goodScans++);
      if (0 < awlseg->singleAccesses) {
        /* Accesses have been proceesed singly */
        /* Record that we genuinely did save a protection-provoked scan */
        STATISTIC(awl->stats.savedScans++);
        STATISTIC(awl->stats.savedAccesses += awlseg->singleAccesses);
      }
    } else {
      /* This is "failed" scan at improper rank. */
      STATISTIC(awl->stats.badScans++);
    } 
    /* Reinitialize the segment statistics */
    awlseg->singleAccesses = 0;
    STATISTIC(awlStatSegInit(awlseg));
  }
}


/* AWLSegCreate -- Create a new segment of at least given size */

static Res AWLSegCreate(AWLSeg *awlsegReturn,
                        RankSet rankSet, Pool pool, Size size,
                        Bool reservoirPermit)
{
  AWL awl;
  Seg seg;
  AWLSeg awlseg;
  Res res;
  Arena arena;
  SegPrefStruct segPrefStruct;

  AVER(awlsegReturn != NULL);
  AVER(RankSetCheck(rankSet));
  AVERT(Pool, pool);
  AVER(size > 0);
  AVER(BoolCheck(reservoirPermit));

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  size = SizeAlignUp(size, ArenaAlign(arena));
  /* beware of large sizes overflowing upon rounding */
  if (size == 0)
    return ResMEMORY;
  segPrefStruct = *SegPrefDefault();
  SegPrefExpress(&segPrefStruct, SegPrefCollected, NULL);
  SegPrefExpress(&segPrefStruct, SegPrefGen, &awl->gen);
  res = SegAlloc(&seg, AWLSegClassGet(), &segPrefStruct, size, pool, 
                 reservoirPermit, rankSet);
  if (res != ResOK)
    return res;

  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  *awlsegReturn = awlseg;
  return ResOK;
}


/* AWLSegAlloc -- allocate an object in a given segment */

static Bool AWLSegAlloc(Addr *baseReturn, Addr *limitReturn,
                        AWLSeg awlseg, AWL awl, Size size)
{
  Count n;        /* number of grains equivalent to alloc size */
  Index i, j;
  Seg seg;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(AWLSeg, awlseg);
  AVERT(AWL, awl);
  AVER(size > 0);
  AVER(size << awl->alignShift >= size);
  seg = AWLSeg2Seg(awlseg);

  if (size > SegSize(seg))
    return FALSE;
  n = size >> awl->alignShift;
  if (!BTFindLongResRange(&i, &j, awlseg->alloc, 0, awlseg->grains, n))
    return FALSE;
  awl->size += size;
  *baseReturn = AddrAdd(SegBase(seg), i << awl->alignShift);
  *limitReturn = AddrAdd(SegBase(seg), j << awl->alignShift);
  return TRUE;
}


/* AWLInit -- initialize an AWL pool */

static Res AWLInit(Pool pool, va_list arg)
{
  AWL awl;
  Format format;
  Chain chain;
  Res res;
  static GenParamStruct genParam = { SizeMAX, 0.5 /* dummy */ };

  /* Weak check, as half-way through initialization. */
  AVER(pool != NULL);

  awl = Pool2AWL(pool);

  format = va_arg(arg, Format);
  AVERT(Format, format);
  pool->format = format;

  res = ChainCreate(&chain, pool->arena, 1, &genParam);
  if (res != ResOK)
    return res;
  awl->chain = chain;
  /* .gen: This must be the nursery in the chain, because it's the only */
  /* generation.  awl->gen is just a hack for segment placement. */
  res = PoolGenInit(&awl->pgen, chain, 0 /* .gen */, pool);
  if (res != ResOK)
    goto failGenInit;

  awl->alignShift = SizeLog2(pool->alignment);
  awl->gen = AWLGen;
  awl->size = (Size)0;

  awl->succAccesses = 0;
  awlStatTotalInit(awl);
  awl->sig = AWLSig;

  AVERT(AWL, awl);
  EVENT_PP(PoolInitAWL, pool, format);
  return ResOK;

failGenInit:
  ChainDestroy(chain);
  return res;
}


/* AWLFinish -- finish an AWL pool */

static void AWLFinish(Pool pool)
{
  AWL awl;
  Ring ring, node, nextNode;

  AVERT(Pool, pool);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);

  ring = &pool->segRing;
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    AVERT(Seg, seg);
    SegFree(seg);
  }
  awl->sig = SigInvalid;
  PoolGenFinish(&awl->pgen);
  ChainDestroy(awl->chain);
}


/* AWLBufferFill -- BufferFill method for AWL */

static Res AWLBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size,
                         Bool reservoirPermit)
{
  Addr base, limit;
  AWLSeg awlseg;
  AWL awl;
  Res res;
  Ring node, nextNode;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(BoolCheck(reservoirPermit));

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);

  RING_FOR(node, &pool->segRing, nextNode) {
    Seg seg;

    seg = SegOfPoolRing(node);
    AVERT(Seg, seg);
    awlseg = Seg2AWLSeg(seg);
    AVERT(AWLSeg, awlseg);

    /* Only try to allocate in the segment if it is not already */
    /* buffered, and has the same ranks as the buffer. */
    if (SegBuffer(seg) == NULL && SegRankSet(seg) == BufferRankSet(buffer))
      if (awlseg->free << awl->alignShift >= size
          && AWLSegAlloc(&base, &limit, awlseg, awl, size))
        goto found;
  }

  /* No free space in existing awlsegs, so create new awlseg */

  res = AWLSegCreate(&awlseg, BufferRankSet(buffer), pool, size,
                     reservoirPermit);
  if (res != ResOK)
    return res;
  base = SegBase(AWLSeg2Seg(awlseg));
  limit = SegLimit(AWLSeg2Seg(awlseg));

found:
  {
    Index i, j;
    Seg seg = AWLSeg2Seg(awlseg);
    i = awlIndexOfAddr(SegBase(seg), awl, base);
    j = awlIndexOfAddr(SegBase(seg), awl, limit);
    AVER(i < j);
    BTSetRange(awlseg->alloc, i, j);
    /* Objects are allocated black. */
    /* Shouldn't this depend on trace phase?  @@@@ */
    BTSetRange(awlseg->mark, i, j);
    BTSetRange(awlseg->scanned, i, j);
    awlseg->free -= j - i;
  }
  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;
}


/* AWLBufferEmpty -- BufferEmpty method for AWL */

static void AWLBufferEmpty(Pool pool, Buffer buffer, Addr init, Addr limit)
{
  AWL awl;
  AWLSeg awlseg;
  Seg seg;
  Addr segBase;
  Index i, j;

  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  seg = BufferSeg(buffer);
  AVERT(Seg, seg);
  AVER(init <= limit);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  segBase = SegBase(seg);

  i = awlIndexOfAddr(segBase, awl, init);
  j = awlIndexOfAddr(segBase, awl, limit);
  AVER(i <= j);
  if (i < j) {
    BTResRange(awlseg->alloc, i, j);
    awlseg->free += j - i;
  }
}


/* AWLWhiten -- segment condemning method */

/* Split out of AWLWhiten because it's used in more than one place. */
static void AWLRangeWhiten(AWLSeg awlseg, Index base, Index limit)
{
  if (base != limit) {
    AVER(base < limit);
    AVER(limit <= awlseg->grains);
    BTResRange(awlseg->mark, base, limit);
    BTResRange(awlseg->scanned, base, limit);
  }
}

static Res AWLWhiten(Pool pool, Trace trace, Seg seg)
{
  AWL awl;
  AWLSeg awlseg;
  Buffer buffer;

  /* all parameters checked by generic PoolWhiten */

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);
  buffer = SegBuffer(seg);

  /* can only whiten for a single trace, */
  /* see design.mps.poolawl.fun.condemn */
  AVER(SegWhite(seg) == TraceSetEMPTY);

  if (buffer == NULL) {
    AWLRangeWhiten(awlseg, 0, awlseg->grains);
    trace->condemned += SegSize(seg);
  } else {
    /* Whiten everything except the buffer. */
    Addr base = SegBase(seg);
    Index scanLimitIndex = awlIndexOfAddr(base, awl, 
					  BufferScanLimit(buffer));
    Index limitIndex = awlIndexOfAddr(base, awl, 
                                      BufferLimit(buffer));

    AWLRangeWhiten(awlseg, 0, scanLimitIndex);
    AWLRangeWhiten(awlseg, limitIndex, awlseg->grains);

    /* Check the buffer is black. */
    /* This really ought to change when we have a non-trivial */
    /* pre-flip phase. @@@@ ('coz then we'll be allocating white) */
    if (scanLimitIndex != limitIndex) {
      AVER(BTIsSetRange(awlseg->mark, scanLimitIndex, limitIndex));
      AVER(BTIsSetRange(awlseg->scanned, scanLimitIndex, limitIndex));
    }

    /* We didn't condemn the buffer, subtract it from the count. */
    /* @@@@ We could subtract all the free grains. */
    trace->condemned += SegSize(seg)
                        - AddrOffset(BufferScanLimit(buffer),
                                     BufferLimit(buffer));
  }

  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
  return ResOK;
}


/* AWLGrey -- Grey method for AWL pools */

/* AWLRangeGrey -- subroutine for AWLGrey */
static void AWLRangeGrey(AWLSeg awlseg, Index base, Index limit)
{
  /* AWLSeg not checked as that's already been done */
  AVER(limit <= awlseg->grains);
  /* copes with degenerate case as that makes caller simpler */
  if (base < limit) {
    BTSetRange(awlseg->mark, base, limit);
    BTResRange(awlseg->scanned, base, limit);
  } else {
    AVER(base == limit);
  }
}

static void AWLGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);

  if (!TraceSetIsMember(SegWhite(seg), trace)) {
    AWL awl;
    AWLSeg awlseg;

    awl = Pool2AWL(pool);
    AVERT(AWL, awl);
    awlseg = Seg2AWLSeg(seg);
    AVERT(AWLSeg, awlseg);

    SegSetGrey(seg, TraceSetAdd(SegGrey(seg), trace));
    if (SegBuffer(seg) != NULL) {
      Addr base = SegBase(seg);
      Buffer buffer = SegBuffer(seg);

      AWLRangeGrey(awlseg,
                   0,
		   awlIndexOfAddr(base, awl, BufferScanLimit(buffer)));
      AWLRangeGrey(awlseg,
                   awlIndexOfAddr(base, awl, BufferLimit(buffer)),
		   awlseg->grains);
    } else {
      AWLRangeGrey(awlseg, 0, awlseg->grains);
    }
  }
}


/* AWLBlacken -- Blacken method for AWL pools */

static void AWLBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AWL awl;
  AWLSeg awlseg;

  AVERT(Pool, pool);
  AVER(TraceSetCheck(traceSet));
  AVERT(Seg, seg);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);
  
  BTSetRange(awlseg->scanned, 0, awlseg->grains);
}


/* AWLDependentObject -- returns the linked object, if any
 *
 * see design.mps.poolawl.fun.dependent-object, and
 * analysis.mps.poolawl.improve.dependent.abstract
 */

static Bool AWLDependentObject(Addr *objReturn, Addr parent)
{
  Word *object;
  Word *wrapper;
  Word fword;
  Word fl;
  Word ff;

  AVER(objReturn != NULL);
  AVER(parent != (Addr)0);

  object = (Word *)parent;
  wrapper = (Word *)object[0];
  AVER(wrapper != NULL);
  /* check wrapper wrapper is non-NULL */
  AVER(wrapper[0] != 0);
  /* check wrapper wrapper is wrapper wrapper wrapper */
  AVER(wrapper[0] == ((Word *)wrapper[0])[0]);
  fword = wrapper[3];
  ff = fword & 3;
  /* Traceable Fixed part */
  AVER(ff == 1);
  fl = fword & ~3uL;
  /* At least one fixed field */
  AVER(fl >= 1);
  if (object[1] == 0)
    return FALSE;
  *objReturn = (Addr)object[1];
  return TRUE;
}


/* awlScanObject -- scan a single object */

static Res awlScanObject(Arena arena, ScanState ss,
                         FormatScanMethod scan, Addr base, Addr limit)
{
  Res res;
  Bool dependent;       /* is there a dependent object? */
  Addr dependentObject; /* base address of dependent object */
  Seg dependentSeg = NULL; /* segment of dependent object */

  AVERT(Arena, arena);
  AVERT(ScanState, ss);
  AVER(FUNCHECK(scan));
  AVER(base != 0);
  AVER(base < limit);

  dependent = AWLDependentObject(&dependentObject, base)
              && SegOfAddr(&dependentSeg, arena, dependentObject);

  if (dependent) {
    /* design.mps.poolawl.fun.scan.pass.object.dependent.expose */
    ShieldExpose(arena, dependentSeg);
    /* design.mps.poolawl.fun.scan.pass.object.dependent.summary */
    SegSetSummary(dependentSeg, RefSetUNIV);
  }

  res = (*scan)(ss, base, limit);
  if (res == ResOK)
    ss->scannedSize += AddrOffset(base, limit);

  if (dependent)
    ShieldCover(arena, dependentSeg);

  return res;
}


/* awlScanSinglePass -- a single scan pass over a segment */

static Res awlScanSinglePass(Bool *anyScannedReturn,
                             ScanState ss, Pool pool,
                             Seg seg, Bool scanAllObjects)
{
  Addr base, limit, bufferScanLimit;
  Addr p;
  Arena arena;
  AWL awl;
  AWLSeg awlseg;
  Buffer buffer;

  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVERT(Bool, scanAllObjects);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  arena = PoolArena(pool);
  AVERT(Arena, arena);

  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);
  *anyScannedReturn = FALSE;
  base = SegBase(seg);
  limit = SegLimit(seg);
  p = base;
  buffer = SegBuffer(seg);
  if (buffer != NULL && BufferScanLimit(buffer) != BufferLimit(buffer))
    bufferScanLimit = BufferScanLimit(buffer);
  else
    bufferScanLimit = limit;

  while(p < limit) {
    Index i;        /* the index into the bit tables corresponding to p */
    Addr objectLimit;

    /* design.mps.poolawl.fun.scan.pass.buffer */
    if (p == bufferScanLimit) {
      p = BufferLimit(buffer);
      continue;
    }

    i = awlIndexOfAddr(base, awl, p);
    if (!BTGet(awlseg->alloc, i)) {
      p = AddrAdd(p, pool->alignment);
      continue;
    }
    objectLimit = (*pool->format->skip)(p);
    /* design.mps.poolawl.fun.scan.pass.object */
    if (scanAllObjects
        || (BTGet(awlseg->mark, i) && !BTGet(awlseg->scanned, i))) {
      Res res = awlScanObject(arena, ss, pool->format->scan,
                              p, objectLimit);
      if (res != ResOK)
        return res;
      *anyScannedReturn = TRUE;
      BTSet(awlseg->scanned, i);
    }
    AVER(p < objectLimit);
    p = AddrAlignUp(objectLimit, pool->alignment);
  }
  AVER(p == limit);

  return ResOK;
}


/* AWLScan -- segment scan method for AWL */

static Res AWLScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  AWL awl;
  AWLSeg awlseg;
  Bool anyScanned;
  Bool scanAllObjects;
  Res res;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);

  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);

  /* If the scanner isn't going to scan all the objects then the */
  /* summary of the unscanned objects must be added into the scan */
  /* state summary, so that it's a valid summary of the entire */
  /* segment on return. */

  /* This pool assumes disjoint white sets and maintains mark and */
  /* scanned tables (effectively non-white and black tables) with */
  /* respect to the trace with respect to which the segment is */
  /* white.  For any other trace, we cannot tell which objects */
  /* are grey and must therefore scan them all. */

  scanAllObjects =
    (TraceSetDiff(ss->traces, SegWhite(seg)) != TraceSetEMPTY);

  do {
    res = awlScanSinglePass(&anyScanned, ss, pool, seg, scanAllObjects);
    if (res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  /* we are done if we scanned all the objects or if we did a pass */
  /* and didn't scan any objects (since then, no new object can have */
  /* gotten fixed) */
  } while(!scanAllObjects && anyScanned);

  *totalReturn = scanAllObjects;
  AWLNoteScan(awl, seg, ss);
  return ResOK;
}


/* AWLFix -- Fix method for AWL */

static Res AWLFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  Ref ref;
  Index i;
  AWL awl;
  AWLSeg awlseg;

  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER(refIO != NULL);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg  = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  ref = *refIO;
  i = awlIndexOfAddr(SegBase(seg), awl, ref);
  
  ss->wasMarked = TRUE;

  switch(ss->rank) {
  case RankAMBIG:
    /* not a real pointer if not aligned or not allocated */
    if (!AddrIsAligned((Addr)ref, pool->alignment) || !BTGet(awlseg->alloc, i))
      return ResOK;
    /* falls through */
  case RankEXACT:
  case RankFINAL:
  case RankWEAK:
    if (!BTGet(awlseg->mark, i)) {
      ss->wasMarked = FALSE;
      if (ss->rank == RankWEAK) {
        *refIO = (Ref)0;
      } else {
        BTSet(awlseg->mark, i);
        SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
      }
    }
    break;
  default:
    NOTREACHED;
    return ResUNIMPL;
  }

  return ResOK;
}


/* AWLReclaim -- reclaim dead objects in an AWL segment */

static void AWLReclaim(Pool pool, Trace trace, Seg seg)
{
  Addr base;
  AWL awl;
  AWLSeg awlseg;
  Index i;
  Count oldFree;
  Count preservedInPlaceCount = (Count)0;
  Size preservedInPlaceSize = (Size)0;
  Size freed; /* amount reclaimed, in bytes */

  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  base = SegBase(seg);

  i = 0; oldFree = awlseg->free;
  while(i < awlseg->grains) {
    Addr p, q;
    Index j;

    if (!BTGet(awlseg->alloc, i)) {
      ++i;
      continue;
    }
    p = AddrAdd(base, i << awl->alignShift);
    if (SegBuffer(seg) != NULL) {
      Buffer buffer = SegBuffer(seg);

      if (p == BufferScanLimit(buffer)
          && BufferScanLimit(buffer) != BufferLimit(buffer)) {
        i = awlIndexOfAddr(base, awl, BufferLimit(buffer));
        continue;
      }
    }
    q = AddrAlignUp(pool->format->skip(p), pool->alignment);
    j = awlIndexOfAddr(base, awl, q);
    AVER(j <= awlseg->grains);
    if (BTGet(awlseg->mark, i)) {
      AVER(BTGet(awlseg->scanned, i));
      BTSetRange(awlseg->mark, i, j);
      BTSetRange(awlseg->scanned, i, j);
      ++preservedInPlaceCount;
      preservedInPlaceSize += AddrOffset(p, q);
    } else {
      BTResRange(awlseg->mark, i, j);
      BTSetRange(awlseg->scanned, i, j);
      BTResRange(awlseg->alloc, i, j);
      awlseg->free += j - i;
    }
    i = j;
  }
  AVER(i == awlseg->grains);

  freed = (awlseg->free - oldFree) << awl->alignShift;
  awl->size -= freed;
  trace->reclaimSize += freed;
  trace->preservedInPlaceCount += preservedInPlaceCount;
  trace->preservedInPlaceSize += preservedInPlaceSize;
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));
  /* @@@@ never frees a segment */
}


/* AWLAccess -- handle a barrier hit */

static Res AWLAccess(Pool pool, Seg seg, Addr addr, 
                     AccessSet mode, MutatorFaultContext context)
{
  AWL awl;
  Res res;

  AVERT(Pool, pool);
  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVER(SegPool(seg) == pool);

  /* Attempt scanning a single reference if permitted */
  if (AWLCanTrySingleAccess(awl, seg, addr)) {
    res = PoolSingleAccess(pool, seg, addr, mode, context);
    switch(res) {
    case ResOK:
      AWLNoteRefAccess(awl, seg, addr);
      return ResOK;
    case ResFAIL:
      /* Not all accesses can be managed singly. Default to segment */
      break;
    default:
      return res;
    }
  } 

  /* Have to scan the entire seg anyway. */
  res = PoolSegAccess(pool, seg, addr, mode, context);
  if (ResOK == res)
    AWLNoteSegAccess(awl, seg, addr);

  return res;
}


/* AWLWalk -- walk all objects */

static void AWLWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
                    void *p, unsigned long s)
{
  AWL awl;
  AWLSeg awlseg;
  Addr object, base, limit;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  base = SegBase(seg);
  object = base;
  limit = SegLimit(seg);

  while(object < limit) {
    /* object is a slight misnomer because it might point to a */
    /* free grain */
    Addr next;
    Index i;

    if (SegBuffer(seg) != NULL) {
      Buffer buffer = SegBuffer(seg);
      if (object == BufferScanLimit(buffer)
          && BufferScanLimit(buffer) != BufferLimit(buffer)) {
        /* skip over buffered area */
        object = BufferLimit(buffer);
        continue;
      }
      /* since we skip over the buffered area we are always */
      /* either before the buffer, or after it, never in it */
      AVER(object < BufferGetInit(buffer) || BufferLimit(buffer) <= object);
    }
    i = awlIndexOfAddr(base, awl, object);
    if (!BTGet(awlseg->alloc, i)) {
      /* This grain is free */
      object = AddrAdd(object, pool->alignment);
      continue;
    }
    next = AddrAlignUp((*pool->format->skip)(object), pool->alignment);
    if (BTGet(awlseg->mark, i) && BTGet(awlseg->scanned, i))
      (*f)(object, pool->format, pool, p, s);
    object = next;
  }
}


/* AWLPoolClass -- the class definition */

DEFINE_POOL_CLASS(AWLPoolClass, this)
{
  INHERIT_CLASS(this, AbstractCollectPoolClass);
  PoolClassMixInFormat(this);
  this->name = "AWL";
  this->size = sizeof(AWLStruct);
  this->offset = offsetof(AWLStruct, poolStruct);
  this->init = AWLInit;
  this->finish = AWLFinish;
  this->bufferClass = RankBufClassGet;
  this->bufferFill = AWLBufferFill;
  this->bufferEmpty = AWLBufferEmpty;
  this->access = AWLAccess;
  this->whiten = AWLWhiten;
  this->grey = AWLGrey;
  this->blacken = AWLBlacken;
  this->scan = AWLScan;
  this->fix = AWLFix;
  this->fixEmergency = AWLFix;
  this->reclaim = AWLReclaim;
  this->walk = AWLWalk;
}


mps_class_t mps_class_awl(void)
{
  return (mps_class_t)AWLPoolClassGet();
}


/* AWLCheck -- check an AWL pool */

static Bool AWLCheck(AWL awl)
{
  CHECKS(AWL, awl);
  CHECKD(Pool, &awl->poolStruct);
  CHECKL(awl->poolStruct.class == AWLPoolClassGet());
  CHECKL(1uL << awl->alignShift == awl->poolStruct.alignment);
  CHECKD(Chain, awl->chain);
  /* 30 is just a sanity check really, not a constraint. */
  CHECKL(0 <= awl->gen && awl->gen <= 30);
  /* Nothing to check about succAccesses. */
  /* Don't bother to check stats. */
  return TRUE;
}

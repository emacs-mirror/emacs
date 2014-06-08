/* poolawl.c: AUTOMATIC WEAK LINKED POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 *
 * DESIGN
 *
 * .design: See <design/poolawl/>.  This is Dylan-specific pool.
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

SRCID(poolawl, "$Id$");


#define AWLSig ((Sig)0x519B7A37) /* SIGnature PooL AWL */


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

/* the type of a function to find an object's dependent object */

typedef Addr (*FindDependentMethod)(Addr object);

/* AWLStruct -- AWL pool structure
 *
 * See <design/poolawl/#poolstruct>
 */

typedef struct AWLStruct {
  PoolStruct poolStruct;
  Shift alignShift;
  PoolGenStruct pgen;       /* generation representing the pool */
  Count succAccesses;       /* number of successive single accesses */
  FindDependentMethod findDependent; /*  to find a dependent object */
  awlStatTotalStruct stats;
  Sig sig;
} AWLStruct, *AWL;

#define Pool2AWL(pool) PARENT(AWLStruct, poolStruct, pool)
#define AWLGrainsSize(awl, grains) ((grains) << (awl)->alignShift)


static Bool AWLCheck(AWL awl);


/* Conversion between indexes and Addrs */
#define awlIndexOfAddr(base, awl, p) \
  (AddrOffset((base), (p)) >> (awl)->alignShift)
#define awlAddrOfIndex(base, awl, i) \
  AddrAdd(base, AWLGrainsSize(awl, i))


/* AWLSegStruct -- AWL segment subclass
 *
 * Subclass of GCSeg
 */

#define AWLSegSig ((Sig)0x519A3759) /* SIGnature AWL SeG */

/* <design/poolawl/#seg> */
typedef struct AWLSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  BT mark;
  BT scanned;
  BT alloc;
  Count grains;
  Count freeGrains;         /* free grains */
  Count oldGrains;          /* grains allocated prior to last collection */
  Count newGrains;          /* grains allocated since last collection */
  Count singleAccesses;     /* number of accesses processed singly */
  awlStatSegStruct stats;
  Sig sig;
} AWLSegStruct, *AWLSeg;

#define Seg2AWLSeg(seg)             ((AWLSeg)(seg))
#define AWLSeg2Seg(awlseg)          ((Seg)(awlseg))


extern SegClass AWLSegClassGet(void);


ATTRIBUTE_UNUSED
static Bool AWLSegCheck(AWLSeg awlseg)
{
  CHECKS(AWLSeg, awlseg);
  CHECKD(GCSeg, &awlseg->gcSegStruct);
  CHECKL(awlseg->mark != NULL);
  CHECKL(awlseg->scanned != NULL);
  CHECKL(awlseg->alloc != NULL);
  CHECKL(awlseg->grains > 0);
  CHECKL(awlseg->grains == awlseg->freeGrains + awlseg->oldGrains + awlseg->newGrains);
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

ARG_DEFINE_KEY(awl_seg_rank_set, RankSet);
#define awlKeySegRankSet (&_mps_key_awl_seg_rank_set)

static Res AWLSegInit(Seg seg, Pool pool, Addr base, Size size,
                      Bool reservoirPermit, ArgList args)
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
  ArgStruct arg;

  AVERT(Seg, seg);
  awlseg = Seg2AWLSeg(seg);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  /* no useful checks for base and size */
  AVERT(Bool, reservoirPermit);
  ArgRequire(&arg, args, awlKeySegRankSet);
  rankSet = arg.val.u;
  AVERT(RankSet, rankSet);
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
  awlseg->freeGrains = bits;
  awlseg->oldGrains = (Count)0;
  awlseg->newGrains = (Count)0;
  awlseg->singleAccesses = 0;
  awlStatSegInit(awlseg);
  awlseg->sig = AWLSegSig;
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
  AVERT(SegClass, class);
}


/* Single access pattern control parameters
 *
 * These control the number of expensive emulated single-accesses we allow
 * before we give up and scan a segment at whatever rank, possibly causing
 * retention of weak objects.
 *
 * AWLSegSALimit is the number of accesses for a single segment in a GC cycle.
 * AWLTotalSALimit is the total number of accesses during a GC cycle.
 *
 * These should be set in config.h, but are here in static variables so that
 * it's possible to tweak them in a debugger.
 */

extern Count AWLSegSALimit;
Count AWLSegSALimit = AWL_SEG_SA_LIMIT;
extern Bool AWLHaveSegSALimit;
Bool AWLHaveSegSALimit = AWL_HAVE_SEG_SA_LIMIT;

extern Count AWLTotalSALimit;
Count AWLTotalSALimit = AWL_TOTAL_SA_LIMIT;
extern Bool AWLHaveTotalSALimit;
Bool AWLHaveTotalSALimit = AWL_HAVE_TOTAL_SA_LIMIT;


/* Determine whether to permit scanning a single ref. */

static Bool AWLCanTrySingleAccess(Arena arena, AWL awl, Seg seg, Addr addr)
{
  AWLSeg awlseg;

  AVERT(AWL, awl);
  AVERT(Seg, seg);
  AVER(addr != NULL);

  /* .assume.noweak */
  /* .assume.alltraceable */
  if (!RankSetIsMember(SegRankSet(seg), RankWEAK))
    return FALSE;

  /* If there are no traces in progress then the segment isn't read
     protected and this is just an ordinary write barrier hit.  No need to
     scan at all. */
  if (arena->flippedTraces == TraceSetEMPTY) {
    AVER(!(SegSM(seg) & AccessREAD));
    return FALSE;
  }

  /* The trace is already in the weak band, so we can scan the whole
     segment without retention anyway.  Go for it. */
  if (TraceRankForAccess(arena, seg) == RankWEAK)
    return FALSE;

  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  /* If there have been too many single accesses in a row then don't
     keep trying them, even if it means retaining objects. */
  if(AWLHaveTotalSALimit) {
    if(awl->succAccesses >= AWLTotalSALimit) {
      STATISTIC(awl->stats.declined++);
      EVENT2(AWLDeclineTotal, seg, (EventFU)awl->succAccesses);
      return FALSE; /* decline single access because of total limit */
    }
  }

  /* If there have been too many single accesses to this segment
     then don't keep trying them, even if it means retaining objects.
     (Observed behaviour in Open Dylan 2012-09-10 by RB.) */
  if(AWLHaveSegSALimit) {
    if(awlseg->singleAccesses >= AWLSegSALimit) {
      STATISTIC(awl->stats.declined++);
      EVENT2(AWLDeclineSeg, seg, (EventFU)awlseg->singleAccesses);
      return FALSE; /* decline single access because of segment limit */
    }
  }

  return TRUE;
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

  AVER(awlsegReturn != NULL);
  AVERT(RankSet, rankSet);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(Bool, reservoirPermit);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  size = SizeAlignUp(size, ArenaAlign(arena));
  /* beware of large sizes overflowing upon rounding */
  if (size == 0)
    return ResMEMORY;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD_FIELD(args, awlKeySegRankSet, u, rankSet);
    res = PoolGenAlloc(&seg, &awl->pgen, AWLSegClassGet(), size,
                       reservoirPermit, args);
  } MPS_ARGS_END(args);
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
  AVER(AWLGrainsSize(awl, size) >= size);
  seg = AWLSeg2Seg(awlseg);

  if (size > SegSize(seg))
    return FALSE;
  n = size >> awl->alignShift;
  if (!BTFindLongResRange(&i, &j, awlseg->alloc, 0, awlseg->grains, n))
    return FALSE;
  *baseReturn = awlAddrOfIndex(SegBase(seg), awl, i);
  *limitReturn = awlAddrOfIndex(SegBase(seg),awl, j);
  return TRUE;
}


/* AWLVarargs -- decode obsolete varargs */

static void AWLVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_FORMAT;
  args[0].val.format = va_arg(varargs, Format);
  args[1].key = MPS_KEY_AWL_FIND_DEPENDENT;
  args[1].val.addr_method = va_arg(varargs, mps_awl_find_dependent_t);
  args[2].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}


/* AWLInit -- initialize an AWL pool */

ARG_DEFINE_KEY(awl_find_dependent, Fun);

static Res AWLInit(Pool pool, ArgList args)
{
  AWL awl;
  Format format;
  FindDependentMethod findDependent;
  Chain chain;
  Res res;
  ArgStruct arg;
  unsigned gen = AWL_GEN_DEFAULT;

  /* Weak check, as half-way through initialization. */
  AVER(pool != NULL);

  awl = Pool2AWL(pool);
  
  ArgRequire(&arg, args, MPS_KEY_FORMAT);
  format = arg.val.format;
  ArgRequire(&arg, args, MPS_KEY_AWL_FIND_DEPENDENT);
  findDependent = (FindDependentMethod)arg.val.addr_method;
  if (ArgPick(&arg, args, MPS_KEY_CHAIN))
    chain = arg.val.chain;
  else {
    chain = ArenaGlobals(PoolArena(pool))->defaultChain;
    gen = 1; /* avoid the nursery of the default chain by default */
  }
  if (ArgPick(&arg, args, MPS_KEY_GEN))
    gen = arg.val.u;

  AVERT(Format, format);
  pool->format = format;
  pool->alignment = format->alignment;

  AVER(FUNCHECK(findDependent));
  awl->findDependent = findDependent;

  AVERT(Chain, chain);
  AVER(gen <= ChainGens(chain));

  res = PoolGenInit(&awl->pgen, ChainGen(chain, gen), pool);
  if (res != ResOK)
    goto failGenInit;

  awl->alignShift = SizeLog2(PoolAlignment(pool));
  awl->succAccesses = 0;
  awlStatTotalInit(awl);
  awl->sig = AWLSig;

  AVERT(AWL, awl);
  EVENT2(PoolInitAWL, pool, format);
  return ResOK;

failGenInit:
  AVER(res != ResOK);
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
    AWLSeg awlseg = Seg2AWLSeg(seg);
    AVERT(AWLSeg, awlseg);
    PoolGenFree(&awl->pgen, seg,
                AWLGrainsSize(awl, awlseg->freeGrains),
                AWLGrainsSize(awl, awlseg->oldGrains),
                AWLGrainsSize(awl, awlseg->newGrains),
                FALSE);
  }
  awl->sig = SigInvalid;
  PoolGenFinish(&awl->pgen);
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
  AVERT(Bool, reservoirPermit);

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
    if (SegBuffer(seg) == NULL
        && SegRankSet(seg) == BufferRankSet(buffer)
        && AWLGrainsSize(awl, awlseg->freeGrains) >= size
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
    AVER(awlseg->freeGrains >= j - i);
    awlseg->freeGrains -= j - i;
    awlseg->newGrains += j - i;
    PoolGenAccountForFill(&awl->pgen, AddrOffset(base, limit), FALSE);
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
    AVER(awlseg->newGrains >= j - i);
    awlseg->newGrains -= j - i;
    awlseg->freeGrains += j - i;
    PoolGenAccountForEmpty(&awl->pgen, AddrOffset(init, limit), FALSE);
  }
}


/* AWLWhiten -- segment condemning method */

/* awlRangeWhiten -- helper function that works on a range.
 *
 * This function abstracts common code from AWLWhiten.
 */
static void awlRangeWhiten(AWLSeg awlseg, Index base, Index limit)
{
  if(base != limit) {
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
  Count uncondemned;

  /* All parameters checked by generic PoolWhiten. */

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);
  buffer = SegBuffer(seg);

  /* Can only whiten for a single trace, */
  /* see <design/poolawl/#fun.condemn> */
  AVER(SegWhite(seg) == TraceSetEMPTY);

  if(buffer == NULL) {
    awlRangeWhiten(awlseg, 0, awlseg->grains);
    uncondemned = (Count)0;
  } else {
    /* Whiten everything except the buffer. */
    Addr base = SegBase(seg);
    Index scanLimitIndex = awlIndexOfAddr(base, awl, BufferScanLimit(buffer));
    Index limitIndex = awlIndexOfAddr(base, awl, BufferLimit(buffer));
    uncondemned = limitIndex - scanLimitIndex;
    awlRangeWhiten(awlseg, 0, scanLimitIndex);
    awlRangeWhiten(awlseg, limitIndex, awlseg->grains);

    /* Check the buffer is black. */
    /* This really ought to change when we have a non-trivial */
    /* pre-flip phase. @@@@ ('coz then we'll be allocating white) */
    if(scanLimitIndex != limitIndex) {
      AVER(BTIsSetRange(awlseg->mark, scanLimitIndex, limitIndex));
      AVER(BTIsSetRange(awlseg->scanned, scanLimitIndex, limitIndex));
    }
  }

  PoolGenAccountForAge(&awl->pgen, AWLGrainsSize(awl, awlseg->newGrains - uncondemned), FALSE);
  awlseg->oldGrains += awlseg->newGrains - uncondemned;
  awlseg->newGrains = uncondemned;
  trace->condemned += AWLGrainsSize(awl, awlseg->oldGrains);
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
  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  BTSetRange(awlseg->scanned, 0, awlseg->grains);
}


/* awlScanObject -- scan a single object */
/* base and limit are both offset by the header size */

static Res awlScanObject(Arena arena, AWL awl, ScanState ss,
                         Format format, Addr base, Addr limit)
{
  Res res;
  Bool dependent;       /* is there a dependent object? */
  Addr dependentObject; /* base address of dependent object */
  Seg dependentSeg = NULL; /* segment of dependent object */

  AVERT(Arena, arena);
  AVERT(AWL, awl);
  AVERT(ScanState, ss);
  AVERT(Format, format);
  AVER(base != 0);
  AVER(base < limit);

  dependentObject = awl->findDependent(base);
  dependent = SegOfAddr(&dependentSeg, arena, dependentObject);
  if (dependent) {
      /* <design/poolawl/#fun.scan.pass.object.dependent.expose> */
      ShieldExpose(arena, dependentSeg);
      /* <design/poolawl/#fun.scan.pass.object.dependent.summary> */
      SegSetSummary(dependentSeg, RefSetUNIV);
  }

  res = (*format->scan)(&ss->ss_s, base, limit);
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
  Addr hp;
  Arena arena;
  AWL awl;
  AWLSeg awlseg;
  Buffer buffer;
  Format format;

  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVERT(Bool, scanAllObjects);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  arena = PoolArena(pool);
  AVERT(Arena, arena);

  format = pool->format;
  AVERT(Format, format);

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

    /* <design/poolawl/#fun.scan.pass.buffer> */
    if (p == bufferScanLimit) {
      p = BufferLimit(buffer);
      continue;
    }

    i = awlIndexOfAddr(base, awl, p);
    if (!BTGet(awlseg->alloc, i)) {
      p = AddrAdd(p, PoolAlignment(pool));
      continue;
    }
    hp = AddrAdd(p, format->headerSize);
    objectLimit = (format->skip)(hp);
    /* <design/poolawl/#fun.scan.pass.object> */
    if (scanAllObjects
        || (BTGet(awlseg->mark, i) && !BTGet(awlseg->scanned, i))) {
      Res res = awlScanObject(arena, awl, ss, pool->format,
                              hp, objectLimit);
      if (res != ResOK)
        return res;
      *anyScannedReturn = TRUE;
      BTSet(awlseg->scanned, i);
    }
    objectLimit = AddrSub(objectLimit, format->headerSize);
    AVER(p < objectLimit);
    AVER(AddrIsAligned(objectLimit, PoolAlignment(pool)));
    p = objectLimit;
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
  Ref clientRef;
  Addr base;
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

  clientRef = *refIO;
  ss->wasMarked = TRUE;

  base = AddrSub((Addr)clientRef, pool->format->headerSize);
  /* can get an ambiguous reference to close to the base of the
   * segment, so when we subtract the header we are not in the
   * segment any longer.  This isn't a real reference,
   * so we can just skip it.  */
  if (base < SegBase(seg)) {
    return ResOK;
  }
  i = awlIndexOfAddr(SegBase(seg), awl, base);

  switch(ss->rank) {
  case RankAMBIG:
    /* not a real pointer if not aligned or not allocated */
    if (!AddrIsAligned(base, sizeof(void *)) || !BTGet(awlseg->alloc, i))
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
  Buffer buffer;
  Index i;
  Count oldFree;
  Format format;
  Count reclaimedGrains = (Count)0;
  Count preservedInPlaceCount = (Count)0;
  Size preservedInPlaceSize = (Size)0;

  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  format = pool->format;

  base = SegBase(seg);
  buffer = SegBuffer(seg);

  i = 0;
  oldFree = awlseg->freeGrains;
  while(i < awlseg->grains) {
    Addr p, q;
    Index j;

    if(!BTGet(awlseg->alloc, i)) {
      ++i;
      continue;
    }
    p = awlAddrOfIndex(base, awl, i);
    if (buffer != NULL
        && p == BufferScanLimit(buffer)
        && BufferScanLimit(buffer) != BufferLimit(buffer))
    {
      i = awlIndexOfAddr(base, awl, BufferLimit(buffer));
      continue;
    }
    q = format->skip(AddrAdd(p, format->headerSize));
    q = AddrSub(q, format->headerSize);
    AVER(AddrIsAligned(q, PoolAlignment(pool)));
    j = awlIndexOfAddr(base, awl, q);
    AVER(j <= awlseg->grains);
    if(BTGet(awlseg->mark, i)) {
      AVER(BTGet(awlseg->scanned, i));
      BTSetRange(awlseg->mark, i, j);
      BTSetRange(awlseg->scanned, i, j);
      ++preservedInPlaceCount;
      preservedInPlaceSize += AddrOffset(p, q);
    } else {
      BTResRange(awlseg->mark, i, j);
      BTSetRange(awlseg->scanned, i, j);
      BTResRange(awlseg->alloc, i, j);
      reclaimedGrains += j - i;
    }
    i = j;
  }
  AVER(i == awlseg->grains);

  AVER(reclaimedGrains <= awlseg->grains);
  AVER(awlseg->oldGrains >= reclaimedGrains);
  awlseg->oldGrains -= reclaimedGrains;
  awlseg->freeGrains += reclaimedGrains;
  PoolGenAccountForReclaim(&awl->pgen, AWLGrainsSize(awl, reclaimedGrains), FALSE);

  trace->reclaimSize += AWLGrainsSize(awl, reclaimedGrains);
  trace->preservedInPlaceCount += preservedInPlaceCount;
  trace->preservedInPlaceSize += preservedInPlaceSize;
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));

  if (awlseg->freeGrains == awlseg->grains && buffer == NULL)
    /* No survivors */
    PoolGenFree(&awl->pgen, seg,
                AWLGrainsSize(awl, awlseg->freeGrains),
                AWLGrainsSize(awl, awlseg->oldGrains),
                AWLGrainsSize(awl, awlseg->newGrains),
                FALSE);
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
  if(AWLCanTrySingleAccess(PoolArena(pool), awl, seg, addr)) {
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
  if(ResOK == res) {
    AWLNoteSegAccess(awl, seg, addr);
  }

  return res;
}


/* AWLWalk -- walk all objects */

static void AWLWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
                    void *p, size_t s)
{
  AWL awl;
  AWLSeg awlseg;
  Addr object, base, limit;
  Format format;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  awl = Pool2AWL(pool);
  AVERT(AWL, awl);
  awlseg = Seg2AWLSeg(seg);
  AVERT(AWLSeg, awlseg);

  format = pool->format;

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
      object = AddrAdd(object, PoolAlignment(pool));
      continue;
    }
    object = AddrAdd(object, format->headerSize);
    next = format->skip(object);
    next = AddrSub(next, format->headerSize);
    AVER(AddrIsAligned(next, PoolAlignment(pool)));
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
  this->varargs = AWLVarargs;
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
  AVERT(PoolClass, this);
}


mps_class_t mps_class_awl(void)
{
  return (mps_class_t)AWLPoolClassGet();
}


/* AWLCheck -- check an AWL pool */

ATTRIBUTE_UNUSED
static Bool AWLCheck(AWL awl)
{
  CHECKS(AWL, awl);
  CHECKD(Pool, &awl->poolStruct);
  CHECKL(awl->poolStruct.class == AWLPoolClassGet());
  CHECKL(AWLGrainsSize(awl, 1) == awl->poolStruct.alignment);
  /* Nothing to check about succAccesses. */
  CHECKL(FUNCHECK(awl->findDependent));
  /* Don't bother to check stats. */
  return TRUE;
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

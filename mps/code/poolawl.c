/* poolawl.c: AUTOMATIC WEAK LINKED POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2018 Ravenbrook Limited.  See end of file for license.
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
#include "locus.h"

SRCID(poolawl, "$Id$");


#define AWLSig ((Sig)0x519B7A37) /* SIGnature PooL AWL */

static Bool awlSegBufferFill(Addr *baseReturn, Addr *limitReturn,
                             Seg seg, Size size, RankSet rankSet);
static void awlSegBufferEmpty(Seg seg, Buffer buffer);
static Res awlSegAccess(Seg seg, Arena arena, Addr addr,
                        AccessSet mode, MutatorContext context);
static Res awlSegWhiten(Seg seg, Trace trace);
static void awlSegGreyen(Seg seg, Trace trace);
static void awlSegBlacken(Seg seg, TraceSet traceSet);
static Res awlSegScan(Bool *totalReturn, Seg seg, ScanState ss);
static Res awlSegFix(Seg seg, ScanState ss, Ref *refIO);
static void awlSegReclaim(Seg seg, Trace trace);
static void awlSegWalk(Seg seg, Format format, FormattedObjectsVisitor f,
                       void *p, size_t s);


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

typedef Addr (*FindDependentFunction)(Addr object);

/* AWLStruct -- AWL pool structure
 *
 * See <design/poolawl/#poolstruct>
 */

typedef struct AWLPoolStruct {
  PoolStruct poolStruct;
  PoolGenStruct pgenStruct; /* generation representing the pool */
  PoolGen pgen;             /* NULL or pointer to pgenStruct */
  Count succAccesses;       /* number of successive single accesses */
  FindDependentFunction findDependent; /*  to find a dependent object */
  awlStatTotalStruct stats;
  Sig sig;                  /* <code/misc.h#sig> */
} AWLPoolStruct, *AWL;


static Bool AWLCheck(AWL awl);


typedef AWL AWLPool;
#define AWLPoolCheck AWLCheck
DECLARE_CLASS(Pool, AWLPool, AbstractCollectPool);


/* AWLSegStruct -- AWL segment subclass
 *
 * Colour is represented as follows:
 * Black: +alloc +mark +scanned
 * White: +alloc -mark -scanned
 * Grey: +alloc +mark -scanned
 * Free: -alloc ?mark ?scanned
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
  Count bufferedGrains;     /* grains in buffers */
  Count newGrains;          /* grains allocated since last collection */
  Count oldGrains;          /* grains allocated prior to last collection */
  Count singleAccesses;     /* number of accesses processed singly */
  awlStatSegStruct stats;
  Sig sig;                  /* <code/misc.h#sig> */
} AWLSegStruct, *AWLSeg;

DECLARE_CLASS(Seg, AWLSeg, MutatorSeg);

ATTRIBUTE_UNUSED
static Bool AWLSegCheck(AWLSeg awlseg)
{
  CHECKS(AWLSeg, awlseg);
  CHECKD(GCSeg, &awlseg->gcSegStruct);
  CHECKL(awlseg->mark != NULL);
  CHECKL(awlseg->scanned != NULL);
  CHECKL(awlseg->alloc != NULL);
  CHECKL(awlseg->grains > 0);
  CHECKL(awlseg->grains == awlseg->freeGrains + awlseg->bufferedGrains
         + awlseg->newGrains + awlseg->oldGrains);
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

static Res AWLSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  AWLSeg awlseg;
  Arena arena;
  RankSet rankSet;
  Count bits;        /* number of grains */
  Res res;
  Size tableSize;
  void *v;
  ArgStruct arg;

  ArgRequire(&arg, args, awlKeySegRankSet);
  rankSet = arg.val.u;
  AVERT(RankSet, rankSet);
  /* .assume.samerank */
  /* AWL only accepts two ranks */
  AVER(RankSetSingle(RankEXACT) == rankSet
       || RankSetSingle(RankWEAK) == rankSet);

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, AWLSeg, init)(seg, pool, base, size, args);
  if (res != ResOK)
    goto failSuperInit;
  awlseg = CouldBeA(AWLSeg, seg);

  AVERT(Pool, pool);
  arena = PoolArena(pool);
  /* no useful checks for base and size */

  bits = PoolSizeGrains(pool, size);
  tableSize = BTSize(bits);
  res = ControlAlloc(&v, arena, 3 * tableSize);
  if (res != ResOK)
    goto failControlAlloc;
  awlseg->mark = v;
  awlseg->scanned = PointerAdd(v, tableSize);
  awlseg->alloc = PointerAdd(v, 2 * tableSize);
  awlseg->grains = bits;
  BTResRange(awlseg->mark, 0, bits);
  BTResRange(awlseg->scanned, 0, bits);
  BTResRange(awlseg->alloc, 0, bits);
  SegSetRankAndSummary(seg, rankSet, RefSetUNIV);
  awlseg->freeGrains = bits;
  awlseg->bufferedGrains = (Count)0;
  awlseg->newGrains = (Count)0;
  awlseg->oldGrains = (Count)0;
  awlseg->singleAccesses = 0;
  awlStatSegInit(awlseg);

  SetClassOfPoly(seg, CLASS(AWLSeg));
  awlseg->sig = AWLSegSig;
  AVERC(AWLSeg, awlseg);

  return ResOK;

failControlAlloc:
  NextMethod(Inst, AWLSeg, finish)(MustBeA(Inst, seg));
failSuperInit:
  AVER(res != ResOK);
  return res;
}


/* AWLSegFinish -- Finish method for AWL segments */

static void AWLSegFinish(Inst inst)
{
  Seg seg = MustBeA(Seg, inst);
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Pool pool = SegPool(seg);
  Arena arena = PoolArena(pool);
  Size tableSize;
  Count segGrains;

  /* This is one of the few places where it is easy to check */
  /* awlseg->grains, so we do */
  segGrains = PoolSizeGrains(pool, SegSize(seg));
  AVER(segGrains == awlseg->grains);
  tableSize = BTSize(segGrains);
  ControlFree(arena, awlseg->mark, 3 * tableSize);
  awlseg->sig = SigInvalid;

  /* finish the superclass fields last */
  NextMethod(Inst, AWLSeg, finish)(inst);
}


/* AWLSegClass -- Class definition for AWL segments */

DEFINE_CLASS(Seg, AWLSeg, klass)
{
  INHERIT_CLASS(klass, AWLSeg, MutatorSeg);
  SegClassMixInNoSplitMerge(klass);  /* no support for this (yet) */
  klass->instClassStruct.finish = AWLSegFinish;
  klass->size = sizeof(AWLSegStruct);
  klass->init = AWLSegInit;
  klass->bufferFill = awlSegBufferFill;
  klass->bufferEmpty = awlSegBufferEmpty;
  klass->access = awlSegAccess;
  klass->whiten = awlSegWhiten;
  klass->greyen = awlSegGreyen;
  klass->blacken = awlSegBlacken;
  klass->scan = awlSegScan;
  klass->fix = awlSegFix;
  klass->fixEmergency = awlSegFix;
  klass->reclaim = awlSegReclaim;
  klass->walk = awlSegWalk;
  AVERT(SegClass, klass);
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
 * These should be set in config.h, but are here in global variables so that
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

static Bool awlSegCanTrySingleAccess(Arena arena, Seg seg, Addr addr)
{
  AWLSeg awlseg;
  AWL awl;

  AVERT(Arena, arena);
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

  awlseg = MustBeA(AWLSeg, seg);
  awl = MustBeA(AWLPool, SegPool(seg));

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
  AWLSeg awlseg = MustBeA(AWLSeg, seg);

  AVERT(AWL, awl);
  AVER(addr != NULL);

  awlseg->singleAccesses++; /* increment seg count of ref accesses */
  STATISTIC({
    if (addr == awlseg->stats.lastAccess) {
      /* If this is a repeated access, increment count  */
      ++ awlseg->stats.sameAccesses;
    }
    awlseg->stats.lastAccess = addr;
  });
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

static void AWLNoteScan(Seg seg, ScanState ss)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  UNUSED(ss);

  /* .assume.mixedrank */
  /* .assume.samerank */
  if (RankSetIsMember(SegRankSet(seg), RankWEAK)) {
    STATISTIC({
      /* If this segment has any RankWEAK references, then record
       * statistics about whether weak splatting is being lost. */
      AWL awl = MustBeA(AWLPool, SegPool(seg));
      if (RankWEAK == ss->rank) {
        /* This is "successful" scan at proper rank. */
        ++ awl->stats.goodScans;
        if (0 < awlseg->singleAccesses) {
          /* Accesses have been proceesed singly. Record that we
           * genuinely did save a protection-provoked scan */
          ++ awl->stats.savedScans;
          awl->stats.savedAccesses += awlseg->singleAccesses;
        }
      } else {
        /* This is "failed" scan at improper rank. */
        ++ awl->stats.badScans;
      }
      awlStatSegInit(awlseg);
    });
    /* Reinitialize the segment statistics */
    awlseg->singleAccesses = 0;
  }
}


/* awlSegBufferFill -- try filling buffer from segment */

static Bool awlSegBufferFill(Addr *baseReturn, Addr *limitReturn,
                             Seg seg, Size size, RankSet rankSet)
{
  Index baseIndex, limitIndex;
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Pool pool = SegPool(seg);
  Count requestedGrains, segGrains, allocatedGrains;
  Addr segBase, base, limit;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));
  AVER(size > 0);
  AVERT(RankSet, rankSet);

  requestedGrains = PoolSizeGrains(pool, size);
  if (awlseg->freeGrains < requestedGrains)
    /* Not enough space to satisfy the request. */
    return FALSE;

  if (SegHasBuffer(seg))
    /* Don't bother trying to allocate from a buffered segment */
    return FALSE;

  if (rankSet != SegRankSet(seg))
    /* Can't satisfy required rank set. */
    return FALSE;

  segGrains = PoolSizeGrains(pool, SegSize(seg));
  if (awlseg->freeGrains == segGrains) {
    /* Whole segment is free: no need for a search. */
    baseIndex = 0;
    limitIndex = segGrains;
    goto found;
  }

  if (!BTFindLongResRange(&baseIndex, &limitIndex, awlseg->alloc,
                          0, segGrains, requestedGrains))
    return FALSE;

found:
  AVER(baseIndex < limitIndex);
  allocatedGrains = limitIndex - baseIndex;
  AVER(requestedGrains <= allocatedGrains);
  AVER(BTIsResRange(awlseg->alloc, baseIndex, limitIndex));
  BTSetRange(awlseg->alloc, baseIndex, limitIndex);
  /* Objects are allocated black. */
  /* TODO: This should depend on trace phase. */
  BTSetRange(awlseg->mark, baseIndex, limitIndex);
  BTSetRange(awlseg->scanned, baseIndex, limitIndex);
  AVER(awlseg->freeGrains >= allocatedGrains);
  awlseg->freeGrains -= allocatedGrains;
  awlseg->bufferedGrains += allocatedGrains;

  segBase = SegBase(seg);
  base = PoolAddrOfIndex(segBase, pool, baseIndex);
  limit = PoolAddrOfIndex(segBase, pool, limitIndex);
  PoolGenAccountForFill(PoolSegPoolGen(pool, seg), AddrOffset(base, limit));

  *baseReturn = base;
  *limitReturn = limit;
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


/* awlNoDependent -- no dependent object */

static Addr awlNoDependent(Addr addr)
{
  UNUSED(addr);
  return NULL;
}


/* AWLInit -- initialize an AWL pool */

ARG_DEFINE_KEY(AWL_FIND_DEPENDENT, Fun);

static Res AWLInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  AWL awl;
  FindDependentFunction findDependent = awlNoDependent;
  Chain chain;
  Res res;
  ArgStruct arg;
  unsigned gen = AWL_GEN_DEFAULT;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);
  UNUSED(klass); /* used for debug pools only */

  if (ArgPick(&arg, args, MPS_KEY_AWL_FIND_DEPENDENT))
    findDependent = (FindDependentFunction)arg.val.addr_method;
  if (ArgPick(&arg, args, MPS_KEY_CHAIN))
    chain = arg.val.chain;
  else {
    chain = ArenaGlobals(arena)->defaultChain;
    gen = 1; /* avoid the nursery of the default chain by default */
  }
  if (ArgPick(&arg, args, MPS_KEY_GEN))
    gen = arg.val.u;

  res = NextMethod(Pool, AWLPool, init)(pool, arena, klass, args);
  if (res != ResOK)
    goto failNextInit;
  awl = CouldBeA(AWLPool, pool);

  /* Ensure a format was supplied in the argument list. */
  AVER(pool->format != NULL);
  pool->alignment = pool->format->alignment;
  pool->alignShift = SizeLog2(pool->alignment);

  AVER(FUNCHECK(findDependent));
  awl->findDependent = findDependent;

  AVERT(Chain, chain);
  AVER(gen <= ChainGens(chain));
  AVER(chain->arena == PoolArena(pool));

  awl->pgen = NULL;

  awl->succAccesses = 0;
  awlStatTotalInit(awl);

  SetClassOfPoly(pool, CLASS(AWLPool));
  awl->sig = AWLSig;
  AVERC(AWLPool, awl);

  res = PoolGenInit(&awl->pgenStruct, ChainGen(chain, gen), pool);
  if (res != ResOK)
    goto failGenInit;
  awl->pgen = &awl->pgenStruct;

  EVENT2(PoolInitAWL, pool, pool->format);

  return ResOK;

failGenInit:
  NextMethod(Inst, AWLPool, finish)(MustBeA(Inst, pool));
failNextInit:
  AVER(res != ResOK);
  return res;
}


/* AWLFinish -- finish an AWL pool */

static void AWLFinish(Inst inst)
{
  Pool pool = MustBeA(AbstractPool, inst);
  AWL awl = MustBeA(AWLPool, pool);
  Ring ring, node, nextNode;

  ring = &pool->segRing;
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    AWLSeg awlseg = MustBeA(AWLSeg, seg);
    AVER(!SegHasBuffer(seg));
    AVERT(AWLSeg, awlseg);
    AVER(awlseg->bufferedGrains == 0);
    PoolGenFree(awl->pgen, seg,
                PoolGrainsSize(pool, awlseg->freeGrains),
                PoolGrainsSize(pool, awlseg->oldGrains),
                PoolGrainsSize(pool, awlseg->newGrains),
                FALSE);
  }
  awl->sig = SigInvalid;
  PoolGenFinish(awl->pgen);

  NextMethod(Inst, AWLPool, finish)(inst);
}


/* awlBufferFill -- BufferFill method for AWL */

static Res awlBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
{
  AWL awl = MustBeA(AWLPool, pool);
  Res res;
  Ring node, nextNode;
  RankSet rankSet;
  Seg seg;
  Bool b;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERC(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  rankSet = BufferRankSet(buffer);
  RING_FOR(node, &pool->segRing, nextNode) {
    seg = SegOfPoolRing(node);
    if (SegBufferFill(baseReturn, limitReturn, seg, size, rankSet))
      return ResOK;
  }

  /* No segment had enough space, so make a new one. */
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD_FIELD(args, awlKeySegRankSet, u, BufferRankSet(buffer));
    res = PoolGenAlloc(&seg, awl->pgen, CLASS(AWLSeg),
                       SizeArenaGrains(size, PoolArena(pool)), args);
  } MPS_ARGS_END(args);
  if (res != ResOK)
    return res;
  b = SegBufferFill(baseReturn, limitReturn, seg, size, rankSet);
  AVER(b);
  return ResOK;
}


/* awlSegBufferEmpty -- empty buffer to segment */

static void awlSegBufferEmpty(Seg seg, Buffer buffer)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Pool pool = SegPool(seg);
  Addr segBase, bufferBase, init, limit;
  Index initIndex, limitIndex;
  Count unusedGrains, usedGrains;

  AVERT(Seg, seg);
  AVERT(Buffer, buffer);
  segBase = SegBase(seg);
  bufferBase = BufferBase(buffer);
  init = BufferGetInit(buffer);
  limit = BufferLimit(buffer);
  AVER(segBase <= bufferBase);
  AVER(bufferBase <= init);
  AVER(init <= limit);
  AVER(limit <= SegLimit(seg));

  initIndex = PoolIndexOfAddr(segBase, pool, init);
  limitIndex = PoolIndexOfAddr(segBase, pool, limit);

  if (initIndex < limitIndex)
    BTResRange(awlseg->alloc, initIndex, limitIndex);

  unusedGrains = limitIndex - initIndex;
  AVER(unusedGrains <= awlseg->bufferedGrains);
  usedGrains = awlseg->bufferedGrains - unusedGrains;
  awlseg->freeGrains += unusedGrains;
  awlseg->bufferedGrains = 0;
  awlseg->newGrains += usedGrains;

  PoolGenAccountForEmpty(PoolSegPoolGen(pool, seg),
                         PoolGrainsSize(pool, usedGrains),
                         PoolGrainsSize(pool, unusedGrains), FALSE);
}


/* awlSegPoolGen -- get pool generation for an AWL segment */

static PoolGen awlSegPoolGen(Pool pool, Seg seg)
{
  AWL awl = MustBeA(AWLPool, pool);
  AVERT(Seg, seg);
  return awl->pgen;
}


/* awlSegWhiten -- segment condemning method */

/* awlSegRangeWhiten -- helper function that works on a range.
 *
 * This function abstracts common code from awlSegWhiten.
 */
static void awlSegRangeWhiten(AWLSeg awlseg, Index base, Index limit)
{
  if(base != limit) {
    AVER(base < limit);
    AVER(limit <= awlseg->grains);
    BTResRange(awlseg->mark, base, limit);
    BTResRange(awlseg->scanned, base, limit);
  }
}

static Res awlSegWhiten(Seg seg, Trace trace)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Pool pool = SegPool(seg);
  PoolGen pgen = PoolSegPoolGen(pool, seg);
  Buffer buffer;
  Count agedGrains, uncondemnedGrains;

  /* All parameters checked by generic SegWhiten. */

  /* Can only whiten for a single trace, */
  /* see <design/poolawl/#fun.condemn> */
  AVER(SegWhite(seg) == TraceSetEMPTY);

  if (!SegBuffer(&buffer, seg)) {
    awlSegRangeWhiten(awlseg, 0, awlseg->grains);
    uncondemnedGrains = (Count)0;
  } else {
    /* Whiten everything except the buffer. */
    Addr base = SegBase(seg);
    Index scanLimitIndex = PoolIndexOfAddr(base, pool, BufferScanLimit(buffer));
    Index limitIndex = PoolIndexOfAddr(base, pool, BufferLimit(buffer));
    uncondemnedGrains = limitIndex - scanLimitIndex;
    awlSegRangeWhiten(awlseg, 0, scanLimitIndex);
    awlSegRangeWhiten(awlseg, limitIndex, awlseg->grains);

    /* Check the buffer is black. */
    /* This really ought to change when we have a non-trivial */
    /* pre-flip phase. @@@@ ('coz then we'll be allocating white) */
    if(scanLimitIndex != limitIndex) {
      AVER(BTIsSetRange(awlseg->mark, scanLimitIndex, limitIndex));
      AVER(BTIsSetRange(awlseg->scanned, scanLimitIndex, limitIndex));
    }
  }

  /* The unused part of the buffer remains buffered: the rest becomes old. */
  AVER(awlseg->bufferedGrains >= uncondemnedGrains);
  agedGrains = awlseg->bufferedGrains - uncondemnedGrains;
  PoolGenAccountForAge(pgen, PoolGrainsSize(pool, agedGrains),
                       PoolGrainsSize(pool, awlseg->newGrains), FALSE);
  awlseg->oldGrains += agedGrains + awlseg->newGrains;
  awlseg->bufferedGrains = uncondemnedGrains;
  awlseg->newGrains = 0;

  if (awlseg->oldGrains > 0) {
    GenDescCondemned(pgen->gen, trace,
                     PoolGrainsSize(pool, awlseg->oldGrains));
    SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
  }
  
  return ResOK;
}


/* awlSegGreyen -- Greyen method for AWL segments */

/* awlSegRangeGreyen -- subroutine for awlSegGreyen */
static void awlSegRangeGreyen(AWLSeg awlseg, Index base, Index limit)
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

static void awlSegGreyen(Seg seg, Trace trace)
{
  Buffer buffer;
  Pool pool;

  AVERT(Seg, seg);
  AVERT(Trace, trace);
  pool = SegPool(seg);
  AVER(PoolArena(pool) == trace->arena);

  if (!TraceSetIsMember(SegWhite(seg), trace)) {
    AWLSeg awlseg = MustBeA(AWLSeg, seg);

    SegSetGrey(seg, TraceSetAdd(SegGrey(seg), trace));
    if (SegBuffer(&buffer, seg)) {
      Addr base = SegBase(seg);

      awlSegRangeGreyen(awlseg,
                        0,
                        PoolIndexOfAddr(base, pool, BufferScanLimit(buffer)));
      awlSegRangeGreyen(awlseg,
                        PoolIndexOfAddr(base, pool, BufferLimit(buffer)),
                        awlseg->grains);
    } else {
      awlSegRangeGreyen(awlseg, 0, awlseg->grains);
    }
  }
}


/* awlSegBlacken -- Blacken method for AWL segments */

static void awlSegBlacken(Seg seg, TraceSet traceSet)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);

  AVERT(TraceSet, traceSet);

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

  res = FormatScan(format, ss, base, limit);

  if (dependent)
    ShieldCover(arena, dependentSeg);

  return res;
}


/* awlSegScanSinglePass -- a single scan pass over a segment */

static Res awlSegScanSinglePass(Bool *anyScannedReturn, ScanState ss,
                                Seg seg, Bool scanAllObjects)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Pool pool = SegPool(seg);
  AWL awl = MustBeA(AWLPool, pool);
  Arena arena = PoolArena(pool);
  Buffer buffer;
  Format format = pool->format;
  Addr base = SegBase(seg);
  Addr limit = SegLimit(seg);
  Addr bufferScanLimit;
  Addr p;
  Addr hp;

  AVERT(ScanState, ss);
  AVERT(Bool, scanAllObjects);

  *anyScannedReturn = FALSE;
  p = base;
  if (SegBuffer(&buffer, seg) && BufferScanLimit(buffer) != BufferLimit(buffer))
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

    i = PoolIndexOfAddr(base, pool, p);
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


/* awlSegScan -- segment scan method for AWL */

static Res awlSegScan(Bool *totalReturn, Seg seg, ScanState ss)
{
  Bool anyScanned;
  Bool scanAllObjects;
  Res res;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);

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
    res = awlSegScanSinglePass(&anyScanned, ss, seg, scanAllObjects);
    if (res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  /* we are done if we scanned all the objects or if we did a pass */
  /* and didn't scan any objects (since then, no new object can have */
  /* gotten fixed) */
  } while(!scanAllObjects && anyScanned);

  *totalReturn = scanAllObjects;
  AWLNoteScan(seg, ss);
  return ResOK;
}


/* awlSegFix -- Fix method for AWL segments */

static Res awlSegFix(Seg seg, ScanState ss, Ref *refIO)
{
  AWLSeg awlseg = MustBeA_CRITICAL(AWLSeg, seg);
  Pool pool = SegPool(seg);
  Ref clientRef;
  Addr base;
  Index i;

  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER_CRITICAL(refIO != NULL);

  clientRef = *refIO;

  base = AddrSub((Addr)clientRef, pool->format->headerSize);

  /* Not a real reference if out of bounds. This can happen if an
     ambiguous reference is closer to the base of the segment than the
     header size. */
  if (base < SegBase(seg)) {
    AVER(ss->rank == RankAMBIG);
    return ResOK;
  }

  /* Not a real reference if unaligned. */
  if (!AddrIsAligned(base, PoolAlignment(pool))) {
    AVER(ss->rank == RankAMBIG);
    return ResOK;
  }

  i = PoolIndexOfAddr(SegBase(seg), pool, base);

  /* Not a real reference if unallocated. */
  if (!BTGet(awlseg->alloc, i)) {
    AVER(ss->rank == RankAMBIG);
    return ResOK;
  }

  if (!BTGet(awlseg->mark, i)) {
    ss->wasMarked = FALSE; /* <design/fix/#was-marked.not> */
    if (ss->rank == RankWEAK) {
      *refIO = (Ref)0;
    } else {
      BTSet(awlseg->mark, i);
      SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
    }
  }

  return ResOK;
}


/* awlSegReclaim -- reclaim dead objects in an AWL segment */

static void awlSegReclaim(Seg seg, Trace trace)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Pool pool = SegPool(seg);
  PoolGen pgen = PoolSegPoolGen(pool, seg);
  Addr base = SegBase(seg);
  Buffer buffer;
  Bool hasBuffer = SegBuffer(&buffer, seg);
  Format format = pool->format;
  Count reclaimedGrains = (Count)0;
  Count preservedInPlaceCount = (Count)0;
  Size preservedInPlaceSize = (Size)0;
  Index i;

  AVERT(Trace, trace);

  i = 0;
  while(i < awlseg->grains) {
    Addr p, q;
    Index j;

    if(!BTGet(awlseg->alloc, i)) {
      ++i;
      continue;
    }
    p = PoolAddrOfIndex(base, pool, i);
    if (hasBuffer
        && p == BufferScanLimit(buffer)
        && BufferScanLimit(buffer) != BufferLimit(buffer))
    {
      i = PoolIndexOfAddr(base, pool, BufferLimit(buffer));
      continue;
    }
    q = format->skip(AddrAdd(p, format->headerSize));
    q = AddrSub(q, format->headerSize);
    AVER(AddrIsAligned(q, PoolAlignment(pool)));
    j = PoolIndexOfAddr(base, pool, q);
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
  PoolGenAccountForReclaim(pgen, PoolGrainsSize(pool, reclaimedGrains), FALSE);

  STATISTIC(trace->reclaimSize += PoolGrainsSize(pool, reclaimedGrains));
  STATISTIC(trace->preservedInPlaceCount += preservedInPlaceCount);
  GenDescSurvived(pgen->gen, trace, 0, preservedInPlaceSize);
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));

  if (awlseg->freeGrains == awlseg->grains && !hasBuffer) {
    /* No survivors */
    AVER(awlseg->bufferedGrains == 0);
    PoolGenFree(pgen, seg,
                PoolGrainsSize(pool, awlseg->freeGrains),
                PoolGrainsSize(pool, awlseg->oldGrains),
                PoolGrainsSize(pool, awlseg->newGrains),
                FALSE);
  }
}


/* awlSegAccess -- handle a barrier hit */

static Res awlSegAccess(Seg seg, Arena arena, Addr addr,
                        AccessSet mode, MutatorContext context)
{
  AWL awl;
  Res res;

  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVERT(AccessSet, mode);
  AVERT(MutatorContext, context);

  awl = MustBeA(AWLPool, SegPool(seg));

  /* Attempt scanning a single reference if permitted */
  if(awlSegCanTrySingleAccess(arena, seg, addr)) {
    res = SegSingleAccess(seg, arena, addr, mode, context);
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
  res = SegWholeAccess(seg, arena, addr, mode, context);
  if(ResOK == res) {
    AWLNoteSegAccess(awl, seg, addr);
  }

  return res;
}


/* awlSegWalk -- walk all objects */

static void awlSegWalk(Seg seg, Format format, FormattedObjectsVisitor f,
                       void *p, size_t s)
{
  AWLSeg awlseg = MustBeA(AWLSeg, seg);
  Pool pool = SegPool(seg);
  Addr object, base, limit;

  AVERT(Format, format);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  base = SegBase(seg);
  object = base;
  limit = SegLimit(seg);

  while(object < limit) {
    /* object is a slight misnomer because it might point to a */
    /* free grain */
    Addr next;
    Index i;
    Buffer buffer;

    if (SegBuffer(&buffer, seg)) {
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
    i = PoolIndexOfAddr(base, pool, object);
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


/* AWLTotalSize -- total memory allocated from the arena */
/* TODO: This code is repeated in AMS */

static Size AWLTotalSize(Pool pool)
{
  AWL awl = MustBeA(AWLPool, pool);
  return awl->pgen->totalSize;
}


/* AWLFreeSize -- free memory (unused by client program) */
/* TODO: This code is repeated in AMS */

static Size AWLFreeSize(Pool pool)
{
  AWL awl = MustBeA(AWLPool, pool);
  return awl->pgen->freeSize;
}


/* AWLPoolClass -- the class definition */

DEFINE_CLASS(Pool, AWLPool, klass)
{
  INHERIT_CLASS(klass, AWLPool, AbstractCollectPool);
  klass->instClassStruct.finish = AWLFinish;
  klass->size = sizeof(AWLPoolStruct);
  klass->varargs = AWLVarargs;
  klass->init = AWLInit;
  klass->bufferClass = RankBufClassGet;
  klass->bufferFill = awlBufferFill;
  klass->segPoolGen = awlSegPoolGen;
  klass->totalSize = AWLTotalSize;
  klass->freeSize = AWLFreeSize;
  AVERT(PoolClass, klass);
}


mps_pool_class_t mps_class_awl(void)
{
  return (mps_pool_class_t)CLASS(AWLPool);
}


/* AWLCheck -- check an AWL pool */

ATTRIBUTE_UNUSED
static Bool AWLCheck(AWL awl)
{
  CHECKS(AWL, awl);
  CHECKC(AWLPool, awl);
  CHECKD(Pool, CouldBeA(Pool, awl));
  if (awl->pgen != NULL)
    CHECKD(PoolGen, awl->pgen);
  /* Nothing to check about succAccesses. */
  CHECKL(FUNCHECK(awl->findDependent));
  /* Don't bother to check stats. */
  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

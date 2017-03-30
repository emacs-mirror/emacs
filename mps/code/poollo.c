/* poollo.c: LEAF POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * .design: See <design/poollo/>.  This is a leaf pool class.
 */

#include "mpsclo.h"
#include "mpm.h"
#include "mps.h"

SRCID(poollo, "$Id$");


/* LOStruct -- leaf object pool instance structure */

#define LOSig           ((Sig)0x51970B07) /* SIGnature LO POoL */

typedef struct LOStruct *LO;

typedef struct LOStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  Shift alignShift;             /* log_2 of pool alignment */
  PoolGenStruct pgenStruct;     /* generation representing the pool */
  PoolGen pgen;                 /* NULL or pointer to pgenStruct */
  Sig sig;
} LOStruct;

#define LOGrainsSize(lo, grains) ((grains) << (lo)->alignShift)

typedef LO LOPool;
#define LOPoolCheck LOCheck
DECLARE_CLASS(Pool, LOPool, AbstractSegBufPool);
DECLARE_CLASS(Seg, LOSeg, GCSeg);


/* forward declaration */
static Bool LOCheck(LO lo);
static Res loSegFix(Seg seg, ScanState ss, Ref *refIO);
static void loSegReclaim(Seg seg, Trace trace);


/* LOGSegStruct -- LO segment structure */

typedef struct LOSegStruct *LOSeg;

#define LOSegSig      ((Sig)0x519705E9) /* SIGnature LO SEG */

typedef struct LOSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  BT mark;                  /* mark bit table */
  BT alloc;                 /* alloc bit table */
  Count freeGrains;         /* free grains */
  Count bufferedGrains;     /* grains in buffers */
  Count newGrains;          /* grains allocated since last collection */
  Count oldGrains;          /* grains allocated prior to last collection */
  Sig sig;                  /* <code/misc.h#sig> */
} LOSegStruct;


/* forward decls */
static Res loSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args);
static void loSegFinish(Inst inst);
static Count loSegGrains(LOSeg loseg);
static Res loSegWhiten(Seg seg, Trace trace);


/* LOSegClass -- Class definition for LO segments */

DEFINE_CLASS(Seg, LOSeg, klass)
{
  INHERIT_CLASS(klass, LOSeg, GCSeg);
  SegClassMixInNoSplitMerge(klass);
  klass->instClassStruct.finish = loSegFinish;
  klass->size = sizeof(LOSegStruct);
  klass->init = loSegInit;
  klass->whiten = loSegWhiten;
  klass->fix = loSegFix;
  klass->fixEmergency = loSegFix;
  klass->reclaim = loSegReclaim;
}


/* LOSegCheck -- check an LO segment */

ATTRIBUTE_UNUSED
static Bool LOSegCheck(LOSeg loseg)
{
  Seg seg = MustBeA(Seg, loseg);
  LO lo = MustBeA(LOPool, SegPool(seg));
  CHECKS(LOSeg, loseg);
  CHECKD(GCSeg, &loseg->gcSegStruct);
  CHECKL(loseg->mark != NULL);
  CHECKL(loseg->alloc != NULL);
  /* Could check exactly how many bits are set in the alloc table. */
  CHECKL(loseg->freeGrains + loseg->bufferedGrains + loseg->newGrains
         + loseg->oldGrains
         == SegSize(seg) >> lo->alignShift);
  return TRUE;
}


/* loSegInit -- Init method for LO segments */

static Res loSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  LOSeg loseg;
  LO lo = MustBeA(LOPool, pool);
  Res res;
  Size tablebytes;      /* # bytes in each control array */
  Arena arena = PoolArena(pool);
  /* number of bits needed in each control array */
  Count grains;
  void *p;

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, LOSeg, init)(seg, pool, base, size, args);
  if(res != ResOK)
    goto failSuperInit;
  loseg = CouldBeA(LOSeg, seg);

  AVER(SegWhite(seg) == TraceSetEMPTY);

  grains = size >> lo->alignShift;
  tablebytes = BTSize(grains);
  res = ControlAlloc(&p, arena, tablebytes);
  if(res != ResOK)
    goto failMarkTable;
  loseg->mark = p;
  res = ControlAlloc(&p, arena, tablebytes);
  if(res != ResOK)
    goto failAllocTable;
  loseg->alloc = p;
  BTResRange(loseg->alloc, 0, grains);
  BTSetRange(loseg->mark, 0, grains);
  loseg->freeGrains = grains;
  loseg->bufferedGrains = (Count)0;
  loseg->newGrains = (Count)0;
  loseg->oldGrains = (Count)0;

  SetClassOfPoly(seg, CLASS(LOSeg));
  loseg->sig = LOSegSig;
  AVERC(LOSeg, loseg);

  return ResOK;

failAllocTable:
  ControlFree(arena, loseg->mark, tablebytes);
failMarkTable:
  NextMethod(Inst, LOSeg, finish)(MustBeA(Inst, seg));
failSuperInit:
  AVER(res != ResOK);
  return res;
}


/* loSegFinish -- Finish method for LO segments */

static void loSegFinish(Inst inst)
{
  Seg seg = MustBeA(Seg, inst);
  LOSeg loseg = MustBeA(LOSeg, seg);
  Pool pool = SegPool(seg);
  Arena arena = PoolArena(pool);
  Size tablesize;
  Count grains;

  loseg->sig = SigInvalid;

  grains = loSegGrains(loseg);
  tablesize = BTSize(grains);
  ControlFree(arena, loseg->alloc, tablesize);
  ControlFree(arena, loseg->mark, tablesize);

  NextMethod(Inst, LOSeg, finish)(inst);
}


ATTRIBUTE_UNUSED
static Count loSegGrains(LOSeg loseg)
{
  Seg seg = MustBeA(Seg, loseg);
  LO lo = MustBeA(LOPool, SegPool(seg));
  Size size = SegSize(seg);
  return size >> lo->alignShift;
}


/* Conversion between indexes and Addrs */
#define loIndexOfAddr(base, lo, p) \
  (AddrOffset((base), (p)) >> (lo)->alignShift)

#define loAddrOfIndex(base, lo, i) \
  (AddrAdd((base), LOGrainsSize((lo), (i))))


/* loSegFree -- mark block from baseIndex to limitIndex free */

static void loSegFree(LOSeg loseg, Index baseIndex, Index limitIndex)
{
  AVERT(LOSeg, loseg);
  AVER(baseIndex < limitIndex);
  AVER(limitIndex <= loSegGrains(loseg));

  AVER(BTIsSetRange(loseg->alloc, baseIndex, limitIndex));
  BTResRange(loseg->alloc, baseIndex, limitIndex);
  BTSetRange(loseg->mark, baseIndex, limitIndex);
}


/* Find a free block of size size in the segment.
 * Return pointer to base and limit of block (which may be
 * bigger than the requested size to accommodate buffering).
 */
static Bool loSegFindFree(Addr *bReturn, Addr *lReturn,
                          LOSeg loseg, Size size)
{
  Index baseIndex, limitIndex;
  Seg seg = MustBeA(Seg, loseg);
  Pool pool = SegPool(seg);
  LO lo = MustBeA(LOPool, pool);
  Count agrains;
  Count grains;
  Addr segBase;

  AVER(bReturn != NULL);
  AVER(lReturn != NULL);
  AVERT(LOSeg, loseg);

  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* agrains is the number of grains corresponding to the size */
  /* of the allocation request */
  agrains = size >> lo->alignShift;
  AVER(agrains >= 1);
  AVER(agrains <= loseg->freeGrains);
  AVER(size <= SegSize(seg));

  if (SegHasBuffer(seg))
    /* Don't bother trying to allocate from a buffered segment */
    return FALSE;

  grains = loSegGrains(loseg);
  if(!BTFindLongResRange(&baseIndex, &limitIndex, loseg->alloc,
                     0, grains, agrains)) {
    return FALSE;
  }

  /* check that BTFindLongResRange really did find enough space */
  AVER(baseIndex < limitIndex);
  AVER(LOGrainsSize(lo, limitIndex - baseIndex) >= size);
  segBase = SegBase(seg);
  *bReturn = loAddrOfIndex(segBase, lo, baseIndex);
  *lReturn = loAddrOfIndex(segBase, lo, limitIndex);

  return TRUE;
}


/* loSegCreate -- Creates a segment of size at least size.
 *
 * Segments will be multiples of ArenaGrainSize.
 */

static Res loSegCreate(LOSeg *loSegReturn, Pool pool, Size size)
{
  LO lo = MustBeA(LOPool, pool);
  Seg seg;
  Res res;

  AVER(loSegReturn != NULL);
  AVER(size > 0);

  res = PoolGenAlloc(&seg, lo->pgen, CLASS(LOSeg),
                     SizeArenaGrains(size, PoolArena(pool)),
                     argsNone);
  if (res != ResOK)
    return res;

  *loSegReturn = MustBeA(LOSeg, seg);
  return ResOK;
}


/* loSegReclaim -- reclaim white objects in an LO segment
 *
 * Could consider implementing this using Walk.
 */

static void loSegReclaim(Seg seg, Trace trace)
{
  Addr p, base, limit;
  Bool marked;
  Count reclaimedGrains = (Count)0;
  LOSeg loseg = MustBeA(LOSeg, seg);
  Pool pool = SegPool(seg);
  LO lo = MustBeA(LOPool, pool);
  Format format = NULL; /* supress "may be used uninitialized" warning */
  Count preservedInPlaceCount = (Count)0;
  Size preservedInPlaceSize = (Size)0;
  Bool b;

  AVERT(LOSeg, loseg);
  AVERT(Trace, trace);

  base = SegBase(seg);
  limit = SegLimit(seg);
  marked = FALSE;

  b = PoolFormat(&format, pool);
  AVER(b);

  /* i is the index of the current pointer,
   * p is the actual address that is being considered.
   * j and q act similarly for a pointer which is used to
   * point at the end of the current object.
   */
  p = base;
  while(p < limit) {
    Buffer buffer;
    Bool hasBuffer = SegBuffer(&buffer, seg);
    Addr q;
    Index i;

    if (hasBuffer) {
      marked = TRUE;
      if (p == BufferScanLimit(buffer)
          && BufferScanLimit(buffer) != BufferLimit(buffer)) {
        /* skip over buffered area */
        p = BufferLimit(buffer);
        continue;
      }
      /* since we skip over the buffered area we are always */
      /* either before the buffer, or after it, never in it */
      AVER(p < BufferGetInit(buffer) || BufferLimit(buffer) <= p);
    }
    i = loIndexOfAddr(base, lo, p);
    if(!BTGet(loseg->alloc, i)) {
      /* This grain is free */
      p = AddrAdd(p, pool->alignment);
      continue;
    }
    q = (*format->skip)(AddrAdd(p, format->headerSize));
    q = AddrSub(q, format->headerSize);
    if(BTGet(loseg->mark, i)) {
      marked = TRUE;
      ++preservedInPlaceCount;
      preservedInPlaceSize += AddrOffset(p, q);
    } else {
      Index j = loIndexOfAddr(base, lo, q);
      /* This object is not marked, so free it */
      loSegFree(loseg, i, j);
      reclaimedGrains += j - i;
    }
    p = q;
  }
  AVER(p == limit);

  AVER(reclaimedGrains <= loSegGrains(loseg));
  AVER(loseg->oldGrains >= reclaimedGrains);
  loseg->oldGrains -= reclaimedGrains;
  loseg->freeGrains += reclaimedGrains;
  PoolGenAccountForReclaim(lo->pgen, LOGrainsSize(lo, reclaimedGrains), FALSE);

  STATISTIC(trace->reclaimSize += LOGrainsSize(lo, reclaimedGrains));
  STATISTIC(trace->preservedInPlaceCount += preservedInPlaceCount);
  GenDescSurvived(lo->pgen->gen, trace, 0, preservedInPlaceSize);
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));

  if (!marked) {
    AVER(loseg->bufferedGrains == 0);
    PoolGenFree(lo->pgen, seg,
                LOGrainsSize(lo, loseg->freeGrains),
                LOGrainsSize(lo, loseg->oldGrains),
                LOGrainsSize(lo, loseg->newGrains),
                FALSE);
  }
}

/* This walks over _all_ objects in the heap, whether they are */
/* black or white, they are still validly formatted as this is */
/* a leaf pool, so there can't be any dangling references */
static void LOWalk(Pool pool, Seg seg, FormattedObjectsVisitor f,
                   void *p, size_t s)
{
  Addr base;
  LO lo = MustBeA(LOPool, pool);
  LOSeg loseg = MustBeA(LOSeg, seg);
  Index i, grains;
  Format format = NULL; /* suppress "may be used uninitialized" warning */
  Bool b;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  b = PoolFormat(&format, pool);
  AVER(b);

  base = SegBase(seg);
  grains = loSegGrains(loseg);
  i = 0;

  while(i < grains) {
    /* object is a slight misnomer because it might point to a */
    /* free grain */
    Addr object = loAddrOfIndex(base, lo, i);
    Addr next;
    Index j;
    Buffer buffer;

    if (SegBuffer(&buffer, seg)) {
      if(object == BufferScanLimit(buffer) &&
         BufferScanLimit(buffer) != BufferLimit(buffer)) {
        /* skip over buffered area */
        object = BufferLimit(buffer);
        i = loIndexOfAddr(base, lo, object);
        continue;
      }
      /* since we skip over the buffered area we are always */
      /* either before the buffer, or after it, never in it */
      AVER(object < BufferGetInit(buffer) || BufferLimit(buffer) <= object);
    }
    if(!BTGet(loseg->alloc, i)) {
      /* This grain is free */
      ++i;
      continue;
    }
    object = AddrAdd(object, format->headerSize);
    next = (*format->skip)(object);
    next = AddrSub(next, format->headerSize);
    j = loIndexOfAddr(base, lo, next);
    AVER(i < j);
    (*f)(object, pool->format, pool, p, s);
    i = j;
  }
}


/* LOVarargs -- decode obsolete varargs */

static void LOVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_FORMAT;
  args[0].val.format = va_arg(varargs, Format);
  args[1].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}


/* LOInit -- initialize an LO pool */

static Res LOInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  LO lo;
  Res res;
  ArgStruct arg;
  Chain chain;
  unsigned gen = LO_GEN_DEFAULT;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);
  UNUSED(klass); /* used for debug pools only */

  res = NextMethod(Pool, LOPool, init)(pool, arena, klass, args);
  if (res != ResOK)
    goto failNextInit;
  lo = CouldBeA(LOPool, pool);

  /* Ensure a format was supplied in the argument list. */
  AVER(pool->format != NULL);

  if (ArgPick(&arg, args, MPS_KEY_CHAIN))
    chain = arg.val.chain;
  else {
    chain = ArenaGlobals(arena)->defaultChain;
    gen = 1; /* avoid the nursery of the default chain by default */
  }
  if (ArgPick(&arg, args, MPS_KEY_GEN))
    gen = arg.val.u;
  
  AVERT(Format, pool->format);
  AVER(FormatArena(pool->format) == arena);
  AVERT(Chain, chain);
  AVER(gen <= ChainGens(chain));
  AVER(chain->arena == arena);

  pool->alignment = pool->format->alignment;
  lo->alignShift = SizeLog2((Size)PoolAlignment(pool));

  lo->pgen = NULL;

  SetClassOfPoly(pool, CLASS(LOPool));
  lo->sig = LOSig;
  AVERC(LOPool, lo);
  
  res = PoolGenInit(&lo->pgenStruct, ChainGen(chain, gen), pool);
  if (res != ResOK)
    goto failGenInit;
  lo->pgen = &lo->pgenStruct;

  EVENT2(PoolInitLO, pool, pool->format);

  return ResOK;

failGenInit:
  NextMethod(Inst, LOPool, finish)(MustBeA(Inst, pool));
failNextInit:
  AVER(res != ResOK);
  return res;
}


/* LOFinish -- finish an LO pool */

static void LOFinish(Inst inst)
{
  Pool pool = MustBeA(AbstractPool, inst);
  LO lo = MustBeA(LOPool, pool);
  Ring node, nextNode;

  RING_FOR(node, &pool->segRing, nextNode) {
    Seg seg = SegOfPoolRing(node);
    LOSeg loseg = MustBeA(LOSeg, seg);
    AVER(!SegHasBuffer(seg));
    AVERT(LOSeg, loseg);
    AVER(loseg->bufferedGrains == 0);
    PoolGenFree(lo->pgen, seg,
                LOGrainsSize(lo, loseg->freeGrains),
                LOGrainsSize(lo, loseg->oldGrains),
                LOGrainsSize(lo, loseg->newGrains),
                FALSE);
  }
  PoolGenFinish(lo->pgen);

  lo->sig = SigInvalid;

  NextMethod(Inst, LOPool, finish)(inst);
}


static Res LOBufferFill(Addr *baseReturn, Addr *limitReturn,
                        Pool pool, Buffer buffer,
                        Size size)
{
  Res res;
  Ring node, nextNode;
  LO lo = MustBeA(LOPool, pool);
  LOSeg loseg;
  Addr base, limit;
  Seg seg;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(BufferRankSet(buffer) == RankSetEMPTY);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* Try to find a segment with enough space already. */
  RING_FOR(node, PoolSegRing(pool), nextNode) {
    seg = SegOfPoolRing(node);
    loseg = MustBeA(LOSeg, seg);
    AVERT(LOSeg, loseg);
    if(LOGrainsSize(lo, loseg->freeGrains) >= size
       && loSegFindFree(&base, &limit, loseg, size))
      goto found;
  }

  /* No segment had enough space, so make a new one. */
  res = loSegCreate(&loseg, pool, size);
  if(res != ResOK)
    return res;
  seg = MustBeA(Seg, loseg);
  base = SegBase(seg);
  limit = SegLimit(seg);

found:
  {
    Index baseIndex, limitIndex;
    Addr segBase;

    segBase = SegBase(seg);
    /* mark the newly buffered region as allocated */
    baseIndex = loIndexOfAddr(segBase, lo, base);
    limitIndex = loIndexOfAddr(segBase, lo, limit);
    AVER(BTIsResRange(loseg->alloc, baseIndex, limitIndex));
    AVER(BTIsSetRange(loseg->mark, baseIndex, limitIndex));
    BTSetRange(loseg->alloc, baseIndex, limitIndex);
    AVER(loseg->freeGrains >= limitIndex - baseIndex);
    loseg->freeGrains -= limitIndex - baseIndex;
    loseg->bufferedGrains += limitIndex - baseIndex;
  }

  PoolGenAccountForFill(lo->pgen, AddrOffset(base, limit));

  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;
}


/* Synchronise the buffer with the alloc Bit Table in the segment. */

static void LOBufferEmpty(Pool pool, Buffer buffer, Addr init, Addr limit)
{
  LO lo = MustBeA(LOPool, pool);
  Addr base, segBase;
  Seg seg;
  LOSeg loseg;
  Index initIndex, limitIndex;
  Count usedGrains, unusedGrains;

  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  seg = BufferSeg(buffer);
  AVERT(Seg, seg);
  AVER(init <= limit);

  loseg = MustBeA(LOSeg, seg);

  base = BufferBase(buffer);
  segBase = SegBase(seg);

  AVER(AddrIsAligned(base, PoolAlignment(pool)));
  AVER(segBase <= base);
  AVER(base < SegLimit(seg));
  AVER(segBase <= init);
  AVER(init <= SegLimit(seg));

  /* convert base, init, and limit, to quantum positions */
  initIndex = loIndexOfAddr(segBase, lo, init);
  limitIndex = loIndexOfAddr(segBase, lo, limit);

  AVER(initIndex <= limitIndex);
  if (initIndex < limitIndex)
    loSegFree(loseg, initIndex, limitIndex);

  unusedGrains = limitIndex - initIndex;
  AVER(loseg->bufferedGrains >= unusedGrains);
  usedGrains = loseg->bufferedGrains - unusedGrains;
  loseg->freeGrains += unusedGrains;
  loseg->bufferedGrains = 0;
  loseg->newGrains += usedGrains;
  PoolGenAccountForEmpty(lo->pgen, LOGrainsSize(lo, usedGrains),
                         LOGrainsSize(lo, unusedGrains), FALSE);
}


/* loSegWhiten -- whiten a segment */

static Res loSegWhiten(Seg seg, Trace trace)
{
  LOSeg loseg = MustBeA(LOSeg, seg);
  Pool pool = SegPool(seg);
  LO lo = MustBeA(LOPool, pool);
  Buffer buffer;
  Count grains, agedGrains, uncondemnedGrains;

  AVERT(Trace, trace);
  AVER(SegWhite(seg) == TraceSetEMPTY);

  grains = loSegGrains(loseg);

  /* Whiten allocated objects; leave free areas black. */
  if (SegBuffer(&buffer, seg)) {
    Addr base = SegBase(seg);
    Index scanLimitIndex = loIndexOfAddr(base, lo, BufferScanLimit(buffer));
    Index limitIndex = loIndexOfAddr(base, lo, BufferLimit(buffer));
    uncondemnedGrains = limitIndex - scanLimitIndex;
    if (0 < scanLimitIndex)
      BTCopyInvertRange(loseg->alloc, loseg->mark, 0, scanLimitIndex);
    if (limitIndex < grains)
      BTCopyInvertRange(loseg->alloc, loseg->mark, limitIndex, grains);
  } else {
    uncondemnedGrains = (Count)0;
    BTCopyInvertRange(loseg->alloc, loseg->mark, 0, grains);
  }

  /* The unused part of the buffer remains buffered: the rest becomes old. */
  AVER(loseg->bufferedGrains >= uncondemnedGrains);
  agedGrains = loseg->bufferedGrains - uncondemnedGrains;
  PoolGenAccountForAge(lo->pgen, LOGrainsSize(lo, agedGrains),
                       LOGrainsSize(lo, loseg->newGrains), FALSE);
  loseg->oldGrains += agedGrains + loseg->newGrains;
  loseg->bufferedGrains = uncondemnedGrains;
  loseg->newGrains = 0;

  if (loseg->oldGrains > 0) {
    GenDescCondemned(lo->pgen->gen, trace, LOGrainsSize(lo, loseg->oldGrains));
    SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
  }

  return ResOK;
}


static Res loSegFix(Seg seg, ScanState ss, Ref *refIO)
{
  LOSeg loseg = MustBeA_CRITICAL(LOSeg, seg);
  Pool pool = SegPool(seg);
  LO lo = MustBeA_CRITICAL(LOPool, pool);
  Ref clientRef;
  Addr base;

  AVERT_CRITICAL(ScanState, ss);
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER_CRITICAL(refIO != NULL);

  ss->wasMarked = TRUE;         /* <design/fix/#protocol.was-marked> */

  clientRef = *refIO;
  base = AddrSub((Addr)clientRef, pool->format->headerSize);
  /* can get an ambiguous reference to close to the base of the
   * segment, so when we subtract the header we are not in the
   * segment any longer.  This isn't a real reference,
   * so we can just skip it.  */
  if (base < SegBase(seg)) {
    return ResOK;
  }

  switch(ss->rank) {
  case RankAMBIG:
    if(!AddrIsAligned(base, PoolAlignment(pool))) {
      return ResOK;
    }
  /* fall through */

  case RankEXACT:
  case RankFINAL:
  case RankWEAK: {
    Size i = AddrOffset(SegBase(seg), base) >> lo->alignShift;

    if(!BTGet(loseg->mark, i)) {
      ss->wasMarked = FALSE;  /* <design/fix/#protocol.was-marked> */
      if(ss->rank == RankWEAK) {
        *refIO = (Addr)0;
      } else {
        BTSet(loseg->mark, i);
      }
    }
  } break;

  default:
    NOTREACHED;
    break;
  }

  return ResOK;
}


/* LOTotalSize -- total memory allocated from the arena */
/* TODO: This code is repeated in AMS */

static Size LOTotalSize(Pool pool)
{
  LO lo = MustBeA(LOPool, pool);
  return lo->pgen->totalSize;
}


/* LOFreeSize -- free memory (unused by client program) */
/* TODO: This code is repeated in AMS */

static Size LOFreeSize(Pool pool)
{
  LO lo = MustBeA(LOPool, pool);
  return lo->pgen->freeSize;
}


/* LOPoolClass -- the class definition */

DEFINE_CLASS(Pool, LOPool, klass)
{
  INHERIT_CLASS(klass, LOPool, AbstractSegBufPool);
  PoolClassMixInFormat(klass);
  PoolClassMixInCollect(klass);
  klass->instClassStruct.finish = LOFinish;
  klass->size = sizeof(LOStruct);
  klass->varargs = LOVarargs;
  klass->init = LOInit;
  klass->bufferFill = LOBufferFill;
  klass->bufferEmpty = LOBufferEmpty;
  klass->walk = LOWalk;
  klass->totalSize = LOTotalSize;
  klass->freeSize = LOFreeSize;
}


/* mps_class_lo -- the external interface to get the LO pool class */

mps_pool_class_t mps_class_lo(void)
{
  return (mps_pool_class_t)CLASS(LOPool);
}


/* LOCheck -- check an LO pool */

ATTRIBUTE_UNUSED
static Bool LOCheck(LO lo)
{
  CHECKS(LO, lo);
  CHECKC(LOPool, lo);
  CHECKD(Pool, &lo->poolStruct);
  CHECKC(LOPool, lo);
  CHECKL(ShiftCheck(lo->alignShift));
  CHECKL(LOGrainsSize(lo, (Count)1) == PoolAlignment(MustBeA(AbstractPool, lo)));
  if (lo->pgen != NULL) {
    CHECKL(lo->pgen == &lo->pgenStruct);
    CHECKD(PoolGen, lo->pgen);
  }
  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

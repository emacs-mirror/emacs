/* impl.c.poollo: LEAF POOL CLASS
 *
 * $HopeName: !poollo.c(trunk.16) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer
 *
 * DESIGN
 *
 * .design: see design.mps.poollo
 *
 * This is the implementation of the leaf pool class.
 */


#include "mpsclo.h"
#include "mpm.h"
#include "mps.h"

SRCID(poollo, "$HopeName: !poollo.c(trunk.16) $");


/* MACROS */

#define LOGen ((Serial)1)

#define PoolPoolLO(pool)        PARENT(LOStruct, poolStruct, pool)

#define ActionLO(action)        PARENT(LOStruct, actionStruct, action)


/* LOStruct -- leaf object pool instance structure */

#define LOSig           ((Sig)0x51970B07) /* SIGnature LO POoL */

typedef struct LOStruct *LO;

typedef struct LOStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  Shift alignShift;             /* log_2 of pool alignment */
  ActionStruct actionStruct;    /* action of collecting this pool */
  Size objectsSize;             /* total size of all objects */
  Size lastRememberedSize;      /* total object size at last collection */
  Serial gen;                   /* associated generation */
  Sig sig;                      /* impl.h.misc.sig */
} LOStruct;

static Bool LOCheck(LO lo);


static Pool (LOPool)(LO lo)
{
  AVERT(LO, lo);

  return &lo->poolStruct;
}


/* LOGSegStruct -- LO segment structure */

typedef struct LOSegStruct *LOSeg;

#define LOSegSig      ((Sig)0x519705E9) /* SIGnature LO SEG */

typedef struct LOSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  LO lo;                    /* owning LO */
  BT mark;                  /* mark bit table */
  BT alloc;                 /* alloc bit table */
  Count free;               /* number of free grains */
  Sig sig;                  /* impl.h.misc.sig */
} LOSegStruct;

#define SegLOSeg(seg)             ((LOSeg)(seg))
#define LOSegSeg(loseg)           ((Seg)(loseg))

extern SegClass EnsureLOSegClass(void);

static Bool LOSegCheck(LOSeg loseg)
{
  CHECKS(LOSeg, loseg);
  CHECKL(GCSegCheck(&loseg->gcSegStruct));
  CHECKU(LO, loseg->lo);
  CHECKL(loseg->mark != NULL);
  CHECKL(loseg->alloc != NULL);
  CHECKL(loseg->free <= /* Could check exactly */
         SegSize(LOSegSeg(loseg)) >> loseg->lo->alignShift);
  return TRUE;
}


/* loSegInit -- Init method for LO segments */

static Res loSegInit(Seg seg, Pool pool, Addr base, Size size,
                     Bool reservoirPermit, va_list args)
{
  SegClass super;
  LOSeg loseg;
  LO lo;
  Res res;
  Size tablebytes;      /* # bytes in each control array */
  Arena arena;
  /* number of bits needed in each control array */
  unsigned long bits;
  void *p;

  AVERT(Seg, seg);
  loseg = SegLOSeg(seg);
  AVERT(Pool, pool);
  arena = PoolArena(pool);
  /* no useful checks for base and size */
  AVER(BoolCheck(reservoirPermit));
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(LOSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if(res != ResOK)
    return res;

  AVER(SegWhite(seg) == TraceSetEMPTY);

  bits = size >> lo->alignShift;
  tablebytes = BTSize(bits);
  res = ControlAlloc(&p, arena, tablebytes, reservoirPermit);
  if(res != ResOK)
    goto failMarkTable;
  loseg->mark = p;
  res = ControlAlloc(&p, arena, tablebytes, reservoirPermit);
  if(res != ResOK)
    goto failAllocTable;
  loseg->alloc = p;
  BTResRange(loseg->alloc, 0, bits);
  BTSetRange(loseg->mark, 0, bits);
  loseg->lo = lo;
  loseg->free = bits;
  loseg->sig = LOSegSig;
  AVERT(LOSeg, loseg);
  return ResOK;

failAllocTable:
  ControlFree(arena, loseg->mark, tablebytes);
failMarkTable:
  super->finish(seg);
  return res;
}


/* loSegFinish -- Finish method for LO segments */

static void loSegFinish(Seg seg)
{
  LO lo;
  LOSeg loseg;
  SegClass super;
  Pool pool;
  Arena arena;
  Size tablesize;
  unsigned long bits;

  AVERT(Seg, seg);
  loseg = SegLOSeg(seg);
  AVERT(LOSeg, loseg);
  pool = SegPool(seg);
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);
  arena = PoolArena(pool);

  bits = SegSize(seg) >> lo->alignShift;
  tablesize = BTSize(bits);
  ControlFree(arena, (Addr)loseg->alloc, tablesize);
  ControlFree(arena, (Addr)loseg->mark, tablesize);
  loseg->sig = SigInvalid;

  /* finish the superclass fields last */
  super = SEG_SUPERCLASS(LOSegClass);
  super->finish(seg);
}


/* LOSegClass -- Class definition for LO segments */

DEFINE_SEG_CLASS(LOSegClass, class)
{
  INHERIT_CLASS(class, GCSegClass);
  SegClassMixInNoSplitMerge(class);
  class->name = "LOSEG";
  class->size = sizeof(LOSegStruct);
  class->init = loSegInit;
  class->finish = loSegFinish;
}


static Count loSegBits(LOSeg loseg)
{
  LO lo;
  Size size;

  AVERT(LOSeg, loseg);

  lo = loseg->lo;
  AVERT(LO, lo);
  size = SegSize(LOSegSeg(loseg));
  return size >> lo->alignShift;
}


/* Conversion between indexes and Addrs */
#define loIndexOfAddr(base, lo, p) \
  (AddrOffset((base), (p)) >> (lo)->alignShift)

#define loAddrOfIndex(base, lo, i) \
  (AddrAdd((base), (i) << (lo)->alignShift))


static void loSegFree(LOSeg loseg, Index baseIndex, Index limitIndex)
{
  Count bits;
  Size size;

  AVERT(LOSeg, loseg);
  AVER(baseIndex < limitIndex);

  bits = loSegBits(loseg);
  AVER(limitIndex <= bits);

  AVER(BTIsSetRange(loseg->alloc, baseIndex, limitIndex));
  BTResRange(loseg->alloc, baseIndex, limitIndex);
  BTSetRange(loseg->mark, baseIndex, limitIndex);
  loseg->free += limitIndex - baseIndex;

  size = (limitIndex - baseIndex) << loseg->lo->alignShift;
  AVER(size <= loseg->lo->objectsSize);
  loseg->lo->objectsSize -= size;
}


/* Find a free block of size size in the segment.
 * Return pointer to base and limit of block (which may be
 * bigger than the requested size to accommodate buffering).
 */
static Bool loSegFindFree(Addr *bReturn, Addr *lReturn,
                          LOSeg loseg, Size size)
{
  Index baseIndex, limitIndex;
  LO lo;
  Seg seg;
  Arena arena;
  Count agrains;
  unsigned long tablesize;
  Addr segBase;

  AVER(bReturn != NULL);
  AVER(lReturn != NULL);
  AVERT(LOSeg, loseg);

  lo = loseg->lo;
  seg = LOSegSeg(loseg);
  AVER(SizeIsAligned(size, LOPool(lo)->alignment));
  arena = PoolArena(LOPool(lo));

  /* agrains is the number of grains corresponding to the size */
  /* of the allocation request */
  agrains = size >> lo->alignShift;
  AVER(agrains >= 1);
  AVER(agrains <= loseg->free);
  AVER(size <= SegSize(seg));

  if(SegBuffer(seg) != NULL) {
    /* Don't bother trying to allocate from a buffered segment */
    return FALSE;
  }

  tablesize = SegSize(seg) >> lo->alignShift;
  if(!BTFindLongResRange(&baseIndex, &limitIndex, loseg->alloc,
                     0, tablesize, agrains)) {
    return FALSE;
  }

  /* check that BTFindLongResRange really did find enough space */
  AVER(baseIndex < limitIndex);
  AVER((limitIndex-baseIndex) << lo->alignShift >= size);
  segBase = SegBase(seg);
  *bReturn = loAddrOfIndex(segBase, lo, baseIndex);
  *lReturn = loAddrOfIndex(segBase, lo, limitIndex);

  return TRUE;
}


/* loSegCreate -- Creates a segment of size at least size.
 *
 * Segments will be ArenaAlign aligned .
 */

static Res loSegCreate(LOSeg *loSegReturn, Pool pool, Size size,
                       Bool withReservoirPermit)
{
  LO lo;
  Seg seg;
  Res res;
  SegPrefStruct segPrefStruct;
  Serial gen;
  Arena arena;
  Size asize;           /* aligned size */

  AVER(loSegReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);

  arena = PoolArena(pool);
  asize = SizeAlignUp(size, ArenaAlign(arena));
  segPrefStruct = *SegPrefDefault();
  gen = lo->gen;
  SegPrefExpress(&segPrefStruct, SegPrefCollected, NULL);
  SegPrefExpress(&segPrefStruct, SegPrefGen, &gen);
  res = SegAlloc(&seg, EnsureLOSegClass(), &segPrefStruct,
                 asize, pool, withReservoirPermit);
  if (ResOK != res)
    return res;

  *loSegReturn = SegLOSeg(seg);
  return ResOK;
}


/* consider implementing Reclaim using Walk @@@@ */
static void loSegReclaim(LOSeg loseg, Trace trace)
{
  Addr p, base, limit;
  Bool marked;
  Count bytesReclaimed = (Count)0;
  Seg seg;
  LO lo;
  Format format;
  Count preservedInPlaceCount = (Count)0;
  Size preservedInPlaceSize = (Size)0;

  AVERT(LOSeg, loseg);
  AVERT(Trace, trace);

  seg = LOSegSeg(loseg);
  lo = loseg->lo;
  format = LOPool(lo)->format;
  AVERT(Format, format);
  base = SegBase(seg);
  limit = SegLimit(seg);
  marked = FALSE;

  /* i is the index of the current pointer,
   * p is the actual address that is being considered.
   * j and q act similarly for a pointer which is used to
   * point at the end of the current object.
   */
  p = base;
  while(p < limit) {
    Buffer buffer = SegBuffer(seg);
    Addr q;
    Index i;

    if(buffer != NULL) {
      marked = TRUE;
      if(p == BufferScanLimit(buffer) &&
         BufferScanLimit(buffer) != BufferLimit(buffer)) {
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
      p = AddrAdd(p, LOPool(lo)->alignment);
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
      bytesReclaimed += AddrOffset(p, q);
    }
    p = q;
  }
  AVER(p == limit);

  AVER(bytesReclaimed <= SegSize(seg));
  trace->reclaimSize += bytesReclaimed;
  trace->preservedInPlaceCount += preservedInPlaceCount;
  trace->preservedInPlaceSize += preservedInPlaceSize;

  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace->ti));

  if(!marked) {
    SegFree(seg);
  }
}

/* This walks over _all_ objects in the heap, whether they are */
/* black or white, they are still validly formatted as this is */
/* a leaf pool, so there can't be any dangling references */
static void LOWalk(Pool pool, Seg seg,
                   FormattedObjectsStepMethod f,
                   void *p, unsigned long s)
{
  Addr base;
  LO lo;
  LOSeg loseg;
  Format format;
  Index i, limit;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  lo = PoolPoolLO(pool);
  AVERT(LO, lo);
  format = pool->format;
  AVERT(Format, format);
  loseg = SegLOSeg(seg);
  AVERT(LOSeg, loseg);

  base = SegBase(seg);
  limit = SegSize(seg) >> lo->alignShift;
  i = 0;

  while(i < limit) {
    /* object is a slight misnomer because it might point to a */
    /* free grain */
    Addr object = loAddrOfIndex(base, lo, i);
    Addr next;
    Index j;

    if(SegBuffer(seg) != NULL) {
      Buffer buffer = SegBuffer(seg);
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


static Res LOInit(Pool pool, va_list arg)
{
  Format format;
  LO lo;
  Arena arena;

  AVERT(Pool, pool);

  arena = PoolArena(pool);

  format = va_arg(arg, Format);
  AVERT(Format, format);

  lo = PoolPoolLO(pool);

  pool->format = format;
  lo->poolStruct.alignment = format->alignment;
  lo->alignShift =
    SizeLog2((unsigned long)PoolAlignment(&lo->poolStruct));
  ActionInit(&lo->actionStruct, pool);
  lo->objectsSize = 0;
  lo->lastRememberedSize = 0;
  lo->gen = LOGen; /* may be modified in debugger */

  lo->sig = LOSig;

  AVERT(LO, lo);
  EVENT_PP(PoolInitLO, pool, format);
  return ResOK;
}

static void LOFinish(Pool pool)
{
  LO lo;
  Ring node, nextNode;

  AVERT(Pool, pool);
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);

  RING_FOR(node, &pool->segRing, nextNode) {
    Seg seg = SegOfPoolRing(node);
    LOSeg loseg = SegLOSeg(seg);
    AVERT(LOSeg, loseg);
    SegFree(seg);
  }

  ActionFinish(&lo->actionStruct);
  lo->sig = SigInvalid;
}


static Res LOBufferFill(Addr *baseReturn, Addr *limitReturn,
                        Pool pool, Buffer buffer,
                        Size size, Bool withReservoirPermit)
{
  Res res;
  Ring node, nextNode;
  LO lo;
  LOSeg loseg;
  Arena arena;
  Addr base, limit;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  lo = PARENT(LOStruct, poolStruct, pool);
  AVERT(LO, lo);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(BufferRankSet(buffer) == RankSetEMPTY);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));
  AVER(BoolCheck(withReservoirPermit));

  arena = PoolArena(pool);

  /* Try to find a segment with enough space already. */
  RING_FOR(node, &pool->segRing, nextNode) {
    Seg seg = SegOfPoolRing(node);
    loseg = SegLOSeg(seg);
    AVERT(LOSeg, loseg);
    if(loseg->free << lo->alignShift >= size &&
       loSegFindFree(&base, &limit, loseg, size)) {
      goto found;
    }
  }

  /* No segment had enough space, so make a new one. */
  res = loSegCreate(&loseg, pool, size, withReservoirPermit);
  if(res != ResOK) {
    goto failCreate;
  }
  base = SegBase(LOSegSeg(loseg));
  limit = SegLimit(LOSegSeg(loseg));

found:
  {
    Index baseIndex, limitIndex;
    Addr segBase;
    segBase = SegBase(LOSegSeg(loseg));
    /* mark the newly buffered region as allocated */
    baseIndex = loIndexOfAddr(segBase, lo, base);
    limitIndex = loIndexOfAddr(segBase, lo, limit);
    AVER(BTIsResRange(loseg->alloc, baseIndex, limitIndex));
    AVER(BTIsSetRange(loseg->mark, baseIndex, limitIndex));
    BTSetRange(loseg->alloc, baseIndex, limitIndex);
    loseg->free -= limitIndex - baseIndex;
  }

  /* update live object size */
  /* Check for overflow insanity */
  AVER(lo->objectsSize < lo->objectsSize + AddrOffset(base, limit));
  lo->objectsSize += AddrOffset(base, limit);

  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;

failCreate:
  return res;
}


/* Synchronise the buffer with the alloc Bit Table in the segment. */

static void LOBufferEmpty(Pool pool, Buffer buffer,
                          Addr init, Addr limit)
{
  LO lo;
  Addr base, segBase;
  Seg seg;
  LOSeg loseg;
  Index baseIndex, initIndex, limitIndex;
  Arena arena;

  AVERT(Pool, pool);
  lo = PARENT(LOStruct, poolStruct, pool);
  AVERT(LO, lo);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  seg = BufferSeg(buffer);
  AVER(SegCheck(seg));
  AVER(init <= limit);

  loseg = SegLOSeg(seg);
  AVERT(LOSeg, loseg);
  AVER(loseg->lo == lo);

  arena = PoolArena(pool);
  base = BufferBase(buffer);
  segBase = SegBase(seg);

  AVER(AddrIsAligned(base, PoolAlignment(pool)));
  AVER(segBase <= base && base < SegLimit(seg));
  AVER(segBase <= init && init <= SegLimit(seg));

  /* convert base, init, and limit, to quantum positions */
  baseIndex = loIndexOfAddr(segBase, lo, base);
  initIndex = loIndexOfAddr(segBase, lo, init);
  limitIndex = loIndexOfAddr(segBase, lo, limit);

  /* Record the unused portion at the end of the buffer */
  /* as being free. */
  AVER(baseIndex == limitIndex ||
       BTIsSetRange(loseg->alloc, baseIndex, limitIndex));
  if(initIndex != limitIndex) {
    loSegFree(loseg, initIndex, limitIndex);
  }
}


/* LOWhiten -- whiten a segment */

static Res LOWhiten(Pool pool, Trace trace, Seg seg)
{
  LO lo;
  unsigned long bits;

  AVERT(Pool, pool);
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);

  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(SegWhite(seg) == TraceSetEMPTY);

  if(SegBuffer(seg) == NULL) {
    LOSeg loseg = SegLOSeg(seg);
    AVERT(LOSeg, loseg);

    bits = SegSize(seg) >> lo->alignShift;
    /* allocated objects should be whitened, free areas should */
    /* be left "black" */
    BTCopyInvertRange(loseg->alloc, loseg->mark, 0, bits);
    /* @@@@ We could subtract all the free grains. */
    trace->condemned += SegSize(seg);
    SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace->ti));
  }

  return ResOK;
}


static Res LOFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  LO lo;
  LOSeg loseg;
  Ref clientRef;
  Addr base;

  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER_CRITICAL(refIO != NULL);
  lo = PARENT(LOStruct, poolStruct, pool);
  AVERT_CRITICAL(LO, lo);
  loseg = SegLOSeg(seg);
  AVERT_CRITICAL(LOSeg, loseg);

  ss->wasMarked = TRUE;         /* design.mps.fix.protocol.was-marked */

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
      ss->wasMarked = FALSE;  /* design.mps.fix.protocol.was-marked */
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


static void LOReclaim(Pool pool, Trace trace, Seg seg)
{
  LO lo;
  LOSeg loseg;

  AVERT(Pool, pool);
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);

  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(TraceSetIsMember(SegWhite(seg), trace->ti));

  loseg = SegLOSeg(seg);
  loSegReclaim(loseg, trace);

  lo->lastRememberedSize = lo->objectsSize;
}

static Res LOTraceBegin(Pool pool, Trace trace)
{
  LO lo;

  AVERT(Pool, pool);
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);
  AVERT(Trace, trace);

  lo->lastRememberedSize = lo->objectsSize;

  return ResOK;
}

int LORatioDenominator = 1;
int LORatioNumerator = 2;
unsigned long LOMinimumCollectableSize = 128*1024uL;

static double LOBenefit(Pool pool, Action action)
{
  LO lo;

  AVERT(Pool, pool);
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);
  AVERT(Action, action);
  AVER(lo == ActionLO(action));

  /* objects > k*lastRemembered, (k = p/q) */
  /* @@@@ ignoring overflow in multiplication */
  if(lo->objectsSize > LOMinimumCollectableSize &&
     lo->objectsSize * LORatioDenominator >
     lo->lastRememberedSize * LORatioNumerator)
     return 1.0;
  return 0.0;
}


/* LOPoolClass -- the class definition */

DEFINE_POOL_CLASS(LOPoolClass, this)
{
  INHERIT_CLASS(this, AbstractCollectPoolClass);
  PoolClassMixInFormat(this);
  this->name = "LO";
  this->size = sizeof(LOStruct);
  this->offset = offsetof(LOStruct, poolStruct);
  this->attr &= ~(AttrSCAN | AttrINCR_RB);
  this->init = LOInit;
  this->finish = LOFinish;
  this->bufferFill = LOBufferFill;
  this->bufferEmpty = LOBufferEmpty;
  this->traceBegin = LOTraceBegin;
  this->whiten = LOWhiten;
  this->grey = PoolNoGrey;
  this->blacken = PoolNoBlacken;
  this->scan = PoolNoScan;
  this->fix = LOFix;
  this->fixEmergency = LOFix;
  this->reclaim = LOReclaim;
  this->benefit = LOBenefit;
  this->walk = LOWalk;
}


mps_class_t mps_class_lo(void)
{
  return (mps_class_t)EnsureLOPoolClass();
}


static Bool LOCheck(LO lo)
{
  CHECKS(LO, lo);
  CHECKD(Pool, &lo->poolStruct);
  CHECKL(lo->poolStruct.class == EnsureLOPoolClass());
  CHECKL(ShiftCheck(lo->alignShift));
  CHECKL(1uL << lo->alignShift == PoolAlignment(&lo->poolStruct));
  CHECKD(Action, &lo->actionStruct);
  /* can't check lastRememberedSize or objectsSize */
  return TRUE;
}

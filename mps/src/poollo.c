/* impl.c.poollo: LEAF POOL CLASS
 *
 * $HopeName: MMsrc!poollo.c(trunk.10) $
 * Copyright (C) 1997,1998 Harlequin Group plc, all rights reserved.
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

SRCID(poollo, "$HopeName: MMsrc!poollo.c(trunk.10) $");


/* MACROS */

#define LOGen ((Serial)1)

#define PoolPoolLO(pool)        PARENT(LOStruct, poolStruct, pool)

#define loGroupBuffer(group)    SegBuffer((group)->seg)

#define ActionLO(action)        PARENT(LOStruct, actionStruct, action)


/* LOStruct -- leaf object pool instance structure */

#define LOSig           ((Sig)0x51970B07) /* SIGnature LO POoL */

typedef struct LOStruct *LO;

typedef struct LOStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  RingStruct groupRing;         /* ring of groups */
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


/* LOGroupStruct -- group structure */

typedef struct LOGroupStruct *LOGroup;

#define LOGroupSig      ((Sig)0x51970960) /* SIGnature LO GROup */

typedef struct LOGroupStruct {
  Sig sig;                      /* impl.h.misc.sig */
  LO lo;                        /* owning LO */
  RingStruct loRing;            /* attachment to the LO structure */
  BT mark;                      /* mark bit table */
  BT alloc;                     /* alloc bit table */
  Seg seg;                      /* segment containing objects */
  Count free;                   /* number of free grains */
} LOGroupStruct;


static Bool LOGroupCheck(LOGroup group)
{
  CHECKS(LOGroup, group);
  CHECKU(LO, group->lo);
  CHECKL(RingCheck(&group->loRing));
  CHECKL(group->mark != NULL);
  CHECKL(group->alloc != NULL);
  CHECKL(SegCheck(group->seg));
  CHECKL(group->free <= /* Could check exactly */
         SegSize(group->seg) >> group->lo->alignShift);
  return TRUE;
}


static Count loGroupBits(LOGroup group)
{
  LO lo;
  Size size;

  AVERT(LOGroup, group);

  lo = group->lo;
  AVERT(LO, lo);
  size = SegSize(group->seg);
  return size >> lo->alignShift;
}



/* Conversion between indexes and Addrs */
#define loIndexOfAddr(base, lo, p) \
  (AddrOffset((base), (p)) >> (lo)->alignShift)

#define loAddrOfIndex(base, lo, i) \
  (AddrAdd((base), (i) << (lo)->alignShift))


static void loGroupFree(LOGroup group,
                        Index baseIndex, Index limitIndex)
{
  Count bits;
  Size size;

  AVERT(LOGroup, group);
  AVER(baseIndex < limitIndex);

  bits = loGroupBits(group);
  AVER(limitIndex <= bits);

  AVER(BTIsSetRange(group->alloc, baseIndex, limitIndex));
  BTResRange(group->alloc, baseIndex, limitIndex);
  BTSetRange(group->mark, baseIndex, limitIndex);
  group->free += limitIndex - baseIndex;

  size = (limitIndex - baseIndex) << group->lo->alignShift;
  AVER(size <= group->lo->objectsSize);
  group->lo->objectsSize -= size;
}


/* Find a free block of size size in the group.
 * Return pointer to base and limit of block (which may be
 * bigger than the requested size to accommodate buffering).
 */
static Bool loGroupFindFree(Addr *bReturn, Addr *lReturn,
                         LOGroup group, Size size)
{
  Index baseIndex, limitIndex;
  LO lo;
  Arena arena;
  Count agrains;
  unsigned long tablesize;
  Addr segBase;

  AVER(bReturn != NULL);
  AVER(lReturn != NULL);
  AVERT(LOGroup, group);

  lo = group->lo;
  AVER(SizeIsAligned(size, LOPool(lo)->alignment));
  arena = PoolArena(LOPool(lo));

  /* agrains is the number of grains corresponding to the size */
  /* of the allocation request */
  agrains = size >> lo->alignShift;
  AVER(agrains >= 1);
  AVER(agrains <= group->free);
  AVER(size <= SegSize(group->seg));

  if(loGroupBuffer(group) != NULL) {
    /* Don't bother trying to allocate from a buffered group */
    return FALSE;
  }

  tablesize = SegSize(group->seg) >> lo->alignShift;
  if(!BTFindLongResRange(&baseIndex, &limitIndex, group->alloc,
                     0, tablesize, agrains)) {
    return FALSE;
  }

  /* check that BTFindLongResRange really did find enough space */
  AVER(baseIndex < limitIndex);
  AVER((limitIndex-baseIndex) << lo->alignShift >= size);
  segBase = SegBase(group->seg);
  *bReturn = loAddrOfIndex(segBase, lo, baseIndex);
  *lReturn = loAddrOfIndex(segBase, lo, limitIndex);

  return TRUE;
}


/* Creates a group of size at least size.
 * Groups will be ArenaAlign aligned */
static Res loGroupCreate(LOGroup *groupReturn, Pool pool, Size size,
                         Bool withReservoirPermit)
{
  LO lo;
  LOGroup group;
  SegPrefStruct segPrefStruct;
  Serial gen;
  Res res;
  Seg seg;
  Size asize;           /* aligned size */
  Size tablebytes;      /* # bytes in each control array */
  Arena arena;
  /* number of bits needed in each control array */
  unsigned long bits; 
  void *p;

  AVER(groupReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  lo = PARENT(LOStruct, poolStruct, pool);
  AVERT(LO, lo);

  arena = PoolArena(pool);

  asize = SizeAlignUp(size, ArenaAlign(arena));
  
  res = ArenaAlloc(&p, arena, (Size)sizeof(LOGroupStruct));
  if(res != ResOK)
    goto failGroup;
  group = (LOGroup)p;

  segPrefStruct = *SegPrefDefault();
  gen = lo->gen;
  SegPrefExpress(&segPrefStruct, SegPrefCollected, NULL);
  SegPrefExpress(&segPrefStruct, SegPrefGen, &gen);
  res = SegAlloc(&seg, &segPrefStruct, asize, pool, withReservoirPermit);
  if(res != ResOK)
    goto failSeg;

  group->seg = seg;
  SegSetP(seg, (void *)group);

  bits = asize >> lo->alignShift;
  tablebytes = BTSize(bits);
  res = ArenaAlloc(&p, arena, tablebytes);
  if(res != ResOK)
    goto failMarkTable;
  group->mark = p;
  res = ArenaAlloc(&p, arena, tablebytes);
  if(res != ResOK)
    goto failAllocTable;
  group->alloc = p;
  BTResRange(group->alloc, 0, bits);
  BTSetRange(group->mark, 0, bits);

  AVER(SegWhite(seg) == TraceSetEMPTY);

  group->lo = lo;
  RingInit(&group->loRing);
  RingAppend(&lo->groupRing, &group->loRing);

  group->free = bits;

  group->sig = LOGroupSig;

  AVERT(LOGroup, group);

  *groupReturn = group;
  return ResOK;

failAllocTable:
  ArenaFree(arena, group->mark, tablebytes);
failMarkTable:
  SegFree(seg);
failSeg:
  ArenaFree(arena, group, (Size)sizeof(LOGroupStruct));
failGroup:
  return res;
}


static void loGroupDestroy(LOGroup group)
{
  LO lo;
  Size tablesize;
  Arena arena;
  unsigned long bits;

  AVERT(LOGroup, group);

  lo = group->lo;
  arena = PoolArena(LOPool(lo));

  bits = SegSize(group->seg) >> lo->alignShift;
  tablesize = BTSize(bits);

  group->sig = SigInvalid;
  SegFree(group->seg);
  ArenaFree(arena, (Addr)group->alloc, tablesize);
  ArenaFree(arena, (Addr)group->mark, tablesize);
  RingRemove(&group->loRing);
  ArenaFree(arena, group, (Size)sizeof(LOGroupStruct));
}


/* consider implementing Reclaim using Walk @@@@ */
static void loGroupReclaim(LOGroup group, Trace trace)
{
  Addr p, base, limit;
  Bool marked;
  Count bytesReclaimed = (Count)0;
  LO lo;
  Count preservedInPlaceCount = (Count)0;
  Size preservedInPlaceSize = (Size)0;

  AVERT(LOGroup, group);
  AVERT(Trace, trace);

  lo = group->lo;
  base = SegBase(group->seg);
  limit = SegLimit(group->seg);
  marked = FALSE;

  /* i is the index of the current pointer,
   * p is the actual address that is being considered.
   * j and q act similarly for a pointer which is used to
   * point at the end of the current object.
   */
  p = base;
  while(p < limit) {
    Buffer buffer = loGroupBuffer(group);
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
    if(!BTGet(group->alloc, i)) {
      /* This grain is free */
      p = AddrAdd(p, LOPool(lo)->alignment);
      continue;
    }
    q = (*LOPool(lo)->format->skip)(p);
    if(BTGet(group->mark, i)) {
      marked = TRUE;
      ++preservedInPlaceCount;
      preservedInPlaceSize += AddrOffset(p, q);
    } else {
      Index j = loIndexOfAddr(base, lo, q);
      /* This object is not marked, so free it */
      loGroupFree(group, i, j);
      bytesReclaimed += AddrOffset(p, q);
    }
    p = q;
  }
  AVER(p == limit);

  AVER(bytesReclaimed <= SegSize(group->seg));
  trace->reclaimSize += bytesReclaimed;
  trace->preservedInPlaceCount += preservedInPlaceCount;
  trace->preservedInPlaceSize += preservedInPlaceSize;

  SegSetWhite(group->seg, TraceSetDel(SegWhite(group->seg), trace->ti));

  if(!marked) {
    loGroupDestroy(group);
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
  LOGroup group;
  Index i, limit;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  lo = PoolPoolLO(pool);
  AVERT(LO, lo);
  group = (LOGroup)SegP(seg);
  LOGroupCheck(group);

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
    if(!BTGet(group->alloc, i)) {
      /* This grain is free */
      ++i;
      continue;
    }
    next = (*pool->format->skip)(object);
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
  
  RingInit(&lo->groupRing);
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

  return ResOK;
}

static void LOFinish(Pool pool)
{
  LO lo;
  Ring node, nextNode;
  
  AVERT(Pool, pool);
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);

  RING_FOR(node, &lo->groupRing, nextNode) {
    LOGroup group = RING_ELT(LOGroup, loRing, node);
    loGroupDestroy(group);
  }

  RingFinish(&lo->groupRing);
  ActionFinish(&lo->actionStruct);
  lo->sig = SigInvalid;
}


static Res LOBufferFill(Seg *segReturn, Addr *baseReturn, 
                        Addr *limitReturn, Pool pool, Buffer buffer, 
                        Size size, Bool withReservoirPermit)
{
  Res res;
  Ring node, nextNode;
  LO lo;
  LOGroup group;
  Arena arena;
  Addr base, limit;

  AVER(segReturn != NULL);
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

  /* Try to find a group with enough space already. */
  RING_FOR(node, &lo->groupRing, nextNode) {
    group = RING_ELT(LOGroup, loRing, node);
    if(group->free << lo->alignShift >= size &&
       loGroupFindFree(&base, &limit, group, size)) {
      goto found;
    }
  }

  /* No group had enough space, so make a new one. */
  res = loGroupCreate(&group, pool, size, withReservoirPermit);
  if(res != ResOK) {
    goto failGroup;
  }
  base = SegBase(group->seg);
  limit = SegLimit(group->seg);

found:
  {
    Index baseIndex, limitIndex;
    Addr segBase;
    segBase = SegBase(group->seg);
    /* mark the newly buffered region as allocated */
    baseIndex = loIndexOfAddr(segBase, lo, base);
    limitIndex = loIndexOfAddr(segBase, lo, limit);
    AVER(BTIsResRange(group->alloc, baseIndex, limitIndex));
    AVER(BTIsSetRange(group->mark, baseIndex, limitIndex));
    BTSetRange(group->alloc, baseIndex, limitIndex);
    group->free -= limitIndex - baseIndex;
  }

  /* update live object size */
  /* Check for overflow insanity */
  AVER(lo->objectsSize < lo->objectsSize + AddrOffset(base, limit));
  lo->objectsSize += AddrOffset(base, limit);

  *segReturn = group->seg;
  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;

failGroup:
  return res;
}


/* Synchronise the buffer with the alloc Bit Table in the group. */

static void LOBufferEmpty(Pool pool, Buffer buffer, Seg seg)
{
  LO lo;
  Addr base, alloc, limit, segBase;
  LOGroup group;
  Index baseIndex, allocIndex, limitIndex;
  Arena arena;

  AVERT(Pool, pool);
  lo = PARENT(LOStruct, poolStruct, pool);
  AVERT(LO, lo);
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));
  AVER(SegCheck(seg));
  
  group = (LOGroup)SegP(seg);

  AVERT(LOGroup, group);
  AVER(group->seg == seg);
  AVER(group->lo == lo);

  arena = PoolArena(pool);
  base = BufferBase(buffer);
  alloc = BufferAlloc(buffer);
  limit = BufferLimit(buffer);
  segBase = SegBase(seg);

  AVER(AddrIsAligned(base, PoolAlignment(pool)));
  AVER(segBase <= base && base < SegLimit(seg));
  AVER(segBase <= alloc && alloc <= SegLimit(seg));

  /* convert base, alloc, and limit, to quantum positions */
  baseIndex = loIndexOfAddr(segBase, lo, base);
  allocIndex = loIndexOfAddr(segBase, lo, alloc);
  limitIndex = loIndexOfAddr(segBase, lo, limit);

  /* Record the unused portion at the end of the buffer */
  /* as being free. */
  AVER(baseIndex == limitIndex ||
       BTIsSetRange(group->alloc, baseIndex, limitIndex));
  if(allocIndex != limitIndex) {
    loGroupFree(group, allocIndex, limitIndex);
  }
}


/* LOWhiten -- whiten a segment */

static Res LOWhiten(Pool pool, Trace trace, Seg seg)
{
  LO lo;
  LOGroup group;
  unsigned long bits;
  
  AVERT(Pool, pool);
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);

  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(SegWhite(seg) == TraceSetEMPTY);

  if(SegBuffer(seg) == NULL) {
    group = (LOGroup)SegP(seg);
    AVERT(LOGroup, group);

    bits = SegSize(group->seg) >> lo->alignShift;
    /* allocated objects should be whitened, free areas should */
    /* be left "black" */
    BTCopyInvertRange(group->alloc, group->mark, 0, bits);
    /* @@@@ We could subtract all the free grains. */
    trace->condemned += SegSize(seg);
    SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace->ti));
  }

  return ResOK;
}


static Res LOFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  LO lo;
  LOGroup group;
  Ref ref;

  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER_CRITICAL(refIO != NULL);
  ref = *refIO;
  lo = PARENT(LOStruct, poolStruct, pool);
  AVERT_CRITICAL(LO, lo);
  group = (LOGroup)SegP(seg);
  AVERT_CRITICAL(LOGroup, group);
  AVER_CRITICAL(group->seg == seg);

  ss->wasMarked = TRUE;         /* design.mps.fix.protocol.was-marked */

  switch(ss->rank) {
  case RankAMBIG:
    if(!AddrIsAligned(ref, PoolAlignment(pool))) {
      return ResOK;
    }
  /* fall through */

  case RankEXACT: 
  case RankFINAL:
  case RankWEAK: {
    Size i = AddrOffset(SegBase(seg),
                        (Addr)ref) >> lo->alignShift;

    if(!BTGet(group->mark, i)) {
      ss->wasMarked = FALSE;  /* design.mps.fix.protocol.was-marked */
      if(ss->rank == RankWEAK) {
        *refIO = (Addr)0;
      } else {
        BTSet(group->mark, i);
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
  LOGroup group;

  AVERT(Pool, pool);
  lo = PoolPoolLO(pool);
  AVERT(LO, lo);

  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(TraceSetIsMember(SegWhite(seg), trace->ti));

  group = (LOGroup)SegP(seg);
  loGroupReclaim(group, trace);

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
  CHECKL(RingCheck(&lo->groupRing));
  CHECKL(ShiftCheck(lo->alignShift));
  CHECKL(1uL << lo->alignShift == PoolAlignment(&lo->poolStruct));
  CHECKD(Action, &lo->actionStruct);
  /* can't check lastRememberedSize or objectsSize */
  return TRUE;
}

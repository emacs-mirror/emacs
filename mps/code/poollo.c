/* impl.c.poollo: LEAF POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * .design: See design.mps.poollo.  This is a leaf pool class.
 */

#include "mpsclo.h"
#include "mpm.h"
#include "mps.h"

SRCID(poollo, "$Id$");


#define LOGen ((Serial)1)


/* LOStruct -- leaf object pool instance structure */

#define LOSig           ((Sig)0x51970B07) /* SIGnature LO POoL */

typedef struct LOStruct *LO;

typedef struct LOStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  Shift alignShift;             /* log_2 of pool alignment */
  Serial gen;                   /* generation for placement */
  Chain chain;                  /* chain used by this pool */
  PoolGenStruct pgen;           /* generation representing the pool */
  Sig sig;
} LOStruct;

#define PoolPoolLO(pool) PARENT(LOStruct, poolStruct, pool)
#define LOPool(lo) (&(lo)->poolStruct)


/* forward declaration */
static Bool LOCheck(LO lo);


/* LOGSegStruct -- LO segment structure */

typedef struct LOSegStruct *LOSeg;

#define LOSegSig      ((Sig)0x519705E9) /* SIGnature LO SEG */

typedef struct LOSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  LO lo;                    /* owning LO */
  BT mark;                  /* mark bit table */
  BT alloc;                 /* alloc bit table */
  Count free;               /* number of free grains */
  Count newAlloc;           /* number of grains allocated since last GC */
  Sig sig;                  /* impl.h.misc.sig */
} LOSegStruct;

#define SegLOSeg(seg)             ((LOSeg)(seg))
#define LOSegSeg(loseg)           ((Seg)(loseg))


/* forward decls */
static Res loSegInit(Seg seg, Pool pool, Addr base, Size size,
                     Bool reservoirPermit, va_list args);
static void loSegFinish(Seg seg);


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


/* LOSegCheck -- check an LO segment */

static Bool LOSegCheck(LOSeg loseg)
{
  CHECKS(LOSeg, loseg);
  CHECKL(GCSegCheck(&loseg->gcSegStruct));
  CHECKU(LO, loseg->lo);
  CHECKL(loseg->mark != NULL);
  CHECKL(loseg->alloc != NULL);
  /* Could check exactly how many bits are set in the alloc table. */
  CHECKL(loseg->free + loseg->newAlloc
         <= SegSize(LOSegSeg(loseg)) >> loseg->lo->alignShift);
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
  loseg->newAlloc = (Count)0;
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


/* loSegFree -- mark block from baseIndex to limitIndex free */

static void loSegFree(LOSeg loseg, Index baseIndex, Index limitIndex)
{
  AVERT(LOSeg, loseg);
  AVER(baseIndex < limitIndex);
  AVER(limitIndex <= loSegBits(loseg));

  AVER(BTIsSetRange(loseg->alloc, baseIndex, limitIndex));
  BTResRange(loseg->alloc, baseIndex, limitIndex);
  BTSetRange(loseg->mark, baseIndex, limitIndex);
  loseg->free += limitIndex - baseIndex;
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
  if (res != ResOK)
    return res;
  PoolGenUpdateZones(&lo->pgen, seg);

  *loSegReturn = SegLOSeg(seg);
  return ResOK;
}


/* loSegReclaim -- reclaim white objects in an LO segment
 *
 * Could consider implementing this using Walk.
 */

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
  base = SegBase(seg);
  limit = SegLimit(seg);
  marked = FALSE;

  format = LOPool(lo)->format;
  AVERT(Format, format);

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
  lo->pgen.totalSize -= bytesReclaimed;
  trace->preservedInPlaceCount += preservedInPlaceCount;
  trace->preservedInPlaceSize += preservedInPlaceSize;

  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));

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
  Index i, limit;
  Format format;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  lo = PoolPoolLO(pool);
  AVERT(LO, lo);
  loseg = SegLOSeg(seg);
  AVERT(LOSeg, loseg);

  format = pool->format;
  AVERT(Format, format);

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
    next = AddrSub(object, format->headerSize);
    j = loIndexOfAddr(base, lo, next);
    AVER(i < j);
    (*f)(object, pool->format, pool, p, s);
    i = j;
  }
}


/* LOInit -- initialize an LO pool */

static Res LOInit(Pool pool, va_list arg)
{
  Format format;
  LO lo;
  Arena arena;
  Res res;
  static GenParamStruct loGenParam = { 1024, 0.2 };

  AVERT(Pool, pool);

  arena = PoolArena(pool);

  format = va_arg(arg, Format);
  AVERT(Format, format);

  lo = PoolPoolLO(pool);

  pool->format = format;
  lo->poolStruct.alignment = format->alignment;
  lo->alignShift =
    SizeLog2((unsigned long)PoolAlignment(&lo->poolStruct));
  lo->gen = LOGen; /* may be modified in debugger */
  res = ChainCreate(&lo->chain, arena, 1, &loGenParam);
  if (res != ResOK)
    return res;
  /* .gen: This must be the nursery in the chain, because it's the only */
  /* generation.  lo->gen is just a hack for segment placement. */
  res = PoolGenInit(&lo->pgen, lo->chain, 0 /* .gen */, pool);
  if (res != ResOK)
    goto failGenInit;

  lo->sig = LOSig;
  AVERT(LO, lo);
  EVENT_PP(PoolInitLO, pool, format);
  return ResOK;

failGenInit:
  ChainDestroy(lo->chain);
  return res;
}


/* LOFinish -- finish an LO pool */

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
    UNUSED(loseg); /* impl.c.mpm.check.unused */
    SegFree(seg);
  }
  PoolGenFinish(&lo->pgen);
  ChainDestroy(lo->chain);

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
    if((loseg->free << lo->alignShift) >= size
       && loSegFindFree(&base, &limit, loseg, size))
      goto found;
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
    loseg->newAlloc += limitIndex - baseIndex;
  }

  lo->pgen.totalSize += AddrOffset(base, limit);
  lo->pgen.newSize += AddrOffset(base, limit);

  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;

failCreate:
  return res;
}


/* Synchronise the buffer with the alloc Bit Table in the segment. */

static void LOBufferEmpty(Pool pool, Buffer buffer, Addr init, Addr limit)
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
  AVERT(Seg, seg);
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
  AVER(baseIndex == limitIndex
       || BTIsSetRange(loseg->alloc, baseIndex, limitIndex));
  if(initIndex != limitIndex) {
    loSegFree(loseg, initIndex, limitIndex);
    lo->pgen.totalSize -= AddrOffset(init, limit);
    /* All of the buffer must be new, since buffered segs are not condemned. */
    AVER(loseg->newAlloc >= limitIndex - baseIndex);
    loseg->newAlloc -= limitIndex - initIndex;
    lo->pgen.newSize -= AddrOffset(init, limit);
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
    /* Allocated objects should be whitened, free areas should */
    /* be left "black". */
    BTCopyInvertRange(loseg->alloc, loseg->mark, 0, bits);
    /* @@@@ We could subtract all the free grains. */
    trace->condemned += SegSize(seg);
    lo->pgen.newSize -= loseg->newAlloc << lo->alignShift;
    loseg->newAlloc = (Count)0;
    SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
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
  AVER(TraceSetIsMember(SegWhite(seg), trace));

  loseg = SegLOSeg(seg);
  loSegReclaim(loseg, trace);
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
  this->whiten = LOWhiten;
  this->grey = PoolNoGrey;
  this->blacken = PoolNoBlacken;
  this->scan = PoolNoScan;
  this->fix = LOFix;
  this->fixEmergency = LOFix;
  this->reclaim = LOReclaim;
  this->walk = LOWalk;
}


/* mps_class_lo -- the external interface to get the LO pool class */

mps_class_t mps_class_lo(void)
{
  return (mps_class_t)EnsureLOPoolClass();
}


/* LOCheck -- check an LO pool */

static Bool LOCheck(LO lo)
{
  CHECKS(LO, lo);
  CHECKD(Pool, &lo->poolStruct);
  CHECKL(lo->poolStruct.class == EnsureLOPoolClass());
  CHECKL(ShiftCheck(lo->alignShift));
  CHECKL(1uL << lo->alignShift == PoolAlignment(&lo->poolStruct));
  CHECKD(Chain, lo->chain);
  CHECKD(PoolGen, &lo->pgen);
  return TRUE;
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

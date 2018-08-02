/* poollo.c: LEAF POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2018 Ravenbrook Limited.  See end of file for license.
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
  PoolGenStruct pgenStruct;     /* generation representing the pool */
  PoolGen pgen;                 /* NULL or pointer to pgenStruct */
  Sig sig;                      /* <code/misc.h#sig> */
} LOStruct;

typedef LO LOPool;
#define LOPoolCheck LOCheck
DECLARE_CLASS(Pool, LOPool, AbstractCollectPool);
DECLARE_CLASS(Seg, LOSeg, MutatorSeg);


/* forward declaration */
static Bool LOCheck(LO lo);


/* LOGSegStruct -- LO segment structure
 *
 * Colour is represented as follows:
 * Black: +alloc +mark
 * White: +alloc -mark
 * Grey: objects have no references so can't be grey
 * Free: -alloc ?mark
 */

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
static Bool loSegBufferFill(Addr *baseReturn, Addr *limitReturn,
                            Seg seg, Size size, RankSet rankSet);
static void loSegBufferEmpty(Seg seg, Buffer buffer);
static Res loSegWhiten(Seg seg, Trace trace);
static Res loSegFix(Seg seg, ScanState ss, Ref *refIO);
static void loSegReclaim(Seg seg, Trace trace);
static void loSegWalk(Seg seg, Format format, FormattedObjectsVisitor f,
                      void *p, size_t s);


/* LOSegClass -- Class definition for LO segments */

DEFINE_CLASS(Seg, LOSeg, klass)
{
  INHERIT_CLASS(klass, LOSeg, MutatorSeg);
  SegClassMixInNoSplitMerge(klass);
  klass->instClassStruct.finish = loSegFinish;
  klass->size = sizeof(LOSegStruct);
  klass->init = loSegInit;
  klass->bufferFill = loSegBufferFill;
  klass->bufferEmpty = loSegBufferEmpty;
  klass->whiten = loSegWhiten;
  klass->fix = loSegFix;
  klass->fixEmergency = loSegFix;
  klass->reclaim = loSegReclaim;
  klass->walk = loSegWalk;
  AVERT(SegClass, klass);
}


/* LOSegCheck -- check an LO segment */

ATTRIBUTE_UNUSED
static Bool LOSegCheck(LOSeg loseg)
{
  Seg seg = MustBeA(Seg, loseg);
  Pool pool = SegPool(seg);
  CHECKS(LOSeg, loseg);
  CHECKD(GCSeg, &loseg->gcSegStruct);
  CHECKL(loseg->mark != NULL);
  CHECKL(loseg->alloc != NULL);
  /* Could check exactly how many bits are set in the alloc table. */
  CHECKL(loseg->freeGrains + loseg->bufferedGrains + loseg->newGrains
         + loseg->oldGrains
         == PoolSizeGrains(pool, SegSize(seg)));
  return TRUE;
}


/* loSegInit -- Init method for LO segments */

static Res loSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  LOSeg loseg;
  Res res;
  Size tableSize;      /* # bytes in each control array */
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

  grains = PoolSizeGrains(pool, size);
  tableSize = BTSize(grains);
  res = ControlAlloc(&p, arena, 2 * tableSize);
  if(res != ResOK)
    goto failControlAlloc;
  loseg->mark = p;
  loseg->alloc = PointerAdd(p, tableSize);
  BTResRange(loseg->alloc, 0, grains);
  loseg->freeGrains = grains;
  loseg->bufferedGrains = (Count)0;
  loseg->newGrains = (Count)0;
  loseg->oldGrains = (Count)0;

  SetClassOfPoly(seg, CLASS(LOSeg));
  loseg->sig = LOSegSig;
  AVERC(LOSeg, loseg);

  return ResOK;

failControlAlloc:
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
  ControlFree(arena, loseg->mark, 2 * tablesize);

  NextMethod(Inst, LOSeg, finish)(inst);
}


ATTRIBUTE_UNUSED
static Count loSegGrains(LOSeg loseg)
{
  Seg seg = MustBeA(Seg, loseg);
  Pool pool = SegPool(seg);
  Size size = SegSize(seg);
  return PoolSizeGrains(pool, size);
}


/* loSegBufferFill -- try filling buffer from segment */

static Bool loSegBufferFill(Addr *baseReturn, Addr *limitReturn,
                            Seg seg, Size size, RankSet rankSet)
{
  Index baseIndex, limitIndex;
  LOSeg loseg = MustBeA_CRITICAL(LOSeg, seg);
  Pool pool = SegPool(seg);
  Count requestedGrains, segGrains, allocatedGrains;
  Addr segBase, base, limit;

  AVER_CRITICAL(baseReturn != NULL);
  AVER_CRITICAL(limitReturn != NULL);
  AVER_CRITICAL(SizeIsAligned(size, PoolAlignment(pool)));
  AVER_CRITICAL(size > 0);
  AVER_CRITICAL(rankSet == RankSetEMPTY);

  requestedGrains = PoolSizeGrains(pool, size);
  if (loseg->freeGrains < requestedGrains)
    /* Not enough space to satisfy the request. */
    return FALSE;

  if (SegHasBuffer(seg))
    /* Don't bother trying to allocate from a buffered segment */
    return FALSE;

  segGrains = PoolSizeGrains(pool, SegSize(seg));
  if (loseg->freeGrains == segGrains) {
    /* Whole segment is free: no need for a search. */
    baseIndex = 0;
    limitIndex = segGrains;
    goto found;
  }

  if (!BTFindLongResRange(&baseIndex, &limitIndex, loseg->alloc,
                          0, segGrains, requestedGrains))
    return FALSE;

found:
  AVER(baseIndex < limitIndex);
  allocatedGrains = limitIndex - baseIndex;
  AVER(requestedGrains <= allocatedGrains);
  AVER(BTIsResRange(loseg->alloc, baseIndex, limitIndex));
  /* Objects are allocated black. */
  /* TODO: This should depend on trace phase. */
  BTSetRange(loseg->alloc, baseIndex, limitIndex);
  BTSetRange(loseg->mark, baseIndex, limitIndex);
  AVER(loseg->freeGrains >= allocatedGrains);
  loseg->freeGrains -= allocatedGrains;
  loseg->bufferedGrains += allocatedGrains;

  segBase = SegBase(seg);
  base = PoolAddrOfIndex(segBase, pool, baseIndex);
  limit = PoolAddrOfIndex(segBase, pool, limitIndex);
  PoolGenAccountForFill(PoolSegPoolGen(pool, seg), AddrOffset(base, limit));

  *baseReturn = base;
  *limitReturn = limit;
  return TRUE;
}


/* loSegBufferEmpty -- empty buffer to segment */

static void loSegBufferEmpty(Seg seg, Buffer buffer)
{
  LOSeg loseg = MustBeA(LOSeg, seg);
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
    BTResRange(loseg->alloc, initIndex, limitIndex);

  unusedGrains = limitIndex - initIndex;
  AVER(unusedGrains <= loseg->bufferedGrains);
  usedGrains = loseg->bufferedGrains - unusedGrains;
  loseg->freeGrains += unusedGrains;
  loseg->bufferedGrains = 0;
  loseg->newGrains += usedGrains;

  PoolGenAccountForEmpty(PoolSegPoolGen(pool, seg),
                         PoolGrainsSize(pool, usedGrains),
                         PoolGrainsSize(pool, unusedGrains), FALSE);
}


/* loSegReclaim -- reclaim white objects in an LO segment
 *
 * Could consider implementing this using Walk.
 */

static void loSegReclaim(Seg seg, Trace trace)
{
  LOSeg loseg = MustBeA(LOSeg, seg);
  Pool pool = SegPool(seg);
  PoolGen pgen = PoolSegPoolGen(pool, seg);
  Addr p, base, limit;
  Buffer buffer;
  Bool hasBuffer = SegBuffer(&buffer, seg);
  Count reclaimedGrains = (Count)0;
  Format format = NULL; /* supress "may be used uninitialized" warning */
  Count preservedInPlaceCount = (Count)0;
  Size preservedInPlaceSize = (Size)0;
  Bool b;

  AVERT(Trace, trace);

  base = SegBase(seg);
  limit = SegLimit(seg);

  b = PoolFormat(&format, pool);
  AVER(b);

  /* i is the index of the current pointer,
   * p is the actual address that is being considered.
   * j and q act similarly for a pointer which is used to
   * point at the end of the current object.
   */
  p = base;
  while(p < limit) {
    Addr q;
    Index i;

    if (hasBuffer) {
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
    i = PoolIndexOfAddr(base, pool, p);
    if(!BTGet(loseg->alloc, i)) {
      /* This grain is free */
      p = AddrAdd(p, pool->alignment);
      continue;
    }
    q = (*format->skip)(AddrAdd(p, format->headerSize));
    q = AddrSub(q, format->headerSize);
    if(BTGet(loseg->mark, i)) {
      ++preservedInPlaceCount;
      preservedInPlaceSize += AddrOffset(p, q);
    } else {
      Index j = PoolIndexOfAddr(base, pool, q);
      /* This object is not marked, so free it */
      BTResRange(loseg->alloc, i, j);
      reclaimedGrains += j - i;
    }
    p = q;
  }
  AVER(p == limit);

  AVER(reclaimedGrains <= loSegGrains(loseg));
  AVER(loseg->oldGrains >= reclaimedGrains);
  loseg->oldGrains -= reclaimedGrains;
  loseg->freeGrains += reclaimedGrains;
  PoolGenAccountForReclaim(pgen, PoolGrainsSize(pool, reclaimedGrains), FALSE);

  STATISTIC(trace->reclaimSize += PoolGrainsSize(pool, reclaimedGrains));
  STATISTIC(trace->preservedInPlaceCount += preservedInPlaceCount);
  GenDescSurvived(pgen->gen, trace, 0, preservedInPlaceSize);
  SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace));

  if (loseg->freeGrains == PoolSizeGrains(pool, SegSize(seg)) && !hasBuffer) {
    AVER(loseg->bufferedGrains == 0);
    PoolGenFree(pgen, seg,
                PoolGrainsSize(pool, loseg->freeGrains),
                PoolGrainsSize(pool, loseg->oldGrains),
                PoolGrainsSize(pool, loseg->newGrains),
                FALSE);
  }
}

/* Walks over _all_ objects in the segnent: whether they are black or
 * white, they are still validly formatted as this is a leaf pool, so
 * there can't be any dangling references.
 */
static void loSegWalk(Seg seg, Format format, FormattedObjectsVisitor f,
                      void *p, size_t s)
{
  Addr base;
  LOSeg loseg = MustBeA(LOSeg, seg);
  Pool pool = SegPool(seg);
  Index i, grains;

  AVERT(Format, format);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  base = SegBase(seg);
  grains = loSegGrains(loseg);
  i = 0;

  while(i < grains) {
    /* object is a slight misnomer because it might point to a */
    /* free grain */
    Addr object = PoolAddrOfIndex(base, pool, i);
    Addr next;
    Index j;
    Buffer buffer;

    if (SegBuffer(&buffer, seg)) {
      if(object == BufferScanLimit(buffer) &&
         BufferScanLimit(buffer) != BufferLimit(buffer)) {
        /* skip over buffered area */
        object = BufferLimit(buffer);
        i = PoolIndexOfAddr(base, pool, object);
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
    j = PoolIndexOfAddr(base, pool, next);
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
  pool->alignShift = SizeLog2(pool->alignment);

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
                PoolGrainsSize(pool, loseg->freeGrains),
                PoolGrainsSize(pool, loseg->oldGrains),
                PoolGrainsSize(pool, loseg->newGrains),
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
  LO lo = MustBeA(LOPool, pool);
  Res res;
  Ring node, nextNode;
  RankSet rankSet;
  Seg seg;
  Bool b;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERC(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(BufferRankSet(buffer) == RankSetEMPTY);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));

  /* Try to find a segment with enough space already. */
  rankSet = BufferRankSet(buffer);
  RING_FOR(node, PoolSegRing(pool), nextNode) {
    seg = SegOfPoolRing(node);
    if (SegBufferFill(baseReturn, limitReturn, seg, size, rankSet))
      return ResOK;
  }

  /* No segment had enough space, so make a new one. */
  res = PoolGenAlloc(&seg, lo->pgen, CLASS(LOSeg),
                     SizeArenaGrains(size, PoolArena(pool)),
                     argsNone);
  if (res != ResOK)
    return res;
  b = SegBufferFill(baseReturn, limitReturn, seg, size, rankSet);
  AVER(b);
  return ResOK;
}


/* Synchronise the buffer with the alloc Bit Table in the segment. */


/* loSegPoolGen -- get pool generation for an LO segment */

static PoolGen loSegPoolGen(Pool pool, Seg seg)
{
  LO lo = MustBeA(LOPool, pool);
  AVERT(Seg, seg);
  return lo->pgen;
}


/* loSegWhiten -- whiten a segment */

static Res loSegWhiten(Seg seg, Trace trace)
{
  LOSeg loseg = MustBeA(LOSeg, seg);
  Pool pool = SegPool(seg);
  PoolGen pgen = PoolSegPoolGen(pool, seg);
  Buffer buffer;
  Count grains, agedGrains, uncondemnedGrains;

  AVERT(Trace, trace);
  AVER(SegWhite(seg) == TraceSetEMPTY);

  grains = loSegGrains(loseg);

  /* Whiten allocated objects; leave free areas black. */
  if (SegBuffer(&buffer, seg)) {
    Addr base = SegBase(seg);
    Index scanLimitIndex = PoolIndexOfAddr(base, pool, BufferScanLimit(buffer));
    Index limitIndex = PoolIndexOfAddr(base, pool, BufferLimit(buffer));
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
  PoolGenAccountForAge(pgen, PoolGrainsSize(pool, agedGrains),
                       PoolGrainsSize(pool, loseg->newGrains), FALSE);
  loseg->oldGrains += agedGrains + loseg->newGrains;
  loseg->bufferedGrains = uncondemnedGrains;
  loseg->newGrains = 0;

  if (loseg->oldGrains > 0) {
    GenDescCondemned(pgen->gen, trace,
                     PoolGrainsSize(pool, loseg->oldGrains));
    SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace));
  }

  return ResOK;
}


static Res loSegFix(Seg seg, ScanState ss, Ref *refIO)
{
  LOSeg loseg = MustBeA_CRITICAL(LOSeg, seg);
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
  if (!BTGet(loseg->alloc, i)) {
    AVER(ss->rank == RankAMBIG);
    return ResOK;
  }

  if(!BTGet(loseg->mark, i)) {
    ss->wasMarked = FALSE;  /* <design/fix/#was-marked.not> */
    if(ss->rank == RankWEAK) {
      *refIO = (Addr)0;
    } else {
      BTSet(loseg->mark, i);
    }
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
  INHERIT_CLASS(klass, LOPool, AbstractCollectPool);
  klass->instClassStruct.finish = LOFinish;
  klass->size = sizeof(LOStruct);
  klass->varargs = LOVarargs;
  klass->init = LOInit;
  klass->bufferFill = LOBufferFill;
  klass->segPoolGen = loSegPoolGen;
  klass->totalSize = LOTotalSize;
  klass->freeSize = LOFreeSize;
  AVERT(PoolClass, klass);
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
  if (lo->pgen != NULL) {
    CHECKL(lo->pgen == &lo->pgenStruct);
    CHECKD(PoolGen, lo->pgen);
  }
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

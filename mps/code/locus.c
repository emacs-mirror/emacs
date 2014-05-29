/* locus.c: LOCUS MANAGER
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * See <design/arenavm/> and <design/locus/> for basic locus stuff.
 * See <design/trace/> for chains. See <design/strategy/> for the
 * collection strategy.
 */

#include "chain.h"
#include "ring.h"
#include "mpm.h"
#include "mpstd.h"
#include <float.h> /* for DBL_MAX */

SRCID(locus, "$Id$");


/* SegPrefCheck -- check the consistency of a segment preference */

Bool SegPrefCheck(SegPref pref)
{
  CHECKS(SegPref, pref);
  CHECKL(BoolCheck(pref->high));
  /* zones can't be checked because it's arbitrary. */
  /* avoid can't be checked because it's arbitrary. */
  return TRUE;
}


/* SegPrefDefault -- return a segment preference representing the defaults */

static SegPrefStruct segPrefDefault = SegPrefDEFAULT;

SegPref SegPrefDefault(void)
{
  return &segPrefDefault;
}

/* SegPrefInit -- initialise a segment preference to the defaults */

void SegPrefInit(SegPref pref)
{
  (void)mps_lib_memcpy(pref, &segPrefDefault, sizeof(SegPrefStruct));
}


/* SegPrefExpress -- express a segment preference */

void SegPrefExpress(SegPref pref, SegPrefKind kind, void *p)
{
  AVERT(SegPref, pref);
  AVER(pref != &segPrefDefault);

  switch(kind) {
  case SegPrefHigh:
    AVER(p == NULL);
    pref->high = TRUE;
    break;

  case SegPrefLow:
    AVER(p == NULL);
    pref->high = FALSE;
    break;

  case SegPrefZoneSet:
    AVER(p != NULL);
    pref->zones = *(ZoneSet *)p;
    break;

  default:
    /* Unknown kinds are ignored for binary compatibility. */
    /* See design.mps.pref. */
    break;
  }
}


/* GenDescCheck -- check a GenDesc */

ATTRIBUTE_UNUSED
static Bool GenDescCheck(GenDesc gen)
{
  CHECKS(GenDesc, gen);
  /* nothing to check for zones */
  /* nothing to check for capacity */
  CHECKL(gen->mortality >= 0.0);
  CHECKL(gen->mortality <= 1.0);
  CHECKD_NOSIG(Ring, &gen->locusRing);
  return TRUE;
}


/* GenDescNewSize -- return effective size of generation */

static Size GenDescNewSize(GenDesc gen)
{
  Size size = 0;
  Ring node, nextNode;

  RING_FOR(node, &gen->locusRing, nextNode) {
    PoolGen pgen = RING_ELT(PoolGen, genRing, node);
    AVERT(PoolGen, pgen);
    size += pgen->newSize;
  }
  return size;
}


/* GenDescTotalSize -- return total size of generation */

static Size GenDescTotalSize(GenDesc gen)
{
  Size size = 0;
  Ring node, nextNode;

  RING_FOR(node, &gen->locusRing, nextNode) {
    PoolGen pgen = RING_ELT(PoolGen, genRing, node);
    AVERT(PoolGen, pgen);
    size += pgen->totalSize;
  }
  return size;
}


/* ChainCreate -- create a generation chain */

Res ChainCreate(Chain *chainReturn, Arena arena, size_t genCount,
                GenParamStruct *params)
{
  size_t i;
  Chain chain;
  GenDescStruct *gens;
  Res res;
  void *p;

  AVER(chainReturn != NULL);
  AVERT(Arena, arena);
  AVER(genCount > 0);
  AVER(params != NULL);
  for (i = 0; i < genCount; ++i) {
    AVER(params[i].capacity > 0);
    AVER(params[i].mortality > 0.0);
    AVER(params[i].mortality < 1.0);
  }

  res = ControlAlloc(&p, arena, genCount * sizeof(GenDescStruct), FALSE);
  if (res != ResOK)
    return res;
  gens = (GenDescStruct *)p;

  for (i = 0; i < genCount; ++i) {
    gens[i].zones = ZoneSetEMPTY;
    gens[i].capacity = params[i].capacity;
    gens[i].mortality = params[i].mortality;
    RingInit(&gens[i].locusRing);
    gens[i].sig = GenDescSig;
    AVERT(GenDesc, &gens[i]);
  }

  res = ControlAlloc(&p, arena, sizeof(ChainStruct), FALSE);
  if (res != ResOK)
    goto failChainAlloc;
  chain = (Chain)p;

  chain->arena = arena;
  RingInit(&chain->chainRing);
  chain->activeTraces = TraceSetEMPTY;
  chain->genCount = genCount;
  chain->gens = gens;
  chain->sig = ChainSig;

  RingAppend(&arena->chainRing, &chain->chainRing);
  AVERT(Chain, chain);
  *chainReturn = chain;
  return ResOK;

failChainAlloc:
  ControlFree(arena, gens, genCount * sizeof(GenDescStruct));
  return res;
}


/* ChainCheck -- check a chain */

Bool ChainCheck(Chain chain)
{
  size_t i;

  CHECKS(Chain, chain);
  CHECKU(Arena, chain->arena);
  CHECKD_NOSIG(Ring, &chain->chainRing);
  CHECKL(TraceSetCheck(chain->activeTraces));
  CHECKL(chain->genCount > 0);
  for (i = 0; i < chain->genCount; ++i) {
    CHECKD(GenDesc, &chain->gens[i]);
  }
  return TRUE;
}


/* ChainDestroy -- destroy a chain */

void ChainDestroy(Chain chain)
{
  Arena arena;
  size_t genCount;
  size_t i;

  AVERT(Chain, chain);
  AVER(chain->activeTraces == TraceSetEMPTY);

  arena = chain->arena;
  genCount = chain->genCount;
  RingRemove(&chain->chainRing);
  chain->sig = SigInvalid;
  for (i = 0; i < genCount; ++i) {
    RingFinish(&chain->gens[i].locusRing);
    chain->gens[i].sig = SigInvalid;
  }
  RingFinish(&chain->chainRing);
  ControlFree(arena, chain->gens, genCount * sizeof(GenDescStruct));
  ControlFree(arena, chain, sizeof(ChainStruct));
}


/* ChainGens -- return the number of generation in chain */

size_t ChainGens(Chain chain)
{
  AVERT(Chain, chain);
  return chain->genCount;
}


/* ChainGen -- return a generation in a chain, or the arena top generation */

GenDesc ChainGen(Chain chain, Index gen)
{
  AVERT(Chain, chain);
  AVER(gen <= chain->genCount);

  if (gen < chain->genCount)
    return &chain->gens[gen];
  else
    return &chain->arena->topGen;
}


/* PoolGenAlloc -- allocate a segment in a pool generation */

Res PoolGenAlloc(Seg *segReturn, PoolGen pgen, SegClass class, Size size,
                 Bool withReservoirPermit, ArgList args)
{
  SegPrefStruct pref;
  Res res;
  Seg seg;
  ZoneSet zones, moreZones;
  Arena arena;
  GenDesc gen;

  AVER(segReturn != NULL);
  AVERT(PoolGen, pgen);
  AVERT(SegClass, class);
  AVER(size > 0);
  AVERT(Bool, withReservoirPermit);
  AVERT(ArgList, args);

  arena = PoolArena(pgen->pool);
  gen = pgen->gen;
  zones = gen->zones;

  SegPrefInit(&pref);
  pref.high = FALSE;
  pref.zones = zones;
  pref.avoid = ZoneSetBlacklist(arena);
  res = SegAlloc(&seg, class, &pref, size, pgen->pool, withReservoirPermit,
                 args);
  if (res != ResOK)
    return res;

  moreZones = ZoneSetUnion(zones, ZoneSetOfSeg(arena, seg));
  gen->zones = moreZones;
  
  if (!ZoneSetSuper(zones, moreZones)) {
    /* Tracking the whole zoneset for each generation gives more
     * understandable telemetry than just reporting the added
     * zones. */
    EVENT3(ArenaGenZoneAdd, arena, gen, moreZones);
  }

  size = SegSize(seg);
  pgen->totalSize += size;
  STATISTIC_STAT ({
    ++ pgen->segs;
    pgen->freeSize += size;
  });
  *segReturn = seg;
  return ResOK;
}


/* ChainDeferral -- time until next ephemeral GC for this chain */

double ChainDeferral(Chain chain)
{
  double time = DBL_MAX;
  size_t i;

  AVERT(Chain, chain);

  if (chain->activeTraces == TraceSetEMPTY) {
    for (i = 0; i < chain->genCount; ++i) {
      double genTime = chain->gens[i].capacity * 1024.0
        - (double)GenDescNewSize(&chain->gens[i]);
      if (genTime < time)
        time = genTime;
    }
  }

  return time;
}


/* ChainCondemnAuto -- condemn approriate parts of this chain
 *
 * This is only called if ChainDeferral returned a value sufficiently
 * low that the tracer decided to start the collection.  (Usually
 * such values are less than zero; see <design/trace/>)
 */
Res ChainCondemnAuto(double *mortalityReturn, Chain chain, Trace trace)
{
  Res res;
  size_t topCondemnedGen, i;
  GenDesc gen;
  ZoneSet condemnedSet = ZoneSetEMPTY;
  Size condemnedSize = 0, survivorSize = 0, genNewSize, genTotalSize;

  AVERT(Chain, chain);
  AVERT(Trace, trace);

  /* Find the highest generation that's over capacity. We will condemn
   * this and all lower generations in the chain. */
  topCondemnedGen = chain->genCount;
  for (;;) {
    /* It's an error to call this function unless some generation is
     * over capacity as reported by ChainDeferral. */
    AVER(topCondemnedGen > 0);
    if (topCondemnedGen == 0)
      return ResFAIL;
    -- topCondemnedGen;
    gen = &chain->gens[topCondemnedGen];
    AVERT(GenDesc, gen);
    genNewSize = GenDescNewSize(gen);
    if (genNewSize >= gen->capacity * (Size)1024)
      break;
  }

  /* At this point, we've decided to condemn topCondemnedGen and all
   * lower generations. */
  for (i = 0; i <= topCondemnedGen; ++i) {
    gen = &chain->gens[i];
    AVERT(GenDesc, gen);
    condemnedSet = ZoneSetUnion(condemnedSet, gen->zones);
    genTotalSize = GenDescTotalSize(gen);
    genNewSize = GenDescNewSize(gen);
    condemnedSize += genTotalSize;
    survivorSize += (Size)(genNewSize * (1.0 - gen->mortality))
                    /* predict survivors will survive again */
                    + (genTotalSize - genNewSize);
  }
  
  AVER(condemnedSet != ZoneSetEMPTY || condemnedSize == 0);
  EVENT3(ChainCondemnAuto, chain, topCondemnedGen, chain->genCount);
  
  /* Condemn everything in these zones. */
  if (condemnedSet != ZoneSetEMPTY) {
    res = TraceCondemnZones(trace, condemnedSet);
    if (res != ResOK)
      return res;
  }

  *mortalityReturn = 1.0 - (double)survivorSize / condemnedSize;
  return ResOK;
}


/* ChainStartGC -- called to notify start of GC for this chain */

void ChainStartGC(Chain chain, Trace trace)
{
  AVERT(Chain, chain);
  AVERT(Trace, trace);

  chain->activeTraces = TraceSetAdd(chain->activeTraces, trace);
}


/* ChainEndGC -- called to notify end of GC for this chain */

void ChainEndGC(Chain chain, Trace trace)
{
  AVERT(Chain, chain);
  AVERT(Trace, trace);

  chain->activeTraces = TraceSetDel(chain->activeTraces, trace);
}


/* PoolGenInit -- initialize a PoolGen */

Res PoolGenInit(PoolGen pgen, GenDesc gen, Pool pool)
{
  /* Can't check pgen, because it's not been initialized. */
  AVER(pgen != NULL);
  AVERT(GenDesc, gen);
  AVERT(Pool, pool);
  AVER(PoolHasAttr(pool, AttrGC));

  pgen->pool = pool;
  pgen->gen = gen;
  RingInit(&pgen->genRing);
  STATISTIC(pgen->segs = 0);
  pgen->totalSize = 0;
  STATISTIC(pgen->freeSize = 0);
  pgen->newSize = 0;
  STATISTIC(pgen->oldSize = 0);
  pgen->newDeferredSize = 0;
  STATISTIC(pgen->oldDeferredSize = 0);
  pgen->sig = PoolGenSig;
  AVERT(PoolGen, pgen);

  RingAppend(&gen->locusRing, &pgen->genRing);
  return ResOK;
}


/* PoolGenFinish -- finish a PoolGen */

void PoolGenFinish(PoolGen pgen)
{
  AVERT(PoolGen, pgen);
  AVER(pgen->totalSize == 0);
  AVER(pgen->newSize == 0);
  AVER(pgen->newDeferredSize == 0);
  STATISTIC_STAT ({
    AVER(pgen->segs == 0);
    AVER(pgen->freeSize == 0);
    AVER(pgen->oldSize == 0);
    AVER(pgen->oldDeferredSize == 0);
  });

  pgen->sig = SigInvalid;
  RingRemove(&pgen->genRing);
}


/* PoolGenCheck -- check a PoolGen */

Bool PoolGenCheck(PoolGen pgen)
{
  CHECKS(PoolGen, pgen);
  /* nothing to check about serial */
  CHECKU(Pool, pgen->pool);
  CHECKU(GenDesc, pgen->gen);
  CHECKD_NOSIG(Ring, &pgen->genRing);
  STATISTIC_STAT ({
    CHECKL((pgen->totalSize == 0) == (pgen->segs == 0));
    CHECKL(pgen->totalSize >= pgen->segs * ArenaAlign(PoolArena(pgen->pool)));
    CHECKL(pgen->totalSize == pgen->freeSize + pgen->newSize + pgen->oldSize
           + pgen->newDeferredSize + pgen->oldDeferredSize);
  });
  return TRUE;
}


/* PoolGenFill -- accounting for allocation
 *
 * The memory was free, is now new (or newDeferred).
 */

void PoolGenFill(PoolGen pgen, Size size, Bool deferred)
{
  AVERT(PoolGen, pgen);
  AVERT(Bool, deferred);

  STATISTIC_STAT ({
    AVER(pgen->freeSize >= size);
    pgen->freeSize -= size;
  });
  if (deferred)
    pgen->newDeferredSize += size;
  else
    pgen->newSize += size;
}


/* PoolGenEmpty -- accounting for emptying a buffer
 *
 * The unused part of the buffer was new (or newDeferred) and is now free.
 */

void PoolGenEmpty(PoolGen pgen, Size unused, Bool deferred)
{
  AVERT(PoolGen, pgen);
  AVERT(Bool, deferred);

  if (deferred) {
    AVER(pgen->newDeferredSize >= unused);
    pgen->newDeferredSize -= unused;
  } else {
    AVER(pgen->newSize >= unused);
    pgen->newSize -= unused;
  }
  STATISTIC(pgen->freeSize += unused);
}


/* PoolGenAge -- accounting for condemning a segment
 *
 * The memory was new (or newDeferred), is now old (or oldDeferred)
 */

void PoolGenAge(PoolGen pgen, Size size, Bool deferred)
{
  AVERT(PoolGen, pgen);
  
  if (deferred) {
    AVER(pgen->newDeferredSize >= size);
    pgen->newDeferredSize -= size;
    STATISTIC(pgen->oldDeferredSize += size);
  } else {
    AVER(pgen->newSize >= size);
    pgen->newSize -= size;
    STATISTIC(pgen->oldSize += size);
  }
}


/* PoolGenReclaim -- accounting for reclaiming
 *
 * The reclaimed memory was old, and is now free.
 */

void PoolGenReclaim(PoolGen pgen, Size reclaimed, Bool deferred)
{
  AVERT(PoolGen, pgen);
  AVERT(Bool, deferred);

  STATISTIC_STAT ({
    if (deferred) {
      AVER(pgen->oldDeferredSize >= reclaimed);
      pgen->oldDeferredSize -= reclaimed;
    } else {
      AVER(pgen->oldSize >= reclaimed);
      pgen->oldSize -= reclaimed;
    }
    pgen->freeSize += reclaimed;
  });
}


/* PoolGenUndefer -- accounting for end of ramp mode
 *
 * The memory was oldDeferred or newDeferred, is now old or new.
 */

void PoolGenUndefer(PoolGen pgen, Size oldSize, Size newSize)
{
  AVERT(PoolGen, pgen);
  STATISTIC_STAT ({
    AVER(pgen->oldDeferredSize >= oldSize);
    pgen->oldDeferredSize -= oldSize;
    pgen->oldSize += oldSize;
  });
  AVER(pgen->newDeferredSize >= newSize);
  pgen->newDeferredSize -= newSize;
  pgen->newSize += newSize;
}


/* PoolGenSegSplit -- accounting for splitting a segment */

void PoolGenSegSplit(PoolGen pgen)
{
  AVERT(PoolGen, pgen);
  STATISTIC(++ pgen->segs);
}


/* PoolGenSegMerge -- accounting for merging a segment */

void PoolGenSegMerge(PoolGen pgen)
{
  AVERT(PoolGen, pgen);
  STATISTIC_STAT ({
    AVER(pgen->segs > 0);
    -- pgen->segs;
  });
}


/* PoolGenFree -- free a segment and update accounting
 *
 * The segment is assumed to finish free.
 */

void PoolGenFree(PoolGen pgen, Seg seg)
{
  Size size;

  AVERT(PoolGen, pgen);
  AVERT(Seg, seg);

  size = SegSize(seg);
  AVER(pgen->totalSize >= size);
  pgen->totalSize -= size;
  STATISTIC_STAT ({
    AVER(pgen->segs > 0);
    -- pgen->segs;
    AVER(pgen->freeSize >= size);
    pgen->freeSize -= size;
  });
  SegFree(seg);
}


/* LocusInit -- initialize the locus module */

void LocusInit(Arena arena)
{
  GenDesc gen = &arena->topGen;

  /* Can't check arena, because it's not been inited. */

  /* TODO: The mortality estimate here is unjustifiable.  Dynamic generation
     decision making needs to be improved and this constant removed. */
  gen->zones = ZoneSetEMPTY;
  gen->capacity = 0; /* unused */
  gen->mortality = 0.51;
  RingInit(&gen->locusRing);
  gen->sig = GenDescSig;
  AVERT(GenDesc, gen);
}


/* LocusFinish -- finish the locus module */

void LocusFinish(Arena arena)
{
  GenDesc gen = &arena->topGen;

  /* Can't check arena, because it's being finished. */

  gen->sig = SigInvalid;
  RingFinish(&gen->locusRing);
}


/* LocusCheck -- check the locus module */

Bool LocusCheck(Arena arena)
{
  /* Can't check arena, because this is part of ArenaCheck. */
  CHECKD(GenDesc, &arena->topGen);
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

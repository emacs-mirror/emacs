/* locus.c: LOCUS MANAGER
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * See <design/arenavm/> and <design/locus/> for basic locus stuff.
 * See <design/trace/> for chains.
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

static Bool GenDescCheck(GenDesc gen)
{
  CHECKS(GenDesc, gen);
  /* nothing to check for zones */
  /* nothing to check for capacity */
  CHECKL(gen->mortality >= 0.0);
  CHECKL(gen->mortality <= 1.0);
  CHECKL(gen->proflow >= 0.0);
  CHECKL(gen->proflow <= 1.0);
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
    gens[i].proflow = 1.0; /* @@@@ temporary */
    RingInit(&gens[i].locusRing);
    gens[i].sig = GenDescSig;
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

  arena = chain->arena; genCount = chain->genCount;
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


/* ChainAlloc -- allocate tracts in a generation */

Res ChainAlloc(Seg *segReturn, Chain chain, Serial genNr, SegClass class,
               Size size, Pool pool, Bool withReservoirPermit,
               ArgList args)
{
  SegPrefStruct pref;
  Res res;
  Seg seg;
  ZoneSet zones, moreZones;
  Arena arena;

  AVERT(Chain, chain);
  AVER(genNr <= chain->genCount);

  arena = chain->arena;
  if (genNr < chain->genCount)
    zones = chain->gens[genNr].zones;
  else
    zones = arena->topGen.zones;

  SegPrefInit(&pref);
  pref.high = FALSE;
  pref.zones = zones;
  pref.avoid = ZoneSetBlacklist(arena);
  res = SegAlloc(&seg, class, &pref, size, pool, withReservoirPermit, args);
  if (res != ResOK)
    return res;

  moreZones = ZoneSetUnion(zones, ZoneSetOfSeg(arena, seg));
  
  if (!ZoneSetSuper(zones, moreZones)) {
    /* Tracking the whole zoneset for each generation number gives
     * more understandable telemetry than just reporting the added
     * zones. */
    EVENT3(ArenaGenZoneAdd, arena, genNr, moreZones);
  }

  if (genNr < chain->genCount)
    chain->gens[genNr].zones = moreZones;
  else
    chain->arena->topGen.zones = moreZones;

  *segReturn = seg;
  return ResOK;
}



/* ChainDeferral -- time until next ephemeral GC for this chain */

double ChainDeferral(Chain chain)
{
  AVERT(Chain, chain);

  if (chain->activeTraces != TraceSetEMPTY)
    return DBL_MAX;
  else
    return chain->gens[0].capacity * 1024.0
           - (double)GenDescNewSize(&chain->gens[0]);
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
  Serial topCondemnedGenSerial, currGenSerial;
  GenDesc gen;
  ZoneSet condemnedSet = ZoneSetEMPTY;
  Size condemnedSize = 0, survivorSize = 0, genNewSize, genTotalSize;

  AVERT(Chain, chain);
  AVERT(Trace, trace);

  /* Find lowest gen within its capacity, set topCondemnedGenSerial to the */
  /* preceeding one. */
  currGenSerial = 0;
  gen = &chain->gens[0];
  AVERT(GenDesc, gen);
  genNewSize = GenDescNewSize(gen);
  do { /* At this point, we've decided to collect currGenSerial. */
    topCondemnedGenSerial = currGenSerial;
    condemnedSet = ZoneSetUnion(condemnedSet, gen->zones);
    genTotalSize = GenDescTotalSize(gen);
    condemnedSize += genTotalSize;
    survivorSize += (Size)(genNewSize * (1.0 - gen->mortality))
                    /* predict survivors will survive again */
                    + (genTotalSize - genNewSize);

    /* is there another one to consider? */
    currGenSerial += 1;
    if (currGenSerial >= chain->genCount)
      break; /* reached the top */
    gen = &chain->gens[currGenSerial];
    AVERT(GenDesc, gen);
    genNewSize = GenDescNewSize(gen);
  } while (genNewSize >= gen->capacity * (Size)1024);
  
  AVER(condemnedSet != ZoneSetEMPTY || condemnedSize == 0);
  EVENT3(ChainCondemnAuto, chain, topCondemnedGenSerial, chain->genCount);
  UNUSED(topCondemnedGenSerial); /* only used for EVENT */
  
  /* Condemn everything in these zones. */
  if (condemnedSet != ZoneSetEMPTY) {
    res = TraceCondemnZones(trace, condemnedSet);
    if (res != ResOK)
      return res;
  }

  *mortalityReturn = 1.0 - (double)survivorSize / condemnedSize;
  return ResOK;
}


/* ChainCondemnAll -- condemn everything in the chain */

Res ChainCondemnAll(Chain chain, Trace trace)
{
  Ring node, nextNode;
  Bool haveWhiteSegs = FALSE;
  Res res;

  /* Condemn every segment in every pool using this chain. */
  /* Finds the pools by iterating over the PoolGens in gen 0. */
  RING_FOR(node, &chain->gens[0].locusRing, nextNode) {
    PoolGen nursery = RING_ELT(PoolGen, genRing, node);
    Pool pool = nursery->pool;
    Ring segNode, nextSegNode;

    AVERT(Pool, pool);
    AVER(PoolHasAttr(pool, AttrGC));
    RING_FOR(segNode, PoolSegRing(pool), nextSegNode) {
      Seg seg = SegOfPoolRing(segNode);

      res = TraceAddWhite(trace, seg);
      if (res != ResOK)
        goto failBegin;
      haveWhiteSegs = TRUE;
    }
  }
 
  return ResOK;

failBegin:
  AVER(!haveWhiteSegs); /* Would leave white sets inconsistent. */
  return res;
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

Res PoolGenInit(PoolGen gen, Chain chain, Serial nr, Pool pool)
{
  /* Can't check gen, because it's not been initialized. */
  AVERT(Chain, chain);
  AVER(nr <= chain->genCount);
  AVERT(Pool, pool);

  gen->nr = nr;
  gen->pool = pool;
  gen->chain = chain;
  RingInit(&gen->genRing);
  gen->totalSize = (Size)0;
  gen->newSize = (Size)0;
  gen->sig = PoolGenSig;

  if(nr != chain->genCount) {
    RingAppend(&chain->gens[nr].locusRing, &gen->genRing);
  } else {
    /* Dynamic generation is linked to the arena, not the chain. */
    RingAppend(&chain->arena->topGen.locusRing, &gen->genRing);
  }
  AVERT(PoolGen, gen);
  return ResOK;
}


/* PoolGenFinish -- finish a PoolGen */

void PoolGenFinish(PoolGen gen)
{
  AVERT(PoolGen, gen);

  gen->sig = SigInvalid;
  RingRemove(&gen->genRing);
}


/* PoolGenCheck -- check a PoolGen */

Bool PoolGenCheck(PoolGen gen)
{
  CHECKS(PoolGen, gen);
  /* nothing to check about serial */
  CHECKU(Pool, gen->pool);
  CHECKU(Chain, gen->chain);
  CHECKD_NOSIG(Ring, &gen->genRing);
  CHECKL(gen->newSize <= gen->totalSize);
  return TRUE;
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
  gen->proflow = 0.0;
  RingInit(&gen->locusRing);
  gen->sig = GenDescSig;
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

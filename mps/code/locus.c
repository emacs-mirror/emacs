/* impl.c.locus: LOCUS MANAGER
 *
 * $HopeName: MMsrc!locus.c(trunk.4) $
 * Copyright (C) 2001 Harlequin Limited.  All rights reserved.
 *
 * DESIGN
 *
 * See design.mps.arena.vm and design.mps.locus for basic locus stuff.
 * See design.mps.trace for chains.
 */

#include "chain.h"
#include "ring.h"
#include "mpm.h"
#include "mpstd.h"
#include <float.h> /* for DBL_MAX */


SRCID(locus, "$HopeName: MMsrc!locus.c(trunk.4) $");


/* SegPrefCheck -- check the consistency of a segment preference */

Bool SegPrefCheck(SegPref pref)
{
  CHECKS(SegPref, pref);
  CHECKL(BoolCheck(pref->high));
  /* zones can't be checked because it's arbitrary. */
  CHECKL(BoolCheck(pref->isGen));
  CHECKL(BoolCheck(pref->isCollected));
  /* gen is an arbitrary serial */
  return TRUE;
}


/* SegPrefDefault -- return a segment preference representing the defaults */

static SegPrefStruct segPrefDefault = SegPrefDEFAULT;

SegPref SegPrefDefault(void)
{
  return &segPrefDefault;
}


/* SegPrefExpress -- express a segment preference */

Res SegPrefExpress(SegPref pref, SegPrefKind kind, void *p)
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

  case SegPrefCollected:
    AVER(p == NULL);
    pref->isCollected = TRUE;
    break;

  case SegPrefGen:
    AVER(p != NULL);
    pref->isGen = TRUE;
    pref->gen = *(Serial *)p;
    break;

  default:
    /* Unknown kinds are ignored for binary compatibility. */
    /* See design.mps.pref. */
    break;
  }

  return ResOK;
}


#if 1

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
  CHECKL(RingCheck(&gen->locusRing));
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
  if (res != ResOK) return res;
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
  CHECKL(RingCheck(&chain->chainRing));
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
 * high that the tracer decided to start the collection.  (Usually
 * more than zero, but sometimes less; see mps.design.trace.)
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
  currGenSerial = 0; gen = &chain->gens[0];
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

    if (++currGenSerial >= chain->genCount)
      break; /* reached the top */
    gen = &chain->gens[currGenSerial];
    AVERT(GenDesc, gen);
    genNewSize = GenDescNewSize(gen);
  } while (genNewSize >= gen->capacity * (Size)1024);

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
    AVER((pool->class->attr & AttrGC) != 0);
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

  if (nr != chain->genCount)
    RingAppend(&chain->gens[nr].locusRing, &gen->genRing);
  else
    /* Dynamic generation is linked to the arena, not the chain. */
    RingAppend(&chain->arena->topGen.locusRing, &gen->genRing);
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
  CHECKL(RingCheck(&gen->genRing));
  CHECKL(gen->newSize <= gen->totalSize);
  return TRUE;
}


/* PoolGenUpdateZones -- update the zone of the generation
 *
 * This is a temporary i/f: eventually the locus manager will update
 * these directly.
 */
void PoolGenUpdateZones(PoolGen gen, Seg seg)
{
  Chain chain;

  AVERT(PoolGen, gen);
  AVERT(Seg, seg);

  chain = gen->chain;
  AVERT(Chain, chain);
  if (gen->nr != chain->genCount)
    chain->gens[gen->nr].zones =
      ZoneSetUnion(chain->gens[gen->nr].zones, ZoneSetOfSeg(chain->arena, seg));
  /* No need to keep track of dynamic gen zoneset. */
}


/* LocusInit -- initialize the locus module */

void LocusInit(Arena arena)
{
  GenDesc gen = &arena->topGen;

  /* Can't check arena, because it's not been inited. */

  gen->zones = ZoneSetEMPTY;
  gen->capacity = 0; /* unused */
  gen->mortality = TraceTopGenMortality; /* @@@@ unused ATM */
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
  GenDescCheck(&arena->topGen);
  return TRUE;
}


#endif

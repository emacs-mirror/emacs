/* impl.h.chain: GENERATION CHAINS
 *
 * $HopeName: MMsrc!chain.h(trunk.1) $
 * Copyright (C) 2001 Harlequin Limited.  All rights reserved.
 */

#ifndef chain_h
#define chain_h

#include "mpmtypes.h"
#include "ring.h"


/* GenParamStruct -- structure for specifying generation parameters */
/* .gen-param: This structure must match impl.h.mps.gen-param. */

typedef struct GenParamStruct {
  Size capacity; /* capacity in kB */
  double mortality;
} GenParamStruct;


/* GenDesc -- descriptor of a generation in a chain */

typedef struct GenDescStruct *GenDesc;

#define GenDescSig ((Sig)0x5199E4DE)  /* SIGnature GEN DEsc */

typedef struct GenDescStruct {
  Sig sig;
  ZoneSet zones; /* zoneset for this generation */
  Size capacity; /* capacity in kB */
  double mortality;
  double proflow; /* predicted proportion of survivors promoted */
  RingStruct locusRing; /* this generation in all the pools using the chain */
} GenDescStruct;


/* PoolGen -- descriptor of a generation in a pool */

typedef struct PoolGenStruct *PoolGen;

#define PoolGenSig ((Sig)0x519B009E)  /* SIGnature POOl GEn */

typedef struct PoolGenStruct {
  Sig sig;
  Serial nr;          /* generation number */
  Pool pool;          /* pool this belongs to */
  Chain chain;        /* chain this belongs to */
  RingStruct genRing; /* this generation in all the pools using this chain */
  Size totalSize;     /* total size of segs in gen in this pool */
  Size newSize;       /* size allocated since last GC */
} PoolGenStruct;


/* Chain -- a generation chain */

#define ChainSig ((Sig)0x519C8A14)  /* SIGnature CHAIN */

typedef struct ChainStruct {
  Sig sig;
  Arena arena;
  RingStruct chainRing; /* list of chains in the arena */
  TraceSet activeTraces; /* set of traces collecting this chain */
  size_t genCount; /* number of generations */
  GenDescStruct *gens; /* the array of generations */
} ChainStruct;


extern Res ChainCreate(Chain *chainReturn, Arena arena, size_t genCount,
                       GenParamStruct *params);
extern void ChainDestroy(Chain chain);
extern Bool ChainCheck(Chain chain);

extern double ChainDeferral(Chain chain);
extern Res ChainCondemnAuto(double *mortalityReturn, Chain chain, Trace trace);
extern Res ChainCondemnAll(Chain chain, Trace trace);
extern void ChainStartGC(Chain chain, Trace trace);
extern void ChainEndGC(Chain chain, Trace trace);
extern size_t ChainGens(Chain chain);


extern Bool PoolGenCheck(PoolGen gen);
extern Res PoolGenInit(PoolGen gen, Chain chain, Serial nr, Pool pool);
extern void PoolGenFinish(PoolGen gen);
extern void PoolGenFlip(PoolGen gen);
#define PoolGenNr(gen) ((gen)->nr)
extern void PoolGenUpdateZones(PoolGen gen, Seg seg);


#endif /* chain_h */

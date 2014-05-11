/* chain.h: GENERATION CHAINS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */

#ifndef chain_h
#define chain_h

#include "mpmtypes.h"
#include "ring.h"


/* GenParamStruct -- structure for specifying generation parameters */
/* .gen-param: This structure must match <code/mps.h#gen-param>. */

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
  RingStruct locusRing; /* Ring of all PoolGen's in this GenDesc (locus) */
} GenDescStruct;


/* PoolGen -- descriptor of a generation in a pool */

typedef struct PoolGenStruct *PoolGen;

#define PoolGenSig ((Sig)0x519B009E)  /* SIGnature POOl GEn */

typedef struct PoolGenStruct {
  Sig sig;
  Serial nr;          /* generation number */
  Pool pool;          /* pool this belongs to */
  Chain chain;        /* chain this belongs to */
  /* link in ring of all PoolGen's in this GenDesc (locus) */
  RingStruct genRing;
  Size totalSize;     /* total size of segs in gen in this pool */
  Size newSize;       /* size allocated since last GC */
  /* newSize when TraceCreate was called. This is used in the
   * TraceStartPoolGen event emitted at the start of a trace; at that
   * time, newSize has already been diminished by Whiten so we can't
   * use that value. TODO: This will not work well with multiple
   * traces. */
  Size newSizeAtCreate;
} PoolGenStruct;


/* Chain -- a generation chain */

#define ChainSig ((Sig)0x519C8A14)  /* SIGnature CHAIN */

typedef struct mps_chain_s {
  Sig sig;
  Arena arena;
  RingStruct chainRing; /* list of chains in the arena */
  TraceSet activeTraces; /* set of traces collecting this chain */
  size_t genCount; /* number of generations */
  GenDescStruct *gens; /* the array of generations */
} ChainStruct;


extern Res GenDescDescribe(GenDesc gen, mps_lib_FILE *stream, Count depth);

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
extern Res ChainAlloc(Seg *segReturn, Chain chain, Serial genNr,
                      SegClass class, Size size, Pool pool,
                      Bool withReservoirPermit, ArgList args);
extern Res ChainDescribe(Chain chain, mps_lib_FILE *stream, Count depth);

extern Bool PoolGenCheck(PoolGen gen);
extern Res PoolGenInit(PoolGen gen, Chain chain, Serial nr, Pool pool);
extern void PoolGenFinish(PoolGen gen);
extern void PoolGenFlip(PoolGen gen);
#define PoolGenNr(gen) ((gen)->nr)
extern Res PoolGenDescribe(PoolGen gen, mps_lib_FILE *stream, Count depth);


#endif /* chain_h */


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

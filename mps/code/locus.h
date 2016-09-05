/* locus.h: GENERATION CHAINS
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 */

#ifndef locus_h
#define locus_h

#include "mpmtypes.h"
#include "ring.h"


/* GenParamStruct -- structure for specifying generation parameters */
/* .gen-param: This structure must match <code/mps.h#gen-param>. */

typedef struct GenParamStruct *GenParam;

typedef struct GenParamStruct {
  Size capacity;                /* capacity in kB */
  double mortality;             /* predicted mortality */
} GenParamStruct;


/* GenTraceStats -- per-generation per-trace statistics */

typedef struct GenTraceStatsStruct *GenTraceStats;

typedef struct GenTraceStatsStruct {
  Size condemned;        /* size of objects condemned by the trace */
  Size forwarded;        /* size of objects that were forwarded by the trace */
  Size preservedInPlace; /* size of objects preserved in place by the trace */
} GenTraceStatsStruct;


/* GenDesc -- descriptor of a generation in a chain */

typedef struct GenDescStruct *GenDesc;

#define GenDescSig ((Sig)0x5199E4DE)  /* SIGnature GEN DEsc */

typedef struct GenDescStruct {
  Sig sig;
  ZoneSet zones;        /* zoneset for this generation */
  Size capacity;        /* capacity in kB */
  double mortality;     /* predicted mortality */
  RingStruct locusRing; /* Ring of all PoolGen's in this GenDesc (locus) */
  RingStruct segRing; /* Ring of GCSegs in this generation */
  GenTraceStatsStruct trace[TraceLIMIT];
} GenDescStruct;


/* PoolGen -- descriptor of a generation in a pool */

typedef struct PoolGenStruct *PoolGen;

#define PoolGenSig ((Sig)0x519B009E)  /* SIGnature POOl GEn */

typedef struct PoolGenStruct {
  Sig sig;
  Pool pool;          /* pool this belongs to */
  GenDesc gen;        /* generation this belongs to */
  /* link in ring of all PoolGen's in this GenDesc (locus) */
  RingStruct genRing;

  /* Accounting of memory in this generation for this pool */  
  Size segs;              /* number of segments */
  Size totalSize;         /* total (sum of segment sizes) */
  Size freeSize;          /* unused (free or lost to fragmentation) */
  Size bufferedSize;      /* held in buffers but not condemned yet */
  Size newSize;           /* allocated since last collection */
  Size oldSize;           /* allocated prior to last collection */
  Size newDeferredSize;   /* new (but deferred) */
  Size oldDeferredSize;   /* old (but deferred) */
} PoolGenStruct;


/* Chain -- a generation chain */

#define ChainSig ((Sig)0x519C8A14)  /* SIGnature CHAIN */

typedef struct mps_chain_s {
  Sig sig;
  Arena arena;
  RingStruct chainRing; /* list of chains in the arena */
  TraceSet activeTraces; /* set of traces collecting this chain */
  size_t genCount; /* number of generations */
  GenDesc gens; /* the array of generations */
} ChainStruct;


extern Bool GenDescCheck(GenDesc gen);
extern Size GenDescNewSize(GenDesc gen);
extern Size GenDescTotalSize(GenDesc gen);
extern void GenDescCondemned(GenDesc gen, Trace trace, Size size);
extern void GenDescSurvived(GenDesc gen, Trace trace, Size forwarded, Size preservedInPlace);
extern Res GenDescDescribe(GenDesc gen, mps_lib_FILE *stream, Count depth);

extern Res ChainCreate(Chain *chainReturn, Arena arena, size_t genCount,
                       GenParam params);
extern void ChainDestroy(Chain chain);
extern Bool ChainCheck(Chain chain);

extern double ChainDeferral(Chain chain);
extern void ChainStartTrace(Chain chain, Trace trace);
extern void ChainEndTrace(Chain chain, Trace trace);
extern size_t ChainGens(Chain chain);
extern GenDesc ChainGen(Chain chain, Index gen);
extern Res ChainDescribe(Chain chain, mps_lib_FILE *stream, Count depth);

extern Bool PoolGenCheck(PoolGen pgen);
extern Res PoolGenInit(PoolGen pgen, GenDesc gen, Pool pool);
extern void PoolGenFinish(PoolGen pgen);
extern Res PoolGenAlloc(Seg *segReturn, PoolGen pgen, SegClass klass,
                        Size size, ArgList args);
extern void PoolGenFree(PoolGen pgen, Seg seg, Size freeSize, Size oldSize,
                        Size newSize, Bool deferred);
extern void PoolGenAccountForFill(PoolGen pgen, Size size);
extern void PoolGenAccountForEmpty(PoolGen pgen, Size used, Size unused, Bool deferred);
extern void PoolGenAccountForAge(PoolGen pgen, Size wasBuffered, Size wasNew, Bool deferred);
extern void PoolGenAccountForReclaim(PoolGen pgen, Size reclaimed, Bool deferred);
extern void PoolGenUndefer(PoolGen pgen, Size oldSize, Size newSize);
extern void PoolGenAccountForSegSplit(PoolGen pgen);
extern void PoolGenAccountForSegMerge(PoolGen pgen);
extern Res PoolGenDescribe(PoolGen gen, mps_lib_FILE *stream, Count depth);

#endif /* locus_h */


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

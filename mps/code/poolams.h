/* impl.h.poolams: AUTOMATIC MARK & SWEEP POOL CLASS INTERFACE
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .purpose: Internal interface to AMS functionality.
 */

#ifndef poolams_h
#define poolams_h

#include "mpm.h"
#include <stdarg.h>


typedef struct AMSStruct *AMS;
typedef struct AMSSegStruct *AMSSeg;


/* AMSRingFunction is the type of the method to find the ring that */
/* the AMS pool is allocating on. */
typedef Ring (*AMSRingFunction)(AMS ams, RankSet rankSet, Size size);
/* AMSSegClassFunction is the type of the method to indicate */
/* the segment class of an AMS pool. Returns a subclass of AMSSegClass. */
/* The type is congruent with SegClassGet functions.  */
typedef SegClass (*AMSSegClassFunction)(void);
/* AMSSegsDestroyFunction is the type of the method to destroy all */
/* segs of an AMS pool. */
typedef void (*AMSSegsDestroyFunction)(AMS ams);
/* AMSSegSizePolicyFunction is the type of the method which picks */
/* a segment size given an object size. */
typedef Res (*AMSSegSizePolicyFunction)(Size *sizeReturn,
                                        Pool pool, Size size,
					RankSet rankSet);
/* AMSObjectFunction is the type of the method that an */
/* AMSIterateFunction applies to each object in a segment. */
typedef Res (*AMSObjectFunction)(
  /* the segment */              Seg seg,
  /* the object grain index */   Index i,
  /* the address of the object */Addr p,
  /*  "   "   after the object */Addr next,
  /* the iteration closure */    void *closure);

#define AMSObjectFunctionCheck(f) \
  ((f) != NULL) /* that's the best we can do */

typedef Res (*AMSIterateFunction)(Seg seg, AMSObjectFunction f, void *closure);


typedef struct AMSStruct {
  PoolStruct poolStruct;       /* generic pool structure */
  Shift grainShift;            /* log2 of grain size */
  Chain chain;                 /* chain used by this pool */
  PoolGenStruct pgen;          /* generation representing the pool */
  Size size;                   /* total segment size of the pool */
  AMSIterateFunction iterate;  /* iterator function */
  AMSSegSizePolicyFunction segSize; /* SegSize policy */
  RingStruct segRing;          /* ring of segments in the pool */
  AMSRingFunction allocRing;   /* fn to get the ring to allocate from */
  AMSSegsDestroyFunction segsDestroy;
  AMSSegClassFunction segClass;/* fn to get the class for segments */
  Sig sig;                     /* design.mps.pool.outer-structure.sig */
} AMSStruct;


typedef struct AMSSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  AMS ams;               /* owning ams */
  RingStruct segRing;    /* ring that this seg belongs to */
  Count grains;          /* number of grains */
  Count free;            /* number of free grains */
  Count newAlloc;        /* number of grains allocated since last GC */
  Bool allocTableInUse;  /* whether we use allocTable */
  Index firstFree;       /* 1st free grain, if allocTable is not used */
  BT allocTable;         /* set if grain is allocated */
  /* design.mps.poolams.colour.single */
  Bool marksChanged;     /* has been marked since last scan */
  Bool ambiguousFixes;   /* has been ambiguously marked since last scan */
  Bool colourTablesInUse;/* whether we use the colour tables */
  BT nongreyTable;       /* set if grain not grey */
  BT nonwhiteTable;      /* set if grain not white */
  Sig sig;
} AMSSegStruct;


/* macros to get between child and parent structures */

#define Seg2AMSSeg(seg) ((AMSSeg)(seg))
#define AMSSeg2Seg(amsseg) ((Seg)(amsseg))

#define Pool2AMS(pool) PARENT(AMSStruct, poolStruct, pool)
#define AMS2Pool(ams) (&(ams)->poolStruct)


/* macros for abstracting index/address computations */
/* design.mps.poolams.addr-index.slow */

/* only use when size is a multiple of the grain size */
#define AMSGrains(ams, size) ((size) >> (ams)->grainShift)

#define AMSGrainsSize(ams, grains) ((grains) << (ams)->grainShift)

#define AMSSegShift(seg) (Seg2AMSSeg(seg)->ams->grainShift)

#define AMS_ADDR_INDEX(seg, addr) \
  ((Index)(AddrOffset(SegBase(seg), addr) >> AMSSegShift(seg)))
#define AMS_INDEX_ADDR(seg, index) \
  AddrAdd(SegBase(seg), (Size)(index) << AMSSegShift(seg))


/* colour ops */

#define AMSIsWhite(seg, index) \
  (!BTGet(Seg2AMSSeg(seg)->nonwhiteTable, index))

#define AMSIsGrey(seg, index) \
  (!BTGet(Seg2AMSSeg(seg)->nongreyTable, index))

#define AMSIsBlack(seg, index) \
  (!AMSIsGrey(seg, index) && !AMSIsWhite(seg, index))

#define AMSIsInvalidColor(seg, index) \
  (AMSIsGrey(seg, index) && AMSIsWhite(seg, index))

#define AMSGreyBlacken(seg, index) \
  BEGIN \
    BTSet(Seg2AMSSeg(seg)->nongreyTable, index); \
  END

#define AMSWhiteGreyen(seg, index) \
  BEGIN \
    BTSet(Seg2AMSSeg(seg)->nonwhiteTable, index); \
    BTRes(Seg2AMSSeg(seg)->nongreyTable, index); \
  END

#define AMSWhiteBlacken(seg, index) \
  BEGIN \
    BTSet(Seg2AMSSeg(seg)->nonwhiteTable, index); \
  END

#define AMSRangeWhiteBlacken(seg, base, limit) \
  BEGIN \
    BTSetRange(Seg2AMSSeg(seg)->nonwhiteTable, base, limit); \
  END

#define AMSRangeWhiten(seg, base, limit) \
  BEGIN \
    BTResRange(Seg2AMSSeg(seg)->nonwhiteTable, base, limit); \
    BTSetRange(Seg2AMSSeg(seg)->nongreyTable, base, limit); \
  END

#define AMSRangeBlacken(seg, base, limit) \
  BEGIN \
    BTSetRange(Seg2AMSSeg(seg)->nonwhiteTable, base, limit); \
    BTSetRange(Seg2AMSSeg(seg)->nongreyTable, base, limit); \
  END

#define AMSFindGrey(pos, dummy, seg, base, limit) \
  BTFindShortResRange(pos, dummy, Seg2AMSSeg(seg)->nongreyTable, \
                      base, limit, 1) \

#define AMSFindWhite(pos, dummy, seg, base, limit) \
  BTFindShortResRange(pos, dummy, Seg2AMSSeg(seg)->nonwhiteTable, \
                      base, limit, 1) \


#define AMS_ALLOCED(seg, index) \
  (Seg2AMSSeg(seg)->allocTableInUse \
   ? BTGet(Seg2AMSSeg(seg)->allocTable, index) \
   : (Seg2AMSSeg(seg)->firstFree > (index)))


/* the rest */

extern Res AMSInitInternal(AMS ams, Format format, Chain chain);
extern void AMSFinish(Pool pool);
extern Bool AMSCheck(AMS ams);

extern Res AMSBufferInit(Pool pool, Buffer buffer, va_list args);
extern Res AMSBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size,
                         Bool withReservoirPermit);
extern void AMSBufferEmpty(Pool pool, Buffer buffer,
                           Addr init, Addr limit);

extern Res AMSWhiten(Pool pool, Trace trace, Seg seg);
extern Res AMSScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg);
extern Res AMSFix(Pool pool, ScanState ss, Seg seg, Ref *refIO);
extern void AMSBlacken(Pool pool, TraceSet traceSet, Seg seg);
extern void AMSReclaim(Pool pool, Trace trace, Seg seg);

#define AMSChain(ams) ((ams)->chain)


typedef SegClass AMSSegClass;
typedef SegClassStruct AMSSegClassStruct;
extern AMSSegClass AMSSegClassGet(void);
extern Bool AMSSegCheck(AMSSeg seg);


typedef PoolClass AMSPoolClass;
typedef PoolClassStruct AMSPoolClassStruct;

extern AMSPoolClass AMSPoolClassGet(void);


#endif /* poolams_h */


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

/* poolams.h: AUTOMATIC MARK & SWEEP POOL CLASS INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .purpose: Internal interface to AMS functionality.  */

#ifndef poolams_h
#define poolams_h

#include "mpmtypes.h"
#include "mpmst.h"
#include "ring.h"
#include "bt.h"
#include "mpscams.h"
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


typedef struct AMSStruct {
  PoolStruct poolStruct;       /* generic pool structure */
  Shift grainShift;            /* log2 of grain size */
  PoolGenStruct pgen;          /* generation representing the pool */
  Size size;                   /* total segment size of the pool */
  AMSSegSizePolicyFunction segSize; /* SegSize policy */
  AMSSegsDestroyFunction segsDestroy;
  AMSSegClassFunction segClass;/* fn to get the class for segments */
  Bool shareAllocTable;        /* the alloc table is also used as white table */
  Sig sig;                     /* <design/pool/#outer-structure.sig> */
} AMSStruct;


typedef struct AMSSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  AMS ams;               /* owning ams */
  Count grains;          /* total grains */
  Count freeGrains;      /* free grains */
  Count oldGrains;       /* grains allocated prior to last collection */
  Count newGrains;       /* grains allocated since last collection */
  Bool allocTableInUse;  /* allocTable is used */
  Index firstFree;       /* 1st free grain, if allocTable is not used */
  BT allocTable;         /* set if grain is allocated */
  /* <design/poolams/#colour.single> */
  Bool marksChanged;     /* seg has been marked since last scan */
  Bool ambiguousFixes;   /* seg has been ambiguously marked since last scan */
  Bool colourTablesInUse;/* the colour tables are in use */
  BT nonwhiteTable;      /* set if grain not white */
  BT nongreyTable;       /* set if not first grain of grey object */
  Sig sig;
} AMSSegStruct;


/* macros to get between child and parent structures */

#define Seg2AMSSeg(seg) ((AMSSeg)(seg))
#define AMSSeg2Seg(amsseg) ((Seg)(amsseg))

#define PoolAMS(pool) PARENT(AMSStruct, poolStruct, pool)
#define AMSPool(ams) (&(ams)->poolStruct)


/* macros for abstracting index/address computations */
/* <design/poolams/#addr-index.slow> */

/* only use when size is a multiple of the grain size */
#define AMSGrains(ams, size) ((size) >> (ams)->grainShift)

#define AMSGrainsSize(ams, grains) ((grains) << (ams)->grainShift)

#define AMSSegShift(seg) (Seg2AMSSeg(seg)->ams->grainShift)

#define AMS_ADDR_INDEX(seg, addr) \
  ((Index)(AddrOffset(SegBase(seg), addr) >> AMSSegShift(seg)))
#define AMS_INDEX_ADDR(seg, index) \
  AddrAdd(SegBase(seg), (Size)(index) << AMSSegShift(seg))


/* colour ops */

#define AMS_IS_WHITE(seg, index) \
  (!BTGet(Seg2AMSSeg(seg)->nonwhiteTable, index))

#define AMS_IS_GREY(seg, index) \
  (!BTGet(Seg2AMSSeg(seg)->nongreyTable, index))

#define AMS_IS_BLACK(seg, index) \
  (!AMS_IS_GREY(seg, index) && !AMS_IS_WHITE(seg, index))

#define AMS_IS_INVALID_COLOUR(seg, index) \
  (AMS_IS_GREY(seg, index) && !AMS_IS_WHITE(seg, index))

#define AMS_WHITE_GREYEN(seg, index) \
  BEGIN \
    BTRes(Seg2AMSSeg(seg)->nongreyTable, index); \
  END

#define AMS_GREY_BLACKEN(seg, index) \
  BEGIN \
    BTSet(Seg2AMSSeg(seg)->nongreyTable, index); \
    BTSet(Seg2AMSSeg(seg)->nonwhiteTable, index); \
  END

#define AMS_WHITE_BLACKEN(seg, index) \
  BEGIN \
    BTSet(Seg2AMSSeg(seg)->nonwhiteTable, index); \
  END

#define AMS_RANGE_WHITE_BLACKEN(seg, base, limit) \
  BEGIN \
    BTSetRange(Seg2AMSSeg(seg)->nonwhiteTable, base, limit); \
  END

#define AMS_RANGE_BLACKEN(seg, base, limit) \
  BEGIN \
    BTSetRange(Seg2AMSSeg(seg)->nonwhiteTable, base, limit); \
    BTSetRange(Seg2AMSSeg(seg)->nongreyTable, base, limit); \
  END

#define AMS_RANGE_WHITEN(seg, base, limit) \
  BEGIN \
    BTResRange(Seg2AMSSeg(seg)->nonwhiteTable, base, limit); \
    BTSetRange(Seg2AMSSeg(seg)->nongreyTable, base, limit); \
  END

#define AMSFindGrey(pos, dummy, seg, base, limit) \
  BTFindShortResRange(pos, dummy, Seg2AMSSeg(seg)->nongreyTable, \
                      base, limit, 1)

#define AMSFindWhite(pos, dummy, seg, base, limit) \
  BTFindShortResRange(pos, dummy, Seg2AMSSeg(seg)->nonwhiteTable, \
                      base, limit, 1)

#define AMS_FIND_WHITE_RANGE(baseOut, limitOut, seg, base, limit) \
  BTFindLongResRange(baseOut, limitOut, Seg2AMSSeg(seg)->nonwhiteTable, \
                     base, limit, 1)

#define AMS_ALLOCED(seg, index) \
  (Seg2AMSSeg(seg)->allocTableInUse \
   ? BTGet(Seg2AMSSeg(seg)->allocTable, index) \
   : (Seg2AMSSeg(seg)->firstFree > (index)))


/* the rest */

extern Res AMSInitInternal(AMS ams, Format format, Chain chain, unsigned gen,
                           Bool shareAllocTable);
extern void AMSFinish(Pool pool);
extern Bool AMSCheck(AMS ams);

extern Res AMSScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg);

#define AMSChain(ams) ((ams)->chain)

extern void AMSSegFreeWalk(AMSSeg amsseg, FreeBlockVisitor f, void *p);

extern void AMSSegFreeCheck(AMSSeg amsseg);


typedef SegClass AMSSegClass;
typedef SegClassStruct AMSSegClassStruct;
extern AMSSegClass AMSSegClassGet(void);
extern Bool AMSSegCheck(AMSSeg seg);


typedef PoolClass AMSPoolClass;
typedef PoolClassStruct AMSPoolClassStruct;

extern AMSPoolClass AMSPoolClassGet(void);
extern AMSPoolClass AMSDebugPoolClassGet(void);


#endif /* poolams_h */


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

/* poolams.h: AUTOMATIC MARK & SWEEP POOL CLASS INTERFACE
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .purpose: Internal interface to AMS functionality.  */

#ifndef poolams_h
#define poolams_h

#include "mpmtypes.h"
#include "mpm.h"
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
  PoolGenStruct pgenStruct;    /* generation representing the pool */
  PoolGen pgen;                /* NULL or pointer to pgenStruct field */
  Size size;                   /* total segment size of the pool */
  AMSSegSizePolicyFunction segSize; /* SegSize policy */
  AMSSegsDestroyFunction segsDestroy;
  AMSSegClassFunction segClass;/* fn to get the class for segments */
  Bool shareAllocTable;        /* the alloc table is also used as white table */
  Sig sig;                     /* <design/pool#.outer-structure.sig> */
} AMSStruct;


typedef struct AMSSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  AMS ams;               /* owning ams */
  Count grains;          /* total grains */
  Count freeGrains;      /* free grains */
  Count bufferedGrains;  /* grains in buffers */
  Count newGrains;       /* grains allocated since last collection */
  Count oldGrains;       /* grains allocated prior to last collection */
  Bool allocTableInUse;  /* allocTable is used */
  Index firstFree;       /* 1st free grain, if allocTable is not used */
  BT allocTable;         /* set if grain is allocated */
  /* <design/poolams#.colour.single> */
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

extern Res AMSInitInternal(AMS ams, Arena arena, PoolClass klass,
                           Chain chain, unsigned gen,
                           Bool shareAllocTable, ArgList args);
extern void AMSFinish(Inst inst);
extern Bool AMSCheck(AMS ams);

#define AMSChain(ams) ((ams)->chain)

extern void AMSSegFreeWalk(AMSSeg amsseg, FreeBlockVisitor f, void *p);

extern void AMSSegFreeCheck(AMSSeg amsseg);

extern Bool AMSSegCheck(AMSSeg seg);


/* class declarations */

typedef AMS AMSPool;
DECLARE_CLASS(Pool, AMSPool, AbstractCollectPool);

typedef AMS AMSDebugPool;
DECLARE_CLASS(Pool, AMSDebugPool, AMSPool);

DECLARE_CLASS(Seg, AMSSeg, MutatorSeg);


#endif /* poolams_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* impl.h.poolams: AUTOMATIC MARK & SWEEP POOL CLASS INTERFACE
 *
 * $HopeName: MMsrc!poolams.h(trunk.10) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * .purpose: Internal interface to AMS functionality.
 */

#ifndef poolams_h
#define poolams_h

#include "mpm.h"
#include <stdarg.h>


typedef struct AMSStruct *AMS;
typedef struct AMSGroupStruct *AMSGroup;


/* GroupInitFunction is the type of a group init method. */
typedef Res (*GroupInitFunction)(AMSGroup group, Pool pool);
/* GroupFinishFunction is the type of a group finish method. */
typedef void (*GroupFinishFunction)(AMSGroup group);
/* AMSRingFunction is the type of the method to find the ring that */
/* the AMS pool is allocating on. */
typedef Ring (*AMSRingFunction)(AMS ams, RankSet rankSet, Size size);
/* AMSGroupsDestroyFunction is the type of the method to destroy all */
/* groups of an AMS pool. */
typedef void (*AMSGroupsDestroyFunction)(AMS ams);
/* AMSSegSizePolicyFunction is the type of the method which picks */
/* a segment (group) size given an object size. */
typedef Res (*AMSSegSizePolicyFunction)(Size *sizeReturn,
                                        Pool pool, Size size,
					RankSet rankSet);
/* AMSObjectFunction is the type of the method that an */
/* AMSIterateFunction applies to each object in a group. */
typedef Res (*AMSObjectFunction)(
  /* the group */                AMSGroup group,
  /* the object grain index */   Index i,
  /* the address of the object */Addr p,
  /*  "   "   after the object */Addr next,
  /* the iteration closure */    void *closure);

#define AMSObjectFunctionCheck(f) \
  ((f) != NULL) /* that's the best we can do */

typedef Res (*AMSIterateFunction)(AMSGroup group,
                                  AMSObjectFunction f, void *closure);


typedef struct AMSStruct {
  PoolStruct poolStruct;       /* generic pool structure */
  Shift grainShift;            /* log2 of grain size */
  ActionStruct actionStruct;   /* action of collecting this pool */
  Size size;                   /* total segment size of the pool */
  Size lastReclaimed;          /* size after last reclaim */
  AMSIterateFunction iterate;  /* iterator function */
  AMSSegSizePolicyFunction segSize; /* SegSize policy */
  RingStruct groupRing;        /* ring of groups in the pool */
  AMSRingFunction allocRing;   /* fn to get the ring to allocate from */
  AMSGroupsDestroyFunction groupsDestroy;
  /* The next four might someday be part of a generic group class. */
  size_t groupSize;            /* size of outer group structure */
  size_t groupOffset;          /* offset of generic group in group */
  GroupInitFunction groupInit;
  GroupFinishFunction groupFinish;
  Sig sig;                     /* design.mps.pool.outer-structure.sig */
} AMSStruct;


typedef struct AMSGroupStruct {
  Sig sig;
  Seg seg;               /* segment of group's memory */
  AMS ams;               /* owning ams */
  RingStruct groupRing;  /* ring that this group belongs to */
  Count grains;          /* number of grains */
  Count free;            /* number of free grains */
  Bool allocTableInUse;  /* whether we use allocTable */
  Index firstFree;       /* 1st free grain, if allocTable is not used */
  BT allocTable;         /* set if grain is allocated */
  /* design.mps.poolams.colour.single */
  Bool marksChanged;     /* has been marked since last scan */
  Bool ambiguousFixes;   /* has been ambiguously marked since last scan */
  Bool colourTablesInUse;/* whether we use the colour tables */
  BT nongreyTable;       /* set if grain not grey */
  BT nonwhiteTable;      /* set if grain not white */
} AMSGroupStruct;


/* macros to get between child and parent structures */

#define PoolPoolAMS(pool) PARENT(AMSStruct, poolStruct, (pool))
#define AMSPool(ams)      (&(ams)->poolStruct)

#define ActionAMS(action) PARENT(AMSStruct, actionStruct, (action))
#define AMSAction(ams)    (&(ams)->actionStruct)


/* macros for abstracting index/address computations */
/* design.mps.poolams.addr-index.slow */

/* only use when size is a multiple of the grain size */
#define AMSGrains(ams, size) ((size) >> (ams)->grainShift)

#define AMSGroupBase(group)  SegBase((group)->seg)
#define AMSGroupLimit(group) SegLimit((group)->seg)

#define AMSGroupShift(group) ((group)->ams->grainShift)

#define AMS_ADDR_INDEX(group, addr) \
  ((Index)(AddrOffset(AMSGroupBase((group)), (addr)) \
           >> AMSGroupShift((group))))
#define AMS_INDEX_ADDR(group, index) \
  AddrAdd(AMSGroupBase((group)), \
          (Size)(index) << AMSGroupShift((group)))

#define AMSSegGroup(seg) ((AMSGroup)SegP((seg)))

#define AMSArena(ams) PoolArena(AMSPool((ams)))

#define AMSGroupArena(group) AMSArena((group)->ams)


/* colour ops */

#define AMSIsWhite(group, index) !BTGet((group)->nonwhiteTable, (index))

#define AMSIsGrey(group, index) !BTGet((group)->nongreyTable, (index))

#define AMSIsBlack(group, index) \
  (!AMSIsGrey((group), (index)) && !AMSIsWhite((group), (index)))

#define AMSIsInvalidColor(group, index) \
  (AMSIsGrey((group), (index)) && AMSIsWhite((group), (index)))

#define AMSGreyBlacken(group, index) \
  BEGIN \
    BTSet((group)->nongreyTable, (index)); \
  END

#define AMSWhiteGreyen(group, index) \
  BEGIN \
    BTSet((group)->nonwhiteTable, (index)); \
    BTRes((group)->nongreyTable, (index)); \
  END

#define AMSWhiteBlacken(group, index) \
  BEGIN \
    BTSet((group)->nonwhiteTable, (index)); \
  END

#define AMSRangeWhiteBlacken(group, base, limit) \
  BEGIN \
    BTSetRange((group)->nonwhiteTable, (base), (limit)); \
  END

#define AMSRangeWhiten(group, base, limit) \
  BEGIN \
    BTResRange((group)->nonwhiteTable, (base), (limit)); \
    BTSetRange((group)->nongreyTable, (base), (limit)); \
  END

#define AMSRangeBlacken(group, base, limit) \
  BEGIN \
    BTSetRange((group)->nonwhiteTable, (base), (limit)); \
    BTSetRange((group)->nongreyTable, (base), (limit)); \
  END

#define AMSFindGrey(pos, dummy, group, base, limit) \
  BTFindShortResRange((pos), (dummy), (group)->nongreyTable, \
                      (base), (limit), 1) \

#define AMSFindWhite(pos, dummy, group, base, limit) \
  BTFindShortResRange((pos), (dummy), (group)->nonwhiteTable, \
                      (base), (limit), 1) \


#define AMS_ALLOCED(group, index) \
  ((group)->allocTableInUse \
   ? BTGet((group)->allocTable, (index)) \
   : ((group)->firstFree > (index)))


extern Res AMSInit(Pool pool, va_list arg);
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

extern Res AMSGroupInit(AMSGroup group, Pool pool);
extern void AMSGroupFinish(AMSGroup group);
extern void AMSGroupDestroy(AMSGroup group);
extern Bool AMSGroupCheck(AMSGroup group);
extern Res AMSGroupDescribe(AMSGroup group, mps_lib_FILE *stream);

typedef PoolClass AMSPoolClass;
typedef PoolClassStruct AMSPoolClassStruct;

extern AMSPoolClass EnsureAMSPoolClass(void);

#endif /* poolams_h */

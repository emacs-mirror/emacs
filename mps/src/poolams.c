/* impl.c.poolams: AUTOMATIC MARK & SWEEP POOL CLASS
 *
 * $HopeName: MMsrc!poolams.c(trunk.2) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 * 
 * NOTES
 * 
 * .readership: any MPS developer.
 * 
 * .scope: Implementation of a basic mark/sweep pool.
 *
 * .purpose: A canonical mark/sweep pool to be used as the basis for
 * other mark/sweep pools and for understanding the issues involved in
 * doing mark/sweep collection in the MPS framework.
 * 
 * .design: See design.mps.poolams.
 */

#include "mpm.h"
#include "mpscams.h"

SRCID(poolams, "$HopeName: MMsrc!poolams.c(trunk.2) $");

/* These two BT utility functions should be in the BT module.
 * See design.mps.poolams.bt.utilities */

static void AMSBTCopyInvRange(BT fromBT, BT toBT, Index base, Index limit)
{
  Index i = base;
  while(i < limit) {
    if (BTGet(fromBT, i))
      BTRes(toBT,i);
    else
      BTSet(toBT,i);
    ++ i;
  }
}

static Bool AMSBTRangesSame(BT BTx, BT BTy, Index base, Index limit)
{
  Index i = base;
  while(i < limit) {
    if (BTGet(BTx, i) != BTGet(BTy, i))
      return FALSE;
    ++ i;
  }
  return TRUE;
}

#define AMSSig          ((Sig)0x519A3599) /* SIGnature AMS */

typedef struct AMSStruct *AMS;
typedef struct AMSStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  Format format;                /* format of objects in this pool */
  Shift grainShift;             /* log2 of grain size */
  ActionStruct actionStruct;    /* action of collecting this pool */
  Size size;                    /* total segment size of the pool */
  Size lastReclaimed;           /* total size of the pool after last reclaim */
  Sig sig;
} AMSStruct;


#define AMSGroupSig     ((Sig)0x519A359B) /* SIGnature AMS GrouP */

typedef struct AMSGroupStruct *AMSGroup;
typedef struct AMSGroupStruct {
  Sig sig;
  Seg seg;                      /* segment of group's memory */
  AMS ams;                      /* owning ams */
  Count grains;                 /* number of grains in this group */
  BT allocTable;                /* set if grain is allocated */

  /* design.mps.poolams.one-condemn */
  Bool marked;                  /* has been marked since last scan */
  BT markTable;                 /* set if grain marked */
  BT scanTable;                 /* set if grain scanned */
} AMSGroupStruct;

/* prototype the check function here; definition is at the end of the
 * file, after the declaration of the pool class structure. */

static Bool AMSCheck(AMS ams);

/* macros to get between child and parent structures */

#define PoolPoolAMS(pool) PARENT(AMSStruct, poolStruct, pool)
#define AMSPool(ams)      (&(ams)->poolStruct)

#define ActionAMS(action) PARENT(AMSStruct, actionStruct, action)
#define AMSAction(ams)    (&(ams)->actionStruct)

/* macros for abstracting index/address computations */

/* design.mps.poolams.addr-index.slow */

#define AMSGroupSpace(group)      PoolSpace(AMSPool((group)->ams))

#define AMSGrains(ams,size)       ((size) >> (ams)->grainShift)

#define AMSGroupBase(group)       SegBase(AMSGroupSpace(group), (group)->seg)

#define AMSGroupLimit(group)      SegLimit(AMSGroupSpace(group), (group)->seg)

#define AMSGroupShift(group)      ((group)->ams->grainShift)

#define AMSGroupOffset(group, addr) AddrOffset(AMSGroupBase(group), addr)

#define AMSGroupAddr(group, offset) AddrAdd(AMSGroupBase(group), offset)

#define AMSAddrIndex(group,addr) ((Index)(AMSGroupOffset(group,addr) \
                                          >> AMSGroupShift(group)))

#define AMSIndexAddr(group,index) AMSGroupAddr(group,                \
                                               (index) << AMSGroupShift(group))

#define AMSSegGroup(seg) ((AMSGroup)SegP(seg))


static Bool AMSGroupCheck(AMSGroup group)
{
  CHECKS(AMSGroup, group);
  CHECKL(SegCheck(group->seg));
  CHECKU(AMS, group->ams);

  /* do the grains check both ways, to avoid rounding and overflow errors */
  CHECKL(group->grains ==
         (SegSize(PoolSpace(AMSPool(group->ams)), group->seg) >>
          group->ams->grainShift));
  CHECKL((group->grains << group->ams->grainShift) ==
         SegSize(PoolSpace(AMSPool(group->ams)), group->seg));
  
  if (SegWhite(group->seg) != TraceSetEMPTY) {
    CHECKL(TraceSetSingle(SegWhite(group->seg)));
  }

  CHECKL(BoolCheck(group->marked));
  /* design.mps.poolams.bt-check */
  CHECKL(group->allocTable != NULL);
  CHECKL(group->markTable != NULL);
  CHECKL(group->scanTable != NULL);
  return TRUE;
}

/* the different valid colours of grains. See design.mps.poolams.invariant. */

enum {
  AMS_WHITE,
  AMS_GREY,
  AMS_BLACK,
  AMS_FREE,
  AMS_ILLEGAL
};

/* find the colour of a grain. See design.mps.poolams.colour.slow */

static int AMSGrainColour(AMSGroup group, Index index)
{
  Bool mark, scan, alloc;

  AVERT(AMSGroup, group);
  AVER(index < group->grains);

  mark = BTGet(group->markTable, index);
  scan = BTGet(group->scanTable, index);
  alloc = BTGet(group->allocTable, index);
  
  if (mark) {     /* mark */
    if (scan) {   /* mark, scan */
      if (alloc)
        return AMS_BLACK;       /*  mark,  scan,  alloc: black */
      else
        return AMS_ILLEGAL;
    } else {      /* mark, !scan */
      if (alloc)
        return AMS_GREY;        /*  mark, !scan,  alloc: grey */
      else
        return AMS_ILLEGAL;
    } 
  } else {        /* !mark */
    if (scan) {   /* !mark, scan */
      if (alloc)
        return AMS_ILLEGAL;
      else
        return AMS_FREE;        /* !mark,  scan, !alloc: free */
    } else {      /* !mark, !scan */
      if (alloc)
        return AMS_WHITE;       /*  mark, !scan,  alloc: white */
      else
        return AMS_ILLEGAL;
    }
  }
}

/* AMSBTCreate -- allocate a BT from the control pool */

static Res AMSBTCreate(BT *btReturn, Space space, Count length)
{
  Res res;
  BT bt;
  void *p;

  AVER(btReturn != NULL);
  AVERT(Space, space);
  AVER(length > 0);

  res = ArenaAlloc(&p, space, BTSize(length));
  if(res != ResOK)
    return res;
  bt = (BT)p;

  BTResRange(bt, 0, length);

  *btReturn = bt;
  return ResOK;
}

/* AMSBTDestroy -- free a BT to the control pool */

static void AMSBTDestroy(BT bt, Space space, Count length)
{
  AVER(bt != NULL);
  AVERT(Space, space);
  AVER(length > 0);
  
  ArenaFree(space, bt, BTSize(length));
}

static Res AMSGroupCreate(AMSGroup *groupReturn, Pool pool, Size size,
                          RankSet rankSet)
{
  AMSGroup group;               /* the group */
  AMS ams;
  Res res;
  Space space;
  Seg seg;
  void *p;
  
  AVER(groupReturn != NULL);
  AVERT(Pool, pool);
  AVER(RankSetCheck(rankSet));
  AVER(size > 0);

  ams = PoolPoolAMS(pool);
  AVERT(AMS,ams);
  
  space = PoolSpace(pool);
  
  size = SizeAlignUp(size, ArenaAlign(space));
  if (size == 0)
    return ResMEMORY;
  
  res = ArenaAlloc(&p, space, (Size)sizeof(AMSGroupStruct));
  if (res != ResOK)
    goto failGroup;
  group = (AMSGroup)p;

  res = PoolSegAlloc(&seg, SegPrefDefault(), pool, size);
  if (res != ResOK)
    goto failSeg;
  
  group->seg = seg;
  SegSetP(seg, (void*)group);
  SegSetRankSet(seg, rankSet);
  if (rankSet != RankSetEMPTY)
    SegSetSummary(seg, RefSetUNIV);

  group->grains = size >> ams->grainShift;
  group->marked = FALSE; /* design.mps.poolams.marked.unused */

  res = AMSBTCreate(&group->allocTable, space, group->grains);
  if (res != ResOK)
    goto failAlloc;

  res = AMSBTCreate(&group->markTable, space, group->grains);
  if (res != ResOK)
    goto failMark;

  res = AMSBTCreate(&group->scanTable, space, group->grains);
  if (res != ResOK)
    goto failScan;
  
  /* design.mps.poolams.invariant.create */
  BTSetRange(group->scanTable, 0, group->grains);

  group->ams = ams;
  group->sig = AMSGroupSig;
  AVERT(AMSGroup, group);
  ams->size += size;

  *groupReturn = group;
  return ResOK;
  
failScan:
  AMSBTDestroy(group->markTable, space, group->grains);
failMark:
  AMSBTDestroy(group->allocTable, space, group->grains);
failAlloc:
  PoolSegFree(pool, seg);
failSeg:
  ArenaFree(space, group, (Size)sizeof(AMSGroupStruct));
failGroup:
  return res;
}

static void AMSGroupDestroy(AMSGroup group)
{
  AMS ams;
  Space space;

  AVERT(AMSGroup, group);
  ams = group->ams;
  AVERT(AMS, ams);
  space = PoolSpace(AMSPool(ams));
  AVERT(Space, space);

  AVER(ams->size >= SegSize(space, group->seg));

  ams->size -= SegSize(space, group->seg);
  ams->lastReclaimed = ams->size;

  group->sig = SigInvalid;

  AMSBTDestroy(group->allocTable, space, group->grains);
  AMSBTDestroy(group->markTable, space, group->grains);
  AMSBTDestroy(group->scanTable, space, group->grains);
  PoolSegFree(AMSPool(ams), group->seg);
  ArenaFree(space, group, (Size)sizeof(AMSGroupStruct));
}  
  
static Res AMSInit(Pool pool, va_list arg)
{
  AMS ams;

  AVERT(Pool, pool);

  ams = PoolPoolAMS(pool);

  ams->format = va_arg(arg, Format);
  AVERT(Format, ams->format);
  pool->alignment = ams->format->alignment;
  ams->grainShift = SizeLog2(pool->alignment);

  ActionInit(AMSAction(ams), pool);

  ams->size = 0;
  ams->lastReclaimed = 0;

  ams->sig = AMSSig;
  AVERT(AMS, ams);

  return ResOK;
}

static void AMSFinish(Pool pool)
{
  AMS ams;
  Ring ring, node;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  ring = PoolSegRing(pool);
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Seg seg = SegOfPoolRing(node);
    AMSGroup group = (AMSGroup)SegP(seg);

    AMSGroupDestroy(group);

    node = next;
  }

  ActionFinish(AMSAction(ams));
  /* can't invalidate the AMS until we've destroyed all the groups */
  ams->sig = SigInvalid;
}

/* attempts to allocate an object of at least the given size in the
 * given group. If successful, returns the base and limit grain
 * indices of the allocated object.  */

static Bool AMSGroupAlloc(Index *baseReturn, Index *limitReturn,
                          AMSGroup group, Size size)
{
  AMS ams;
  Size grains;
  Bool b;
  Index base, limit;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(AMSGroup, group);

  ams = group->ams;
  AVERT(AMS, ams);

  AVER(size > 0);
  AVER(SizeIsAligned(size, AMSPool(ams)->alignment));

  grains = AMSGrains(ams, size);
  AVER(grains >= 1);
  if (grains > group->grains)
    return FALSE;

  b = BTFindLongResRange(&base, &limit, group->allocTable,
                         0, group->grains, grains);
  if (!b)
    return FALSE;

  /* design.mps.poolams.invariant.free */
  AVER(BTIsResRange(group->markTable, base, limit));
  AVER(BTIsSetRange(group->scanTable, base, limit));
  AVER(BTIsResRange(group->allocTable, base, limit));

  /* design.mps.poolams.invariant.black */
  BTSetRange(group->allocTable, base, limit);
  BTSetRange(group->markTable, base, limit);

  *baseReturn = base;
  *limitReturn = limit;
  return TRUE;
}


static Res AMSBufferFill(Seg *segReturn,
                         Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
{
  Res res;
  AMS ams;
  AMSGroup group;
  Ring node, ring;
  Index base, limit;
  Bool b;

  AVER(segReturn != NULL);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  size = SizeAlignUp(size, PoolAlignment(pool));
  if (size == 0) { /* overflow */
    return ResMEMORY;
  }

  /* design.mps.poolams.fill.slow */
  ring = PoolSegRing(pool);
  RING_FOR(node, ring) {
    Seg seg = SegOfPoolRing(node);
    if (SegBuffer(seg) == NULL) {
      group = AMSSegGroup(seg);
      AVERT(AMSGroup, group);
      b = AMSGroupAlloc(&base, &limit, group, size);
      if (b)
        goto found;
    }
  }
  
  res = AMSGroupCreate(&group, pool, size, BufferRankSet(buffer));
  if (res != ResOK)
    return res;
  b = AMSGroupAlloc(&base, &limit, group, size);
  AVER(b);
  
found:
  *segReturn = group->seg;
  *baseReturn = AMSIndexAddr(group, base);
  *limitReturn = AMSIndexAddr(group, limit);
  return ResOK;
}

static void AMSBufferEmpty(Pool pool, Buffer buffer)
{
  AMS ams;
  Addr init, limit;
  Index initIndex, limitIndex;
  Seg seg;
  AMSGroup group;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  AVERT(Buffer,buffer);
  AVER(!BufferIsReset(buffer));
  AVER(BufferIsReady(buffer));
  
  seg = BufferSeg(buffer);
  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  AVER(group->seg == seg);
  
  init = BufferGetInit(buffer);
  limit = BufferLimit(buffer);

  AVER(AddrIsAligned(init, PoolAlignment(pool)));
  AVER(AddrIsAligned(limit, PoolAlignment(pool)));

  if (init == limit)
    return;

  initIndex = AMSAddrIndex(group, init);
  limitIndex = AMSAddrIndex(group, limit);

  /* design.mps.poolams.invariant.black, design.mps.poolams.invariant.fill */
  AVER(BTIsSetRange(group->markTable, initIndex, limitIndex));
  AVER(BTIsSetRange(group->scanTable, initIndex, limitIndex));
  AVER(BTIsSetRange(group->allocTable, initIndex, limitIndex));

  /* design.mps.poolams.invariant.free, design.mps.poolams.invariant.empty */
  BTResRange(group->markTable, initIndex, limitIndex);
  BTResRange(group->allocTable, initIndex, limitIndex);
}

static void AMSRangeCondemn(AMSGroup group, Index base, Index limit)
{
  if (base != limit) {
    AVER(base < limit);
    AVER(limit <= group->grains);
    
    /* either black or free, see design.mps.poolams.invariant */
    AVER(BTIsSetRange(group->scanTable, base, limit));
    AVER(AMSBTRangesSame(group->allocTable, group->markTable, base, limit));
    
    /* black -> white, free -> free, see design.mps.poolams.invariant */
    BTResRange(group->markTable, base, limit);
    AMSBTCopyInvRange(group->allocTable, group->scanTable, base, limit);
  }
}

static Res AMSCondemn(Pool pool, Trace trace, Seg seg, Action action)
{
  AMS ams;
  AMSGroup group;
  Buffer buffer;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  AVERT(Trace, trace);
  AVER(SegCheck(seg));
  AVERT(Action, action);
  AVER(ams == ActionAMS(action));

  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  AVER(group->seg == seg);
  AVER(group->ams == ams);

  buffer = SegBuffer(seg);
  if (buffer != NULL) { /* design.mps.poolams.condemn.buffer */
    Index scanLimitIndex, limitIndex;
    scanLimitIndex = AMSAddrIndex(group, BufferScanLimit(buffer));
    limitIndex = AMSAddrIndex(group, BufferLimit(buffer));
    
    AMSRangeCondemn(group, 0, scanLimitIndex);
    AMSRangeCondemn(group, limitIndex, group->grains);
  } else { /* condemn whole seg */
    AMSRangeCondemn(group, 0, group->grains);
  }

  group->marked = FALSE; /* design.mps.poolams.marked.condemn */
  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace->ti));

  return ResOK;
}

static Res AMSScanGroupOnce(ScanState ss, AMS ams, AMSGroup group,
                            Seg seg, Space space, Bool scanAllObjects)
{
  Res res;
  Format format;
  Align alignment;
  Addr p;
  Addr limit;

  limit = SegLimit(space, seg);
  format = ams->format;
  alignment = AMSPool(ams)->alignment;

  p = SegBase(space, seg);
  while (p < limit) {
    Addr next;
    Buffer buffer = SegBuffer(seg);

    AVER(AddrIsAligned(p, alignment));
    
    if (buffer != NULL &&
        p == BufferScanLimit(buffer) &&
        p != BufferLimit(buffer)) { /* design.mps.poolams.scan.buffer */
      p = BufferLimit(buffer); 
    } else { /* not in the buffer */

      Index i = AMSAddrIndex(group,p);
      int colour = AMSGrainColour(group, i);
      AVER(colour != AMS_ILLEGAL);
      if (colour == AMS_FREE) { /* no object here */
        next = AddrAdd(p, alignment);
      } else { /* there is an object here */
        next = (*format->skip)(p);
        if (scanAllObjects || (colour == AMS_GREY)) {
          next = (*format->skip)(p);
          res = (*format->scan)(ss, p, next);
          if (res != ResOK) {
            return res;
          }
          BTSet(group->scanTable, i);
        }
      }
      p = next;
    }
  }
  AVER(p == limit);
  return ResOK;
}

static Res AMSScan(ScanState ss, Pool pool, Seg seg)
{
  Res res;
  AMS ams;
  Space space;
  AMSGroup group;
  Bool scanOnce;
  Bool scanAllObjects;

  AVERT(ScanState, ss);
  AVER(ss->summary == RefSetEMPTY); /* to make afterSummary correct */
  AVER(ss->fixed == RefSetEMPTY);   /* to make afterSummary correct */
  
  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  space = PoolSpace(pool);

  AVER(SegCheck(seg));
  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);

  /* design.mps.poolams.scan.all */
  scanAllObjects = (TraceSetDiff(ss->traces, SegWhite(seg)) != TraceSetEMPTY);
  /* design.mps.poolams.scan.once */
  scanOnce = (TraceSetInter(ss->traces, SegWhite(seg)) == TraceSetEMPTY);

  AVER(!scanOnce || scanAllObjects); /* scanOnce implies scanAllObjects */
  
  if (scanOnce) {

    Bool wasMarked = group->marked; /* for checking */
    group->marked = FALSE;          /* for checking */
    res = AMSScanGroupOnce(ss, ams, group, seg, space, scanAllObjects);
    AVER(!group->marked);
    group->marked = wasMarked;      /* restore marked flag */
    if (res != ResOK)
      return res;

  } else {

    AVER(group->marked);
    do { /* design.mps.poolams.marked.scan */
      group->marked = FALSE; 
      res = AMSScanGroupOnce(ss, ams, group, seg, space, scanAllObjects);
      if (res != ResOK) {
        group->marked = TRUE; /* design.mps.poolams.marked.scan.fail */
        return res;
      }
    } while(group->marked);

  }

  AVER(RefSetSub(ss->summary, SegSummary(seg)));

  if (!scanAllObjects) { /* design.mps.poolams.summary */
    /* design.mps.poolams.summary.scan.part.summary */
    ss->summary = RefSetUnion(ss->summary, SegSummary(seg));
    /* design.mps.poolams.summary.scan.part.fixed */
    ss->fixed = RefSetUnion(ss->fixed, RefSetInter(SegSummary(seg),
                                                   ss->white));
  }

  return ResOK;
}

static Res AMSFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AMS ams;
  AMSGroup group;
  Space space;
  Index i;
  Ref ref;
  int colour;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  AVERT(ScanState, ss);
  AVER(refIO != NULL);

  AVER(SegCheck(seg));
  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  
  space = PoolSpace(pool);

  ref = *refIO;
  i = AMSAddrIndex(group, ref);
  colour = AMSGrainColour(group, i);
  AVER(colour != AMS_ILLEGAL);
  
  ss->wasMarked = TRUE;

  switch (ss->rank) {
  case RankAMBIG:
    /* not a real pointer if not aligned or not allocated */
    if(!AddrIsAligned((Addr)ref, pool->alignment) ||
       (colour == AMS_FREE)) {
      return ResOK;
    }
    /* falls through */
  case RankEXACT:
  case RankFINAL:
  case RankWEAK:
    AVER(AddrIsAligned((Addr)ref, pool->alignment));
    AVER(colour != AMS_FREE);
    if(colour == AMS_WHITE) {
      ss->wasMarked = FALSE;
      if(ss->rank == RankWEAK) { /* then splat the reference */
        *refIO = (Ref)0;
      } else {
        BTSet(group->markTable, i); /* turn this object grey */

        /* design.mps.poolams.fix.to-black */
        if (RefSetInter(SegSummary(seg), ss->white) != RefSetEMPTY) {
          TraceSegGreyen(space, seg, ss->traces); /* turn this segment grey */
          group->marked = TRUE; /* design.mps.poolams.marked.fix */
        } else {
          BTSet(group->scanTable, i); /* turn this object black */
        }
      }
    }
    break;
  default:
    NOTREACHED;
  }

  return ResOK;
}

static void AMSReclaim(Pool pool, Trace trace, Seg seg)
{
  AMS ams;
  AMSGroup group;
  Space space;
  Format format;
  Addr p;
  Addr limit;
  Buffer buffer;
  Bool anySurvivors;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  group = AMSSegGroup(seg);

  AVER(group->marked == FALSE); /* design.mps.poolams.marked.reclaim */
  
  space = PoolSpace(pool);
  limit = SegLimit(space, seg);
  format = ams->format;
  buffer = SegBuffer(seg);
  p = SegBase(space, seg);
  anySurvivors = FALSE;

  while (p < limit) {
    Addr next;
    AVER(AddrIsAligned(p, pool->alignment));
    
    if (buffer != NULL &&
        p == BufferScanLimit(buffer) &&
        p != BufferLimit(buffer)) { /* design.mps.poolams.reclaim.buffer */
      p = BufferLimit(buffer); 
    } else { /* not in the buffer */

      Index i = AMSAddrIndex(group, p);
      int colour = AMSGrainColour(group, i);
      AVER(colour != AMS_ILLEGAL);
      AVER(colour != AMS_GREY); /* no grey objects now */
      if (colour == AMS_FREE) { /* no object here */
        next = AddrAdd(p, pool->alignment);
      } else { /* there is an object here */
        next = (*format->skip)(p);
        if (colour == AMS_WHITE) { /* then we can free it */
          Index j = AMSAddrIndex(group, next);
          /* design.mps.poolams.invariant.free */
          BTResRange(group->markTable, i, j);
          BTSetRange(group->scanTable, i, j);
          BTResRange(group->allocTable, i, j);
        } else {
          anySurvivors = TRUE;
        }
      }
      p = next;
    }
  }
  AVER(p == limit);
  
  if ((buffer == NULL) && !anySurvivors) {
    AMSGroupDestroy(group);
  } else {
    SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace->ti));
  }
}

/* design.mps.poolams.benefit.guess */
static double AMSBenefit(Pool pool, Action action)
{
  AMS ams;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  AVERT(Action, action);
  AVER(ams == ActionAMS(action));

  /* design.mps.poolams.benefit.repeat */
  if ((ams->size > 1024*1024) &&
      (ams->size > ams->lastReclaimed + ams->lastReclaimed))
    return 1.0;
  else
    return 0.0;
}

static Res AMSSegDescribe(AMS ams, Seg seg, mps_lib_FILE *stream)
{
  Res res;
  AMSGroup group;
  Buffer buffer;
  Index i;

  AVERT(AMS, ams);
  AVER(SegCheck(seg));
  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  AVER(group->ams == ams);

  buffer = SegBuffer(seg);

  if (buffer != NULL) {
    AVERT(Buffer, buffer);
    AVER(BufferSeg(buffer) == seg);
  }

  res = WriteF(stream,
               "AMS Group $P {\n", (WriteFP)group,
               "  seg $P [$A-$A]\n",
               (WriteFP)seg, AMSGroupBase(group), AMSGroupLimit(group),
               "  AMS $P\n", (WriteFP)ams,
               "  grains $W\n", (WriteFW)group->grains,
               "  tables: alloc $P, mark $P, scan $P\n",
                 (WriteFP)group->allocTable,
                 (WriteFP)group->markTable,
                 (WriteFP)group->scanTable,
               "  map: ",
               NULL);
               
  for (i=0 ; i < group->grains; ++ i) {
    char c = 0;
    if (buffer != NULL) {
      Index baseIndex, limitIndex, scanLimitIndex, initIndex, allocIndex;

      baseIndex = AMSAddrIndex(group, BufferBase(buffer));
      limitIndex = AMSAddrIndex(group, BufferLimit(buffer));
      scanLimitIndex = AMSAddrIndex(group, BufferScanLimit(buffer));
      initIndex = AMSAddrIndex(group, BufferGetInit(buffer));
      allocIndex = AMSAddrIndex(group, BufferAlloc(buffer));

      if (i == limitIndex)
        c = ']';
      else if (i == baseIndex)
        c = '[';
      else if (i == scanLimitIndex)
        c = '<';
      else if (i == initIndex)
        c = '|';
      else if (i == allocIndex)
        c = '>';

      if (c != 0) {
        res = WriteF(stream, "$C", c, NULL);
        if (res != ResOK)
          return res;
      }
    }

    switch(AMSGrainColour(group, i)) {
    case AMS_FREE:
      c = '.';
      break;
    case AMS_WHITE:
      c = '-';
      break;
    case AMS_GREY:
      c = '+';
      break;
    case AMS_BLACK:
      c = '*';
      break;
    case AMS_ILLEGAL: /* maybe we've broken an invariant from the debugger */
      c = '!';
      break;
    default:
      NOTREACHED;
    }
    res = WriteF(stream, "$C", c, NULL);
    if (res != ResOK)
      return res;
    if (i % 64 == 63) {
      res = WriteF(stream, "\n       ", NULL);
      if (res != ResOK)
        return res;
    }
  }

  res = WriteF(stream, "\n} AMS Group $P\n", (WriteFP)group);
  return res;
}

static Res AMSDescribe(Pool pool, mps_lib_FILE *stream)
{
  AMS ams;
  Ring node;
  Res res;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  res = WriteF(stream,
               "AMS $P {\n", (WriteFP)ams,
               "  pool $P ($U)\n",
               (WriteFP)pool, (WriteFU)pool->serial,
               "  size $W, lastReclaimed $W\n",
               (WriteFW)ams->size, (WriteFW)ams->lastReclaimed,
               "  format $P ($U)\n",
               (WriteFP)ams->format, (WriteFU)ams->format->serial,
               "  grain shift $U\n", (WriteFU)ams->grainShift,
               NULL);
  if (res != ResOK)
    return res;

  res = WriteF(stream,
               "  segments [* = black, + = grey, - = white, . = free,\n"
               "            ! = bad, [.<.|.>.] = buffer b, sl, i, a, l]:\n",
               NULL);
  if (res != ResOK)
    return res;

  
  RING_FOR(node, PoolSegRing(pool)) {
    Seg seg = SegOfPoolRing(node);
    AMSSegDescribe(ams, seg, stream);
  }

  res = WriteF(stream, "} AMS $P\n",(WriteFP)ams, NULL);
  if (res != ResOK)
    return res;

  return ResOK;
}

/* PoolClassAMSStruct -- the class descriptor */

static PoolClassStruct PoolClassAMSStruct = {
  PoolClassSig,
  "AMS",                        /* name */
  sizeof(AMSStruct),            /* size */
  offsetof(AMSStruct, poolStruct),      /* offset */
  AttrFMT | AttrSCAN | AttrBUF | AttrBUF_RESERVE | AttrGC | AttrINCR_RB,
  AMSInit,                      /* init */
  AMSFinish,                    /* finish */
  PoolNoAlloc,                   /* design.mps.poolams.no-alloc */
  PoolNoFree,                    /* design.mps.poolams.no-free */
  PoolTrivBufferInit,           /* design.mps.poolams.triv-buffer-init */
  AMSBufferFill,                /* bufferFill */
  AMSBufferEmpty,               /* bufferEmpty */
  PoolTrivBufferFinish,         /* design.mps.poolams.triv-buffer-finish */
  PoolTrivTraceBegin,           /* design.mps.poolams.triv-trace-begin */
  AMSCondemn,                   /* condemn */
  PoolTrivGrey,                 /* design.mps.poolams.triv-grey */
  AMSScan,                      /* scan */
  AMSFix,                       /* fix */
  AMSReclaim,                   /* reclaim */
  PoolTrivTraceEnd,             /* design.mps.poolams.triv-trace-end */
  AMSBenefit,                   /* benefit */
  AMSDescribe,                  /* describe */
  PoolClassSig                  /* impl.h.mpm.class.end-sig */
};

static Bool AMSCheck(AMS ams)
{
  CHECKS(AMS, ams);
  CHECKD(Pool, AMSPool(ams));
  CHECKL(AMSPool(ams)->class == &PoolClassAMSStruct);
  CHECKD(Format, ams->format);
  CHECKL((1 << ams->grainShift) == AMSPool(ams)->alignment);
  CHECKD(Action, AMSAction(ams));
  CHECKL(ams->lastReclaimed <= ams->size);

  return TRUE;
}

/* mps_class_ams -- return the pool class descriptor to the client */

mps_class_t mps_class_ams(void)
{
  return (mps_class_t)&PoolClassAMSStruct;
}


/* impl.c.poolams: AUTOMATIC MARK & SWEEP POOL CLASS
 *
 * $HopeName: MMsrc!poolams.c(trunk.37) $
 * Copyright (C) 1998.  Harlequin Group plc.  All rights reserved.
 * 
 * .readership: any MPS developer.
 * 
 * .purpose: A canonical mark/sweep pool to be used as the basis for
 * other mark/sweep pools and for understanding the issues involved in
 * doing mark/sweep collection in the MPS framework.
 * 
 * .design: See design.mps.poolams.
 *
 * TRANSGRESSSIONS
 *
 * .no-check.local: We have decided to omit checks in local functions of
 * structure arguments that are simply passed down through the caller
 * (as opposed to being constructed by the caller).
 */

#include "mpscams.h"
#include "poolams.h"
#include "mpm.h"
#include <stdarg.h>

SRCID(poolams, "$HopeName: MMsrc!poolams.c(trunk.37) $");


#define AMSSig          ((Sig)0x519A3599) /* SIGnature AMS */
#define AMSGroupSig     ((Sig)0x519A359B) /* SIGnature AMS GrouP */


/* AMSGroupCheck -- check the group */

Bool AMSGroupCheck(AMSGroup group)
{
  CHECKS(AMSGroup, group);
  CHECKL(SegCheck(group->seg));
  CHECKL(AMSSegGroup(group->seg) == group);
  CHECKU(AMS, group->ams);
  CHECKL(AMSPool(group->ams) == SegPool(group->seg));
  CHECKL(RingCheck(&group->groupRing));

  CHECKL(group->grains == AMSGrains(group->ams, SegSize(group->seg)));
  CHECKL(group->grains > 0);
  CHECKL(group->grains >= group->free);

  if(SegWhite(group->seg) != TraceSetEMPTY)
    /* design.mps.poolams.colour.single */
    CHECKL(TraceSetIsSingle(SegWhite(group->seg)));

  CHECKL(BoolCheck(group->marksChanged));
  CHECKL(group->allocTable != NULL);
  CHECKL(group->nongreyTable != NULL);
  CHECKL(group->nonwhiteTable != NULL);

  return TRUE;
}


/* AMSGroupCreate -- create a single group
 *
 * .group.class: Eventually this will evolve into a more general group
 * subclassing mechanism, so I've split the AMS-specific initialization
 * into a separate function, AMSGroupInit.
 */

Res AMSGroupInit(AMSGroup group, Pool pool)
{
  Res res;
  Arena arena;
  AMS ams;
  Size size;

  /* group is in the process of being initialised, don't check it! */
  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  arena = PoolArena(pool);

  size = SegSize(group->seg);
  group->grains = size >> ams->grainShift;
  group->free = group->grains;
  group->marksChanged = FALSE; /* design.mps.poolams.marked.unused */
  group->ambiguousFixes = FALSE;

  res = BTCreate(&group->allocTable, arena, group->grains);
  if(res != ResOK)
    goto failAlloc;

  res = BTCreate(&group->nongreyTable, arena, group->grains);
  if(res != ResOK)
    goto failGrey;

  res = BTCreate(&group->nonwhiteTable, arena, group->grains);
  if(res != ResOK)
    goto failWhite;

  /* start off using firstFree, see design.mps.poolams.no-bit */
  group->allocTableInUse = FALSE;
  group->firstFree = 0;
  group->colourTablesInUse = FALSE;

  group->ams = ams;
  RingInit(&group->groupRing);
  RingAppend((ams->allocRing)(ams, SegRankSet(group->seg), size),
             &group->groupRing);

  group->sig = AMSGroupSig;
  ams->size += size;
  AVERT(AMSGroup, group);

  return ResOK;

  /* keep the destructions in step with AMSGroupFinish */
failWhite:
  BTDestroy(group->nongreyTable, arena, group->grains);
failGrey:
  BTDestroy(group->allocTable, arena, group->grains);
failAlloc:
  return res;
}


/* AMSSegSizePolicy
 *
 * Picks a segment size.  This policy simply rounds the size
 * up to the arena alignment. */
static Res AMSSegSizePolicy(Size *sizeReturn,
                            Pool pool, Size size, RankSet rankSet)
{
  Arena arena;

  AVER(sizeReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVER(RankSetCheck(rankSet));

  arena = PoolArena(pool);

  size = SizeAlignUp(size, ArenaAlign(arena));
  if(size == 0) {
    /* overflow */
    return ResMEMORY;
  }
  *sizeReturn = size;
  return ResOK;
}

static Res AMSGroupCreate(AMSGroup *groupReturn, Pool pool, Size size,
                          SegPref segPref, RankSet rankSet,
                          Bool withReservoirPermit)
{
  AMSGroup group;
  AMS ams;
  Res res;
  Arena arena;
  Seg seg;
  void *p;                      /* for allocating the group structure */

  AVER(groupReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(RankSet, rankSet);
  AVERT(SegPref, segPref);
  AVER(BoolCheck(withReservoirPermit));

  ams = PoolPoolAMS(pool);
  AVERT(AMS,ams);
  arena = PoolArena(pool);

  res = ams->segSize(&size, pool, size, rankSet);
  if(res != ResOK)
    goto failSize;

  res = ArenaAlloc(&p, arena, ams->groupSize);
  if(res != ResOK)
    goto failGroup;
  /* p is the address of the subclass-specific group structure. */
  /* We calculate the address of the generic group structure within the */
  /* instance by using the offset information from the class. */
  group = (AMSGroup)PointerAdd(p, ams->groupOffset);

  res = SegAlloc(&seg, segPref, size, pool, withReservoirPermit);
  if(res != ResOK)
    goto failSeg;

  group->seg = seg;
  SegSetP(seg, (void*)group);
  /* see design.mps.seg.field.rankset */
  if(rankSet != RankSetEMPTY) {
    SegSetRankAndSummary(seg, rankSet, RefSetUNIV);
  } else {
    SegSetRankAndSummary(seg, rankSet, RefSetEMPTY);
  }

  /* Do class-specific initialization. */
  res = (*ams->groupInit)(group, pool);
  if(res != ResOK)
    goto failInit;

  *groupReturn = group;
  return ResOK;

  /* keep the destructions in step with AMSGroupDestroy */
failInit:
  SegFree(seg);
failSeg:
  ArenaFree(arena, group, ams->groupSize);
failGroup:
failSize:
  return res;
}


/* AMSGroupDestroy -- destroy a single group */

void AMSGroupFinish(AMSGroup group)
{
  AMS ams;
  Arena arena;

  AVERT(AMSGroup, group);
  ams = group->ams;
  AVERT(AMS, ams);
  arena = PoolArena(AMSPool(ams));

  /* keep the destructions in step with AMSGroupInit failure cases */
  BTDestroy(group->nonwhiteTable, arena, group->grains);
  BTDestroy(group->nongreyTable, arena, group->grains);
  BTDestroy(group->allocTable, arena, group->grains);
}

void AMSGroupDestroy(AMSGroup group)
{
  AMS ams;
  Arena arena;

  AVERT(AMSGroup, group);
  ams = group->ams;
  AVERT(AMS, ams);
  arena = PoolArena(AMSPool(ams));

  AVER(SegBuffer(group->seg) == NULL);

  (*ams->groupFinish)(group);

  RingRemove(&group->groupRing);
  RingFinish(&group->groupRing);

  AVER(ams->size >= SegSize(group->seg));
  ams->size -= SegSize(group->seg);

  group->sig = SigInvalid;

  /* keep the destructions in step with AMSGroupCreate failure cases */
  SegFree(group->seg);
  ArenaFree(arena, group, ams->groupSize);
}  


/* AMSPoolRing -- the ring of groups in the pool */

static Ring AMSPoolRing(AMS ams, RankSet rankSet, Size size)
{
  /* arguments checked in the caller */
  UNUSED(rankSet); UNUSED(size);
  return &ams->groupRing;
}


/* AMSGroupsDestroy -- destroy all the groups */

static void AMSGroupsDestroy(AMS ams)
{
  Ring ring, node;              /* for iterating over the segments */

  ring = PoolSegRing(AMSPool(ams));
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Seg seg = SegOfPoolRing(node);
    AMSGroup group = (AMSGroup)SegP(seg);

    AVER(group->ams == ams);
    AMSGroupDestroy(group);
    node = next;
  }
}


static Res AMSIterate(AMSGroup group,
                      AMSObjectFunction f, void *closure);


/* AMSInit -- the pool class initialization method
 * 
 *  Takes one additional argument: the format of the objects
 *  allocated in the pool.  See design.mps.poolams.init.
 */

Res AMSInit(Pool pool, va_list arg)
{
  AMS ams;

  AVERT(Pool, pool);

  ams = PoolPoolAMS(pool);
  pool->format = va_arg(arg, Format);
  AVERT(Format, pool->format);

  pool->alignment = pool->format->alignment;
  ams->grainShift = SizeLog2(PoolAlignment(pool));
  ActionInit(AMSAction(ams), pool);
  RingInit(&ams->groupRing);

  /* The next seven might be overridden by a subclass. */
  ams->iterate = AMSIterate; /* should be done using a format variant */
  ams->segSize = AMSSegSizePolicy;
  ams->allocRing = AMSPoolRing;
  ams->groupsDestroy = AMSGroupsDestroy;
  ams->groupSize = sizeof(AMSGroupStruct);
  ams->groupOffset = (size_t)0;
  ams->groupInit = AMSGroupInit;
  ams->groupFinish = AMSGroupFinish;

  ams->size = 0;
  ams->lastReclaimed = 0;

  ams->sig = AMSSig;
  AVERT(AMS, ams);

  return ResOK;
}


/* AMSFinish -- the pool class finishing method
 * 
 * Destroys all the groups in the pool.  Can't invalidate the AMS until
 * we've destroyed all the groups, as it may be checked.
 */

void AMSFinish(Pool pool)
{
  AMS ams;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  (ams->groupsDestroy)(ams);
  ActionFinish(AMSAction(ams));
  /* can't invalidate the AMS until we've destroyed all the groups */
  ams->sig = SigInvalid;
}


/* AMSGroupAlloc -- try to allocate an area in the given group
 * 
 * Tries to find an area of at least the given size.  If successful,
 * makes that area black, if necessary, and returns its base and limit
 * grain indices.
 */

static Bool AMSGroupAlloc(Index *baseReturn, Index *limitReturn,
                          AMSGroup group, Size size)
{
  AMS ams;
  Size grains;
  Bool canAlloc;      /* can we allocate in this group? */
  Index base, limit;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  /* group has already been checked, in AMSBufferFill. */

  ams = group->ams;
  AVERT(AMS, ams);

  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(AMSPool(ams))));

  grains = AMSGrains(ams, size);
  AVER(grains > 0);
  if(grains > group->grains)
    return FALSE;

  if(group->allocTableInUse) {
    canAlloc = BTFindLongResRange(&base, &limit, group->allocTable,
                                  0, group->grains, grains);
    if(!canAlloc)
      return FALSE;
    BTSetRange(group->allocTable, base, limit);
  } else {
    if(group->firstFree > group->grains - grains)
      return FALSE;
    base = group->firstFree; limit = group->grains;
    group->firstFree = limit;
  }

  group->free -= limit - base;
  *baseReturn = base;
  *limitReturn = limit;
  return TRUE;
}


/* AMSBufferInit -- the buffer init method
 *
 * This just sets rankSet.  See design.mps.poolams.buffer-init.
 */

Res AMSBufferInit(Pool pool, Buffer buffer, va_list args)
{
  Rank rank = va_arg(args, Rank);

  AVERT(Pool, pool);
  AVERT(AMS, PoolPoolAMS(pool));
  AVERT(Rank, rank);

  buffer->rankSet = RankSetSingle(rank);
  return ResOK;
}


/* AMSBufferFill -- the pool class buffer fill method
 * 
 * Iterates over the segments looking for space.  See
 * design.mps.poolams.fill.
 */

Res AMSBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                  Pool pool, Buffer buffer, Size size,
                  Bool withReservoirPermit)
{
  Res res;
  AMS ams;
  AMSGroup group;
  Ring node, ring, nextNode;    /* for iterating over the segments */
  Index base, limit;
  RankSet rankSet;
  Bool b;                       /* the return value of AMSGroupAlloc */
  SegPrefStruct segPrefStruct;

  AVER(segReturn != NULL);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, PoolAlignment(pool)));
  AVER(BoolCheck(withReservoirPermit));

  /* Check that we're not in the grey mutator phase (see */
  /* design.mps.poolams.fill.colour). */
  AVER(PoolArena(pool)->busyTraces == PoolArena(pool)->flippedTraces);

  rankSet = BufferRankSet(buffer);
  ring = (ams->allocRing)(ams, rankSet, size);
  /* design.mps.poolams.fill.slow */
  RING_FOR(node, ring, nextNode) {
    group = RING_ELT(AMSGroup, groupRing, node);
    AVERT_CRITICAL(AMSGroup, group);
    if(group->free >= AMSGrains(ams, size)) {
      Seg seg = group->seg;

      if(SegRankSet(seg) == rankSet && SegBuffer(seg) == NULL) {
        b = AMSGroupAlloc(&base, &limit, group, size);
        if(b)
          goto found;
      }
    }
  }

  /* no group has enough room; make a new group */
  segPrefStruct = *SegPrefDefault();
  SegPrefExpress(&segPrefStruct, SegPrefCollected, NULL);
  res = AMSGroupCreate(&group, pool, size, &segPrefStruct, rankSet,
                       withReservoirPermit);
  if(res != ResOK)
    return res;
  b = AMSGroupAlloc(&base, &limit, group, size);

found:
  AVER(b);
  *segReturn = group->seg;
  *baseReturn = AMS_INDEX_ADDR(group, base);
  *limitReturn = AMS_INDEX_ADDR(group, limit);
  return ResOK;
}


/* AMSBufferEmpty -- the pool class buffer empty method
 * 
 * Frees the unused part of the buffer.  The colour of the area doesn't
 * need to be changed.  See design.mps.poolams.empty.
 */

void AMSBufferEmpty(Pool pool, Buffer buffer, Seg seg)
{
  AMS ams;
  Addr init, limit;
  Index initIndex, limitIndex;
  AMSGroup group;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  AVERT(Buffer,buffer);
  AVER(BufferIsReady(buffer));
  AVER(SegCheck(seg));

  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  AVER(group->seg == seg);

  init = BufferGetInit(buffer);
  limit = BufferLimit(buffer);

  AVER(AddrIsAligned(init, PoolAlignment(pool)));
  AVER(AddrIsAligned(limit, PoolAlignment(pool)));

  if(init == limit)
    return;

  initIndex = AMS_ADDR_INDEX(group, init);
  limitIndex = AMS_ADDR_INDEX(group, limit);

  if(group->allocTableInUse) {
    /* check that it's allocated */
    AVER(BTIsSetRange(group->allocTable, initIndex, limitIndex));
    BTResRange(group->allocTable, initIndex, limitIndex);
  } else {
    /* check that it's allocated */
    AVER(limitIndex <= group->firstFree);
    if(limitIndex == group->firstFree) /* is it at the end? */ {
      group->firstFree = initIndex;
    } else { /* start using allocTable */
      group->allocTableInUse = TRUE;
      BTSetRange(group->allocTable, 0, group->firstFree);
      if(group->firstFree < group->grains)
        BTResRange(group->allocTable, group->firstFree, group->grains);
      BTResRange(group->allocTable, initIndex, limitIndex);
    }
  }
  group->free += limitIndex - initIndex;
}


/* AMSRangeCondemn -- Condemn a part of a group
 * 
 * I.e., alloc -> white, free -> black.
 * Allow calling it with base = limit, to simplify the callers.
 */

static void AMSRangeCondemn(AMSGroup group, Index base, Index limit)
{
  if(base != limit) {
    AVER(base < limit);
    AVER(limit <= group->grains);

    if(group->allocTableInUse) {
      BTSetRange(group->nongreyTable, base, limit);
      BTCopyInvertRange(group->allocTable, group->nonwhiteTable,
                        base, limit);
    } else {
      if(base < group->firstFree) {
        AMSRangeWhiten(group, base, group->firstFree);
      }
      if(group->firstFree < limit) {
        AMSRangeBlacken(group, group->firstFree, limit);
      }
    }
  }
}


/* AMSWhiten -- the pool class segment condemning method */

Res AMSWhiten(Pool pool, Trace trace, Seg seg)
{
  AMS ams;
  AMSGroup group;
  Buffer buffer;                /* the seg's buffer, if it has one */

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  AVERT(Trace, trace);
  AVER(SegCheck(seg));

  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);
  AVER(group->ams == ams);

  /* design.mps.poolams.colour.single */
  AVER(SegWhite(seg) == TraceSetEMPTY);
  AVER(!group->colourTablesInUse);

  group->colourTablesInUse = TRUE;
  buffer = SegBuffer(seg);
  if(buffer != NULL) { /* design.mps.poolams.condemn.buffer */
    Index scanLimitIndex, limitIndex;
    scanLimitIndex = AMS_ADDR_INDEX(group, BufferScanLimit(buffer));
    limitIndex = AMS_ADDR_INDEX(group, BufferLimit(buffer));

    AMSRangeCondemn(group, 0, scanLimitIndex);
    if(scanLimitIndex < limitIndex)
      AMSRangeBlacken(group, scanLimitIndex, limitIndex);
    AMSRangeCondemn(group, limitIndex, group->grains);
    /* We didn't condemn the buffer, subtract it from the count. */
    trace->condemned -= AddrOffset(BufferScanLimit(buffer),
                                   BufferLimit(buffer));
  } else { /* condemn whole seg */
    AMSRangeCondemn(group, 0, group->grains);
  }

  trace->condemned += SegSize(seg);
  group->marksChanged = FALSE; /* design.mps.poolams.marked.condemn */
  group->ambiguousFixes = FALSE;

  /* d.m.p.condemn.white [can't locate this tag] */
  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace->ti));

  return ResOK;
}



/* AMSIterate -- applies a function to each object in a group
 * 
 * AMSIterate(ams, group, seg, arena, f, closure) applies f to all the
 * objects in the group.  It skips the buffer, if any (from
 * BufferScanLimit to BufferLimit).
 */

static Res AMSIterate(AMSGroup group,
                      AMSObjectFunction f, void *closure)
{
  Res res;
  AMS ams;
  Format format;
  Align alignment;
  Seg seg;
  Index i;
  Addr p, next, limit;
  Buffer buffer;

  AVERT(AMSGroup, group);
  AVERT(AMSObjectFunction, f);
  /* Can't check closure */

  ams = group->ams;
  AVERT(AMS, ams);
  format = AMSPool(ams)->format;
  AVERT(Format, format);
  alignment = PoolAlignment(AMSPool(ams));

  seg = group->seg;
  AVERT(Seg, seg);
  p = SegBase(seg);
  limit = SegLimit(seg);
  buffer = SegBuffer(seg);

  while (p < limit) { /* loop over the objects in the group */
    if(buffer != NULL
       && p == BufferScanLimit(buffer) && p != BufferLimit(buffer)) {
      /* skip buffer */
      next = BufferLimit(buffer); 
      AVER(AddrIsAligned(next, alignment));
    } else {
      AVER((buffer == NULL)
	   || (p < BufferScanLimit(buffer))
	   || (p >= BufferLimit(buffer)));  /* not in the buffer */

      i = AMS_ADDR_INDEX(group, p);
      if(!AMS_ALLOCED(group, i)) { /* no object here */
        next = AddrAdd(p, alignment); /* @@@@ this could be improved */
      } else { /* there is an object here */
        next = (*format->skip)(p);
        AVER(AddrIsAligned(next, alignment));
	res = (*f)(group, i, p, next, closure);
	if(res != ResOK)
	  return res;
      }
    }
    AVER(next > p); /* make sure we make progress */
    p = next;
  }
  AVER(p == limit);
  return ResOK;
}


/* AMSScanObject -- scan a single object
 * 
 * This is the object function passed to AMSIterate by AMSScan.
 */

struct AMSScanClosureStruct {
  ScanState ss;
  Bool scanAllObjects;
};

typedef struct AMSScanClosureStruct *AMSScanClosure;

static Res AMSScanObject(AMSGroup group,
			 Index i, Addr p, Addr next, void *clos)
{
  AMSScanClosure closure;
  Format format;
  Res res;

  /* group has already been checked, in AMSIterate. */
  AVER(i < group->grains);
  AVER(p != 0);
  AVER(p < next);
  AVER(clos != NULL);
  closure = clos;
  AVERT(ScanState, closure->ss);
  AVER(BoolCheck(closure->scanAllObjects));

  format = AMSPool(group->ams)->format;
  AVERT(Format, format);

  /* @@@@ This isn't quite right for multiple traces. */
  if(closure->scanAllObjects || AMSIsGrey(group, i)) {
    res = (*format->scan)(closure->ss, p, next);
    if(res != ResOK)
      return res;
    closure->ss->scannedSize += AddrOffset(p, next);
    if(!closure->scanAllObjects) {
      Index j = AMS_ADDR_INDEX(group, next);
      AVER(!AMSIsInvalidColor(group, i));
      AMSGreyBlacken(group, i);
      if(i+1 < j)
        AMSRangeWhiteBlacken(group, i+1, j);
    }
  }

  return ResOK;
}


/* AMSScan -- the pool class segment scanning method
 *
 * See design.mps.poolams.scan
 */

Res AMSScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  Res res;
  AMS ams;
  Arena arena;
  AMSGroup group;
  struct AMSScanClosureStruct closureStruct;
  Format format;
  Align alignment;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  arena = PoolArena(pool);
  AVER(SegCheck(seg));
  group = AMSSegGroup(seg);
  AVERT(AMSGroup, group);

  /* Check that we're not in the grey mutator phase (see */
  /* design.mps.poolams.not-req.grey). */
  AVER(TraceSetSub(ss->traces, arena->flippedTraces));

  closureStruct.scanAllObjects =
    (TraceSetDiff(ss->traces, SegWhite(seg)) != TraceSetEMPTY);
  closureStruct.ss = ss;
  /* @@@@ This isn't quite right for multiple traces. */
  if(closureStruct.scanAllObjects) {
    /* The whole seg (except the buffer) is grey for some trace. */
    res = (ams->iterate)(group, AMSScanObject, &closureStruct);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
    *totalReturn = TRUE;
  } else {
    AVER(group->marksChanged); /* something must have changed */
    AVER(group->colourTablesInUse);
    format = pool->format;
    AVERT(Format, format);
    alignment = PoolAlignment(AMSPool(ams));
    do { /* design.mps.poolams.scan.iter */
      group->marksChanged = FALSE; /* design.mps.poolams.marked.scan */
      /* design.mps.poolams.ambiguous.middle */
      if(group->ambiguousFixes) {
        res = (ams->iterate)(group, AMSScanObject, &closureStruct);
        if(res != ResOK) {
          /* design.mps.poolams.marked.scan.fail */
          group->marksChanged = TRUE;
          *totalReturn = FALSE;
          return res;
        }
      } else {
        Index i, j = 0;
        Addr p, next;

        while(j < group->grains
              && AMSFindGrey(&i, &j, group, j, group->grains)) {
          AVER(!AMSIsInvalidColor(group, i));
          p = AMS_INDEX_ADDR(group, i);
          next = (*format->skip)(p);
          AVER(AddrIsAligned(next, alignment));
          j = AMS_ADDR_INDEX(group, next);
          res = (*format->scan)(ss, p, next);
          if(res != ResOK) {
            /* design.mps.poolams.marked.scan.fail */
            group->marksChanged = TRUE;
            *totalReturn = FALSE;
            return res;
          }
          ss->scannedSize += AddrOffset(p, next);
          AMSGreyBlacken(group, i);
          if(i+1 < j)
            AMSRangeWhiteBlacken(group, i+1, j);
        }
      }
    } while(group->marksChanged);
    *totalReturn = FALSE;
  }

  return ResOK;
}


/* AMSFix -- the pool class fixing method */

Res AMSFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AMSGroup group;
  Index i;                      /* the index of the fixed grain */
  Ref ref;

  AVERT_CRITICAL(Pool, pool);
  AVER_CRITICAL(CHECKT(AMS, PoolPoolAMS(pool)));
  AVERT_CRITICAL(ScanState, ss);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(refIO != NULL);

  group = AMSSegGroup(seg);
  AVERT_CRITICAL(AMSGroup, group);
  /* It's a white seg, so it must have colour tables. */
  AVER_CRITICAL(group->colourTablesInUse);

  /* @@@@ We should check that we're not in the grey mutator phase */
  /* (see design.mps.poolams.not-req.grey), but there's no way of */
  /* doing that here (this can be called from RootScan, during flip). */

  ref = *refIO;
  i = AMS_ADDR_INDEX(group, ref);
  AVER_CRITICAL(!AMSIsInvalidColor(group, i));

  ss->wasMarked = TRUE;

  switch (ss->rank) {
  case RankAMBIG:
    /* not a real pointer if not aligned or not allocated */
    if(!AddrIsAligned((Addr)ref, PoolAlignment(pool))
       || !AMS_ALLOCED(group, i)) {
      break;
    }
    group->ambiguousFixes = TRUE;
    /* falls through */
  case RankEXACT:
  case RankFINAL:
  case RankWEAK:
    AVER_CRITICAL(AddrIsAligned((Addr)ref, PoolAlignment(pool)));
    AVER_CRITICAL(AMS_ALLOCED(group, i));
    if(AMSIsWhite(group, i)) {
      ss->wasMarked = FALSE;
      if(ss->rank == RankWEAK) { /* then splat the reference */
        *refIO = (Ref)0;
      } else {
        ++ss->preservedInPlaceCount; /* Size updated on reclaim */
        if(SegRankSet(seg) == RankSetEMPTY && ss->rank != RankAMBIG) {
          /* design.mps.poolams.fix.to-black */
          Addr next;

          next = (*pool->format->skip)(ref);
          /* Part of the object might be grey, because of ambiguous */
          /* fixes, but that's OK, because scan will ignore that. */
          AMSRangeWhiteBlacken(group, i, AMS_ADDR_INDEX(group, next));
        } else { /* turn it grey */
          AMSWhiteGreyen(group, i);
          SegSetGrey(seg, TraceSetUnion(SegGrey(seg), ss->traces));
          /* mark it for scanning - design.mps.poolams.marked.fix */
          group->marksChanged = TRUE;
        }
      }
    }
    break;
  default:
    NOTREACHED;
  }

  return ResOK;
}


/* AMSBlacken -- the pool class blackening method
 *
 * Turn all grey objects black.
 */

void AMSBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AMS ams;
  AMSGroup group;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);

  /* If it's white for any trace, remove the greyness from tables. */
  if(TraceSetInter(traceSet, SegWhite(seg)) != TraceSetEMPTY) {
    group = AMSSegGroup(seg);
    AVERT(AMSGroup, group);
    AVER(group->marksChanged); /* there must be something grey */
    group->marksChanged = FALSE;
    /* This will turn grey->black, and not affect black or white. */
    BTSetRange(group->nongreyTable, 0, group->grains);
  }
}


/* AMSReclaim -- the pool class reclamation method */

void AMSReclaim(Pool pool, Trace trace, Seg seg)
{
  AMS ams;
  AMSGroup group;
  Format format;
  Align alignment;
  Count reclaimed = 0;
  Index i, j = 0;
  Addr p, next;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);
  AVERT(Seg, seg);

  group = AMSSegGroup(seg);
  /* It's a white seg, so it must have colour tables. */
  AVER(group->colourTablesInUse);
  AVER(!group->marksChanged); /* there must be nothing grey */
  format = pool->format;
  AVERT(Format, format);
  alignment = PoolAlignment(AMSPool(ams));

  /* Start using allocTable */
  if(!group->allocTableInUse) {
    group->allocTableInUse = TRUE;
    if(0 < group->firstFree)
      BTSetRange(group->allocTable, 0, group->firstFree);
    if(group->firstFree < group->grains)
      BTResRange(group->allocTable, group->firstFree, group->grains);
  }

  /* Loop over all white objects and free them */
  while(j < group->grains
        && AMSFindWhite(&i, &j, group, j, group->grains)) {
    AVER(!AMSIsInvalidColor(group, i));
    p = AMS_INDEX_ADDR(group, i);
    next = (*format->skip)(p);
    AVER(AddrIsAligned(next, alignment));
    j = AMS_ADDR_INDEX(group, next);
    BTResRange(group->allocTable, i, j);
    reclaimed += j - i;
  }

  group->free += reclaimed;
  trace->reclaimSize += reclaimed << ams->grainShift;
  /* preservedInPlaceCount is updated on fix */
  trace->preservedInPlaceSize += 
    (group->grains - group->free) << ams->grainShift;

  if(group->free == group->grains && SegBuffer(seg) == NULL) {
    /* No survivors */
    AMSGroupDestroy(group);
    /* design.mps.poolams.benefit.guess */
    ams->lastReclaimed = ams->size;
  } else {
    group->colourTablesInUse = FALSE;
    SegSetWhite(seg, TraceSetDel(SegWhite(seg), trace->ti));
  }
}


/* AMSBenefit -- the pool class benefit computation method
 *
 * This does not compute a real benefit, but something which works
 * well enough to run tests.  See design.mps.poolams.benefit.guess.
 */

int AMSRatioDenominator = 1;
int AMSRatioNumerator = 2;
Size AMSMinimumCollectableSize = 128*(Size)1024;

static double AMSBenefit(Pool pool, Action action)
{
  AMS ams;
  double benefit;

  AVERT(Pool, pool);
  ams = PoolPoolAMS(pool);
  AVERT(AMS, ams);

  AVERT(Action, action);
  AVER(ams == ActionAMS(action));

  if((ams->size > AMSMinimumCollectableSize)
      && (ams->size * AMSRatioNumerator
          > ams->lastReclaimed * AMSRatioDenominator)) {
    /* design.mps.poolams.benefit.repeat */
    ams->lastReclaimed = ams->size; 
    benefit = 1.0;
  } else {
    benefit = 0.0;
  }
  return benefit;
}


/* AMSGroupDescribe -- describe an AMS group */

#define WRITE_BUFFER_LIMIT(stream, group, i, buffer, accessor, char) \
  BEGIN \
    if((buffer) != NULL \
       && (i) == AMS_ADDR_INDEX(group, accessor(buffer))) { \
      Res _res = WriteF(stream, char, NULL); \
      if(_res != ResOK) return _res; \
    } \
  END

Res AMSGroupDescribe(AMSGroup group, mps_lib_FILE *stream)
{
  Res res;
  Seg seg;
  Buffer buffer;               /* the segment's buffer, if it has one */
  Index i;

  if(!CHECKT(AMSGroup, group)) return ResFAIL;
  if(stream == NULL) return ResFAIL;

  seg = group->seg;
  buffer = SegBuffer(seg);

  res = WriteF(stream,
               "AMS Group $P {\n", (WriteFP)group,
               "  seg $P [$A,$A)\n", (WriteFP)seg,
               (WriteFA)AMSGroupBase(group),
               (WriteFA)AMSGroupLimit(group),
               "  AMS $P\n", (WriteFP)group->ams,
               "  grains $W\n", (WriteFW)group->grains,
               NULL);
  if(res != ResOK)
    return res;
  if(group->allocTableInUse)
    res = WriteF(stream,
                 "  alloctable $P\n", (WriteFP)group->allocTable,
                 NULL);
  else
    res = WriteF(stream,
                 "  firstFree $W\n", (WriteFW)group->firstFree,
                 NULL);
  if(res != ResOK)
    return res;
  res = WriteF(stream,
               "  tables: nongrey $P, nonwhite $P\n",
               (WriteFP)group->nongreyTable,
               (WriteFP)group->nonwhiteTable,
               "  map: \n",
               NULL);
  if(res != ResOK)
    return res;

  for (i=0; i < group->grains; ++i) {
    char c = 0;

    if(i % 64 == 0) {
      res = WriteF(stream, "\n  ", NULL);
      if(res != ResOK)
        return res;
    }

    WRITE_BUFFER_LIMIT(stream, group, i, buffer, BufferBase, "[");
    WRITE_BUFFER_LIMIT(stream, group, i, buffer, BufferGetInit, "|");
    WRITE_BUFFER_LIMIT(stream, group, i, buffer, BufferAlloc, ">");

    if(AMS_ALLOCED(group, i)) {
      if(group->colourTablesInUse) {
        if(AMSIsInvalidColor(group, i))
          c = '!';
        else if(AMSIsWhite(group, i))
          c = '-';
        else if(AMSIsGrey(group, i))
          c = '+';
        else /* must be black */
          c = '*';
      } else
        c = '.';
    } else
      c = ' ';
    res = WriteF(stream, "$C", c, NULL);
    if(res != ResOK)
      return res;

    WRITE_BUFFER_LIMIT(stream, group, i+1, buffer, BufferScanLimit, "<");
    WRITE_BUFFER_LIMIT(stream, group, i+1, buffer, BufferLimit, "]");
  }

  res = WriteF(stream, "\n} AMS Group $P\n", (WriteFP)group, NULL);
  return res;
}


/* AMSDescribe -- the pool class description method
 * 
 * Iterates over the segments, describing all of them.
 */

static Res AMSDescribe(Pool pool, mps_lib_FILE *stream)
{
  AMS ams;
  Ring node, nextNode;
  Res res;

  if(!CHECKT(Pool, pool)) return ResFAIL;
  ams = PoolPoolAMS(pool);
  if(!CHECKT(AMS, ams)) return ResFAIL;
  if(stream == NULL) return ResFAIL;

  res = WriteF(stream,
               (WriteFP)pool, (WriteFU)pool->serial,
               "  size $W, lastReclaimed $W\n",
               (WriteFW)ams->size, (WriteFW)ams->lastReclaimed,
               "  grain shift $U\n", (WriteFU)ams->grainShift,
               "  action $P ($U)\n",
               (WriteFP)AMSAction(ams), (WriteFU)AMSAction(ams)->serial,
               NULL);
  if(res != ResOK)
    return res;

  res = WriteF(stream,
               "  segments\n"
               "    * = black, + = grey, - = white, . = alloc, ! = bad\n"
               "    buffers: [ = base, < = scan limit, | = init,\n"
               "             > = alloc, ] = limit\n",
               NULL);
  if(res != ResOK)
    return res;

  RING_FOR(node, &ams->groupRing, nextNode) {
    AMSGroup group = RING_ELT(AMSGroup, groupRing, node);
    res = AMSGroupDescribe(group, stream);
    if(res != ResOK)
      return res;
  }
  return ResOK;
}


/* AMSPoolClass -- the class definition */

/* impl.h.poolams contains the type definition. Hence the use */
/* of DEFINE_CLASS rather than DEFINE_POOL_CLASS */

DEFINE_CLASS(AMSPoolClass, this)
{
  INHERIT_CLASS(this, AbstractCollectPoolClass);
  PoolClassMixInFormat(this);
  this->name = "AMS";
  this->size = sizeof(AMSStruct);
  this->offset = offsetof(AMSStruct, poolStruct);
  this->init = AMSInit;
  this->finish = AMSFinish;
  this->bufferInit = AMSBufferInit;
  this->bufferFill = AMSBufferFill;
  this->bufferEmpty = AMSBufferEmpty;
  this->whiten = AMSWhiten;
  this->blacken = AMSBlacken;
  this->scan = AMSScan;
  this->fix = AMSFix;
  this->fixEmergency = AMSFix;
  this->reclaim = AMSReclaim;
  this->benefit = AMSBenefit;
  this->describe = AMSDescribe;
}

/* AMSCheck -- the check method for an AMS */

Bool AMSCheck(AMS ams)
{
  CHECKS(AMS, ams);
  CHECKD(Pool, AMSPool(ams));
  CHECKL(IsSubclass(AMSPool(ams)->class, EnsureAMSPoolClass()));
  CHECKL(PoolAlignment(AMSPool(ams)) == ((Size)1 << ams->grainShift));
  CHECKL(PoolAlignment(AMSPool(ams)) == AMSPool(ams)->format->alignment);
  CHECKD(Action, AMSAction(ams));
  CHECKL(AMSAction(ams)->pool == AMSPool(ams));
  CHECKL(SizeIsAligned(ams->size, ArenaAlign(PoolArena(AMSPool(ams)))));
  CHECKL(SizeIsAligned(ams->lastReclaimed,
		       ArenaAlign(PoolArena(AMSPool(ams)))));
  CHECKL(ams->iterate != NULL);
  CHECKL(RingCheck(&ams->groupRing));
  CHECKL(ams->allocRing != NULL);
  CHECKL(ams->groupsDestroy != NULL);
  /* Nothing to check about groupSize and groupOffset. */
  CHECKL(ams->groupInit != NULL);
  CHECKL(ams->groupFinish != NULL);

  return TRUE;
}


/* mps_class_ams -- return the pool class descriptor to the client */

mps_class_t mps_class_ams(void)
{
  return (mps_class_t)EnsureAMSPoolClass();
}

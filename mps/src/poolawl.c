/* impl.c.poolawl: AUTOMATIC WEAK LINKED POOL CLASS
 *
 * $HopeName: MMsrc!poolawl.c(trunk.8) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 *
 * DESIGN
 *
 * .design: design.mps.poolawl
 */


#include "mpm.h"
#include "mpscawl.h"

SRCID(poolawl, "$HopeName: MMsrc!poolawl.c(trunk.8) $");


#define AWLSig	((Sig)0x519b7a37)	/* SIGPooLAWL */

/* design.mps.poolawl.poolstruct */
typedef struct AWLStruct {
  PoolStruct poolStruct;
  Format format;
  Shift alignShift;
  ActionStruct actionStruct;
  double lastCollected;
  Sig sig;
} AWLStruct, *AWL;

#define AWLGroupSig ((Sig)0x519a379b)	/* SIGAWLGrouP */

/* design.mps.poolawl.group */
typedef struct AWLGroupStruct {
  Sig sig;
  Seg seg;
  BT mark;
  BT scanned;
  BT alloc;
} AWLGroupStruct, *AWLGroup;


static Bool AWLCheck(AWL awl);
static Bool AWLGroupCheck(AWLGroup group);


/* PoolPoolAWL -- convert generic Pool to AWL */

#define PoolPoolAWL(pool) \
  PARENT(AWLStruct, poolStruct, (pool))

#define ActionAWL(action)	PARENT(AWLStruct, actionStruct, action)


static void AWLGroupDestroy(AWLGroup group)
{
  AWL awl;
  Pool pool;
  Seg seg;
  Size tableSize;
  Space space;

  seg = group->seg;
  pool = SegPool(seg);
  AVERT(Pool, pool);
  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  space = PoolSpace(pool);
  AVERT(Space, space);
  tableSize = BTSize(SegSize(space, seg) >> awl->alignShift);
  PoolSegFree(pool, seg);
  SpaceFree(space, (Addr)group->mark, tableSize);
  SpaceFree(space, (Addr)group->scanned, tableSize);
  SpaceFree(space, (Addr)group->alloc, tableSize);
  group->sig = SigInvalid;
  SpaceFree(space, (Addr)group, sizeof *group);
}
  
 
static Res AWLGroupCreate(AWLGroup *groupReturn,
                          Buffer buffer, Pool pool, Size size)
{
  AWL awl;
  Seg seg;
  AWLGroup group;
  void *v;
  Count bits;	/* number of grains */
  Res res;
  Size tableSize;
  Space space;

  AVER(groupReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);

  space = PoolSpace(pool);
  AVERT(Space, space);

  size = SizeAlignUp(size, ArenaAlign(space));
  /* beware of large sizes overflowing upon rounding */
  if(size == 0) {
    return ResMEMORY;
  }
  res = PoolSegAlloc(&seg, SegPrefDefault(), pool, size);
  if(res != ResOK)
    goto failSegAlloc;
  res = SpaceAlloc(&v, space, sizeof *group);
  if(res != ResOK)
    goto failSpaceAlloc0;
  group = v;
  bits = size >> awl->alignShift;
  tableSize = BTSize(bits);
  res = SpaceAlloc(&v, space, tableSize);
  if(res != ResOK)
    goto failSpaceAlloc1;
  group->mark = v;
  res = SpaceAlloc(&v, space, tableSize);
  if(res != ResOK)
    goto failSpaceAlloc2;
  group->scanned = v;
  res = SpaceAlloc(&v, space, tableSize);
  if(res != ResOK)
    goto failSpaceAlloc3;
  group->alloc = v;
  BTResRange(group->mark, 0, bits);
  BTResRange(group->scanned, 0, bits);
  BTResRange(group->alloc, 0, bits);
  SegSetSummary(seg, RefSetUNIV);
  SegSetRankSet(seg, BufferRankSet(buffer));
  SegSetP(seg, group);
  group->seg = seg;
  group->sig = AWLGroupSig;
  AVERT(AWLGroup, group);
  *groupReturn = group;
  return ResOK;

failSpaceAlloc3:
  SpaceFree(space, (Addr)group->scanned, tableSize);
failSpaceAlloc2:
  SpaceFree(space, (Addr)group->mark, tableSize);
failSpaceAlloc1:
  SpaceFree(space, (Addr)group, sizeof *group);
failSpaceAlloc0:
  PoolSegFree(pool, seg);
failSegAlloc:
  return res;
}


static Bool AWLGroupAlloc(Addr *baseReturn, Addr *limitReturn,
                          AWLGroup group, AWL awl, Size size)
{
  Count n, bits;
  Index i, j;
  Space space;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(AWLGroup, group);
  AVERT(AWL, awl);
  AVER(size > 0);

  space = PoolSpace(&awl->poolStruct);
  AVERT(Space, space);


  if(size > SegSize(space, group->seg)) {
    return FALSE;
  }
  bits = SegSize(space, group->seg) >> awl->alignShift;
  n = size >> awl->alignShift;
  if(BTFindLongResRange(&i, &j, group->alloc, 0, bits, n)) {
    *baseReturn = AddrAdd(SegBase(space, group->seg), i << awl->alignShift);
    *limitReturn = AddrAdd(SegBase(space, group->seg), j << awl->alignShift);
    return TRUE;
  }
  return FALSE;
}


static Res AWLInit(Pool pool, va_list arg)
{
  AWL awl;
  Format format;

  awl = PoolPoolAWL(pool);

  format = va_arg(arg, Format);

  AVERT(Format, format);
  awl->format = format;
  awl->alignShift = SizeLog2(pool->alignment);
  ActionInit(&awl->actionStruct, pool);
  awl->lastCollected = PoolSpace(pool)->allocTime;
  awl->sig = AWLSig;

  AVERT(AWL, awl);

  return ResOK;
}


static void AWLFinish(Pool pool)
{
  AWL awl;
  Ring ring, node;

  /* pool argument already checked by generic PoolFinish */

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);

  ring = &pool->segRing;
  node = RingNext(ring);
  while(node != ring) {
    Ring next = RingNext(node);
    Seg seg = SegOfPoolRing(node);
    AWLGroup group;

    AVERT(Seg, seg);
    group = (AWLGroup)SegP(seg);
    AVERT(AWLGroup, group);
    AWLGroupDestroy(group);
    node = next;
  }
  ActionFinish(&awl->actionStruct);
}


static Res AWLBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
{
  Addr base, limit;
  AWLGroup group;
  AWL awl;
  Res res;
  Ring node;
  Space space;

  AVER(baseReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);

  space = PoolSpace(pool);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);

  RING_FOR(node, &pool->segRing) {
    Seg seg;

    seg = SegOfPoolRing(node);
    AVERT(Seg, seg);
    group = (AWLGroup)SegP(seg);
    AVERT(AWLGroup, group);

    if(AWLGroupAlloc(&base, &limit, group, awl, size))
      goto found;
  }

  /* No free space in existing groups, so create new group */

  res = AWLGroupCreate(&group, buffer, pool, size);
  if(res != ResOK) {
    return res;
  }
  base = SegBase(space, group->seg);
  limit = SegLimit(space, group->seg);

found:
  {
    Index i, j;
    i = AddrOffset(SegBase(space, group->seg), base) >> awl->alignShift;
    j = AddrOffset(SegBase(space, group->seg), limit) >> awl->alignShift;
    BTSetRange(group->alloc, i, j);
  }
  *segReturn = group->seg;
  *baseReturn = base;
  *limitReturn = limit;
  return ResOK;
}


static void AWLBufferEmpty(Pool pool, Buffer buffer)
{
  AWL awl;
  AWLGroup group;
  Addr segBase;
  Index i, j;

  AVERT(Pool, pool);
  AVERT(Buffer, buffer);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  group = (AWLGroup)SegP(BufferSeg(buffer));
  AVERT(AWLGroup, group);

  segBase = SegBase(PoolSpace(pool), BufferSeg(buffer));

  i = AddrOffset(segBase, BufferGetInit(buffer)) >> awl->alignShift;
  j = AddrOffset(segBase, BufferLimit(buffer)) >> awl->alignShift;
  AVER(i <= j);
  if(i < j) {
    BTResRange(group->alloc, i, j);
  }
}


static Res AWLCondemn(Pool pool, Trace trace, Seg seg, Action action)
{
  Count bits;
  AWL awl;
  AWLGroup group;

  /* all parameters checked by generic PoolCondemn */

  /* can only condemn for a single trace, */
  /* see design.mps.poolawl.fun.condemn */
  AVER(SegWhite(seg) == TraceSetEMPTY);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);

  AVERT(Action, action);
  AVER(awl == ActionAWL(action));

  group = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);
  bits = SegSize(PoolSpace(pool), seg) >> awl->alignShift;
  
  BTResRange(group->mark, 0, bits);
  BTResRange(group->scanned, 0, bits);
  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace->ti));
  
  return ResOK;
}

static void AWLGrey(Pool pool, Trace trace, Seg seg)
{
  /* parameters checkd by generic PoolGrey */
  if(!TraceSetIsMember(SegWhite(seg), trace->ti)) {
    AWL awl;
    AWLGroup group;
    Count bits;

    awl = PoolPoolAWL(pool);
    AVERT(AWL, awl);
    group = (AWLGroup)SegP(seg);
    AVERT(AWLGroup, group);

    SegSetGrey(seg, TraceSetAdd(SegGrey(seg), trace->ti));
    ShieldRaise(trace->space, seg, AccessREAD);
    bits = SegSize(PoolSpace(pool), seg) >> awl->alignShift;
    BTSetRange(group->mark, 0, bits);
    BTResRange(group->scanned, 0, bits);
  }
}


/* Returns the linked object (or possibly there is none) */
static Bool AWLDependentObject(Addr *objReturn, Addr parent)
{
  Word *object;
  Word *wrapper;
  Word fword;
  Word fl;
  Word ff;

  AVER(objReturn != NULL);
  AVER(parent != (Addr)0);

  object = (Word *)parent;
  wrapper = (Word *)object[0];
  AVER(wrapper != NULL);
  /* check wrapper wrapper is non-NULL */
  AVER(wrapper[0] != 0);
  /* check wrapper wrapper is wrapper wrapper wrapper */
  AVER(wrapper[0] == ((Word *)wrapper[0])[0]);
  fword = wrapper[2];
  ff = fword & 3;
  /* Traceable Fixed part */
  AVER(ff == 1);
  fl = fword & ~3uL;
  /* At least one fixed field */
  AVER(fl >= 1);
  if(object[1] == 0) {
    return FALSE;
  }
  *objReturn = (Addr)object[1];
  return TRUE;
}


static Res AWLScan(ScanState ss, Pool pool, Seg seg)
{
  Addr base, limit;
  Addr p;
  AWL awl;
  AWLGroup group;
  Bool finished;
  Count bits;
  Space space;

  /* parameters checked by generic PoolScan */

  group = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);

  space = PoolSpace(pool);
  bits = SegSize(space, seg) >> awl->alignShift;
  base = SegBase(space, seg);
  limit = SegLimit(space, seg);

notFinished:
  finished = TRUE;
  p = base;
  while(p < limit) {
    Index i;	/* the index into the bit tables corresponding to p */
    Addr objectEnd;
    /* design.mps.poolawl.fun.scan.buffer */
    if(SegBuffer(seg)) {
      if(p == BufferScanLimit(SegBuffer(seg))) {
	p = BufferLimit(SegBuffer(seg));
	continue;
      }
    }
    i = AddrOffset(base, p) >> awl->alignShift;
    /* design.mps.poolawl.fun.scan.free */
    if(!BTGet(group->alloc, i)) {
      p = AddrAdd(p, pool->alignment);
      continue;
    }
    /* design.mps.poolawl.fun.scan.object-end */
    objectEnd = awl->format->skip(p);
    /* design.mps.poolawl.fun.scan.scan */
    if(BTGet(group->mark, i) && !BTGet(group->scanned, i)) {
      Addr dependentObj;
      Seg dependentSeg;
      Bool dependent;
      Res res;

      finished = FALSE;
      BTSet(group->scanned, i);
      /* is there a dependent object that needs exposing? */
      dependent = AWLDependentObject(&dependentObj, p);
      if(dependent) {
	Bool b;

	b = SegOfAddr(&dependentSeg, space, dependentObj);
	AVER(b == TRUE);
	ShieldExpose(space, dependentSeg);
	TraceSetSummary(space, dependentSeg, RefSetUNIV);
      }
      res = awl->format->scan(ss, p, objectEnd);
      if(dependent) {
        ShieldCover(space, dependentSeg);
      }
      if(res != ResOK) {
        return res;
      }
    }
    p = AddrAlignUp(objectEnd, pool->alignment);
  }
  if(!finished)
    goto notFinished;
  
  return ResOK;
}


static Res AWLFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  Ref ref;
  Index i;
  AWL awl;
  AWLGroup group;
  Space space;

  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);
  AVER(refIO != NULL);

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  group  = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);

  space = PoolSpace(pool);
  AVERT(Space, space);

  ref = *refIO;
  i = AddrOffset(SegBase(space, seg), ref) >> awl->alignShift;
  
  ss->wasMarked = TRUE;

  switch(ss->rank) {
  case RankAMBIG:
    /* not a real pointer if not aligned or not allocated */
    if(!AddrIsAligned((Addr)ref, pool->alignment) ||
       !BTGet(group->alloc, i)) {
      return ResOK;
    }
  /* falls through */
  case RankEXACT:
  case RankFINAL:
  case RankWEAK:
    if(!BTGet(group->mark, i)) {
      ss->wasMarked = FALSE;
      if(ss->rank == RankWEAK) {
	*refIO = (Ref)0;
      } else {
	BTSet(group->mark, i);
	TraceSegGreyen(space, seg, ss->traces);
      }
    }
    break;
  
  default:
    NOTREACHED;
    return ResUNIMPL;
  }

  return ResOK;
}


static void AWLReclaim(Pool pool, Trace trace, Seg seg)
{
  AWL awl;
  AWLGroup group;
  Count bits;
  Index i;
  Space space;

  /* parameters checked by generic PoolReclaim */

  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  group = (AWLGroup)SegP(seg);
  AVERT(AWLGroup, group);

  space = PoolSpace(pool);
  AVERT(Space, space);

  bits = SegSize(space, seg) >> awl->alignShift;

  i = 0;
  while(i < bits) {
    Addr p;
    Index j;
    if(!BTGet(group->alloc, i)) {
      ++i;
      continue;
    }
    p = AddrAdd(SegBase(space, seg), i << awl->alignShift);
    j = AddrOffset(SegBase(space, seg), awl->format->skip(p)) >>
        awl->alignShift;
    AVER(j <= bits);
    if(!BTGet(group->mark, i)) {
      BTResRange(group->alloc, i, j);
    }
    i = j;
  }
  AVER(i == bits);

  BTResRange(group->mark, 0, bits);
}

static Res AWLTraceBegin(Pool pool, Trace trace, Action action)
{
  AWL awl;

  AVERT(Pool, pool);
  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  AVERT(Trace, trace);
  AVERT(Action, action);
  AVER(awl == ActionAWL(action));

  awl->lastCollected = PoolSpace(pool)->allocTime;
  return ResOK;
}

/* @@@@ completely made-up benefit calculation: each AWL pool gradually
 * becomes a better candidate for collection as allocation goes
 * by. Starting a trace on a pool makes it a bad candidate. nickb
 * 1997-06-19 */

static double AWLBenefit(Pool pool, Action action)
{
  AWL awl;

  AVERT(Pool, pool);
  awl = PoolPoolAWL(pool);
  AVERT(AWL, awl);
  AVERT(Action, action);
  AVER(awl == ActionAWL(action));

  return (PoolSpace(pool)->allocTime - awl->lastCollected) - 10*1024*1024.0;
}


struct PoolClassStruct PoolClassAWLStruct = {
  PoolClassSig,
  "AWL",
  sizeof(AWLStruct),
  offsetof(AWLStruct, poolStruct),
  AttrFMT | AttrSCAN | AttrBUF | AttrBUF_RESERVE | AttrGC | AttrINCR_RB,
  AWLInit,
  AWLFinish,
  PoolNoAlloc,
  PoolNoFree,
  PoolTrivBufferInit,
  AWLBufferFill,
  AWLBufferEmpty,
  PoolTrivBufferFinish,
  AWLTraceBegin,
  AWLCondemn,
  AWLGrey,
  AWLScan,
  AWLFix,
  AWLReclaim,
  PoolTrivTraceEnd,
  AWLBenefit,
  PoolTrivDescribe,
  PoolClassSig
};


mps_class_t mps_class_awl(void)
{
  return (mps_class_t)&PoolClassAWLStruct;
}


static Bool AWLCheck(AWL awl)
{
  CHECKS(AWL, awl);
  CHECKD(Pool, &awl->poolStruct);
  CHECKL(awl->poolStruct.class == &PoolClassAWLStruct);
  CHECKL(1uL << awl->alignShift == awl->poolStruct.alignment);
  CHECKD(Action, &awl->actionStruct);
  CHECKL(awl->poolStruct.space->allocTime >= awl->lastCollected);
  return TRUE;
}


static Bool AWLGroupCheck(AWLGroup group)
{
  CHECKS(AWLGroup, group);
  CHECKL(SegCheck(group->seg));
  CHECKL(group->mark != NULL);
  CHECKL(group->scanned != NULL);
  CHECKL(group->alloc != NULL);
  return TRUE;
}

/* impl.c.poolmrg
 * 
 * MANUAL RANK GUARDIAN POOL
 * 
 * $HopeName: MMsrc!poolmrg.c(trunk.2) $
 * Copyright(C) 1995,1997 Harlequin Group, all rights reserved
 *
 * READERSHIP
 *
 * Any MPS developer.  It will help to read design.mps.poolmrg.
 * 
 * DESIGN
 * 
 * See design.mps.poolmrg.
 * 
 * .update.wasold: At the moment the fix interface provides no way for
 * the caller to determine whether the referent was in old space.
 * This is necessary for finalization.  For now, the callsite to fix
 * (which will need to be changed when the fix interface changes) bears
 * this tag.
 *
 * .access.exact: There is no way to to determine the "correct" rank to
 * scan at when an access fault is taken (we should use whatever rank the
 * tracer is at).  We default to scanning at the RankEXACT.
 *
 * .improve.rank: At the moment, the pool is a guardian for the final
 * rank.  It could be generalized to be a guardian for an arbitrary
 * rank (a guardian for RankEXACT would tell you if the object was
 * ambiguously referenced, for example).  The code that would need to be
 * modified bears this tag.
 */


#include "mpm.h"
#include "poolmrg.h"


SRCID(poolmrg, "$HopeName: MMsrc!poolmrg.c(trunk.2) $");

#define MRGSig          ((Sig)0x519B0349)

typedef struct MRGStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  RingStruct entry;		/* design.mps.poolmrg.poolstruct.entry */
  RingStruct exit;		/* design.mps.poolmrg.poolstruct.exit */
  RingStruct free;		/* design.mps.poolmrg.poolstruct.free */
  RingStruct group;		/* design.mps.poolmrg.poolstruct.group */
  Size extendBy;		/* design.mps.poolmrg.extend */
  Sig sig;			/* impl.h.mps.sig */
} MRGStruct;

#define PoolPoolMRG(pool) PARENT(MRGStruct, poolStruct, pool)

static Pool MRGPool(MRG mrg);

/* design.mps.poolmrg.guardian.assoc */
static Index indexOfRefPart(Addr a, Space space)
{
  Seg seg;
  Bool b;
  Addr base;
  Addr *pbase, *pa;

  b = SegOfAddr(&seg, space, a);
  AVER(b);
  base = SegBase(space, seg);
  pbase = (Addr *)base;
  pa = (Addr *)a;
  return pa - pbase;
}

/* design.mps.poolmrg.guardian.assoc */
static Index indexOfLinkPart(Addr a, Space space)
{
  Seg seg;
  Bool b;
  Addr base ;
  RingStruct *pbase, *pa;

  b = SegOfAddr(&seg, space, a);
  AVER(b);
  base = SegBase(space, seg);
  pbase = (RingStruct *)base;
  pa = (RingStruct *)a;
  return pa - pbase;
}

#define MRGGroupSig	((Sig)0x5193499b)

typedef struct MRGGroupStruct {
  Sig sig;                      /* impl.h.misc.sig */
  RingStruct group;		/* design.mps.poolmrg.group.group */
  TraceSet grey;                /* design.mps.poolmrg.group.grey */
  Seg refseg;                   /* design.mps.poolmrg.group.segs */
  Seg linkseg;                  /* design.mps.poolmrg.group.segs */
} MRGGroupStruct;
typedef MRGGroupStruct *MRGGroup;

static void MRGGroupDestroy(MRGGroup group, MRG mrg)
{
  Pool pool;

  pool = MRGPool(mrg);
  RingRemove(&group->group);
  PoolSegFree(pool, group->refseg);
  PoolSegFree(pool, group->linkseg);
  SpaceFree(PoolSpace(pool), (Addr)group, (Size)sizeof(MRGGroupStruct));
}

static Res MRGGroupCreate(MRGGroup *groupReturn, MRG mrg)
{
  Addr base;
  MRGGroup group;
  Pool pool;
  Res res;
  RingStruct *linkpart;
  Seg linkseg;
  Seg refseg;
  Size linksegsize;
  Space space;
  Addr *refpart;
  Word i, guardians;
  void *v;

  pool = MRGPool(mrg);
  space = PoolSpace(pool);
  res = SpaceAlloc(&v, space, (Size)sizeof(MRGGroupStruct));
  if(res != ResOK)
    goto failSpaceAlloc;
  group = v;
  res = PoolSegAlloc(&refseg, pool, mrg->extendBy);
  if(res != ResOK)
    goto failRefSegAlloc;

  guardians = mrg->extendBy / sizeof(Addr);	/* per seg */
  linksegsize = guardians * sizeof(RingStruct);
  linksegsize = SizeAlignUp(linksegsize, ArenaAlign(space));
  res = PoolSegAlloc(&linkseg, pool, linksegsize);
  if(res != ResOK)
    goto failLinkSegAlloc;

  /* Link Segment is coerced to an array of RingStructs, each one */
  /* is appended to the free Ring. */
  /* The ref part of each guardian is cleared. */

  AVER(guardians > 0);
  base = SegBase(space, linkseg);
  linkpart = (RingStruct *)base;
  refpart = (Addr *)SegBase(space, refseg);

  for(i=0; i<guardians; ++i) {
    RingInit(&linkpart[i]);
    RingAppend(&mrg->free, &linkpart[i]);
    refpart[i] = 0;
  }
  AVER((Addr)(&linkpart[i]) <= SegLimit(space, linkseg));
  AVER((Addr)(&refpart[i]) <= SegLimit(space, refseg));
  refseg->rank = RankFINAL;

  group->refseg = refseg;
  group->linkseg = linkseg;
  refseg->p = group;
  linkseg->p = group;
  group->grey = TraceSetEMPTY;
  RingInit(&group->group);
  RingAppend(&mrg->group, &group->group);
  group->sig = MRGGroupSig;

  return ResOK;

failLinkSegAlloc:
  PoolSegFree(pool, refseg);
failRefSegAlloc:
  SpaceFree(space, (Addr)group, (Size)sizeof(MRGGroupStruct)); 
failSpaceAlloc:
  return res;
}

static Res MRGGroupScan(ScanState ss, MRGGroup group, MRG mrg)
{
  Addr base;
  Res res;
  Space space;
  Addr *refpart;
  Word guardians, i;
  int wasOld = 0;

  space = PoolSpace(MRGPool(mrg));

  ShieldExpose(space, group->refseg);

  guardians = mrg->extendBy / sizeof(Addr);	/* per seg */
  AVER(guardians > 0);
  base = SegBase(space, group->refseg);
  refpart = (Addr *)base;
  TRACE_SCAN_BEGIN(ss) {
    for(i=0; i<guardians; ++i) {
      if(!TRACE_FIX1(ss, refpart[i])) continue;
      res = TRACE_FIX2(ss, &refpart[i]);	/* .update.wasold */
      if(res != ResOK) {
	return res;
      }
      if(wasOld && ss->rank == RankFINAL) {	/* .improve.rank */
	RingStruct *linkpart =
	  (RingStruct *)SegBase(space, group->linkseg);
	RingRemove(&linkpart[i]);
	RingAppend(&mrg->exit, &linkpart[i]);
      }
    }
  } TRACE_SCAN_END(ss);

  group->grey = TraceSetDel(group->grey, ss->traceId);
  ShieldLower(space, group->refseg, AccessREAD | AccessWRITE);
  ShieldCover(space, group->refseg);

  return ResOK;
}


static Bool MRGCheck(MRG mrg);

static Res MRGInit(Pool pool, va_list args)
{
  MRG mrg = PoolPoolMRG(pool);

  UNUSED(args);
  
  RingInit(&mrg->entry);
  RingInit(&mrg->exit);
  RingInit(&mrg->free);
  RingInit(&mrg->group);
  mrg->extendBy = ArenaAlign(PoolSpace(pool));
  mrg->sig = MRGSig;

  AVERT(MRG, mrg);

  return ResOK;
}

static void MRGFinish(Pool pool)
{
  MRG mrg;
  Ring node;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  node = RingNext(&mrg->group);
  while(node != &mrg->group) {
    Ring next = RingNext(node);
    MRGGroup group = RING_ELT(MRGGroup, group, node);
    MRGGroupDestroy(group, mrg);

    node = next;
  }

  mrg->sig = SigInvalid;
}

static Pool MRGPool(MRG mrg)
{
  AVERT(MRG, mrg);
  return &mrg->poolStruct;
}

static Res MRGAlloc(Addr *pReturn, Pool pool, Size size)
{
  Addr *refpart;
  Bool b;
  Index gi;
  MRG mrg;
  MRGGroup group;
  MRGGroup junk;		/* .group.useless */
  Res res;
  Ring f;
  Seg seg;
  Space space;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  AVER(pReturn != NULL);
  AVER(size == sizeof(Addr));	/* design.mps.poolmrg.alloc.one-size */

  space = PoolSpace(pool);

  f = RingNext(&mrg->free);

  /* design.mps.poolmrg.alloc.grow */

  if(f == &mrg->free) {			/* (Ring has no elements) */
    res = MRGGroupCreate(&junk, mrg);	/* .group.useless: group isn't used */
    if(res != ResOK) {
      return res;
    }
    f = RingNext(&mrg->free);
  }
  AVER(f != &mrg->free);

  /* design.mps.poolmrg.alloc.pop */
  RingRemove(f);
  RingAppend(&mrg->entry, f);
  gi = indexOfLinkPart((Addr)f, space);
  b = SegOfAddr(&seg, space, (Addr)f);
  AVER(b);
  group = seg->p;
  refpart = (Addr *)SegBase(space, group->refseg);

  /* design.mps.poolmrg.guardian.ref.alloc */
  *pReturn = (Addr)(&refpart[gi]);
  return ResOK;
}

static void MRGFree(Pool pool, Addr old, Size size)
{
  MRG mrg;
  Index gi;
  Space space;
  Seg seg;
  MRGGroup group;
  Bool b;
  RingStruct *linkpart;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  AVER(old != (Addr)0);
  AVER(size == sizeof(Addr));

  space = PoolSpace(pool);
  b = SegOfAddr(&seg, space, old);
  AVER(b);
  group = seg->p;
  linkpart = (RingStruct *)SegBase(space, group->linkseg);

  /* design.mps.poolmrg.guardian.ref.free */
  gi = indexOfRefPart(old, space);

  AVERT(Ring, &linkpart[gi]);
  RingRemove(&linkpart[gi]);
  RingAppend(&mrg->free, &linkpart[gi]);
  *(Addr *)old = 0;   	/* design.mps.poolmrg.free.overwrite */
}

static Res MRGDescribe(Pool pool, mps_lib_FILE *stream)
{
  MRG mrg;
  Ring r;
  Space space;
  Bool b;
  MRGGroup group;
  Index gi;
  Seg seg;
  Addr *refpart;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);
  /* Cannot check stream */

  space = PoolSpace(pool);

  WriteF(stream, "  extendBy $W\n", mrg->extendBy, NULL);
  WriteF(stream, "  Entry queue:\n", NULL);
  RING_FOR(r, &mrg->entry) {
    b = SegOfAddr(&seg, space, (Addr)r);
    AVER(b);
    group = seg->p;
    refpart = (Addr *)SegBase(space, group->refseg);
    gi = indexOfLinkPart((Addr)r, space);
    WriteF(stream,
	   "    at $A ref $A\n",
	   (WriteFA)&refpart[gi], (WriteFA)refpart[gi],
	   NULL);
  }
  WriteF(stream, "  Exit queue:\n", NULL);
  RING_FOR(r, &mrg->exit) {
    b = SegOfAddr(&seg, space, (Addr)r);
    AVER(b);
    group = seg->p;
    refpart = (Addr *)SegBase(space, group->refseg);
    gi = indexOfLinkPart((Addr)r, space);
    WriteF(stream,
	   "    at $A ref $A\n",
	   (WriteFA)&refpart[gi], (WriteFA)refpart[gi],
	   NULL);
  }

  return ResOK;
}

static void MRGGrey(Pool pool, Space space, TraceId ti)
{
  MRG mrg;
  Ring r;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);
  AVERT(Space, space);
  AVERT(TraceId, ti);

  RING_FOR(r, &mrg->group) {
    MRGGroup group;

    group = RING_ELT(MRGGroup, group, r);
    group->grey = TraceSetAdd(group->grey, ti);
    ShieldRaise(space, group->refseg, AccessREAD | AccessWRITE);
  }
}

static Res MRGScan(ScanState ss, Pool pool, Bool *finishedReturn)
{
  MRG mrg;
  Res res;

  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);
  AVER(finishedReturn != NULL);

  if(ss->rank == RankFINAL) {
    Ring r;

    RING_FOR(r, &mrg->group) {
      MRGGroup group;

      group = RING_ELT(MRGGroup, group, r);
      if(TraceSetIsMember(group->grey, ss->traceId)) {
	res = MRGGroupScan(ss, group, mrg);
	if(res != ResOK) {
	  return res;
	}
	*finishedReturn = FALSE;
	return ResOK;
      }
    }
  }

  *finishedReturn = TRUE;
  return ResOK;
}

/* .amc.copy: This code is an almost exact copy of the analogous
 * method in impl.c.amc.  This is worrying.
 */
static void MRGAccess(Pool pool, Seg seg, AccessSet mode)
{
  Space space;
  MRG mrg;
  MRGGroup group;
  Res res;
  ScanStateStruct ss;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);
  AVERT(Seg, seg);
  AVER(seg->pool == pool);
  /* Cannot check AccessSet */

  space = PoolSpace(pool);

  group = (MRGGroup)seg->p;

  ss.fix = TraceFix;
  ss.zoneShift = space->zoneShift;
  ss.summary = RefSetEMPTY;
  ss.space = space;
  ss.sig = ScanStateSig;
  ss.rank = RankEXACT;  /* .access.exact */
  ss.weakSplat = (Addr)0xadd4badd;

  /* impl.c.amc.access.multi (!) */
  for(ss.traceId = 0; ss.traceId < TRACE_MAX; ++ss.traceId) {
    if(TraceSetIsMember(space->busyTraces, ss.traceId)) {
      ss.condemned = space->trace[ss.traceId].condemned;
      res = MRGGroupScan(&ss, group, mrg);
      AVER(res == ResOK);       /* impl.c.amc.access.error (!) */
    }
  }
}

static PoolClassStruct PoolClassMRGStruct = {
  PoolClassSig,                         /* sig */
  "MRG",                                /* name */
  sizeof(MRGStruct),                    /* size */
  offsetof(MRGStruct, poolStruct),      /* offset */
  AttrSCAN | AttrALLOC | AttrFREE | AttrINCR_RB,
  MRGInit,                              /* init */
  MRGFinish,                            /* finish */
  MRGAlloc,                             /* alloc */
  MRGFree,                              /* free */
  PoolNoBufferInit,                     /* bufferInit */
  PoolNoBufferFinish,                   /* bufferFinish */
  PoolNoBufferFill,                     /* bufferFill */
  PoolNoBufferTrip,                     /* bufferTrip */
  PoolNoBufferExpose,                   /* bufferExpose */
  PoolNoBufferCover,                    /* bufferCover */
  PoolNoCondemn,                        /* condemn */
  MRGGrey,                              /* grey */
  MRGScan,                              /* scan */
  PoolNoFix,                            /* fix */
  PoolNoReclaim,                        /* reclaim */
  MRGAccess,                            /* access */
  MRGDescribe,                          /* describe */
  PoolClassSig                          /* impl.h.mpmst.class.end-sig */
};

PoolClass PoolClassMRG(void)
{
  return &PoolClassMRGStruct;
}

/* .check.norecurse: the expression &mrg->poolStruct is used instead of
 * the more natural MRGPool(mrg).  The latter results in infinite
 * recursion because MRGPool calls MRGCheck.
 */
static Bool MRGCheck(MRG mrg)
{
  Space space;

  CHECKS(MRG, mrg);
  CHECKD(Pool, &mrg->poolStruct);
  CHECKL(mrg->poolStruct.class == &PoolClassMRGStruct);
  CHECKL(RingCheck(&mrg->entry));
  CHECKL(RingCheck(&mrg->exit));
  CHECKL(RingCheck(&mrg->free));
  CHECKL(RingCheck(&mrg->group));
  space = PoolSpace(&mrg->poolStruct);  /* .check.norecurse */
  CHECKL(mrg->extendBy == ArenaAlign(space));
  return TRUE;
}

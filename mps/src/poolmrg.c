/* impl.c.poolmrg
 * 
 * MANUAL RANK GUARDIAN POOL
 * 
 * $HopeName: MMsrc!poolmrg.c(trunk.12) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * Any MPS developer.  It will help to read design.mps.poolmrg.
 * 
 * DESIGN
 * 
 * See design.mps.poolmrg.
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

SRCID(poolmrg, "$HopeName: MMsrc!poolmrg.c(trunk.12) $");


#define MRGSig          ((Sig)0x519369B0) /* SIGnature MRG POol */

typedef struct MRGStruct {
  PoolStruct poolStruct;        /* generic pool structure */
  RingStruct entry;             /* design.mps.poolmrg.poolstruct.entry */
  RingStruct exit;              /* design.mps.poolmrg.poolstruct.exit */
  RingStruct free;              /* design.mps.poolmrg.poolstruct.free */
  RingStruct group;             /* design.mps.poolmrg.poolstruct.group */
  Size extendBy;                /* design.mps.poolmrg.extend */
  Sig sig;                      /* impl.h.mps.sig */
} MRGStruct;

#define PoolPoolMRG(pool) PARENT(MRGStruct, poolStruct, pool)

static Pool MRGPool(MRG mrg);

/* design.mps.poolmrg.guardian.assoc */
static Index indexOfRefPart(Addr a, Arena arena)
{
  Seg seg;
  Bool b;
  Addr base;
  Addr *pbase, *pa;

  b = SegOfAddr(&seg, arena, a);
  AVER(b);
  base = SegBase(arena, seg);
  pbase = (Addr *)base;
  pa = (Addr *)a;
  return pa - pbase;
}

/* design.mps.poolmrg.guardian.assoc */
static Index indexOfLinkPart(Addr a, Arena arena)
{
  Seg seg;
  Bool b;
  Addr base ;
  RingStruct *pbase, *pa;

  b = SegOfAddr(&seg, arena, a);
  AVER(b);
  base = SegBase(arena, seg);
  pbase = (RingStruct *)base;
  pa = (RingStruct *)a;
  return pa - pbase;
}

#define MRGGroupSig     ((Sig)0x5193699b) /* SIGnature MRG GrouP */

typedef struct MRGGroupStruct {
  Sig sig;                      /* impl.h.misc.sig */
  RingStruct group;		/* design.mps.poolmrg.group.group */
  Seg refseg;                   /* design.mps.poolmrg.group.segs */
  Seg linkseg;                  /* design.mps.poolmrg.group.segs */
} MRGGroupStruct;
typedef MRGGroupStruct *MRGGroup;

static void MRGGroupDestroy(MRGGroup group, MRG mrg)
{
  Arena arena;

  arena = PoolArena(MRGPool(mrg));
  RingRemove(&group->group);
  SegFree(arena, group->refseg);
  SegFree(arena, group->linkseg);
  ArenaFree(arena, group, (Size)sizeof(MRGGroupStruct));
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
  Arena arena;
  Addr *refpart;
  Word i, guardians;
  void *v;

  pool = MRGPool(mrg);
  arena = PoolArena(pool);
  res = ArenaAlloc(&v, arena, (Size)sizeof(MRGGroupStruct));
  if(res != ResOK)
    goto failArenaAlloc;
  group = v;
  res = SegAlloc(&refseg, SegPrefDefault(), arena, mrg->extendBy, pool);
  if(res != ResOK)
    goto failRefSegAlloc;

  guardians = mrg->extendBy / sizeof(Addr);     /* per seg */
  linksegsize = guardians * sizeof(RingStruct);
  linksegsize = SizeAlignUp(linksegsize, ArenaAlign(arena));
  res = SegAlloc(&linkseg, SegPrefDefault(), arena, linksegsize, pool);
  if(res != ResOK)
    goto failLinkSegAlloc;

  /* Link Segment is coerced to an array of RingStructs, each one */
  /* is appended to the free Ring. */
  /* The ref part of each guardian is cleared. */

  AVER(guardians > 0);
  base = SegBase(arena, linkseg);
  linkpart = (RingStruct *)base;
  refpart = (Addr *)SegBase(arena, refseg);

  for(i=0; i<guardians; ++i) {
    RingInit(&linkpart[i]);
    RingAppend(&mrg->free, &linkpart[i]);
    refpart[i] = 0;
  }
  AVER((Addr)(&linkpart[i]) <= SegLimit(arena, linkseg));
  AVER((Addr)(&refpart[i]) <= SegLimit(arena, refseg));
  SegSetRankSet(refseg, RankSetSingle(RankFINAL)); /* design.mps.seg.field.rankSet.start */
  SegSetSummary(refseg, RefSetUNIV);               /* design.mps.seg.field.summary.start */

  group->refseg = refseg;
  group->linkseg = linkseg;
  SegSetP(refseg, group);
  SegSetP(linkseg, group);
  RingInit(&group->group);
  RingAppend(&mrg->group, &group->group);
  group->sig = MRGGroupSig;

  return ResOK;

failLinkSegAlloc:
  SegFree(arena, refseg);
failRefSegAlloc:
  ArenaFree(arena, group, (Size)sizeof(MRGGroupStruct)); 
failArenaAlloc:
  return res;
}

static Res MRGGroupScan(ScanState ss, MRGGroup group, MRG mrg)
{
  Addr base;
  Res res;
  Arena arena;
  Addr *refpart;
  Word guardians, i;

  arena = PoolArena(MRGPool(mrg));

  guardians = mrg->extendBy / sizeof(Addr);	/* per seg */
  AVER(guardians > 0);
  base = SegBase(arena, group->refseg);
  refpart = (Addr *)base;
  TRACE_SCAN_BEGIN(ss) {
    for(i=0; i<guardians; ++i) {
      if(!TRACE_FIX1(ss, refpart[i])) continue;
      res = TRACE_FIX2(ss, &refpart[i]);
      if(res != ResOK) {
        return res;
      }
      if(ss->rank == RankFINAL && !ss->wasMarked) {     /* .improve.rank */
        RingStruct *linkpart =
          (RingStruct *)SegBase(arena, group->linkseg);
        RingRemove(&linkpart[i]);
        RingAppend(&mrg->exit, &linkpart[i]);
      }
    }
  } TRACE_SCAN_END(ss);

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
  mrg->extendBy = ArenaAlign(PoolArena(pool));
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
  MRGGroup junk;                /* .group.useless */
  Res res;
  Ring f;
  Seg seg;
  Arena arena;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  AVER(pReturn != NULL);
  AVER(size == sizeof(Addr));   /* design.mps.poolmrg.alloc.one-size */

  arena = PoolArena(pool);

  f = RingNext(&mrg->free);

  /* design.mps.poolmrg.alloc.grow */

  if(f == &mrg->free) {                 /* (Ring has no elements) */
    res = MRGGroupCreate(&junk, mrg);   /* .group.useless: group isn't used */
    if(res != ResOK) {
      return res;
    }
    f = RingNext(&mrg->free);
  }
  AVER(f != &mrg->free);

  /* design.mps.poolmrg.alloc.pop */
  RingRemove(f);
  RingAppend(&mrg->entry, f);
  gi = indexOfLinkPart((Addr)f, arena);
  b = SegOfAddr(&seg, arena, (Addr)f);
  AVER(b);
  group = SegP(seg);
  refpart = (Addr *)SegBase(arena, group->refseg);

  /* design.mps.poolmrg.guardian.ref.alloc */
  *pReturn = (Addr)(&refpart[gi]);
  return ResOK;
}

static void MRGFree(Pool pool, Addr old, Size size)
{
  MRG mrg;
  Index gi;
  Arena arena;
  Seg seg;
  MRGGroup group;
  Bool b;
  RingStruct *linkpart;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  AVER(old != (Addr)0);
  AVER(size == sizeof(Addr));

  arena = PoolArena(pool);
  b = SegOfAddr(&seg, arena, old);
  AVER(b);
  group = SegP(seg);
  linkpart = (RingStruct *)SegBase(arena, group->linkseg);

  /* design.mps.poolmrg.guardian.ref.free */
  gi = indexOfRefPart(old, arena);

  AVERT(Ring, &linkpart[gi]);
  RingRemove(&linkpart[gi]);
  RingAppend(&mrg->free, &linkpart[gi]);
  *(Addr *)old = 0;     /* design.mps.poolmrg.free.overwrite */
}

static Res MRGDescribe(Pool pool, mps_lib_FILE *stream)
{
  MRG mrg;
  Ring r;
  Arena arena;
  Bool b;
  MRGGroup group;
  Index gi;
  Seg seg;
  Addr *refpart;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);
  /* Cannot check stream */

  arena = PoolArena(pool);

  WriteF(stream, "  extendBy $W\n", mrg->extendBy, NULL);
  WriteF(stream, "  Entry queue:\n", NULL);
  RING_FOR(r, &mrg->entry) {
    b = SegOfAddr(&seg, arena, (Addr)r);
    AVER(b);
    group = SegP(seg);
    refpart = (Addr *)SegBase(arena, group->refseg);
    gi = indexOfLinkPart((Addr)r, arena);
    WriteF(stream,
           "    at $A ref $A\n",
           (WriteFA)&refpart[gi], (WriteFA)refpart[gi],
           NULL);
  }
  WriteF(stream, "  Exit queue:\n", NULL);
  RING_FOR(r, &mrg->exit) {
    b = SegOfAddr(&seg, arena, (Addr)r);
    AVER(b);
    group = SegP(seg);
    refpart = (Addr *)SegBase(arena, group->refseg);
    gi = indexOfLinkPart((Addr)r, arena);
    WriteF(stream,
           "    at $A ref $A\n",
           (WriteFA)&refpart[gi], (WriteFA)refpart[gi],
           NULL);
  }

  return ResOK;
}

static Res MRGScan(ScanState ss, Pool pool, Seg seg)
{
  MRG mrg;
  Res res;
  MRGGroup group;

  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);
  AVERT(Seg, seg);

  AVER(SegRankSet(seg) == RankSetSingle(RankFINAL));
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);
  group = (MRGGroup)SegP(seg);
  AVER(seg == group->refseg);

  res = MRGGroupScan(ss, group, mrg);
  if(res != ResOK) return res;

  return ResOK;
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
  PoolNoBufferFill,                     /* bufferFill */
  PoolNoBufferEmpty,                    /* bufferEmpty */
  PoolNoBufferFinish,                   /* bufferFinish */
  PoolNoTraceBegin,			/* traceBegin */
  PoolNoCondemn,                        /* condemn */
  PoolTrivGrey,                         /* grey */
  MRGScan,                              /* scan */
  PoolNoFix,                            /* fix */
  PoolNoReclaim,                        /* reclaim */
  PoolNoTraceEnd,			/* traceEnd */
  PoolNoBenefit,			/* benefit */
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
  Arena arena;

  CHECKS(MRG, mrg);
  CHECKD(Pool, &mrg->poolStruct);
  CHECKL(mrg->poolStruct.class == &PoolClassMRGStruct);
  CHECKL(RingCheck(&mrg->entry));
  CHECKL(RingCheck(&mrg->exit));
  CHECKL(RingCheck(&mrg->free));
  CHECKL(RingCheck(&mrg->group));
  arena = PoolArena(&mrg->poolStruct);  /* .check.norecurse */
  CHECKL(mrg->extendBy == ArenaAlign(arena));
  return TRUE;
}

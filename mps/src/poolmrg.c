/* impl.c.poolmrg: MANUAL RANK GUARDIAN POOL
 * 
 * $HopeName: MMsrc!poolmrg.c(trunk.30) $
 * Copyright (C) 1997 Harlequin Group plc.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 * 
 * DESIGN
 * 
 * .design: See design.mps.poolmrg.
 *
 * NOTES
 * 
 * .improve.rank: At the moment, the pool is a guardian for the final
 * rank.  It could be generalized to be a guardian for an arbitrary
 * rank (a guardian for RankEXACT would tell you if the object was
 * ambiguously referenced, for example).  The code that would need to be
 * modified bears this tag.
 * 
 * TRANSGRESSIONS
 *
 * .addr.void-star: Breaks design.mps.type.addr.use all over the place,
 * accessing the segments acquired from SegAlloc with C pointers.  It
 * would not be practical to use ArenaPeek/Poke everywhere.  Blocks
 * acquired from ArenaAlloc must be directly accessible from C, or else
 * none of the pools would work.  Therefore, if we implement a variant
 * where Addr != void*, we just use the same magic for the control pool
 * and MRG pools, whatever that might be.
 */


#include "mpm.h"
#include "poolmrg.h"

SRCID(poolmrg, "$HopeName: MMsrc!poolmrg.c(trunk.30) $");


/* Types */

/* enumerate the states of a Guardian */
enum {
  MRGGuardianFREE,
  MRGGuardianPREFINAL,
  MRGGuardianFINAL,
  MRGGuardianPOSTFINAL
};

/* Link -- Unprotectable part of guardian */
typedef struct LinkStruct *Link;
typedef struct LinkStruct {
  int state;                     /* Free, Prefinal, Final, Postfinal */
  union {
    MessageStruct messageStruct; /* state = Final */
    RingStruct linkRing;         /* state one of {Free, Prefinal} */
  } the;
} LinkStruct;

#define linkOfMessage(message) \
  PARENT(LinkStruct, the.messageStruct, (message))

#define linkOfRing(ring) \
  PARENT(LinkStruct, the.linkRing, (ring))

/* RefPart -- Protectable part of guardian 
 *
 * This is trivial, but provides a useful abstraction
 * at no performance cost.
 */

typedef struct RefPartStruct *RefPart;
typedef struct RefPartStruct {
  Ref ref;
} RefPartStruct;


/* MRGRefPartRef,MRGRefPartSetRef -- Peek and poke the reference 
 *
 * Might be more efficient to take a seg, rather than calculate it
 * every time.
 *
 * See also .ref.direct which accesses it directly.
 */

static Ref MRGRefPartRef(Arena arena, RefPart refPart)
{
  Ref ref;

  AVERT(Arena, arena);
  AVER(refPart != NULL);

  ref = ArenaPeek(arena, (Addr)&refPart->ref);

  return ref;
}

static Addr MRGRefPartRefAddr(RefPart refPart)
{
  AVER(refPart != NULL);

  return (Addr)&refPart->ref;
}

static void MRGRefPartSetRef(Arena arena, RefPart refPart, Ref ref)
{
  AVERT(Arena, arena);
  AVER(refPart != NULL);

  ArenaPoke(arena, (Addr)&refPart->ref, ref);
}


#define MRGSig          ((Sig)0x519369B0) /* SIGnature MRG POol */

typedef struct MRGStruct {
  PoolStruct poolStruct;    /* generic pool structure */
  RingStruct entryRing;     /* design.mps.poolmrg.poolstruct.entry */
  RingStruct freeRing;      /* design.mps.poolmrg.poolstruct.free */
  RingStruct groupRing;     /* design.mps.poolmrg.poolstruct.group */
  Size extendBy;            /* design.mps.poolmrg.extend */
  Sig sig;                  /* impl.h.mps.sig */
} MRGStruct;

#define PoolPoolMRG(pool) PARENT(MRGStruct, poolStruct, pool)


#define MRGGroupSig     ((Sig)0x5193699b) /* SIGnature MRG GrouP */

typedef struct MRGGroupStruct {
  Sig sig;                      /* impl.h.misc.sig */
  RingStruct mrgRing;           /* design.mps.poolmrg.group.group */
  Seg refPartSeg;               /* design.mps.poolmrg.group.segs */
  Seg linkSeg;                  /* design.mps.poolmrg.group.segs */
} MRGGroupStruct;
typedef MRGGroupStruct *MRGGroup;

static Bool MRGGroupCheck(MRGGroup group)
{
  CHECKS(MRGGroup, group);
  CHECKL(SegPool(group->refPartSeg) == SegPool(group->linkSeg));
  CHECKL(RingCheck(&group->mrgRing));
  CHECKL(SegCheck(group->refPartSeg));
  CHECKL(SegCheck(group->linkSeg));
  return TRUE;
}

/* .check.norecurse: the expression &mrg->poolStruct is used instead */
/* of the more natural MRGPool(mrg).  The latter results in infinite */
/* recursion because MRGPool calls MRGCheck. */

static Bool MRGCheck(MRG mrg)
{
  Arena arena;

  CHECKS(MRG, mrg);
  CHECKD(Pool, &mrg->poolStruct);
  CHECKL(mrg->poolStruct.class == PoolClassMRG());
  CHECKL(RingCheck(&mrg->entryRing));
  CHECKL(RingCheck(&mrg->freeRing));
  CHECKL(RingCheck(&mrg->groupRing));
  arena = PoolArena(&mrg->poolStruct);  /* .check.norecurse */
  CHECKL(mrg->extendBy == ArenaAlign(arena));
  return TRUE;
}


static Count MRGGuardiansPerSeg(MRG mrg)
{
  Count nGuardians;
  AVERT(MRG, mrg);

  nGuardians = mrg->extendBy / sizeof(Ref);
  AVER(nGuardians > 0);

  return(nGuardians);
}

/* design.mps.poolmrg.guardian.assoc */

#define refPartOfIndex(group, index) \
  ((RefPart)SegBase((group)->refPartSeg) + (index))

static RefPart MRGRefPartOfLink(Link link, Arena arena)
{
  Seg seg;
  Bool b;
  Link linkBase;
  Index index;
  MRGGroup group;

  AVER(link != NULL); /* Better checks done by SegOfAddr */
  AVERT(Arena, arena);

  b = SegOfAddr(&seg, arena, (Addr)link);
  AVER(b);
  linkBase = (Link)SegBase(seg);
  AVER(link >= linkBase);
  index = link - linkBase; 
  AVER(index < MRGGuardiansPerSeg(PoolPoolMRG(SegPool(seg))));

  AVER(SegPool(seg)->class == PoolClassMRG());
  group = (MRGGroup)SegP(seg);
  AVERT(MRGGroup, group);
  AVER(group->linkSeg == seg);

  return refPartOfIndex(group, index);
}

#define linkOfIndex(group, index) \
  ((Link)SegBase((group)->linkSeg) + (index))

static Link MRGLinkOfRefPart(RefPart refPart, Arena arena)
{
  Seg seg;
  Bool b;
  RefPart refPartBase;
  Index index;
  MRGGroup group;

  AVER(refPart != NULL); /* Better checks done by SegOfAddr */
  AVERT(Arena, arena);

  b = SegOfAddr(&seg, arena, (Addr)refPart);
  AVER(b);
  refPartBase = (RefPart)SegBase(seg);
  AVER(refPart >= refPartBase);
  index = refPart - refPartBase; 
  AVER(index < MRGGuardiansPerSeg(PoolPoolMRG(SegPool(seg))));

  AVER(SegPool(seg)->class == PoolClassMRG());
  group = SegP(seg);
  AVERT(MRGGroup, group);
  AVER(group->refPartSeg == seg);

  return linkOfIndex(group, index);
}


/* MRGGuardianInit -- Initialises both parts of a guardian */ 

static void MRGGuardianInit(MRG mrg, Link link, RefPart refPart) 
{
  AVERT(MRG, mrg);
  AVER(link != NULL);
  AVER(refPart != NULL);

  RingInit(&link->the.linkRing);
  link->state = MRGGuardianFREE;
  RingAppend(&mrg->freeRing, &link->the.linkRing);
  /* design.mps.poolmrg.free.overwrite */
  MRGRefPartSetRef(PoolArena(&mrg->poolStruct), refPart, 0); 
}


/* MRGMessage* -- Implementation of MRG's MessageClass 
 */

/* deletes the message (frees up the memory) */
static void MRGMessageDelete(Message message)
{
  RefPart refPart;
  Pool pool;
  Arena arena;
  Link link;

  AVERT(Message, message);

  arena = MessageArena(message);

  /* Calculate pool */
  {
    Bool b;
    Seg seg; 
    b = SegOfAddr(&seg, arena, (Addr)message);
    AVER(b);

    pool = SegPool(seg);
  }
  AVER(pool->class == PoolClassMRG());

  link = linkOfMessage(message);
  MessageFinish(message);
  AVER(link->state == MRGGuardianFINAL);
  link->state = MRGGuardianPOSTFINAL;
  refPart = MRGRefPartOfLink(link, arena);
  PoolFree(pool, (Addr)refPart, sizeof(RefPartStruct));
}

static void MRGMessageFinalizationRef(Ref *refReturn,
                                      Arena arena, Message message)
{
  Addr refAddr;
  Link link;
  Ref ref;
  RefPart refPart;

  AVER(refReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Message, message);

  AVER(message->type == MessageTypeFinalization);

  link = linkOfMessage(message);
  AVER(link->state == MRGGuardianFINAL);
  refPart = MRGRefPartOfLink(link, arena);

  refAddr = MRGRefPartRefAddr(refPart);

  /* ensure that the reference is not (white and flipped) */
  ref = (Ref)ArenaRead(arena, refAddr);

  AVER(ref != 0);
  *refReturn = ref;
}

static MessageClassStruct MRGMessageClassStruct = {
  MessageClassSig,             /* sig */
  "MRGFinal",                  /* name */
  MRGMessageDelete,            /* Delete */
  MRGMessageFinalizationRef,   /* FinalizationRef */
  /* CollectionStats* */
  MessageNoCollectionStatsLiveSize,          
  MessageNoCollectionStatsCondemnedSize,   
  MessageNoCollectionStatsNotCondemnedSize,
  MessageClassSig              /* design.mps.message.class.sig.double */
};


static Pool MRGPool(MRG mrg)
{
  AVERT(MRG, mrg);
  return &mrg->poolStruct;
}


/* MRGGroupDestroy --- Destroys Group
 *
 * .group.destroy:  We don't worry about the effect that destroying
 * this group has on any of the pool rings.
 */

static void MRGGroupDestroy(MRGGroup group, MRG mrg)
{
  Pool pool;

  AVERT(MRGGroup, group);
  AVERT(MRG, mrg);

  pool = MRGPool(mrg);
  RingRemove(&group->mrgRing);
  RingFinish(&group->mrgRing);
  group->sig = SigInvalid;
  SegFree(group->refPartSeg);
  SegFree(group->linkSeg);
  ArenaFree(PoolArena(pool), (Addr)group, (Size)sizeof(MRGGroupStruct));
}

static Res MRGGroupCreate(MRGGroup *groupReturn, MRG mrg,
                          Bool withReservoirPermit)
{
  RefPart refPartBase;
  Count nGuardians;       /* guardians per seg */
  Index i;
  Link linkBase;
  MRGGroup group;
  Pool pool;
  Res res;
  Seg linkSeg, refPartSeg;
  Size linkSegSize;
  Arena arena;
  void *p;

  AVER(groupReturn != NULL);
  AVERT(MRG, mrg);
  AVER(BoolCheck(withReservoirPermit));

  pool = MRGPool(mrg);
  arena = PoolArena(pool);
  res = ArenaAlloc(&p, arena, (Size)sizeof(MRGGroupStruct));
  if(res != ResOK)
    goto failArenaAlloc;
  group = p;
  res = SegAlloc(&refPartSeg, SegPrefDefault(), mrg->extendBy, pool,
                 withReservoirPermit);
  if(res != ResOK)
    goto failRefPartSegAlloc;

  nGuardians = MRGGuardiansPerSeg(mrg); 
  linkSegSize = nGuardians * sizeof(LinkStruct);
  linkSegSize = SizeAlignUp(linkSegSize, ArenaAlign(arena));
  res = SegAlloc(&linkSeg, SegPrefDefault(), linkSegSize, pool,
                 withReservoirPermit);
  if(res != ResOK)
    goto failLinkSegAlloc;

  linkBase = (Link)SegBase(linkSeg);
  refPartBase = (RefPart)SegBase(refPartSeg);

  /* design.mps.seg.field.rankset.start, .improve.rank */
  SegSetRankSet(refPartSeg, RankSetSingle(RankFINAL)); 

  for(i = 0; i < nGuardians; ++i) 
    MRGGuardianInit(mrg, linkBase + i, refPartBase + i);
  AVER((Addr)(&linkBase[i]) <= SegLimit(linkSeg));
  AVER((Addr)(&refPartBase[i]) <= SegLimit(refPartSeg));

  group->refPartSeg = refPartSeg;
  group->linkSeg = linkSeg;
  SegSetP(refPartSeg, group);
  SegSetP(linkSeg, group);
  RingInit(&group->mrgRing);
  RingAppend(&mrg->groupRing, &group->mrgRing);
  group->sig = MRGGroupSig;
  AVERT(MRGGroup, group);
  *groupReturn = group;

  return ResOK;

failLinkSegAlloc:
  SegFree(refPartSeg);
failRefPartSegAlloc:
  ArenaFree(arena, group, (Size)sizeof(MRGGroupStruct)); 
failArenaAlloc:
  return res;
}


/* MRGFinalise -- finalize the indexth guardian in the group 
 */
static void MRGFinalize(Arena arena, MRGGroup group, Index index)
{
  Link link;
  Message message;

  AVERT(Arena, arena);
  AVERT(MRGGroup, group);
  AVER(index < MRGGuardiansPerSeg(PoolPoolMRG(SegPool(group->refPartSeg))));

  link = linkOfIndex(group, index);

  /* only finalize it if it hasn't been finalized already */
  if(link->state != MRGGuardianFINAL) {
    AVER(link->state == MRGGuardianPREFINAL);
    RingRemove(&link->the.linkRing);
    RingFinish(&link->the.linkRing);
    link->state = MRGGuardianFINAL;
    message = &link->the.messageStruct;
    MessageInit(arena, message, &MRGMessageClassStruct);
    MessagePost(arena, message);
  }
}


static Res MRGGroupScan(ScanState ss, MRGGroup group, MRG mrg)
{
  Res res;
  Arena arena;

  RefPart refPart;
  Index i;
  Count nGuardians;

  AVERT(ScanState, ss);
  AVERT(MRGGroup, group);
  AVERT(MRG, mrg);

  arena = PoolArena(MRGPool(mrg));

  nGuardians = MRGGuardiansPerSeg(mrg); 
  AVER(nGuardians > 0);
  TRACE_SCAN_BEGIN(ss) {
    for(i=0; i < nGuardians; ++i) {
      refPart = refPartOfIndex(group, i);

      /* free guardians are not scanned */
      if(MRGLinkOfRefPart(refPart, arena)->state != MRGGuardianFREE) {
	ss->wasMarked = TRUE;
	/* .ref.direct: We can access the reference directly */
	/* because we are in a scan and the shield is exposed. */
	if(TRACE_FIX1(ss, refPart->ref)) {
	  res = TRACE_FIX2(ss, &(refPart->ref));
	  if(res != ResOK) 
	    return res;
	
	  if(ss->rank == RankFINAL && !ss->wasMarked) { /* .improve.rank */
	    MRGFinalize(arena, group, i);
	  }
	}
      }
    }
  } TRACE_SCAN_END(ss);

  return ResOK;
}


static Res MRGInit(Pool pool, va_list args)
{
  MRG mrg;
  
  AVER(pool != NULL); /* Can't check more; see pool contract @@@@ */
  UNUSED(args);
  
  mrg = PoolPoolMRG(pool);

  RingInit(&mrg->entryRing);
  RingInit(&mrg->freeRing);
  RingInit(&mrg->groupRing);
  mrg->extendBy = ArenaAlign(PoolArena(pool));
  mrg->sig = MRGSig;

  AVERT(MRG, mrg);

  return ResOK;
}

static void MRGFinish(Pool pool)
{
  MRG mrg;
  Ring node, nextNode;

  AVERT(Pool, pool);
  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  /* .finish.ring: Before destroying the groups, we isolate the */
  /* rings in the pool structure.  The problem we are avoiding here */
  /* is when the rings point to memory that has been unmapped by one */
  /* groupDestroy and a subsequent groupDestroy calls MRGCheck which */
  /* checks the rings which causes the program to fault because */
  /* RingCheck will access unmapped memory. */

  /* We call RingRemove on the master node for the rings, thereby */
  /* effectively emptying them, but leaving the rest of the ring */
  /* "dangling".  This is okay as we are about to destroy all the */
  /* groups so the contents of the rings will dissappear soon. */

  /* .finish.no-final: Note that this relies on the fact that no */
  /* Guardians are in the FINAL state and hence on the Arena Message */
  /* Queue.  We are guaranteed this because MRGFinish is only called */
  /* from ArenaDestroy, and the message queue has been emptied prior */
  /* to the call.  See impl.c.arena.message.queue.empty */

  if(!RingIsSingle(&mrg->entryRing)) {
    RingRemove(&mrg->entryRing);
  }
  if(!RingIsSingle(&mrg->freeRing)) {
    RingRemove(&mrg->freeRing);
  }

  RING_FOR(node, &mrg->groupRing, nextNode) {
    MRGGroup group = RING_ELT(MRGGroup, mrgRing, node);
    MRGGroupDestroy(group, mrg);
  }

  mrg->sig = SigInvalid;
  RingFinish(&mrg->groupRing);
  /* design.mps.poolmrg.trans.no-finish */
}

Res MRGRegister(Pool pool, Ref ref)
{
  Ring freeNode;
  Arena arena;
  Link link;
  RefPart refPart;
  MRG mrg;
  Res res;
  MRGGroup junk; /* unused */

  AVERT(Pool, pool);
  AVER(ref != 0);

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* design.mps.poolmrg.alloc.grow */
  if(RingIsSingle(&mrg->freeRing)) {
    /* .group.useless: group isn't used */
    /* @@@@ Should the client be able to use the reservoir for this? */
    res = MRGGroupCreate(&junk, mrg, /* withReservoirPermit */ FALSE);   
    if(res != ResOK) 
      return res;
  }
  AVER(!RingIsSingle(&mrg->freeRing));
  freeNode = RingNext(&mrg->freeRing);

  link = linkOfRing(freeNode);
  AVER(link->state == MRGGuardianFREE);
  /* design.mps.poolmrg.alloc.pop */
  RingRemove(freeNode);
  link->state = MRGGuardianPREFINAL;
  RingAppend(&mrg->entryRing, freeNode);

  /* design.mps.poolmrg.guardian.ref.alloc */
  refPart = MRGRefPartOfLink(link, arena);
  MRGRefPartSetRef(arena, refPart, ref);

  return ResOK;
}

static void MRGFree(Pool pool, Addr old, Size size)
{
  MRG mrg;
  Arena arena;
  Link link;
  RefPart refPart;

  AVERT(Pool, pool);
  AVER(old != (Addr)0);
  AVER(size == sizeof(RefPartStruct));

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  refPart = (RefPart)old;

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* design.mps.poolmrg.guardian.ref.free */
  link = MRGLinkOfRefPart(refPart, arena);
  AVER(link->state == MRGGuardianPOSTFINAL);

  MRGGuardianInit(mrg, link, refPart);
}


/* Describe
 *
 * This could be improved by implementing MRGSegDescribe
 * and having MRGDescribe iterate over all the pool's segments.
 */

static Res MRGDescribe(Pool pool, mps_lib_FILE *stream)
{
  MRG mrg;
  Arena arena;
  Ring node, nextNode;
  RefPart refPart;

  AVERT(Pool, pool);
  /* Cannot check stream */

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  WriteF(stream, "  extendBy $W\n", mrg->extendBy, NULL);
  WriteF(stream, "  Entry queue:\n", NULL);
  RING_FOR(node, &mrg->entryRing, nextNode) {
    refPart = MRGRefPartOfLink(linkOfRing(node), arena);
    WriteF(stream,
           "    at $A Ref $A\n",
           (WriteFA)refPart, (WriteFA)MRGRefPartRef(arena, refPart),
           NULL);
  }

  return ResOK;
}

static Res MRGScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  MRG mrg;
  Res res;
  MRGGroup group;

  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);

  mrg = PoolPoolMRG(pool);
  AVERT(MRG, mrg);

  AVER(SegRankSet(seg) == RankSetSingle(RankFINAL)); /* .improve.rank */
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);
  group = (MRGGroup)SegP(seg);
  AVER(seg == group->refPartSeg);

  res = MRGGroupScan(ss, group, mrg);
  if(res != ResOK)  {
    *totalReturn = FALSE;
    return res;
  }

  *totalReturn = TRUE;
  return ResOK;
}


static PoolClassStruct PoolClassMRGStruct = {
  PoolClassSig,
  "MRG",                                /* name */
  sizeof(MRGStruct),                    /* size */
  offsetof(MRGStruct, poolStruct),      /* offset */
  NULL,                                 /* super */
  AttrSCAN | AttrFREE | AttrINCR_RB,
  MRGInit,
  MRGFinish,
  PoolNoAlloc,
  MRGFree,
  PoolNoBufferInit,
  PoolNoBufferFill,
  PoolNoBufferEmpty,
  PoolNoBufferFinish,
  PoolNoTraceBegin,
  PoolNoAccess,
  PoolNoWhiten,
  PoolTrivGrey,
  PoolTrivBlacken,
  MRGScan,
  PoolNoFix,
  PoolNoFix,
  PoolNoReclaim,
  PoolNoBenefit,
  PoolNoAct,
  PoolNoRampBegin,
  PoolNoRampEnd,
  PoolNoWalk,
  MRGDescribe,
  PoolNoDebugMixin,
  PoolClassSig                          /* impl.h.mpmst.class.end-sig */
};


PoolClass PoolClassMRG(void)
{
  return &PoolClassMRGStruct;
}

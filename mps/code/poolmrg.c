/* poolmrg.c: MANUAL RANK GUARDIAN POOL
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * .design: See <design/poolmrg/>.
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
 * .addr.void-star: Breaks <design/type/#addr.use> all over the place,
 * accessing the segments acquired from SegAlloc with C pointers.  It
 * would not be practical to use ArenaPeek/Poke everywhere.  Blocks
 * acquired from ControlAlloc must be directly accessible from C, or else
 * none of the pools would work.  Therefore, if we implement a variant
 * where Addr != void*, we just use the same magic for the control pool
 * and MRG pools, whatever that might be.
 */

#include "mpm.h"
#include "poolmrg.h"

SRCID(poolmrg, "$Id$");


/* Types */

/* enumerate the states of a Guardian */
enum {
  MRGGuardianFREE = 1,
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
  AVER(refPart != NULL);

  ArenaPoke(arena, (Addr)&refPart->ref, ref);
}


/* MRGStruct -- MRG pool structure */

#define MRGSig          ((Sig)0x519369B0) /* SIGnature MRG POol */

typedef struct MRGStruct {
  PoolStruct poolStruct;    /* generic pool structure */
  RingStruct entryRing;     /* <design/poolmrg/#poolstruct.entry> */
  RingStruct freeRing;      /* <design/poolmrg/#poolstruct.free> */
  RingStruct refRing;       /* <design/poolmrg/#poolstruct.refring> */
  Size extendBy;            /* <design/poolmrg/#extend> */
  Sig sig;                  /* <code/mps.h#sig> */
} MRGStruct;

#define Pool2MRG(pool) PARENT(MRGStruct, poolStruct, pool)
#define MRG2Pool(mrg) (&(mrg)->poolStruct)


/* MRGCheck -- check an MRG pool */

static Bool MRGCheck(MRG mrg)
{
  CHECKS(MRG, mrg);
  CHECKD(Pool, &mrg->poolStruct);
  CHECKL(MRG2Pool(mrg)->class == PoolClassMRG());
  CHECKL(RingCheck(&mrg->entryRing));
  CHECKL(RingCheck(&mrg->freeRing));
  CHECKL(RingCheck(&mrg->refRing));
  CHECKL(mrg->extendBy == ArenaAlign(PoolArena(MRG2Pool(mrg))));
  return TRUE;
}


#define MRGRefSegSig     ((Sig)0x51936965) /* SIGnature MRG Ref Seg */
#define MRGLinkSegSig    ((Sig)0x51936915) /* SIGnature MRG Link Seg */

typedef struct MRGLinkSegStruct *MRGLinkSeg;
typedef struct MRGRefSegStruct *MRGRefSeg;

typedef struct MRGLinkSegStruct {
  SegStruct segStruct;      /* superclass fields must come first */
  MRGRefSeg refSeg;         /* <design/poolmrg/#mrgseg.link.refseg> */
  Sig sig;                  /* <code/misc.h#sig> */
} MRGLinkSegStruct;

typedef struct MRGRefSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  RingStruct mrgRing;       /* <design/poolmrg/#mrgseg.ref.segring> */
  MRGLinkSeg linkSeg;       /* <design/poolmrg/#mrgseg.ref.linkseg> */
  Sig sig;                  /* <code/misc.h#sig> */
} MRGRefSegStruct;

/* macros to get between child and parent seg structures */

#define Seg2LinkSeg(seg)     ((MRGLinkSeg)(seg))
#define LinkSeg2Seg(linkseg) ((Seg)(linkseg))
                            
#define Seg2RefSeg(seg)      ((MRGRefSeg)(seg))
#define RefSeg2Seg(refseg)   ((Seg)(refseg))


/* forward declarations */

static SegClass MRGLinkSegClassGet(void);
static SegClass MRGRefSegClassGet(void);


/* MRGLinkSegCheck -- check a link segment
 *
 * .link.nullref: During initialization of a link segment the refSeg
 * field will be NULL. This will be initialized when the reference
 * segment is initialized.  See <design/poolmrg/#mrgseg.link.refseg>.
 */
static Bool MRGLinkSegCheck(MRGLinkSeg linkseg)
{
  Seg seg;

  CHECKS(MRGLinkSeg, linkseg);
  CHECKL(SegCheck(&linkseg->segStruct));
  seg = LinkSeg2Seg(linkseg);
  if (NULL != linkseg->refSeg) { /* see .link.nullref */
    CHECKL(SegPool(seg) == SegPool(RefSeg2Seg(linkseg->refSeg)));
    CHECKU(MRGRefSeg, linkseg->refSeg);
    CHECKL(linkseg->refSeg->linkSeg == linkseg);
  }
  return TRUE;
}

static Bool MRGRefSegCheck(MRGRefSeg refseg)
{
  Seg seg;

  CHECKS(MRGRefSeg, refseg);
  CHECKL(GCSegCheck(&refseg->gcSegStruct));
  seg = RefSeg2Seg(refseg);
  CHECKL(SegPool(seg) == SegPool(LinkSeg2Seg(refseg->linkSeg)));
  CHECKL(RingCheck(&refseg->mrgRing));
  CHECKD(MRGLinkSeg, refseg->linkSeg);
  CHECKL(refseg->linkSeg->refSeg == refseg);
  return TRUE;
}


/* MRGLinkSegInit -- initialise a link segment */

static Res MRGLinkSegInit(Seg seg, Pool pool, Addr base, Size size,
                          Bool reservoirPermit, va_list args)
{
  SegClass super;
  MRGLinkSeg linkseg;
  MRG mrg;
  Res res;

  AVERT(Seg, seg);
  linkseg = Seg2LinkSeg(seg);
  AVERT(Pool, pool);
  mrg = Pool2MRG(pool);
  AVERT(MRG, mrg);
  /* no useful checks for base and size */
  AVER(BoolCheck(reservoirPermit));

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(MRGLinkSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if (res != ResOK)
    return res;
  linkseg->refSeg = NULL; /* .link.nullref */
  linkseg->sig = MRGLinkSegSig;
  AVERT(MRGLinkSeg, linkseg);

  return ResOK;
}


/* MRGRefSegInit -- initialise a ref segment 
 *
 * .ref.initarg: The paired link segment is passed as an additional
 * (vararg) parameter when creating the ref segment. Initially the
 * refSeg field of the link segment is NULL (see .link.nullref).
 * It's initialized here to the newly initialized ref segment.
 */
static Res MRGRefSegInit(Seg seg, Pool pool, Addr base, Size size,
                         Bool reservoirPermit, va_list args)
{
  MRGLinkSeg linkseg = va_arg(args, MRGLinkSeg);  /* .ref.initarg */
  MRGRefSeg refseg;
  MRG mrg;
  SegClass super;
  Res res;

  AVERT(Seg, seg);
  refseg = Seg2RefSeg(seg);
  AVERT(Pool, pool);
  mrg = Pool2MRG(pool);
  AVERT(MRG, mrg);
  /* no useful checks for base and size */
  AVER(BoolCheck(reservoirPermit));
  AVERT(MRGLinkSeg, linkseg);

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(MRGRefSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if (res != ResOK)
    return res;

  /* <design/seg/#field.rankset.start>, .improve.rank */
  SegSetRankSet(seg, RankSetSingle(RankFINAL));

  RingInit(&refseg->mrgRing);
  RingAppend(&mrg->refRing, &refseg->mrgRing);
  refseg->linkSeg = linkseg;
  AVER(NULL == linkseg->refSeg); /* .link.nullref */
  refseg->sig = MRGRefSegSig;
  linkseg->refSeg = refseg;      /* .ref.initarg */

  AVERT(MRGRefSeg, refseg);
  AVERT(MRGLinkSeg, linkseg);

  return ResOK;
}


/* MRGLinkSegClass -- Class definition */

DEFINE_SEG_CLASS(MRGLinkSegClass, class)
{
  INHERIT_CLASS(class, SegClass);
  SegClassMixInNoSplitMerge(class);  /* no support for this */
  class->name = "MRGLSEG";
  class->size = sizeof(MRGLinkSegStruct);
  class->init = MRGLinkSegInit;
}


/* MRGRefSegClass -- Class definition */

DEFINE_SEG_CLASS(MRGRefSegClass, class)
{
  INHERIT_CLASS(class, GCSegClass);
  SegClassMixInNoSplitMerge(class);  /* no support for this */
  class->name = "MRGRSEG";
  class->size = sizeof(MRGRefSegStruct);
  class->init = MRGRefSegInit;
}


static Count MRGGuardiansPerSeg(MRG mrg)
{
  Count nGuardians;
  AVERT(MRG, mrg);

  nGuardians = mrg->extendBy / sizeof(Ref);
  AVER(nGuardians > 0);

  return(nGuardians);
}


/* <design/poolmrg/#guardian.assoc> */

#define refPartOfIndex(refseg, index) \
  ((RefPart)SegBase(RefSeg2Seg(refseg)) + (index))

static RefPart MRGRefPartOfLink(Link link, Arena arena)
{
  Seg seg;
  Bool b;
  Link linkBase;
  Index index;
  MRGLinkSeg linkseg;

  AVER(link != NULL); /* Better checks done by SegOfAddr */

  b = SegOfAddr(&seg, arena, (Addr)link);
  AVER(b);
  AVER(SegPool(seg)->class == PoolClassMRG());
  linkseg = Seg2LinkSeg(seg);
  AVERT(MRGLinkSeg, linkseg);
  linkBase = (Link)SegBase(seg);
  AVER(link >= linkBase);
  index = link - linkBase;
  AVER(index < MRGGuardiansPerSeg(Pool2MRG(SegPool(seg))));

  return refPartOfIndex(linkseg->refSeg, index);
}

#define linkOfIndex(linkseg, index) \
  ((Link)SegBase(LinkSeg2Seg(linkseg)) + (index))

static Link MRGLinkOfRefPart(RefPart refPart, Arena arena)
{
  Seg seg;
  Bool b;
  RefPart refPartBase;
  Index index;
  MRGRefSeg refseg;

  AVER(refPart != NULL); /* Better checks done by SegOfAddr */

  b = SegOfAddr(&seg, arena, (Addr)refPart);
  AVER(b);
  AVER(SegPool(seg)->class == PoolClassMRG());
  refseg = Seg2RefSeg(seg);
  AVERT(MRGRefSeg, refseg);
  refPartBase = (RefPart)SegBase(seg);
  AVER(refPart >= refPartBase);
  index = refPart - refPartBase;
  AVER(index < MRGGuardiansPerSeg(Pool2MRG(SegPool(seg))));

  return linkOfIndex(refseg->linkSeg, index);
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
  /* <design/poolmrg/#free.overwrite> */
  MRGRefPartSetRef(PoolArena(&mrg->poolStruct), refPart, 0);
}


/* MRGMessage* -- Implementation of MRG's MessageClass */


/* MRGMessageDelete -- deletes the message (frees up the memory) */

static void MRGMessageDelete(Message message)
{
  RefPart refPart;
  Pool pool;
  Arena arena;
  Link link;

  AVERT(Message, message);

  arena = MessageArena(message);

  { /* Calculate pool */
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


/* MRGMessageFinalizationRef -- extract the finalized reference from the msg */

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

  AVER(message->type == MessageTypeFINALIZATION);

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
  MessageNoGCLiveSize,         /* GCLiveSize */   
  MessageNoGCCondemnedSize,    /* GCCondemnedSize */
  MessageNoGCNotCondemnedSize, /* GCNoteCondemnedSize */
  MessageClassSig              /* <design/message/#class.sig.double> */
};


/* MRGSegPairDestroy --- Destroys a pair of segments (link & ref)
 *
 * .segpair.destroy: We don't worry about the effect that destroying
 * these segs has on any of the pool rings.
 */
static void MRGSegPairDestroy(MRGRefSeg refseg, MRG mrg)
{
  Pool pool;

  pool = MRG2Pool(mrg);
  RingRemove(&refseg->mrgRing);
  RingFinish(&refseg->mrgRing);
  refseg->sig = SigInvalid;
  SegFree(LinkSeg2Seg(refseg->linkSeg));
  SegFree(RefSeg2Seg(refseg));
}


/* MRGSegPairCreate -- create a pair of segments (link & ref) */

static Res MRGSegPairCreate(MRGRefSeg *refSegReturn, MRG mrg,
                            Bool withReservoirPermit)
{
  RefPart refPartBase;
  Count nGuardians;       /* guardians per seg */
  Index i;
  Link linkBase;
  Pool pool;
  Res res;
  Seg segLink, segRefPart;
  MRGLinkSeg linkseg;
  MRGRefSeg refseg;
  Size linkSegSize;
  Arena arena;

  AVER(refSegReturn != NULL);

  pool = MRG2Pool(mrg);
  arena = PoolArena(pool);

  nGuardians = MRGGuardiansPerSeg(mrg);
  linkSegSize = nGuardians * sizeof(LinkStruct);
  linkSegSize = SizeAlignUp(linkSegSize, ArenaAlign(arena));

  res = SegAlloc(&segLink, EnsureMRGLinkSegClass(),
                 SegPrefDefault(), linkSegSize, pool,
                 withReservoirPermit);
  if (res != ResOK)
    goto failLinkSegAlloc;
  linkseg = Seg2LinkSeg(segLink);

  res = SegAlloc(&segRefPart, EnsureMRGRefSegClass(),
                 SegPrefDefault(), mrg->extendBy, pool,
                 withReservoirPermit,
                 linkseg); /* .ref.initarg */
  if (res != ResOK)
    goto failRefPartSegAlloc;
  refseg = Seg2RefSeg(segRefPart);

  linkBase = (Link)SegBase(segLink);
  refPartBase = (RefPart)SegBase(segRefPart);

  for(i = 0; i < nGuardians; ++i)
    MRGGuardianInit(mrg, linkBase + i, refPartBase + i);
  AVER((Addr)(&linkBase[i]) <= SegLimit(segLink));
  AVER((Addr)(&refPartBase[i]) <= SegLimit(segRefPart));

  *refSegReturn = refseg;

  return ResOK;

failRefPartSegAlloc:
  SegFree(segLink);
failLinkSegAlloc:
  return res;
}


/* MRGFinalise -- finalize the indexth guardian in the segment */

static void MRGFinalize(Arena arena, MRGLinkSeg linkseg, Index index)
{
  Link link;
  Message message;

  AVER(index < MRGGuardiansPerSeg(Pool2MRG(SegPool(LinkSeg2Seg(linkseg)))));

  link = linkOfIndex(linkseg, index);

  /* only finalize it if it hasn't been finalized already */
  if (link->state != MRGGuardianFINAL) {
    AVER(link->state == MRGGuardianPREFINAL);
    RingRemove(&link->the.linkRing);
    RingFinish(&link->the.linkRing);
    link->state = MRGGuardianFINAL;
    message = &link->the.messageStruct;
    MessageInit(arena, message, &MRGMessageClassStruct, MessageTypeFINALIZATION);
    MessagePost(arena, message);
  }
}


static Res MRGRefSegScan(ScanState ss, MRGRefSeg refseg, MRG mrg)
{
  Res res;
  Arena arena;
  MRGLinkSeg linkseg;

  RefPart refPart;
  Index i;
  Count nGuardians;

  AVERT(ScanState, ss);
  AVERT(MRGRefSeg, refseg);
  AVERT(MRG, mrg);

  arena = PoolArena(MRG2Pool(mrg));
  linkseg = refseg->linkSeg;

  nGuardians = MRGGuardiansPerSeg(mrg);
  AVER(nGuardians > 0);
  TRACE_SCAN_BEGIN(ss) {
    for(i=0; i < nGuardians; ++i) {
      refPart = refPartOfIndex(refseg, i);

      /* free guardians are not scanned */
      if (linkOfIndex(linkseg, i)->state != MRGGuardianFREE) {
	ss->wasMarked = TRUE;
	/* .ref.direct: We can access the reference directly */
	/* because we are in a scan and the shield is exposed. */
	if (TRACE_FIX1(ss, refPart->ref)) {
	  res = TRACE_FIX2(ss, &(refPart->ref));
	  if (res != ResOK)
	    return res;
	
	  if (ss->rank == RankFINAL && !ss->wasMarked) { /* .improve.rank */
	    MRGFinalize(arena, linkseg, i);
	  }
	}
      }
    }
  } TRACE_SCAN_END(ss);

  return ResOK;
}


/* MRGInit -- init method for MRG */

static Res MRGInit(Pool pool, va_list args)
{
  MRG mrg;
 
  AVER(pool != NULL); /* Can't check more; see pool contract @@@@ */
  UNUSED(args);
 
  mrg = Pool2MRG(pool);

  RingInit(&mrg->entryRing);
  RingInit(&mrg->freeRing);
  RingInit(&mrg->refRing);
  mrg->extendBy = ArenaAlign(PoolArena(pool));
  mrg->sig = MRGSig;

  AVERT(MRG, mrg);
  EVENT_PPP(PoolInit, pool, PoolArena(pool), ClassOfPool(pool));
  return ResOK;
}


/* MRGFinish -- finish a MRG pool */

static void MRGFinish(Pool pool)
{
  MRG mrg;
  Ring node, nextNode;

  AVERT(Pool, pool);
  mrg = Pool2MRG(pool);
  AVERT(MRG, mrg);

  /* .finish.ring: Before destroying the segments, we isolate the */
  /* rings in the pool structure.  The problem we are avoiding here */
  /* is when the rings point to memory that has been unmapped by one */
  /* segPairDestroy and a subsequent segPairDestroy calls MRGCheck which */
  /* checks the rings which causes the program to fault because */
  /* RingCheck will access unmapped memory. */

  /* We call RingRemove on the master node for the rings, thereby */
  /* effectively emptying them, but leaving the rest of the ring */
  /* "dangling".  This is okay as we are about to destroy all the */
  /* segments so the contents of the rings will dissappear soon. */

  /* .finish.no-final: Note that this relies on the fact that no */
  /* Guardians are in the FINAL state and hence on the Arena Message */
  /* Queue.  We are guaranteed this because MRGFinish is only called */
  /* from ArenaDestroy, and the message queue has been emptied prior */
  /* to the call.  See <code/arena.c#message.queue.empty> */

  if (!RingIsSingle(&mrg->entryRing)) {
    RingRemove(&mrg->entryRing);
  }
  if (!RingIsSingle(&mrg->freeRing)) {
    RingRemove(&mrg->freeRing);
  }

  RING_FOR(node, &mrg->refRing, nextNode) {
    MRGRefSeg refseg = RING_ELT(MRGRefSeg, mrgRing, node);
    MRGSegPairDestroy(refseg, mrg);
  }

  mrg->sig = SigInvalid;
  RingFinish(&mrg->refRing);
  /* <design/poolmrg/#trans.no-finish> */
}


Res MRGRegister(Pool pool, Ref ref)
{
  Ring freeNode;
  Arena arena;
  Link link;
  RefPart refPart;
  MRG mrg;
  Res res;
  MRGRefSeg junk; /* unused */

  AVERT(Pool, pool);
  AVER(ref != 0);

  mrg = Pool2MRG(pool);
  AVERT(MRG, mrg);

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* <design/poolmrg/#alloc.grow> */
  if (RingIsSingle(&mrg->freeRing)) {
    /* .refseg.useless: refseg isn't used */
    /* @@@@ Should the client be able to use the reservoir for this? */
    res = MRGSegPairCreate(&junk, mrg, /* withReservoirPermit */ FALSE);  
    if (res != ResOK)
      return res;
  }
  AVER(!RingIsSingle(&mrg->freeRing));
  freeNode = RingNext(&mrg->freeRing);

  link = linkOfRing(freeNode);
  AVER(link->state == MRGGuardianFREE);
  /* <design/poolmrg/#alloc.pop> */
  RingRemove(freeNode);
  link->state = MRGGuardianPREFINAL;
  RingAppend(&mrg->entryRing, freeNode);

  /* <design/poolmrg/#guardian.ref.alloc> */
  refPart = MRGRefPartOfLink(link, arena);
  MRGRefPartSetRef(arena, refPart, ref);

  return ResOK;
}


/* MRGFree -- free a guardian */

static void MRGFree(Pool pool, Addr old, Size size)
{
  MRG mrg;
  Arena arena;
  Link link;
  RefPart refPart;

  AVERT(Pool, pool);
  AVER(old != (Addr)0);
  AVER(size == sizeof(RefPartStruct));

  mrg = Pool2MRG(pool);
  AVERT(MRG, mrg);

  refPart = (RefPart)old;

  arena = PoolArena(pool);
  AVERT(Arena, arena);

  /* <design/poolmrg/#guardian.ref.free> */
  link = MRGLinkOfRefPart(refPart, arena);
  AVER(link->state == MRGGuardianPOSTFINAL);

  MRGGuardianInit(mrg, link, refPart);
}


/* MRGDescribe -- describe an MRG pool
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
  Res res;

  if (!CHECKT(Pool, pool)) return ResFAIL;
  mrg = Pool2MRG(pool);
  if (!CHECKT(MRG, mrg)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  arena = PoolArena(pool);
  res = WriteF(stream, "  extendBy $W\n", mrg->extendBy, NULL);
  if (res != ResOK) return res;
  res = WriteF(stream, "  Entry queue:\n", NULL);
  if (res != ResOK) return res;
  RING_FOR(node, &mrg->entryRing, nextNode) {
    refPart = MRGRefPartOfLink(linkOfRing(node), arena);
    res = WriteF(stream, "    at $A Ref $A\n",
                 (WriteFA)refPart, (WriteFA)MRGRefPartRef(arena, refPart),
                 NULL);
    if (res != ResOK) return res;
  }

  return ResOK;
}


static Res MRGScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  MRG mrg;
  Res res;
  MRGRefSeg refseg;

  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);

  mrg = Pool2MRG(pool);
  AVERT(MRG, mrg);

  AVER(SegRankSet(seg) == RankSetSingle(RankFINAL)); /* .improve.rank */
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);
  refseg = Seg2RefSeg(seg);
  AVERT(MRGRefSeg, refseg);

  res = MRGRefSegScan(ss, refseg, mrg);
  if (res != ResOK)  {
    *totalReturn = FALSE;
    return res;
  }

  *totalReturn = TRUE;
  return ResOK;
}


DEFINE_POOL_CLASS(MRGPoolClass, this)
{
  INHERIT_CLASS(this, AbstractPoolClass);
  this->name = "MRG";
  this->size = sizeof(MRGStruct);
  this->offset = offsetof(MRGStruct, poolStruct);
  this->attr |= (AttrSCAN | AttrFREE | AttrINCR_RB);
  this->init = MRGInit;
  this->finish = MRGFinish;
  this->free = MRGFree;
  this->grey = PoolTrivGrey;
  this->blacken = PoolTrivBlacken;
  this->scan = MRGScan;
  this->describe = MRGDescribe;
}


PoolClass PoolClassMRG(void)
{
  return MRGPoolClassGet();
}


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

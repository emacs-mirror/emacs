/* poolmrg.c: MANUAL RANK GUARDIAN POOL
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 * 
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

#include "ring.h"
#include "mpm.h"
#include "poolmrg.h"

SRCID(poolmrg, "$Id$");


/* Types */

/* enumerate the states of a guardian */
enum {
  MRGGuardianFREE = 1,
  MRGGuardianPREFINAL,
  MRGGuardianFINAL
};


/* Link -- Unprotectable part of guardian */

typedef struct LinkStruct *Link;
typedef struct LinkStruct {
  int state;                     /* Free, Prefinal, Final */
  union LinkStructUnion {
    MessageStruct messageStruct; /* state = Final */
    RingStruct linkRing;         /* state one of {Free, Prefinal} */
  } the;
} LinkStruct;

#define linkOfMessage(message) \
  PARENT(LinkStruct, the, PARENT(union LinkStructUnion, messageStruct, (message)))

#define linkOfRing(ring) \
  PARENT(LinkStruct, the, PARENT(union LinkStructUnion, linkRing, (ring)))


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

  ref = ArenaPeek(arena, &refPart->ref);
  return ref;
}

static Ref *MRGRefPartRefAddr(RefPart refPart)
{
  AVER(refPart != NULL);

  return &refPart->ref;
}

static void MRGRefPartSetRef(Arena arena, RefPart refPart, Ref ref)
{
  AVER(refPart != NULL);

  ArenaPoke(arena, &refPart->ref, ref);
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

typedef MRG MRGPool;
#define MRGPoolCheck MRGCheck
DECLARE_CLASS(Pool, MRGPool, AbstractPool);


/* MRGCheck -- check an MRG pool */


ATTRIBUTE_UNUSED
static Bool MRGCheck(MRG mrg)
{
  Pool pool = CouldBeA(AbstractPool, mrg);
  CHECKS(MRG, mrg);
  CHECKC(MRGPool, mrg);
  CHECKD(Pool, pool);
  CHECKC(MRGPool, mrg);
  CHECKD_NOSIG(Ring, &mrg->entryRing);
  CHECKD_NOSIG(Ring, &mrg->freeRing);
  CHECKD_NOSIG(Ring, &mrg->refRing);
  CHECKL(mrg->extendBy == ArenaGrainSize(PoolArena(pool)));
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


/* forward declarations */

DECLARE_CLASS(Seg, MRGLinkSeg, Seg);
DECLARE_CLASS(Seg, MRGRefSeg, GCSeg);


/* MRGLinkSegCheck -- check a link segment
 *
 * .link.nullref: During initialization of a link segment the refSeg
 * field will be NULL. This will be initialized when the reference
 * segment is initialized.  See <design/poolmrg/#mrgseg.link.refseg>.
 */

ATTRIBUTE_UNUSED
static Bool MRGLinkSegCheck(MRGLinkSeg linkseg)
{
  Seg seg = CouldBeA(Seg, linkseg);

  CHECKS(MRGLinkSeg, linkseg);
  CHECKD(Seg, seg);
  if (NULL != linkseg->refSeg) { /* see .link.nullref */
    CHECKU(MRGRefSeg, linkseg->refSeg);
    CHECKL(SegPool(seg) == SegPool(CouldBeA(Seg, linkseg->refSeg)));
    CHECKL(linkseg->refSeg->linkSeg == linkseg);
  }
  return TRUE;
}

ATTRIBUTE_UNUSED
static Bool MRGRefSegCheck(MRGRefSeg refseg)
{
  GCSeg gcseg = CouldBeA(GCSeg, refseg);
  Seg seg = CouldBeA(Seg, gcseg);

  CHECKS(MRGRefSeg, refseg);
  CHECKD(GCSeg, gcseg);
  CHECKL(SegPool(seg) == SegPool(CouldBeA(Seg, refseg->linkSeg)));
  CHECKD_NOSIG(Ring, &refseg->mrgRing);
  CHECKD(MRGLinkSeg, refseg->linkSeg);
  CHECKL(refseg->linkSeg->refSeg == refseg);
  return TRUE;
}


/* MRGLinkSegInit -- initialise a link segment */

static Res MRGLinkSegInit(Seg seg, Pool pool, Addr base, Size size,
                          ArgList args)
{
  MRGLinkSeg linkseg;
  Res res;

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, MRGLinkSeg, init)(seg, pool, base, size, args);
  if (res != ResOK)
    return res;
  linkseg = CouldBeA(MRGLinkSeg, seg);

  /* no useful checks for base and size */

  linkseg->refSeg = NULL; /* .link.nullref */

  SetClassOfPoly(seg, CLASS(MRGLinkSeg));
  linkseg->sig = MRGLinkSegSig;
  AVERC(MRGLinkSeg, linkseg);

  return ResOK;
}


/* MRGRefSegInit -- initialise a ref segment */

ARG_DEFINE_KEY(mrg_seg_link_seg, Pointer);
#define mrgKeyLinkSeg (&_mps_key_mrg_seg_link_seg)

static Res MRGRefSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  MRGLinkSeg linkseg;
  MRGRefSeg refseg;
  MRG mrg = MustBeA(MRGPool, pool);
  Res res;
  ArgStruct arg;
  
  /* .ref.initarg: The paired link segment is passed as a keyword
     argument when creating the ref segment. Initially the
     refSeg field of the link segment is NULL (see .link.nullref).
     It's initialized here to the newly initialized ref segment. */
  ArgRequire(&arg, args, mrgKeyLinkSeg);
  linkseg = arg.val.p;

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, MRGRefSeg, init)(seg, pool, base, size, args);
  if (res != ResOK)
    return res;
  refseg = CouldBeA(MRGRefSeg, seg);

  /* no useful checks for base and size */
  AVERT(MRGLinkSeg, linkseg);

  /* <design/seg/#field.rankset.start>, .improve.rank */
  SegSetRankSet(seg, RankSetSingle(RankFINAL));

  RingInit(&refseg->mrgRing);
  RingAppend(&mrg->refRing, &refseg->mrgRing);
  refseg->linkSeg = linkseg;
  AVER(NULL == linkseg->refSeg); /* .link.nullref */

  SetClassOfPoly(seg, CLASS(MRGRefSeg));
  refseg->sig = MRGRefSegSig;
  linkseg->refSeg = refseg;      /* .ref.initarg */

  AVERC(MRGRefSeg, refseg);
  AVERT(MRGLinkSeg, linkseg);

  return ResOK;
}


/* MRGLinkSegClass -- Class definition */

DEFINE_CLASS(Seg, MRGLinkSeg, klass)
{
  INHERIT_CLASS(klass, MRGLinkSeg, Seg);
  SegClassMixInNoSplitMerge(klass);  /* no support for this */
  klass->size = sizeof(MRGLinkSegStruct);
  klass->init = MRGLinkSegInit;
}


/* MRGRefSegClass -- Class definition */

DEFINE_CLASS(Seg, MRGRefSeg, klass)
{
  INHERIT_CLASS(klass, MRGRefSeg, GCSeg);
  SegClassMixInNoSplitMerge(klass);  /* no support for this */
  klass->size = sizeof(MRGRefSegStruct);
  klass->init = MRGRefSegInit;
}


static Count MRGGuardiansPerSeg(MRG mrg)
{
  Count nGuardians;
  AVERT(MRG, mrg);

  nGuardians = mrg->extendBy / sizeof(Ref);
  AVER(nGuardians > 0);

  return nGuardians;
}


/* <design/poolmrg/#guardian.assoc> */


#define refPartOfIndex(refseg, index) \
  ((RefPart)SegBase(MustBeA(Seg, refseg)) + (index))


static RefPart MRGRefPartOfLink(Link link, Arena arena)
{
  Seg seg = NULL;       /* suppress "may be used uninitialized" */
  Bool b;
  Link linkBase;
  Index indx;
  MRGLinkSeg linkseg;

  AVER(link != NULL); /* Better checks done by SegOfAddr */

  b = SegOfAddr(&seg, arena, (Addr)link);
  AVER(b);
  AVERC(MRGPool, SegPool(seg));
  linkseg = MustBeA(MRGLinkSeg, seg);
  linkBase = (Link)SegBase(seg);
  AVER(link >= linkBase);
  indx = (Index)(link - linkBase);
  AVER(indx < MRGGuardiansPerSeg(MustBeA(MRGPool, SegPool(seg))));

  return refPartOfIndex(linkseg->refSeg, indx);
}


#define linkOfIndex(linkseg, index) \
  ((Link)SegBase(MustBeA(Seg, linkseg)) + (index))


#if 0
static Link MRGLinkOfRefPart(RefPart refPart, Arena arena)
{
  Seg seg;
  Bool b;
  RefPart refPartBase;
  Index indx;
  MRGRefSeg refseg;

  AVER(refPart != NULL); /* Better checks done by SegOfAddr */

  b = SegOfAddr(&seg, arena, (Addr)refPart);
  AVER(b);
  AVER(SegPool(seg)->klass == PoolClassMRG());
  refseg = Seg2RefSeg(seg);
  AVERT(MRGRefSeg, refseg);
  refPartBase = (RefPart)SegBase(seg);
  AVER(refPart >= refPartBase);
  indx = refPart - refPartBase;
  AVER(indx < MRGGuardiansPerSeg(PoolMRG(SegPool(seg))));

  return linkOfIndex(refseg->linkSeg, indx);
}
#endif


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
  MRGRefPartSetRef(PoolArena(MustBeA(AbstractPool, mrg)), refPart, 0);
}


/* MRGMessage* -- Implementation of MRG's MessageClass */


/* MRGMessageDelete -- deletes the message (frees up the guardian) */

static void MRGMessageDelete(Message message)
{
  Pool pool = NULL;             /* suppress "may be used uninitialized" */
  Arena arena;
  Link link;
  Bool b;

  AVERT(Message, message);

  arena = MessageArena(message);
  b = PoolOfAddr(&pool, arena, (Addr)message);
  AVER(b);
  AVERC(MRGPool, pool);

  link = linkOfMessage(message);
  AVER(link->state == MRGGuardianFINAL);
  MessageFinish(message);
  MRGGuardianInit(MustBeA(MRGPool, pool), link, MRGRefPartOfLink(link, arena));
}


/* MRGMessageFinalizationRef -- extract the finalized reference from the msg */

static void MRGMessageFinalizationRef(Ref *refReturn,
                                      Arena arena, Message message)
{
  Ref *refp;
  Link link;
  Ref ref;
  RefPart refPart;

  AVER(refReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Message, message);

  AVER(MessageGetType(message) == MessageTypeFINALIZATION);

  link = linkOfMessage(message);
  AVER(link->state == MRGGuardianFINAL);
  refPart = MRGRefPartOfLink(link, arena);

  refp = MRGRefPartRefAddr(refPart);

  /* ensure that the reference is not (white and flipped) */
  ref = ArenaRead(arena, refp);

  AVER(ref != 0);
  *refReturn = ref;
}


static MessageClassStruct MRGMessageClassStruct = {
  MessageClassSig,             /* sig */
  "MRGFinal",                  /* name */
  MessageTypeFINALIZATION,     /* Message Type */
  MRGMessageDelete,            /* Delete */
  MRGMessageFinalizationRef,   /* FinalizationRef */
  MessageNoGCLiveSize,         /* GCLiveSize */   
  MessageNoGCCondemnedSize,    /* GCCondemnedSize */
  MessageNoGCNotCondemnedSize, /* GCNotCondemnedSize */
  MessageNoGCStartWhy,         /* GCStartWhy */
  MessageClassSig              /* <design/message/#class.sig.double> */
};


/* MRGSegPairDestroy --- Destroys a pair of segments (link & ref)
 *
 * .segpair.destroy: We don't worry about the effect that destroying
 * these segs has on any of the pool rings.
 */
static void MRGSegPairDestroy(MRGRefSeg refseg)
{
  RingRemove(&refseg->mrgRing);
  RingFinish(&refseg->mrgRing);
  refseg->sig = SigInvalid;
  SegFree(MustBeA(Seg, refseg->linkSeg));
  SegFree(MustBeA(Seg, refseg));
}


/* MRGSegPairCreate -- create a pair of segments (link & ref) */

static Res MRGSegPairCreate(MRGRefSeg *refSegReturn, MRG mrg)
{
  Pool pool = MustBeA(AbstractPool, mrg);
  Arena arena = PoolArena(pool);
  RefPart refPartBase;
  Count nGuardians;       /* guardians per seg */
  Index i;
  Link linkBase;
  Res res;
  Seg segLink, segRefPart;
  MRGLinkSeg linkseg;
  MRGRefSeg refseg;
  Size linkSegSize;

  AVER(refSegReturn != NULL);

  nGuardians = MRGGuardiansPerSeg(mrg);
  linkSegSize = nGuardians * sizeof(LinkStruct);
  linkSegSize = SizeArenaGrains(linkSegSize, arena);

  res = SegAlloc(&segLink, CLASS(MRGLinkSeg),
                 LocusPrefDefault(), linkSegSize, pool,
                 argsNone);
  if (res != ResOK)
    goto failLinkSegAlloc;
  linkseg = MustBeA(MRGLinkSeg, segLink);
  
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD_FIELD(args, mrgKeyLinkSeg, p, linkseg); /* .ref.initarg */
    res = SegAlloc(&segRefPart, CLASS(MRGRefSeg),
                   LocusPrefDefault(), mrg->extendBy, pool,
                   args);
  } MPS_ARGS_END(args);
  if (res != ResOK)
    goto failRefPartSegAlloc;
  refseg = MustBeA(MRGRefSeg, segRefPart);

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


/* MRGFinalize -- finalize the indexth guardian in the segment */

static void MRGFinalize(Arena arena, MRGLinkSeg linkseg, Index indx)
{
  Link link;
  Message message;

  AVER(indx < MRGGuardiansPerSeg(MustBeA(MRGPool, SegPool(MustBeA(Seg, linkseg)))));

  link = linkOfIndex(linkseg, indx);

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

  arena = PoolArena(MustBeA(AbstractPool, mrg));
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
        ss->scannedSize += sizeof *refPart;
      }
    }
  } TRACE_SCAN_END(ss);

  return ResOK;
}


/* MRGInit -- init method for MRG */

static Res MRGInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  MRG mrg;
  Res res;
 
  AVER(pool != NULL);
  AVERT(ArgList, args);
  UNUSED(args);
  UNUSED(klass); /* used for debug pools only */

  res = NextMethod(Pool, MRGPool, init)(pool, arena, klass, args);
  if (res != ResOK)
    goto failNextInit;
  mrg = CouldBeA(MRGPool, pool);
 
  RingInit(&mrg->entryRing);
  RingInit(&mrg->freeRing);
  RingInit(&mrg->refRing);
  mrg->extendBy = ArenaGrainSize(PoolArena(pool));

  SetClassOfPoly(pool, CLASS(MRGPool));
  mrg->sig = MRGSig;
  AVERC(MRGPool, mrg);

  return ResOK;

failNextInit:
  AVER(res != ResOK);
  return res;
}


/* MRGFinish -- finish a MRG pool */

static void MRGFinish(Inst inst)
{
  Pool pool = MustBeA(AbstractPool, inst);
  MRG mrg = MustBeA(MRGPool, pool);
  Ring node, nextNode;

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
    MRGSegPairDestroy(refseg);
  }

  mrg->sig = SigInvalid;
  RingFinish(&mrg->refRing);
  /* <design/poolmrg/#trans.no-finish> */

  NextMethod(Inst, MRGPool, finish)(inst);
}


/* MRGRegister -- register an object for finalization */

Res MRGRegister(Pool pool, Ref ref)
{
  MRG mrg = MustBeA(MRGPool, pool);
  Arena arena = PoolArena(pool);
  Ring freeNode;
  Link link;
  RefPart refPart;
  Res res;
  MRGRefSeg junk; /* unused */

  AVER(ref != 0);

  /* <design/poolmrg/#alloc.grow> */
  if (RingIsSingle(&mrg->freeRing)) {
    res = MRGSegPairCreate(&junk, mrg);  
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


/* MRGDeregister -- deregister (once) an object for finalization
 *
 * TODO: Definalization loops over all finalizable objects in the heap,
 * and so using it could accidentally be disastrous for performance.
 * See job003953 and back out changelist 187123 if this is fixed.
 */

Res MRGDeregister(Pool pool, Ref obj)
{
  MRG mrg = MustBeA(MRGPool, pool);
  Arena arena = PoolArena(pool);
  Ring node, nextNode;
  Count nGuardians;       /* guardians per seg */

  /* Can't check obj */

  nGuardians = MRGGuardiansPerSeg(mrg);

  /* map over the segments */
  RING_FOR(node, &mrg->refRing, nextNode) {
    MRGRefSeg refSeg = RING_ELT(MRGRefSeg, mrgRing, node);
    MRGLinkSeg linkSeg;
    Count i;
    Link link;
    RefPart refPart;

    AVERT(MRGRefSeg, refSeg);
    linkSeg = refSeg->linkSeg;
    /* map over each guardian in the segment */
    for(i = 0, link = (Link)SegBase(MustBeA(Seg, linkSeg)),
          refPart = (RefPart)SegBase(MustBeA(Seg, refSeg));
        i < nGuardians;
        ++i, ++link, ++refPart) {
      /* check if it's allocated and points to obj */
      if (link->state == MRGGuardianPREFINAL
          && MRGRefPartRef(arena, refPart) == obj) {
        RingRemove(&link->the.linkRing);
        RingFinish(&link->the.linkRing);
        MRGGuardianInit(mrg, link, refPart);
        return ResOK;
      }
    }
  }
  return ResFAIL;
}


/* MRGDescribe -- describe an MRG pool
 *
 * This could be improved by implementing MRGSegDescribe
 * and having MRGDescribe iterate over all the pool's segments.
 */

static Res MRGDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  Pool pool = CouldBeA(AbstractPool, inst);
  MRG mrg = CouldBeA(MRGPool, pool);
  Arena arena;
  Ring node, nextNode;
  RefPart refPart;
  Res res;

  if (!TESTC(MRGPool, mrg))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  res = NextMethod(Inst, MRGPool, describe)(inst, stream, depth);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth + 2, "extendBy $W\n", (WriteFW)mrg->extendBy, NULL);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth + 2, "Entry queue:\n", NULL);
  if (res != ResOK)
    return res;
  arena = PoolArena(pool);
  RING_FOR(node, &mrg->entryRing, nextNode) {
    Bool outsideShield = !ArenaShield(arena)->inside;
    refPart = MRGRefPartOfLink(linkOfRing(node), arena);
    if (outsideShield) {
      ShieldEnter(arena);
    }
    res = WriteF(stream, depth + 2, "at $A Ref $A\n",
                 (WriteFA)refPart, (WriteFA)MRGRefPartRef(arena, refPart),
                 NULL);
    if (outsideShield) {
      ShieldLeave(arena);
    }
    if (res != ResOK)
      return res;
  }

  return ResOK;
}


static Res MRGScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  MRG mrg = MustBeA(MRGPool, pool);
  MRGRefSeg refseg = MustBeA(MRGRefSeg, seg);
  Res res;

  AVERT(ScanState, ss);
  AVER(SegRankSet(seg) == RankSetSingle(RankFINAL)); /* .improve.rank */
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);

  res = MRGRefSegScan(ss, refseg, mrg);
  if (res != ResOK)  {
    *totalReturn = FALSE;
    return res;
  }

  *totalReturn = TRUE;
  return ResOK;
}


DEFINE_CLASS(Pool, MRGPool, klass)
{
  INHERIT_CLASS(klass, MRGPool, AbstractPool);
  klass->instClassStruct.describe = MRGDescribe;
  klass->instClassStruct.finish = MRGFinish;
  klass->size = sizeof(MRGStruct);
  klass->init = MRGInit;
  klass->grey = PoolTrivGrey;
  klass->blacken = PoolTrivBlacken;
  klass->scan = MRGScan;
}


PoolClass PoolClassMRG(void)
{
  return CLASS(MRGPool);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

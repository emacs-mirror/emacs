/* global.c: ARENA-GLOBAL INTERFACES
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .sources: See <design/arena/>.  design.mps.thread-safety is relevant
 * to the functions ArenaEnter and ArenaLeave in this file.
 *
 *
 * TRANSGRESSIONS
 *
 * .static: Static data is used in ArenaAccess (in order to find the
 * appropriate arena) and GlobalsInit.  It's checked in GlobalsCheck.
 * See <design/arena/#static>.
 *
 * .non-mod: The Globals structure has many fields which properly belong
 * to other modules (see <code/mpmst.h>); GlobalsInit contains code which
 * breaks the usual module abstractions.  Such instances are documented
 * with a tag to the relevant module implementation.  Most of the
 * functions should be in some other module, they just ended up here by
 * confusion over naming.  */

#include "dongle.h"
#include "poolmrg.h"
#include "mps.h" /* finalization */
#include "poolmv.h"
#include "mpm.h"


SRCID(global, "$Id$");


/* All static data objects are declared here. See .static */

/* <design/arena/#static.ring.init> */
static Bool arenaRingInit = FALSE;
static RingStruct arenaRing;       /* <design/arena/#static.ring> */


/* ArenaControlPool -- get the control pool */

#define ArenaControlPool(arena) MVPool(&(arena)->controlPoolStruct)


/* arenaClaimRingLock, arenaReleaseRingLock -- lock/release the arena ring
 *
 * See <design/arena/#static.ring.lock>.  */

static void arenaClaimRingLock(void)
{
  LockClaimGlobal();  /* claim the global lock to protect arenaRing */
}

static void arenaReleaseRingLock(void)
{
  LockReleaseGlobal();  /* release the global lock protecting arenaRing */
}


/* arenaAnnounce -- add a new arena into the global ring of arenas
 *
 * On entry, the arena must not be locked (there should be no need,
 * because other threads can't know about it).  On exit, it will be.  */

static void arenaAnnounce(Arena arena)
{
  Globals arenaGlobals;

  /* arena checked in ArenaEnter */

  arenaClaimRingLock();
  ArenaEnter(arena);
  arenaGlobals = ArenaGlobals(arena);
  AVERT(Globals, arenaGlobals);
  RingAppend(&arenaRing, &arenaGlobals->globalRing);
  arenaReleaseRingLock();
}


/* arenaDenounce -- remove an arena from the global ring of arenas
 *
 * After this, no other thread can access the arena through ArenaAccess.
 * On entry, the arena should be locked.  On exit, it will still be, but
 * the lock has been released and reacquired in the meantime, so callers
 * should not assume anything about the state of the arena.  */

static void arenaDenounce(Arena arena)
{
  Globals arenaGlobals;

  AVERT(Arena, arena);

  /* Temporarily give up the arena lock to avoid deadlock, */
  /* see design.mps.thread-safety.deadlock. */
  ArenaLeave(arena);

  /* Detach the arena from the global list. */
  arenaClaimRingLock();
  ArenaEnter(arena);
  arenaGlobals = ArenaGlobals(arena);
  AVERT(Globals, arenaGlobals);
  RingRemove(&arenaGlobals->globalRing);
  arenaReleaseRingLock();
}


/* GlobalsCheck -- check the arena globals */

Bool GlobalsCheck(Globals arenaGlobals)
{
  Arena arena;
  TraceId ti;
  Trace trace;
  Index i;
  Size depth;
  RefSet rs;
  Rank rank;

  CHECKS(Globals, arenaGlobals);
  arena = GlobalsArena(arenaGlobals);
  CHECKL(RingCheck(&arenaGlobals->globalRing));

  CHECKL(MPSVersion() == arenaGlobals->mpsVersionString);

  if (arenaGlobals->lock != NULL)
    CHECKD_NOSIG(Lock, arenaGlobals->lock);

  /* no check possible on pollThreshold */
  CHECKL(BoolCheck(arenaGlobals->insidePoll));
  CHECKL(BoolCheck(arenaGlobals->clamped));
  CHECKL(arenaGlobals->fillMutatorSize >= 0.0);
  CHECKL(arenaGlobals->emptyMutatorSize >= 0.0);
  CHECKL(arenaGlobals->allocMutatorSize >= 0.0);
  CHECKL(arenaGlobals->fillMutatorSize - arenaGlobals->emptyMutatorSize
         >= arenaGlobals->allocMutatorSize);
  CHECKL(arenaGlobals->fillInternalSize >= 0.0);
  CHECKL(arenaGlobals->emptyInternalSize >= 0.0);

  CHECKL(BoolCheck(arenaGlobals->bufferLogging));
  CHECKL(RingCheck(&arenaGlobals->poolRing));
  CHECKL(RingCheck(&arenaGlobals->rootRing));
  CHECKL(RingCheck(&arena->formatRing));
  CHECKL(RingCheck(&arena->messageRing));
  /* Don't check enabledMessageTypes */
  CHECKL(BoolCheck(arena->isFinalPool));
  if (arena->isFinalPool) {
    CHECKD(Pool, arena->finalPool);
  } else {
    CHECKL(arena->finalPool == NULL);
  }

  CHECKL(RingCheck(&arena->threadRing));

  CHECKL(BoolCheck(arena->insideShield));
  CHECKL(arena->shCacheLimit <= ShieldCacheSIZE);
  CHECKL(arena->shCacheI < arena->shCacheLimit);
  CHECKL(BoolCheck(arena->suspended));

  depth = 0;
  for (i = 0; i < arena->shCacheLimit; ++i) {
    Seg seg = arena->shCache[i];
    if (seg != NULL) {
      CHECKD(Seg, seg);
      depth += SegDepth(seg);
    }
  }
  CHECKL(depth <= arena->shDepth);

  CHECKL(TraceSetCheck(arena->busyTraces));
  CHECKL(TraceSetCheck(arena->flippedTraces));
  CHECKL(TraceSetSuper(arena->busyTraces, arena->flippedTraces));

  TRACE_SET_ITER(ti, trace, TraceSetUNIV, arena)
    /* <design/arena/#trace> */
    if (TraceSetIsMember(arena->busyTraces, trace)) {
      CHECKD(Trace, trace);
    } else {
      /* <design/arena/#trace.invalid> */
      CHECKL(trace->sig == SigInvalid);
    }
  TRACE_SET_ITER_END(ti, trace, TraceSetUNIV, arena);
  for(rank = 0; rank < RankLIMIT; ++rank)
    CHECKL(RingCheck(&arena->greyRing[rank]));
  CHECKL(RingCheck(&arena->chainRing));

  /* can't write a check for arena->epoch */

  /* check that each history entry is a subset of the next oldest */
  rs = RefSetEMPTY;
  /* note this loop starts from 1; there is no history age 0 */
  for (i=1; i <= LDHistoryLENGTH; ++ i) {
    /* check history age 'i'; 'j' is the history index. */
    Index j = (arena->epoch + LDHistoryLENGTH - i) % LDHistoryLENGTH;
    CHECKL(RefSetSub(rs, arena->history[j]));
    rs = arena->history[j];
  }
  /* the oldest history entry must be a subset of the prehistory */
  CHECKL(RefSetSub(rs, arena->prehistory));

  /* we also check the statics now. <design/arena/#static.check> */
  CHECKL(BoolCheck(arenaRingInit));
  CHECKL(RingCheck(&arenaRing));

  return TRUE;
}


/* GlobalsInit -- initialize the globals of the arena */

Res GlobalsInit(Globals arenaGlobals)
{
  Arena arena;
  Index i;
  Rank rank;

  /* This is one of the first things that happens, */
  /* so check static consistency here. */
  AVER(MPMCheck());

  if (!DongleTestFull())
    return ResFAIL;

  arenaClaimRingLock();
  /* Ensure static things are initialized. */
  if (!arenaRingInit) {
    /* there isn't an arena ring yet */
    /* <design/arena/#static.init> */
    arenaRingInit = TRUE;
    RingInit(&arenaRing);
    ProtSetup();
  }
  EventInit();
  arenaReleaseRingLock();

  arena = GlobalsArena(arenaGlobals);

  RingInit(&arenaGlobals->globalRing);

  arenaGlobals->lock = NULL;

  arenaGlobals->pollThreshold = 0.0;
  arenaGlobals->insidePoll = FALSE;
  arenaGlobals->clamped = FALSE;
  arenaGlobals->fillMutatorSize = 0.0;
  arenaGlobals->emptyMutatorSize = 0.0;
  arenaGlobals->allocMutatorSize = 0.0;
  arenaGlobals->fillInternalSize = 0.0;
  arenaGlobals->emptyInternalSize = 0.0;

  arenaGlobals->mpsVersionString = MPSVersion();
  arenaGlobals->bufferLogging = FALSE;
  RingInit(&arenaGlobals->poolRing);
  arenaGlobals->poolSerial = (Serial)0;
  RingInit(&arenaGlobals->rootRing);
  arenaGlobals->rootSerial = (Serial)0;

  RingInit(&arena->threadRing);
  arena->threadSerial = (Serial)0;
  RingInit(&arena->formatRing);
  arena->formatSerial = (Serial)0;
  RingInit(&arena->messageRing);
  arena->enabledMessageTypes = NULL;
  arena->isFinalPool = FALSE;
  arena->finalPool = NULL;
  arena->busyTraces = TraceSetEMPTY;    /* <code/trace.c> */
  arena->flippedTraces = TraceSetEMPTY; /* <code/trace.c> */
  arena->insideShield = FALSE;          /* <code/shield.c> */
  arena->shCacheI = (Size)0;
  arena->shCacheLimit = (Size)1;
  arena->shDepth = (Size)0;
  arena->suspended = FALSE;
  for(i = 0; i < ShieldCacheSIZE; i++)
    arena->shCache[i] = NULL;

  for (i=0; i < TraceLIMIT; i++) {
    /* <design/arena/#trace.invalid> */
    arena->trace[i].sig = SigInvalid;
  }
  for(rank = 0; rank < RankLIMIT; ++rank)
    RingInit(&arena->greyRing[rank]);
  STATISTIC(arena->writeBarrierHitCount = 0);
  RingInit(&arena->chainRing);

  arena->epoch = (Epoch)0;              /* <code/ld.c> */
  arena->prehistory = RefSetEMPTY;
  for(i = 0; i < LDHistoryLENGTH; ++i)
    arena->history[i] = RefSetEMPTY;

  arenaGlobals->sig = GlobalsSig;
  AVERT(Globals, arenaGlobals);
  return ResOK;
}


/* GlobalsCompleteCreate -- complete creating the globals of the arena
 *
 * This is like the final initializations in a Create method, except
 * there's no separate GlobalsCreate.  */

Res GlobalsCompleteCreate(Globals arenaGlobals)
{
  Arena arena;
  Res res;
  void *p;

  AVERT(Globals, arenaGlobals);
  arena = GlobalsArena(arenaGlobals);

  /* initialize the message stuff, <design/message/> */
  {
    void *v;

    res = ControlAlloc(&v, arena, BTSize(MessageTypeLIMIT), FALSE);
    if (res != ResOK)
      return res;
    arena->enabledMessageTypes = v;
    BTResRange(arena->enabledMessageTypes, 0, MessageTypeLIMIT);
  }

  res = ControlAlloc(&p, arena, LockSize(), FALSE);
  if (res != ResOK)
    return res;
  arenaGlobals->lock = (Lock)p;
  LockInit(arenaGlobals->lock);

  arenaAnnounce(arena);

  return ResOK;

  /* @@@@ error path */
}


/* GlobalsFinish -- finish the globals of the arena */

void GlobalsFinish(Globals arenaGlobals)
{
  Arena arena;
  Rank rank;

  AVERT(Globals, arenaGlobals);
  arena = GlobalsArena(arenaGlobals);

  STATISTIC_STAT(EVENT_PW(ArenaWriteFaults, arena,
                          arena->writeBarrierHitCount));

  arenaGlobals->sig = SigInvalid;

  RingFinish(&arena->formatRing);
  RingFinish(&arena->messageRing);
  RingFinish(&arena->threadRing);
  for(rank = 0; rank < RankLIMIT; ++rank)
    RingFinish(&arena->greyRing[rank]);
  RingFinish(&arenaGlobals->rootRing);
  RingFinish(&arenaGlobals->poolRing);
  RingFinish(&arenaGlobals->globalRing);
}


/* GlobalsPrepareToDestroy -- prepare to destroy the globals of the arena
 *
 * This is like the final initializations in a Destroy method, except
 * there's no separate GlobalsDestroy.  */

void GlobalsPrepareToDestroy(Globals arenaGlobals)
{
  Arena arena;

  AVERT(Globals, arenaGlobals);

  arena = GlobalsArena(arenaGlobals);
  arenaDenounce(arena);

  LockReleaseMPM(arenaGlobals->lock);
  /* Theoretically, another thread could grab the lock here, but it's */
  /* not worth worrying about, since an attempt after the lock has been */
  /* destroyed would lead to a crash just the same. */
  LockFinish(arenaGlobals->lock);

  /* .message.queue.empty: Empty the queue of messages before */
  /* proceeding to finish the arena.  It is important that this */
  /* is done before destroying the finalization pool as otherwise */
  /* the message queue would have dangling pointers to messages */
  /* whose memory has been unmapped. */
  MessageEmpty(arena);

  /* throw away the BT used by messages */
  if (arena->enabledMessageTypes != NULL) {
    ControlFree(arena, (void *)arena->enabledMessageTypes,
                BTSize(MessageTypeLIMIT));
    arena->enabledMessageTypes = NULL;
  }

  /* destroy the final pool (see <design/finalize/>) */
  if (arena->isFinalPool) {
    /* All this subtlety is because PoolDestroy will call */
    /* ArenaCheck several times.  The invariant on finalPool */
    /* and isFinalPool should hold before, after, and during */
    /* the PoolDestroy call */
    Pool pool = arena->finalPool;

    arena->isFinalPool = FALSE;
    arena->finalPool = NULL;
    PoolDestroy(pool);
  }
}


/* ArenaEnter -- enter the state where you can look at the arena */

#if defined(THREAD_SINGLE) && defined(PROTECTION_NONE)
void (ArenaEnter)(Arena arena)
{
  /* Don't need to lock, just check. */
  AVERT(Arena, arena);
}
#else
void ArenaEnter(Arena arena)
{
  AVER(CHECKT(Arena, arena));

  StackProbe(StackProbeDEPTH);
  LockClaim(ArenaGlobals(arena)->lock);
  AVERT(Arena, arena); /* can't AVER it until we've got the lock */
  ShieldEnter(arena);
}
#endif


/* ArenaLeave -- leave the state where you can look at MPM data structures */

#if defined(THREAD_SINGLE) && defined(PROTECTION_NONE)
void (ArenaLeave)(Arena arena)
{
  /* Don't need to lock, just check. */
  AVERT(Arena, arena);
}
#else
void ArenaLeave(Arena arena)
{
  AVERT(Arena, arena);
  ShieldLeave(arena);
  ProtSync(arena);              /* <design/prot/#if.sync> */
  LockReleaseMPM(ArenaGlobals(arena)->lock);
}
#endif


/* mps_exception_info -- pointer to exception info
 *
 * This is a hack to make exception info easier to find in a release
 * version.  The format is platform-specific.  We won't necessarily
 * publish this.  */

MutatorFaultContext mps_exception_info = NULL;


/* ArenaAccess -- deal with an access fault
 *
 * This is called when a protected address is accessed.  The mode
 * corresponds to which mode flags need to be cleared in order for the
 * access to continue.  */

Bool ArenaAccess(Addr addr, AccessSet mode, MutatorFaultContext context)
{
  Seg seg;
  Ring node, nextNode;
  Res res;

  arenaClaimRingLock();    /* <design/arena/#lock.ring> */
  mps_exception_info = context;
  AVER(RingCheck(&arenaRing));

  RING_FOR(node, &arenaRing, nextNode) {
    Globals arenaGlobals = RING_ELT(Globals, globalRing, node);
    Arena arena = GlobalsArena(arenaGlobals);
    Root root;

    ArenaEnter(arena);     /* <design/arena/#lock.arena> */
    /* @@@@ The code below assumes that Roots and Segs are disjoint. */
    /* It will fall over (in TraceSegAccess probably) if there is a */
    /* protected root on a segment. */
    /* It is possible to overcome this restriction. */
    if (SegOfAddr(&seg, arena, addr)) {
      mps_exception_info = NULL;
      arenaReleaseRingLock();
      /* An access in a different thread may have already caused the
       * protection to be cleared.  This avoids calling TraceAccess on
       * protection that has already been cleared on a separate thread.  */
      mode &= SegPM(seg);
      if (mode != AccessSetEMPTY) {
        res = PoolAccess(SegPool(seg), seg, addr, mode, context);
        AVER(res == ResOK); /* Mutator can't continue unless this succeeds */
      }
      ArenaLeave(arena);
      return TRUE;
    } else if (RootOfAddr(&root, arena, addr)) {
      mps_exception_info = NULL;
      arenaReleaseRingLock();
      mode &= RootPM(root);
      if (mode != AccessSetEMPTY)
        RootAccess(root, mode);
      ArenaLeave(arena);
      return TRUE;
    }

    ArenaLeave(arena);
  }

  mps_exception_info = NULL;
  arenaReleaseRingLock();
  return FALSE;
}


/* ArenaPoll -- trigger periodic actions
 *
 * Poll all background activities to see if they need to do anything.
 * ArenaPoll does nothing if the amount of committed memory is less than
 * the arena poll threshold.  This means that actions are taken as the
 * memory demands increase.
 *
 * @@@@ This is where time is "stolen" from the mutator in addition
 * to doing what it asks and servicing accesses.  This is where the
 * amount of time should be controlled, perhaps by passing time
 * limits to the various other activities.
 *
 * @@@@ Perhaps this should be based on a process table rather than a
 * series of manual steps for looking around.  This might be worthwhile
 * if we introduce background activities other than tracing.  */

#ifdef MPS_PROD_EPCORE
void (ArenaPoll)(Globals globals)
{
  /* Don't poll, just check. */
  AVERT(Globals, globals);
}
#else
void ArenaPoll(Globals globals)
{
  double size;

  AVERT(Globals, globals);

  if (!DONGLE_TEST_QUICK()) {
    /* Cripple it by deleting the control pool. */
    GlobalsArena(globals)->poolReady = FALSE; /* suppress check */
    PoolFinish(ArenaControlPool(GlobalsArena(globals)));
    return;
  }
  if (globals->clamped)
    return;
  size = globals->fillMutatorSize;
  if (globals->insidePoll || size < globals->pollThreshold)
    return;

  globals->insidePoll = TRUE;

  (void)ArenaStep(globals, 0.0);

  globals->insidePoll = FALSE;
}
#endif

Bool ArenaStep(Globals globals, double interval)
{
  double size;
  Bool b;

  UNUSED(interval);

  AVERT(Globals, globals);

  b = TracePoll(globals);

  size = globals->fillMutatorSize;
  globals->pollThreshold = size + ArenaPollALLOCTIME;
  AVER(globals->pollThreshold > size); /* enough precision? */

  return b;
}

/* ArenaFinalize -- registers an object for finalization
 *
 * See <design/finalize/>.  */

Res ArenaFinalize(Arena arena, Ref obj)
{
  Res res;

  AVERT(Arena, arena);
  /* Could consider checking that Ref is valid. */

  if (!arena->isFinalPool) {
    Pool pool;

    res = PoolCreate(&pool, arena, PoolClassMRG());
    if (res != ResOK)
      return res;
    arena->finalPool = pool;
    arena->isFinalPool = TRUE;
  }
  AVER(arena->isFinalPool);

  res = MRGRegister(arena->finalPool, (Ref)obj);
  return res;
}


/* Peek / Poke */

Ref ArenaPeek(Arena arena, Addr addr)
{
  Seg seg;
  Bool b;

  AVERT(Arena, arena);

  b = SegOfAddr(&seg, arena, addr);
  if (b) {
    return ArenaPeekSeg(arena, seg, addr);
  } else {
    Ref ref;
    ref = *(Ref *)addr;
    return ref;
  }
}

Ref ArenaPeekSeg(Arena arena, Seg seg, Addr addr)
{
  Ref ref;

  AVERT(Arena, arena);
  AVERT(Seg, seg);

  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  /* Consider checking addr's alignment using seg->pool->alignment */

  ShieldExpose(arena, seg);
  ref = *(Ref *)addr;
  ShieldCover(arena, seg);
  return ref;
}

void ArenaPoke(Arena arena, Addr addr, Ref ref)
{
  Seg seg;
  Bool b;

  AVERT(Arena, arena);
  /* Can't check addr as it is arbitrary */
  /* Can't check ref as it is arbitrary */

  b = SegOfAddr(&seg, arena, addr);
  if (b) {
    ArenaPokeSeg(arena, seg, addr, ref);
  } else {
    *(Ref *)addr = ref;
  }
}

void ArenaPokeSeg(Arena arena, Seg seg, Addr addr, Ref ref)
{
  RefSet summary;

  AVERT(Arena, arena);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  /* Consider checking addr's alignment using seg->pool->alignment */
  /* ref is arbitrary and can't be checked */

  ShieldExpose(arena, seg);
  *(Ref *)addr = ref;
  summary = SegSummary(seg);
  summary = RefSetAdd(arena, summary, (Addr)ref);
  SegSetSummary(seg, summary);
  ShieldCover(arena, seg);
}


/* ArenaRead -- read a single reference, possibly through a barrier
 *
 * This forms part of a software barrier.  It provides fine-grain access
 * to single references in segments.  */

Ref ArenaRead(Arena arena, Addr addr)
{
  Bool b;
  Seg seg;

  AVERT(Arena, arena);

  b = SegOfAddr(&seg, arena, addr);
  AVER(b == TRUE);

  /* .read.flipped: We AVER that the reference that we are reading */
  /* refers to an object for which all the traces that the object is */
  /* white for are also flipped.  This is because we don't have any */
  /* write-barrier (in the sense of write-barrier collectors) */
  /* mechanism in place for reading (strictly speaking, writing */
  /* it somewhere after having read it) references that are white. */
  AVER(TraceSetSub(SegWhite(seg), arena->flippedTraces));

  /* .read.conservative: @@@@ Should scan at rank phase-of-trace, */
  /* not RankEXACT which is conservative.  See also */
  /* <code/trace.c#scan.conservative> for a similar nasty. */
  TraceScanSingleRef(arena->flippedTraces, RankEXACT, arena,
                     seg, (Ref *)addr);
  /* get the possibly fixed reference */
  return ArenaPeekSeg(arena, seg, addr);
}


/* GlobalsDescribe -- describe the arena globals */

Res GlobalsDescribe(Globals arenaGlobals, mps_lib_FILE *stream)
{
  Res res;
  Arena arena;
  Ring node, nextNode;
  Index i;

  if (!CHECKT(Globals, arenaGlobals)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  arena = GlobalsArena(arenaGlobals);
  res = WriteF(stream,
	       "  mpsVersion $S\n", arenaGlobals->mpsVersionString,
               "  lock $P\n", (WriteFP)arenaGlobals->lock,
               "  pollThreshold $U kB\n",
               (WriteFU)(arenaGlobals->pollThreshold / 1024),
               arenaGlobals->insidePoll ? "inside poll\n" : "outside poll\n",
               arenaGlobals->clamped ? "clamped\n" : "released\n",
               "  fillMutatorSize $U kB\n",
                 (WriteFU)(arenaGlobals->fillMutatorSize / 1024),
               "  emptyMutatorSize $U kB\n",
                 (WriteFU)(arenaGlobals->emptyMutatorSize / 1024),
               "  allocMutatorSize $U kB\n",
                 (WriteFU)(arenaGlobals->allocMutatorSize / 1024),
               "  fillInternalSize $U kB\n",
                 (WriteFU)(arenaGlobals->fillInternalSize / 1024),
               "  emptyInternalSize $U kB\n",
                 (WriteFU)(arenaGlobals->emptyInternalSize / 1024),
               "  poolSerial $U\n", (WriteFU)arenaGlobals->poolSerial,
               "  rootSerial $U\n", (WriteFU)arenaGlobals->rootSerial,
               "  formatSerial $U\n", (WriteFU)arena->formatSerial,
               "  threadSerial $U\n", (WriteFU)arena->threadSerial,
               arena->insideShield ? "inside shield\n" : "outside shield\n",
               "  busyTraces    $B\n", (WriteFB)arena->busyTraces,
               "  flippedTraces $B\n", (WriteFB)arena->flippedTraces,
               /* @@@@ no TraceDescribe function */
               "  epoch $U\n", (WriteFU)arena->epoch,
               NULL);
  if (res != ResOK) return res;

  for(i=0; i < LDHistoryLENGTH; ++ i) {
    res = WriteF(stream,
                 "    history[$U] = $B\n", i, arena->history[i],
                 NULL);
    if (res != ResOK) return res;
  }

  res = WriteF(stream,
               "    [note: indices are raw, not rotated]\n"
               "    prehistory = $B\n", (WriteFB)arena->prehistory,
               NULL);
  if (res != ResOK) return res;

  res = WriteF(stream,
               "  suspended $S\n", arena->suspended ? "YES" : "NO",
               "  shDepth $U\n", arena->shDepth,
               "  shCacheI $U\n", arena->shCacheI,
               /* @@@@ should SegDescribe the cached segs? */
               NULL);
  if (res != ResOK) return res;

  res = RootsDescribe(arenaGlobals, stream);
  if (res != ResOK) return res;

  RING_FOR(node, &arenaGlobals->poolRing, nextNode) {
    Pool pool = RING_ELT(Pool, arenaRing, node);
    res = PoolDescribe(pool, stream);
    if (res != ResOK) return res;
  }

  RING_FOR(node, &arena->formatRing, nextNode) {
    Format format = RING_ELT(Format, arenaRing, node);
    res = FormatDescribe(format, stream);
    if (res != ResOK) return res;
  }

  RING_FOR(node, &arena->threadRing, nextNode) {
    Thread thread = ThreadRingThread(node);
    res = ThreadDescribe(thread, stream);
    if (res != ResOK) return res;
  }

  /* @@@@ What about grey rings? */
  return res;
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

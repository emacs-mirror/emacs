/* impl.c.global: ARENA-GLOBAL INTERFACES
 *
 * $HopeName: MMsrc!global.c(trunk.5) $
 * Copyright (C) 2001 Harlequin Limited.  All rights reserved.
 *
 * .sources: See design.mps.arena.  design.mps.thread-safety is relevant
 * to the functions ArenaEnter and ArenaLeave in this file.
 *
 * .non-mod: The Arena structure has many fields which properly belong
 * to other modules (see impl.h.mpmst); ArenaCreate contains code which
 * breaks the usual module abstractions.  Such instances are documented
 * with a tag to the relevant module implementation.  Most of the
 * functions should be in some other module, they just ended up here by
 * confusion over naming.
 *
 *
 * TRANSGRESSIONS
 *
 * .static: Static data is used in ArenaAccess (in order to find the
 * appropriate arena) and ArenaCreate (in order to get a fresh serial
 * number).  See design.mps.arena.static.
 */

#include "dongle.h"
#include "poolmrg.h"
#include "mps.h" /* finalization */
#include "poolmv.h"
#include "mpm.h"


SRCID(global, "$HopeName: MMsrc!global.c(trunk.5) $");


/* All static data objects are declared here. See .static */

/* design.mps.arena.static.ring.init */
static Bool arenaRingInit = FALSE; 
static RingStruct arenaRing;       /* design.mps.arena.static.ring */
static Serial arenaSerial;         /* design.mps.arena.static.serial */


/* ArenaControlPool -- get the control pool */

#define ArenaControlPool(arena) MVPool(&(arena)->controlPoolStruct)


/* arenaClaimRingLock, arenaReleaseRingLock -- lock/release the arena ring
 *
 * See design.mps.arena.static.ring.lock.
 */

static void arenaClaimRingLock(void)
{
  LockClaimGlobal();  /* claim the global lock to protect arenaRing */
}

static void arenaReleaseRingLock(void)
{
  LockReleaseGlobal();  /* release the global lock protecting arenaRing */
}


/* ArenaCheck -- check the arena */

Bool ArenaCheck(Arena arena)
{
  TraceId ti;
  Trace trace;
  Index i;
  Size depth;
  RefSet rs;
  Rank rank;

  CHECKS(Arena, arena);
  /* design.mps.arena.static.serial */
  CHECKL(arena->serial < arenaSerial); 
  CHECKL(RingCheck(&arena->globalRing));

  CHECKL(MPSVersion() == arena->mpsVersionString);

  CHECKD_NOSIG(Lock, arena->lock);

  /* no check possible on arena->pollThreshold */
  CHECKL(BoolCheck(arena->insidePoll));

  CHECKL(BoolCheck(arena->bufferLogging));

  CHECKL(ArenaAllocCheck(arena));

  CHECKL(RingCheck(&arena->poolRing));
  CHECKL(RingCheck(&arena->rootRing));
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
  CHECKL(arena->shCacheLimit <= SHIELD_CACHE_SIZE);
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
    /* design.mps.arena.trace */
    if (TraceSetIsMember(arena->busyTraces, trace)) {
      CHECKD(Trace, trace);
    } else {
      /* design.mps.arena.trace.invalid */
      CHECKL(trace->sig == SigInvalid);
    }
  TRACE_SET_ITER_END(ti, trace, TraceSetUNIV, arena);
  for(rank = 0; rank < RankLIMIT; ++rank)
    CHECKL(RingCheck(&arena->greyRing[rank]));
  CHECKL(BoolCheck(arena->clamped));
  CHECKL(RingCheck(&arena->chainRing));

  /* can't write a check for arena->epoch */

  /* check that each history entry is a subset of the next oldest */
  rs = RefSetEMPTY;
  /* note this loop starts from 1; there is no history age 0 */
  for (i=1; i <= ARENA_LD_LENGTH; ++ i) {
    /* check history age 'i'; 'j' is the history index. */
    Index j = (arena->epoch + ARENA_LD_LENGTH - i) % ARENA_LD_LENGTH;
    CHECKL(RefSetSub(rs, arena->history[j]));
    rs = arena->history[j];
  }
  /* the oldest history entry must be a subset of the prehistory */
  CHECKL(RefSetSub(rs, arena->prehistory));

  /* we also check the statics now. design.mps.arena.static.check */
  CHECKL(BoolCheck(arenaRingInit));
  CHECKL(RingCheck(&arenaRing));
  /* can't check arenaSerial */

  return TRUE;
}


/* ArenaInit -- initialize the generic part of the arena
 *
 * .init.caller: Unlike PoolInit, this is called by the class init
 * methods, not the generic Create.  This is because the class is
 * responsible for allocating the descriptor.
 */

void ArenaInit(Arena arena, Lock lock, ArenaClass class)
{
  Index i;
  Rank rank;

  /* We do not check the arena argument, because it's uninitialized. */
  /* Likewise lock. */
  AVERT(ArenaClass, class);

  RingInit(&arena->globalRing);
  ArenaAllocInit(arena, class);
  arena->mpsVersionString = MPSVersion();

  LockInit(lock);
  arena->lock = lock;

  RingInit(&arena->poolRing);
  arena->poolSerial = (Serial)0;
  RingInit(&arena->rootRing);
  arena->rootSerial = (Serial)0;
  RingInit(&arena->threadRing);
  arena->threadSerial = (Serial)0;
  RingInit(&arena->formatRing);
  arena->formatSerial = (Serial)0;
  RingInit(&arena->messageRing);
  arena->enabledMessageTypes = NULL;
  arena->isFinalPool = FALSE;
  arena->finalPool = NULL;
  arena->busyTraces = TraceSetEMPTY;    /* impl.c.trace */
  arena->flippedTraces = TraceSetEMPTY; /* impl.c.trace */
  arena->insideShield = FALSE;          /* impl.c.shield */
  arena->shCacheI = (Size)0;
  arena->shCacheLimit = (Size)1;
  arena->shDepth = (Size)0;
  arena->suspended = FALSE;
  for(i = 0; i < SHIELD_CACHE_SIZE; i++)
    arena->shCache[i] = NULL;
  arena->pollThreshold = 0.0;
  arena->insidePoll = FALSE;
  arena->bufferLogging = FALSE;
  for (i=0; i < TraceLIMIT; i++) {
    /* design.mps.arena.trace.invalid */
    arena->trace[i].sig = SigInvalid;   
  }
  for(rank = 0; rank < RankLIMIT; ++rank)
    RingInit(&arena->greyRing[rank]);
  STATISTIC(arena->writeBarrierHitCount = 0);
  arena->clamped = FALSE;
  RingInit(&arena->chainRing);

  arena->epoch = (Epoch)0;              /* impl.c.ld */
  arena->prehistory = RefSetEMPTY;
  for(i = 0; i < ARENA_LD_LENGTH; ++i)
    arena->history[i] = RefSetEMPTY;

  arena->sig = ArenaSig;
  arena->serial = arenaSerial;  /* design.mps.arena.static.serial */
  ++arenaSerial;
  
  AVERT(Arena, arena);
}


/* ArenaCreateV -- create and bootstrap the arena */

Res ArenaCreateV(Arena *arenaReturn, ArenaClass class, va_list args)
{
  Res res;
  Arena arena;

  AVER(MPMCheck());
  AVER(arenaReturn != NULL);
  AVERT(ArenaClass, class);

  if (!DongleTestFull())
    return ResFAIL;
  arenaClaimRingLock();
  EventInit();
  if (!arenaRingInit) {
    /* there isn't an arena ring yet */
    /* design.mps.arena.static.init */
    arenaRingInit = TRUE;
    RingInit(&arenaRing);
    arenaSerial = (Serial)0;
    ProtSetup();
  }

  res = ArenaAllocCreate(&arena, class, args);
  if (res != ResOK)
    goto failInit;

  ArenaEnter(arena);
  AVERT(Arena, arena);

  /* initialize the reservoir, design.mps.reservoir */
  res = ReservoirInit(&arena->reservoirStruct, arena);
  if (res != ResOK) 
    goto failReservoirInit;

  res = ControlInit(arena);
  if (res != ResOK) 
    goto failControlInit;

  /* initialize the message stuff, design.mps.message */
  {
    void *v;

    res = ControlAlloc(&v, arena, BTSize(MessageTypeLIMIT), FALSE);
    if (res != ResOK)
      goto failEnabledBTAlloc;
    arena->enabledMessageTypes = v;
    BTResRange(arena->enabledMessageTypes, 0, MessageTypeLIMIT);
  }

  /* Add initialized arena to the global list of arenas. */
  RingAppend(&arenaRing, &arena->globalRing);
  arenaReleaseRingLock();

  *arenaReturn = arena;
  return ResOK;

failEnabledBTAlloc:
  ControlFinish(arena);
failControlInit:
  ReservoirFinish(&arena->reservoirStruct);
failReservoirInit:
  (*class->finish)(arena);
failInit:
  arenaReleaseRingLock();

  return res;
}


/* ArenaFinish -- finish the generic part of the arena
 *
 * .finish.caller: Unlike PoolFinish, this is called by the class finish
 * methods, not the generic Destroy.  This is because the class is
 * responsible for deallocating the descriptor.
 */

void ArenaFinish(Arena arena)
{
  Rank rank;

  STATISTIC_STAT(EVENT_PW(ArenaWriteFaults, arena,
                          arena->writeBarrierHitCount));

  arena->sig = SigInvalid;
  LockFinish(arena->lock);
  RingFinish(&arena->poolRing);
  RingFinish(&arena->formatRing);
  RingFinish(&arena->messageRing);
  RingFinish(&arena->rootRing);
  RingFinish(&arena->threadRing);
  RingFinish(&arena->globalRing);
  for(rank = 0; rank < RankLIMIT; ++rank)
    RingFinish(&arena->greyRing[rank]);
  ArenaAllocFinish(arena);
}


/* ArenaDestroy -- deallocate and destroy the arena */

void ArenaDestroy(Arena arena)
{
  Reservoir reservoir;

  AVERT(Arena, arena);
  AVER(!arena->insidePoll);
  reservoir = ArenaReservoir(arena);
  AVERT(Reservoir, reservoir);

  /* Empty the reservoir - see .reservoir.finish */
  ReservoirSetLimit(reservoir, 0);

  /* Temporarily give up the arena lock to avoid deadlock */
  /* see design.mps.arena.lock.avoid.conflict */
  ArenaLeave(arena);

  /* Detach the arena from the global list. */
  arenaClaimRingLock();
  RingRemove(&arena->globalRing);
  arenaReleaseRingLock();

  /* Reclaim the arena lock and re-test assumptions */
  ArenaEnter(arena);
  AVERT(Arena, arena);
  AVER(!arena->insidePoll);

  /* throw away the BT used by messages */
  if (arena->enabledMessageTypes != NULL) {
    ControlFree(arena, (void *)arena->enabledMessageTypes, 
                BTSize(MessageTypeLIMIT));
    arena->enabledMessageTypes = NULL;
  }

  /* .message.queue.empty: Empty the queue of messages before */
  /* proceeding to finish the arena.  It is important that this */
  /* is done before destroying the finalization pool as otherwise */
  /* the message queue would have dangling pointers to messages */
  /* whose memory has been unmapped. */
  MessageEmpty(arena);

  /* destroy the final pool (see design.mps.finalize) */
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

  /* Destroy the control pool & reservoir pool. */
  arena->poolReady = FALSE;
  ControlFinish(arena);
  ReservoirFinish(reservoir);

  ArenaLeave(arena);

  ArenaAllocDestroy(arena);

  EventFinish();
}


/* ArenaEnter -- enter the state where you can look at MPM data structures */

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

  StackProbe(STACK_PROBE_DEPTH);
  LockClaim(arena->lock);
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
  ProtSync(arena);              /* design.mps.prot.if.sync */
  LockReleaseMPM(arena->lock);
}
#endif


/* mps_exception_info -- pointer to exception info
 *
 * This is a hack to make exception info easier to find in a release
 * version.  The format is platform-specific.  We won't necessarily
 * publish this.
 */

MutatorFaultContext mps_exception_info = NULL;


/* ArenaAccess -- deal with an access fault
 *
 * This is called when a protected address is accessed.  The mode
 * corresponds to which mode flags need to be cleared in order
 * for the access to continue.
 */

Bool ArenaAccess(Addr addr, AccessSet mode, MutatorFaultContext context)
{
  Seg seg;
  Ring node, nextNode;
  Res res;

  arenaClaimRingLock();    /* design.mps.arena.lock.ring */
  mps_exception_info = context;
  AVER(RingCheck(&arenaRing));

  RING_FOR(node, &arenaRing, nextNode) {
    Arena arena = RING_ELT(Arena, globalRing, node);
    Root root;

    ArenaEnter(arena);     /* design.mps.arena.lock.arena */
    AVERT(Arena, arena);   /* can't AVER until we've got the lock */
    /* @@@@ The code below assumes that Roots and Segs are disjoint. */
    /* It will fall over (in TraceSegAccess probably) if there is a */
    /* protected root on a segment. */
    /* It is possible to overcome this restriction. */
    if (SegOfAddr(&seg, arena, addr)) {
      mps_exception_info = NULL;
      arenaReleaseRingLock();
      /* An access in a different thread may have already caused
       * the protection to be cleared.  This avoids calling
       * TraceAccess on protection that has already been cleared on
       * a separate thread.
       */
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
 * ArenaPoll does nothing if the amount of committed memory is less
 * than the arena poll threshold.  This means that actions are taken
 * as the memory demands increase.
 * 
 * @@@@ This is where time is "stolen" from the mutator in addition
 * to doing what it asks and servicing accesses.  This is where the
 * amount of time should be controlled, perhaps by passing time
 * limits to the various other activities.
 *
 * @@@@ Perhaps this should be based on a process table rather than
 * a series of manual steps for looking around.  This might be
 * worthwhile if we introduce background activities other than
 * tracing.
 */

#ifdef MPS_PROD_EPCORE
void (ArenaPoll)(Arena arena)
{
  /* Don't poll, just check. */
  AVERT(Arena, arena);
}
#else
void ArenaPoll(Arena arena)
{
  double size;

  AVERT(Arena, arena);

  if (!DONGLE_TEST_QUICK()) {
    /* Cripple it by deleting the control pool. */
    arena->poolReady = FALSE; /* suppress check */
    PoolFinish(ArenaControlPool(arena));
    return;
  }
  if (arena->clamped)
    return;
  size = arena->fillMutatorSize;
  if (arena->insidePoll || size < arena->pollThreshold)
    return;

  arena->insidePoll = TRUE;

  TracePoll(arena);

  size = arena->fillMutatorSize;
  arena->pollThreshold = size + ArenaPollALLOCTIME;
  AVER(arena->pollThreshold > size); /* enough precision? */

  arena->insidePoll = FALSE;
}
#endif


/* ArenaFinalize -- registers an object for finalization
 *
 * See design.mps.finalize.
 */

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
 * to single references in segments.
 */

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
  /* impl.c.trace.scan.conservative for a similar nasty. */
  TraceScanSingleRef(arena->flippedTraces, RankEXACT, arena,
                     seg, (Ref *)addr);
  /* get the possibly fixed reference */
  return ArenaPeekSeg(arena, seg, addr);
}


/* ArenaDescribe -- describe the arena */

Res ArenaDescribe(Arena arena, mps_lib_FILE *stream)
{
  Res res;
  Ring node, nextNode;
  Index i;

  if (!CHECKT(Arena, arena)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "Arena $P ($U) {\n",    
               (WriteFP)arena, (WriteFU)arena->serial,
               "  class $P (\"$S\")\n", 
               (WriteFP)arena->class, arena->class->name,
	       "  mpsVersion $S\n",
	       arena->mpsVersionString,
               NULL);
  if (res != ResOK) return res;

  res = ArenaAllocDescribe(arena, stream);
  if (res != ResOK) return res;

  res = WriteF(stream,
               "  lock $P\n",          (WriteFP)arena->lock,
               "  pollThreshold $U KB\n",
               (WriteFU)(arena->pollThreshold / 1024),
               "  insidePoll $S\n",    arena->insidePoll ? "YES" : "NO",
               "  poolSerial $U\n", (WriteFU)arena->poolSerial,
               "  rootSerial $U\n", (WriteFU)arena->rootSerial,
               "  formatSerial $U\n", (WriteFU)arena->formatSerial,
               "  threadSerial $U\n", (WriteFU)arena->threadSerial,
               "  insideShield $S\n",  
               arena->insideShield ? "YES" : "NO",
               "  busyTraces    $B\n", (WriteFB)arena->busyTraces,
               "  flippedTraces $B\n", (WriteFB)arena->flippedTraces,
               /* @@@@ no TraceDescribe function */
               "  epoch $U\n",         (WriteFU)arena->epoch,
               NULL);
  if (res != ResOK) return res;

  for(i=0; i < ARENA_LD_LENGTH; ++ i) {
    res = WriteF(stream,
                 "    history[$U] = $B\n", i, arena->history[i],
                 NULL);
    if (res != ResOK) return res;
  }
  
  res = WriteF(stream,
               "    [note: indices are raw, not rotated]\n"
               "    prehistory = $B\n",    (WriteFB)arena->prehistory,
               NULL);
  if (res != ResOK) return res;

  res = WriteF(stream,
               "  suspended $S\n", arena->suspended ? "YES" : "NO",
               "  shDepth $U\n", arena->shDepth,
               "  shCacheI $U\n", arena->shCacheI,
               /* @@@@ should SegDescribe the cached segs? */
               NULL);
  if (res != ResOK) return res;

  RING_FOR(node, &arena->rootRing, nextNode) {
    Root root = RING_ELT(Root, arenaRing, node);
    res = RootDescribe(root, stream);
    if (res != ResOK) return res;
  }

  RING_FOR(node, &arena->poolRing, nextNode) {
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

  res = WriteF(stream,
               "} Arena $P ($U)\n", (WriteFP)arena, 
               (WriteFU)arena->serial,
               NULL);
  return res;
}

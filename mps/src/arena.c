/* impl.c.arena: ARENA IMPLEMENTATION
 *
 * $HopeName: !arena.c(trunk.71) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * .intro: This is the implementation of Arenas.
 *
 * .purpose: An arena is an instantiation of the memory manager.
 *
 * .sources: design.mps.arena is the main design document.
 * design.mps.thread-safety is relevant to the functions ArenaEnter and
 * ArenaLeave in this file.
 *
 * .req: The arena is required to support all per-instantiation
 * behaviour of the memory manager.
 *
 * NOTES
 *
 * .non-mod: The Arena structure has many fields which properly belong
 * to other modules (see impl.h.mpmst); ArenaCreate contains code which
 * breaks the usual module abstractions.  Such instances are documented
 * with a tag to the relevant module implementation.
 *
 * TRANSGRESSIONS
 *
 * .static: Static data is used in ArenaAccess (in order to find the
 * appropriate arena) and ArenaCreate (in order to get a fresh serial
 * number). See design.mps.arena.static.
 */

#include "mpm.h"
#include "dongle.h"

/* finalization */
#include "poolmrg.h"
#include "mps.h"

SRCID(arena, "$HopeName: !arena.c(trunk.71) $");


/* All static data objects are declared here. See .static */

/* design.mps.arena.static.ring.init */
static Bool arenaRingInit = FALSE;
static RingStruct arenaRing;       /* design.mps.arena.static.ring */
static Serial arenaSerial;         /* design.mps.arena.static.serial */


/* ArenaControlPool -- get the control pool */

#define ArenaControlPool(arena) MVPool(&(arena)->controlPoolStruct)


/* Functions to lock the arena ring. design.mps.arena.static.ring.lock */

static void arenaClaimRingLock(void)
{
  LockClaimGlobal();  /* claim the global lock to protect arenaRing */
}

static void arenaReleaseRingLock(void)
{
  LockReleaseGlobal();  /* release the global lock protecting arenaRing */
}


/* ArenaReservoir - return the reservoir for the arena */

Reservoir ArenaReservoir(Arena arena)
{
  AVERT(Arena, arena);
  return &arena->reservoirStruct;
}


/* AbstractArenaClass  -- The abstact arena class definition
 *
 * .null: Most abstract class methods are set to NULL.
 * See design.mps.arena.class.abstract.null.
 */

typedef ArenaClassStruct AbstractArenaClassStruct;

DEFINE_CLASS(AbstractArenaClass, class)
{
  INHERIT_CLASS(&class->protocol, ProtocolClass);
  class->name = "ABSARENA";
  class->size = 0;
  class->offset = 0;
  class->init = NULL;
  class->finish = NULL;
  class->reserved = NULL;
  class->committed = NULL;
  class->spareCommitExceeded = ArenaNoSpareCommitExceeded;
  class->extend = ArenaNoExtend;
  class->retract = ArenaNoRetract;
  class->isReserved = NULL;
  class->alloc = NULL;
  class->free = NULL;
  class->tractOfAddr = NULL;
  class->tractFirst = NULL;
  class->tractNext = NULL;
  class->tractNextContig = NULL;
  class->describe = ArenaTrivDescribe;
  class->sig = ArenaClassSig;
}


/* ArenaClassCheck -- check the consistency of an arena class */

Bool ArenaClassCheck(ArenaClass class)
{
  CHECKL(ProtocolClassCheck(&class->protocol));
  CHECKL(class->name != NULL); /* Should be <=6 char C identifier */
  CHECKL(class->size >= sizeof(ArenaStruct));
  /* Offset of generic Pool within class-specific instance cannot be */
  /* greater than the size of the class-specific portion of the */
  /* instance */
  CHECKL(class->offset <= (size_t)(class->size - sizeof(ArenaStruct)));
  CHECKL(FUNCHECK(class->init));
  CHECKL(FUNCHECK(class->finish));
  CHECKL(FUNCHECK(class->reserved));
  CHECKL(FUNCHECK(class->committed));
  CHECKL(FUNCHECK(class->extend));
  CHECKL(FUNCHECK(class->retract));
  CHECKL(FUNCHECK(class->alloc));
  CHECKL(FUNCHECK(class->free));
  CHECKL(FUNCHECK(class->tractOfAddr));
  CHECKL(FUNCHECK(class->tractFirst));
  CHECKL(FUNCHECK(class->tractNext));
  CHECKL(FUNCHECK(class->describe));
  CHECKS(ArenaClass, class);
  return TRUE;
}


/* ArenaCheck -- check the consistency of a generic arena structure */

Bool ArenaCheck(Arena arena)
{
  TraceId ti;
  Index i;
  Size depth;
  RefSet rs;
  Rank rank;

  CHECKS(Arena, arena);
  /* design.mps.arena.static.serial */
  CHECKL(arena->serial < arenaSerial);
  CHECKD(ArenaClass, arena->class);
  CHECKL(RingCheck(&arena->globalRing));
  CHECKL(MPSVersion() == arena->mpsVersionString);

  CHECKL(BoolCheck(arena->poolReady));
  if(arena->poolReady) {               /* design.mps.arena.pool.ready */
    CHECKD(MV, &arena->controlPoolStruct);
    CHECKD(Reservoir, &arena->reservoirStruct);
  }
  /* Can't check that limit>=size because we may call ArenaCheck */
  /* while the size is being adjusted. */

  CHECKL(LockCheck(arena->lock));

  /* no check possible on arena->pollThreshold */
  CHECKL(BoolCheck(arena->insidePoll));
  CHECKL(BoolCheck(arena->clamped));

  CHECKL(BoolCheck(arena->bufferLogging));
  CHECKL(arena->fillMutatorSize >= 0.0);
  CHECKL(arena->emptyMutatorSize >= 0.0);
  CHECKL(arena->allocMutatorSize >= 0.0);
  CHECKL(arena->fillMutatorSize - arena->emptyMutatorSize >=
         arena->allocMutatorSize);
  CHECKL(arena->fillInternalSize >= 0.0);
  CHECKL(arena->emptyInternalSize >= 0.0);
  /* commitLimit is arbitrary, can't be checked. */
  /* (it's probably >= ArenaCommitted(), but we can't call that */
  /* due to recursion problems) */

  CHECKL(ShiftCheck(arena->zoneShift));
  CHECKL(AlignCheck(arena->alignment));

  CHECKL(RingCheck(&arena->poolRing));
  CHECKL(RingCheck(&arena->rootRing));
  CHECKL(RingCheck(&arena->formatRing));
  CHECKL(RingCheck(&arena->messageRing));
  /* Don't check enabledMessageTypes */
  CHECKL(BoolCheck(arena->isFinalPool));
  if(arena->isFinalPool) {
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
  for (i=0; i < arena->shCacheLimit; ++i) {
    Seg seg = arena->shCache[i];
    if (seg != (Seg)0) {
      CHECKL(SegCheck(seg));
      depth += SegDepth(seg);
    }
  }
  CHECKL(depth <= arena->shDepth);

  CHECKL(TraceSetCheck(arena->busyTraces));
  CHECKL(TraceSetCheck(arena->flippedTraces));
  CHECKL(TraceSetSuper(arena->busyTraces, arena->flippedTraces));

  for(ti = 0; ti < TRACE_MAX; ++ti) {
    /* design.mps.arena.trace */
    if(TraceSetIsMember(arena->busyTraces, ti)) {
      Trace trace = ArenaTrace(arena, ti);
      CHECKD(Trace,trace);
    } else {
      /* design.mps.arena.trace.invalid */
      CHECKL(ArenaTrace(arena,ti)->sig == SigInvalid);
    }
  }

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

  for(rank = 0; rank < RankMAX; ++rank)
    CHECKL(RingCheck(&arena->greyRing[rank]));

  if (NULL == arena->lastTract) {
    CHECKL(NULL == arena->lastTractBase);
  } else {
    CHECKL(TractBase(arena->lastTract) == arena->lastTractBase);
  }

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

  /* We do not check the arena argument, because it's _supposed_ to */
  /* point to an uninitialized block of memory. */
  AVERT(ArenaClass, class);

  arena->class = class;
  RingInit(&arena->globalRing);
  arena->mpsVersionString = MPSVersion();
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
  for (i=0; i < TRACE_MAX; i++) {
    /* design.mps.arena.trace.invalid */
    arena->trace[i].sig = SigInvalid;
  }
  LockInit(lock);
  arena->lock = lock;
  arena->insideShield = FALSE;          /* impl.c.shield */
  arena->shCacheI = (Size)0;
  arena->shCacheLimit = (Size)1;
  arena->shDepth = (Size)0;
  arena->suspended = FALSE;
  for(i = 0; i < SHIELD_CACHE_SIZE; i++)
    arena->shCache[i] = (Seg)0;
  arena->pollThreshold = 0.0;
  arena->insidePoll = FALSE;
  arena->clamped = FALSE;
  arena->epoch = (Epoch)0;              /* impl.c.ld */
  arena->prehistory = RefSetEMPTY;
  for(i = 0; i < ARENA_LD_LENGTH; ++i)
    arena->history[i] = RefSetEMPTY;
  arena->bufferLogging = FALSE;
  arena->fillMutatorSize = 0.0;
  arena->emptyMutatorSize = 0.0;
  arena->allocMutatorSize = 0.0;
  arena->fillInternalSize = 0.0;
  arena->emptyInternalSize = 0.0;
  /* commitLimit may be overridden by init (but probably not */
  /* as there's not much point) */
  arena->commitLimit = (Size)-1;
  arena->spareCommitted = (Size)0;
  arena->spareCommitLimit = ARENA_INIT_SPARE_COMMIT_LIMIT;
  /* alignment is usually overridden by init */
  arena->alignment = MPS_PF_ALIGN;
  /* usually overridden by init */
  arena->zoneShift = ARENA_ZONESHIFT;
  arena->poolReady = FALSE;     /* design.mps.arena.pool.ready */
  for(rank = 0; rank < RankMAX; ++rank)
    RingInit(&arena->greyRing[rank]);
  STATISTIC(arena->writeBarrierHitCount = 0);
  arena->lastTract = NULL;
  arena->lastTractBase = NULL;

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
  if(!arenaRingInit) {
    /* there isn't an arena ring yet */
    /* design.mps.arena.static.init */
    arenaRingInit = TRUE;
    RingInit(&arenaRing);
    arenaSerial = (Serial)0;
    ProtSetup();
  }

  /* Do initialization.  This will call ArenaInit (see .init.caller). */
  res = (*class->init)(&arena, class, args);
  if(res != ResOK)
    goto failInit;

  ArenaEnter(arena);
  AVERT(Arena, arena);

  /* initialize the reservoir, design.mps.reservoir */
  res = ReservoirInit(&arena->reservoirStruct, arena);
  if(res != ResOK)
    goto failReservoirInit;

  /* design.mps.arena.pool.init */
  res = PoolInit(ArenaControlPool(arena), arena, PoolClassMV(),
                 ARENA_CONTROL_EXTENDBY, ARENA_CONTROL_AVGSIZE,
                 ARENA_CONTROL_MAXSIZE);
  if(res != ResOK)
    goto failControlInit;
  arena->poolReady = TRUE;      /* design.mps.arena.pool.ready */

  /* initialize the message stuff, design.mps.message */
  {
    void *v;

    res = ControlAlloc(&v, arena, BTSize(MessageTypeMAX),
                       /* withReservoirPermit */ FALSE);
    if(res != ResOK)
      goto failEnabledBTAlloc;
    arena->enabledMessageTypes = v;
    BTResRange(arena->enabledMessageTypes, 0, MessageTypeMAX);
  }

  /* Add initialized arena to the global list of arenas. */
  RingAppend(&arenaRing, &arena->globalRing);
  arenaReleaseRingLock();

  *arenaReturn = arena;
  return ResOK;

failEnabledBTAlloc:
  PoolFinish(ArenaControlPool(arena));
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
  for(rank = 0; rank < RankMAX; ++rank)
    RingFinish(&arena->greyRing[rank]);
}


/* ArenaDestroy -- deallocate and destroy the arena */

void ArenaDestroy(Arena arena)
{
  ArenaClass class;
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

  class = arena->class;

  /* throw away the BT used by messages */
  if(arena->enabledMessageTypes != NULL) {
    ControlFree(arena, (void *)arena->enabledMessageTypes,
                BTSize(MessageTypeMAX));
    arena->enabledMessageTypes = NULL;
  }

  /* .message.queue.empty: Empty the queue of messages before */
  /* proceeding to finish the arena.  It is important that this */
  /* is done before destroying the finalization pool as otherwise */
  /* the message queue would have dangling pointers to messages */
  /* whose memory has been unmapped. */
  MessageEmpty(arena);

  /* destroy the final pool (see design.mps.finalize) */
  if(arena->isFinalPool) {
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
  PoolFinish(ArenaControlPool(arena));
  ReservoirFinish(reservoir);

  ArenaLeave(arena);

  /* Call class-specific finishing.  This will call ArenaFinish. */
  (*class->finish)(arena);

  EventFinish();
}


/* ArenaEnter -- enter the state where you can look at MPM data
 *               structures
 */

#if defined(THREAD_SINGLE) && defined(PROTECTION_NONE)
void (ArenaEnter)(Arena arena)
{
  /* Don't need to lock, just check. */
  AVERT(Arena, arena);
}
#else
void ArenaEnter(Arena arena)
{
  AVER(arena != NULL);
  AVER(arena->sig == ArenaSig);

  StackProbe(STACK_PROBE_DEPTH);
  LockClaim(arena->lock);
  AVERT(Arena, arena); /* can't AVER it until we've got the lock */
  ShieldEnter(arena);
}
#endif


/* ArenaLeave -- leave the state where you can look at MPM data
 *               structures
 */

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

MutatorFaultContext mps_exception_info;


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

  mps_exception_info = context;
  arenaClaimRingLock();    /* design.mps.arena.lock.ring */
  RING_FOR(node, &arenaRing, nextNode) {
    Arena arena = RING_ELT(Arena, globalRing, node);
    Root root;

    ArenaEnter(arena);     /* design.mps.arena.lock.arena */
    AVERT(Arena, arena);   /* can't AVER until we've got the lock */
    /* @@@@ The code below assumes that Roots and Segs are disjoint. */
    /* It will fall over (in TraceSegAccess probably) if there is a */
    /* protected root on a segment. */
    /* It is possible to overcome this restriction. */
    if(SegOfAddr(&seg, arena, addr)) {
      arenaReleaseRingLock();
      /* An access in a different thread may have already caused
       * the protection to be cleared.  This avoids calling
       * TraceAccess on protection that has already been cleared on
       * a separate thread.
       */
      mode &= SegPM(seg);
      if(mode != AccessSetEMPTY) {
        res = PoolAccess(SegPool(seg), seg, addr, mode, context);
        AVER(res == ResOK); /* Mutator can't continue unless this succeeds */
      }
      ArenaLeave(arena);
      return TRUE;
    } else if(RootOfAddr(&root, arena, addr)) {
      arenaReleaseRingLock();
      mode &= RootPM(root);
      if(mode != AccessSetEMPTY)
        RootAccess(root, mode);
      ArenaLeave(arena);
      return TRUE;
    }

    ArenaLeave(arena);
  }

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
  if(arena->clamped)
    return;
  size = arena->fillMutatorSize;
  if(arena->insidePoll || size < arena->pollThreshold)
    return;

  arena->insidePoll = TRUE;

  ArenaStep(arena);

  arena->insidePoll = FALSE;
}
#endif


void ArenaClamp(Arena arena)
{
  AVERT(Arena, arena);
  arena->clamped = TRUE;
}

void ArenaRelease(Arena arena)
{
  AVERT(Arena, arena);
  arena->clamped = FALSE;
}

void ArenaPark(Arena arena)
{
  TraceId ti;

  AVERT(Arena, arena);

  arena->clamped = TRUE;

  while(arena->busyTraces != TraceSetEMPTY) {
    /* Poll active traces to make progress. */
    for(ti = 0; ti < TRACE_MAX; ++ti)
      if(TraceSetIsMember(arena->busyTraces, ti)) {
        Trace trace = ArenaTrace(arena, ti);

        TracePoll(trace);

        if(trace->state == TraceFINISHED)
          TraceDestroy(trace);
      }
  }
}


/* Take a single step of any active trace. */

void ArenaStep(Arena arena)
{
    TraceId ti;
    double size;

    AVERT(Arena, arena);

    /* Poll actions to see if any new action is to be taken. */
    ActionPoll(arena);

    if (arena->busyTraces != TraceSetEMPTY) {
        /* Find an active trace to poll. */
        for(ti = 0; ti < TRACE_MAX; ++ti) {
            if(TraceSetIsMember(arena->busyTraces, ti)) {
                Trace trace = ArenaTrace(arena, ti);
                TracePoll(trace);
                if(trace->state == TraceFINISHED)
                    TraceDestroy(trace);
            }
        }
    }

    /* set poll threshold so we don't steal any more mutator time too soon. */
    size = arena->fillMutatorSize;
    arena->pollThreshold = size + ARENA_POLL_MAX;
    AVER(arena->pollThreshold > size); /* enough precision? */
}


Res ArenaCollect(Arena arena)
{
  Trace trace;
  Res res;
  Ring poolNode, nextPoolNode;

  AVERT(Arena, arena);
  ArenaPark(arena);

  res = TraceCreate(&trace, arena);
  /* should be a trace available -- we're parked */
  AVER(res != ResLIMIT);
  if(res != ResOK)
    goto failCreate;

  /* Identify the condemned set and turn it white. */
  RING_FOR(poolNode, ArenaPoolRing(arena), nextPoolNode) {
    Pool pool = RING_ELT(Pool, arenaRing, poolNode);
    Ring segNode, nextSegNode;

    if((pool->class->attr & AttrGC) != 0) {
      res = PoolTraceBegin(pool, trace);
      if(res != ResOK)
        goto failBegin;

      RING_FOR(segNode, PoolSegRing(pool), nextSegNode) {
        Seg seg = SegOfPoolRing(segNode);

        res = TraceAddWhite(trace, seg);
        if(res != ResOK)
          goto failAddWhite;
      }
    }
  }

  TraceStart(trace, 0.0, 0.0);

  ArenaPark(arena);

  return ResOK;

failAddWhite:
  NOTREACHED; /* @@@@ Would leave white sets inconsistent. */
failBegin:
  TraceDestroy(trace);
failCreate:
  return res;
}


/* ArenaDescribe -- describe the arena
 *
 * See design.mps.describe.
 */

Res ArenaDescribe(Arena arena, mps_lib_FILE *stream)
{
  Res res;
  Ring node, nextNode;
  Index i;

  if(!CHECKT(Arena, arena))
    return ResFAIL;
  if(stream == NULL)
    return ResFAIL;

  res = WriteF(stream,
               "Arena $P ($U) {\n",
               (WriteFP)arena, (WriteFU)arena->serial,
               "  class $P (\"$S\")\n",
               (WriteFP)arena->class, arena->class->name,
	       "  mpsVersion $S\n",
	       arena->mpsVersionString,
               "  poolReady $S\n",     arena->poolReady ? "YES" : "NO",
               "  controlPool $P\n",
               (WriteFP)&arena->controlPoolStruct,
               "  lock $P\n",          (WriteFP)arena->lock,
               "  pollThreshold $U KB\n",
               (WriteFU)(arena->pollThreshold / 1024),
               "  insidePoll $S\n",    arena->insidePoll ? "YES" : "NO",
               "  fillMutatorSize $U KB\n",
                 (WriteFU)(arena->fillMutatorSize / 1024),
               "  emptyMutatorSize $U KB\n",
                 (WriteFU)(arena->emptyMutatorSize / 1024),
               "  allocMutatorSize $U KB\n",
                 (WriteFU)(arena->allocMutatorSize / 1024),
               "  fillInternalSize $U KB\n",
                 (WriteFU)(arena->fillInternalSize / 1024),
               "  emptyInternalSize $U KB\n",
                 (WriteFU)(arena->emptyInternalSize / 1024),
               NULL);
  if(res != ResOK)
    return res;

  res = WriteF(stream,
               "  commitLimit $W\n", (WriteFW)arena->commitLimit,
	       "  spareCommitted $W\n", (WriteFW)arena->spareCommitted,
	       "  spareCommitLimit $W\n", (WriteFW)arena->spareCommitLimit,
               "  zoneShift $U\n", (WriteFU)arena->zoneShift,
               "  alignment $W\n", (WriteFW)arena->alignment,
               "  poolSerial $U\n", (WriteFU)arena->poolSerial,
               "  rootSerial $U\n", (WriteFU)arena->rootSerial,
               "  formatSerial $U\n", (WriteFU)arena->formatSerial,
               "  threadSerial $U\n", (WriteFU)arena->threadSerial,
               "  insideShield $S\n",
               arena->insideShield ? "YES" : "NO",
               "  busyTraces    $B\n", (WriteFB)arena->busyTraces,
               "  flippedTraces $B\n", (WriteFB)arena->flippedTraces,
               "    (no TraceDescribe function)\n",
               "  epoch $U\n",         (WriteFU)arena->epoch,
               NULL);
  if(res != ResOK)
    return res;

  res = (*arena->class->describe)(arena, stream);
  if(res != ResOK)
    return res;

  for(i=0; i < ARENA_LD_LENGTH; ++ i) {
    res = WriteF(stream,
                 "    history[$U] = $B\n", i, arena->history[i],
                 NULL);
    if(res != ResOK)
      return res;
  }

  res = WriteF(stream,
               "    [note: indices are raw, not rotated]\n"
               "    prehistory = $B\n",    (WriteFB)arena->prehistory,
               NULL);
  if(res != ResOK)
    return res;

  res = WriteF(stream,
               "  suspended $S\n", arena->suspended ? "YES" : "NO",
               "  shDepth $U\n", arena->shDepth,
               "  shCacheI $U\n", arena->shCacheI,
               "    (no SegDescribe function)\n",
               NULL);
  if(res != ResOK)
    return res;

  RING_FOR(node, &arena->rootRing, nextNode) {
    Root root = RING_ELT(Root, arenaRing, node);
    res = RootDescribe(root, stream);
    if(res != ResOK)
      return res;
  }

  RING_FOR(node, &arena->poolRing, nextNode) {
    Pool pool = RING_ELT(Pool, arenaRing, node);
    res = PoolDescribe(pool, stream);
    if(res != ResOK)
      return res;
  }

  RING_FOR(node, &arena->formatRing, nextNode) {
    Format format = RING_ELT(Format, arenaRing, node);
    res = FormatDescribe(format, stream);
    if(res != ResOK)
      return res;
  }

  RING_FOR(node, &arena->threadRing, nextNode) {
    Thread thread = ThreadRingThread(node);
    res = ThreadDescribe(thread, stream);
    if(res != ResOK)
      return res;
  }

  /* @@@@ What about grey rings? */

  res = WriteF(stream,
               "} Arena $P ($U)\n", (WriteFP)arena,
               (WriteFU)arena->serial,
               NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}

Res ArenaDescribeTracts(Arena arena, mps_lib_FILE *stream)
{
  Res res;
  Tract tract;
  Bool b;
  Addr oldLimit, base, limit;
  Size size;

  if(!CHECKT(Arena, arena))
    return ResFAIL;
  if(stream == NULL)
    return ResFAIL;

  b = TractFirst(&tract, arena);
  oldLimit = TractBase(tract);
  while(b) {
    base = TractBase(tract);
    limit = TractLimit(tract);
    size = ArenaAlign(arena);

    if(TractBase(tract) > oldLimit) {
      res = WriteF(stream,
                   "[$P, $P) $W $U   ---\n",
                   (WriteFP)oldLimit,
                   (WriteFP)base,
                   (WriteFW)AddrOffset(oldLimit, base),
                   (WriteFU)AddrOffset(oldLimit, base),
                   NULL);
      if(res != ResOK)
        return res;
    }

    res = WriteF(stream,
                 "[$P, $P) $W $U   $P ($S)\n",
                 (WriteFP)base,
                 (WriteFP)limit,
                 (WriteFW)size,
                 (WriteFW)size,
                 (WriteFP)TractPool(tract),
                 (WriteFS)(TractPool(tract)->class->name),
                 NULL);
    if(res != ResOK)
      return res;
    b = TractNext(&tract, arena, TractBase(tract));
    oldLimit = limit;
  }
  return ResOK;
}


/* ControlAlloc -- allocate a small block directly from the control pool
 *
 * .arena.control-pool: Actually the block will be allocated from the
 * control pool, which is an MV pool embedded in the arena itself.
 *
 * .controlalloc.addr: In implementations where Addr is not compatible
 * with void* (design.mps.type.addr.use), ControlAlloc must take care of
 * allocating so that the block can be addressed with a void*.
 */

Res ControlAlloc(void **baseReturn, Arena arena, size_t size,
                 Bool withReservoirPermit)
{
  Addr base;
  Res res;

  AVERT(Arena, arena);
  AVER(baseReturn != NULL);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  res = PoolAlloc(&base, ArenaControlPool(arena), (Size)size,
                  withReservoirPermit);
  if(res != ResOK)
    return res;

  *baseReturn = (void *)base; /* see .controlalloc.addr */
  return ResOK;
}


/* ControlFree -- free a block allocated using ControlAlloc */

void ControlFree(Arena arena, void* base, size_t size)
{
  AVERT(Arena, arena);
  AVER(base != NULL);
  AVER(size > 0);

  PoolFree(ArenaControlPool(arena), (Addr)base, (Size)size);
}


/* ArenaAlloc -- allocate some tracts from the arena */

Res ArenaAlloc(Addr *baseReturn, SegPref pref, Size size, Pool pool,
               Bool withReservoirPermit)
{
  Res res;
  Arena arena;
  Addr base;
  Tract baseTract;
  Reservoir reservoir;

  AVER(baseReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);
  AVER(BoolCheck(withReservoirPermit));

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(size, arena->alignment));
  reservoir = ArenaReservoir(arena);
  AVERT(Reservoir, reservoir);

  res = ReservoirEnsureFull(reservoir);
  if (res != ResOK) {
    AVER(ResIsAllocFailure(res));
    if (!withReservoirPermit)
      return res;
  }

  res = (*arena->class->alloc)(&base, &baseTract, pref, size, pool);
  if(res == ResOK) {
    goto goodAlloc;
  } else if(withReservoirPermit) {
    AVER(ResIsAllocFailure(res));
    res = ReservoirWithdraw(&base, &baseTract, reservoir, size, pool);
    if(res == ResOK)
      goto goodAlloc;
  }
  EVENT_PWP(ArenaAllocFail, arena, size, pool);
  return res;

goodAlloc:
  /* cache the tract - design.mps.arena.tract.cache */
  arena->lastTract = baseTract;
  arena->lastTractBase = base;

  EVENT_PPAWP(ArenaAlloc, arena, baseTract, base, size, pool);
  *baseReturn = base;
  return ResOK;
}


/* ArenaFree -- free some tracts to the arena */

void ArenaFree(Addr base, Size size, Pool pool)
{
  Arena arena;
  Addr limit;
  Reservoir reservoir;
  Res res;

  AVERT(Pool, pool);
  AVER(base != NULL);
  AVER(size > (Size)0);
  arena = PoolArena(pool);
  AVERT(Arena, arena);
  reservoir = ArenaReservoir(arena);
  AVERT(Reservoir, reservoir);
  AVER(AddrIsAligned(base, arena->alignment));
  AVER(SizeIsAligned(size, arena->alignment));

  /* uncache the tract if in range - design.mps.arena.tract.uncache */
  limit = AddrAdd(base, size);
  if ((arena->lastTractBase >= base) && (arena->lastTractBase < limit)) {
    arena->lastTract = NULL;
    arena->lastTractBase = NULL;
  }

  res = ReservoirEnsureFull(reservoir);
  if (res == ResOK) {
    (*arena->class->free)(base, size, pool);
  } else {
    AVER(ResIsAllocFailure(res));
    ReservoirDeposit(reservoir, base, size);
  }

  EVENT_PAW(ArenaFree, arena, base, size);
  return;
}


Size ArenaReserved(Arena arena)
{
  AVERT(Arena, arena);
  return (*arena->class->reserved)(arena);
}


Size ArenaCommitted(Arena arena)
{
  AVERT(Arena, arena);
  return (*arena->class->committed)(arena);
}

Size ArenaSpareCommitted(Arena arena)
{
  AVERT(Arena, arena);
  return arena->spareCommitted;
}

Size ArenaSpareCommitLimit(Arena arena)
{
  AVERT(Arena, arena);
  return arena->spareCommitLimit;
}

void ArenaSetSpareCommitLimit(Arena arena, Size limit)
{
  AVERT(Arena, arena);
  /* Can't check limit, as all possible values are allowed. */

  arena->spareCommitLimit = limit;
  if(arena->spareCommitLimit < arena->spareCommitted) {
    arena->class->spareCommitExceeded(arena);
  }

  EVENT_PW(SpareCommitLimitSet, arena, limit);
  return;
}

/* Used by arenas which don't use spare committed memory */
void ArenaNoSpareCommitExceeded(Arena arena)
{
  AVERT(Arena, arena);
  return;
}


Size ArenaCommitLimit(Arena arena)
{
  AVERT(Arena, arena);
  return arena->commitLimit;
}

Res ArenaSetCommitLimit(Arena arena, Size limit)
{
  Size committed;
  Res res;

  AVERT(Arena, arena);
  AVER(ArenaCommitted(arena) <= arena->commitLimit);

  committed = ArenaCommitted(arena);
  if(limit < committed) {
    /* Attempt to set the limit below current committed */
    if (limit >= committed - arena->spareCommitted) {
      /* could set the limit by flushing any spare committed memory */
      arena->class->spareCommitExceeded(arena);
      AVER(limit >= ArenaCommitted(arena));
      arena->commitLimit = limit;
      res = ResOK;
    } else {
      res = ResFAIL;
    }
  } else {
    arena->commitLimit = limit;
    res = ResOK;
  }
  EVENT_PWU(CommitLimitSet, arena, limit, (res == ResOK));
  return res;
}


double ArenaMutatorAllocSize(Arena arena)
{
  AVERT(Arena, arena);
  return arena->fillMutatorSize - arena->emptyMutatorSize;
}


Res ArenaExtend(Arena arena, Addr base, Size size)
{
  Res res;

  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > 0);

  res = (*arena->class->extend)(arena, base, size);
  if(res != ResOK)
    return res;

  EVENT_PAW(ArenaExtend, arena, base, size);

  return ResOK;
}


Res ArenaRetract(Arena arena, Addr base, Size size)
{
  Res res;

  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > 0);

  res = (*arena->class->retract)(arena, base, size);
  if(res != ResOK)
    return res;

  EVENT_PAW(ArenaRetract, arena, base, size);

  return ResOK;
}


Bool ArenaIsReservedAddr(Arena arena, Addr addr)
{
  AVERT(Arena, arena);
  /* addr is arbitrary */

  return (*arena->class->isReserved)(arena, addr);
}


/* ArenaNoExtend -- fail to extend the arena by a chunk */

Res ArenaNoExtend(Arena arena, Addr base, Size size)
{
  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > (Size)0);

  NOTREACHED;
  return ResUNIMPL;
}


/* ArenaNoRetract -- fail to retract a chunk from the arena */

Res ArenaNoRetract(Arena arena, Addr base, Size size)
{
  AVERT(Arena, arena);
  AVER(base != (Addr)0);
  AVER(size > (Size)0);

  NOTREACHED;
  return ResUNIMPL;
}


/* ArenaTrivDescribe -- produce trivial description of an arena */

Res ArenaTrivDescribe(Arena arena, mps_lib_FILE *stream)
{
  AVERT(Arena, arena);
  AVER(stream != NULL);

  return WriteF(stream,
    "  No class-specific description available.\n", NULL);
}


/* SegPrefCheck -- check the consistency of a segment preference */

Bool SegPrefCheck(SegPref pref)
{
  CHECKS(SegPref, pref);
  CHECKL(BoolCheck(pref->high));
  /* refSet can't be checked because it's an arbitrary bit pattern. */
  CHECKL(BoolCheck(pref->isGen));
  CHECKL(BoolCheck(pref->isCollected));
  /* gen is an arbitrary serial */
  return TRUE;
}


/* SegPrefDefault -- return a segment preference representing the
 *                   defaults
 */

static SegPrefStruct segPrefDefault = {
  SegPrefSig,                           /* sig */
  ARENA_DEFAULT_SEG_HIGH,               /* high */
  ARENA_DEFAULT_REFSET,                 /* refSet */
  FALSE,                                /* isCollected */
  FALSE,                                /* isGen */
  (Serial)0,                            /* gen */
};

SegPref SegPrefDefault(void)
{
  return &segPrefDefault;
}


/* SegPrefExpress -- express a segment preference */

Res SegPrefExpress(SegPref pref, SegPrefKind kind, void *p)
{
  AVERT(SegPref, pref);
  AVER(pref != &segPrefDefault);

  switch(kind) {
  case SegPrefHigh:
    AVER(p == NULL);
    pref->high = TRUE;
    break;

  case SegPrefLow:
    AVER(p == NULL);
    pref->high = FALSE;
    break;

  case SegPrefRefSet:
    AVER(p != NULL);
    pref->refSet = *(RefSet *)p;
    break;

  case SegPrefCollected:
    AVER(p == NULL);
    pref->isCollected = TRUE;
    break;

  case SegPrefGen:
    AVER(p != NULL);
    pref->isGen = TRUE;
    pref->gen = *(Serial *)p;
    break;

  default:
    /* Unknown kinds are ignored for binary compatibility. */
    /* See design.mps.pref. */
    break;
  }

  return ResOK;
}


/* ArenaFinalize -- registers an object for finalization
 *
 * See design.mps.finalize.
 */

Res ArenaFinalize(Arena arena, Ref obj)
{
  Res res;

  AVERT(Arena, arena);
  /* Could consider checking that Ref is valid. */

  if(!arena->isFinalPool) {
    Pool pool;

    res = PoolCreate(&pool, arena, PoolClassMRG());
    if(res != ResOK) {
      return res;
    }
    arena->finalPool = pool;
    arena->isFinalPool = TRUE;
  }
  AVER(arena->isFinalPool);

  res = MRGRegister(arena->finalPool, (Ref)obj);
  if(res != ResOK) {
    return res;
  }

  return ResOK;
}


/* Has Addr */

Bool ArenaHasAddr(Arena arena, Addr addr)
{
  Seg seg;

  AVERT(Arena, arena);
  return SegOfAddr(&seg, arena, addr);
}


/* Peek / Poke */

Ref ArenaPeek(Arena arena, Addr addr)
{
  Seg seg;
  Bool b;

  AVERT(Arena, arena);

  b = SegOfAddr(&seg, arena, addr);
  if(b) {
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
  if(b) {
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

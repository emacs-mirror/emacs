/* impl.c.arena: ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arena.c(trunk.39) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 *
 * .readership: Any MPS developer
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

/* finalization */
#include "poolmrg.h"
#include "mps.h"

SRCID(arena, "$HopeName: MMsrc!arena.c(trunk.39) $");


/* All static data objects are declared here. See .static */

/* design.mps.arena.static.ring.init */
static Bool arenaRingInit = FALSE; 
static RingStruct arenaRing;       /* design.mps.arena.static.ring */
/* design.mps.arena.static.ring.lock */
static LockStruct arenaRingLock;   
static Serial arenaSerial;         /* design.mps.arena.static.serial */

#define SegArena(seg) PoolArena(SegPool(seg))


/* ArenaClassCheck -- check the consistency of an arena class */

Bool ArenaClassCheck(ArenaClass class)
{
  CHECKS(ArenaClass, class);
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
  CHECKL(FUNCHECK(class->segAlloc));
  CHECKL(FUNCHECK(class->segFree));
  CHECKL(FUNCHECK(class->segBase));
  CHECKL(FUNCHECK(class->segLimit));
  CHECKL(FUNCHECK(class->segOfAddr));
  CHECKL(FUNCHECK(class->segFirst));
  CHECKL(FUNCHECK(class->segNext));
  CHECKL(FUNCHECK(class->describe));
  CHECKL(class->endSig == ArenaClassSig);
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

  CHECKL(BoolCheck(arena->poolReady));
  if(arena->poolReady)                 /* design.mps.arena.pool.ready */
    CHECKD(MV, &arena->controlPoolStruct);
  CHECKD(Lock, &arena->lockStruct);

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
  CHECKD(Lock, &arenaRingLock);
  /* can't check arenaSerial */
  
  for(rank = 0; rank < RankMAX; ++rank)
    CHECKL(RingCheck(&arena->greyRing[rank]));

  return TRUE;
}


/* ArenaInit -- initialize the generic part of the arena
 *
 * .init.caller: Unlike PoolInit, this is called by the class init
 * methods, not the generic Create.  This is because the class is
 * responsible for allocating the descriptor.
 */

void ArenaInit(Arena arena, ArenaClass class)
{
  Index i;
  Rank rank;

  /* We do not check the arena argument, because it's _supposed_ to */
  /* point to an uninitialized block of memory. */
  AVERT(ArenaClass, class);

  arena->class = class;
  RingInit(&arena->globalRing);
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
  LockInit(&arena->lockStruct);
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
  arena->bufferLogging = 0;
  arena->fillMutatorSize = 0.0;
  arena->emptyMutatorSize = 0.0;
  arena->allocMutatorSize = 0.0;
  arena->fillInternalSize = 0.0;
  arena->emptyInternalSize = 0.0;
  /* usually overridden by init */
  arena->alignment = MPS_PF_ALIGN;
  /* usually overridden by init */
  arena->zoneShift = ARENA_ZONESHIFT;
  arena->poolReady = FALSE;     /* design.mps.arena.pool.ready */
  for(rank = 0; rank < RankMAX; ++rank)
    RingInit(&arena->greyRing[rank]);

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

  EventInit();

  if(arenaRingInit) {
    /* Race condition; see design.mps.arena.static.ring.init */
    LockClaim(&arenaRingLock);
  } else { /* there isn't a arena ring yet */
    /* design.mps.arena.static.init */
    LockInit(&arenaRingLock);
    LockClaim(&arenaRingLock);
    arenaRingInit = TRUE;
    RingInit(&arenaRing);
    arenaSerial = (Serial)0;
    ProtSetup();
  }

  /* Do initialization.  This will call ArenaInit (see .init.caller). */
  res = (*class->init)(&arena, args);
  if(res != ResOK)
    goto failInit;
  
  ArenaEnter(arena);
  AVERT(Arena, arena);

  /* design.mps.arena.pool.init */
  res = PoolInit(&arena->controlPoolStruct.poolStruct, 
                 arena, PoolClassMV(),
                 ARENA_CONTROL_EXTENDBY, ARENA_CONTROL_AVGSIZE,
                 ARENA_CONTROL_MAXSIZE);
  if(res != ResOK) goto failControlInit;
  arena->poolReady = TRUE;      /* design.mps.arena.pool.ready */
  
  /* initialize the message stuff, design.mps.message */
  {
    void *v;

    res = ArenaAlloc(&v, arena, BTSize(MessageTypeMAX));
    if(res != ResOK)
      goto failEnabledBTAlloc;
    arena->enabledMessageTypes = v;
    BTResRange(arena->enabledMessageTypes, 0, MessageTypeMAX);
  }

  /* Add initialized arena to the global list of arenas. */
  RingAppend(&arenaRing, &arena->globalRing);
  LockReleaseMPM(&arenaRingLock);

  *arenaReturn = arena;
  return ResOK;

failEnabledBTAlloc:
  PoolFinish(&arena->controlPoolStruct.poolStruct);
failControlInit:
  (*class->finish)(arena);
failInit:
  LockReleaseMPM(&arenaRingLock);

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

  arena->sig = SigInvalid;
  LockFinish(&arena->lockStruct);
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

  AVERT(Arena, arena);
  AVER(!arena->insidePoll);

  /* Detach the arena from the global list. */
  LockClaim(&arenaRingLock);
  RingRemove(&arena->globalRing);
  LockReleaseMPM(&arenaRingLock);

  class = arena->class;

  /* throw away the BT used by messages */
  if(arena->enabledMessageTypes != NULL) {
    ArenaFree(arena, (void *)arena->enabledMessageTypes, 
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

  /* Destroy the control pool. */
  arena->poolReady = FALSE;
  PoolFinish(&arena->controlPoolStruct.poolStruct);

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
  LockClaim(&arena->lockStruct);
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
  LockReleaseMPM(&arena->lockStruct);
}
#endif


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

  LockClaim(&arenaRingLock);    /* design.mps.arena.lock.ring */
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
      LockReleaseMPM(&arenaRingLock);
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
      LockReleaseMPM(&arenaRingLock);
      mode &= RootPM(root);
      if(mode != AccessSetEMPTY)
	RootAccess(root, mode);
      ArenaLeave(arena);
      return TRUE;
    }

    ArenaLeave(arena);
  }

  LockReleaseMPM(&arenaRingLock);
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

  if(arena->clamped)
    return;
  size = arena->fillMutatorSize;
  if(arena->insidePoll || size < arena->pollThreshold)
    return;

  arena->insidePoll = TRUE;

  /* Poll actions to see if any new action is to be taken. */
  ActionPoll(arena);

  /* Temporary hacky progress control added here and in trace.c */
  /* for change.dylan.honeybee.170466, and substantially modified */
  /* for change.epcore.minnow.160062. */
  if(arena->busyTraces != TraceSetEMPTY) {
    Trace trace = ArenaTrace(arena, (TraceId)0);
    AVER(arena->busyTraces == TraceSetSingle((TraceId)0));
    TracePoll(trace);
    if(trace->state == TraceFINISHED) {
      TraceDestroy(trace);
    }
  }

  size = arena->fillMutatorSize;
  arena->pollThreshold = size + ARENA_POLL_MAX;
  AVER(arena->pollThreshold > size); /* enough precision? */

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
 
  res = TraceStart(trace, 0.0, 0.0);
  if(res != ResOK)
    goto failStart;
 
  ArenaPark(arena);
 
  return ResOK;
 
failStart:
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

  if(!CHECKT(Arena, arena)) return ResFAIL;
  if(stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "Arena $P ($U) {\n",    
               (WriteFP)arena, (WriteFU)arena->serial,
               "  class $P (\"$S\")\n", 
               (WriteFP)arena->class, arena->class->name,
               "  poolReady $S\n",     arena->poolReady ? "YES" : "NO",
               "  controlPool $P\n",   
               (WriteFP)&arena->controlPoolStruct,
               "  lock $P\n",          (WriteFP)&arena->lockStruct,
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
  if(res != ResOK) return res;

  res = (*arena->class->describe)(arena, stream);
  if(res != ResOK) return res;

  for(i=0; i < ARENA_LD_LENGTH; ++ i) {
    res = WriteF(stream,
                 "    history[$U] = $B\n", i, arena->history[i],
                 NULL);
    if(res != ResOK) return res;
  }
  
  res = WriteF(stream,
               "    [note: indices are raw, not rotated]\n"
               "    prehistory = $B\n",    (WriteFB)arena->prehistory,
               NULL);
  if(res != ResOK) return res;

  res = WriteF(stream,
               "  suspended $S\n", arena->suspended ? "YES" : "NO",
               "  shDepth $U\n", arena->shDepth,
               "  shCacheI $U\n", arena->shCacheI,
               "    (no SegDescribe function)\n",
               NULL);
  if(res != ResOK) return res;

  RING_FOR(node, &arena->rootRing, nextNode) {
    Root root = RING_ELT(Root, arenaRing, node);
    res = RootDescribe(root, stream);
    if(res != ResOK) return res;
  }

  RING_FOR(node, &arena->poolRing, nextNode) {
    Pool pool = RING_ELT(Pool, arenaRing, node);
    res = PoolDescribe(pool, stream);
    if(res != ResOK) return res;
  }

  RING_FOR(node, &arena->formatRing, nextNode) {
    Format format = RING_ELT(Format, arenaRing, node);
    res = FormatDescribe(format, stream);
    if(res != ResOK) return res;
  }

  RING_FOR(node, &arena->threadRing, nextNode) {
    Thread thread = RING_ELT(Thread, arenaRing, node);
    res = ThreadDescribe(thread, stream);
    if(res != ResOK) return res;
  }

  /* @@@@ What about grey rings? */

  res = WriteF(stream,
               "} Arena $P ($U)\n", (WriteFP)arena, 
               (WriteFU)arena->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}


/* ArenaAlloc -- allocate a small block directly from the arena
 *
 * .arena.control-pool: Actually the block will be allocated from the
 * control pool, which is an MV pool embedded in the arena itself.
 *
 * .arenaalloc.addr: In implementations where Addr is not compatible
 * with void* (design.mps.type.addr.use), ArenaAlloc must take care of
 * allocating so that the block can be addressed with a void*.
 */

Res ArenaAlloc(void **baseReturn, Arena arena, size_t size)
{
  Addr base;
  Res res;
  Pool pool;

  AVERT(Arena, arena);
  AVER(baseReturn != NULL);
  AVER(size > 0);

  pool = MVPool(&arena->controlPoolStruct);
  res = PoolAlloc(&base, pool, (Size)size);
  if(res != ResOK) return res;

  *baseReturn = (void *)base; /* see .arenaalloc.addr */
  return ResOK;
}


/* ArenaFree -- free a block allocated using ArenaAlloc */

void ArenaFree(Arena arena, void* base, size_t size)
{
  Pool pool;

  AVERT(Arena, arena);
  AVER(base != NULL);
  AVER(size > 0);

  pool = MVPool(&arena->controlPoolStruct);
  PoolFree(pool, (Addr)base, (Size)size);
}


/* SegAlloc -- allocate a segment from the arena */

Res SegAlloc(Seg *segReturn, SegPref pref, Size size, Pool pool)
{
  Res res;
  Seg seg;
  Arena arena;

  AVER(segReturn != NULL);
  AVERT(SegPref, pref);
  AVER(size > (Size)0);
  AVERT(Pool, pool);

  arena = PoolArena(pool);
  AVERT(Arena, arena);
  AVER(SizeIsAligned(size, arena->alignment));

  res = (*arena->class->segAlloc)(&seg, pref, size, pool);
  if(res != ResOK) return res;

  *segReturn = seg;
  return ResOK;
}


/* SegFree -- free a segment to the arena */

void SegFree(Seg seg)
{
  Arena arena;

  AVERT(Seg, seg);
  arena = SegArena(seg);
  AVERT(Arena, arena);
  (*arena->class->segFree)(seg);
}


/* .seg.critical: These segment functions are low-level and used 
 * through-out. They are therefore on the critical path and their 
 * AVERs are so-marked.
 */

/* SegBase -- return the base address of a segment */

Addr SegBase(Seg seg)
{
  Arena arena;

  AVERT_CRITICAL(Seg, seg); /* .seg.critical */
  arena = SegArena(seg);
  AVERT_CRITICAL(Arena, arena);
  return (*arena->class->segBase)(seg);
}


/* SegLimit -- return the limit address of a segment */

Addr SegLimit(Seg seg)
{
  Arena arena;
  AVERT_CRITICAL(Seg, seg); /* .seg.critical */
  arena = SegArena(seg);
  AVERT_CRITICAL(Arena, arena);
  return (*arena->class->segLimit)(seg);
}


/* SegSize -- return the size of a segment */

Size SegSize(Seg seg)
{
  Arena arena;
  AVERT_CRITICAL(Seg, seg); /* .seg.critcial */
  arena = SegArena(seg);
  AVERT_CRITICAL(Arena, arena);
  return (*arena->class->segSize)(seg);
}


/* SegOfAddr -- return the segment the given address is in, if any */

Bool SegOfAddr(Seg *segReturn, Arena arena, Addr addr)
{
  AVER(segReturn != NULL);
  AVERT(Arena, arena);

  return (*arena->class->segOfAddr)(segReturn, arena, addr);
}


/* SegFirst -- return the first segment in the arena
 *
 * This is used to start an iteration over all segments in the arena.
 */

Bool SegFirst(Seg *segReturn, Arena arena)
{
  AVER(segReturn != NULL);
  AVERT(Arena, arena);

  return (*arena->class->segFirst)(segReturn, arena);
}


/* SegNext -- return the "next" segment in the arena
 *
 * This is used as the iteration step when iterating over all
 * segments in the arena.
 *
 * SegNext finds the segment with the lowest base address which is
 * greater than a specified address.  The address must be (or once
 * have been) the base address of a segment.
 */

Bool SegNext(Seg *segReturn, Arena arena, Addr addr)
{
  AVER_CRITICAL(segReturn != NULL); /* .seg.critical */
  AVERT_CRITICAL(Arena, arena);

  return (*arena->class->segNext)(segReturn, arena, addr);
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
  if(res != ResOK) return res;
  
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
  if(res != ResOK) return res;
  
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
  Res res;

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
  res = TraceScanSingleRef(arena->flippedTraces,
                           arena, seg,
			   RankEXACT, (Ref *)addr);
  AVER(res == ResOK);    /* @@@@ We shouldn't assume that this succeeds */
  /* get the possibly fixed reference */
  return ArenaPeekSeg(arena, seg, addr);
}


/* Heap Walking
 *
 * .trans.mod: There's no particular reason these functions belong in
 * arena.c, it's just a matter of convenience.
 */

#define FormattedObjectsStepClosureSig ((Sig)0x519F05C1)

typedef struct FormattedObjectsStepClosureStruct *FormattedObjectsStepClosure;
typedef struct FormattedObjectsStepClosureStruct {
  Sig sig;
  mps_formatted_objects_stepper_t f;
  void *p;
  size_t s;
} FormattedObjectsStepClosureStruct;

static Bool FormattedObjectsStepClosureCheck(FormattedObjectsStepClosure c)
{
  CHECKS(FormattedObjectsStepClosure, c);
  CHECKL(FUNCHECK(c->f));
  /* p and s fields are arbitrary closures which cannot be checked */
  return TRUE;
}

static void ArenaFormattedObjectsStep(Addr object, Format format, Pool pool,
                                      void *p, Size s)
{
  FormattedObjectsStepClosure c;
  /* Can't check object */
  AVERT(Format, format);
  AVERT(Pool, pool);
  c = p;
  AVERT(FormattedObjectsStepClosure, c);
  AVER(s == 0);

  (*c->f)((mps_addr_t)object, (mps_fmt_t)format, (mps_pool_t)pool, 
          c->p, c->s);
}

/* so called because it walk all formatted objects in an arena */
static void ArenaFormattedObjectsWalk(Arena arena,
                                      FormattedObjectsStepMethod f,
	            	              void *p, Size s)
{
  Seg seg;
  FormattedObjectsStepClosure c;

  AVERT(Arena, arena);
  AVER(FUNCHECK(f));
  AVER(f == ArenaFormattedObjectsStep);
  /* p and s are arbitrary closure and can't be checked */
  AVER(p != NULL);
  AVER(s == 0);

  c = p;
  AVERT(FormattedObjectsStepClosure, c);

  if(SegFirst(&seg, arena)) {
    Addr base;
    do {
      Pool pool;
      base = SegBase(seg);
      pool = SegPool(seg);
      if(pool->class->attr & AttrFMT) {
        ShieldExpose(arena, seg);
        PoolWalk(pool, seg, f, p, s);
	ShieldCover(arena, seg);
      }
    } while(SegNext(&seg, arena, base));
  }
}

void mps_arena_formatted_objects_walk(mps_arena_t mps_arena,
                                      mps_formatted_objects_stepper_t f,
			              void *p,
			              size_t s)
{
  Arena arena = (Arena)mps_arena;
  FormattedObjectsStepClosureStruct c;

  ArenaEnter(arena);
  AVERT(Arena, arena);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures, hence can't be checked */
  c.sig = FormattedObjectsStepClosureSig;
  c.f = f;
  c.p = p;
  c.s = s;
  ArenaFormattedObjectsWalk(arena, ArenaFormattedObjectsStep, &c, 0);
  ArenaLeave(arena);
}

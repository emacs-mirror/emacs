/* impl.c.arena: ARENA IMPLEMENTATION
 *
 * $HopeName: MMsrc!arena.c(trunk.12) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
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
 * behaviour of the memory manager.  [I've just made up this
 * requirement, can you tell?  NickB 1997-07-21]
 * 
 * .where.type: The Arena type is defined in impl.h.mpmtypes.
 * .where.struct: The ArenaStruct type is defined in impl.h.mpmst.
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

SRCID(arena, "$HopeName: MMsrc!arena.c(trunk.12) $");


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

  /* we check the fields in order. We can't yet check the serials,
   * pollThreshold, actionInterval, or epoch.  nickb 1997-07-21 */

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

  /* no check on arena->actionInterval */
  CHECKL(arena->allocTime >= 0.0);

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
  CHECKL(arena->shCacheI < SHIELD_CACHE_SIZE);
  CHECKL(BoolCheck(arena->suspended));

  depth = 0;
  for (i=0; i < SHIELD_CACHE_SIZE; ++i) {
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
  arena->shDepth = (Size)0;
  arena->suspended = FALSE;
  for(i = 0; i < SHIELD_CACHE_SIZE; i++)
    arena->shCache[i] = (Seg)0;
  arena->pollThreshold = (Size)0;
  arena->insidePoll = FALSE;
  arena->clamped = FALSE;
  /* design.mps.arena.poll.interval */
  arena->actionInterval = ARENA_POLL_MAX;  
  arena->epoch = (Epoch)0;              /* impl.c.ld */
  arena->prehistory = RefSetEMPTY;
  for(i = 0; i < ARENA_LD_LENGTH; ++i)
    arena->history[i] = RefSetEMPTY;
  arena->allocTime = 0.0;
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
  if(res != ResOK) goto failInit;

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
  AVER(!arena->insideShield);

  class = arena->class;

  /* throw away the BT used by messages */
  if(arena->enabledMessageTypes != NULL) {
    ArenaFree(arena, (void *)arena->enabledMessageTypes, 
              BTSize(MessageTypeMAX));
    arena->enabledMessageTypes = NULL;
  }

  /* Empty the queue of messages before proceeding to finish */
  /* the arena */
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

  /* Detach the arena from the global list. */
  LockClaim(&arenaRingLock);
  RingRemove(&arena->globalRing);
  LockReleaseMPM(&arenaRingLock);

  /* Destroy the control pool. */
  arena->poolReady = FALSE;
  PoolFinish(&arena->controlPoolStruct.poolStruct);

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

Bool ArenaAccess(Addr addr, AccessSet mode)
{
  Seg seg;
  Ring node, nextNode;

  LockClaim(&arenaRingLock);    /* design.mps.arena.lock.ring */
  RING_FOR(node, &arenaRing, nextNode) {
    Arena arena = RING_ELT(Arena, globalRing, node);
    Root root;

    ArenaEnter(arena);     /* design.mps.arena.lock.arena */
    AVERT(Arena, arena);   /* can't AVER until we've got the lock */
    /* @@@@ The code below assumes that Roots and Segs are disjoint. */
    /* It will fall over (in TraceAccess) if there is a protected */
    /* root on a segment. */
    /* It is possible to overcome this restriction. */
    if(SegOfAddr(&seg, arena, addr)) {
      LockReleaseMPM(&arenaRingLock);
      /* An access in a different thread may have already caused
       * the protection to be cleared.  This avoids calling
       * TraceAccess on protection that has already been cleared on
       * a separate thread.
       */
      mode &= SegPM(seg);
      if(mode != AccessSetEMPTY)
        TraceAccess(arena, seg, mode);
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
  Size size;
  Res res;
  Count i;

  AVERT(Arena, arena);

  if(arena->clamped)
    return;

  size = ArenaCommitted(arena);
  if(arena->insidePoll || size < arena->pollThreshold)
    return;
  /* design.mps.arena.poll.when */

  arena->insidePoll = TRUE;

  /* Poll actions to see if any new action is to be taken. */
  ActionPoll(arena);

  /* Temporary hacky progress control added here and in trace.c */
  /* for change.dylan.honeybee.170466. */
  if(arena->busyTraces != TraceSetEMPTY) {
    Trace trace = ArenaTrace(arena, (TraceId)0);
    AVER(arena->busyTraces == TraceSetSingle((TraceId)0));
    i = trace->rate;
    while(i > 0 && arena->busyTraces != TraceSetEMPTY) {
      res = TracePoll(trace);
      AVER(res == ResOK); /* @@@@ */
      if(trace->state == TraceFINISHED) {
        /* @@@@ Pick up results and use for prediction. */
        TraceDestroy(trace);
      }
      --i;
    }
  }

  size = ArenaCommitted(arena);
  arena->pollThreshold = size + ARENA_POLL_MAX;

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
  Res res;
 
  AVERT(Arena, arena);
 
  arena->clamped = TRUE;
 
  while(arena->busyTraces != TraceSetEMPTY) {
    /* Poll active traces to make progress. */
    for(ti = 0; ti < TRACE_MAX; ++ti)
      if(TraceSetIsMember(arena->busyTraces, ti)) {
        Trace trace = ArenaTrace(arena, ti);
 
        res = TracePoll(trace);
        AVER(res == ResOK); /* @@@@ */
 
        /* @@@@ Pick up results and use for prediction. */
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
 
        /* avoid buffered segments and non-auto pools? */
        res = TraceAddWhite(trace, seg);
        if(res != ResOK)
          goto failAddWhite;
      }
    }
  }
 
  TraceStart(trace);
  if(res != ResOK)
    goto failStart;
 
  ArenaPark(arena);
 
  return ResOK;
 
failStart:
  NOTREACHED;
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
  Word megs; /* @@@@ */
  Index i;

  AVERT(Arena, arena);
  AVER(stream != NULL);

  res = WriteF(stream,
               "Arena $P ($U) {\n",    
               (WriteFP)arena, (WriteFU)arena->serial,
               "  class $P (\"$S\")\n", 
               (WriteFP)arena->class, arena->class->name,
               "  poolReady $S\n",     arena->poolReady ? "YES" : "NO",
               "  controlPool $P\n",   
               (WriteFP)&arena->controlPoolStruct,
               "  lock $P\n",          (WriteFP)&arena->lockStruct,
               "  pollThreshold $U\n", (WriteFU)arena->pollThreshold,
               "  insidePoll $S\n",    arena->insidePoll ? "YES" : "NO",
               NULL);
  if(res != ResOK) return res;

  megs = (Word)(arena->allocTime / 1048576.0); /* @@@@ */

  res = WriteF(stream,
               "  allocTime $UM+$U\n",
               (WriteFU)megs,
               (WriteFU)(arena->allocTime - megs * 1048576.0),
               NULL);
  if(res != ResOK) return res;
  
  res = WriteF(stream,
               "  actionInterval $U\n", (WriteFU)arena->actionInterval,
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
 */

Res ArenaAlloc(void **baseReturn, Arena arena, Size size)
{
  Addr base;
  Res res;
  Pool pool;

  AVERT(Arena, arena);
  AVER(baseReturn != NULL);
  AVER(size > 0);

  pool = MVPool(&arena->controlPoolStruct);
  res = PoolAlloc(&base, pool, size);
  if(res != ResOK) return res;

  /* .arenaalloc.addr-conv: This is the place where we go from */
  /* the managed  addresses of PoolAlloc to the unmanaged */
  /* addresses of ArenaAlloc. */
  *baseReturn = (void *)base;
  return ResOK;
}


/* ArenaFree -- free a block allocated using ArenaAlloc */

void ArenaFree(Arena arena, void* base, Size size)
{
  Pool pool;
  AVERT(Arena, arena);
  AVER(base != NULL);
  AVER(size > 0);

  pool = MVPool(&arena->controlPoolStruct);
  PoolFree(pool, base, size);
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

/* These segment functions are low-level and used through-out. 
 * They are therefore on the critical path and their AVERs are 
 * so-marked.
 */

/* SegBase -- return the base address of a segment */

Addr SegBase(Seg seg)
{
  Arena arena;

  AVERT_CRITICAL(Seg, seg);
  arena = SegArena(seg);
  AVERT_CRITICAL(Arena, arena);
  return (*arena->class->segBase)(seg);
}


/* SegLimit -- return the limit address of a segment */

Addr SegLimit(Seg seg)
{
  Arena arena;
  AVERT_CRITICAL(Seg, seg);
  arena = SegArena(seg);
  AVERT_CRITICAL(Arena, arena);
  return (*arena->class->segLimit)(seg);
}


/* SegSize -- return the size of a segment */

Size SegSize(Seg seg)
{
  Arena arena;
  AVERT_CRITICAL(Seg, seg);
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
  AVER_CRITICAL(segReturn != NULL);
  AVERT_CRITICAL(Arena, arena);

  return (*arena->class->segNext)(segReturn, arena, addr);
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
  return TRUE;
}


/* SegPrefDefault -- return a segment preference representing the 
 *                   defaults
 */

static SegPrefStruct segPrefDefault = {
  SegPrefSig,                           /* sig */
  FALSE,                                /* high */
  RefSetUNIV,                           /* refSet */
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

  default:
    /* Unknown kinds are ignored for binary compatibility. */
    /* See design.mps.pref. */
    break;
  }

  return ResOK;
}



/* Finalization
 *
 * registers an object for finalization.
 * see design.mps.finalize
 */

Res ArenaFinalize(Arena arena, Addr obj)
{
  Res res;

  AVERT(Arena, arena);
  AVER(obj != NULL);

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

Word ArenaPeek(Arena arena, Addr addr)
{
  Seg seg;
  Bool b;

  AVERT(Arena, arena);

  b = SegOfAddr(&seg, arena, addr);
  if(b) {
    return ArenaPeekSeg(arena, seg, addr);
  } else {
    Word w;
    w = *(Word *)addr;
    return w;
  }
}

Word ArenaPeekSeg(Arena arena, Seg seg, Addr addr)
{
  Word w;

  AVERT(Arena, arena);
  AVERT(Seg, seg);

  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));

  ShieldExpose(arena, seg);
  w = *(Word *)addr;
  ShieldCover(arena, seg);
  return w;
}

void ArenaPoke(Arena arena, Addr addr, Word word)
{
  Seg seg;
  Bool b;

  AVERT(Arena, arena);
  /* can't check word, will check addr shortly */

  b = SegOfAddr(&seg, arena, addr);
  if(b) {
    ArenaPokeSeg(arena, seg, addr, word);
  } else {
    *(Word *)addr = word;
  }
}

void ArenaPokeSeg(Arena arena, Seg seg, Addr addr, Word word)
{
  RefSet summary;

  AVERT(Arena, arena);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  /* word is arbitrary and can't be checked */

  ShieldExpose(arena, seg);
  *(Word *)addr = word;
  summary = SegSummary(seg);
  summary = RefSetAdd(arena, summary, (Addr)word);
  SegSetSummary(seg, summary);
  ShieldCover(arena, seg);
}

/* impl.c.pool: POOL IMPLEMENTATION
 *
 * $HopeName: MMsrc!pool.c(trunk.61) $
 * Copyright (C) 1997. Harlequin Group plc. All rights reserved.
 *
 * READERSHIP
 *
 * .readership: any MPS developer
 *
 * DESIGN
 *
 * .design: See design.mps.class-interface and design.mps.pool [both of
 * these are somewhat dated, but still useful in part -- drj 1998-04-17]
 *
 * PURPOSE
 *
 * .purpose: This is the implementation of the generic pool interface.
 * There are three sorts of functions provided:
 * .purpose.support: Support functions for manipulating and accessing
 * Pool and PoolClass objects (create, destroy, check, various
 * accessors, and other miscellaneous functions).
 * .purpose.dispatch: Dispatch functions that implement the generic
 * function dispatch mechanism for Pool Classes (PoolAlloc, PoolFix,
 * etc).
 * .purpose.core: A selection of default, trivial, or useful methods
 * that Pool Classes can use as the implementations for some of their
 * methods. (such as PoolTrivWhiten, PoolNoFix, PoolCollectAct, etc).
 *
 * SOURCES
 *
 * .source: See .design also.  PoolStruct and PoolClassStruct, the
 * central types for this module, are defined in impl.h.mpmst, the
 * corresponding abstract types in impl.h.mpmtypes.  Declarations and
 * prototypes are in impl.h.mpm.  Several functions have macro versions
 * defined in impl.h.mpm.
 */

#include "mpm.h"

SRCID(pool, "$HopeName: MMsrc!pool.c(trunk.61) $");


Bool PoolClassCheck(PoolClass class)
{
  CHECKS(PoolClass, class);
  CHECKL(class->name != NULL); /* Should be <=6 char C identifier */
  CHECKL(class->size >= sizeof(PoolStruct));
  /* Offset of generic Pool within class-specific instance cannot be */
  /* greater than the size of the class-specific portion of the instance */
  CHECKL(class->offset <= (size_t)(class->size - sizeof(PoolStruct)));
  CHECKL(AttrCheck(class->attr));
  CHECKL(FUNCHECK(class->init));
  CHECKL(FUNCHECK(class->finish));
  CHECKL(FUNCHECK(class->alloc));
  CHECKL(FUNCHECK(class->free));
  CHECKL(FUNCHECK(class->bufferInit));
  CHECKL(FUNCHECK(class->bufferFill));
  CHECKL(FUNCHECK(class->bufferEmpty));
  CHECKL(FUNCHECK(class->bufferFinish));
  CHECKL(FUNCHECK(class->traceBegin));
  CHECKL(FUNCHECK(class->whiten));
  CHECKL(FUNCHECK(class->grey));
  CHECKL(FUNCHECK(class->scan));
  CHECKL(FUNCHECK(class->fix));
  CHECKL(FUNCHECK(class->reclaim));
  CHECKL(FUNCHECK(class->benefit));
  CHECKL(FUNCHECK(class->act));
  CHECKL(FUNCHECK(class->walk));
  CHECKL(FUNCHECK(class->describe));
  CHECKL(class->endSig == PoolClassSig);
  return TRUE;
}

Bool PoolCheck(Pool pool)
{
  /* Checks ordered as per struct decl in impl.h.mpmst.pool */
  CHECKS(Pool, pool);
  /* Break modularity for checking efficiency */
  CHECKL(pool->serial < pool->arena->poolSerial);
  CHECKD(PoolClass, pool->class);
  CHECKU(Arena, pool->arena);
  CHECKL(RingCheck(&pool->arenaRing));
  CHECKL(RingCheck(&pool->bufferRing));
  /* Cannot check pool->bufferSerial */
  CHECKL(RingCheck(&pool->segRing));
  CHECKL(RingCheck(&pool->actionRing));
  /* Cannot check pool->actionSerial */
  CHECKL(AlignCheck(pool->alignment));
  /* normally pool->format iff pool->class->attr&AttrFMT, but not */
  /* during pool initialization */
  if(pool->format != NULL) {
    CHECKL((pool->class->attr & AttrFMT) != 0);
  }
  CHECKL(pool->fillMutatorSize >= 0.0);
  CHECKL(pool->emptyMutatorSize >= 0.0);
  CHECKL(pool->fillInternalSize >= 0.0);
  CHECKL(pool->emptyInternalSize >= 0.0);
  return TRUE;
}


/* PoolInit, PoolInitV -- initialize a pool
 *
 * Initialize the generic fields of the pool and calls class-specific init. 
 * See design.mps.pool.align
 */

Res PoolInit(Pool pool, Arena arena, PoolClass class, ...)
{
  Res res;
  va_list args;
  va_start(args, class);
  res = PoolInitV(pool, arena, class, args);
  va_end(args);
  return res;
}

Res PoolInitV(Pool pool, Arena arena,
              PoolClass class, va_list args)
{
  Res res;
  Word classId;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(PoolClass, class);

  pool->class = class;
  /* label the pool class with its name */
  /* @@@@ need a field in the class to stop doing this repeatedly */
  classId = EventInternString(class->name);
  /* @@@@ this breaks design.mps.type.addr.use */
  EventLabelAddr((Addr)class, classId);

  pool->arena = arena;
  /* .ring.init: See .ring.finish */
  RingInit(&pool->arenaRing);
  RingInit(&pool->bufferRing);
  RingInit(&pool->segRing);
  RingInit(&pool->actionRing);
  pool->bufferSerial = (Serial)0;
  pool->actionSerial = (Serial)0;
  pool->alignment = MPS_PF_ALIGN;
  pool->format = NULL;
  pool->fillMutatorSize = 0.0;
  pool->emptyMutatorSize = 0.0;
  pool->fillInternalSize = 0.0;
  pool->emptyInternalSize = 0.0;

  /* Initialise signature last; see design.mps.sig */
  pool->sig = PoolSig;
  pool->serial = arena->poolSerial;
  ++(arena->poolSerial);

  AVERT(Pool, pool);

  /* Do class-specific initialization. */
  res = (*class->init)(pool, args);
  if(res != ResOK)
    goto failInit;

  /* Add initialized pool to list of pools in arena. */
  RingAppend(ArenaPoolRing(arena), &pool->arenaRing);

  EVENT_PPP(PoolInit, pool, arena, class);

  return ResOK;

failInit:
  pool->sig = SigInvalid;      /* Leave arena->poolSerial incremented */
  RingFinish(&pool->actionRing);
  RingFinish(&pool->segRing);
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->arenaRing);
  return res;
}

/* PoolCreate, PoolCreateV: Allocate and initialise pool */

Res PoolCreate(Pool *poolReturn, Arena arena, 
               PoolClass class, ...)
{
  Res res;
  va_list args;
  va_start(args, class);
  res = PoolCreateV(poolReturn, arena, class, args);
  va_end(args);
  return res;
}

Res PoolCreateV(Pool *poolReturn, Arena arena,  
                PoolClass class, va_list args)
{
  Res res;
  Pool pool;
  void *base;

  AVER(poolReturn != NULL);
  AVERT(Arena, arena);
  AVERT(PoolClass, class);

  /* .space.alloc: Allocate the pool instance structure with the size */
  /* requested  in the pool class.  See .space.free */
  res = ArenaAlloc(&base, arena, class->size); 
  if(res != ResOK)
    goto failArenaAlloc;

  /* base is the address of the class-specific pool structure. */
  /* We calculate the address of the generic pool structure within the */
  /* instance by using the offset information from the class. */
  pool = (Pool)PointerAdd(base, class->offset);

  /* Initialize the pool. */  
  res = PoolInitV(pool, arena, class, args);
  if(res != ResOK) 
    goto failPoolInit;
  
  *poolReturn = pool;  
  return ResOK;

failPoolInit:
  ArenaFree(arena, base, class->size);
failArenaAlloc:
  return res;
}


/* PoolFinish -- Finish pool including class-specific and generic fields. */

void PoolFinish(Pool pool)
{
  AVERT(Pool, pool);  
  
  /* Do any class-specific finishing. */
  (*pool->class->finish)(pool);

  /* Detach the pool from the arena, and unsig it. */
  RingRemove(&pool->arenaRing);
  pool->sig = SigInvalid;
  
  /* .ring.finish: Finish the generic fields.  See .ring.init */
  RingFinish(&pool->actionRing);
  RingFinish(&pool->segRing);
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->arenaRing);
  
  EVENT_P(PoolFinish, pool);
}

/* PoolDestroy -- Finish and free pool. */

void PoolDestroy(Pool pool)
{
  PoolClass class;
  Arena arena;
  Addr base;

  AVERT(Pool, pool);  
  
  class = pool->class; /* } In case PoolFinish changes these */
  arena = pool->arena; /* } */

  /* Finish the pool instance structure. */
  PoolFinish(pool);

  /* .space.free: Free the pool instance structure.  See .space.alloc */
  base = AddrSub((Addr)pool, (Size)(class->offset));
  ArenaFree(arena, base, (Size)(class->size));
}


/* PoolAlloc -- allocate a block of memory from a pool */

Res PoolAlloc(Addr *pReturn, Pool pool, Size size, 
              Bool withReservoirPermit)
{
  Res res;

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER((pool->class->attr & AttrALLOC) != 0);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  res = (*pool->class->alloc)(pReturn, pool, size, withReservoirPermit);
  if(res != ResOK)
    return res;
  /* Make sure that the allocated address was in the pool's memory. */
  /* .hasaddr.critical: The PoolHasAddr check is expensive, and in */
  /* allocation-bound programs this is on the critical path. */
  AVER_CRITICAL(PoolHasAddr(pool, *pReturn));

  /* All PoolAllocs should advance the allocation clock, so we count */
  /* it all in the fillMutatorSize field. */
  pool->fillMutatorSize += size;
  PoolArena(pool)->fillMutatorSize += size;

  EVENT_PAW(PoolAlloc, pool, *pReturn, size);

  return ResOK;
}


/* PoolFree -- deallocate a block of memory allocated from the pool */

void PoolFree(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER((pool->class->attr & AttrFREE) != 0);
  AVER(old != NULL);
  /* The pool methods should check that old is in pool. */
  AVER(size > 0);
  (*pool->class->free)(pool, old, size);
  
  EVENT_PAW(PoolFree, pool, old, size);
}


Res PoolTraceBegin(Pool pool, Trace trace)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVER(PoolArena(pool) == trace->arena);
  return (*pool->class->traceBegin)(pool, trace);
}

Res PoolAccess(Pool pool, Seg seg, Addr addr,
               AccessSet mode, MutatorFaultContext context)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  /* Can't check mode as there is no check method */
  /* Can't check MutatorFaultContext as there is no check method */

  return (*pool->class->access)(pool, seg, addr, mode, context);
}

Res PoolWhiten(Pool pool, Trace trace, Seg seg)
{  
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(PoolArena(pool) == trace->arena);
  AVER(SegPool(seg) == pool);
  return (*pool->class->whiten)(pool, trace, seg);
}

void PoolGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);
  AVER(pool->arena == trace->arena);
  AVER(SegPool(seg) == pool);
  (*pool->class->grey)(pool, trace, seg);
}

void PoolBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);
  AVER(SegPool(seg) == pool);
  (*pool->class->blacken)(pool, traceSet, seg);
}

Res PoolScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(ss->arena == pool->arena);

  /* The segment must belong to the pool. */
  AVER(pool == SegPool(seg));

  /* Should only scan for a rank for which there are references */
  /* in the segment.  (not true) */
  /* We actually want to check that the rank we are scanning at */
  /* (ss->rank) is at least as big as all the ranks in */
  /* the segment (SegRankSet(seg)).  It is tricky to check that, */
  /* so we only check that either ss->rank is in the segment's */
  /* ranks, or that ss->rank is exact. */
  /* See impl.c.trace.scan.conservative */
  AVER(ss->rank == RankEXACT || RankSetIsMember(SegRankSet(seg), ss->rank));

  /* Should only scan segments which contain grey objects. */
  AVER(TraceSetInter(SegGrey(seg), ss->traces) != TraceSetEMPTY);

  return (*pool->class->scan)(totalReturn, ss, pool, seg);
}

/* See impl.h.mpm for macro version; see design.mps.pool.req.fix */
Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO)
{
  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(pool == SegPool(seg));
  AVER(refIO != NULL);

  /* Should only be fixing references to white segments. */
  AVER(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);

  return PoolFix(pool, ss, seg, refIO);
}

void PoolFixEmergency(Pool pool, ScanState ss, Seg seg, Addr *refIO)
{
  Res res;

  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(pool == SegPool(seg));
  AVER(refIO != NULL);

  /* Should only be fixing references to white segments. */
  AVER(TraceSetInter(SegWhite(seg), ss->traces) != TraceSetEMPTY);

  res = (pool->class->fixEmergency)(pool, ss, seg, refIO);
  AVER(res == ResOK);
}

void PoolReclaim(Pool pool, Trace trace, Seg seg)
{
  AVERT_CRITICAL(Pool, pool);
  AVERT_CRITICAL(Trace, trace);
  AVERT_CRITICAL(Seg, seg);
  AVER_CRITICAL(pool->arena == trace->arena);
  AVER_CRITICAL(SegPool(seg) == pool);

  /* There shouldn't be any grey things left for this trace. */
  AVER_CRITICAL(!TraceSetIsMember(SegGrey(seg), trace->ti));

  /* Should only be reclaiming segments which are still white. */
  AVER_CRITICAL(TraceSetIsMember(SegWhite(seg), trace->ti));

  (*pool->class->reclaim)(pool, trace, seg);
}


double PoolBenefit(Pool pool, Action action)
{
  AVERT(Pool, pool);
  AVERT(Action, action);
  AVER(action->pool == pool);
  return (*pool->class->benefit)(pool, action);
}


Res PoolAct(Pool pool, Action action)
{
  AVERT(Pool, pool);
  AVERT(Action, action);
  AVER(action->pool == pool);
  return (*pool->class->act)(pool, action);
}

void PoolWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
              void *p, Size s)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures, hence can't be checked */
  (*pool->class->walk)(pool, seg, f, p, s);
}



Res PoolDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  Ring node, nextNode;

  AVERT(Pool, pool);
  AVER(stream != NULL);
  
  res = WriteF(stream,
               "Pool $P ($U) {\n", (WriteFP)pool, (WriteFU)pool->serial,
               "  class $P (\"$S\")\n", 
               (WriteFP)pool->class, pool->class->name,
               "  arena $P ($U)\n", 
               (WriteFP)pool->arena, (WriteFU)pool->arena->serial,
               "  alignment $W\n", (WriteFW)pool->alignment,
               NULL);
  if(res != ResOK)
    return res;
  if(NULL != pool->format) {
    res = FormatDescribe(pool->format, stream);
    if(res != ResOK)
      return res;
  }
  res = WriteF(stream,
               "  fillMutatorSize $UKb\n",
                 (WriteFU)(pool->fillMutatorSize / 1024),
               "  emptyMutatorSize $UKb\n",
                 (WriteFU)(pool->emptyMutatorSize / 1024),
               "  fillInternalSize $UKb\n",
                 (WriteFU)(pool->fillInternalSize / 1024),
               "  emptyInternalSize $UKb\n",
                 (WriteFU)(pool->emptyInternalSize / 1024),
               NULL);
  if(res != ResOK)
    return res;

  res = (*pool->class->describe)(pool, stream);
  if(res != ResOK)
    return res;

  RING_FOR(node, &pool->bufferRing, nextNode) {
    Buffer buffer = RING_ELT(Buffer, poolRing, node);
    res = BufferDescribe(buffer, stream);
    if(res != ResOK)
      return res;
  }

  res = WriteF(stream,
               "} Pool $P ($U)\n", (WriteFP)pool, (WriteFU)pool->serial,
               NULL);
  if(res != ResOK)
    return res;

  return ResOK;
}


/* .pool.space: Thread-safe; see design.mps.interface.c.thread-safety */
/* See impl.h.mpm for macro version */
Arena (PoolArena)(Pool pool)
{
  /* Can't AVER pool as that would not be thread-safe */
  /* AVERT(Pool, pool); */

  return pool->arena;
}


/* PoolFormat
 *
 * Returns the format of the pool (the format of objects in the
 * pool).  If the pool is unformatted or doesn't declare a format
 * then this function returns FALSE and does not update *formatReturn.
 * Otherwise this function returns TRUE and *formatReturn is updated
 * to be the pool's format. */
Bool PoolFormat(Format *formatReturn, Pool pool)
{
  AVER(formatReturn != NULL);
  AVERT(Pool, pool);

  if(pool->format) {
    *formatReturn = pool->format;
    return TRUE;
  }
  return FALSE;
}


Bool PoolOfAddr(Pool *poolReturn, Arena arena, Addr addr)
{
  Seg seg;

  AVER(poolReturn != NULL);
  AVERT(Arena, arena);

  if(SegOfAddr(&seg, arena, addr)) {
    *poolReturn = SegPool(seg);
    return TRUE;
  }

  return FALSE;
}


Bool PoolHasAddr(Pool pool, Addr addr)
{
  Pool addrPool;
  Arena arena;
  Bool managed;

  AVERT(Pool, pool);

  arena = PoolArena(pool);
  managed = PoolOfAddr(&addrPool, arena, addr);
  if(managed && addrPool == pool)
    return TRUE;
  else
    return FALSE;
}


/* See impl.h.mpm for macro version */
Align (PoolAlignment)(Pool pool)
{
  AVERT(Pool, pool);
  return pool->alignment;
}


double PoolMutatorAllocSize(Pool pool)
{
  AVERT(Pool, pool);
  return pool->fillMutatorSize - pool->emptyMutatorSize;
}


/* PoolNo*, PoolTriv* -- Trivial and non-methods for Pool Classes 
 * See design.mps.pool.no and design.mps.pool.triv
 */

void PoolTrivFinish(Pool pool)
{
  AVERT(Pool, pool);
  NOOP;
}

Res PoolNoAlloc(Addr *pReturn, Pool pool, Size size,
                Bool withReservoirPermit)
{
  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));
  NOTREACHED;
  return ResUNIMPL;
}

Res PoolTrivAlloc(Addr *pReturn, Pool pool, Size size,
                  Bool withReservoirPermit)
{
  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));
  return ResLIMIT;
}

void PoolNoFree(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER(old != NULL);
  AVER(size > 0);
  NOTREACHED;
}

void PoolTrivFree(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER(old != NULL);
  AVER(size > 0);
  NOOP;                         /* trivial free has no effect */
}


/* PoolNoBufferInit -- buffer init for pools without buffers */

Res PoolNoBufferInit(Pool pool, Buffer buffer, va_list args)
{
  AVERT(Pool, pool);
  UNUSED(buffer); UNUSED(args);
  NOTREACHED;
  return ResUNIMPL;
}


/* PoolTrivBufferInit -- default initialization for buffers
 *
 * The generic method initialised all generic fields; nothing to do.
 */

Res PoolTrivBufferInit(Pool pool, Buffer buffer, va_list args)
{
  AVERT(Pool, pool);
  UNUSED(buffer); UNUSED(args);
  return ResOK;
}


void PoolNoBufferFinish(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  NOTREACHED;
}

void PoolTrivBufferFinish(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  NOOP;
}

Res PoolNoBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                     Pool pool, Buffer buffer, Size size,
                     Bool withReservoirPermit)
{
  AVER(segReturn != NULL);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));
  NOTREACHED;
  return ResUNIMPL;
}

Res PoolTrivBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                       Pool pool, Buffer buffer, Size size,
                       Bool withReservoirPermit)
{
  Res res;
  Addr p;
  Seg seg;
  Bool b;

  AVER(segReturn != NULL);
  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));

  res = PoolAlloc(&p, pool, size, withReservoirPermit);
  if(res != ResOK) return res;
  
  b = SegOfAddr(&seg, PoolArena(pool), p);
  AVER(b);
  
  *segReturn = seg;
  *baseReturn = p;
  *limitReturn = AddrAdd(p, size);
  return ResOK;
}

void PoolNoBufferEmpty(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(!BufferIsReset(buffer));
  AVER(BufferIsReady(buffer));
  NOTREACHED;
}

void PoolTrivBufferEmpty(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(!BufferIsReset(buffer));
  AVER(BufferIsReady(buffer));
}

Res PoolNoDescribe(Pool pool, mps_lib_FILE *stream)
{
  AVERT(Pool, pool);
  AVER(stream != NULL);
  NOTREACHED;
  return ResUNIMPL;
}

Res PoolTrivDescribe(Pool pool, mps_lib_FILE *stream)
{
  AVERT(Pool, pool);
  AVER(stream != NULL);
  return WriteF(stream, "  No class-specific description available.\n", NULL);
}

Res PoolNoTraceBegin(Pool pool, Trace trace)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVER(PoolArena(pool) == trace->arena);
  NOTREACHED;
  return ResUNIMPL;
}

Res PoolTrivTraceBegin(Pool pool, Trace trace)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVER(PoolArena(pool) == trace->arena);
  return ResOK;
}

/* NoAccess
 *
 * Should be used (for the access method) by Pool Classes which do
 * not expect to ever have pages which the mutator will fault on.
 * That is, no protected pages, or only pages which are inaccessible
 * by the mutator are protected.
 */
Res PoolNoAccess(Pool pool, Seg seg, Addr addr,
                 AccessSet mode, MutatorFaultContext context)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  /* can't check AccessSet as there is no Check method */
  /* can't check context as there is no Check method */
  UNUSED(mode);
  UNUSED(context);

  NOTREACHED;
  return ResUNIMPL;
}

/* SegAccess
 *
 * Should be used (for the access method) by Pool Classes which intend
 * to handle page faults by scanning the entire segment and lowering
 * the barrier.
 */
Res PoolSegAccess(Pool pool, Seg seg, Addr addr,
                  AccessSet mode, MutatorFaultContext context)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVER(SegPool(seg) == pool);
  /* can't check AccessSet as there is no Check method */
  /* can't check context as there is no Check method */

  UNUSED(addr);
  UNUSED(context);
  TraceSegAccess(PoolArena(pool), seg, mode);
  return ResOK;
}

/* SingleAccess
 *
 * Handles page faults by attempting emulation.  If the faulting
 * instruction cannot be emulated then this function returns ResFAIL.
 *
 * Due to the assumptions made below, pool classes should only use
 * this function if all words in an object are tagged or traceable.
 *
 * .single-access.assume.ref: It currently assumes that the address
 * being faulted on contains a plain reference or a tagged non-reference.
 * .single-access.improve.format: * later this will be abstracted
 * through the cleint object format interface, so that
 * no such assumption is necessary.
 */
Res PoolSingleAccess(Pool pool, Seg seg, Addr addr,
                     AccessSet mode, MutatorFaultContext context)
{
  Arena arena;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(SegBase(seg) <= addr);
  AVER(addr < SegLimit(seg));
  AVER(SegPool(seg) == pool);
  /* can't check AccessSet as there is no Check method */
  /* can't check context as there is no Check method */

  arena = PoolArena(pool);

  if(ProtCanStepInstruction(context)) {
    Ref ref;
    Res res;

    ShieldExpose(arena, seg);

    if(mode & SegSM(seg) & AccessREAD) {
      /* read access */
      /* .single-access.assume.ref */
      /* .single-access.improve.format */
      ref = *(Ref *)addr;
      /* Check that the reference is aligned to a word boundary */
      /* (we assume it is not a reference otherwise) */
      if(WordIsAligned((Word)ref, sizeof(Word))) {
        /* See the note in TraceSegAccess about using RankEXACT here */
        /* (impl.c.trace.scan.conservative) */
	TraceScanSingleRef(arena->flippedTraces, RankEXACT, arena,
	                   seg, (Ref *)addr);
      }
    }
    res = ProtStepInstruction(context);
    AVER(res == ResOK);

    /* update SegSummary according to the possibly changed reference */
    ref = *(Ref *)addr;
    SegSetSummary(seg, RefSetAdd(arena, SegSummary(seg), ref));

    ShieldCover(arena, seg);

    return ResOK;
  } else {
    /* couldn't single-step instruction */
    return ResFAIL;
  }
}


Res PoolTrivWhiten(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);

  SegSetWhite(seg, TraceSetAdd(SegWhite(seg), trace->ti));

  return ResOK;
}

Res PoolNoWhiten(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);
  NOTREACHED;
  return ResUNIMPL;
}

void PoolNoGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);
  NOTREACHED;
}

void PoolTrivGrey(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);

  /* @@@@ The trivial grey method probably shouldn't exclude */
  /* the white segments, since they might also contain grey objects. */
  if(!TraceSetIsMember(SegWhite(seg), trace->ti))
    SegSetGrey(seg, TraceSetSingle(trace->ti));
}

void PoolNoBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);
  NOTREACHED;
}

void PoolTrivBlacken(Pool pool, TraceSet traceSet, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(TraceSet, traceSet);
  AVERT(Seg, seg);

  /* the trivial blacken method does nothing; for pool classes which do
   * not keep additional colour information. */
  NOOP;
}

Res PoolNoScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  NOTREACHED;
  return ResUNIMPL;
}

Res PoolNoFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(refIO != NULL);
  NOTREACHED;
  return ResUNIMPL;
}

void PoolNoReclaim(Pool pool, Trace trace, Seg seg)
{
  AVERT(Pool, pool);
  AVERT(Trace, trace);
  AVERT(Seg, seg);
  NOTREACHED;
}

double PoolNoBenefit(Pool pool, Action action)
{
  AVERT(Pool, pool);
  AVERT(Action, action);
  AVER(action->pool == pool);
  NOTREACHED;
  return (double)0;
}

Res PoolNoAct(Pool pool, Action action)
{
  AVERT(Pool, pool);
  AVERT(Action, action);
  AVER(action->pool == pool);
  NOTREACHED;
  return ResUNIMPL;
}


/* PoolCollectAct -- perform the action of collecting the entire pool
 *
 * @@@@ This should be in a module such as collect.c, but this is a
 * short term patch for change.dylan.sunflower.10.170440.
 */

Res PoolCollectAct(Pool pool, Action action)
{
  Trace trace;
  Res res;
  Arena arena;
  Ring ring, node, nextNode;
  Seg seg;

  AVERT(Pool, pool);
  AVERT(Action, action);
  AVER(action->pool == pool);

  arena = PoolArena(pool);

  res = TraceCreate(&trace, arena);
  if(res != ResOK)
    goto failCreate;

  res = PoolTraceBegin(pool, trace);
  if(res != ResOK)
    goto failBegin;
  
  /* Identify the condemned set and turn it white. */
  ring = PoolSegRing(pool);
  RING_FOR(node, ring, nextNode) {
    seg = SegOfPoolRing(node);

    res = TraceAddWhite(trace, seg);
    if(res != ResOK)
      goto failAddWhite;
  }

  /* @@@@ mortality and finishing time are set to reasonable values, */
  /* while we wait for a more intelligent strategy. */
  TraceStart(trace, 0.5, 0.5 * trace->condemned);

  return ResOK;

failAddWhite:
  NOTREACHED; /* @@@@ Would leave white sets inconsistent. */
failBegin:
  TraceDestroy(trace);
failCreate:
  return res;
}


void PoolNoRampBegin(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buf);
  NOTREACHED;
}


void PoolNoRampEnd(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buf);
  NOTREACHED;
}


void PoolTrivRampBegin(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buf);
}


void PoolTrivRampEnd(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buf);
}


void PoolNoWalk(Pool pool, Seg seg,
                FormattedObjectsStepMethod f,
                void *p, Size s)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures, hence can't be checked */
  UNUSED(p);
  UNUSED(s);

  NOTREACHED;
}

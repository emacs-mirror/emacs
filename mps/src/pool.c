/* impl.c.pool: POOL IMPLEMENTATION
 *
 * $HopeName: MMsrc!pool.c(trunk.18) $
 * Copyright (C) 1994,1995,1996 Harlequin Group, all rights reserved
 *
 * This is the implementation of the generic pool interface.  The
 * functions here dispatch to pool-specific methods.
 *
 * See impl.h.mpmst for definition of Pool.  
 * See design.mps.pool for design. 
 */

#include "mpm.h"

SRCID(pool, "$HopeName: MMsrc!pool.c(trunk.18) $");


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
  CHECKL(FUNCHECK(class->bufferFinish));
  CHECKL(FUNCHECK(class->bufferFill));
  CHECKL(FUNCHECK(class->bufferTrip));
  CHECKL(FUNCHECK(class->bufferExpose));
  CHECKL(FUNCHECK(class->bufferCover));
  CHECKL(FUNCHECK(class->condemn));
  CHECKL(FUNCHECK(class->grey));
  CHECKL(FUNCHECK(class->scan));
  CHECKL(FUNCHECK(class->fix));
  CHECKL(FUNCHECK(class->reclaim));
  CHECKL(FUNCHECK(class->access));
  CHECKL(FUNCHECK(class->describe));
  CHECKL(class->endSig == PoolClassSig);
  return TRUE;
}

Bool PoolCheck(Pool pool)
{
  CHECKS(Pool, pool);
  CHECKU(Space, pool->space);
  /* Break modularity for checking efficiency */
  CHECKL(pool->serial < pool->space->poolSerial);
  CHECKD(PoolClass, pool->class);
  CHECKL(RingCheck(&pool->spaceRing));
  CHECKL(RingCheck(&pool->bufferRing));
  /* Cannot check pool->bufferSerial */
  CHECKL(AlignCheck(pool->alignment));
  return TRUE;
}


/* PoolInit, PoolInitV -- initialize a pool
 *
 * Initialize the generic fields of the pool and calls class-specific init. 
 * See design.mps.pool.align
 */

Res PoolInit(Pool pool, Space space, PoolClass class, ...)
{
  Res res;
  va_list args;
  va_start(args, class);
  res = PoolInitV(pool, space, class, args);
  va_end(args);
  return res;
}

Res PoolInitV(Pool pool, Space space, PoolClass class, va_list args)
{
  Res res;

  AVER(pool != NULL);
  AVERT(Space, space);
  AVERT(PoolClass, class);

  pool->class = class;
  pool->space = space;
  /* .ring.init: See .ring.finish */
  RingInit(&pool->spaceRing);
  RingInit(&pool->bufferRing);
  pool->alignment = ARCH_ALIGN;
  RingAppend(SpacePoolRing(space), &pool->spaceRing);

  /* Initialise signature last; see design.mps.sig */
  pool->sig = PoolSig;
  pool->serial = space->poolSerial;
  ++(space->poolSerial);

  AVERT(Pool, pool);

  /* Do class-specific initialization. */
  res = (*class->init)(pool, args);
  if(res != ResOK)
    goto failInit;

  return ResOK;

failInit:
  pool->sig = SigInvalid;      /* Leave space->poolSerial incremented */
  RingFinish(&pool->bufferRing);
  RingRemove(&pool->spaceRing);
  RingFinish(&pool->spaceRing);
  return res;
}

/* PoolCreate, PoolCreateV: Allocate and initialise pool */

Res PoolCreate(Pool *poolReturn, PoolClass class, Space space, ...)
{
  Res res;
  va_list args;
  va_start(args, space);
  res = PoolCreateV(poolReturn, class, space, args);
  va_end(args);
  return res;
}

Res PoolCreateV(Pool *poolReturn, PoolClass class,
                Space space, va_list args)
{
  Res res;
  Pool pool;
  void *base;

  AVER(poolReturn != NULL);
  AVERT(Space, space);
  AVERT(PoolClass, class);

  /* .space.alloc: Allocate the pool instance structure with the size */
  /* requested  in the pool class.  See .space.free */
  res = SpaceAlloc(&base, space, class->size); 
  if(res != ResOK)
      goto failSpaceAlloc;

  /* base is the address of the class-specific pool structure. */
  /* We calculate the address of the generic pool structure within the */
  /* instance by using the offset information from the class. */
  pool = (Pool)PointerAdd(base, class->offset);

  /* Initialize the pool. */  
  res = PoolInitV(pool, space, class, args);
  if(res != ResOK) 
    goto failPoolInit;
  
  *poolReturn = pool;  
  return ResOK;

failPoolInit:
  SpaceFree(space, base, class->size);
failSpaceAlloc:
  return res;
}

/* PoolFinish -- Finish pool including class-specific and generic fields. */

void PoolFinish(Pool pool)
{
  AVERT(Pool, pool);  
  
  /* Do any class-specific finishing. */
  (*pool->class->finish)(pool);

  /* There must be no buffers attached to the pool at */
  /* this point.  The class-specific finish method is */
  /* allowed to remove them. */
  AVER(RingCheckSingle(&pool->bufferRing)); 
  
  /* Detach the pool from the space, and unsig it. */
  RingRemove(&pool->spaceRing);
  pool->sig = SigInvalid;
  
  /* .ring.finish: Finish the generic fields.  See .ring.init */
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->spaceRing);
}

/* PoolDestroy -- Finish and free pool. */

void PoolDestroy(Pool pool)
{
  PoolClass class;
  Space space;
  Addr base;

  AVERT(Pool, pool);  
  
  class = pool->class; /* } In case PoolFinish changes these */
  space = pool->space; /* } */

  /* Finish the pool instance structure. */
  PoolFinish(pool);

  /* .space.free: Free the pool instance structure.  See .space.alloc */
  base = AddrSub((Addr)pool, (Size)(class->offset));
  SpaceFree(space, base, (Size)(class->size));
}

Res PoolAlloc(Addr *pReturn, Pool pool, Size size)
{
  Res res;

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);

  res = (*pool->class->alloc)(pReturn, pool, size);
  if(res != ResOK) return res;

  /* Make sure that the allocated address was in the pool's memory. */
  AVER(PoolHasAddr(pool, *pReturn));

  return ResOK;
}

void PoolFree(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER(old != NULL);
  AVER(PoolHasAddr(pool, old));
  AVER(size > 0);
  (*pool->class->free)(pool, old, size);
}

Res PoolCondemn(RefSet *condemnedReturn, Pool pool,
                  Space space, TraceId ti)
{  
  AVER(condemnedReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Space, space);
  AVER(pool->space == space);
  AVERT(TraceId, ti);
  AVER(ti != TraceIdNONE);
  return (*pool->class->condemn)(condemnedReturn, pool, space, ti);
}

void PoolGrey(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  AVERT(Space, space);
  AVER(pool->space == space);
  AVERT(TraceId, ti);
  AVER(ti != TraceIdNONE);
  (*pool->class->grey)(pool, space, ti);
}

Res PoolScan(ScanState ss, Pool pool, Bool *finishedReturn)
{
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVER(finishedReturn != NULL);
  return (*pool->class->scan)(ss, pool, finishedReturn);
}

/* See impl.h.mpm for macro version; see design.mps.pool.req.fix */
Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO)
{
  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(refIO != NULL);
  return PoolFix(pool, ss, seg, refIO);
}

void PoolReclaim(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  AVERT(Space, space);
  AVER(pool->space == space);
  (*pool->class->reclaim)(pool, space, ti);
}

void PoolAccess(Pool pool, Seg seg, AccessSet mode)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  (*pool->class->access)(pool, seg, mode);
}


Res PoolDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  Ring node;

  AVERT(Pool, pool);
  AVER(stream != NULL);
  
  res = WriteF(stream,
               "Pool $P ($U) {\n", (void *)pool, (unsigned long)pool->serial,
               "  class $P (\"$S\")\n", (void *)pool->class, pool->class->name,
               "  space $P ($U)\n", (void *)pool->space, (unsigned long)pool->space->serial,
               "  alignment $W\n", (Word)pool->alignment,
               NULL);
  if(res != ResOK) return res;

  res = (*pool->class->describe)(pool, stream);
  if(res != ResOK) return res;

  node = RingNext(&pool->bufferRing);
  while(node != &pool->bufferRing) {
    Buffer buffer = RING_ELT(Buffer, poolRing, node);
    res = BufferDescribe(buffer, stream);
    if(res != ResOK) return res;
    node = RingNext(node);
  }

  res = WriteF(stream,
               "} Pool $P ($U)\n", (void *)pool, (unsigned long)pool->serial,
               NULL);
  if(res != ResOK) return res;

  return ResOK;
}


/* .pool.space: Thread safe; see design.mps.interface.c.thread-safety */
/* See impl.h.mpm for macro version */
Space (PoolSpace)(Pool pool)
{
  AVERT(Pool, pool);
  return pool->space;
}


Res PoolSegAlloc(Seg *segReturn, Pool pool, Size size)
{
  Res res;
  Seg seg;
  Space space;

  AVER(segReturn != NULL);
  AVERT(Pool, pool);
  space = PoolSpace(pool);
  AVER(SizeIsAligned(size, ArenaAlign(space)));

  res = SegAlloc(&seg, space, size, pool);
  if(res != ResOK) return res;

  *segReturn = seg;
  return ResOK;
}


void PoolSegFree(Pool pool, Seg seg)
{
  Space space;

  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(seg->pool == pool);

  space = PoolSpace(pool);

  ShieldFlush(space); /* See impl.c.shield.shield.flush */

  SegFree(space, seg);
}


Bool PoolOfAddr(Pool *poolReturn, Space space, Addr addr)
{
  Seg seg;

  AVER(poolReturn != NULL);
  /* Cannot AVERT space here, because PoolOfAddr is called under SpaceCheck */

  if(SegOfAddr(&seg, space, addr)) {
    *poolReturn = seg->pool;
    return TRUE;
  }

  return FALSE;
}


Bool PoolHasAddr(Pool pool, Addr addr)
{
  Pool addrPool;
  Space space;
  Bool managed;

  AVERT(Pool, pool);

  space = PoolSpace(pool);
  managed = PoolOfAddr(&addrPool, space, addr);
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


/* PoolNo*, PoolTriv* -- Trivial and non-methods for Pool Classes 
 * See design.mps.pool.no and design.mps.pool.triv
 */

void PoolTrivFinish(Pool pool)
{
  AVERT(Pool, pool);
  NOOP;
}

Res PoolNoAlloc(Addr *pReturn, Pool pool, Size size)
{
  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  NOTREACHED;
  return ResUNIMPL;
}

Res PoolTrivAlloc(Addr *pReturn, Pool pool, Size size)
{
  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
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

Res PoolNoBufferInit(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  NOTREACHED;
  return ResUNIMPL;
}

/* The generic method initialised all generic fields; */
/* This doesn't override any fields */
Res PoolTrivBufferInit(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  return ResOK;
}

void PoolNoBufferFinish(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buf);
  NOTREACHED;
}

void PoolTrivBufferFinish(Pool pool, Buffer buf)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buf);
  NOOP;
}

Res PoolNoBufferFill(Addr *baseReturn, Pool pool, Buffer buffer, Size size)
{
  AVER(baseReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  NOTREACHED;
  return ResUNIMPL;
}

Bool PoolNoBufferTrip(Pool pool, Buffer buffer, Addr base, Size size)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(base != NULL);
  AVER(size > 0);
  NOTREACHED;
  return FALSE;
}

void PoolNoBufferExpose(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  NOTREACHED;
}

void PoolNoBufferCover(Pool pool, Buffer buffer)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  NOTREACHED;
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

Res PoolNoCondemn(RefSet *condemnedReturn, Pool pool, Space space, TraceId ti)
{
  AVER(condemnedReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Space, space);
  AVER(TraceIdCheck(ti));
  NOTREACHED;
  return ResUNIMPL;
}

void PoolNoGrey(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  AVERT(Space, space);
  AVER(TraceIdCheck(ti));
  NOTREACHED;
}

Res PoolNoScan(ScanState ss, Pool pool, Bool *finishedReturn)
{
  AVERT(ScanState, ss);
  AVERT(Pool, pool);
  AVER(finishedReturn != NULL);
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

void PoolNoReclaim(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  AVERT(Space, space);
  AVER(TraceIdCheck(ti));
  NOTREACHED;
}

void PoolNoAccess(Pool pool, Seg seg, AccessSet mode)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  NOTREACHED;
}

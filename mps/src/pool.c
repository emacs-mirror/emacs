/* impl.c.pool: POOL IMPLEMENTATION
 *
 * $HopeName: MMsrc!pool.c(MMdevel_lib.4) $
 * Copyright (C) 1994,1995,1996 Harlequin Group, all rights reserved
 *
 * This is the implementation of the generic pool interface.  The
 * functions here dispatch to pool-specific methods.
 */

#include "mpm.h"

SRCID(pool, "$HopeName: MMsrc!pool.c(MMdevel_lib.4) $");


Bool PoolClassCheck(PoolClass class)
{
  CHECKS(PoolClass, class);
  CHECKL(class->name != NULL);
  CHECKL(class->size >= sizeof(PoolStruct));
  CHECKL(class->offset <= (size_t)(class->size - sizeof(PoolStruct)));
  CHECKL(class->init != NULL);
  CHECKL(class->finish != NULL);
  CHECKL(class->alloc != NULL);
  CHECKL(class->free != NULL);
  CHECKL(class->bufferInit != NULL);
  CHECKL(class->bufferFinish != NULL);
  CHECKL(class->bufferFill != NULL);
  CHECKL(class->bufferTrip != NULL);
  CHECKL(class->bufferExpose != NULL);
  CHECKL(class->bufferCover != NULL);
  CHECKL(class->condemn != NULL);
  CHECKL(class->grey != NULL);
  CHECKL(class->scan != NULL);
  CHECKL(class->fix != NULL);
  CHECKL(class->reclaim != NULL);
  CHECKL(class->access != NULL);
  CHECKL(class->describe != NULL);
  CHECKL(class->endSig == PoolClassSig);
  return TRUE;
}

Bool PoolCheck(Pool pool)
{
  CHECKS(Pool, pool);
  CHECKU(Space, pool->space);
  CHECKL(pool->serial < pool->space->poolSerial);
  CHECKD(PoolClass, pool->class);
  CHECKL(RingCheck(&pool->spaceRing));
  CHECKL(RingCheck(&pool->bufferRing));
  CHECKL(AlignCheck(pool->alignment));
  return TRUE;
}


/* PoolInitV -- initialize a pool
 *
 * Initialize the generic fields of the pool.  The pool gets the
 * default alignment initially.
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

  pool->class = class;
  pool->space = space;
  RingInit(&pool->spaceRing);
  RingInit(&pool->bufferRing);
  pool->alignment = ARCH_ALIGN;

  pool->sig = PoolSig;
  pool->serial = space->poolSerial;
  ++space->poolSerial;

  AVERT(Pool, pool);

  /* Do class-specific initialization. */
  res = (*class->init)(pool, args);
  if(res != ResOK)
    goto failInit;

  RingAppend(SpacePoolRing(space), &pool->spaceRing);
  return ResOK;

failInit:
  pool->sig = SigInvalid;
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->spaceRing);
  return res;
}


Res PoolCreate(Pool *poolReturn, PoolClass class, Space space, ...)
{
  Res res;
  va_list arg;
  va_start(arg, space);
  res = PoolCreateV(poolReturn, class, space, arg);
  va_end(arg);
  return res;
}

Res PoolCreateV(Pool *poolReturn, PoolClass class,
                  Space space, va_list arg)
{
  Res res;
  Pool pool;
  Addr base;

  AVER(poolReturn != NULL);
  AVERT(Space, space);

  /* Allocate the pool instance structure with the size requested */
  /* in the pool class. */
  res = SpaceAlloc(&base, space, class->size);
  if(res != ResOK) return res;

  /* Calculate the adress of the generic pool structure within the */
  /* instance by using the offset information from the class. */
  pool = (Pool)AddrAdd(base, class->offset);

  /* Initialize the pool. */  
  res = PoolInitV(pool, space, class, arg);
  if(res != ResOK) {
    SpaceFree(space, base, class->size);
    return res;
  }
  
  *poolReturn = pool;  
  return ResOK;
}


void PoolFinish(Pool pool)
{
  PoolClass class;

  AVERT(Pool, pool);  
  
  class = pool->class;

  /* Do any class-specific finishing. */
  (*class->finish)(pool);
  
  /* Detach the pool from the space, and unsig it. */
  RingRemove(&pool->spaceRing);
  pool->sig = SigInvalid;
  
  /* Finish the generic fields. */
  RingFinish(&pool->bufferRing);
  RingFinish(&pool->spaceRing);
}

void PoolDestroy(Pool pool)
{
  PoolClass class;
  Space space;
  Addr base;

  AVERT(Pool, pool);  
  
  class = pool->class;
  space = pool->space;

  /* Finish the pool instance structure. */
  PoolFinish(pool);

  /* Free the pool instance structure. */
  base = AddrSub((Addr)pool, class->offset);
  SpaceFree(space, base, class->size);
}


Res (PoolAlloc)(Addr *pReturn, Pool pool, Size size)
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

void (PoolFree)(Pool pool, Addr old, Size size)
{
  AVERT(Pool, pool);
  AVER(old != (Addr)0);
  AVER(PoolHasAddr(pool, old));
  (*pool->class->free)(pool, old, size);
}

Res (PoolCondemn)(RefSet *condemnedReturn, Pool pool,
                  Space space, TraceId ti)
{
  AVERT(Pool, pool);
  return (*pool->class->condemn)(condemnedReturn, pool, space, ti);
}

void (PoolGrey)(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  (*pool->class->grey)(pool, space, ti);
}

Res (PoolScan)(ScanState ss, Pool pool, Bool *finishedReturn)
{
  AVERT(Pool, pool);
  return (*pool->class->scan)(ss, pool, finishedReturn);
}

Res (PoolFix)(Pool pool, ScanState ss, Seg seg, Addr *refIO)
{
  AVERT(Pool, pool);
  return PoolFix(pool, ss, seg, refIO);
}

void (PoolReclaim)(Pool pool, Space space, TraceId ti)
{
  AVERT(Pool, pool);
  (*pool->class->reclaim)(pool, space, ti);
}

void (PoolAccess)(Pool pool, Seg seg, AccessSet mode)
{
  AVERT(Pool, pool);
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


/* Thread safe */
Space (PoolSpace)(Pool pool)
{
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
  if(res != ResOK)
    return res;

  seg->pool = pool;

  *segReturn = seg;
  return ResOK;
}


void PoolSegFree(Pool pool, Seg seg)
{
  Space space;

  AVERT(Pool, pool);

  space = PoolSpace(pool);

  ShieldFlush(space);

  SegFree(space, seg);
}


Bool PoolOfAddr(Pool *poolReturn, Space space, Addr addr)
{
  Seg seg;

  AVER(poolReturn != NULL);

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

  AVERT(Pool, pool);

  space = PoolSpace(pool);
  if(PoolOfAddr(&addrPool, space, addr) && addrPool == pool)
    return TRUE;
  else
    return FALSE;
}


Align (PoolAlignment)(Pool pool)
{
  AVERT(Pool, pool);
  return pool->alignment;
}


/* PoolNo*, PoolTriv* -- Trivial and non-methods for Pool Classes
 *
 * If a pool class doesn't implement a method, and doesn't expect it
 * to be called, it should use a non-method (PoolNo*) which will cause
 * an assertion failure if they are reached.
 *
 * If a pool class supports a protocol but does not require any more
 * than a trivial implementation, it should use a trivial method
 * (PoolTriv*) which will do the trivial thing.
 */

Res PoolNoAlloc(Addr *pReturn, Pool pool, Size size)
{
  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  NOTREACHED;
  return ResUNIMPL;
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

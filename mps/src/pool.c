/* impl.c.pool: POOL IMPLEMENTATION
 *
 * $HopeName: MMsrc!pool.c(trunk.10) $
 * Copyright (C) 1994,1995,1996 Harlequin Group, all rights reserved
 *
 * This is the implementation of the generic pool interface.  The
 * functions here dispatch to pool-specific methods.
 */

#include "std.h"
#include "lib.h"
#include "error.h"
#include "pool.h"
#include "poolst.h"
#include "space.h"
#include "ref.h"
#include "trace.h"
#include "prot.h"
#include <stddef.h>
#include <stdarg.h>

SRCID("$HopeName: MMsrc!pool.c(trunk.10) $");


Bool PoolIsValid(Pool pool, ValidationType validParam)
{
  AVER(pool != NULL);
  AVER(pool->sig == PoolSig);
  AVER(ISVALIDNESTED(DequeNode, &pool->spaceDeque));
  AVER(ISVALIDNESTED(Deque, &pool->segDeque));
  AVER(ISVALIDNESTED(Deque, &pool->bufferDeque));
  AVER(IsPoT(pool->alignment));
  return TRUE;
}


void PoolInit(Pool pool, Space space, PoolClass class)
{
  AVER(pool != NULL);
  AVER(ISVALID(Space, space));

  pool->class = class;
  DequeNodeInit(&pool->spaceDeque);
  DequeInit(&pool->segDeque);
  DequeInit(&pool->bufferDeque);
  pool->alignment = ARCH_ALIGNMOD;

  pool->sig = PoolSig;

  AVER(ISVALID(Pool, pool));

  DequeAppend(SpacePoolDeque(space), &pool->spaceDeque);
}


void PoolFinish(Pool pool)
{
  AVER(ISVALID(Pool, pool));

  DequeNodeRemove(&pool->spaceDeque);
  DequeNodeFinish(&pool->spaceDeque);

  DequeFinish(&pool->bufferDeque);
  DequeFinish(&pool->segDeque);

  pool->sig = SigInvalid;
}
  

Error PoolCreate(Pool *poolReturn, PoolClass class, Space space, ...)
{
  Error e;
  va_list arg;
  va_start(arg, space);
  e = PoolCreateV(poolReturn, class, space, arg);
  va_end(arg);
  return e;
}

Error PoolCreateV(Pool *poolReturn, PoolClass class,
                  Space space, va_list arg)
{
  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  return (*class->create)(poolReturn, space, arg);
}

void PoolDestroy(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  (*pool->class->destroy)(pool);
}


Error (PoolAlloc)(Addr *pReturn, Pool pool, Size size)
{
  Error e;

  AVER(pReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(size > 0);

  e = (*pool->class->alloc)(pReturn, pool, size);
  if(e != ErrSUCCESS) return e;

  /* Make sure that the allocated address was in the pool's memory. */  
  AVER(PoolHasAddr(pool, (Addr)*pReturn));

  SpacePoll(PoolSpace(pool));

  return ErrSUCCESS;
}

void PoolFree(Pool pool, Addr old, Size size)
{
  AVER(ISVALID(Pool, pool));
  AVER(old != (Addr)0);
  AVER(PoolHasAddr(pool, old));

  if(pool->class->free != NULL)
    (*pool->class->free)(pool, old, size);
}


Error PoolCondemn(Pool pool, Trace trace)
{
  AVER(pool->class->condemn != NULL);
  return (*pool->class->condemn)(pool, trace);
}

void PoolMark(Pool pool, Trace trace)
{
  if(pool->class->mark != NULL)
    (*pool->class->mark)(pool, trace);
}

Error PoolScan(Pool pool, Trace trace)
{
  if(pool->class->scan != NULL)
    return (*pool->class->scan)(pool, trace);
  return ErrSUCCESS;
}

Error PoolFix(Pool pool, Trace trace, Arena arena, Addr *refIO)
{
  if(pool->class->fix != NULL)
    return (*pool->class->fix)(pool, trace, arena, refIO);
  return ErrSUCCESS;
}

void PoolReclaim(Pool pool, Trace trace)
{
  AVER(pool->class->reclaim != NULL);
  (*pool->class->reclaim)(pool, trace);
}


void PoolAccess(Pool pool, Addr seg, ProtMode mode)
{
  if(pool->class->access != NULL)
    (*pool->class->access)(pool, seg, mode);
}


Size PoolPoll(Pool pool)
{
  if(pool->class->poll != NULL)
    return (*pool->class->poll)(pool);
  return SPACE_POLL_MAX;
}


Error PoolDescribe(Pool pool, LibStream stream)
{
  AVER(ISVALID(Pool, pool));
  AVER(stream != NULL);

  LibFormat(stream,
          "Pool %p {\n"
          "  Class %s\n"
          "  alignment %lu\n",
          pool,
          pool->class->name,
          (unsigned long)pool->alignment);

  if(DequeLength(&pool->bufferDeque) > 0)
  {
    DequeNode node = DequeFirst(&pool->bufferDeque);
    
    LibFormat(stream, "  Buffers\n");
    
    while(node != DequeSentinel(&pool->bufferDeque))
    {
      node = DequeNodeNext(node);
    }
  }

  if(pool->class->describe == NULL)
    LibFormat(stream, "  No class-specific description available.\n");
  else
    (void)(*pool->class->describe)(pool, stream);

  LibFormat(stream, "} Pool %p\n", pool);

  return ErrSUCCESS;
}


/* Thread safe */
Space (PoolSpace)(Pool pool)
{
  return PARENT(SpaceStruct, poolDeque, pool->spaceDeque.deque);
}

PoolClass (PoolGetClass)(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  return pool->class;
}


Error PoolSegAlloc(Addr *segReturn, Pool pool, Addr size)
{
  Error e;
  Arena arena;
  Pool arpool;
  Addr seg;

  AVER(segReturn != NULL);
  AVER(ISVALID(Pool, pool));
  arena = SpaceArena(PoolSpace(pool));
  AVER(IsAligned(ArenaGrain(arena), size));

  arpool = PoolArenaPool(arena);
  e = PoolAlloc(&seg, arpool, size);
  if(e != ErrSUCCESS)
    return e;

  ArenaPut(arena, seg, ARENA_POOL, (void *)pool);

  *segReturn = seg;
  return ErrSUCCESS;
}


void PoolSegFree(Pool pool, Addr seg, Addr size)
{
  Arena arena;
  Pool arpool;

  AVER(ISVALID(Pool, pool));

  arena = SpaceArena(PoolSpace(pool));
  arpool = PoolArenaPool(arena);

  ArenaPut(arena, seg, ARENA_POOL, (void *)arpool);

  PoolFree(arpool, (Addr)seg, (Size)size);
}


Pool PoolOfSeg(Arena arena, Addr seg)
{
  Pool pool;

  pool = (Pool)ArenaGet(arena, seg, ARENA_POOL);
  AVER(ISVALID(Pool, pool));

  return pool;
}

Bool PoolOfAddr(Pool *poolReturn, Arena arena, Addr addr)
{
  Addr seg;
  
  AVER(poolReturn != NULL);
  AVER(ISVALID(Arena, arena));

  if(ArenaSegBase(&seg, arena, addr))
  {
    Pool pool = PoolOfSeg(arena, seg);
    *poolReturn = pool;
    return TRUE;
  }
  
  return FALSE;
}


Bool PoolHasAddr(Pool pool, Addr addr)
{
  Pool addrPool;
  Arena arena;

  AVER(ISVALID(Pool, pool));

  arena = SpaceArena(PoolSpace(pool));
  if(PoolOfAddr(&addrPool, arena, addr) && addrPool == pool)
    return TRUE;
  else
    return FALSE;
}

     
DequeNode (PoolSpaceDeque)(Pool pool)
{
  AVER(ISVALID(Pool, pool));

  return &pool->spaceDeque;
}


Deque (PoolBufferDeque)(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  return &pool->bufferDeque;
}

Addr (PoolAlignment)(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  return pool->alignment;
}

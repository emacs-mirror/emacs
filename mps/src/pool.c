/*  ==== POOLS ====
 *
 *  $HopeName: MMsrc/!pool.c(trunk.1)$
 *
 *  Copyright (C) 1994,1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of the generic pool interface.  The
 *  functions here dispatch to pool-specific methods.
 *
 *  Notes
 *   2. Some explanatory comments would be nice.  richard 1994-08-25
 */

#include "std.h"
#include "lib.h"
#include "error.h"
#include "pool.h"
#include "poolst.h"
#include "space.h"
#include <stddef.h>
#include <stdarg.h>


#ifdef DEBUG_SIGN
static SigStruct PoolSigStruct;
#endif


#ifdef DEBUG_ASSERT

Bool PoolIsValid(Pool pool, ValidationType validParam)
{
  AVER(pool != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &PoolSigStruct));
  AVER(pool->sig == &PoolSigStruct);
#endif
  AVER(ISVALIDNESTED(DequeNode, &pool->spaceDeque));
  AVER(ISVALIDNESTED(Deque, &pool->segDeque));
  AVER(ISVALIDNESTED(Deque, &pool->bufferDeque));
  AVER(IsPoT(pool->alignment));
  return(TRUE);
}

#endif /* DEBUG_ASSERT */


void PoolInit(Pool pool, Space space, PoolClass class)
{
  AVER(pool != NULL);
  AVER(ISVALID(Space, space));

  pool->class = class;
  DequeNodeInit(&pool->spaceDeque);
  DequeInit(&pool->segDeque);
  DequeInit(&pool->bufferDeque);
  pool->alignment = ARCH_ALIGNMOD;

#ifdef DEBUG_SIGN
  SigInit(&PoolSigStruct, "Pool");
  pool->sig = &PoolSigStruct;
#endif

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

#ifdef DEBUG_SIGN
  pool->sig = SigInvalid;
#endif
}
  

Error PoolCreate(Pool *poolReturn, PoolClass class, Space space, ...)
{
  Error e;
  va_list arg;
  va_start(arg, space);
  e = PoolCreateV(poolReturn, class, space, arg);
  va_end(arg);
  return(e);
}

Error PoolCreateV(Pool *poolReturn, PoolClass class, Space space, va_list arg)
{
  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  return((*class->create)(poolReturn, space, arg));
}

void PoolDestroy(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  (*pool->class->destroy)(pool);
}


Error (PoolAllocP)(void **pReturn, Pool pool, size_t size)
{
  Error e;

  AVER(pReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(size > 0);

  e = (*pool->class->allocP)(pReturn, pool, size);
  if(e != ErrSUCCESS) return(e);

  /* Make sure that the allocated address was in the pool's memory. */  

  return(ErrSUCCESS);
}


void PoolFreeP(Pool pool, void *old, size_t size)
{
  AVER(ISVALID(Pool, pool));
  AVER(old != NULL);

  if(pool->class->freeP != NULL)
    (*pool->class->freeP)(pool, old, size);
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

  return(ErrSUCCESS);
}


Space (PoolSpace)(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  return(PARENT(SpaceStruct, poolDeque, DequeNodeParent(&pool->spaceDeque)));
}

PoolClass (PoolGetClass)(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  return(pool->class);
}


Error PoolSegCreate(Addr *segReturn, Pool pool, Addr size)
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
  e = PoolAllocP((void **)&seg, arpool, size);
  if(e != ErrSUCCESS) return(e);
  ArenaPut(arena, seg, 0, (void *)pool);

  *segReturn = seg;
  return(ErrSUCCESS);
}


Pool PoolOfSeg(Arena arena, Addr seg)
{
  Pool pool;

  pool = (Pool)ArenaGet(arena, seg, 0);
  AVER(ISVALID(Pool, pool));
  return(pool);
}

DequeNode (PoolSpaceDeque)(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  return(&pool->spaceDeque);
}


Deque (PoolBufferDeque)(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  return(&pool->bufferDeque);
}

Addr (PoolAlignment)(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  return(pool->alignment);
}

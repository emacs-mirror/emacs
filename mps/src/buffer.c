/*  ==== ALLOCATION BUFFERS ====
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is the implementation of allocation buffers.
 *
 *  Since allocation buffers are exposed, most of the documentation
 *  is in the interface (buffer.h).  This comment documents the
 *  internal interface, between the pool and the buffer
 *  implementation.
 *
 *  The pool must allocate the buffer descriptor, but initialize it
 *  by calling BufferInit.  The descriptor this creates will fall
 *  through to the reserve method on the first allocation.  In
 *  general, pools should not assign resources to the buffer until
 *  the first allocation, since the buffer may never be used.
 *
 *  The pool may update the base, init, alloc, and limit fields when
 *  the fallback methods are called.  In addition, the pool may set
 *  the limit to zero at any time.  The effect of this is to cause
 *  the _next_ allocation in the buffer to fall through to the buffer
 *  reserve method, and allow the buffer to be flushed and relocated.
 *  A buffer may not be relocated under other circumstances because
 *  there is a race between updating the descriptor and the client
 *  allocation sequence.
 *
 *  When a request does not reasonably fit in the buffer, the pool
 *  may wish to allocate the object "to one side" but continue to use
 *  the buffer for other, smaller requests.  To do this, the reserve
 *  method simply returns a pointer to an object outside the buffer
 *  while leaving the alloc and init pointers equal.  When the client
 *  commits the object, the commit method will be called with a
 *  pointer to this object.
 *
 *  Notes
 *
 *  1. After an object is reserved "to one side" of the buffer, the
 *  fallback reserve method can't assert that the object has been
 *  committed and therefore enforce nesting.  richard 1995-05-03
 *
 *  2. The above description does not allow the pool to extend the
 *  buffer (i.e. raise the limit) asynchronously, although this is
 *  possible in principle.  Maybe we want to relax this if we see a
 *  case where it would be useful.  richard 1995-05-15
 */


#include "std.h"
#include "lib.h"
#include "buffer.h"
#include "pool.h"


#ifdef DEBUG_SIGN
static SigStruct BufferSigStruct;
#endif


DequeNode BufferPoolDeque(Buffer buffer)
{
  AVER(ISVALID(Buffer, buffer));
  
  return(&buffer->poolDeque);
}


Pool BufferPool(Buffer buffer)
{
  Deque deque;
  Pool pool;
  
  AVER(ISVALID(Buffer, buffer));

  deque = DequeNodeParent(BufferPoolDeque(buffer));
  pool = PARENT(PoolStruct, bufferDeque, deque);
  
  return(pool);
}


Error BufferCreate(Buffer *bufferReturn, Pool pool)
{
  AVER(bufferReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(pool->class->bufferCreate != NULL);
  
  return((*pool->class->bufferCreate)(bufferReturn, pool));
}


void BufferDestroy(Buffer buffer)
{
  Pool pool;

  AVER(ISVALID(Buffer, buffer));

  pool = BufferPool(buffer);
  AVER(pool->class->bufferDestroy != NULL);

  AVER(buffer->init == buffer->alloc);

  (*pool->class->bufferDestroy)(buffer);
}


#ifdef DEBUG_ASSERT

Bool BufferIsValid(Buffer buffer, ValidationType validParam)
{
  AVER(buffer != NULL);
#ifdef DEBUG_SIGN
  AVER(ISVALIDNESTED(Sig, &BufferSigStruct));
  AVER(buffer->sig == &BufferSigStruct);
#endif
  AVER(ISVALIDNESTED(DequeNode, &buffer->poolDeque));
  AVER(buffer->base <= buffer->init);
  AVER(buffer->init <= buffer->alloc);
  AVER(buffer->alloc <= buffer->limit || buffer->limit == 0);
  AVER(buffer->reserve != NULL);
  AVER(buffer->commit != NULL);
  AVER(buffer->trip != NULL);
  /* AVER(buffer->alignment == BufferPool(buffer)->alignment); */
  AVER(IsPoT(buffer->alignment));
  AVER(IsAligned(buffer->alignment, buffer->base));
  AVER(IsAligned(buffer->alignment, buffer->init));
  AVER(IsAligned(buffer->alignment, buffer->alloc));
  AVER(IsAligned(buffer->alignment, buffer->limit));
  return(TRUE);
}

#endif


void BufferInit(Buffer buffer,
		Pool pool,
                Error (*reserve)(Addr *pReturn, Buffer buffer,
				 Addr size),
                Bool (*commit)(Buffer buffer, Addr p, Addr size),
                Bool (*trip)(Buffer buffer, Addr p, Addr size))
{
  AVER(ISVALID(Pool, pool));
  AVER(buffer != NULL);

  buffer->base = 0;
  buffer->init = 0;
  buffer->alloc = 0;
  buffer->limit = 0;
  buffer->reserve = reserve;
  buffer->commit = commit;
  buffer->trip = trip;
  buffer->alignment = pool->alignment;
  buffer->p = NULL;
  buffer->i = 0;

  DequeNodeInit(&buffer->poolDeque);
  DequeAppend(&pool->bufferDeque, &buffer->poolDeque);

#ifdef DEBUG_SIGN  
  SigInit(&BufferSigStruct, "Buffer");
  buffer->sig = &BufferSigStruct;
#endif

  AVER(ISVALID(Buffer, buffer));
}


void BufferFinish(Buffer buffer)
{
  AVER(ISVALID(Buffer, buffer));

  DequeNodeRemove(&buffer->poolDeque);

#ifdef DEBUG_SIGN
  buffer->sig = SigInvalid;
#endif
}


/* This is the reserve sequence that the mutator must go through to */
/* reserve space for a proto-object. */

Error BufferReserve(Addr *pReturn, Buffer buffer, Addr size)
{
  Error e;
  Addr next;

  AVER(pReturn != NULL);
  AVER(ISVALID(Buffer, buffer));
  AVER(size > 0);
  AVER(IsAligned(BufferPool(buffer)->alignment, size));

  /* Ensure that the buffer is in the ready state, i.e. it */
  /* is not already being allocated in. */

  AVER(buffer->alloc == buffer->init);

  /* Is there enough room in the unallocated portion of the buffer to */
  /* satisfy the request?  If so, just increase the alloc marker and */
  /* return a pointer to the area below it. */

  next = buffer->alloc + size;
  if(next > buffer->alloc && next <= buffer->limit)
  {
    buffer->alloc = next;
    *pReturn = buffer->init;
    return(ErrSUCCESS);
  }
  
  /* If the buffer can't accommodate the request, fall through to the */
  /* pool-specific allocation method. */

  e = (*buffer->reserve)(pReturn, buffer, size);
  
  AVER(ISVALID(Buffer, buffer));

  return(e);
}


/* After initializing the proto-object, the mutator calls commit to */
/* tell the pool that it is valid.  Commit may return FALSE to */
/* indicate that the client must go back and reserve again. */

Bool BufferCommit(Buffer buffer, Addr p, Addr size)
{
  AVER(ISVALID(Buffer, buffer));
  AVER(size > 0);
  AVER(IsAligned(BufferPool(buffer)->alignment, size));
  
  /* If a flip occurs before this point, the pool will see init */
  /* below the object, so it will be trashed and the commit */
  /* must fail when trip is called.  The pool will also see */
  /* a pointer p which points to the invalid object at init. */

  /* If I==A then the object was reserved outside the */
  /* buffer, so pass the information on to the pool. */

  if(buffer->init == buffer->alloc)
    return((*buffer->commit)(buffer, p, size));

  AVER(p == buffer->init);
  AVER(buffer->init + size == buffer->alloc);
    
  /* Atomically update the init pointer to declare that the object */
  /* is initialized (though it may be invalid if a flip occurred). */

  buffer->init = buffer->alloc;
  
  /* **** Memory barrier here on the DEC Alpha. */

  /* If a flip occurs at this point, the pool will see init */
  /* above the object, which is valid, so it will be collected */
  /* the commit must succeed when trip is called.  The pointer */
  /* p will have been fixed up. */

  /* trip the buffer if a flip has occurred. */

  if(buffer->limit == 0)
    return((*buffer->trip)(buffer, p, size));

  /* No flip occurred, so succeed. */

  return(TRUE);
}



Error BufferDescribe(Buffer buffer, LibStream stream)
{
  AVER(ISVALID(Buffer, buffer));
  AVER(stream != NULL);

  LibFormat(stream,
            "Buffer %p {\n"
            "  Pool %p\n"
            "  alignment %lu\n"
            "  base 0x%lX  init 0x%lX  alloc 0x%lX  limit 0x%lX\n"
            "  reserve %p  commit %p  trip %p\n"
            "} Buffer %p\n",
            (void *)buffer,
            (void *)BufferPool(buffer),
            (unsigned long)buffer->alignment,
            (unsigned long)buffer->base,
            (unsigned long)buffer->init,
            (unsigned long)buffer->alloc,
            (unsigned long)buffer->limit,
            (void *)buffer->reserve, (void *)buffer->commit,
            (void *)buffer->trip, (void *)buffer);

  return(ErrSUCCESS);
}

/* impl.c.buffer: ALLOCATION BUFFER IMPLEMENTATION
 *
 * $HopeName: MMsrc!buffer.c(MMdevel_lib.3) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved
 *
 * This is the interface to allocation buffers.
 *
 * An allocation buffer is an interface to a pool which provides
 * very fast allocation, and defers the need for synchronization in
 * a multi-threaded environment.
 *
 * Pools which contain formatted objects must be synchronized so
 * that the pool can know when an object is valid.  Allocation from
 * such pools is done in two stages: reserve and commit.  The client
 * first reserves memory, then initializes it, then commits.
 * Committing the memory declares that it contains a valid formatted
 * object.  Under certain conditions, some pools may cause the
 * commit operation to fail.  (See the documentation for the pool.)
 * Failure to commit indicates that the whole allocation failed and
 * must be restarted.  When a pool with commit failure, the
 * allocation sequence could look something like this:
 *
 * do {
 *   res = BufferReserve(&p, buffer, size);
 *   if(res != ResOK) return res;       // allocation fails, reason res
 *   initialize(p);                     // p now points at valid object
 * } while(!BufferCommit(buffer, p, size));
 *
 * Pools which do not contain formatted objects can use a one-step
 * allocation as usual.  Effectively any random rubbish counts as a
 * "valid object" to such pools.
 *
 * An allocation buffer is an area of memory which is pre-allocated
 * from a pool, plus a buffer descriptor, which contains, inter
 * alia, four pointers: base, init, alloc, and limit.  Base points
 * to the base address of the area, limit to the last address plus
 * one.  Init points to the first uninitialized address in the
 * buffer, and alloc points to the first unallocated address.
 *
 *    L . - - - - - .
 *      |           |
 *      |   junk    |
 *      |           |       the "busy" state, after Reserve
 *    A |-----------|
 *      |  uninit   |
 *    I |-----------|
 *      |   init    |
 *      |           |
 *    B `-----------'
 *
 *    L . - - - - - .
 *      |           |
 *      |   junk    |
 *      |           |       the "ready" state, after Commit
 *  A=I |-----------|
 *      |           |
 *      |           |
 *      |   init    |
 *      |           |
 *    B `-----------'
 *
 * Access to these pointers is restricted in order to allow
 * synchronization between the pool and the client.  The client may
 * only write to init and alloc, but in a restricted and atomic way
 * detailed below.  The pool may read the contents of the buffer
 * descriptor at _any_ time.  During calls to the fill and trip
 * methods, the pool may update any or all of the fields
 * in the buffer descriptor.  The pool may update the limit at _any_
 * time.
 *
 * Only one thread may use a buffer at once, unless the client
 * places a mutual exclusion around the buffer access in the usual
 * way.  In such cases it is usually better to create one buffer for
 * each thread.
 *
 * Here are pseudo-code descriptions of the reserve and commit
 * operations.  These may be implemented in-line by the client.
 * Note that the client is responsible for ensuring that the size
 * (and therefore the alloc and init pointers) are aligned according
 * to the buffer's alignment.
 *
 * Reserve(buf, size)                   ; size must be aligned to pool
 *   if buf->limit - buf->alloc >= size then
 *     buf->alloc +=size                ; must be atomic update
 *     p = buf->init
 *   else
 *     res = BufferFill(&p, buf, size)  ; buf contents may change
 *
 * Commit(buf, p, size)
 *   buf->init = buf->alloc             ; must be atomic update
 *   if buf->limit == 0 then
 *     b = BufferTrip(buf, p, size)     ; buf contents may change
 *
 * The pool must allocate the buffer descriptor and initialize it by
 * calling BufferInit.  The descriptor this creates will fall
 * through to the fill method on the first allocation.  In general,
 * pools should not assign resources to the buffer until the first
 * allocation, since the buffer may never be used.
 *
 * The pool may update the base, init, alloc, and limit fields when
 * the fallback methods are called.  In addition, the pool may set
 * the limit to zero at any time.  The effect of this is either:
 *
 *   1. cause the _next_ allocation in the buffer to fall through to
 *      the buffer fill method, and allow the buffer to be flushed
 *      and relocated;
 *
 *   2. cause the buffer trip method to be called if the client was
 *      between reserve and commit.
 *
 * A buffer may not be relocated under other circumstances because
 * there is a race between updating the descriptor and the client
 * allocation sequence.
 */

#include "mpm.h"

SRCID(buffer, "$HopeName: MMsrc!buffer.c(MMdevel_lib.3) $");


Ring BufferPoolRing(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return &buffer->poolRing;
}

Pool (BufferPool)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return buffer->pool;
}


/* BufferCreate -- create an allocation buffer in a pool
 *
 * The buffer structure is allocated from the space control pool,
 * and initialized with generic valid contents.  If the pool
 * provides a bufferInit method then it is called and may perform
 * additional initialization, otherwise the base, init,
 * alloc, and limit fields are set to zero, so that the fill method
 * will get called the first time a reserve operation is attempted.
 *
 * Iff successful, *bufferReturn is updated with a pointer to the
 * buffer descriptor, and ResOK is returned.
 */

Res BufferCreate(Buffer *bufferReturn, Pool pool, Rank rank)
{
  Res res;
  Buffer buffer;
  Space space;

  AVER(bufferReturn != NULL);
  AVERT(Pool, pool);
  AVER((pool->class->attr & AttrBUF) != 0);
  
  space = PoolSpace(pool);

  /* Allocate the buffer structure. */  
  res = SpaceAlloc((Addr *)&buffer, space, sizeof(BufferStruct));
  if(res != ResOK) return res;

  /* Initialize the generic fields of the buffer. */
  buffer->base = 0;
  buffer->pool = pool;
  buffer->space = space;
  buffer->ap.init = 0;
  buffer->ap.alloc = 0;
  buffer->ap.limit = 0;
  buffer->alignment = pool->alignment;
  buffer->exposed = FALSE;
  buffer->seg = NULL;
  buffer->rank = rank;
  buffer->shieldMode = AccessSetEMPTY;
  buffer->grey = TraceSetEMPTY;
  buffer->p = NULL;
  buffer->i = 0;
  RingInit(&buffer->poolRing);

  /* Dispatch to the pool class method to perform any extra */
  /* initialization of the buffer. */
  res = (*pool->class->bufferInit)(pool, buffer);
  if(res != ResOK) {
    SpaceFree(space, (Addr)buffer, sizeof(BufferStruct));
    return res;
  }

  /* Now that it's initialized, sign the buffer and check it. */
  buffer->sig = BufferSig;
  buffer->serial = pool->bufferSerial;
  ++pool->bufferSerial;
  AVERT(Buffer, buffer);

  /* Attach the initialized buffer to the pool. */
  RingAppend(&pool->bufferRing, &buffer->poolRing);

  *bufferReturn = buffer;
  return ResOK;
}


/* BufferDestroy -- destroy an allocation buffer
 *
 * Destroy frees a buffer descriptor.  The buffer must be in the
 * "ready" state, i.e. not between a Reserve and Commit.  Allocation
 * in the area of memory to which the descriptor refers must cease
 * after Destroy is called.
 *
 * Destroying an allocation buffer does not affect objects which have
 * been allocated, it just frees resources associated with the buffer
 * itself.
 *
 * The pool class's bufferDestroy method is called and then the
 * buffer structure is uninitialized and freed.
 */

void BufferDestroy(Buffer buffer)
{
  Space space;
  Pool pool;

  AVERT(Buffer, buffer);

  /* Make a copy of the space before the buffer gets finished. */
  space = buffer->space;
  pool = buffer->pool;

  AVER((pool->class->attr & AttrBUF) != 0);
  AVER(BufferIsReady(buffer));
  AVER(buffer->exposed == FALSE);

  /* Detach the buffer from its owning pool. */
  RingRemove(&buffer->poolRing);
  
  /* Dispatch to the pool class method to finish the buffer. */
  (*pool->class->bufferFinish)(pool, buffer);

  /* Unsign the finished buffer. */
  buffer->sig = SigInvalid;
  
  /* Finish off the generic buffer fields and deallocate the */
  /* buffer structure. */
  RingFinish(&buffer->poolRing);
  SpaceFree(space, (Addr)buffer, sizeof(BufferStruct));
}


Bool BufferCheck(Buffer buffer)
{
  CHECKS(Buffer, buffer);
  CHECKU(Pool, buffer->pool);
  CHECKL(buffer->serial < buffer->pool->bufferSerial);
  CHECKU(Space, buffer->space);
  CHECKL(RingCheck(&buffer->poolRing));
  CHECKL(TraceSetCheck(buffer->grey));
  CHECKL(buffer->base <= buffer->ap.init);
  CHECKL(buffer->ap.init <= buffer->ap.alloc);
  CHECKL(buffer->ap.alloc <= buffer->ap.limit || buffer->ap.limit == 0);
  CHECKL(buffer->alignment == buffer->pool->alignment);
  CHECKL(AlignCheck(buffer->alignment));
  CHECKL(AddrIsAligned(buffer->base, buffer->alignment));
  CHECKL(AddrIsAligned(buffer->ap.init, buffer->alignment));
  CHECKL(AddrIsAligned(buffer->ap.alloc, buffer->alignment));
  CHECKL(AddrIsAligned(buffer->ap.limit, buffer->alignment));
  if(buffer->seg != NULL) {
    CHECKL(SegCheck(buffer->seg));
    CHECKL(buffer->rank == buffer->seg->rank);
  }
  return TRUE;
}


/* BufferSet/Reset -- set/reset a buffer
 *
 * Set sets the buffer base, init, alloc, and limit fields so that
 * the buffer is ready to start allocating in area of memory.  The
 * alloc field is a copy of the init field.
 *
 * Reset sets the buffer base, init, alloc, and limit fields to
 * zero, so that the next reserve request will call the fill
 * method.
 *
 * BufferIsReset returns TRUE iff the buffer is in the reset state,
 * i.e.  with base, init, alloc, and limit set to zero.
 */

void BufferSet(Buffer buffer, Seg seg, Addr base, Addr init, Addr limit)
{
  AVERT(Buffer, buffer);

  buffer->seg = seg;
  buffer->base = base;
  buffer->ap.init = init;
  buffer->ap.alloc = init;
  buffer->ap.limit = limit;
}

void BufferReset(Buffer buffer)
{
  AVERT(Buffer, buffer);

  buffer->seg = NULL;
  buffer->base = 0;
  buffer->ap.init = 0;
  buffer->ap.alloc = 0;
  buffer->ap.limit = 0;
}


/* Buffer Information
 *
 * BufferPoolRing is a convenience function for accessing the ring
 * node which attaches a buffer descriptor to a pool.
 *
 * BufferPool returns the pool to which a buffer is attached.
 *
 * BufferIsReady returns TRUE iff the buffer is not between a
 * reserve and commit.  The result is only reliable if the client is
 * not currently using the buffer, since it may update the alloc and
 * init pointers asynchronously.
 *
 * BufferAP returns the APStruct substructure of a buffer.
 *
 * BufferOfAP is a thread-safe (impl.c.mpsi.thread-safety) method of
 * getting the buffer which owns an APStruct.
 *
 * BufferSpace is a thread-safe (impl.c.mpsi.thread-safety) method of
 * getting the space which owns a buffer.
 */

Bool BufferIsReset(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->base == 0 &&
     buffer->ap.init == 0 &&
     buffer->ap.alloc == 0 &&
     buffer->ap.limit == 0)
    return TRUE;

  return FALSE;
}


Bool BufferIsReady(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->ap.init == buffer->ap.alloc)
    return TRUE;

  return FALSE;
}

AP BufferAP(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return &buffer->ap;
}

/* This method must be thread-safe.  See impl.c.mpsi.thread-safety. */
Buffer BufferOfAP(AP ap)
{
  return PARENT(BufferStruct, ap, ap);
}

/* This method must be thread-safe.  See impl.c.mpsi.thread-safety. */
Space BufferSpace(Buffer buffer)
{
  return buffer->space;
}


/* BufferReserve -- reserve memory from an allocation buffer
 *
 * This is a provided version of the reserve procedure described
 * above.  The size must be aligned according to the buffer
 * alignment.  Iff successful, ResOK is returned and
 * *pReturn updated with a pointer to the reserved memory.
 * Otherwise *pReturn it not touched.  The reserved memory is not
 * guaranteed to have any particular contents.  The memory must be
 * initialized with a valid object (according to the pool to which
 * the buffer belongs) and then passed to the Commit method (see
 * below).  Reserve may not be applied twice to a buffer without a
 * commit in-between.  In other words, Reserve/Commit pairs do not
 * nest.
 */

Res BufferReserve(Addr *pReturn, Buffer buffer, Word size)
{
  Addr next;

  AVER(pReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  AVER(BufferIsReady(buffer));

  /* Is there enough room in the unallocated portion of the buffer to */
  /* satisfy the request?  If so, just increase the alloc marker and */
  /* return a pointer to the area below it. */

  next = AddrAdd(buffer->ap.alloc, size);
  if(next > buffer->ap.alloc && next <= buffer->ap.limit)
  {
    buffer->ap.alloc = next;
    *pReturn = buffer->ap.init;
    return ResOK;
  }

  /* If the buffer can't accommodate the request, fall through to the */
  /* pool-specific allocation method. */

  return BufferFill(pReturn, buffer, size);
}


/* BufferFill -- refill an empty buffer
 *
 * If there is not enough space in a buffer to allocate in-line,
 * BufferFill must be called to "refill" the buffer.  (See the
 * description of the in-line Reserve method in the leader comment.)
 */

Res BufferFill(Addr *pReturn, Buffer buffer, Word size)
{
  Res res;
  Pool pool;

  AVER(pReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  AVER(BufferIsReady(buffer));

  pool = BufferPool(buffer);
  res = (*pool->class->bufferFill)(pReturn, pool, buffer, size);

  AVERT(Buffer, buffer);

  return res;
}


/* BufferCommit -- commit memory previously reserved
 *
 * Commit notifies the pool that memory which has been previously
 * reserved (see above) has been initialized with a valid object
 * (according to the pool to which the buffer belongs).  The pointer
 * p must be the same as that returned by Reserve, and the size must
 * match the size passed to Reserve.
 *
 * Commit may not be applied twice to a buffer without a reserve
 * in-between.  In other words, objects must be reserved,
 * initialized, then committed only once.
 *
 * Commit returns TRUE iff successful.  If commit fails and returns
 * FALSE, the client may try to allocate again by going back to the
 * reserve stage, and may not use the memory at p again for any
 * purpose.
 *
 * Some classes of pool may cause commit to fail under rare
 * circumstances.
 */

Bool BufferCommit(Buffer buffer, Addr p, Word size)
{
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));

  /* If a flip occurs before this point, the pool will see init */
  /* below the object, so it will be trashed and the commit */
  /* must fail when trip is called.  The pool will also see */
  /* a pointer p which points to the invalid object at init. */

  AVER(p == buffer->ap.init);
  AVER(AddrAdd(buffer->ap.init, size) == buffer->ap.alloc);

  /* Atomically update the init pointer to declare that the object */
  /* is initialized (though it may be invalid if a flip occurred). */

  buffer->ap.init = buffer->ap.alloc;

  /* **** Memory barrier here on the DEC Alpha. */

  /* If a flip occurs at this point, the pool will see init */
  /* above the object, which is valid, so it will be collected */
  /* the commit must succeed when trip is called.  The pointer */
  /* p will have been fixed up. */

  /* trip the buffer if a flip has occurred. */

  if(buffer->ap.limit == 0)
    return BufferTrip(buffer, p, size);

  /* No flip occurred, so succeed. */

  return TRUE;
}


/* BufferTrip -- act on a tripped buffer
 *
 * The pool which owns a buffer may asyncronously set the buffer limit
 * to zero in order to get control over the buffer.  If this occurs
 * after a Reserve, then the Commit method calls BufferTrip.  (See
 * the description of the in-line Commit in the leader comment.)
 */

Bool BufferTrip(Buffer buffer, Addr p, Word size)
{
  Pool pool;

  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));

  pool = BufferPool(buffer);
  return (*pool->class->bufferTrip)(pool, buffer, p, size);
}


/* BufferShieldExpose/Cover -- buffer shield control
 *
 * BufferExpose guarantees that buffered memory is exposed between a
 * reserve and commit operation.  BufferCover guarantees that
 * buffered memory is covered.
 */

void BufferExpose(Buffer buffer)
{
  Pool pool;

  AVERT(Buffer, buffer);

  buffer->exposed = TRUE;
  pool = BufferPool(buffer);
  (*pool->class->bufferExpose)(pool, buffer);
}

void BufferCover(Buffer buffer)
{
  Pool pool;

  AVERT(Buffer, buffer);

  buffer->exposed = FALSE;
  pool = BufferPool(buffer);
  (*pool->class->bufferCover)(pool, buffer);
}


Res BufferDescribe(Buffer buffer, mps_lib_FILE *stream)
{
  AVERT(Buffer, buffer);
  AVER(stream != NULL);

  WriteF(stream,
         "Buffer $P ($U) {\n", (void *)buffer, (unsigned long)buffer->serial,
         "  base $A  init $A  alloc $A  limit $A\n",
         buffer->base, buffer->ap.init, buffer->ap.alloc, buffer->ap.limit,
         "  Pool $P\n",        (void *)buffer->pool,
         "  Seg $P\n",         (void *)buffer->seg,
         "  rank $U\n",        (unsigned long)buffer->rank,
         "  alignment $W\n",   (Word)buffer->alignment,
         "  grey $B\n",        (unsigned long)buffer->grey,
         "  shieldMode $B\n",  (unsigned long)buffer->shieldMode,
         "  p $P  i $U\n",     buffer->p, (unsigned long)buffer->i,
         "} Buffer $P ($U)\n", (void *)buffer, (unsigned long)buffer->serial,
         NULL);

  return ResOK;
}

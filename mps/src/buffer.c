/* impl.c.buffer: ALLOCATION BUFFER IMPLEMENTATION
 *
 * $HopeName: MMsrc!buffer.c(trunk.17) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved
 *
 * This is (part of) the implementation of allocation buffers.
 *
 * Several macros which also form part of the implementation are
 * in impl.h.mps.
 *
 * Several macros forming part of impl.h.mps should be consistent
 * with the macros and functions in this module.
 *
 * Declarations for functions defined here are in impl.h.mpm.
 *
 * Declarations for the relevant structures are in impl.h.mpmst.
 *
 * DESIGN
 *
 * See design.mps.buffer.
 *
 * TRANSGRESSIONS
 *
 * .trans.mod: There are several instances where pool structures are
 * directly accessed by this module (because impl.c.pool does not
 * provide an adequate (or adequately documented) interface.  They
 * bear this tag.
 */

#include "mpm.h"

SRCID(buffer, "$HopeName: MMsrc!buffer.c(trunk.17) $");


/* BufferCreate -- create an allocation buffer in a pool
 * 
 * design.mps.buffer.method.create
 */

Res BufferCreate(Buffer *bufferReturn, Pool pool, Rank rank)
{
  Res res;
  Buffer buffer;
  Space space;
  void *p;

  AVER(bufferReturn != NULL);
  AVERT(Pool, pool);
  /* The PoolClass should support buffer protocols */
  AVER((pool->class->attr & AttrBUF)); /* .trans.mod */
  AVER(RankCheck(rank));
  
  space = PoolSpace(pool);

  /* Allocate the buffer structure. */  
  res = SpaceAlloc(&p, space, sizeof(BufferStruct));
  if(res != ResOK) return res;
  buffer = p;

  /* Initialize the buffer.  See impl.h.mpmst for a definition of the
   * structure */
  /* sig and serial comes later .init.sig-serial */
  buffer->space = space;
  buffer->pool = pool;
  buffer->seg = NULL;
  buffer->rank = rank;
  buffer->base = (Addr)0;
  buffer->apStruct.init = (Addr)0;
  buffer->apStruct.alloc = (Addr)0;
  buffer->apStruct.limit = (Addr)0;
  buffer->alignment = pool->alignment; /* .trans.mod */
  buffer->exposed = FALSE;
  RingInit(&buffer->poolRing);
  buffer->shieldMode = AccessSetEMPTY;
  buffer->grey = TraceSetEMPTY;
  buffer->p = NULL;
  buffer->i = 0;

  /* Dispatch to the pool class method to perform any extra */
  /* initialization of the buffer. */
  res = (*pool->class->bufferInit)(pool, buffer);
  if(res != ResOK) {
    SpaceFree(space, (Addr)buffer, sizeof(BufferStruct));
    return res;
  }

  /* .init.sig-serial: Now that it's initialized, sign the buffer,
   * give it a serial number, and check it. */
  buffer->sig = BufferSig;
  buffer->serial = pool->bufferSerial; /* .trans.mod */
  ++pool->bufferSerial;
  AVERT(Buffer, buffer);

  /* Attach the initialized buffer to the pool. */
  RingAppend(&pool->bufferRing, &buffer->poolRing);

  *bufferReturn = buffer;
  return ResOK;
}


/* BufferDestroy -- destroy an allocation buffer
 *
 * design.mps.buffer.method.destroy
 */

void BufferDestroy(Buffer buffer)
{
  Space space;
  Pool pool;

  AVERT(Buffer, buffer);

  /* Make a copy of the space before the buffer gets finished. */
  space = buffer->space;
  pool = buffer->pool;

  /* The PoolClass should support buffer protocols */
  AVER((pool->class->attr & AttrBUF)); /* .trans.mod */
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

/* BufferCheck
 *
 * See design.mps.buffer.method.check for design.
 * See impl.h.mpmst for structure definition.
 */

Bool BufferCheck(Buffer buffer)
{
  CHECKS(Buffer, buffer);
  CHECKL(buffer->serial < buffer->pool->bufferSerial); /* .trans.mod */
  CHECKU(Space, buffer->space);
  CHECKU(Pool, buffer->pool);
  /* seg and rank checked in anomalous order */
  CHECKL(RankCheck(buffer->rank));	/* design.mps.check.type.no-sig */
  if(buffer->seg != NULL) {
    CHECKL(SegCheck(buffer->seg));	/* design.mps.check.type.no-sig */
    CHECKL(buffer->rank == buffer->seg->rank);
  }
  CHECKL(buffer->base <= buffer->apStruct.init);
  CHECKL(buffer->apStruct.init <= buffer->apStruct.alloc);
  CHECKL(buffer->apStruct.alloc <= buffer->apStruct.limit ||
	 buffer->apStruct.limit == 0);
  CHECKL(buffer->alignment == buffer->pool->alignment);
  CHECKL(AlignCheck(buffer->alignment));
  CHECKL(AddrIsAligned(buffer->base, buffer->alignment));
  CHECKL(AddrIsAligned(buffer->apStruct.init, buffer->alignment));
  CHECKL(AddrIsAligned(buffer->apStruct.alloc, buffer->alignment));
  CHECKL(AddrIsAligned(buffer->apStruct.limit, buffer->alignment));
  /* .improve.bool-check: */
  CHECKL(buffer->exposed == TRUE || buffer->exposed == FALSE);
  CHECKL(RingCheck(&buffer->poolRing));	/* design.mps.check.type.no-sig */
  /* .improve.accessset: There is no AccessSetCheck */
  CHECKL(TraceSetCheck(buffer->grey));	/* design.mps.check.type.no-sig */
  /* buffer->p, and buffer->i are arbitrary and cannot be checked */
  return TRUE;
}


/* BufferSet/Reset -- set/reset a buffer
 *
 * Set sets the buffer base, init, alloc, and limit fields so that
 * the buffer is ready to start allocating in area of memory.  The
 * alloc field is a copy of the init field.
 *
 * Reset sets the seg, base, init, alloc, and limit fields to
 * zero, so that the next reserve request will call the fill
 * method.
 */

void BufferSet(Buffer buffer, Seg seg, Addr base, Addr init, Addr limit)
{
  AVERT(Buffer, buffer);
  AVER(SegCheck(seg));
  AVER(BufferIsReady(buffer));
  /* No check for base, init, limit */

  buffer->seg = seg;
  buffer->base = base;
  buffer->apStruct.init = init;
  buffer->apStruct.alloc = init;
  buffer->apStruct.limit = limit;
}

void BufferReset(Buffer buffer)
{
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));

  buffer->seg = NULL;
  buffer->base = (Addr)0;
  buffer->apStruct.init = (Addr)0;
  buffer->apStruct.alloc = (Addr)0;
  buffer->apStruct.limit = (Addr)0;
}


/* Buffer Information
 *
 * BufferIsReset returns TRUE if and only if the buffer is in the
 * reset state, i.e.  with base, init, alloc, and limit set to zero.
 *
 * BufferIsReady returns TRUE iff the buffer is not between a
 * reserve and commit.  The result is only reliable if the client is
 * not currently using the buffer, since it may update the alloc and
 * init pointers asynchronously.
 *
 * BufferAP returns the APStruct substructure of a buffer.
 *
 * BufferOfAP is a thread-safe (design.mps.interface.c.thread-safety)
 * method of getting the buffer which owns an APStruct.
 *
 * BufferSpace is a thread-safe (design.mps.interface.c.thread-safety)
 * method of getting the space which owns a buffer.
 *
 * BufferPool returns the pool to which a buffer is attached.
 */

Bool BufferIsReset(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->seg == NULL &&
     buffer->base == (Addr)0 &&
     buffer->apStruct.init == (Addr)0 &&
     buffer->apStruct.alloc == (Addr)0 &&
     buffer->apStruct.limit == (Addr)0)
    return TRUE;

  return FALSE;
}

Bool BufferIsReady(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->apStruct.init == buffer->apStruct.alloc)
    return TRUE;

  return FALSE;
}

AP BufferAP(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return &buffer->apStruct;
}

/* design.mps.buffer.method.ofap */
/* This method must be thread-safe. See */
/* design.mps.interface.c.thread-safety. */
Buffer BufferOfAP(AP ap)
{
  /* .design.mps.misc.parent.thread-safe */
  return PARENT(BufferStruct, apStruct, ap);
}

/* design.mps.buffer.method.space */
/* This method must be thread-safe.  See */
/* design.mps.interface.c.thread-safety. */
Space BufferSpace(Buffer buffer)
{
  return buffer->space;
}

Pool (BufferPool)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return buffer->pool;
}


/* BufferReserve -- reserve memory from an allocation buffer
 */

Res BufferReserve(Addr *pReturn, Buffer buffer, Size size)
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

  next = AddrAdd(buffer->apStruct.alloc, size);
  if(next > buffer->apStruct.alloc && next <= buffer->apStruct.limit)
  {
    buffer->apStruct.alloc = next;
    *pReturn = buffer->apStruct.init;
    return ResOK;
  }

  /* If the buffer can't accommodate the request, fall through to the */
  /* pool-specific allocation method. */

  return BufferFill(pReturn, buffer, size);
}


/* BufferFill -- refill an empty buffer
 */

Res BufferFill(Addr *pReturn, Buffer buffer, Size size)
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
 */

Bool BufferCommit(Buffer buffer, Addr p, Size size)
{
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  /* Buffer is "busy" */
  AVER(!BufferIsReady(buffer));

  /* See design.mps.collection.flip.
   * If a flip occurs before this point, when the pool reads
   * buffer->init it will point below the object, so it will be trashed
   * and the commit must fail when trip is called.  The pool will also
   * read p (during the call to trip) which points to the invalid
   * object at init.
   */

  AVER(p == buffer->apStruct.init);
  AVER(AddrAdd(buffer->apStruct.init, size) == buffer->apStruct.alloc);

  /* Atomically update the init pointer to declare that the object */
  /* is initialized (though it may be invalid if a flip occurred). */

  buffer->apStruct.init = buffer->apStruct.alloc;

  /* .improve.memory-barrier: Memory barrier here on the DEC Alpha
   * (and other relaxed memory order architectures). */

  /* If a flip occurs at this point, the pool will see init */
  /* above the object, which is valid, so it will be collected. */
  /* The commit must succeed when trip is called.  The pointer */
  /* p will have been fixed up. */

  /* Trip the buffer if a flip has occurred. */

  if(buffer->apStruct.limit == 0)
    return BufferTrip(buffer, p, size);

  /* No flip occurred, so succeed. */

  return TRUE;
}


/* BufferTrip -- act on a tripped buffer
 */

Bool BufferTrip(Buffer buffer, Addr p, Size size)
{
  Pool pool;

  AVERT(Buffer, buffer);
  AVER(p == buffer->apStruct.init);
  AVER(size > 0);
  AVER(SizeIsAligned(size, buffer->alignment));
  AVER(AddrAdd(buffer->apStruct.init, size) == buffer->apStruct.alloc);

  pool = BufferPool(buffer);
  return (*pool->class->bufferTrip)(pool, buffer, p, size);
}


/* BufferExpose/Cover -- buffer shield control
 *
 * See design.mps.buffer.method.expose.cover
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


/* See impl.h.mpmst for structure definitions */
Res BufferDescribe(Buffer buffer, mps_lib_FILE *stream)
{
  int res;

  AVERT(Buffer, buffer);
  AVER(stream != NULL);

  res = WriteF(stream,
         "Buffer $P ($U) {\n", (WriteFP)buffer, (WriteFU)buffer->serial,
	 "  Space $P\n",       (WriteFP)buffer->space,
         "  Pool $P\n",        (WriteFP)buffer->pool,
         "  Seg $P\n",         (WriteFP)buffer->seg,
         "  rank $U\n",        (WriteFU)buffer->rank,
         "  base $A  init $A  alloc $A  limit $A\n",
           buffer->base, buffer->apStruct.init,
	   buffer->apStruct.alloc, buffer->apStruct.limit,
         "  alignment $W\n",   (WriteFW)buffer->alignment,
	 "  exposed $U\n",     (WriteFU)buffer->exposed,
	 /* poolRing is uninteresting */
         "  grey $B\n",        (WriteFB)buffer->grey,
         "  shieldMode $B\n",  (WriteFB)buffer->shieldMode,
         "  p $P  i $U\n",     buffer->p, (WriteFU)buffer->i,
         "} Buffer $P ($U)\n", (WriteFP)buffer, (WriteFU)buffer->serial,
         NULL);

  return res;
}

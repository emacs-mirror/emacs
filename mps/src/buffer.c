/* impl.c.buffer: ALLOCATION BUFFER IMPLEMENTATION
 *
 * $HopeName: MMsrc!buffer.c(trunk.28) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
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

SRCID(buffer, "$HopeName: MMsrc!buffer.c(trunk.28) $");


/* BufferCheck -- check consistency of a buffer */

Bool BufferCheck(Buffer buffer)
{
  CHECKS(Buffer, buffer);
  CHECKL(buffer->serial < buffer->pool->bufferSerial); /* .trans.mod */
  CHECKU(Space, buffer->space);
  CHECKU(Pool, buffer->pool);
  CHECKL(buffer->space == buffer->pool->space);
  CHECKL(RingCheck(&buffer->poolRing));	/* design.mps.check.type.no-sig */
  CHECKL(RankSetCheck(buffer->rankSet));
  CHECKL(buffer->alignment == buffer->pool->alignment);
  CHECKL(AlignCheck(buffer->alignment));

  /* If any of the buffer's fields indicate that it is reset, make */
  /* sure it is really reset.  Otherwise, check various properties */
  /* of the non-reset fields. */
  if(buffer->seg == NULL ||
     buffer->base == (Addr)0 ||
     buffer->initAtFlip == (Addr)0 ||
     buffer->apStruct.init == (Addr)0 ||
     buffer->apStruct.alloc == (Addr)0 ||
     buffer->poolLimit == (Addr)0) {
    CHECKL(buffer->seg == NULL);
    CHECKL(buffer->base == (Addr)0);
    CHECKL(buffer->initAtFlip == (Addr)0);
    CHECKL(buffer->apStruct.init == (Addr)0);
    CHECKL(buffer->apStruct.alloc == (Addr)0);
    CHECKL(buffer->apStruct.limit == (Addr)0);
    CHECKL(buffer->poolLimit == (Addr)0);
  } else {
    /* The buffer is attached to a segment.  Make sure its fields */
    /* tally with those of the segment. */
    CHECKL(SegCheck(buffer->seg)); /* design.mps.check.type.no-sig */
    CHECKL(SegBuffer(buffer->seg) == buffer);
    CHECKL(SegPool(buffer->seg) == buffer->pool);
    CHECKL(buffer->rankSet == SegRankSet(buffer->seg));

    /* These fields should obey the ordering */
    /* base <= initAtFlip <= init <= alloc <= poolLimit */
    CHECKL(buffer->base <= buffer->initAtFlip);
    CHECKL(buffer->initAtFlip <= buffer->apStruct.init);
    CHECKL(buffer->apStruct.init <= buffer->apStruct.alloc);
    CHECKL(buffer->apStruct.alloc <= buffer->poolLimit);

    /* Check that the fields are aligned to the buffer alignment. */
    CHECKL(AddrIsAligned(buffer->base, buffer->alignment));
    CHECKL(AddrIsAligned(buffer->initAtFlip, buffer->alignment));
    CHECKL(AddrIsAligned(buffer->apStruct.init, buffer->alignment));
    CHECKL(AddrIsAligned(buffer->apStruct.alloc, buffer->alignment));
    CHECKL(AddrIsAligned(buffer->apStruct.limit, buffer->alignment));
    CHECKL(AddrIsAligned(buffer->poolLimit, buffer->alignment));

    /* If the buffer isn't trapped then "limit" should be the limit */
    /* set by the owning pool.  Otherwise, "init" is either at the */
    /* same place it was at flip (.commit.before) or has been set */
    /* to "alloc" (.commit.after). */
    if(buffer->apStruct.limit != (Addr)0)
      CHECKL(buffer->apStruct.limit == buffer->poolLimit);
    else {
      CHECKL(buffer->apStruct.init == buffer->initAtFlip ||
             buffer->apStruct.init == buffer->apStruct.alloc);
      /* Only buffers which allocate pointers get trapped. */
      CHECKL(buffer->rankSet != RankSetEMPTY);
    }
  }

  /* buffer->p, and buffer->i are arbitrary values determined by the */
  /* owning pool and cannot be checked */

  return TRUE;
}


/* BufferDescribe -- write out description of buffer
 *
 * See impl.h.mpmst for structure definitions.
 */

Res BufferDescribe(Buffer buffer, mps_lib_FILE *stream)
{
  int res;

  AVERT(Buffer, buffer);
  AVER(stream != NULL);

  res = WriteF(stream,
               "Buffer $P ($U) {\n",
               (WriteFP)buffer, (WriteFU)buffer->serial,
               "  Space $P\n",       (WriteFP)buffer->space,
               "  Pool $P\n",        (WriteFP)buffer->pool,
               "  Seg $P\n",         (WriteFP)buffer->seg,
               "  rankSet $U\n",     (WriteFU)buffer->rankSet,
               "  alignment $W\n",   (WriteFW)buffer->alignment,
               "  base $A\n",        buffer->base,
               "  initAtFlip $A\n",  buffer->initAtFlip,
               "  init $A\n",        buffer->apStruct.init,
               "  alloc $A\n",       buffer->apStruct.alloc,
               "  limit $A\n",       buffer->apStruct.limit,
               "  poolLimit $A\n",   buffer->poolLimit,
               "  p $P  i $U\n",     buffer->p, (WriteFU)buffer->i,
               "} Buffer $P ($U)\n",
               (WriteFP)buffer, (WriteFU)buffer->serial,
               NULL);

  return res;
}


/* BufferCreate -- create an allocation buffer
 *
 * See design.mps.buffer.method.create.
 */

Res BufferCreate(Buffer *bufferReturn, Pool pool, Rank rank)
{
  Res res;
  Buffer buffer;
  Space space;
  void *p;

  AVER(bufferReturn != NULL);
  AVERT(Pool, pool);
  AVER(RankCheck(rank));

  space = PoolSpace(pool);

  /* Allocate memory for the buffer descriptor structure. */
  res = SpaceAlloc(&p, space, sizeof(BufferStruct));
  if(res != ResOK) goto failAlloc;
  buffer = p;

  /* Initialize the buffer descriptor structure. */
  res = BufferInit(buffer, pool, rank);
  if(res != ResOK) goto failInit;

  *bufferReturn = buffer;
  return ResOK;

failInit:
  SpaceFree(space, (Addr)buffer, sizeof(BufferStruct));
failAlloc:
  return res;
}


/* BufferInit -- initialize an allocation buffer
 *
 * See design.mps.buffer.method.init.
 */

Res BufferInit(Buffer buffer, Pool pool, Rank rank)
{
  Res res;

  AVER(buffer != NULL);
  AVERT(Pool, pool);
  /* The PoolClass should support buffer protocols */
  AVER((pool->class->attr & AttrBUF)); /* .trans.mod */
  AVER(RankCheck(rank));
  
  /* Initialize the buffer.  See impl.h.mpmst for a definition of */
  /* the structure.  sig and serial comes later .init.sig-serial */
  buffer->space = PoolSpace(pool);
  buffer->pool = pool;
  RingInit(&buffer->poolRing);
  buffer->alignment = pool->alignment; /* .trans.mod */
  buffer->seg = NULL;
  buffer->rankSet = RankSetSingle(rank);
  buffer->base = (Addr)0;
  buffer->initAtFlip = (Addr)0;
  buffer->apStruct.init = (Addr)0;
  buffer->apStruct.alloc = (Addr)0;
  buffer->apStruct.limit = (Addr)0;
  buffer->poolLimit = (Addr)0;
  buffer->p = NULL;
  buffer->i = 0;

  /* Dispatch to the pool class method to perform any extra */
  /* initialization of the buffer. */
  res = (*pool->class->bufferInit)(pool, buffer);
  if(res != ResOK) return res;

  /* .init.sig-serial: Now that it's initialized, sign the buffer, */
  /* give it a serial number, and check it. */
  buffer->sig = BufferSig;
  buffer->serial = pool->bufferSerial; /* .trans.mod */
  ++pool->bufferSerial;
  AVERT(Buffer, buffer);

  /* Attach the initialized buffer to the pool. */
  RingAppend(&pool->bufferRing, &buffer->poolRing);

  return ResOK;
}


/* BufferDetach -- detach a buffer from a segment */

static void BufferDetach(Buffer buffer, Pool pool)
{
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));

  if(!BufferIsReset(buffer)) {
    /* Ask the owning pool to do whatever it needs to before the */
    /* buffer is detached (e.g. copy buffer state into pool state). */
    (*pool->class->bufferEmpty)(pool, buffer);

    /* Reset the buffer. */
    SegSetBuffer(buffer->seg, NULL);
    buffer->seg = NULL;
    buffer->base = (Addr)0;
    buffer->initAtFlip = (Addr)0;
    buffer->apStruct.init = (Addr)0;
    buffer->apStruct.alloc = (Addr)0;
    buffer->apStruct.limit = (Addr)0;
    buffer->poolLimit = (Addr)0;
  }
}


/* BufferDestroy -- destroy an allocation buffer
 *
 * design.mps.buffer.method.destroy
 */

void BufferDestroy(Buffer buffer)
{
  Space space;
  AVERT(Buffer, buffer);
  space = buffer->space;
  BufferFinish(buffer);
  SpaceFree(space, (Addr)buffer, sizeof(BufferStruct));
}


/* BufferFinish -- finish an allocation buffer */

void BufferFinish(Buffer buffer)
{
  Pool pool;

  AVERT(Buffer, buffer);

  pool = BufferPool(buffer);

  /* The PoolClass should support buffer protocols */
  AVER((pool->class->attr & AttrBUF)); /* .trans.mod */
  AVER(BufferIsReady(buffer));

  BufferDetach(buffer, pool);

  /* Ask the pool to do any pool-specific finishing. */
  (*pool->class->bufferFinish)(pool, buffer);

  /* Detach the buffer from its owning pool and unsig it. */
  RingRemove(&buffer->poolRing);
  buffer->sig = SigInvalid;
  
  /* Finish off the generic buffer fields. */
  RingFinish(&buffer->poolRing);
}


/* BufferIsReset -- test whether a buffer is in the "reset" state
 *
 * A buffer is "reset" when it is not attached to a segment.  In this
 * state all of the pointers into the segment are zero.  This condition
 * is checked by BufferCheck.
 */

Bool BufferIsReset(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->seg == NULL)
    return TRUE;

  return FALSE;
}


/* BufferIsReady -- test whether a buffer is ready for reserve
 *
 * BufferIsReady returns TRUE if and only if the buffer is not between
 * a reserve and commit.  The result is only reliable if the client is
 * not currently using the buffer, since it may update the alloc and
 * init pointers asynchronously.
 */

Bool BufferIsReady(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->apStruct.init == buffer->apStruct.alloc)
    return TRUE;

  return FALSE;
}


/* BufferReserve -- reserve memory from an allocation buffer
 *
 * .reserve: Keep in sync with impl.h.mps.reserve.
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
  if(next > buffer->apStruct.alloc && next <= buffer->apStruct.limit) {
    buffer->apStruct.alloc = next;
    *pReturn = buffer->apStruct.init;
    return ResOK;
  }

  /* If the buffer can't accommodate the request, call "fill". */
  return BufferFill(pReturn, buffer, size);
}


/* BufferFill -- refill an empty buffer
 *
 * BufferFill is entered by the "reserve" operation on a buffer if
 * there isn't enough room between "alloc" and "limit" to satisfy
 * an allocation request.  This might be because the buffer has been
 * trapped and "limit" has been set to zero.
 */

Res BufferFill(Addr *pReturn, Buffer buffer, Size size)
{
  Res res;
  Space space;
  Pool pool;
  Seg seg;
  Addr base, limit, next;

  AVER(pReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  AVER(BufferIsReady(buffer));

  pool = BufferPool(buffer);
  space = BufferSpace(buffer);

  /* If we're here because the buffer was trapped, then the mutator */
  /* must not have been between reserve and commit when flip */
  /* happened.  In that case, there's no need to invalidate any */
  /* object or fail to reserve (provided there's enough space in */
  /* the buffer).  Untrap the buffer, and try again. */
  if(!BufferIsReset(buffer) && buffer->apStruct.limit == (Addr)0) {
    buffer->apStruct.limit = buffer->poolLimit;
    next = AddrAdd(buffer->apStruct.alloc, size);
    if(next > buffer->apStruct.alloc &&
       next <= buffer->apStruct.limit) {
      buffer->apStruct.alloc = next;
      *pReturn = buffer->apStruct.init;
      return ResOK;
    }
  }

  /* There really isn't enough room for the allocation now. */
  AVER(AddrAdd(buffer->apStruct.alloc, size) > buffer->apStruct.limit ||
       AddrAdd(buffer->apStruct.alloc, size) < buffer->apStruct.alloc);

  BufferDetach(buffer, pool);

  /* Ask the pool for a segment and some memory. */
  res = (*pool->class->bufferFill)(&seg, &base, &limit,
                                   pool, buffer, size);
  if(res != ResOK) return res;

  AVER(SegCheck(seg));
  AVER(SegBuffer(seg) == NULL);
  AVER(SegBase(space, seg) <= base);
  AVER(AddrAdd(base, size) <= limit);
  AVER(limit <= SegLimit(space, seg));

  /* Set up the buffer to point at the memory given by the pool */
  /* and do the allocation that was requested by the client. */
  buffer->seg = seg;
  SegSetBuffer(seg, buffer);
  buffer->base = base;
  buffer->initAtFlip = base;
  buffer->apStruct.init = base;
  buffer->apStruct.alloc = AddrAdd(base, size);
  buffer->apStruct.limit = limit;
  buffer->poolLimit = limit;

  AVERT(Buffer, buffer);

  *pReturn = base;
  return res;
}


/* BufferCommit -- commit memory previously reserved
 *
 * .commit: Keep in sync with impl.h.mps.commit.
 */

Bool BufferCommit(Buffer buffer, Addr p, Size size)
{
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  AVER(!BufferIsReady(buffer));

  /* See design.mps.collection.flip. */
  /* .commit.before: If a flip occurs before this point, when the */
  /* pool reads "initAtFlip" it will point below the object, so it */
  /* will be trashed and the commit must fail when trip is called.  */
  AVER(p == buffer->apStruct.init);
  AVER(AddrAdd(buffer->apStruct.init, size) == buffer->apStruct.alloc);

  /* .commit.update: Atomically update the init pointer to declare */
  /* that the object is initialized (though it may be invalid if a */
  /* flip occurred). */
  buffer->apStruct.init = buffer->apStruct.alloc;

  /* .improve.memory-barrier: Memory barrier here on the DEC Alpha */
  /* (and other relaxed memory order architectures). */
  /* .commit.after: If a flip occurs at this point, the pool will */
  /* see "initAtFlip" above the object, which is valid, so it will */
  /* be collected.  The commit must succeed when trip is called.  */
  /* The pointer "p" will have been fixed up.  (@@@@ Will it?) */
  /* .commit.trip: Trip the buffer if a flip has occurred. */
  if(buffer->apStruct.limit == 0)
    return BufferTrip(buffer, p, size);

  /* No flip occurred, so succeed. */

  return TRUE;
}


/* BufferTrip -- act on a trapped buffer */

Bool BufferTrip(Buffer buffer, Addr p, Size size)
{
  Pool pool;

  AVERT(Buffer, buffer);
  AVER(p != 0);
  AVER(size > 0);
  AVER(SizeIsAligned(size, buffer->alignment));

  /* The limit field should be zero, because that's how trip gets */
  /* called.  See .commit.trip. */
  AVER(buffer->apStruct.limit == 0);

  /* The init and alloc fields should be equal at this point, because */
  /* the step .commit.update has happened. */
  AVER(buffer->apStruct.init == buffer->apStruct.alloc);

  /* The p parameter points at the base address of the allocated */
  /* block, the end of which should now coincide with the init and */
  /* alloc fields. */
  AVER(AddrAdd(p, size) == buffer->apStruct.init);

  pool = BufferPool(buffer);

  AVER(PoolHasAddr(pool, p));
  AVER(SegPool(BufferSeg(buffer)) == pool);

  /* .trip.untrap: If the flip occurred before commit set "init" */
  /* to "alloc" (see .commit.before) then the object is invalid */
  /* (won't've been scanned) so undo the allocation and fail commit. */
  /* Otherwise (see .commit.after) the object is valid (will've been */
  /* scanned) so commit can simply succeed. */
  if(buffer->apStruct.init == buffer->initAtFlip)
    return TRUE;
  else {
    buffer->apStruct.init = p;
    buffer->apStruct.alloc = p;
    buffer->apStruct.limit = buffer->poolLimit;
    return FALSE;
  }
}


/* BufferFlip -- trap buffer at GC flip time
 *
 * .flip: Tells the buffer that a flip has occurred.  If the buffer is
 * between reserve and commit, and has a rank (i.e. references),
 * and has the two-phase protocol, then the object being initialized
 * is invalidated by failing the next commit.  The buffer code handles
 * this automatically.  If the buffer is reset there is no effect,
 * since there is no object to invalidate.  If the buffer is already
 * flipped (i.e. "limit" is zero) there is no effect, since the
 * object is already invalid by a previous trace.  The buffer becomes
 * unflipped at the next reserve or commit operation.  This is handled
 * by BufferFill (.fill.untrap) or BufferTrip (.trip.untrap).
 */

void BufferFlip(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->rankSet != RankSetEMPTY &&
     buffer->apStruct.limit != (Addr)0) {
    /* removing this aver because it isn't true.  after a flip */
    /* when a buffer is unflipped by updating limit, initAtFlip */
    /* isn't updated */
    /* see change.dylan.sunflower.3.170429 */
#if 0
    AVER(buffer->initAtFlip == buffer->base);
#endif
    buffer->initAtFlip = buffer->apStruct.init;
    buffer->apStruct.limit = (Addr)0;
  }
}


/* BufferScanLimit -- return limit of data to which to scan
 *
 * Returns the highest address to which it is safe to scan objects
 * in the buffer.  When the buffer is not flipped, this is the
 * "init" of the AP.  When the buffer is flipped, it is the value
 * that "init" had at flip time.  [Could make BufferScanLimit
 * return the AP "alloc" when using ambiguous scanning.]
 */

Addr BufferScanLimit(Buffer buffer)
{
  if(buffer->apStruct.limit != (Addr)0)
    return buffer->apStruct.init;
  else
    return buffer->initAtFlip;
}


AP (BufferAP)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return BufferAP(buffer);
}

/* design.mps.buffer.method.ofap */
/* This method must be thread-safe. See */
/* design.mps.interface.c.thread-safety. */
Buffer (BufferOfAP)(AP ap)
{
  /* Can't AVER ap as that would not be thread safe */
  /* No Check method for AP, so no AVER */
  /* .design.mps.misc.parent.thread-safe */
  return BufferOfAP(ap);
}

/* design.mps.buffer.method.space */
/* This method must be thread-safe.  See */
/* design.mps.interface.c.thread-safety. */
Space (BufferSpace)(Buffer buffer)
{
  /* Can't AVER buffer as that wouldn not be thread safe */
  /* AVERT(Buffer, buffer); */
  return BufferSpace(buffer);
}

Pool (BufferPool)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return BufferPool(buffer);
}

Seg (BufferSeg)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return BufferSeg(buffer);
}

RankSet (BufferRankSet)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return BufferRankSet(buffer);
}

Addr (BufferBase)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return BufferBase(buffer);
}

Addr (BufferGetInit)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return BufferGetInit(buffer);
}

Addr (BufferAlloc)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return BufferAlloc(buffer);
}

Addr (BufferLimit)(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return BufferLimit(buffer);
}

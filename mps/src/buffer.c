/* impl.c.buffer: ALLOCATION BUFFER IMPLEMENTATION
 *
 * $HopeName: MMsrc!buffer.c(trunk.45) $
 * Copyright (C) 1997, 1998 Harlequin Group plc.  All rights reserved.
 *
 * This is (part of) the implementation of allocation buffers.
 *
 * Several macros which also form part of the implementation are
 * in impl.h.mps.
 *
 * Several macros forming part of impl.h.mps should be consistent
 * with the macros and functions in this module.
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

SRCID(buffer, "$HopeName: MMsrc!buffer.c(trunk.45) $");


/* BufferCheck -- check consistency of a buffer */
/* .check.use-trapped: This checking function uses BufferIsTrapped, */
/* So BufferIsTrapped can't do checking as that would cause an */
/* inifinite loop. */
Bool BufferCheck(Buffer buffer)
{
  CHECKS(Buffer, buffer);
  CHECKL(buffer->serial < buffer->pool->bufferSerial); /* .trans.mod */
  CHECKU(Arena, buffer->arena);
  CHECKU(Pool, buffer->pool);
  CHECKL(buffer->arena == buffer->pool->arena);
  CHECKL(RingCheck(&buffer->poolRing)); /* design.mps.check.type.no-sig */
  CHECKL(BoolCheck(buffer->isMutator));
  CHECKL(buffer->fillSize >= 0.0);
  CHECKL(buffer->emptySize >= 0.0);
  CHECKL(buffer->emptySize <= buffer->fillSize);
  CHECKL(RankSetCheck(buffer->rankSet));
  CHECKL(buffer->alignment == buffer->pool->alignment);
  CHECKL(AlignCheck(buffer->alignment));

  /* If any of the buffer's fields indicate that it is reset, make */
  /* sure it is really reset.  Otherwise, check various properties */
  /* of the non-reset fields. */
  if((buffer->mode & BufferModeATTACHED) == 0 ||
     buffer->seg == NULL ||
     buffer->base == (Addr)0 ||
     buffer->apStruct.init == (Addr)0 ||
     buffer->apStruct.alloc == (Addr)0 ||
     buffer->poolLimit == (Addr)0) {
    CHECKL((buffer->mode & BufferModeATTACHED) == 0);
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
    CHECKL(buffer->mode & BufferModeATTACHED);
    CHECKL(SegCheck(buffer->seg)); /* design.mps.check.type.no-sig */
    CHECKL(SegBuffer(buffer->seg) == buffer);
    CHECKL(SegPool(buffer->seg) == buffer->pool);
    CHECKL(buffer->rankSet == SegRankSet(buffer->seg));

    /* These fields should obey the ordering */
    /* base <= init <= alloc <= poolLimit */
    CHECKL(buffer->base <= buffer->apStruct.init);
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
    /* to "alloc" (.commit.after).  Also, when the buffer is */
    /* flipped, initAtFlip should hold the init at flip, which is */
    /* between the base and current init.  Otherwise, initAtFlip */
    /* is kept at zero to avoid misuse (see */
    /* request.dylan.170429.sol.zero). */
    if(!BufferIsTrapped(buffer)) { /* .check.use-trapped */
      CHECKL(buffer->apStruct.limit == buffer->poolLimit);
      CHECKL(buffer->initAtFlip == (Addr)0);
    } else {
      CHECKL(buffer->apStruct.limit == (Addr)0);
      if(buffer->mode & BufferModeFLIPPED) {
        CHECKL(buffer->apStruct.init == buffer->initAtFlip ||
               buffer->apStruct.init == buffer->apStruct.alloc);
        CHECKL(buffer->base <= buffer->initAtFlip);
        CHECKL(buffer->initAtFlip <= buffer->apStruct.init);
        /* Only buffers that allocate pointers get flipped. */
        CHECKL(buffer->rankSet != RankSetEMPTY);
      }
      if(buffer->mode & BufferModeLOGGED) {
        /* Nothing special to check in the logged mode */
	NOOP;
      }
    }
  }

  /* buffer->p, and buffer->i are arbitrary values determined by the */
  /* owning pool and cannot be checked */
  /* .improve.check.class: Add a Pool Class methd so that the pool */
  /* class can check these fields in the buffer. */

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
               "  Arena $P\n",       (WriteFP)buffer->arena,
               "  Pool $P\n",        (WriteFP)buffer->pool,
               buffer->isMutator ?
                 "  Mutator Buffer\n" : "  Internal Buffer\n",
               "  Mode $B\n",        (WriteFB)(buffer->mode),
               "  fillSize $UKb\n",  (WriteFU)(buffer->fillSize / 1024),
               "  emptySize $UKb\n", (WriteFU)(buffer->emptySize / 1024),
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


/* BufferInitV -- initialize an allocation buffer */

static Res BufferInitV(Buffer buffer, Pool pool, Bool isMutator, va_list args)
{
  Arena arena;
  Res res;

  AVER(buffer != NULL);
  AVERT(Pool, pool);
  /* The PoolClass should support buffer protocols */
  AVER((pool->class->attr & AttrBUF)); /* .trans.mod */
  
  arena = PoolArena(pool);
  /* Initialize the buffer.  See impl.h.mpmst for a definition of */
  /* the structure.  sig and serial comes later .init.sig-serial */
  buffer->arena = arena;
  buffer->pool = pool;
  RingInit(&buffer->poolRing);
  buffer->isMutator = isMutator;
  if(arena->bufferLogging) {
    buffer->mode = BufferModeLOGGED;
  } else {
    buffer->mode = 0;
  }
  buffer->fillSize = 0.0;
  buffer->emptySize = 0.0;
  buffer->alignment = pool->alignment; /* .trans.mod */
  buffer->seg = NULL;
  buffer->rankSet = RankSetEMPTY;
  buffer->base = (Addr)0;
  buffer->initAtFlip = (Addr)0;
  buffer->apStruct.init = (Addr)0;
  buffer->apStruct.alloc = (Addr)0;
  buffer->apStruct.limit = (Addr)0;
  buffer->poolLimit = (Addr)0;
  buffer->rampCount = 0;
  buffer->p = NULL;
  buffer->i = 0;

  /* Dispatch to the pool class method to perform any extra */
  /* initialization of the buffer. */
  res = (*pool->class->bufferInit)(pool, buffer, args);
  if(res != ResOK)
    return res;

  /* .init.sig-serial: Now that it's initialized, sign the buffer, */
  /* give it a serial number, and check it. */
  buffer->sig = BufferSig;
  buffer->serial = pool->bufferSerial; /* .trans.mod */
  ++pool->bufferSerial;
  AVERT(Buffer, buffer);

  /* Attach the initialized buffer to the pool. */
  RingAppend(&pool->bufferRing, &buffer->poolRing);

  EVENT_PPU(BufferInit, buffer, pool, (unsigned)isMutator);

  return ResOK;
}


/* BufferCreate -- create an allocation buffer
 *
 * See design.mps.buffer.method.create.
 */

Res BufferCreate(Buffer *bufferReturn, Pool pool, ...)
{
  Res res;
  va_list args;

  va_start(args, pool);
  res = BufferCreateV(bufferReturn, pool, FALSE, args);
  va_end(args);
  return res;
}


/* BufferCreateV -- create an allocation buffer, with varargs
 *
 * See design.mps.buffer.method.create.
 */

Res BufferCreateV(Buffer *bufferReturn,
                  Pool pool, Bool isMutator, va_list args)
{
  Res res;
  Buffer buffer;
  Arena arena;
  void *p;

  AVER(bufferReturn != NULL);
  AVERT(Pool, pool);

  arena = PoolArena(pool);

  /* Allocate memory for the buffer descriptor structure. */
  res = ArenaAlloc(&p, arena, sizeof(BufferStruct));
  if(res != ResOK)
    goto failAlloc;
  buffer = p;

  /* Initialize the buffer descriptor structure. */
  res = BufferInitV(buffer, pool, isMutator, args);
  if(res != ResOK)
    goto failInit;

  *bufferReturn = buffer;
  return ResOK;

failInit:
  ArenaFree(arena, buffer, sizeof(BufferStruct));
failAlloc:
  return res;
}


/* BufferDetach -- detach a buffer from a segment */

void BufferDetach(Buffer buffer, Pool pool)
{
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));

  if(!BufferIsReset(buffer)) {
    Size spare;
    /* Ask the owning pool to do whatever it needs to before the */
    /* buffer is detached (e.g. copy buffer state into pool state). */
    (*pool->class->bufferEmpty)(pool, buffer);

    spare = AddrOffset(buffer->apStruct.alloc, 
                       buffer->poolLimit);
    buffer->emptySize += spare;
    if(buffer->isMutator) {
      buffer->pool->emptyMutatorSize += spare;
      buffer->arena->emptyMutatorSize += spare;
      buffer->arena->allocMutatorSize += AddrOffset(buffer->base,
                                                    buffer->apStruct.alloc);
    } else {
      buffer->pool->emptyInternalSize += spare;
      buffer->arena->emptyInternalSize += spare;
    }

    /* Reset the buffer. */
    SegSetBuffer(buffer->seg, NULL);
    buffer->seg = NULL;
    buffer->base = (Addr)0;
    buffer->initAtFlip = (Addr)0;
    buffer->apStruct.init = (Addr)0;
    buffer->apStruct.alloc = (Addr)0;
    buffer->apStruct.limit = (Addr)0;
    buffer->poolLimit = (Addr)0;
    buffer->mode &= ~(BufferModeATTACHED|BufferModeFLIPPED);
  }
}


/* BufferDestroy -- destroy an allocation buffer
 *
 * design.mps.buffer.method.destroy
 */

void BufferDestroy(Buffer buffer)
{
  Arena arena;

  AVERT(Buffer, buffer);
  arena = buffer->arena;
  BufferFinish(buffer);
  ArenaFree(arena, buffer, sizeof(BufferStruct));
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

  EVENT_P(BufferFinish, buffer);
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

/* BufferIsMutator
 *
 * returns TRUE iff mutator was created at mutator request (ie a
 * mutator buffer).
 */
Bool BufferIsMutator(Buffer buffer)
{
  AVERT(Buffer, buffer);

  return buffer->isMutator;
}


/* BufferReserve -- reserve memory from an allocation buffer
 *
 * .reserve: Keep in sync with impl.h.mps.reserve.
 */

Res BufferReserve(Addr *pReturn, Buffer buffer, Size size,
                  Bool withReservoirPermit)
{
  Addr next;

  AVER(pReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  AVER(BufferIsReady(buffer));
  AVER(BoolCheck(withReservoirPermit));

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
  return BufferFill(pReturn, buffer, size, withReservoirPermit);
}


/* BufferFill -- refill an empty buffer
 *
 * BufferFill is entered by the "reserve" operation on a buffer if
 * there isn't enough room between "alloc" and "limit" to satisfy
 * an allocation request.  This might be because the buffer has been
 * trapped and "limit" has been set to zero.
 */

Res BufferFill(Addr *pReturn, Buffer buffer, Size size,
               Bool withReservoirPermit)
{
  Res res;
  Pool pool;
  Seg seg;
  Addr base, limit, next;
  Size filled;

  AVER(pReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  AVER(BufferIsReady(buffer));

  pool = BufferPool(buffer);

  /* If we're here because the buffer was trapped, then we attempt */
  /* the allocation here. */
  if(!BufferIsReset(buffer) && buffer->apStruct.limit == (Addr)0) {
    /* .fill.unflip: If the buffer is flipped then we unflip the buffer. */
    if(buffer->mode & BufferModeFLIPPED) {
      buffer->mode &= ~BufferModeFLIPPED;
      /* restore apStruct.limit if appropriate */
      if(!BufferIsTrapped(buffer)) {
        buffer->apStruct.limit = buffer->poolLimit;
      }
      buffer->initAtFlip = (Addr)0;
    }
    /* .fill.logged: If the buffer is logged then we leave it logged. */

    next = AddrAdd(buffer->apStruct.alloc, size);
    if(next > buffer->apStruct.alloc &&
       next <= buffer->poolLimit) {
      buffer->apStruct.alloc = next;
      if(buffer->mode & BufferModeLOGGED) {
        EVENT_PAW(BufferReserve, buffer, buffer->apStruct.init, size);
      }
      *pReturn = buffer->apStruct.init;
      return ResOK;
    }
  }

  /* There really isn't enough room for the allocation now. */
  AVER(AddrAdd(buffer->apStruct.alloc, size) > buffer->poolLimit ||
       AddrAdd(buffer->apStruct.alloc, size) < buffer->apStruct.alloc);

  BufferDetach(buffer, pool);

  /* Ask the pool for a segment and some memory. */
  res = (*pool->class->bufferFill)(&seg, &base, &limit,
                                   pool, buffer, size,
                                   withReservoirPermit);
  if(res != ResOK)
    return res;

  AVER(SegCheck(seg));
  AVER(SegBuffer(seg) == NULL);
  AVER(SegBase(seg) <= base);
  AVER(AddrAdd(base, size) <= limit);
  AVER(limit <= SegLimit(seg));

  /* Set up the buffer to point at the memory given by the pool */
  /* and do the allocation that was requested by the client. */
  buffer->mode |= BufferModeATTACHED;
  buffer->seg = seg;
  SegSetBuffer(seg, buffer);
  buffer->base = base;
  buffer->apStruct.init = base;
  buffer->apStruct.alloc = AddrAdd(base, size);
  /* only set limit if not logged */
  if((buffer->mode & BufferModeLOGGED) == 0) {
    buffer->apStruct.limit = limit;
  } else {
    AVER(buffer->apStruct.limit == (Addr)0);
  }
  AVER(buffer->initAtFlip == (Addr)0);
  buffer->poolLimit = limit;

  filled = AddrOffset(base, limit);
  buffer->fillSize += filled;
  if(buffer->isMutator) {
    buffer->pool->fillMutatorSize += filled;
    buffer->arena->fillMutatorSize += filled;
  } else {
    buffer->pool->fillInternalSize += filled;
    buffer->arena->fillInternalSize += filled;
  }

  AVERT(Buffer, buffer);

  if(buffer->mode & BufferModeLOGGED) {
    EVENT_PAW(BufferReserve, buffer, buffer->apStruct.init, size);
  }

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


/* BufferTrip -- act on a trapped buffer
 *
 * Called from BufferCommit (and its equivalents) when invoked on a
 * trapped buffer (indicated by limit == 0).  This function can
 * decide whether to succeed or fail the commit. */
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
  /* Of course we should be trapped. */
  AVER(BufferIsTrapped(buffer));

  /* The init and alloc fields should be equal at this point, because */
  /* the step .commit.update has happened. */
  AVER(buffer->apStruct.init == buffer->apStruct.alloc);

  /* The p parameter points at the base address of the allocated */
  /* block, the end of which should now coincide with the init and */
  /* alloc fields. */
  /* Note that we don't _really_ care about p too much.  We don't */
  /* do anything else with it apart from these checks. (in particular */
  /* it seems like the algorithms could be modified to cope with the */
  /* case of the object having been copied between Commit updating i */
  /* and testing limit) */
  AVER(AddrAdd(p, size) == buffer->apStruct.init);

  pool = BufferPool(buffer);

  AVER(PoolHasAddr(pool, p));
  AVER(SegPool(BufferSeg(buffer)) == pool);

  /* .trip.unflip: If the flip occurred before commit set "init" */
  /* to "alloc" (see .commit.before) then the object is invalid */
  /* (won't've been scanned) so undo the allocation and fail commit. */
  /* Otherwise (see .commit.after) the object is valid (will've been */
  /* scanned) so commit can simply succeed. */
  if((buffer->mode & BufferModeFLIPPED) &&
     buffer->apStruct.init != buffer->initAtFlip) {
    /* Reset just enough state for Reserve/Fill to work. */
    /* The buffer is left trapped and we leave the untrapping */
    /* for the next reserve (which goes out of line to Fill */
    /* (.fill.unflip) because the buffer is still trapped) */
    buffer->apStruct.init = p;
    buffer->apStruct.alloc = p;
    return FALSE;
  }

  /* Emit event including class if loggged */
  if(buffer->mode & BufferModeLOGGED) {
    Bool b;
    Format format;
    Addr clientClass;

    b = PoolFormat(&format, buffer->pool);
    if(b) {
      clientClass = format->class(p);
    } else if(sizeof(Addr) <= size) {
      /* hack to get the class of an object for unformatted pools. */
      /* .trip.assume.align: Assume p is Addr * aligned. */
      clientClass = *(Addr *)p;
    } else {
      clientClass = (Addr)0;
    }
    EVENT_PAWA(BufferCommit, buffer, p, size, clientClass);
    /* Of course, it's not _really_ unused unless you're not */
    /* using telemetry.  This is a HACK @@@@.  It should be */
    /* removed when telemetry is fixed to use its arguments. */
    UNUSED(clientClass);
  }
  return TRUE;
}


/* BufferFlip -- trap buffer at GC flip time
 *
 * .flip: Tells the buffer that a flip has occurred.  If the buffer is
 * between reserve and commit, and has a rank (i.e. references), and
 * has the two-phase protocol, then the object being initialized is
 * invalidated by failing the next commit.  The buffer code handles
 * this automatically (ie the pool implementation is not involved).  If
 * the buffer is reset there is no effect, since there is no object to
 * invalidate.  If the buffer is already flipped there is no effect,
 * since the object is already invalid by a previous trace.  The buffer
 * becomes unflipped at the next reserve or commit operation (actually
 * reserve because commit is lazy).  This is handled by BufferFill
 * (.fill.unflip) or BufferTrip (.trip.unflip).
 */

void BufferFlip(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if(buffer->rankSet != RankSetEMPTY &&
     (buffer->mode & BufferModeFLIPPED) == 0 &&
     !BufferIsReset(buffer)) {
    AVER(buffer->initAtFlip == (Addr)0);
    buffer->initAtFlip = buffer->apStruct.init;
    /* Memory Barrier here? @@@@ */
    buffer->apStruct.limit = (Addr)0;
    buffer->mode |= BufferModeFLIPPED;
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
  if(buffer->mode & BufferModeFLIPPED) {
    return buffer->initAtFlip;
  } else {
    return buffer->apStruct.init;
  }
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
Arena (BufferArena)(Buffer buffer)
{
  /* Can't AVER buffer as that would not be thread-safe. */
  /* AVERT(Buffer, buffer); */
  return BufferArena(buffer);
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

Bool BufferIsTrapped(Buffer buffer)
{
  /* Can't check buffer, see .check.use-trapped */
  return (buffer->mode & (BufferModeFLIPPED|BufferModeLOGGED)) != 0;
}


/* Alloc pattern functions
 *
 * Just represent the two patterns by two different pointers to dummies.
 */

AllocPatternStruct AllocPatternRampStruct = {'\0'};

AllocPattern AllocPatternRamp(void)
{
  return &AllocPatternRampStruct;
}

AllocPatternStruct AllocPatternRampCollectAllStruct = {'\0'};

AllocPattern AllocPatternRampCollectAll(void)
{
  return &AllocPatternRampCollectAllStruct;
}

static Bool AllocPatternCheck(AllocPattern pattern)
{
  CHECKL(pattern == &AllocPatternRampCollectAllStruct
         || pattern == &AllocPatternRampStruct);
  UNUSED(pattern); /* impl.c.mpm.check.unused */
  return TRUE;
}


/* BufferRampBegin -- note an entry into a ramp pattern
 *
 * .ramp.hack: We count the number of times the ap has begun ramp mode
 * (and not ended), so we can do reset by ending all the current ramps.
 */

void BufferRampBegin(Buffer buffer, AllocPattern pattern)
{
  Pool pool;

  AVERT(Buffer, buffer);
  AVERT(AllocPattern, pattern);

  AVER(buffer->rampCount < UINT_MAX);
  ++buffer->rampCount;

  pool = BufferPool(buffer);
  AVERT(Pool, pool);
  (*pool->class->rampBegin)(pool, buffer,
                            pattern == &AllocPatternRampCollectAllStruct);
}


/* BufferRampEnd -- note an exit from a ramp pattern */

Res BufferRampEnd(Buffer buffer)
{
  Pool pool;

  AVERT(Buffer, buffer);

  if(buffer->rampCount == 0)
    return ResFAIL;
  --buffer->rampCount;

  pool = BufferPool(buffer);
  AVERT(Pool, pool);
  (*pool->class->rampEnd)(pool, buffer);
  return ResOK;
}


/* BufferRampReset -- exit from ramp mode */

void BufferRampReset(Buffer buffer)
{
  Pool pool;

  AVERT(Buffer, buffer);

  if(buffer->rampCount == 0)
    return;

  pool = BufferPool(buffer);
  AVERT(Pool, pool);
  do
    (*pool->class->rampEnd)(pool, buffer);
  while(--buffer->rampCount > 0);
}

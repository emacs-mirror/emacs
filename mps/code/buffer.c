/* impl.c.buffer: ALLOCATION BUFFER IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This is (part of) the implementation of allocation buffers.
 * Several macros which also form part of the implementation are in
 * impl.h.mps.  Several macros forming part of impl.h.mps should be
 * consistent with the macros and functions in this module.
 *
 * DESIGN
 *
 * .design: See design.mps.buffer.
 *
 * .ap.async: The mutator is allowed to change certain AP fields
 * asynchronously.  Functions that can be called on buffers not
 * synchronized with the mutator must take care when reading these
 * fields.  Such functions are marked with this tag.
 *
 * TRANSGRESSIONS
 *
 * .trans.mod: There are several instances where pool structures are
 * directly accessed by this module because impl.c.pool does not provide
 * an adequate (or adequately documented) interface.  They bear this
 * tag.  */

#include "mpm.h"

SRCID(buffer, "$Id$");


/* forward declarations */
static void BufferFrameNotifyPopPending(Buffer buffer);


/* BufferCheck -- check consistency of a buffer
 *
 * See .ap.async.  */

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
  CHECKL(buffer->alignment == buffer->pool->alignment);
  CHECKL(AlignCheck(buffer->alignment));
  CHECKL(BoolCheck(buffer->apStruct.enabled));

  if (buffer->apStruct.enabled) {
    /* no useful check for frameptr - mutator may be updating it */
    CHECKL(BoolCheck(buffer->apStruct.lwPopPending));
  } else {
    CHECKL(buffer->apStruct.lwPopPending == FALSE);
    CHECKL(buffer->apStruct.frameptr == NULL);
  }

  /* If any of the buffer's fields indicate that it is reset, make */
  /* sure it is really reset.  Otherwise, check various properties */
  /* of the non-reset fields. */
  if (buffer->mode & BufferModeTRANSITION) {
    /* nothing to check */
  } else if ((buffer->mode & BufferModeATTACHED) == 0
             || buffer->base == (Addr)0
             || buffer->apStruct.init == (Addr)0
             || buffer->apStruct.alloc == (Addr)0
             || buffer->poolLimit == (Addr)0) {
    CHECKL((buffer->mode & BufferModeATTACHED) == 0);
    CHECKL(buffer->base == (Addr)0);
    CHECKL(buffer->initAtFlip == (Addr)0);
    CHECKL(buffer->apStruct.init == (Addr)0);
    CHECKL(buffer->apStruct.alloc == (Addr)0);
    CHECKL(buffer->apStruct.limit == (Addr)0);
    /* Nothing reliable to check for lightweight frame state */
    CHECKL(buffer->poolLimit == (Addr)0);
  } else {
    Addr aplimit;

    /* The buffer is attached to a region of memory.   */
    /* Check consistency. */
    CHECKL(buffer->mode & BufferModeATTACHED);

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

    /* .lwcheck: If LW frames are enabled, the buffer may become */
    /* trapped asynchronously. It can't become untrapped */
    /* asynchronously, though. See design.mps.alloc-frame.lw-frame.pop. */
    /* Read a snapshot value of the limit field. Use this to determine */
    /* if we are trapped, and to permit more useful checking when not */
    /* yet trapped. */
    aplimit = buffer->apStruct.limit;

    /* If the buffer isn't trapped then "limit" should be the limit */
    /* set by the owning pool.  Otherwise, "init" is either at the */
    /* same place it was at flip (.commit.before) or has been set */
    /* to "alloc" (.commit.after).  Also, when the buffer is */
    /* flipped, initAtFlip should hold the init at flip, which is */
    /* between the base and current init.  Otherwise, initAtFlip */
    /* is kept at zero to avoid misuse (see */
    /* request.dylan.170429.sol.zero). */

    if ((buffer->apStruct.enabled && aplimit == (Addr)0) /* see .lwcheck */
        || (!buffer->apStruct.enabled && BufferIsTrapped(buffer))) {
      /* .check.use-trapped: This checking function uses BufferIsTrapped, */
      /* So BufferIsTrapped can't do checking as that would cause an */
      /* infinite loop. */
      CHECKL(aplimit == (Addr)0);
      if (buffer->mode & BufferModeFLIPPED) {
        CHECKL(buffer->apStruct.init == buffer->initAtFlip
               || buffer->apStruct.init == buffer->apStruct.alloc);
        CHECKL(buffer->base <= buffer->initAtFlip);
        CHECKL(buffer->initAtFlip <= buffer->apStruct.init);
      }
      /* Nothing special to check in the logged mode. */
    } else {
      CHECKL(aplimit == buffer->poolLimit); /* see .lwcheck */
      CHECKL(buffer->initAtFlip == (Addr)0);
    }
  }

  return TRUE;
}


/* BufferDescribe -- write out description of buffer
 *
 * See impl.h.mpmst for structure definitions.  */

Res BufferDescribe(Buffer buffer, mps_lib_FILE *stream)
{
  Res res;

  if (!CHECKT(Buffer, buffer)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "Buffer $P ($U) {\n",
               (WriteFP)buffer, (WriteFU)buffer->serial,
               "  class $P (\"$S\")\n",
               (WriteFP)buffer->class, buffer->class->name,
               "  Arena $P\n",       (WriteFP)buffer->arena,
               "  Pool $P\n",        (WriteFP)buffer->pool,
               buffer->isMutator ?
                 "  Mutator Buffer\n" : "  Internal Buffer\n",
               "  Mode $B\n",        (WriteFB)(buffer->mode),
               "  fillSize $UKb\n",  (WriteFU)(buffer->fillSize / 1024),
               "  emptySize $UKb\n", (WriteFU)(buffer->emptySize / 1024),
               "  alignment $W\n",   (WriteFW)buffer->alignment,
               "  base $A\n",        buffer->base,
               "  initAtFlip $A\n",  buffer->initAtFlip,
               "  init $A\n",        buffer->apStruct.init,
               "  alloc $A\n",       buffer->apStruct.alloc,
               "  limit $A\n",       buffer->apStruct.limit,
               "  poolLimit $A\n",   buffer->poolLimit,
               NULL);
  if (res != ResOK) return res;

  res = buffer->class->describe(buffer, stream);
  if (res != ResOK) return res;

  res = WriteF(stream, "} Buffer $P ($U)\n",
               (WriteFP)buffer, (WriteFU)buffer->serial,
               NULL);
  return res;
}


/* BufferInitV -- initialize an allocation buffer */

static Res BufferInitV(Buffer buffer, BufferClass class,
                       Pool pool, Bool isMutator, va_list args)
{
  Arena arena;
  Res res;

  AVER(buffer != NULL);
  AVERT(BufferClass, class);
  AVERT(Pool, pool);
  /* The PoolClass should support buffer protocols */
  AVER((pool->class->attr & AttrBUF)); /* .trans.mod */
 
  arena = PoolArena(pool);
  /* Initialize the buffer.  See impl.h.mpmst for a definition of */
  /* the structure.  sig and serial comes later .init.sig-serial */
  buffer->arena = arena;
  buffer->class = class;
  buffer->pool = pool;
  RingInit(&buffer->poolRing);
  buffer->isMutator = isMutator;
  if (ArenaGlobals(arena)->bufferLogging) {
    buffer->mode = BufferModeLOGGED;
  } else {
    buffer->mode = 0;
  }
  buffer->fillSize = 0.0;
  buffer->emptySize = 0.0;
  buffer->alignment = pool->alignment; /* .trans.mod */
  buffer->base = (Addr)0;
  buffer->initAtFlip = (Addr)0;
  buffer->apStruct.init = (Addr)0;
  buffer->apStruct.alloc = (Addr)0;
  buffer->apStruct.limit = (Addr)0;
  buffer->apStruct.frameptr = NULL;
  buffer->apStruct.enabled = FALSE;
  buffer->apStruct.lwPopPending = FALSE;
  buffer->poolLimit = (Addr)0;
  buffer->rampCount = 0;

  /* .init.sig-serial: Now the vanilla stuff is initialized, */
  /* sign the buffer and give it a serial number. It can */
  /* then be safely checked in subclass methods. */
  buffer->sig = BufferSig;
  buffer->serial = pool->bufferSerial; /* .trans.mod */
  ++pool->bufferSerial;
  AVERT(Buffer, buffer);

  /* Dispatch to the buffer class method to perform any  */
  /* class-specific initialization of the buffer. */
  res = (*class->init)(buffer, pool, args);
  if (res != ResOK)
    goto failInit;

  /* Attach the initialized buffer to the pool. */
  RingAppend(&pool->bufferRing, &buffer->poolRing);

  return ResOK;

failInit:
  RingFinish(&buffer->poolRing);
  buffer->sig = SigInvalid;
  return res;
}


/* BufferCreate -- create an allocation buffer
 *
 * See design.mps.buffer.method.create.  */

Res BufferCreate(Buffer *bufferReturn, BufferClass class,
                 Pool pool, Bool isMutator, ...)
{
  Res res;
  va_list args;

  va_start(args, isMutator);
  res = BufferCreateV(bufferReturn, class, pool, isMutator, args);
  va_end(args);
  return res;
}


/* BufferCreateV -- create an allocation buffer, with varargs
 *
 * See design.mps.buffer.method.create.  */

Res BufferCreateV(Buffer *bufferReturn, BufferClass class,
                  Pool pool, Bool isMutator, va_list args)
{
  Res res;
  Buffer buffer;
  Arena arena;
  void *p;

  AVER(bufferReturn != NULL);
  AVERT(BufferClass, class);
  AVERT(Pool, pool);

  arena = PoolArena(pool);

  /* Allocate memory for the buffer descriptor structure. */
  res = ControlAlloc(&p, arena, class->size,
                     /* withReservoirPermit */ FALSE);
  if (res != ResOK)
    goto failAlloc;
  buffer = p;

  /* Initialize the buffer descriptor structure. */
  res = BufferInitV(buffer, class, pool, isMutator, args);
  if (res != ResOK)
    goto failInit;

  *bufferReturn = buffer;
  return ResOK;

failInit:
  ControlFree(arena, buffer, class->size);
failAlloc:
  return res;
}


/* BufferDetach -- detach a buffer from a region  */

void BufferDetach(Buffer buffer, Pool pool)
{
  AVERT(Buffer, buffer);
  AVER(BufferIsReady(buffer));

  if (!BufferIsReset(buffer)) {
    Addr init, limit;
    Size spare;

    buffer->mode |= BufferModeTRANSITION;
    init = buffer->apStruct.init;
    limit = buffer->poolLimit;
    /* Ask the owning pool to do whatever it needs to before the */
    /* buffer is detached (e.g. copy buffer state into pool state). */
    (*pool->class->bufferEmpty)(pool, buffer, init, limit);
    /* Use of lightweight frames must have been disabled by now */
    AVER(BufferFrameState(buffer) == BufferFrameDISABLED);

    /* run any class-specific detachment method */
    buffer->class->detach(buffer);

    spare = AddrOffset(init, limit);
    buffer->emptySize += spare;
    if (buffer->isMutator) {
      buffer->pool->emptyMutatorSize += spare;
      ArenaGlobals(buffer->arena)->emptyMutatorSize += spare;
      ArenaGlobals(buffer->arena)->allocMutatorSize +=
        AddrOffset(buffer->base, init);
    } else {
      buffer->pool->emptyInternalSize += spare;
      ArenaGlobals(buffer->arena)->emptyInternalSize += spare;
    }

    /* Reset the buffer. */
    buffer->base = (Addr)0;
    buffer->initAtFlip = (Addr)0;
    buffer->apStruct.init = (Addr)0;
    buffer->apStruct.alloc = (Addr)0;
    buffer->apStruct.limit = (Addr)0;
    buffer->poolLimit = (Addr)0;
    buffer->mode &=
      ~(BufferModeATTACHED|BufferModeFLIPPED|BufferModeTRANSITION);
    BufferFrameSetState(buffer, BufferFrameDISABLED);

    EVENT_PW(BufferEmpty, buffer, spare);
  }
}


/* BufferDestroy -- destroy an allocation buffer
 *
 * See design.mps.buffer.method.destroy.  */

void BufferDestroy(Buffer buffer)
{
  Arena arena;
  BufferClass class;

  AVERT(Buffer, buffer);
  arena = buffer->arena;
  class = buffer->class;
  AVERT(BufferClass, class);
  BufferFinish(buffer);
  ControlFree(arena, buffer, class->size);
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

  /* design.mps.alloc-frame.lw-frame.sync.trip */
  if (BufferIsTrappedByMutator(buffer)) {
    BufferFrameNotifyPopPending(buffer);
  }

  BufferDetach(buffer, pool);

  /* Dispatch to the buffer class method to perform any  */
  /* class-specific finishing of the buffer. */
  (*buffer->class->finish)(buffer);

  /* Detach the buffer from its owning pool and unsig it. */
  RingRemove(&buffer->poolRing);
  buffer->sig = SigInvalid;
 
  /* Finish off the generic buffer fields. */
  RingFinish(&buffer->poolRing);

  EVENT_P(BufferFinish, buffer);
}


/* BufferIsReset -- test whether a buffer is in the "reset" state
 *
 * A buffer is "reset" when it is not attached.  In this state all of
 * the pointers into the region are zero.  This condition is checked by
 * BufferCheck.  */

Bool BufferIsReset(Buffer buffer)
{
  AVERT(Buffer, buffer);

  return !(buffer->mode & BufferModeATTACHED);
}


/* BufferIsReady -- test whether a buffer is ready for reserve
 *
 * BufferIsReady returns TRUE if and only if the buffer is not between a
 * reserve and commit.  The result is only reliable if the client is not
 * currently using the buffer, since it may update the alloc and init
 * pointers asynchronously.  */

Bool BufferIsReady(Buffer buffer)
{
  AVERT(Buffer, buffer);

  return buffer->apStruct.init == buffer->apStruct.alloc;
}


/* BufferIsMutator -- test whether buffer belongs to mutator
 *
 * Returns TRUE iff mutator was created for the mutator.  */

Bool BufferIsMutator(Buffer buffer)
{
  AVERT(Buffer, buffer);

  return buffer->isMutator;
}


/* BufferSetUnflipped
 *
 * Unflip a buffer if it was flipped.  */

static void BufferSetUnflipped(Buffer buffer)
{
  AVERT(Buffer, buffer);
  AVER(buffer->mode & BufferModeFLIPPED);
  buffer->mode &= ~BufferModeFLIPPED;
  /* restore apStruct.limit if appropriate */
  if (!BufferIsTrapped(buffer)) {
    buffer->apStruct.limit = buffer->poolLimit;
  }
  buffer->initAtFlip = (Addr)0;
}


/* BufferFrameState
 *
 * Returns the frame state of a buffer.  See
 * design.mps.alloc-frame.lw-frame.states.  */

FrameState BufferFrameState(Buffer buffer)
{
  AVERT(Buffer, buffer);
  if (buffer->apStruct.enabled) {
    if (buffer->apStruct.lwPopPending) {
      return BufferFramePOP_PENDING;
    } else {
      AVER(buffer->apStruct.frameptr == NULL);
      return BufferFrameVALID;
    }
  } else {
    AVER(buffer->apStruct.frameptr == NULL);
    AVER(buffer->apStruct.lwPopPending == FALSE);
    return BufferFrameDISABLED;
  }
}


/* BufferFrameSetState
 *
 * Sets the frame state of a buffer.  Only the mutator may set the
 * PopPending state.  See design.mps.alloc-frame.lw-frame.states.  */

void BufferFrameSetState(Buffer buffer, FrameState state)
{
  AVERT(Buffer, buffer);
  AVER(state == BufferFrameVALID || state == BufferFrameDISABLED);
  buffer->apStruct.frameptr = NULL;
  buffer->apStruct.lwPopPending = FALSE;
  buffer->apStruct.enabled = (state == BufferFrameVALID);
}


/* BufferSetAllocAddr
 *
 * Sets the init & alloc pointers of a buffer.  */

void BufferSetAllocAddr(Buffer buffer, Addr addr)
{
  AVERT(Buffer, buffer);
  /* Can't check Addr */
  AVER(BufferIsReady(buffer));
  AVER(buffer->base <= addr);
  AVER(buffer->poolLimit >= addr);

  buffer->apStruct.init = addr;
  buffer->apStruct.alloc = addr;
}


/* BufferFrameNotifyPopPending
 *
 * Notifies the pool when a lightweight frame pop operation has been
 * deferred and needs to be processed.  See
 * design.mps.alloc-frame.lw-frame.sync.trip.  */

static void BufferFrameNotifyPopPending(Buffer buffer)
{
  AllocFrame frame;
  Pool pool;
  AVER(BufferIsTrappedByMutator(buffer));
  AVER(BufferFrameState(buffer) == BufferFramePOP_PENDING);
  frame = (AllocFrame)buffer->apStruct.frameptr;
  /* Unset PopPending state & notify the pool */
  BufferFrameSetState(buffer, BufferFrameVALID);
  /* If the frame is no longer trapped, undo the trap by resetting */
  /* the AP limit pointer */
  if (!BufferIsTrapped(buffer)) {
    buffer->apStruct.limit = buffer->poolLimit;
  }
  pool = BufferPool(buffer);
  (*pool->class->framePopPending)(pool, buffer, frame);
}



/* BufferFramePush
 *
 * See design.mps.alloc-frame.  */

Res BufferFramePush(AllocFrame *frameReturn, Buffer buffer)
{
  Pool pool;
  AVERT(Buffer, buffer);
  AVER(frameReturn != NULL);


  /* Process any flip or PopPending */
  if (!BufferIsReset(buffer) && buffer->apStruct.limit == (Addr)0) {
    /* .fill.unflip: If the buffer is flipped then we unflip the buffer. */
    if (buffer->mode & BufferModeFLIPPED) {
      BufferSetUnflipped(buffer);
    }
 
    /* check for PopPending */
    if (BufferIsTrappedByMutator(buffer)) {
      BufferFrameNotifyPopPending(buffer);
    }
  }
  pool = BufferPool(buffer);
  return (*pool->class->framePush)(frameReturn, pool, buffer);
}


/* BufferFramePop
 *
 * See design.mps.alloc-frame.  */

Res BufferFramePop(Buffer buffer, AllocFrame frame)
{
  Pool pool;
  AVERT(Buffer, buffer);
  /* frame is of an abstract type & can't be checked */
  pool = BufferPool(buffer);
  return (*pool->class->framePop)(pool, buffer, frame);
 
}



/* BufferReserve -- reserve memory from an allocation buffer
 *
 * .reserve: Keep in sync with impl.h.mps.reserve.  */

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
  if (next > buffer->apStruct.alloc && next <= buffer->apStruct.limit) {
    buffer->apStruct.alloc = next;
    *pReturn = buffer->apStruct.init;
    return ResOK;
  }

  /* If the buffer can't accommodate the request, call "fill". */
  return BufferFill(pReturn, buffer, size, withReservoirPermit);
}


/* BufferAttach -- attach a region to a buffer
 *
 * BufferAttach is entered because of a BufferFill, or because of a Pop
 * operation on a lightweight frame.  */

void BufferAttach(Buffer buffer, Addr base, Addr limit,
                  Addr init, Size size)
{
  Size filled;

  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  AVER(AddrAdd(base, size) <= limit);
  AVER(base <= init);
  AVER(init <= limit);

  /* Set up the buffer to point at the supplied region */
  buffer->mode |= BufferModeATTACHED;
  buffer->base = base;
  buffer->apStruct.init = init;
  buffer->apStruct.alloc = AddrAdd(init, size);
  /* only set limit if not logged */
  if ((buffer->mode & BufferModeLOGGED) == 0) {
    buffer->apStruct.limit = limit;
  } else {
    AVER(buffer->apStruct.limit == (Addr)0);
  }
  AVER(buffer->initAtFlip == (Addr)0);
  buffer->poolLimit = limit;

  filled = AddrOffset(init, limit);
  buffer->fillSize += filled;
  if (buffer->isMutator) {
    if (base != init) { /* see design.mps.buffer.count.alloc.how */
      Size prealloc = AddrOffset(base, init);
      ArenaGlobals(buffer->arena)->allocMutatorSize -= prealloc;
    }
    buffer->pool->fillMutatorSize += filled;
    ArenaGlobals(buffer->arena)->fillMutatorSize += filled;
  } else {
    buffer->pool->fillInternalSize += filled;
    ArenaGlobals(buffer->arena)->fillInternalSize += filled;
  }

  /* run any class-specific attachment method */
  buffer->class->attach(buffer, base, limit, init, size);

  AVERT(Buffer, buffer);
  EVENT_PWAW(BufferFill, buffer, size, base, filled);
}


/* BufferFill -- refill an empty buffer
 *
 * BufferFill is entered by the "reserve" operation on a buffer if there
 * isn't enough room between "alloc" and "limit" to satisfy an
 * allocation request.  This might be because the buffer has been
 * trapped and "limit" has been set to zero.  */

Res BufferFill(Addr *pReturn, Buffer buffer, Size size,
               Bool withReservoirPermit)
{
  Res res;
  Pool pool;
  Addr base, limit, next;

  AVER(pReturn != NULL);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(SizeIsAligned(size, BufferPool(buffer)->alignment));
  AVER(BufferIsReady(buffer));

  pool = BufferPool(buffer);

  /* If we're here because the buffer was trapped, then we attempt */
  /* the allocation here. */
  if (!BufferIsReset(buffer) && buffer->apStruct.limit == (Addr)0) {
    /* .fill.unflip: If the buffer is flipped then we unflip the buffer. */
    if (buffer->mode & BufferModeFLIPPED) {
      BufferSetUnflipped(buffer);
    }

    /* design.mps.alloc-frame.lw-frame.sync.trip */
    if (BufferIsTrappedByMutator(buffer)) {
      BufferFrameNotifyPopPending(buffer);
    }

    /* .fill.logged: If the buffer is logged then we leave it logged. */
    next = AddrAdd(buffer->apStruct.alloc, size);
    if (next > buffer->apStruct.alloc &&
       next <= buffer->poolLimit) {
      buffer->apStruct.alloc = next;
      if (buffer->mode & BufferModeLOGGED) {
        EVENT_PAW(BufferReserve, buffer, buffer->apStruct.init, size);
      }
      *pReturn = buffer->apStruct.init;
      return ResOK;
    }
  }

  /* There really isn't enough room for the allocation now. */
  AVER(AddrAdd(buffer->apStruct.alloc, size) > buffer->poolLimit
       || AddrAdd(buffer->apStruct.alloc, size) < buffer->apStruct.alloc);

  BufferDetach(buffer, pool);

  /* Ask the pool for some memory. */
  res = (*pool->class->bufferFill)(&base, &limit,
                                   pool, buffer, size,
                                   withReservoirPermit);
  if (res != ResOK)
    return res;

  /* Set up the buffer to point at the memory given by the pool */
  /* and do the allocation that was requested by the client. */
  BufferAttach(buffer, base, limit, base, size);

  if (buffer->mode & BufferModeLOGGED) {
    EVENT_PAW(BufferReserve, buffer, buffer->apStruct.init, size);
  }

  *pReturn = base;
  return res;
}



/* BufferCommit -- commit memory previously reserved
 *
 * .commit: Keep in sync with impl.h.mps.commit.  */

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
  if (buffer->apStruct.limit == 0)
    return BufferTrip(buffer, p, size);

  /* No flip occurred, so succeed. */

  return TRUE;
}


/* BufferTrip -- act on a trapped buffer
 *
 * Called from BufferCommit (and its equivalents) when invoked on a
 * trapped buffer (indicated by limit == 0).  This function can decide
 * whether to succeed or fail the commit.  */

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
  /* But the mutator shouldn't have caused the trap */
  AVER(!BufferIsTrappedByMutator(buffer));

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

  /* .trip.unflip: If the flip occurred before commit set "init" */
  /* to "alloc" (see .commit.before) then the object is invalid */
  /* (won't've been scanned) so undo the allocation and fail commit. */
  /* Otherwise (see .commit.after) the object is valid (will've been */
  /* scanned) so commit can simply succeed. */
  if ((buffer->mode & BufferModeFLIPPED)
      && buffer->apStruct.init != buffer->initAtFlip) {
    /* Reset just enough state for Reserve/Fill to work. */
    /* The buffer is left trapped and we leave the untrapping */
    /* for the next reserve (which goes out of line to Fill */
    /* (.fill.unflip) because the buffer is still trapped) */
    buffer->apStruct.init = p;
    buffer->apStruct.alloc = p;
    return FALSE;
  }

  /* Emit event including class if loggged */
  if (buffer->mode & BufferModeLOGGED) {
    Bool b;
    Format format;
    Addr clientClass;

    b = PoolFormat(&format, buffer->pool);
    if (b) {
      clientClass = format->class(p);
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
 * between reserve and commit, and has a rank (i.e. references), and has
 * the two-phase protocol, then the object being initialized is
 * invalidated by failing the next commit.  The buffer code handles this
 * automatically (ie the pool implementation is not involved).  If the
 * buffer is reset there is no effect, since there is no object to
 * invalidate.  If the buffer is already flipped there is no effect,
 * since the object is already invalid by a previous trace.  The buffer
 * becomes unflipped at the next reserve or commit operation (actually
 * reserve because commit is lazy).  This is handled by BufferFill
 * (.fill.unflip) or BufferTrip (.trip.unflip).  */

void BufferFlip(Buffer buffer)
{
  AVERT(Buffer, buffer);

  if (BufferRankSet(buffer) != RankSetEMPTY
      && (buffer->mode & BufferModeFLIPPED) == 0
      && !BufferIsReset(buffer)) {
    AVER(buffer->initAtFlip == (Addr)0);
    buffer->initAtFlip = buffer->apStruct.init;
    /* Memory Barrier here? @@@@ */
    buffer->apStruct.limit = (Addr)0;
    buffer->mode |= BufferModeFLIPPED;
  }
}


/* BufferScanLimit -- return limit of data to which to scan
 *
 * Returns the highest address to which it is safe to scan objects in
 * the buffer.  When the buffer is not flipped, this is the "init" of
 * the AP.  When the buffer is flipped, it is the value that "init" had
 * at flip time.  [Could make BufferScanLimit return the AP "alloc" when
 * using ambiguous scanning.]  See .ap.async.  */

Addr BufferScanLimit(Buffer buffer)
{
  if (buffer->mode & BufferModeFLIPPED) {
    return buffer->initAtFlip;
  } else {
    return buffer->apStruct.init;
  }
}


Seg BufferSeg(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return buffer->class->seg(buffer);
}


RankSet BufferRankSet(Buffer buffer)
{
  AVERT(Buffer, buffer);
  return buffer->class->rankSet(buffer);
}

void BufferSetRankSet(Buffer buffer, RankSet rankset)
{
  AVERT(Buffer, buffer);
  AVERT(RankSet, rankset);
  buffer->class->setRankSet(buffer, rankset);
}


/* BufferReassignSeg -- adjust the seg of an attached buffer
 *
 * Used for segment splitting and merging.  */

void BufferReassignSeg(Buffer buffer, Seg seg)
{
  AVERT(Buffer, buffer);
  AVERT(Seg, seg);
  AVER(!BufferIsReset(buffer));
  AVER(BufferBase(buffer) >= SegBase(seg));
  AVER(BufferLimit(buffer) <= SegLimit(seg));
  AVER(BufferPool(buffer) == SegPool(seg));
  buffer->class->reassignSeg(buffer, seg);
}


/* BufferIsTrapped
 *
 * Indicates whether the buffer is trapped - either by MPS or the
 * mutator.  See .ap.async.  */

Bool BufferIsTrapped(Buffer buffer)
{
  /* Can't check buffer, see .check.use-trapped */
  return BufferIsTrappedByMutator(buffer)
         || ((buffer->mode & (BufferModeFLIPPED|BufferModeLOGGED)) != 0);
}


/* BufferIsTrappedByMutator
 *
 * Indicates whether the mutator trapped the buffer.  See
 * design.mps.alloc-frame.lw-frame.sync.trip and .ap.async.  */

Bool BufferIsTrappedByMutator(Buffer buffer)
{
  AVER(!buffer->apStruct.lwPopPending || buffer->apStruct.enabled);
  /* Can't check buffer, see .check.use-trapped */
  return buffer->apStruct.lwPopPending;
}


/* Alloc pattern functions
 *
 * Just represent the two patterns by two different pointers to dummies.  */

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
 * (and not ended), so we can do reset by ending all the current ramps.  */

void BufferRampBegin(Buffer buffer, AllocPattern pattern)
{
  Pool pool;

  AVERT(Buffer, buffer);
  AVERT(AllocPattern, pattern);

  ++buffer->rampCount;
  AVER(buffer->rampCount > 0);

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

  if (buffer->rampCount == 0)
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

  if (buffer->rampCount == 0)
    return;

  pool = BufferPool(buffer);
  AVERT(Pool, pool);
  do
    (*pool->class->rampEnd)(pool, buffer);
  while(--buffer->rampCount > 0);
}



/* BufferClass -- support for the basic Buffer class */


/* bufferTrivInit -- basic buffer init method */

static Res bufferTrivInit (Buffer buffer, Pool pool, va_list args)
{
  /* initialization happens in BufferInitV so checks are safe */
  AVERT(Buffer, buffer);
  AVERT(Pool, pool);
  UNUSED(args);
  EVENT_PPU(BufferInit, buffer, pool, buffer->isMutator);
  return ResOK;
}


/* bufferTrivFinish -- basic buffer finish method */

static void bufferTrivFinish (Buffer buffer)
{
  /* No special finish for simple buffers */
  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  NOOP;
}


/* bufferTrivAttach -- basic buffer attach method */

static void bufferTrivAttach(Buffer buffer, Addr base, Addr limit,
                             Addr init, Size size)
{
  /* No special attach method for simple buffers */
  AVERT(Buffer, buffer);
  /* Other parameters are consistency checked in BufferAttach */
  UNUSED(base);
  UNUSED(limit);
  UNUSED(init);
  UNUSED(size);
  NOOP;
}


/* bufferTrivDetach -- basic buffer detach method */

static void bufferTrivDetach(Buffer buffer)
{
  /* No special detach method for simple buffers */
  AVERT(Buffer, buffer);
  NOOP;
}


/* bufferNoSeg -- basic buffer BufferSeg accessor method
 *
 * .noseg: basic buffers don't support segments, so this method should
 * not be called.  */

static Seg bufferNoSeg (Buffer buffer)
{
  AVERT(Buffer, buffer);
  NOTREACHED;  /* .noseg */
  return NULL;
}



/* bufferTrivRankSet -- basic BufferRankSet accessor method */

static RankSet bufferTrivRankSet (Buffer buffer)
{
  AVERT(Buffer, buffer);
  /* vanilla buffers can only have empty rank set */
  return RankSetEMPTY;
}


/* bufferNoSetRankSet -- basic BufferSetRankSet setter method
 *
 * .norank: basic buffers don't support ranksets, so this method should
 * not be called.  */

static void bufferNoSetRankSet (Buffer buffer, RankSet rankset)
{
  AVERT(Buffer, buffer);
  AVERT(RankSet, rankset);
  NOTREACHED; /* .norank */
}


/* bufferNoReassignSeg -- basic BufferReassignSeg method
 *
 * .noseg: basic buffers don't support attachment to sements, so this
 * method should not be called.  */

static void bufferNoReassignSeg (Buffer buffer, Seg seg)
{
  AVERT(Buffer, buffer);
  AVERT(Seg, seg);
  NOTREACHED; /* .noseg */
}


/* bufferTrivDescribe -- basic Buffer describe method */

static Res bufferTrivDescribe(Buffer buffer, mps_lib_FILE *stream)
{
  if (!CHECKT(Buffer, buffer)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  /* dispatching function does it all */
  return ResOK;
}


/* BufferClassCheck -- check the consistency of a BufferClass */

Bool BufferClassCheck(BufferClass class)
{
  CHECKL(ProtocolClassCheck(&class->protocol));
  CHECKL(class->name != NULL); /* Should be <=6 char C identifier */
  CHECKL(class->size >= sizeof(BufferStruct));
  CHECKL(FUNCHECK(class->init));
  CHECKL(FUNCHECK(class->finish));
  CHECKL(FUNCHECK(class->attach));
  CHECKL(FUNCHECK(class->detach));
  CHECKL(FUNCHECK(class->seg));
  CHECKL(FUNCHECK(class->rankSet));
  CHECKL(FUNCHECK(class->setRankSet));
  CHECKL(FUNCHECK(class->reassignSeg));
  CHECKL(FUNCHECK(class->describe));
  CHECKS(BufferClass, class);
  return TRUE;
}


/* BufferClass -- the vanilla buffer class definition
 *
 * See design.mps.buffer.class.hierarchy.buffer.  */

DEFINE_CLASS(BufferClass, class)
{
  INHERIT_CLASS(&class->protocol, ProtocolClass);
  class->name = "BUFFER";
  class->size = sizeof(BufferStruct);
  class->init = bufferTrivInit;
  class->finish = bufferTrivFinish;
  class->attach = bufferTrivAttach;
  class->detach = bufferTrivDetach;
  class->describe = bufferTrivDescribe;
  class->seg = bufferNoSeg;
  class->rankSet = bufferTrivRankSet;
  class->setRankSet = bufferNoSetRankSet;
  class->reassignSeg = bufferNoReassignSeg;
  class->sig = BufferClassSig;
}



/* SegBufClass -- support for the SegBuf subclass */


/* BufferSegBuf -- convert generic Buffer to a SegBuf */

#define BufferSegBuf(buffer) ((SegBuf)(buffer))


/* SegBufCheck -- check consistency of a SegBuf */

Bool SegBufCheck(SegBuf segbuf)
{
  Buffer buffer;

  CHECKS(SegBuf, segbuf);
  buffer = &segbuf->bufferStruct;
  CHECKL(BufferCheck(buffer));
  CHECKL(RankSetCheck(segbuf->rankSet));

  if (buffer->mode & BufferModeTRANSITION) {
    /* nothing to check */
  } else if ((buffer->mode & BufferModeATTACHED) == 0) {
    CHECKL(segbuf->seg == NULL);
  } else {
    /* The buffer is attached to a segment. */
    CHECKL(segbuf->seg != NULL);
    CHECKL(SegCheck(segbuf->seg));
    /* To avoid recursive checking, leave it to SegCheck to make */
    /* sure the buffer and segment fields tally. */
   
    if (buffer->mode & BufferModeFLIPPED) {
      /* Only buffers that allocate pointers get flipped. */
      CHECKL(segbuf->rankSet != RankSetEMPTY);
    }
  }

  return TRUE;
}


/* segBufInit -- SegBuf init method */

static Res segBufInit (Buffer buffer, Pool pool, va_list args)
{
  BufferClass super;
  SegBuf segbuf;
  Res res;

  AVERT(Buffer, buffer);
  AVERT(Pool, pool);
  segbuf = BufferSegBuf(buffer);

  /* Initialize the superclass fields first via next-method call */
  super = BUFFER_SUPERCLASS(SegBufClass);
  res = super->init(buffer, pool, args);
  if (res != ResOK)
    return res;

  segbuf->seg = NULL;
  segbuf->sig = SegBufSig;
  segbuf->rankSet = RankSetEMPTY;

  AVERT(SegBuf, segbuf);
  EVENT_PPU(BufferInitSeg, buffer, pool, buffer->isMutator);
  return ResOK;
}


/* segBufFinish -- SegBuf finish method */

static void segBufFinish (Buffer buffer)
{
  BufferClass super;
  SegBuf segbuf;

  AVERT(Buffer, buffer);
  AVER(BufferIsReset(buffer));
  segbuf = BufferSegBuf(buffer);
  AVERT(SegBuf, segbuf);

  segbuf->sig = SigInvalid;

  /* finish the superclass fields last */
  super = BUFFER_SUPERCLASS(SegBufClass);
  super->finish(buffer);
}


/* segBufAttach -- SegBuf attach method */

static void segBufAttach(Buffer buffer, Addr base, Addr limit,
                         Addr init, Size size)
{
  SegBuf segbuf;
  Seg seg;
  Arena arena;
  Bool found;

  AVERT(Buffer, buffer);
  /* Other parameters are consistency checked in BufferAttach */
  UNUSED(init);
  UNUSED(size);

  segbuf = BufferSegBuf(buffer);
  arena = BufferArena(buffer);
  found = SegOfAddr(&seg, arena, base);
  AVER(found);
  AVER(segbuf->seg == NULL);
  AVER(SegBuffer(seg) == NULL);
  AVER(SegBase(seg) <= base);
  AVER(limit <= SegLimit(seg));

  /* attach the buffer to the segment */
  SegSetBuffer(seg, buffer);
  segbuf->seg = seg;

  AVERT(SegBuf, segbuf);
}


/* segBufDetach -- SegBuf detach method */

static void segBufDetach(Buffer buffer)
{
  SegBuf segbuf;
  Seg seg;

  AVERT(Buffer, buffer);
  segbuf = BufferSegBuf(buffer);
  AVERT(SegBuf, segbuf);

  seg = segbuf->seg;
  AVER(seg != NULL);
  SegSetBuffer(seg, NULL);
  segbuf->seg = NULL;
}


/* segBufSeg -- BufferSeg accessor method for SegBuf instances */

static Seg segBufSeg (Buffer buffer)
{
  SegBuf segbuf;

  AVERT(Buffer, buffer);
  segbuf = BufferSegBuf(buffer);
  AVERT(SegBuf, segbuf);
  return segbuf->seg;
}


/* segBufRankSet -- BufferRankSet accessor for SegBuf instances */

static RankSet segBufRankSet (Buffer buffer)
{
  SegBuf segbuf;

  AVERT(Buffer, buffer);
  segbuf = BufferSegBuf(buffer);
  AVERT(SegBuf, segbuf);
  return segbuf->rankSet;
}


/* segBufSetRankSet -- BufferSetRankSet setter method for SegBuf */

static void segBufSetRankSet (Buffer buffer, RankSet rankset)
{
  SegBuf segbuf;

  AVERT(Buffer, buffer);
  AVERT(RankSet, rankset);
  segbuf = BufferSegBuf(buffer);
  AVERT(SegBuf, segbuf);
  segbuf->rankSet = rankset;
}


/* segBufReassignSeg -- BufferReassignSeg method for SegBuf
 *
 * Used to support segment merging and splitting.
 *
 * .invseg: On entry the buffer is attached to an invalid segment, which
 * can't be checked. The method is called to make the attachment valid.  */

static void segBufReassignSeg (Buffer buffer, Seg seg)
{
  SegBuf segbuf;

  AVERT(Buffer, buffer);
  AVERT(Seg, seg);
  segbuf = BufferSegBuf(buffer);
  /* Can't check segbuf on entry. See .invseg */
  AVER(NULL != segbuf->seg);
  AVER(seg != segbuf->seg);
  segbuf->seg = seg;
  AVERT(SegBuf, segbuf);
}


/* segBufDescribe --  describe method for SegBuf */

static Res segBufDescribe(Buffer buffer, mps_lib_FILE *stream)
{
  SegBuf segbuf;
  BufferClass super;
  Res res;

  if (!CHECKT(Buffer, buffer)) return ResFAIL;
  if (stream == NULL) return ResFAIL;
  segbuf = BufferSegBuf(buffer);
  if (!CHECKT(SegBuf, segbuf)) return ResFAIL;

  /* Describe the superclass fields first via next-method call */
  super = BUFFER_SUPERCLASS(SegBufClass);
  res = super->describe(buffer, stream);
  if (res != ResOK) return res;

  res = WriteF(stream,
               "  Seg $P\n",         (WriteFP)segbuf->seg,
               "  rankSet $U\n",     (WriteFU)segbuf->rankSet,
               NULL);

  return res;
}


/* SegBufClass -- SegBuf class definition
 *
 * Supports an association with a single segment when attached.  See
 * design.mps.buffer.class.hierarchy.segbuf.  */

typedef BufferClassStruct SegBufClassStruct;

DEFINE_CLASS(SegBufClass, class)
{
  INHERIT_CLASS(class, BufferClass);
  class->name = "SEGBUF";
  class->size = sizeof(SegBufStruct);
  class->init = segBufInit;
  class->finish = segBufFinish;
  class->attach = segBufAttach;
  class->detach = segBufDetach;
  class->describe = segBufDescribe;
  class->seg = segBufSeg;
  class->rankSet = segBufRankSet;
  class->setRankSet = segBufSetRankSet;
  class->reassignSeg = segBufReassignSeg;
}


/* RankBufClass -- support for the RankBufClass subclass */


/* rankBufInit -- RankBufClass init method */

static Res rankBufInit (Buffer buffer, Pool pool, va_list args)
{
  /* Assumes pun compatibility between Rank and mps_rank_t */
  /* Which is checked by mpsi_check in impl.c.mpsi */
  Rank rank = va_arg(args, Rank);
  BufferClass super;
  Res res;

  AVERT(Buffer, buffer);
  AVERT(Pool, pool);
  AVER(RankCheck(rank));

  /* Initialize the superclass fields first via next-method call */
  super = BUFFER_SUPERCLASS(RankBufClass);
  res = super->init(buffer, pool, args);
  if (res != ResOK)
    return res;

  BufferSetRankSet(buffer, RankSetSingle(rank));

  /* There's nothing to check that the superclass doesn't, so no AVERT. */
  EVENT_PPUU(BufferInitRank, buffer, pool, buffer->isMutator, rank);
  return ResOK;
}


/* RankBufClass -- RankBufClass class definition
 *
 * A subclass of SegBufClass, sharing structure for instances.
 *
 * Supports initialization to a rank supplied at creation time.  */

typedef BufferClassStruct RankBufClassStruct;

DEFINE_CLASS(RankBufClass, class)
{
  INHERIT_CLASS(class, SegBufClass);
  class->name = "RANKBUF";
  class->init = rankBufInit;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* impl.c.poolsnc: STACK NO CHECKING POOL CLASS
 *
 * $HopeName: MMsrc!poolsnc.c(trunk.4) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 *
 * DESIGN
 *
 * .design: design.mps.poolsnc
 *
 * LIGHTWEIGHT FRAMES
 * 
 * .lw-frame-state: The pool uses lightweight frames as its only
 * type of allocation frame. The lightweight frame state is set to 
 * Valid whenever a buffer has a segment and Disabled otherwise.
 * See design.mps.alloc-frame.lw-frame.states.
 *
 * .lw-frame-null: The frame marker NULL is used as a special value
 * to indicate bottom of stack.
 */

#include "mpscsnc.h"
#include "mpm.h"


SRCID(poolsnc, "$HopeName: MMsrc!poolsnc.c(trunk.4) $");


#define SNCSig  ((Sig)0x519b754c)       /* SIGPooLSNC */

#define SNCGen  ((Serial)1) /* "generation" for SNC pools */


/* design.mps.poolsnc.poolstruct */
typedef struct SNCStruct {
  PoolStruct poolStruct;
  Seg freeSegs;
  SegPrefStruct segPrefStruct;
  Sig sig;
} SNCStruct, *SNC;



/* PoolPoolSNC -- convert generic Pool to SNC */

#define PoolPoolSNC(pool) \
  PARENT(SNCStruct, poolStruct, (pool))



static Bool SNCCheck(SNC snc);


/* Management of segment chains
 *
 * Each buffer has an associated segment chain in stack order
 * (top of stack first). We subclass the buffer to maintain the
 * head of the chain. Segments are chained using the SegP field.
 */



/* SNCBufStruct -- SNC Buffer subclass
 *
 * This subclass of BufferedSeg hold a segment chain.
 */

#define SNCBufSig ((Sig)0x51954CBF) /* SIGnature SNC BuFfer  */ 

typedef struct SNCBufStruct *SNCBuf;

typedef struct SNCBufStruct {
  BufferedSegStruct bufSegStruct; /* superclass fields must come first */
  Seg topseg;                     /* The segment chain head */
  Sig sig;                        /* design.mps.sig */
} SNCBufStruct;


/* BufferSNCBuf -- convert generic Buffer to an SNCBuf */

#define BufferSNCBuf(buffer) ((SNCBuf)(buffer))


/* SNCBufCheck -- check consistency of an SNCBuf */

static Bool SNCBufCheck(SNCBuf sncbuf)
{
  BufferedSeg bufseg;

  CHECKS(SNCBuf, sncbuf);
  bufseg = &sncbuf->bufSegStruct;
  CHECKL(BufferedSegCheck(bufseg));
  if (sncbuf->topseg != NULL) {
    CHECKL(SegCheck(sncbuf->topseg));
  }
  return TRUE;
}


/* sncBufferTopSeg -- return the head of segment chain from an SNCBuf */

static Seg sncBufferTopSeg(Buffer buffer)
{
  SNCBuf sncbuf;
  AVERT(Buffer, buffer);
  sncbuf = BufferSNCBuf(buffer);
  AVERT(SNCBuf, sncbuf);
  return sncbuf->topseg;
}


/* sncBufferSetTopSeg -- set the head of segment chain from an SNCBuf */

static void sncBufferSetTopSeg(Buffer buffer, Seg seg)
{
  SNCBuf sncbuf;
  AVERT(Buffer, buffer);
  AVERT(Seg, seg);
  sncbuf = BufferSNCBuf(buffer);
  AVERT(SNCBuf, sncbuf);
  sncbuf->topseg = seg;
}


/* SNCBufInit -- Initialize an SNCBuf */

static Res SNCBufInit (Buffer buffer, Pool pool)
{
  SNCBuf sncbuf;
  BufferClass superclass = EnsureBufferedSegClass();

  AVERT(Buffer, buffer);
  AVERT(Pool, pool);

  /* call next method */
  (*superclass->init)(buffer, pool);

  sncbuf = BufferSNCBuf(buffer);
  sncbuf->topseg = NULL;
  sncbuf->sig = SNCBufSig;

  AVERT(SNCBuf, sncbuf);
  return ResOK;
}


/* SNCBufFinish -- Finish an SNCBuf */

static void SNCBufFinish(Buffer buffer)
{
  BufferClass super;
  SNCBuf sncbuf;

  AVERT(Buffer, buffer);
  sncbuf = BufferSNCBuf(buffer);
  AVERT(SNCBuf, sncbuf);

  sncbuf->sig = SigInvalid;

  /* finish the superclass fields last */
  super = EnsureBufferedSegClass();
  super->finish(buffer);
}


/* SNCBufClass -- The class definition */

DEFINE_BUFFER_CLASS(SNCBufClass, class)
{
  INHERIT_CLASS(class, BufferedSegClass);
  class->name = "SNCBUF";
  class->size = sizeof(SNCBufStruct);
  class->init = SNCBufInit;
  class->finish = SNCBufFinish;
}


#define sncSegNext(seg) ((Seg)SegP((seg)))
#define sncSegSetNext(seg, next) (SegSetP((seg), (void*)(next)))



/* sncRecordAllocatedSeg  - stores a segment on the buffer chain */

static void sncRecordAllocatedSeg(Buffer buffer, Seg seg)
{
  AVERT(Buffer, buffer);
  AVERT(Seg, seg);
  AVER(sncSegNext(seg) == NULL);

  sncSegSetNext(seg, sncBufferTopSeg(buffer));
  sncBufferSetTopSeg(buffer, seg);
}


/* sncRecordFreeSeg  - stores a segment on the freelist */

static void sncRecordFreeSeg(SNC snc, Seg seg)
{
  AVERT(SNC, snc);
  AVERT(Seg, seg);
  AVER(sncSegNext(seg) == NULL);

  /* Make sure it's not grey, and set to RankSetEMPTY */
  /* This means it won't be scanned */
  SegSetGrey(seg, TraceSetEMPTY);
  SegSetRankAndSummary(seg, RankSetEMPTY, RefSetEMPTY);

  sncSegSetNext(seg, snc->freeSegs);
  snc->freeSegs = seg;
}


/* sncPopPartialSegChain
 *
 * Pops segments from the buffer chain up to a specified limit
 */

static void sncPopPartialSegChain(SNC snc, Buffer buf, Seg upTo)
{
  Seg free;
  AVERT(SNC, snc);
  AVERT(Buffer, buf);
  if (upTo != NULL) {
    AVERT(Seg, upTo);
  }

  /* Iterate the buffer chain of segments freeing all until upTo */
  free = sncBufferTopSeg(buf);
  while (free != upTo) {
    Seg next;
    AVER(free != NULL);
    next = sncSegNext(free);
    sncSegSetNext(free, NULL);
    sncRecordFreeSeg(snc, free);
    free = next;
  }
  /* Make upTo the head of the buffer chain */
  sncBufferSetTopSeg(buf, upTo);
}


/* sncFindFreeSeg
 *
 * attempts to find and detach a large enough segment from the 
 * freelist. returns TRUE on success.
 */

static Bool sncFindFreeSeg(Seg *segReturn, SNC snc, Size size)
{
  Seg free = snc->freeSegs;
  Seg last = NULL;

  AVERT(SNC, snc);
  AVER(size > 0);

  /* iterate over the free list returning anything big enough */
  while (free != NULL) {
    AVERT(Seg, free);
    if (SegSize(free) >= size) {
      /* This segment is big enough. Detach & return it */
      if (last == NULL) {
        snc->freeSegs = sncSegNext(free);
      } else {
        sncSegSetNext(last, sncSegNext(free));
      }
      sncSegSetNext(free, NULL);
      *segReturn = free;
      return TRUE;
    }
    last = free;
    free = sncSegNext(free);
  }

  return FALSE;
}


static Res SNCInit(Pool pool, va_list arg)
{
  SNC snc;
  Format format;

  /* weak check, as half way through initialization */
  AVER(pool != NULL);

  snc = PoolPoolSNC(pool);

  format = va_arg(arg, Format);

  AVERT(Format, format);
  pool->format = format;
  snc->freeSegs = NULL;
  /* Use the default segpref for the pool. At least this should avoid */
  /* clashes with collected pools */
  snc->segPrefStruct = *SegPrefDefault();
  snc->sig = SNCSig;
  AVERT(SNC, snc);

  return ResOK;
}


static void SNCFinish(Pool pool)
{
  SNC snc;
  Ring ring, node, nextNode;

  AVERT(Pool, pool);
  snc = PoolPoolSNC(pool);
  AVERT(SNC, snc);

  ring = &pool->segRing;
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    AVERT(Seg, seg);
    SegFree(seg);
  }
}


/* SNCBufferInit -- the buffer init method */

static Res SNCBufferInit(Pool pool, Buffer buffer, va_list args)
{
  Rank rank = va_arg(args, Rank);
  SNC snc;

  AVERT(Pool, pool);
  AVER(RankCheck(rank));
  AVER(rank == RankEXACT);  /* SNC only accepts RankEXACT */
  snc = PoolPoolSNC(pool);
  AVERT(SNC, snc);
  BufferSetRankSet(buffer, RankSetSingle(rank));
  /* Initialize buffer's segment chain to empty */
  sncBufferSetTopSeg(buffer, NULL);
  return ResOK;
}


static Res SNCBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size,
                         Bool withReservoirPermit)
{
  SNC snc;
  Arena arena;
  Res res;
  Seg seg;
  Size asize;           /* aligned size */

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  AVER(size > 0);
  AVER(BoolCheck(withReservoirPermit));
  AVER(BufferIsReset(buffer));

  snc = PoolPoolSNC(pool);
  AVERT(SNC, snc);

  /* Try to find a free segment with enough space already */
  if (sncFindFreeSeg(&seg, snc, size)) {
    goto found;
  }

  /* No free seg, so create a new one */
  arena = PoolArena(pool);
  asize = SizeAlignUp(size, ArenaAlign(arena));
  res = SegAlloc(&seg, &snc->segPrefStruct, asize, 
                 pool, withReservoirPermit);
  if(res != ResOK) {
    return res;
  }
  sncSegSetNext(seg, NULL);

found:
  /* design.mps.seg.field.rankSet.start */
  if(BufferRankSet(buffer) == RankSetEMPTY) {
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetEMPTY);
  } else {
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetUNIV);
  }

  AVERT(Seg, seg);
  /* put the segment on the buffer chain */
  sncRecordAllocatedSeg(buffer, seg);
  /* Permit the use of lightweight frames - .lw-frame-state */
  BufferFrameSetState(buffer, BufferFrameVALID);
  *baseReturn = SegBase(seg);
  *limitReturn = SegLimit(seg);
  return ResOK;
}


static void SNCBufferEmpty(Pool pool, Buffer buffer, 
                           Addr init, Addr limit)
{
  SNC snc;
  Seg seg;
  Arena arena;
  Size size;

  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  seg = BufferSeg(buffer);
  AVER(init <= limit);
  AVER(SegLimit(seg) == limit);
  snc = PoolPoolSNC(pool);
  AVERT(SNC, snc);
  AVER(BufferFrameState(buffer) == BufferFrameVALID);
  /* .lw-frame-state */
  BufferFrameSetState(buffer, BufferFrameDISABLED); 

  arena = BufferArena(buffer);

  /* Pad the end unused space at the end of the segment */
  size = AddrOffset(init, limit);
  if(size > 0) {
    ShieldExpose(arena, seg);
    (*pool->format->pad)(init, size);
    ShieldCover(arena, seg);
  }
}

static void SNCBufferFinish(Pool pool, Buffer buffer)
{
  SNC snc;

  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  snc = PoolPoolSNC(pool);
  AVERT(SNC, snc);
  AVER(BufferIsReset(buffer));

  /* Put any segments which haven't bee popped onto the free list */
  sncPopPartialSegChain(snc, buffer, NULL);
}


static Res SNCScan(Bool *totalReturn, ScanState ss, Pool pool, Seg seg)
{
  Addr base, limit;
  Format format;
  SNC snc;
  Res res;

  AVER(totalReturn != NULL);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVERT(Pool, pool);
  snc = PoolPoolSNC(pool);
  AVERT(SNC, snc);

  format = pool->format;
  base = SegBase(seg);
    
  /* If the segment is buffered, only walk as far as the end */
  /* of the initialized objects.  */
  if(SegBuffer(seg) != NULL) {
    limit = BufferScanLimit(SegBuffer(seg));
  } else {
    limit = SegLimit(seg);
  }
  
  if(base < limit) {
    res = (*format->scan)(ss, base, limit);
    if(res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  } else {
    AVER(base == limit);
  }

  ss->scannedSize += AddrOffset(base, limit);

  *totalReturn = TRUE;
  return ResOK;
}



static Res SNCFramePush(AllocFrame *frameReturn, Pool pool, Buffer buf)
{
  FrameState state;
  AVER(frameReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buf);

  state = BufferFrameState(buf);
  /* Sould have been notified of pending pops before this */
  AVER(state == BufferFrameVALID || state == BufferFrameDISABLED);
  if (state == BufferFrameDISABLED) {
    AVER(BufferIsReset(buf));  /* The buffer must be reset */
    AVER(sncBufferTopSeg(buf) == NULL);  /* The stack must be empty  */
    /* Use NULL to indicate an empty stack. .lw-frame-null */
    *frameReturn = NULL;
  } else {
    /* Use the scan limit as the lightweight frame pointer */
    *frameReturn = (AllocFrame)BufferScanLimit(buf);
  }
  return ResOK;
}



static Res SNCFramePop(Pool pool, Buffer buf, AllocFrame frame)
{
  AVERT(Pool, pool);
  AVERT(Buffer, buf);
  /* Normally the Pop would be handled as a lightweight pop */
  /* The only reason that might not happen is if the stack is empty */
  AVER(sncBufferTopSeg(buf) == NULL);
  /* The only valid frame must also be NULL - .lw-frame-null  */
  AVER(frame == NULL);
  /* Popping an empty frame is a NOOP */
  return ResOK;
}


static void SNCFramePopPending(Pool pool, Buffer buf, AllocFrame frame)
{
  Addr addr;
  SNC snc;
  AVERT(Pool, pool);
  AVERT(Buffer, buf);
  /* frame is an Addr and can't be directly checked */
  snc = PoolPoolSNC(pool);
  AVERT(SNC, snc);

  AVER(BufferFrameState(buf) == BufferFrameVALID);
  
  if (frame == NULL) {
    /* corresponds to a pop to bottom of stack. .lw-frame-null */
    BufferDetach(buf, pool);
    sncPopPartialSegChain(snc, buf, NULL);

  } else {
    Arena arena;
    Seg seg;
    Bool foundSeg;

    arena = PoolArena(pool);
    addr = (Addr)frame;
    foundSeg = SegOfAddr(&seg, arena, addr);
    AVER(foundSeg);

    if (SegBuffer(seg) == buf) {
      /* don't need to change the segment - just the alloc pointers */
      AVER(addr <= BufferScanLimit(buf));  /* check direction of pop */
      BufferSetAllocAddr(buf, addr);
    } else {
      /* need to change segment  */
      BufferDetach(buf, pool);
      sncPopPartialSegChain(snc, buf, seg);
      BufferAttach(buf, SegBase(seg), SegLimit(seg), addr, (Size)0);
      /* Permit the use of lightweight frames - .lw-frame-state */
      BufferFrameSetState(buf, BufferFrameVALID);
    }
  }
}


static void SNCWalk(Pool pool, Seg seg, FormattedObjectsStepMethod f,
                    void *p, unsigned long s)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  /* Avoid applying the function to grey objects. */
  /* They may have pointers to old-space. */
  if(SegGrey(seg) == TraceSetEMPTY) {
    Addr object = SegBase(seg);
    Addr nextObject;
    Addr limit;
    SNC snc;
    Format format;

    snc = PoolPoolSNC(pool);
    AVERT(SNC, snc);
    format = pool->format;

    /* If the segment is buffered, only walk as far as the end */
    /* of the initialized objects.  cf. SNCScan */
    if(SegBuffer(seg) != NULL) {
      limit = BufferScanLimit(SegBuffer(seg));
    } else {
      limit = SegLimit(seg);
    }

    while(object < limit) {
      (*f)(object, pool->format, pool, p, s);
      nextObject = (*pool->format->skip)(object);
      AVER(nextObject > object);
      object = nextObject;
    }
    AVER(object == limit);
  }
}


/* SNCPoolClass -- the class definition */

DEFINE_POOL_CLASS(SNCPoolClass, this)
{
  INHERIT_CLASS(this, AbstractScanPoolClass);
  PoolClassMixInFormat(this);
  this->name = "SNC";
  this->size = sizeof(SNCStruct);
  this->offset = offsetof(SNCStruct, poolStruct);
  this->init = SNCInit;
  this->finish = SNCFinish;
  this->bufferInit = SNCBufferInit;
  this->bufferFill = SNCBufferFill;
  this->bufferEmpty = SNCBufferEmpty;
  this->bufferFinish = SNCBufferFinish;
  this->scan = SNCScan;
  this->framePush = SNCFramePush;
  this->framePop = SNCFramePop;
  this->framePopPending = SNCFramePopPending;
  this->walk = SNCWalk;
  this->bufferClass = EnsureSNCBufClass;
}


mps_class_t mps_class_snc(void)
{
  return (mps_class_t)EnsureSNCPoolClass();
}


static Bool SNCCheck(SNC snc)
{
  CHECKS(SNC, snc);
  CHECKD(Pool, &snc->poolStruct);
  CHECKD(SegPref, &snc->segPrefStruct);
  CHECKL(snc->poolStruct.class == EnsureSNCPoolClass());
  if (snc->freeSegs != NULL) {
    CHECKL(SegCheck(snc->freeSegs));
  }
  return TRUE;
}


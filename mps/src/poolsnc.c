/* impl.c.poolsnc: AUTOMATIC WEAK LINKED POOL CLASS
 *
 * $HopeName: MMsrc!poolsnc.c() $
 * Copyright (C) 1998.  Harlequin Group plc.  All rights reserved.
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


SRCID(poolsnc, "$HopeName: MMsrc!poolsnc.c() $");


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
 * (top of stack first). The free segments are also stored 
 * as a segment chain.
 * Segments are chained using the SegP field.
 */

#define sncSegNext(seg) ((Seg)SegP((seg)))
#define sncSegSetNext(seg, next) (SegSetP((seg), (void*)(next)))

#define sncBufferTopSeg(buffer) ((Seg)((buffer)->p))
#define sncBufferSetTopSeg(buffer, seg) ((buffer)->p = (void*)(seg))


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


/* sncSegIsFree
 *
 * determines whether a segment is free by iterating down the free chain
 */

static Bool sncSegIsFree(SNC snc, Seg seg)
{
  Seg free;

  AVERT(SNC, snc);
  AVERT(Seg, seg);

  free = snc->freeSegs;
  while (free != NULL) {
    AVERT(Seg, free);
    if (free == seg) {
      return TRUE;
    }
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
  buffer->rankSet = RankSetSingle(rank);
  /* Initialize buffer's segment chain to empty */
  sncBufferSetTopSeg(buffer, NULL);
  return ResOK;
}


static Res SNCBufferFill(Seg *segReturn, Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size,
                         Bool withReservoirPermit)
{
  SNC snc;
  Arena arena;
  Res res;
  Seg seg;
  Size asize;           /* aligned size */

  AVER(segReturn != NULL);
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
  /* design.mps.seg.field.rankSet.start */
  if(BufferRankSet(buffer) == RankSetEMPTY) {
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetEMPTY);
  } else {
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetUNIV);
  }

found:
  AVERT(Seg, seg);
  /* put the segment on the buffer chain */
  sncRecordAllocatedSeg(buffer, seg);
  /* Permit the use of lightweight frames - .lw-frame-state */
  BufferFrameSetState(buffer, BufferFrameVALID);
  *segReturn = seg;
  *baseReturn = SegBase(seg);
  *limitReturn = SegLimit(seg);
  return ResOK;
}


static void SNCBufferEmpty(Pool pool, Buffer buffer)
{
  SNC snc;
  Seg seg;
  Arena arena;
  Size size;

  AVERT(Pool, pool);
  AVERT(Buffer, buffer);
  snc = PoolPoolSNC(pool);
  AVERT(SNC, snc);
  AVER(BufferFrameState(buffer) == BufferFrameVALID);
  /* .lw-frame-state */
  BufferFrameSetState(buffer, BufferFrameDISABLED); 

  seg = BufferSeg(buffer);
  arena = BufferArena(buffer);

  /* Pad the end unused space at the end of the segment */
  size = AddrOffset(BufferGetInit(buffer), SegLimit(seg));
  if(size > 0) {
    ShieldExpose(arena, seg);
    (*pool->format->pad)(BufferGetInit(buffer), size);
    ShieldCover(arena, seg);
  }
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

  /* If the segment has become free then there's no need to scan it */
  if (!sncSegIsFree(snc, seg)) {
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
  }

  *totalReturn = TRUE;
  return ResOK;
}



static Res SNCFix(Pool pool, ScanState ss, Seg seg, Ref *refIO)
{
  AVERT(Pool, pool);
  AVERT(ScanState, ss);
  AVERT(Seg, seg);
  AVER(refIO != NULL);

  /* The fix method is a NOOP. */
  /* The reference might be a valid reference or even a dangling */
  /* reference to either an allocated or free segment of the pool */
  /* A NOOP fix will at least avoid breaking the GC when this */
  /* happens */

  ss->wasMarked = TRUE;

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
      BufferAttach(buf, seg, 
                   SegBase(seg), SegLimit(seg), addr, (Size)0);
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
  this->scan = SNCScan;
  this->fix = SNCFix;
  this->fixEmergency = SNCFix;
  this->framePush = SNCFramePush;
  this->framePop = SNCFramePop;
  this->framePopPending = SNCFramePopPending;
  this->walk = SNCWalk;
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


/* poolsnc.c: STACK NO CHECKING POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
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
 * See <design/alloc-frame/#lw-frame.states>.
 *
 * .lw-frame-null: The frame marker NULL is used as a special value
 * to indicate bottom of stack.
 */

#include "mpscsnc.h"
#include "mpm.h"

SRCID(poolsnc, "$Id$");


/* SNCStruct -- structure for an SNC pool
 *
 * See design.mps.poolsnc.poolstruct.
 */

#define SNCSig  ((Sig)0x519b754c)       /* SIGPooLSNC */

typedef struct SNCStruct {
  PoolStruct poolStruct;
  Seg freeSegs;
  Sig sig;
} SNCStruct, *SNC;

#define PoolSNC(pool) PARENT(SNCStruct, poolStruct, (pool))
#define SNCPool(snc) (&(snc)->poolStruct)


/* Forward declarations */

extern SegClass SNCSegClassGet(void);
extern BufferClass SNCBufClassGet(void);
static Bool SNCCheck(SNC snc);
static void sncPopPartialSegChain(SNC snc, Buffer buf, Seg upTo);


/* Management of segment chains
 *
 * Each buffer has an associated segment chain in stack order
 * (top of stack first). We subclass the buffer to maintain the
 * head of the chain. Segments are chained using the SegP field.
 */



/* SNCBufStruct -- SNC Buffer subclass
 *
 * This subclass of RankBuf holds a segment chain.
 */

#define SNCBufSig ((Sig)0x51954CBF) /* SIGnature SNC BuFfer  */

typedef struct SNCBufStruct *SNCBuf;

typedef struct SNCBufStruct {
  SegBufStruct segBufStruct;      /* superclass fields must come first */
  Seg topseg;                     /* The segment chain head -- may be NULL */
  Sig sig;                        /* <design/sig/> */
} SNCBufStruct;


/* BufferSNCBuf -- convert generic Buffer to an SNCBuf */

#define BufferSNCBuf(buffer) ((SNCBuf)(buffer))


/* SNCBufCheck -- check consistency of an SNCBuf */

ATTRIBUTE_UNUSED
static Bool SNCBufCheck(SNCBuf sncbuf)
{
  SegBuf segbuf;

  CHECKS(SNCBuf, sncbuf);
  segbuf = &sncbuf->segBufStruct;
  CHECKD(SegBuf, segbuf);
  if (sncbuf->topseg != NULL) {
    CHECKD(Seg, sncbuf->topseg);
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
  if (NULL != seg)
    AVERT(Seg, seg);
  sncbuf = BufferSNCBuf(buffer);
  AVERT(SNCBuf, sncbuf);
  sncbuf->topseg = seg;
}


/* SNCBufInit -- Initialize an SNCBuf */

static Res SNCBufInit(Buffer buffer, Pool pool, ArgList args)
{
  SNCBuf sncbuf;
  Res res;
  BufferClass superclass;

  AVERT(Buffer, buffer);
  AVERT(Pool, pool);

  /* call next method */
  superclass = BUFFER_SUPERCLASS(SNCBufClass);
  res = (*superclass->init)(buffer, pool, args);
  if (res != ResOK)
    return res;

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
  SNC snc;
  Pool pool;

  AVERT(Buffer, buffer);
  sncbuf = BufferSNCBuf(buffer);
  AVERT(SNCBuf, sncbuf);
  pool = BufferPool(buffer);

  snc = PoolSNC(pool);
  /* Put any segments which haven't bee popped onto the free list */
  sncPopPartialSegChain(snc, buffer, NULL);

  sncbuf->sig = SigInvalid;

  /* finish the superclass fields last */
  super = BUFFER_SUPERCLASS(SNCBufClass);
  super->finish(buffer);
}


/* SNCBufClass -- The class definition */

DEFINE_BUFFER_CLASS(SNCBufClass, class)
{
  INHERIT_CLASS(class, RankBufClass);
  class->name = "SNCBUF";
  class->size = sizeof(SNCBufStruct);
  class->init = SNCBufInit;
  class->finish = SNCBufFinish;
  AVERT(BufferClass, class);
}



/* SNCSegStruct -- SNC segment subclass
 *
 * This subclass of GCSeg links segments in chains.
 */

#define SNCSegSig ((Sig)0x51954C59)    /* SIGSNCSeG */

typedef struct SNCSegStruct *SNCSeg;

typedef struct SNCSegStruct {
  GCSegStruct gcSegStruct;  /* superclass fields must come first */
  SNCSeg next;              /* Next segment in chain, or NULL */
  Sig sig;
} SNCSegStruct;

#define SegSNCSeg(seg)             ((SNCSeg)(seg))
#define SNCSegSeg(sncseg)          ((Seg)(sncseg))

#define sncSegNext(seg) \
  (SNCSegSeg(SegSNCSeg(seg)->next))

#define sncSegSetNext(seg, nextseg) \
  ((void)(SegSNCSeg(seg)->next = SegSNCSeg(nextseg)))

ATTRIBUTE_UNUSED
static Bool SNCSegCheck(SNCSeg sncseg)
{
  CHECKS(SNCSeg, sncseg);
  CHECKD(GCSeg, &sncseg->gcSegStruct);
  if (NULL != sncseg->next) {
    CHECKS(SNCSeg, sncseg->next);
  }
  return TRUE;
}


/* sncSegInit -- Init method for SNC segments */

static Res sncSegInit(Seg seg, Pool pool, Addr base, Size size,
                      Bool reservoirPermit, ArgList args)
{
  SegClass super;
  SNCSeg sncseg;
  Res res;

  AVERT(Seg, seg);
  sncseg = SegSNCSeg(seg);
  AVERT(Pool, pool);
  /* no useful checks for base and size */
  AVERT(Bool, reservoirPermit);

  /* Initialize the superclass fields first via next-method call */
  super = SEG_SUPERCLASS(SNCSegClass);
  res = super->init(seg, pool, base, size, reservoirPermit, args);
  if (res != ResOK)
    return res;

  sncseg->next = NULL;
  sncseg->sig = SNCSegSig;
  AVERT(SNCSeg, sncseg);
  return ResOK;
}


/* SNCSegClass -- Class definition for SNC segments */

DEFINE_SEG_CLASS(SNCSegClass, class)
{
  INHERIT_CLASS(class, GCSegClass);
  SegClassMixInNoSplitMerge(class);  /* no support for this (yet) */
  class->name = "SNCSEG";
  class->size = sizeof(SNCSegStruct);
  class->init = sncSegInit;
  AVERT(SegClass, class);
}


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


/* SNCVarargs -- decode obsolete varargs */

static void SNCVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_FORMAT;
  args[0].val.format = va_arg(varargs, Format);
  args[1].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}


/* SNCInit -- initialize an SNC pool */

static Res SNCInit(Pool pool, ArgList args)
{
  SNC snc;
  Format format;
  ArgStruct arg;

  /* weak check, as half-way through initialization */
  AVER(pool != NULL);

  snc = PoolSNC(pool);

  ArgRequire(&arg, args, MPS_KEY_FORMAT);
  format = arg.val.format;

  AVERT(Format, format);
  pool->format = format;
  snc->freeSegs = NULL;
  snc->sig = SNCSig;

  AVERT(SNC, snc);
  EVENT2(PoolInitSNC, pool, format);
  return ResOK;
}


/* SNCFinish -- finish an SNC pool */

static void SNCFinish(Pool pool)
{
  SNC snc;
  Ring ring, node, nextNode;

  AVERT(Pool, pool);
  snc = PoolSNC(pool);
  AVERT(SNC, snc);

  ring = &pool->segRing;
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    AVERT(Seg, seg);
    SegFree(seg);
  }
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
  AVERT(Bool, withReservoirPermit);
  AVER(BufferIsReset(buffer));

  snc = PoolSNC(pool);
  AVERT(SNC, snc);

  /* Try to find a free segment with enough space already */
  if (sncFindFreeSeg(&seg, snc, size)) {
    goto found;
  }

  /* No free seg, so create a new one */
  arena = PoolArena(pool);
  asize = SizeArenaGrains(size, arena);
  res = SegAlloc(&seg, SNCSegClassGet(), LocusPrefDefault(),
                 asize, pool, withReservoirPermit, argsNone);
  if (res != ResOK)
    return res;

found:
  /* <design/seg/#field.rankSet.start> */
  if (BufferRankSet(buffer) == RankSetEMPTY)
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetEMPTY);
  else
    SegSetRankAndSummary(seg, BufferRankSet(buffer), RefSetUNIV);

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
  snc = PoolSNC(pool);
  AVERT(SNC, snc);
  AVER(BufferFrameState(buffer) == BufferFrameVALID);
  /* .lw-frame-state */
  BufferFrameSetState(buffer, BufferFrameDISABLED);

  arena = BufferArena(buffer);

  /* Pad the end unused space at the end of the segment */
  size = AddrOffset(init, limit);
  if (size > 0) {
    ShieldExpose(arena, seg);
    (*pool->format->pad)(init, size);
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
  snc = PoolSNC(pool);
  AVERT(SNC, snc);

  format = pool->format;
  base = SegBase(seg);
   
  /* If the segment is buffered, only walk as far as the end */
  /* of the initialized objects.  */
  if (SegBuffer(seg) != NULL) {
    limit = BufferScanLimit(SegBuffer(seg));
  } else {
    limit = SegLimit(seg);
  }
 
  if (base < limit) {
    res = (*format->scan)(&ss->ss_s, base, limit);
    if (res != ResOK) {
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
  snc = PoolSNC(pool);
  AVERT(SNC, snc);

  AVER(BufferFrameState(buf) == BufferFrameVALID);
 
  if (frame == NULL) {
    /* corresponds to a pop to bottom of stack. .lw-frame-null */
    BufferDetach(buf, pool);
    sncPopPartialSegChain(snc, buf, NULL);

  } else {
    Arena arena;
    Seg seg = NULL;     /* suppress "may be used uninitialized" */
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


static void SNCWalk(Pool pool, Seg seg, FormattedObjectsVisitor f,
                    void *p, size_t s)
{
  AVERT(Pool, pool);
  AVERT(Seg, seg);
  AVER(FUNCHECK(f));
  /* p and s are arbitrary closures and can't be checked */

  /* Avoid applying the function to grey objects. */
  /* They may have pointers to old-space. */
  if (SegGrey(seg) == TraceSetEMPTY) {
    Addr object = SegBase(seg);
    Addr nextObject;
    Addr limit;
    SNC snc;
    Format format;

    snc = PoolSNC(pool);
    AVERT(SNC, snc);
    format = pool->format;

    /* If the segment is buffered, only walk as far as the end */
    /* of the initialized objects.  Cf. SNCScan. */
    if (SegBuffer(seg) != NULL)
      limit = BufferScanLimit(SegBuffer(seg));
    else
      limit = SegLimit(seg);

    while(object < limit) {
      (*f)(object, format, pool, p, s);
      nextObject = (*format->skip)(object);
      AVER(nextObject > object);
      object = nextObject;
    }
    AVER(object == limit);
  }
}


/* SNCTotalSize -- total memory allocated from the arena */

static Size SNCTotalSize(Pool pool)
{
  SNC snc;
  Ring ring, node, nextNode;
  Size total = 0;

  AVERT(Pool, pool);
  snc = PoolSNC(pool);
  AVERT(SNC, snc);

  ring = &pool->segRing;
  RING_FOR(node, ring, nextNode) {
    Seg seg = SegOfPoolRing(node);
    AVERT(Seg, seg);
    total += SegSize(seg);
  }

  return total;
}


/* SNCFreeSize -- free memory (unused by client program) */

static Size SNCFreeSize(Pool pool)
{
  SNC snc;
  Seg seg;
  Size free = 0;

  AVERT(Pool, pool);
  snc = PoolSNC(pool);
  AVERT(SNC, snc);

  seg = snc->freeSegs;
  while (seg != NULL) {
    AVERT(Seg, seg);
    free += SegSize(seg);
    seg = sncSegNext(seg);
  }

  return free;
}


/* SNCPoolClass -- the class definition */

DEFINE_POOL_CLASS(SNCPoolClass, this)
{
  INHERIT_CLASS(this, AbstractScanPoolClass);
  PoolClassMixInFormat(this);
  this->name = "SNC";
  this->size = sizeof(SNCStruct);
  this->offset = offsetof(SNCStruct, poolStruct);
  this->varargs = SNCVarargs;
  this->init = SNCInit;
  this->finish = SNCFinish;
  this->bufferFill = SNCBufferFill;
  this->bufferEmpty = SNCBufferEmpty;
  this->scan = SNCScan;
  this->framePush = SNCFramePush;
  this->framePop = SNCFramePop;
  this->framePopPending = SNCFramePopPending;
  this->walk = SNCWalk;
  this->bufferClass = SNCBufClassGet;
  this->totalSize = SNCTotalSize;
  this->freeSize = SNCFreeSize;
  AVERT(PoolClass, this);
}


mps_pool_class_t mps_class_snc(void)
{
  return (mps_pool_class_t)SNCPoolClassGet();
}


/* SNCCheck -- Check an SNC pool */

ATTRIBUTE_UNUSED
static Bool SNCCheck(SNC snc)
{
  CHECKS(SNC, snc);
  CHECKD(Pool, SNCPool(snc));
  CHECKL(SNCPool(snc)->class == SNCPoolClassGet());
  if (snc->freeSegs != NULL) {
    CHECKD(Seg, snc->freeSegs);
  }
  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

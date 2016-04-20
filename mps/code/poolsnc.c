/* poolsnc.c: STACK NO CHECKING POOL CLASS
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
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

typedef SNC SNCPool;
#define SNCPoolCheck SNCCheck
DECLARE_CLASS(Pool, SNCPool, AbstractScanPool);

DECLARE_CLASS(Seg, SNCSeg, GCSeg);
DECLARE_CLASS(Buffer, SNCBuf, RankBuf);
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


/* SNCBufCheck -- check consistency of an SNCBuf */

ATTRIBUTE_UNUSED
static Bool SNCBufCheck(SNCBuf sncbuf)
{
  SegBuf segbuf = MustBeA(SegBuf, sncbuf);
  CHECKS(SNCBuf, sncbuf);
  CHECKD(SegBuf, segbuf);
  if (sncbuf->topseg != NULL) {
    CHECKD(Seg, sncbuf->topseg);
  }
  return TRUE;
}


/* sncBufferTopSeg -- return the head of segment chain from an SNCBuf */

static Seg sncBufferTopSeg(Buffer buffer)
{
  SNCBuf sncbuf = MustBeA(SNCBuf, buffer);
  return sncbuf->topseg;
}


/* sncBufferSetTopSeg -- set the head of segment chain from an SNCBuf */

static void sncBufferSetTopSeg(Buffer buffer, Seg seg)
{
  SNCBuf sncbuf = MustBeA(SNCBuf, buffer);
  if (NULL != seg)
    AVERT(Seg, seg);
  sncbuf->topseg = seg;
}


/* SNCBufInit -- Initialize an SNCBuf */

static Res SNCBufInit(Buffer buffer, Pool pool, Bool isMutator, ArgList args)
{
  SNCBuf sncbuf;
  Res res;

  /* call next method */
  res = NextMethod(Buffer, SNCBuf, init)(buffer, pool, isMutator, args);
  if (res != ResOK)
    return res;
  sncbuf = CouldBeA(SNCBuf, buffer);

  sncbuf->topseg = NULL;

  SetClassOfPoly(buffer, CLASS(SNCBuf));
  sncbuf->sig = SNCBufSig;
  AVERC(SNCBuf, sncbuf);

  return ResOK;
}


/* SNCBufFinish -- Finish an SNCBuf */

static void SNCBufFinish(Buffer buffer)
{
  SNCBuf sncbuf = MustBeA(SNCBuf, buffer);
  SNC snc = MustBeA(SNCPool, BufferPool(buffer));

  /* Put any segments which haven't been popped onto the free list */
  sncPopPartialSegChain(snc, buffer, NULL);

  sncbuf->sig = SigInvalid;

  NextMethod(Buffer, SNCBuf, finish)(buffer);
}


/* SNCBufClass -- The class definition */

DEFINE_CLASS(Buffer, SNCBuf, klass)
{
  INHERIT_CLASS(klass, SNCBuf, RankBuf);
  klass->size = sizeof(SNCBufStruct);
  klass->init = SNCBufInit;
  klass->finish = SNCBufFinish;
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

#define sncSegNext(seg) RVALUE(SNCSegSeg(SegSNCSeg(seg)->next))
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

static Res sncSegInit(Seg seg, Pool pool, Addr base, Size size, ArgList args)
{
  SNCSeg sncseg;
  Res res;

  /* Initialize the superclass fields first via next-method call */
  res = NextMethod(Seg, SNCSeg, init)(seg, pool, base, size, args);
  if (res != ResOK)
    return res;
  sncseg = CouldBeA(SNCSeg, seg);

  AVERT(Pool, pool);
  /* no useful checks for base and size */

  sncseg->next = NULL;

  SetClassOfPoly(seg, CLASS(SNCSeg));
  sncseg->sig = SNCSegSig;
  AVERC(SNCSeg, sncseg);

  return ResOK;
}


/* SNCSegClass -- Class definition for SNC segments */

DEFINE_CLASS(Seg, SNCSeg, klass)
{
  INHERIT_CLASS(klass, SNCSeg, GCSeg);
  SegClassMixInNoSplitMerge(klass);  /* no support for this (yet) */
  klass->size = sizeof(SNCSegStruct);
  klass->init = sncSegInit;
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

static void sncRecordFreeSeg(Arena arena, SNC snc, Seg seg)
{
  AVERT(SNC, snc);
  AVERT(Seg, seg);
  AVER(sncSegNext(seg) == NULL);

  /* Make sure it's not grey, and set to RankSetEMPTY */
  /* This means it won't be scanned */
  SegSetGrey(seg, TraceSetEMPTY);
  SegSetRankAndSummary(seg, RankSetEMPTY, RefSetEMPTY);

  /* Pad the whole segment so we don't try to walk it. */
  ShieldExpose(arena, seg);
  (*SNCPool(snc)->format->pad)(SegBase(seg), SegSize(seg));
  ShieldCover(arena, seg);

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
    sncRecordFreeSeg(BufferArena(buf), snc, free);
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

static Res SNCInit(Pool pool, Arena arena, PoolClass klass, ArgList args)
{
  SNC snc;
  Res res;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(ArgList, args);
  UNUSED(klass); /* used for debug pools only */

  res = PoolAbsInit(pool, arena, klass, args);
  if (res != ResOK)
    return res;
  snc = CouldBeA(SNCPool, pool);

  /* Ensure a format was supplied in the argument list. */
  AVER(pool->format != NULL);

  pool->alignment = pool->format->alignment;
  snc->freeSegs = NULL;

  SetClassOfPoly(pool, CLASS(SNCPool));
  snc->sig = SNCSig;
  AVERC(SNCPool, snc);

  EVENT2(PoolInitSNC, pool, pool->format);

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

  PoolAbsFinish(pool);
}


static Res SNCBufferFill(Addr *baseReturn, Addr *limitReturn,
                         Pool pool, Buffer buffer, Size size)
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
  res = SegAlloc(&seg, CLASS(SNCSeg), LocusPrefDefault(),
                 asize, pool, argsNone);
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

  arena = BufferArena(buffer);

  /* Pad the unused space at the end of the segment */
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
  limit = SegBufferScanLimit(seg);
 
  if (base < limit) {
    res = FormatScan(format, ss, base, limit);
    if (res != ResOK) {
      *totalReturn = FALSE;
      return res;
    }
  } else {
    AVER(base == limit);
  }

  *totalReturn = TRUE;
  return ResOK;
}



static Res SNCFramePush(AllocFrame *frameReturn, Pool pool, Buffer buf)
{
  AVER(frameReturn != NULL);
  AVERT(Pool, pool);
  AVERT(Buffer, buf);

  if (BufferIsReset(buf)) {
    AVER(sncBufferTopSeg(buf) == NULL);  /* The stack must be empty  */
    /* Use NULL to indicate an empty stack. .lw-frame-null */
    *frameReturn = NULL;
  } else if (BufferGetInit(buf) < SegLimit(BufferSeg(buf))) {
    /* Frame pointer is limit of initialized objects in buffer. */
    *frameReturn = (AllocFrame)BufferGetInit(buf);
  } else {
    /* Can't use the limit of initialized objects as the frame pointer
     * because it's not in the segment (see job003882). Instead, refill
     * the buffer and put the frame pointer at the beginning. */
    Res res;
    Addr base, limit;
    BufferDetach(buf, pool);
    res = SNCBufferFill(&base, &limit, pool, buf, PoolAlignment(pool));
    if (res != ResOK)
      return res;
    BufferAttach(buf, base, limit, base, 0);
    AVER(BufferGetInit(buf) < SegLimit(BufferSeg(buf)));    
    *frameReturn = (AllocFrame)BufferGetInit(buf);
  }
  return ResOK;
}


static Res SNCFramePop(Pool pool, Buffer buf, AllocFrame frame)
{
  Addr addr;
  SNC snc;
  AVERT(Pool, pool);
  AVERT(Buffer, buf);
  /* frame is an Addr and can't be directly checked */
  snc = PoolSNC(pool);
  AVERT(SNC, snc);
 
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
    AVER(SegPool(seg) == pool);

    if (SegBuffer(seg) == buf) {
      /* don't need to change the segment - just the alloc pointers */
      AVER(addr <= BufferScanLimit(buf));  /* check direction of pop */
      BufferSetAllocAddr(buf, addr);
    } else {
      /* need to change segment  */
      BufferDetach(buf, pool);
      sncPopPartialSegChain(snc, buf, seg);
      BufferAttach(buf, SegBase(seg), SegLimit(seg), addr, (Size)0);
    }
  }

  return ResOK;
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
    limit = SegBufferScanLimit(seg);

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

DEFINE_CLASS(Pool, SNCPool, klass)
{
  INHERIT_CLASS(klass, SNCPool, AbstractScanPool);
  PoolClassMixInFormat(klass);
  klass->size = sizeof(SNCStruct);
  klass->varargs = SNCVarargs;
  klass->init = SNCInit;
  klass->finish = SNCFinish;
  klass->bufferFill = SNCBufferFill;
  klass->bufferEmpty = SNCBufferEmpty;
  klass->scan = SNCScan;
  klass->framePush = SNCFramePush;
  klass->framePop = SNCFramePop;
  klass->walk = SNCWalk;
  klass->bufferClass = SNCBufClassGet;
  klass->totalSize = SNCTotalSize;
  klass->freeSize = SNCFreeSize;
}


mps_pool_class_t mps_class_snc(void)
{
  return (mps_pool_class_t)CLASS(SNCPool);
}


/* SNCCheck -- Check an SNC pool */

ATTRIBUTE_UNUSED
static Bool SNCCheck(SNC snc)
{
  CHECKS(SNC, snc);
  CHECKC(SNCPool, snc);
  CHECKD(Pool, SNCPool(snc));
  if (snc->freeSegs != NULL) {
    CHECKD(Seg, snc->freeSegs);
  }
  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

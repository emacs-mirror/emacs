/* impl.c.poolmv: MANUAL VARIABLE POOL
 *
 * $HopeName: MMsrc!poolmv.c(trunk.15) $
 * Copyright (C) 1994, 1995 Harlequin Group, all rights reserved
 *
 * **** RESTRICTION: This pool may not allocate from the arena control
 *                   pool, since it is used to implement that pool.
 *                   It may call PoolCreate, which allocates from the
 *                   poolPool.
 *
 * An observation: Freeing memory introduces more information
 * into the system than allocating it.  This causes the problem described
 * in note 2.
 *
 * Notes
 *  1. Need to measure typical fragmentation levels and adjust the
 *     blockExtendBy parameter appropriately.  richard 1994-11-08
 *  2. free can lose memory if it can't allocate a block descriptor.  The
 *     memory could be pushed onto a special chain to be reclaimed later.
 *     richard 1994-11-09
 *  3. The span chain could be adaptive.  richard 1994-11-09
 *  4. Spans with no blocks could be freed.  richard 1994-11-09
 *  5. An MFS pool for the block descriptors is justified, but not really
 *     for the spans, which are much rarer. richard 1994-11-09
 *  7. Check should check pointer destinations are in the right pools.
 *     richard 1994-11-10
 *  8. By changing MVSpanAlloc it might be possible to keep track of all
 *     allocated blocks using descriptors, for debugging purposes.  richard
 *     1994-11-10
 *  9. (See note 7.) Check methods can't easily get hold of the relevant
 *     pools in ordr to check pointers using PoolAddrPool.
 *     1995-01-19 drj
 */

#include "mpm.h"
#include "poolmv.h"
#include "poolmfs.h"
#include "mpscmv.h"

SRCID(poolmv, "$HopeName: MMsrc!poolmv.c(trunk.15) $");


#define BLOCKPOOL(mv)   (MFSPool(&(mv)->blockPoolStruct))
#define SPANPOOL(mv)    (MFSPool(&(mv)->spanPoolStruct))
#define PoolPoolMV(pool)        PARENT(MVStruct, poolStruct, pool)


/*  == Class Structure ==  */

#if 0
static Res MVInit(Pool pool, va_list arg);
static void MVFinish(Pool pool);
static Res MVAlloc(Addr *pReturn, Pool pool, Size size);
static void MVFree(Pool pool, Addr old, Size size);
static Res MVDescribe(Pool pool, mps_lib_FILE *stream);
#endif /* 0 */


/* MVBlockStruct -- block structure
 *
 * The pool maintains a descriptor structure for each contiguous
 * allocated block of memory it manages.  The descriptor is on a simple
 * linked-list of such descriptors, which is in ascending order of
 * address.
 */

typedef struct MVBlockStruct *MVBlock;
typedef struct MVBlockStruct {
  MVBlock next;
  Addr base, limit;
} MVBlockStruct;


/* MVBlockCheck -- check the consistency of a block structure */

static Bool MVBlockCheck(MVBlock block)
{
  AVER(block != NULL);
  AVER(block->limit >= block->base);
  /* Check that it is in the block pool.  See note 7. */
  /* This turns out to be considerably tricky, as we cannot get hold
   * of the blockPool (pool is not a parameter). */
  return TRUE;
}


/* MVSpanStruct -- span structure
 *
 * The pool maintains a wrapper for each allocated segment which
 * contains a chain of descriptors for the allocated memory in that
 * segment.  It also contains sentinel block descriptors which mark the
 * start and end of the span.  These blocks considerably simplify
 * allocation, and may be zero-sized.
 */

typedef struct MVSpanStruct *MVSpan;
typedef struct MVSpanStruct {
  RingStruct spans;		/* all the spans */ 
  MV mv;                        /* owning MV pool */
  Seg seg;                      /* segment underlying the span */
  MVBlockStruct base;           /* sentinel at base of span */
  MVBlockStruct limit;          /* sentinel at limit of span */
  MVBlock blocks;               /* allocated blocks */
  Size space;                   /* total free space in segment */
  unsigned blockCount;          /* number of blocks on chain */
} MVSpanStruct;


Pool MVPool(MV mv)
{
  AVERT(MV, mv);
  return &mv->poolStruct;
}


/* MVSpanCheck -- check the consistency of a span structure */

static Bool MVSpanCheck(MVSpan span)
{
  CHECKL(span != NULL);
  CHECKU(MV, span->mv);
  CHECKL(RingCheck(&span->spans));
  CHECKL(MVBlockCheck(&span->base));
  CHECKL(MVBlockCheck(&span->limit));
  /* The block chain starts with the base sentinel. */
  CHECKL(span->blocks == &span->base);
  /* Since there is a limit sentinel, the chain can't end just after the */
  /* base sentinel... */
  CHECKL(span->base.next != NULL);
  /* ...and it's sure to have at least two blocks on it. */
  CHECKL(span->blockCount >= 2);
  /* This is just defined this way.  It shouldn't change. */
  CHECKL(span->limit.next == NULL);
  /* The sentinels should mark the ends of the segment. */
  CHECKL(span->base.base == SegBase(PoolSpace(MVPool(span->mv)), span->seg));
  CHECKL(span->limit.limit == SegLimit(PoolSpace(MVPool(span->mv)), span->seg));
  /* The sentinels mustn't overlap. */
  CHECKL(span->base.limit <= span->limit.base);
  /* The remaining space can't be more than the gap between the sentinels. */
  CHECKL(span->space <= AddrOffset(span->base.limit, span->limit.base));
  /* Check that it is in the span pool.  See note 7. */
  return TRUE;
}


static Res MVInit(Pool pool, va_list arg)
{
  Size extendBy, avgSize, maxSize, blockExtendBy, spanExtendBy;
  MV mv;
  Space space;
  Res res;

  extendBy = va_arg(arg, Size);
  avgSize = va_arg(arg, Size);
  maxSize = va_arg(arg, Size);

  AVER(extendBy > 0);
  AVER(avgSize > 0);
  AVER(avgSize <= extendBy);
  AVER(maxSize > 0);
  AVER(extendBy <= maxSize);

  mv = PoolPoolMV(pool);
  space = PoolSpace(pool);

  /* At 100% fragmentation we will need one block descriptor for every other */
  /* allocated block, or (extendBy/avgSize)/2 descriptors.  See note 1. */
  blockExtendBy = sizeof(MVBlockStruct) * (extendBy/avgSize)/2;

  res = PoolInit(&mv->blockPoolStruct.poolStruct, space, PoolClassMFS(),
                 blockExtendBy, sizeof(MVBlockStruct));
  if(res != ResOK)
    return res;

  spanExtendBy = sizeof(MVSpanStruct) * (maxSize/extendBy);

  res = PoolInit(&mv->spanPoolStruct.poolStruct, space, PoolClassMFS(),
                 spanExtendBy, sizeof(MVSpanStruct));
  if(res != ResOK)
    return res;

  mv->extendBy = extendBy;
  mv->avgSize  = avgSize;
  mv->maxSize  = maxSize;
  RingInit(&mv->spans);
  mv->space = 0;
  mv->lost = 0;
  mv->sig = MVSig;

  AVERT(MV, mv);

  return ResOK;
}


static void MVFinish(Pool pool)
{
  MV mv;
  Ring spans, node = NULL; /* gcc whinge stop */
  MVSpan span;

  AVERT(Pool, pool);
  mv = PoolPoolMV(pool);
  AVERT(MV, mv);

  /* Destroy all the segments attached to the pool. */
  spans = &mv->spans;
  RING_FOR(node, spans) {
    span = RING_ELT(MVSpan, spans, node);
    AVERT(MVSpan, span);
    PoolSegFree(pool, span->seg);
  }

  mv->sig = SigInvalid;

  PoolFinish(&mv->blockPoolStruct.poolStruct);
  PoolFinish(&mv->spanPoolStruct.poolStruct);
}


/* MVSpanAlloc -- allocate space from a span of memory
 *
 * MVSpanAlloc searches a span for a free block of the requested size.  If it
 * finds one it allocates it from the span, updates *addrReturn to point
 * to it, and returns TRUE.
 */

static Bool MVSpanAlloc(Addr *addrReturn, MVSpan span, Size size,
                        Pool blockPool)
{
  Size gap;
  MVBlock block;

  AVERT(MVSpan, span);
  AVER(size > 0);
  AVER(addrReturn != NULL);

  block = span->blocks;
  AVER(block == &span->base);   /* should be the base sentinel */

  /* We're guaranteed at least one gap between sentinels, and therefore at */
  /* least one iteration of this loop.  So, the test is at the end.  */
  do {
    AVER(block->next != NULL);

    gap = AddrOffset(block->limit, block->next->base);

    if(gap >= size) {
      Addr new = block->limit;

      /* If the gap is exactly the right size then the preceeding and */
      /* following blocks can be merged, into the preceeding one, */
      /* unless the following block is the end sentinel. */
      if(gap == size && block->next != &span->limit) {
        MVBlock old = block->next;
        block->limit = old->limit;
        block->next = old->next;
        PoolFree(blockPool, (Addr)old, sizeof(MVBlockStruct));
        --span->blockCount;
      } else
        block->limit = AddrAdd(block->limit, size);

      span->space -= size;
      *addrReturn = new;
      return TRUE;
    }

    block = block->next;
  }
  while(block->next != NULL);

  return FALSE;
}


/* MVSpanFree -- free an area in a span of memory
 *
 * Searches a span for a block which contains the area specified by the
 * base and limit, and frees it within that span.  This may involve
 * allocating a block descriptor, which may fail, in which case an error is
 * returned.
 */

static Res MVSpanFree(MVSpan span, Addr base, Addr limit, Pool blockPool)
{
  MVBlock *prev, block;

  AVERT(MVSpan, span);
  AVER(span->base.base <= base && limit <= span->limit.limit);
  AVERT(Pool, blockPool);

  prev = &span->blocks;
  block = span->blocks;
  AVER(block == &span->base); /* should be base sentinel */
  do {
    int isBase = block == &span->base;
    int isLimit = block == &span->limit;
    int isSentinel = isBase || isLimit;

    AVERT(MVBlock, block);

    /* Is the freed area within the block? */
    if(block->base <= base && limit <= block->limit) {
      if(!isSentinel && block->base == base && limit == block->limit) {
        AVER(block->next != NULL); /* should at least be a sentinel */
        *prev = block->next;
        PoolFree(blockPool, (Addr)block, sizeof(MVBlockStruct));
        --span->blockCount;
      } else if(!isBase && block->base == base)
        block->base = limit;
      else if(!isLimit && limit == block->limit)
        block->limit = base;
      else {
        Res res;
        MVBlock new;

        /* The freed area is buried in the middle of the block, so the */
        /* block must be split into two parts.  */
        res = PoolAlloc((Addr *)&new, blockPool, sizeof(MVBlockStruct));
        if(res != ResOK) return res;

        /* If the freed area is in the base sentinel then insert the new */
        /* descriptor after it, otherwise insert before. */
        if(isBase) {
          new->base = limit;
          new->limit = block->limit;
          block->limit = base;
          new->next = block->next;
          AVER(new->next != NULL); /* should at least be a sentinel */
          block->next = new;
        } else {
          new->base = block->base;
          new->limit = base;
          block->base = limit;
          new->next = block;
          *prev = new;
        }

        AVERT(MVBlock, new);
        ++span->blockCount;
      }

      AVERT(MVBlock, block);

      span->space += AddrOffset(base, limit);

      return ResOK;
    }

    prev = &block->next;
    block = block->next;
  }
  while(block != NULL);

  /* The freed area is in the span, but not within a block. */
  NOTREACHED;

  return ResOK;
}


/*  == Allocate ==  */

static Res MVAlloc(Addr *pReturn, Pool pool, Size size)
{
  Res res;
  MVSpan span;
  Space space;
  MV mv;
  Size segSize;
  Ring spans, node = NULL; /* gcc whinge stop */

  AVERT(Pool, pool);
  mv = PoolPoolMV(pool);
  AVERT(MV, mv);

  AVER(pReturn != NULL);
  AVER(size > 0);

  size = SizeAlignUp(size, pool->alignment);

  if(size <= mv->space) {
    spans = &mv->spans;
    RING_FOR(node, spans) {
      span = RING_ELT(MVSpan, spans, node);
      if(size <= span->space) {
        Addr new;

        if(MVSpanAlloc(&new, span, size, BLOCKPOOL(mv))) {
          mv->space -= size;
          AVER(AddrIsAligned(new, pool->alignment));
          *pReturn = new;
          return ResOK;
        }
      }
    }
  }

  /* There is no block large enough in any of the spans, so extend the */
  /* pool with a new segment which will hold the requested allocation. */
  /* Allocate a new span descriptor and initialize it to point at the */
  /* segment. */
  res = PoolAlloc((Addr *)&span, SPANPOOL(mv), sizeof(MVSpanStruct));
  if(res != ResOK)
    return res;

  if(size <= mv->extendBy)
    segSize = mv->extendBy;
  else
    segSize = size;

  space = PoolSpace(pool);
  segSize = SizeAlignUp(segSize, ArenaAlign(space));

  res = PoolSegAlloc(&span->seg, pool, segSize);
  if(res != ResOK) {
    PoolFree(SPANPOOL(mv), (Addr)span, sizeof(MVSpanStruct));
    return res;
  }

  span->mv = mv;
  span->seg->p = (void *)span;
  RingInit(&span->spans);
  span->base.base = span->base.limit = SegBase(space, span->seg);
  span->limit.base = span->limit.limit = SegLimit(space, span->seg);
  span->space = AddrOffset(span->base.limit, span->limit.base);
  span->limit.next = NULL;
  span->base.next = &span->limit;
  span->blocks = &span->base;
  span->blockCount = 2;

  span->base.limit = AddrAdd(span->base.limit, size);
  span->space -= size;

  AVERT(MVSpan, span);

  mv->space += span->space;
  RingAppend(&mv->spans, &span->spans);

  *pReturn = span->base.base;
  return ResOK;
}


static void MVFree(Pool pool, Addr old, Size size)
{
  Addr base, limit;
  MVSpan span;
  MV mv;
  Res res;
  Bool b;
  Seg seg;

  AVERT(Pool, pool);
  mv = PoolPoolMV(pool);
  AVERT(MV, mv);

  AVER(old != (Addr)0);
  AVER(size > 0);

  size = SizeAlignUp(size, pool->alignment);
  base = old;
  limit = AddrAdd(base, size);

  /* Map the pointer onto the segment which contains it, and thence */
  /* onto the span. */
  b = SegOfAddr(&seg, PoolSpace(pool), old);
  AVER(b);
  span = (MVSpan)seg->p;
  AVERT(MVSpan, span);

  /* the to be freed area should be within the span just found */
  AVER(span->base.base <= base && limit <= span->limit.limit);

  /* Unfortunately, if allocating the new block descriptor fails we */
  /* can't do anything, and the memory is lost.  See note 2. */
  res = MVSpanFree(span, base, limit, BLOCKPOOL(mv));
  if(res != ResOK)
    mv->lost += size;
  else
    mv->space += size;
  
  /* free space should be less than total space */
  AVER(AddrAdd(span->base.base, span->space) <= span->limit.limit);
  if(AddrAdd(span->base.base, span->space) == span->limit.limit) {
    AVER(span->blockCount == 2);
    /* both blocks are the trivial sentinel blocks */
    AVER(span->base.limit == span->base.base);
    AVER(span->limit.limit == span->limit.base);
    PoolSegFree(pool, span->seg);
    RingRemove(&span->spans);
    PoolFree(SPANPOOL(mv), (Addr)span, sizeof(MVSpanStruct));
  }

  return;                   /* should free spans.  See note 4. */
}


static Res MVDescribe(Pool pool, mps_lib_FILE *stream)
{
  Res res;
  MV mv;
  MVSpan span;
  Align step;
  Size length;
  char c;
  Ring spans, node = NULL; /* gcc whinge stop */

  AVERT(Pool, pool);
  mv = PoolPoolMV(pool);
  AVERT(MV, mv);

  AVER(stream != NULL);

  res = WriteF(stream,
               "  blockPool $P ($U)\n",
               (WriteFP)BLOCKPOOL(mv), (WriteFU)BLOCKPOOL(mv)->serial,
               "  spanPool  $P ($U)\n",
               (WriteFP)SPANPOOL(mv), (WriteFU)SPANPOOL(mv)->serial,
               "  extendBy  $W\n",  (WriteFW)mv->extendBy,
               "  avgSize   $W\n",  (WriteFW)mv->avgSize,
               "  maxSize   $W\n",  (WriteFW)mv->maxSize,
               "  space     $P\n",  (WriteFP)mv->space,
               NULL);
  if(res != ResOK) return res;               

  res = WriteF(stream, "  Spans\n", NULL);
  if(res != ResOK) return res;

  spans = &mv->spans;
  RING_FOR(node, spans) {
    span = RING_ELT(MVSpan, spans, node);
    AVERT(MVSpan, span);

    res = WriteF(stream,
                 "    span $P",   (WriteFP)span,
                 "  seg $P",      (WriteFP)span->seg,
                 "  space $W",    (WriteFW)span->space,
                 "  blocks $U\n", (WriteFU)span->blockCount,
                 NULL);
    if(res != ResOK) return res;
  }

  res = WriteF(stream, "  Span allocation maps\n", NULL);
  if(res != ResOK) return res;

  step = pool->alignment;
  length = 0x40 * step;

  spans = &mv->spans;
  RING_FOR(node, spans) {
    Addr i, j;
    MVBlock block;
    span = RING_ELT(MVSpan, spans, node);
    res = WriteF(stream, "    MVSpan $P\n", (WriteFP)span, NULL);
    if(res != ResOK) return res;

    block = span->blocks;
    AVER(block == &span->base); /* should be start sentinel */

    for(i = span->base.base; i < span->limit.limit; i = AddrAdd(i, length)) {
      res = WriteF(stream, "    $A ", i, NULL);
      if(res != ResOK) return res;

      for(j = i;
          j < AddrAdd(i, length) && j < span->limit.limit;
          j = AddrAdd(j, step)) {

        if(j == block->base) {
          if(AddrAdd(j, step) == block->limit)
            c = '@';
          else
            c = '[';
        } else if(AddrAdd(j, step) == block->limit)
          c = ']';
        else if(j > block->base && j < block->limit)
          c = '=';
        else
          c = '.';

        if(j >= block->limit) {
          block = block->next;
          AVER(block != NULL);  /* shouldn't pass limit sentinel */
        }
        
        res = WriteF(stream, "$C", c, NULL);
        if(res != ResOK) return res;
      }
      res = WriteF(stream, "\n", NULL);
      if(res != ResOK) return res;
    }
  }

  return ResOK;
}


static PoolClassStruct PoolClassMVStruct = {
  PoolClassSig,
  "MV",                                 /* name */
  sizeof(MVStruct),                     /* size */
  offsetof(MVStruct, poolStruct),       /* offset */
  AttrALLOC | AttrFREE,                 /* attr */
  MVInit,                               /* init */
  MVFinish,                             /* finish */
  MVAlloc,                              /* alloc */
  MVFree,                               /* free */
  PoolNoBufferInit,                     /* bufferInit */
  PoolNoBufferFinish,                   /* bufferFinish */
  PoolNoBufferFill,                     /* bufferFill */
  PoolNoBufferTrip,                     /* bufferTrip */
  PoolNoBufferExpose,                   /* bufferExpose */
  PoolNoBufferCover,                    /* bufferCover */
  PoolNoCondemn,                        /* condemn */
  PoolNoGrey,                           /* mark */
  PoolNoScan,                           /* scan */
  PoolNoFix,                            /* fix */
  PoolNoReclaim,                        /* relcaim */
  PoolNoAccess,                         /* access */
  MVDescribe,                           /* describe */
  PoolClassSig                          /* impl.h.mpmst.class.end-sig */
};

PoolClass PoolClassMV(void)
{
  return &PoolClassMVStruct;
}


/* MPS Interface Extension */

mps_class_t mps_class_mv(void)
{
  return (mps_class_t)(PoolClassMV());
}

/* Free bytes */

size_t mps_mv_free_size(mps_pool_t mps_pool)
{
  Pool pool;
  MV mv;
  MVSpan span;
  Size f = 0;
  Ring spans, node = NULL; /* gcc whinge stop */

  pool = (Pool)mps_pool;

  AVERT(Pool, pool);
  mv = PoolPoolMV(pool);
  AVERT(MV, mv);

  spans = &mv->spans;
  RING_FOR(node, spans) {
  span = RING_ELT(MVSpan, spans, node);
    AVERT(MVSpan, span);
    f += span->space;
  }

  return (size_t)f;
}

size_t mps_mv_size(mps_pool_t mps_pool)
{
  Pool pool;
  MV mv;
  MVSpan span;
  Space space;
  Size f = 0;
  Ring spans, node = NULL; /* gcc whinge stop */

  pool = (Pool)mps_pool;

  AVERT(Pool, pool);
  mv = PoolPoolMV(pool);
  AVERT(MV, mv);
  space = PoolSpace(pool);

  spans = &mv->spans;
  RING_FOR(node, spans) {
  span = RING_ELT(MVSpan, spans, node);
    AVERT(MVSpan, span);
    f += SegSize(space, span->seg);
  }

  return (size_t)f;
} 





/* MVCheck -- check the consistency of an MV structure */

Bool MVCheck(MV mv)
{
  CHECKS(MV, mv);
  CHECKD(Pool, &mv->poolStruct);
  CHECKL(mv->poolStruct.class == &PoolClassMVStruct);
  CHECKD(MFS, &mv->blockPoolStruct);
  CHECKD(MFS, &mv->spanPoolStruct);
  CHECKL(mv->extendBy > 0);
  CHECKL(mv->avgSize > 0);
  CHECKL(mv->extendBy >= mv->avgSize);
  /* Could do more checks here. */
  return TRUE;
}

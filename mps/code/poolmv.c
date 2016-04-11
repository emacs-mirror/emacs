/* poolmv.c: MANUAL VARIABLE POOL
 *
 * $Id$
 * Copyright (c) 2001-2015 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * **** RESTRICTION: This pool may not allocate from the arena control
 *                   pool, since it is used to implement that pool.
 *
 * An observation: Freeing memory introduces more information
 * into the system than allocating it.  This causes the problem
 * described in note 2.
 *
 * Notes
 *  1. Need to measure typical fragmentation levels and adjust the
 *     blockExtendBy parameter appropriately.  richard 1994-11-08
 *  2. free can lose memory if it can't allocate a block descriptor.  The
 *     memory could be pushed onto a special chain to be reclaimed later.
 *     richard 1994-11-09
 *  3. The span chain could be adaptive.  richard 1994-11-09
 *  5. An MFS pool for the block descriptors is justified, but not really
 *     for the spans, which are much rarer. richard 1994-11-09
 *  8. By changing MVSpanAlloc it might be possible to keep track of all
 *     allocated blocks using descriptors, for debugging purposes.  richard
 *     1994-11-10
 */

#include "mpscmv.h"
#include "dbgpool.h"
#include "poolmv.h"
#include "poolmfs.h"
#include "mpscmvff.h"
#include "mpm.h"

SRCID(poolmv, "$Id$");


#define mvBlockPool(mv) MFSPool(&(mv)->blockPoolStruct)
#define mvSpanPool(mv) MFSPool(&(mv)->spanPoolStruct)


#define PoolMV(pool) PARENT(MVStruct, poolStruct, pool)


/* MVDebug -- MV Debug pool class */

typedef struct MVDebugStruct {
  MVStruct MVStruct;             /* MV structure */
  PoolDebugMixinStruct debug;    /* debug mixin */
} MVDebugStruct;

typedef MVDebugStruct *MVDebug;


#define MV2MVDebug(mv)   PARENT(MVDebugStruct, MVStruct, mv)
#define MVDebug2MV(mvd)  (&((mvd)->MVStruct))


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

ATTRIBUTE_UNUSED
static Bool MVBlockCheck(MVBlock block)
{
  AVER(block != NULL);
  AVER(block->limit >= block->base);
  /* Check that it is in the block pool.  See note 7. */
  /* This turns out to be considerably tricky, as we cannot get hold */
  /* of the blockPool (pool is not a parameter). */
  return TRUE;
}


/* MVSpanStruct -- span structure
 *
 * The pool maintains a wrapper for each span allocated from the arena
 * which contains a chain of descriptors for the allocated memory in that
 * span.  It also contains sentinel block descriptors which mark the
 * start and end of the span.  These blocks considerably simplify
 * allocation, and may be zero-sized.
 *
 * .design.largest: If 'largestKnown' is TRUE, 'largest' is the size
 * of the largest free block in the span. Otherwise, 'largest' is
 * one more than the span size.
 *
 * .design.largest.alloc: When seeking a span in which to allocate,
 * a span should not be examined if 'largest' is less than the
 * space sought.
 *
 * .design.largest.free: When freeing, compute the size of the new
 * free area. If it is larger than 'largest', set 'largest' to it.
 */

#define MVSpanSig       ((Sig)0x5193F5BA) /* SIGnature MV SPAn */

typedef struct MVSpanStruct *MVSpan;
typedef struct MVSpanStruct {
  Sig sig;                      /* <design/sig/> */
  RingStruct spans;             /* all the spans */
  MV mv;                        /* owning MV pool */
  Tract tract;                  /* first tract of the span */
  Size size;                    /* size of the span */
  MVBlockStruct base;           /* sentinel at base of span */
  MVBlockStruct limit;          /* sentinel at limit of span */
  MVBlock blocks;               /* allocated blocks */
  Size free;                    /* free space in span */
  Size largest;                 /* .design.largest */
  Bool largestKnown;            /* .design.largest */
  unsigned blockCount;          /* number of blocks on chain */
} MVSpanStruct;


#define SpanSize(span) \
  AddrOffset((span)->base.base, (span)->limit.limit)
#define SpanInsideSentinels(span) \
  AddrOffset((span)->base.limit, (span)->limit.base)


/* MVSpanCheck -- check the consistency of a span structure */

ATTRIBUTE_UNUSED
static Bool MVSpanCheck(MVSpan span)
{
  Addr base, limit;

  CHECKS(MVSpan, span);

  CHECKD_NOSIG(Ring, &span->spans);
  CHECKU(MV, span->mv);
  CHECKD_NOSIG(Tract, span->tract);
  CHECKD_NOSIG(MVBlock, &span->base);
  CHECKD_NOSIG(MVBlock, &span->limit);
  /* The block chain starts with the base sentinel. */
  CHECKL(span->blocks == &span->base);
  /* Since there is a limit sentinel, the chain can't end just after the */
  /* base sentinel... */
  CHECKL(span->base.next != NULL);
  /* ... and it's sure to have at least two blocks on it. */
  CHECKL(span->blockCount >= 2);
  /* This is just defined this way.  It shouldn't change. */
  CHECKL(span->limit.next == NULL);
  /* The sentinels should mark the ends of the span. */
  base = TractBase(span->tract);
  limit = AddrAdd(base, span->size);
  CHECKL(span->base.base == base);
  CHECKL(span->limit.limit == limit);
  /* The sentinels mustn't overlap. */
  CHECKL(span->base.limit <= span->limit.base);
  /* The free space can't be more than the gap between the sentinels. */
  CHECKL(span->free <= SpanInsideSentinels(span));

  CHECKL(BoolCheck(span->largestKnown));
  if (span->largestKnown) { /* .design.largest */
    CHECKL(span->largest <= span->free);
    /* at least this much is free */
  } else {
    CHECKL(span->largest == SpanSize(span)+1);
  }

  /* Note that even if the CHECKs are compiled away there is still a
   * significant cost in looping over the tracts, hence this guard. */
#if defined(AVER_AND_CHECK_ALL)
  {
    Addr addr;
    Arena arena;
    Tract tract;
    /* Each tract of the span must refer to the span */
    arena = PoolArena(TractPool(span->tract));
    TRACT_FOR(tract, addr, arena, base, limit) {
      CHECKD_NOSIG(Tract, tract);
      CHECKL(TractP(tract) == (void *)span);
    }
    CHECKL(addr == limit);
  }
#endif

  return TRUE;
}


/* MVVarargs -- decode obsolete varargs */

static void MVVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_EXTEND_BY;
  args[0].val.size = va_arg(varargs, Size);
  args[1].key = MPS_KEY_MEAN_SIZE;
  args[1].val.size = va_arg(varargs, Size);
  args[2].key = MPS_KEY_MAX_SIZE;
  args[2].val.size = va_arg(varargs, Size);
  args[3].key = MPS_KEY_ARGS_END;
  AVERT(ArgList, args);
}

static void MVDebugVarargs(ArgStruct args[MPS_ARGS_MAX], va_list varargs)
{
  args[0].key = MPS_KEY_POOL_DEBUG_OPTIONS;
  args[0].val.pool_debug_options = va_arg(varargs, mps_pool_debug_option_s *);
  MVVarargs(args + 1, varargs);
}


/* MVInit -- init method for class MV */

static Res MVInit(Pool pool, ArgList args)
{
  Align align = MV_ALIGN_DEFAULT;
  Size extendBy = MV_EXTEND_BY_DEFAULT;
  Size avgSize = MV_AVG_SIZE_DEFAULT;
  Size maxSize = MV_MAX_SIZE_DEFAULT;
  Size blockExtendBy, spanExtendBy;
  MV mv;
  Arena arena;
  Res res;
  ArgStruct arg;
  
  if (ArgPick(&arg, args, MPS_KEY_ALIGN))
    align = arg.val.align;
  if (ArgPick(&arg, args, MPS_KEY_EXTEND_BY))
    extendBy = arg.val.size;
  if (ArgPick(&arg, args, MPS_KEY_MEAN_SIZE))
    avgSize = arg.val.size;
  if (ArgPick(&arg, args, MPS_KEY_MAX_SIZE))
    maxSize = arg.val.size;

  arena = PoolArena(pool);

  AVERT(Align, align);
  AVER(align <= ArenaGrainSize(arena));
  AVER(extendBy > 0);
  AVER(avgSize > 0);
  AVER(avgSize <= extendBy);
  AVER(maxSize > 0);
  AVER(extendBy <= maxSize);

  pool->alignment = align;
  mv = PoolMV(pool);

  /* At 100% fragmentation we will need one block descriptor for every other */
  /* allocated block, or (extendBy/avgSize)/2 descriptors.  See note 1. */
  blockExtendBy = sizeof(MVBlockStruct) * (extendBy/avgSize)/2;
  if(blockExtendBy < sizeof(MVBlockStruct)) {
    blockExtendBy = sizeof(MVBlockStruct);
  }

  MPS_ARGS_BEGIN(piArgs) {
    MPS_ARGS_ADD(piArgs, MPS_KEY_EXTEND_BY, blockExtendBy);
    MPS_ARGS_ADD(piArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(MVBlockStruct));
    res = PoolInit(mvBlockPool(mv), arena, PoolClassMFS(), piArgs);
  } MPS_ARGS_END(piArgs);
  if(res != ResOK)
    goto failBlockPoolInit;

  spanExtendBy = sizeof(MVSpanStruct) * (maxSize/extendBy);

  MPS_ARGS_BEGIN(piArgs) {
    MPS_ARGS_ADD(piArgs, MPS_KEY_EXTEND_BY, spanExtendBy);
    MPS_ARGS_ADD(piArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(MVSpanStruct));
    res = PoolInit(mvSpanPool(mv), arena, PoolClassMFS(), piArgs);
  } MPS_ARGS_END(piArgs);
  if(res != ResOK)
    goto failSpanPoolInit;

  mv->extendBy = extendBy;
  mv->avgSize  = avgSize;
  mv->maxSize  = maxSize;
  RingInit(&mv->spans);
   
  mv->free = 0;
  mv->lost = 0;

  mv->sig = MVSig;
  AVERT(MV, mv);
  EVENT5(PoolInitMV, pool, arena, extendBy, avgSize, maxSize);
  return ResOK;

failSpanPoolInit:
  PoolFinish(mvBlockPool(mv));
failBlockPoolInit:
  return res;
}


/* MVFinish -- finish method for class MV */

static void MVFinish(Pool pool)
{
  MV mv;
  Ring spans, node = NULL, nextNode; /* gcc whinge stop */
  MVSpan span;

  AVERT(Pool, pool);
  mv = PoolMV(pool);
  AVERT(MV, mv);

  /* Destroy all the spans attached to the pool. */
  spans = &mv->spans;
  RING_FOR(node, spans, nextNode) {
    span = RING_ELT(MVSpan, spans, node);
    AVERT(MVSpan, span);
    ArenaFree(TractBase(span->tract), span->size, pool);
  }

  mv->sig = SigInvalid;

  PoolFinish(mvBlockPool(mv));
  PoolFinish(mvSpanPool(mv));
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
  Size largest = 0;
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

    if (gap > largest) {
      largest = gap;
      AVER(largest <= span->largest);
    }

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

      if (gap == span->largest) { /* we've used a 'largest' gap */
        AVER(span->largestKnown);
        span->largestKnown = FALSE;
        span->largest = SpanSize(span) + 1;  /* .design.largest */
      }

      span->free -= size;
      *addrReturn = new;
      return TRUE;
    }

    block = block->next;
  }
  while(block->next != NULL);

  /* we've looked at all the gaps, so now we know the largest */
  AVER(span->largestKnown == FALSE);
  span->largestKnown = TRUE;
  span->largest = largest;

  return FALSE;
}


/* MVSpanFree -- free an area in a span of memory
 *
 * Searches a span for a block which contains the area specified by the
 * base and limit, and frees it within that span.  This may involve
 * allocating a block descriptor, which may fail, in which case an error is
 * returned.
 *
 * There are eight cases, depending on what we are freeing:
 * 1. whole of non-sentinel
 * 2. in body of any block
 * 3. at base of non-base
 * 4. at limit of non-limit
 * 5. whole of base sentinel
 * 6. whole of limit sentinel
 * 7. at base of base sentinel
 * 8. at limit of limit sentinel
 */

static Res MVSpanFree(MVSpan span, Addr base, Addr limit, Pool blockPool)
{
  MVBlock prev, block;
  Size freeAreaSize = 0; /* .design.largest.free */

  AVERT(MVSpan, span);
  AVER(span->base.base <= base);
  AVER(limit <= span->limit.limit);
  AVERT(Pool, blockPool);

  prev = NULL;
  block = span->blocks;

  AVER(block == &span->base); /* should be base sentinel */
  do {
    AVERT(MVBlock, block);

    /* Is the freed area within the block? */
    if(block->base <= base && limit <= block->limit) {
      Bool isBase = block == &span->base;
      Bool isLimit = block == &span->limit;
      Bool isSentinel = isBase || isLimit;

      if(!isSentinel && block->base == base && limit == block->limit) {
        /* case 1 : the whole of a non-sentinel block */
        AVER(block->next != NULL); /* there must at least be a sentinel */
        AVER(prev != NULL); /* block isn't sentinel */
        freeAreaSize = AddrOffset(prev->limit, block->next->base);
        prev->next = block->next;
        PoolFree(blockPool, (Addr)block, sizeof(MVBlockStruct));
        --span->blockCount;
      } else if(!isBase && block->base == base) {
        /* cases 3 and 6: at base of a block other than the base sentinel */
        AVER(prev != NULL); /* block isn't sentinel */
        freeAreaSize = AddrOffset(prev->limit, limit);
        block->base = limit;
      } else if(!isLimit && limit == block->limit) {
        /* cases 4 and 5: at limit of a block other than the limit sentinel */
        AVER(block->next != NULL); /* should at least be a sentinel */
        freeAreaSize = AddrOffset(base, block->next->base);
        block->limit = base;
      } else {
        /* cases 2, 7, and 8: making a new fragment */
        Res res;
        MVBlock new;
        Addr addr;

        /* The freed area is buried in the middle of the block, so the */
        /* block must be split into two parts.  */
        res = PoolAlloc(&addr, blockPool, sizeof(MVBlockStruct));
        if (res != ResOK)
          return res;
        new = (MVBlock)addr;

        freeAreaSize = AddrOffset(base, limit);

        /* If the freed area is in the base sentinel then insert the new */
        /* descriptor after it, otherwise insert before. */
        if(isBase) { /* case 7: new fragment at the base of the span */
          new->base = limit;
          new->limit = block->limit;
          block->limit = base;
          new->next = block->next;
          AVER(new->next != NULL); /* should at least be a sentinel */
          block->next = new;
        } else { /* cases 2 and 8 */
          new->base = block->base;
          new->limit = base;
          block->base = limit;
          new->next = block;
          AVER(prev != NULL);
          prev->next = new;
        }

        AVERT(MVBlock, new);
        ++span->blockCount;
      }

      AVERT(MVBlock, block);

      span->free += AddrOffset(base, limit);

      if (freeAreaSize > span->largest) { /* .design.largest */
        AVER(span->largestKnown);
        span->largest = freeAreaSize;
      }

      return ResOK;
    }

    prev = block;
    block = block->next;
  } while(block != NULL);

  /* The freed area is in the span, but not within a block. */
  NOTREACHED;

  return ResOK;
}


/* MVAlloc -- allocate method for class MV */

static Res MVAlloc(Addr *pReturn, Pool pool, Size size)
{
  Res res;
  MVSpan span;
  Arena arena;
  Addr base, limit, addr;
  Tract tract;
  MV mv;
  Size regionSize;
  Ring spans, node = NULL, nextNode; /* gcc whinge stop */

  AVER(pReturn != NULL);
  AVERT(Pool, pool);
  mv = PoolMV(pool);
  AVERT(MV, mv);
  AVER(size > 0);

  size = SizeAlignUp(size, pool->alignment);

  if(size <= mv->free) {
    spans = &mv->spans;
    RING_FOR(node, spans, nextNode) {
      span = RING_ELT(MVSpan, spans, node);
      if((size <= span->largest) &&          /* .design.largest.alloc */
         (size <= span->free)) {
        Addr new;

        if(MVSpanAlloc(&new, span, size, mvBlockPool(mv))) {
          mv->free -= size;
          AVER(AddrIsAligned(new, pool->alignment));
          *pReturn = new;
          return ResOK;
        }
      }
    }
  }

  /* There is no block large enough in any of the spans, so extend the */
  /* pool with a new region which will hold the requested allocation. */
  /* Allocate a new span descriptor and initialize it to point at the */
  /* region. */
  res = PoolAlloc(&addr, mvSpanPool(mv), sizeof(MVSpanStruct));
  if(res != ResOK)
    return res;
  span = (MVSpan)addr;

  if(size <= mv->extendBy)
    regionSize = mv->extendBy;
  else
    regionSize = size;

  arena = PoolArena(pool);
  regionSize = SizeArenaGrains(regionSize, arena);

  res = ArenaAlloc(&base, LocusPrefDefault(), regionSize, pool);
  if(res != ResOK) { /* try again with a region big enough for this object */
    regionSize = SizeArenaGrains(size, arena);
    res = ArenaAlloc(&base, LocusPrefDefault(), regionSize, pool);
    if (res != ResOK) {
      PoolFree(mvSpanPool(mv), (Addr)span, sizeof(MVSpanStruct));
      return res;
    }
  }
  limit = AddrAdd(base, regionSize);

  DebugPoolFreeSplat(pool, base, limit);

  span->size = regionSize;
  span->tract = TractOfBaseAddr(arena, base);
  span->mv = mv;
  /* Set the p field for each tract of the span  */
  TRACT_FOR(tract, addr, arena, base, limit) {
    AVERT(Tract, tract);
    AVER(TractP(tract) == NULL);
    AVER(TractPool(tract) == pool);
    TractSetP(tract, (void *)span);
  }
  AVER(addr == limit);
  RingInit(&span->spans);
  span->base.base = span->base.limit = base;
  span->limit.base = span->limit.limit = limit;
  span->free = AddrOffset(span->base.limit, span->limit.base);
  span->limit.next = NULL;
  span->base.next = &span->limit;
  span->blocks = &span->base;
  span->blockCount = 2;
  span->base.limit = AddrAdd(span->base.limit, size);
  span->free -= size;
  span->largest = span->free;
  span->largestKnown = TRUE;

  span->sig = MVSpanSig;
  AVERT(MVSpan, span);

  mv->free += span->free;
  RingInsert(&mv->spans, &span->spans);
  /* use RingInsert so that we examine this new span first when allocating */

  *pReturn = span->base.base;
  return ResOK;
}


/* MVFree -- free method for class MV */

static void MVFree(Pool pool, Addr old, Size size)
{
  Addr base, limit;
  MVSpan span;
  MV mv;
  Res res;
  Bool b;
  Tract tract = NULL;           /* suppress "may be used uninitialized" */

  AVERT(Pool, pool);
  mv = PoolMV(pool);
  AVERT(MV, mv);

  AVER(old != (Addr)0);
  AVER(AddrIsAligned(old, pool->alignment));
  AVER(size > 0);

  size = SizeAlignUp(size, pool->alignment);
  base = old;
  limit = AddrAdd(base, size);

  /* Map the pointer onto the tract which contains it, and thence */
  /* onto the span. */
  b = TractOfAddr(&tract, PoolArena(pool), old);
  AVER(b);
  span = (MVSpan)TractP(tract);
  AVERT(MVSpan, span);

  /* the to be freed area should be within the span just found */
  AVER(span->base.base <= base);
  AVER(limit <= span->limit.limit);

  /* Unfortunately, if allocating the new block descriptor fails we */
  /* can't do anything, and the memory is lost.  See note 2. */
  res = MVSpanFree(span, base, limit, mvBlockPool(mv));
  if(res != ResOK)
    mv->lost += size;
  else
    mv->free += size;
 
  /* free space should be less than total space */
  AVER(span->free <= SpanInsideSentinels(span));
  if(span->free == SpanSize(span)) { /* the whole span is free */
    AVER(span->blockCount == 2);
    /* both blocks are the trivial sentinel blocks */
    AVER(span->base.limit == span->base.base);
    AVER(span->limit.limit == span->limit.base);
    mv->free -= span->free;
    ArenaFree(TractBase(span->tract), span->size, pool);
    RingRemove(&span->spans);
    RingFinish(&span->spans);
    PoolFree(mvSpanPool(mv), (Addr)span, sizeof(MVSpanStruct));
  }
}


/* MVDebugMixin - find debug mixin in class MVDebug */

static PoolDebugMixin MVDebugMixin(Pool pool)
{
  MV mv;

  AVERT(Pool, pool);
  mv = PoolMV(pool);
  AVERT(MV, mv);
  /* Can't check MVDebug, because this is called during MVDebug init */
  return &(MV2MVDebug(mv)->debug);
}


/* MVTotalSize -- total memory allocated from the arena */

static Size MVTotalSize(Pool pool)
{
  MV mv;
  Size size = 0;
  Ring node, next;

  AVERT(Pool, pool);
  mv = PoolMV(pool);
  AVERT(MV, mv);

  RING_FOR(node, &mv->spans, next) {
    MVSpan span = RING_ELT(MVSpan, spans, node);
    AVERT(MVSpan, span);
    size += span->size;
  }

  return size;
}


/* MVFreeSize -- free memory (unused by client program) */

static Size MVFreeSize(Pool pool)
{
  MV mv;
  Size size = 0;
  Ring node, next;

  AVERT(Pool, pool);
  mv = PoolMV(pool);
  AVERT(MV, mv);

  RING_FOR(node, &mv->spans, next) {
    MVSpan span = RING_ELT(MVSpan, spans, node);
    AVERT(MVSpan, span);
    size += span->free;
  }

  AVER(size == mv->free + mv->lost);
  return size;
}


static Res MVDescribe(Pool pool, mps_lib_FILE *stream, Count depth)
{
  Res res;
  MV mv;
  MVSpan span;
  Align step;
  Size length;
  char c;
  Ring spans, node = NULL, nextNode; /* gcc whinge stop */

  if (!TESTT(Pool, pool))
    return ResFAIL;
  mv = PoolMV(pool);
  if (!TESTT(MV, mv))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream, depth,
               "blockPool $P ($U)\n",
               (WriteFP)mvBlockPool(mv), (WriteFU)mvBlockPool(mv)->serial,
               "spanPool  $P ($U)\n",
               (WriteFP)mvSpanPool(mv), (WriteFU)mvSpanPool(mv)->serial,
               "extendBy  $W\n",  (WriteFW)mv->extendBy,
               "avgSize   $W\n",  (WriteFW)mv->avgSize,
               "maxSize   $W\n",  (WriteFW)mv->maxSize,
               "free      $W\n",  (WriteFP)mv->free,
               "lost      $W\n",  (WriteFP)mv->lost,
               NULL);
  if(res != ResOK) return res;              

  step = pool->alignment;
  length = 0x40 * step;

  spans = &mv->spans;
  RING_FOR(node, spans, nextNode) {
    Addr i, j;
    MVBlock block;
    span = RING_ELT(MVSpan, spans, node);
    res = WriteF(stream, depth, "MVSpan $P {\n", (WriteFP)span, NULL);
    if (res != ResOK)
      return res;

    res = WriteF(stream, depth + 2,
                 "span    $P\n", (WriteFP)span,
                 "tract   $P\n", (WriteFP)span->tract,
                 "free    $W\n", (WriteFW)span->free,
                 "blocks  $U\n", (WriteFU)span->blockCount,
                 "largest ",
                 NULL);
    if (res != ResOK)
      return res;

    if (span->largestKnown) /* .design.largest */
      res = WriteF(stream, 0, "$W\n", (WriteFW)span->largest, NULL);
    else
      res = WriteF(stream, 0, "unknown\n", NULL);
    if (res != ResOK)
      return res;

    block = span->blocks;

    for(i = span->base.base; i < span->limit.limit; i = AddrAdd(i, length)) {
      res = WriteF(stream, depth + 2, "$A ", (WriteFA)i, NULL);
      if (res != ResOK)
        return res;

      for(j = i;
          j < AddrAdd(i, length) && j < span->limit.limit;
          j = AddrAdd(j, step)) {

        if(j >= block->limit) {
          block = block->next;
          if(block == NULL) return ResFAIL; /* shouldn't pass limit */
        }

        if(j == block->base) {
          if(AddrAdd(j, step) == block->limit)
            c = 'O';
          else
            c = '[';
        } else if(j < block->base)
          c = '.';
        else if(AddrAdd(j, step) == block->limit)
          c = ']';
        else /* j > block->base && j < block->limit */
          c = '=';
        res = WriteF(stream, 0, "$C", (WriteFC)c, NULL);
        if (res != ResOK)
          return res;
      }
      res = WriteF(stream, 0, "\n", NULL);
      if (res != ResOK)
        return res;
    }
    res = WriteF(stream, depth, "} MVSpan $P\n", (WriteFP)span, NULL);
    if (res != ResOK)
      return res;
  }

  return ResOK;
}


/* Pool class MV */


DEFINE_POOL_CLASS(MVPoolClass, this)
{
  INHERIT_CLASS(this, AbstractBufferPoolClass);
  this->name = "MV";
  this->size = sizeof(MVStruct);
  this->offset = offsetof(MVStruct, poolStruct);
  this->varargs = MVVarargs;
  this->init = MVInit;
  this->finish = MVFinish;
  this->alloc = MVAlloc;
  this->free = MVFree;
  this->totalSize = MVTotalSize;
  this->freeSize = MVFreeSize;
  this->describe = MVDescribe;
  AVERT(PoolClass, this);
}


MVPoolClass PoolClassMV(void)
{
  return EnsureMVPoolClass();
}


/* Pool class MVDebug */

DEFINE_POOL_CLASS(MVDebugPoolClass, this)
{
  INHERIT_CLASS(this, MVPoolClass);
  PoolClassMixInDebug(this);
  this->name = "MVDBG";
  this->size = sizeof(MVDebugStruct);
  this->varargs = MVDebugVarargs;
  this->debugMixin = MVDebugMixin;
  AVERT(PoolClass, this);
}


/* class functions
 *
 * Note this is an MPS interface extension
 */

mps_pool_class_t mps_class_mv(void)
{
  return (mps_pool_class_t)(EnsureMVPoolClass());
}

mps_pool_class_t mps_class_mv_debug(void)
{
  return (mps_pool_class_t)(EnsureMVDebugPoolClass());
}


/* MVCheck -- check the consistency of an MV structure */

Bool MVCheck(MV mv)
{
  CHECKS(MV, mv);
  CHECKD(Pool, MVPool(mv));
  CHECKL(IsSubclassPoly(MVPool(mv)->class, EnsureMVPoolClass()));
  CHECKD(MFS, &mv->blockPoolStruct);
  CHECKD(MFS, &mv->spanPoolStruct);
  CHECKL(mv->extendBy > 0);
  CHECKL(mv->avgSize > 0);
  CHECKL(mv->extendBy >= mv->avgSize);
  /* TODO: More checks are possible.  Consider what else could be checked. */
  return TRUE;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2015 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

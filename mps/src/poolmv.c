/* impl.c.poolmv: MANUAL VARIABLE POOL
 *
 * $HopeName: MMsrc!poolmv.c(trunk.10) $
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
 *  7. IsValid should check pointer destinations are in the right pools.
 *     richard 1994-11-10
 *  8. By changing SpanAlloc it might be possible to keep track of all
 *     allocated blocks using descriptors, for debugging purposes.  richard
 *     1994-11-10
 *  9. (See note 7.) IsValid methods can't easily get hold of the relevant
 *     pools in ordr to check pointers using PoolAddrPool.
 *     1995-01-19 drj
 */
 
#include "std.h"
#include "lib.h"
#include "poolar.h"
#include "pool.h"
#include "poolst.h"
#include "poolmv.h"
#include "poolmvst.h"
#include "poolmfs.h"
#include "mpscmv.h"
#include <stdarg.h>

SRCID("$HopeName: MMsrc!poolmv.c(trunk.10) $");


#define BLOCKPOOL(mv)   (PoolMFSPool(&(mv)->blockPoolStruct))
#define SPANPOOL(mv)    (PoolMFSPool(&(mv)->spanPoolStruct))


/*  == Class Structure ==  */

static Error create(Pool *poolReturn, Space space, va_list arg);
static void  destroy(Pool pool);
static Error alloc(Addr *pReturn, Pool pool, Size size);
static void free_(Pool pool, Addr old, Size size);
static Error describe(Pool pool, LibStream stream);

static PoolClassStruct PoolClassMVStruct;

PoolClass PoolClassMV(void)
{
  PoolClassInit(&PoolClassMVStruct,
                "MV",
                sizeof(PoolMVStruct), offsetof(PoolMVStruct, poolStruct),
                create, destroy,
                alloc, free_,
                NULL, NULL,             /* bufferCreate, bufferDestroy */
                NULL, NULL,		/* bufferFill, bufferTrip */
                NULL, NULL,		/* bufferExpose, bufferCover */
                NULL, NULL,             /* mark, scan */
                NULL, NULL,             /* fix, relcaim */
                NULL, NULL,             /* access, poll */
                describe);
  return &PoolClassMVStruct;
}


/* MPS Interface Extension */

mps_class_t mps_class_mv(void)
{
  return (mps_class_t)(PoolClassMV());
}


/*  == Block descriptor ==
 *
 *  The pool maintains a descriptor structure for each contiguous
 *  allocated block of memory it manages.  The descriptor is on a simple
 *  linked-list of such descriptors, which is in ascending order of
 *  address.
 */

typedef struct BlockStruct
{
  struct BlockStruct *next;
  Addr base, limit;
} BlockStruct, *Block;


#ifdef DEBUG

static Bool BlockIsValid(Block block, ValidationType validParam)
{
  AVER(block != NULL);
  AVER(block->limit >= block->base);
  /* Check that it is in the block pool.  See note 7. */
  /* This turns out to be considerably tricky, as we cannot get hold
   * of the blockPool (pool is not a parameter). */
  return TRUE;
}

#endif


/*  == Span descriptor ==
 *
 *  The pool maintains a wrapper for each allocated segment which
 *  contains a chain of descriptors for the allocated memory in that
 *  segment.  It also contains sentinel block descriptors which mark the
 *  start and end of the span.  These blocks considerably simplify
 *  allocation, and may be zero-sized.
 */

typedef struct SpanStruct
{
  struct SpanStruct *next;
  Addr seg;                     /* segment underlying the span */
  BlockStruct base;             /* sentinel at base of span */
  BlockStruct limit;            /* sentinel at limit of span */
  Block blocks;                 /* allocated blocks */
  Addr space;                   /* total free space in segment */
  unsigned blockCount;          /* number of blocks on chain */
} SpanStruct, *Span;


#ifdef DEBUG

static Bool SpanIsValid(Span span, ValidationType validParam)
{
  AVER(span != NULL);
  /* seg */
  AVER(ISVALIDNESTED(Block, &span->base));
  AVER(ISVALIDNESTED(Block, &span->limit));

  /* The block chain starts with the base sentinel. */
  AVER(span->blocks == &span->base);
  /* Since there is a limit sentinel, the chain can't end just after the */
  /* base sentinel... */
  AVER(span->base.next != NULL);
  /* ...and it's sure to have at least two blocks on it. */
  AVER(span->blockCount >= 2);
  /* This is just defined this way.  It shouldn't change. */
  AVER(span->limit.next == NULL);
  /* The sentinels should mark the ends of the segment. */
  AVER(span->base.base == span->seg);
  /* we used to be able to find out where the end of a seg was.
   * Now we need the arena as well, and we can't get that from
   * the span */
/*
  AVER(span->limit.limit == span->seg + ArenaSegSize(arena, span->seg));
*/
  /* The sentinels mustn't overlap. */
  AVER(span->base.limit <= span->limit.base);
  /* The remaining space can't be more than the gap between the sentinels. */
  AVER(span->space <= span->limit.base - span->base.limit);

  /* Check that it is in the span pool.  See note 7. */

  return TRUE;
}

#endif


#ifdef DEBUG

Bool PoolMVIsValid(PoolMV poolMV, ValidationType validParam)
{
  AVER(ISVALIDNESTED(Pool, &poolMV->poolStruct));
  AVER(ISVALIDNESTED(PoolMFS, &poolMV->blockPoolStruct));
  AVER(ISVALIDNESTED(PoolMFS, &poolMV->spanPoolStruct));
  AVER(poolMV->extendBy > 0);
  AVER(poolMV->avgSize > 0);
  AVER(poolMV->extendBy >= poolMV->avgSize);

  /* Could do more checks here. */

  return TRUE;
}

#endif /* DEBUG */


Pool PoolMVPool(PoolMV poolMV)
{
  AVER(ISVALID(PoolMV, poolMV));
  return &poolMV->poolStruct;
}


Error PoolMVCreate(PoolMV *poolMVReturn, Space space,
                   Size extendBy, Size avgSize, Size maxSize)
{
  Error e;
  PoolMV poolMV;

  AVER(poolMVReturn != NULL);
  AVER(ISVALID(Space, space));

  e = PoolAlloc((Addr *)&poolMV, SpaceControlPool(space),
    sizeof(PoolMVStruct));
  if(e != ErrSUCCESS)
    return e;
  
  e = PoolMVInit(poolMV, space, extendBy, avgSize, maxSize);
  if(e != ErrSUCCESS) {
    PoolFree(SpaceControlPool(space), (Addr)poolMV, sizeof(PoolMVStruct));
    return e;
  }
  
  *poolMVReturn = poolMV;
  return ErrSUCCESS;
}

static Error create(Pool *poolReturn, Space space, va_list arg)
{
  Size extendBy, avgSize, maxSize;
  PoolMV poolMV;
  Error e;

  AVER(poolReturn != NULL);
  AVER(ISVALID(Space, space));
  
  extendBy = va_arg(arg, Size);
  avgSize = va_arg(arg, Size);
  maxSize = va_arg(arg, Size);
  
  e = PoolMVCreate(&poolMV, space, extendBy, avgSize, maxSize);
  if(e != ErrSUCCESS)
    return e;
  
  *poolReturn = PoolMVPool(poolMV);
  return ErrSUCCESS;
}


void PoolMVDestroy(PoolMV poolMV)
{
  Pool control;
  AVER(ISVALID(PoolMV, poolMV));
  control = SpaceControlPool(PoolSpace(PoolMVPool(poolMV)));
  PoolMVFinish(poolMV);
  PoolFree(control, (Addr)poolMV, sizeof(PoolMVStruct));
}

static void destroy(Pool pool)
{
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassMVStruct);
  PoolMVDestroy(PARENT(PoolMVStruct, poolStruct, pool));
}


Error PoolMVInit(PoolMV poolMV, Space space, Size extendBy,
                 Size avgSize, Size maxSize)
{
  Error e;
  Size blockExtendBy, spanExtendBy;

  AVER(poolMV != NULL);
  AVER(ISVALID(Space, space));
  AVER(extendBy > 0);
  AVER(avgSize > 0);
  AVER(avgSize <= extendBy);
  AVER(maxSize > 0);
  AVER(extendBy <= maxSize);

  PoolInit(&poolMV->poolStruct, space, PoolClassMV());

  /* At 100% fragmentation we will need one block descriptor for every other */
  /* allocated block, or (extendBy/avgSize)/2 descriptors.  See note 1. */

  blockExtendBy = sizeof(BlockStruct) * (extendBy/avgSize)/2;

  e = PoolMFSInit(&poolMV->blockPoolStruct, space,
    blockExtendBy, sizeof(BlockStruct));
  if(e != ErrSUCCESS)
    return e;

  spanExtendBy = sizeof(SpanStruct) * (maxSize/extendBy);

  e = PoolMFSInit(&poolMV->spanPoolStruct, space,
    spanExtendBy, sizeof(SpanStruct));
  if(e != ErrSUCCESS)
    return e;

  poolMV->extendBy = extendBy;
  poolMV->avgSize  = avgSize;
  poolMV->maxSize  = maxSize;
  poolMV->spans = NULL;
  poolMV->space = 0;
  poolMV->lost = 0;

  AVER(ISVALID(PoolMV, poolMV));

  return ErrSUCCESS;
}


void PoolMVFinish(PoolMV poolMV)
{
  Pool pool;
  Span span;

  AVER(ISVALID(PoolMV, poolMV));

  /* Destroy all the segments attached to the pool. */

  pool = PoolMVPool(poolMV);
  span = poolMV->spans;
  while(span != NULL) {
    AVER(ISVALID(Span, span));
    PoolSegFree(pool, span->seg, span->limit.limit - span->base.base);
    span = span->next;
  }

  PoolFinish(&poolMV->poolStruct);
  PoolMFSFinish(&poolMV->blockPoolStruct);
  PoolMFSFinish(&poolMV->spanPoolStruct);
}


/*  == Allocate space in span ==
 *
 *  SpanAlloc searches a span for a free block of the requested size.  If it
 *  finds one it allocates it from the span, updates *addrReturn to point
 *  to it, and returns TRUE.
 */

static Bool SpanAlloc(Addr *addrReturn, Span span, Addr size,
                      Pool blockPool)
{
  Addr gap;
  Block block;

  AVER(ISVALID(Span, span));
  AVER(size > 0);
  AVER(addrReturn != NULL);

  block = span->blocks;
  AVER(block == &span->base);   /* should be the base sentinel */

  /* We're guaranteed at least one gap between sentinels, and therefore at */
  /* least one iteration of this loop.  So, the test is at the end.  */

  do
  {
    AVER(block->next != NULL);

    gap = block->next->base - block->limit;

    if(gap >= size)
    {
      Addr new = block->limit;

      /* If the gap is exactly the right size then the preceeding and */
      /* following blocks can be merged, into the preceeding one, */
      /* unless the following block is the end sentinel. */

      if(gap == size && block->next != &span->limit)
      {
        Block old = block->next;
        block->limit = old->limit;
        block->next = old->next;
        PoolFree(blockPool, (Addr)old, sizeof(BlockStruct));
        --span->blockCount;
      }
      else
        block->limit += size;

      span->space -= size;
      *addrReturn = new;
      return TRUE;
    }

    block = block->next;
  }
  while(block->next != NULL);

  return FALSE;
}


/*  == Free area in span ==
 *
 *  Searches a span for a block which contains the area specified by the
 *  base and limit, and frees it within that span.  This may involve
 *  allocating a block descriptor, which may fail, in which case an error is
 *  returned.
 */

static Error SpanFree(Span span, Addr base, Addr limit, Pool blockPool)
{
  Block *prev, block;

  AVER(ISVALID(Span, span));
  AVER(span->base.base <= base && limit <= span->limit.limit);
  AVER(ISVALID(Pool, blockPool));

  prev = &span->blocks;
  block = span->blocks;
  AVER(block == &span->base); /* should be base sentinel */
  do
  {
    int isBase = block == &span->base;
    int isLimit = block == &span->limit;
    int isSentinel = isBase || isLimit;

    AVER(ISVALID(Block, block));

    /* Is the freed area within the block? */

    if(block->base <= base && limit <= block->limit)
    {
      if(!isSentinel && block->base == base && limit == block->limit)
      {
        AVER(block->next != NULL); /* should at least be a sentinel */
        *prev = block->next;
        PoolFree(blockPool, (Addr)block, sizeof(BlockStruct));
        --span->blockCount;
      }
      else if(!isBase && block->base == base)
        block->base = limit;
      else if(!isLimit && limit == block->limit)
        block->limit = base;
      else
      {
        Error e;
        Block new;

        /* The freed area is buried in the middle of the block, so the */
        /* block must be split into two parts.  */

        e = PoolAlloc((Addr *)&new, blockPool, sizeof(BlockStruct));
        if(e != ErrSUCCESS) return e;

        /* If the freed area is in the base sentinel then insert the new */
        /* descriptor after it, otherwise insert before. */
        if(isBase)
        {
          new->base = limit;
          new->limit = block->limit;
          block->limit = base;
          new->next = block->next;
          AVER(new->next != NULL); /* should at least be a sentinel */
          block->next = new;
        }
        else
        {
          new->base = block->base;
          new->limit = base;
          block->base = limit;
          new->next = block;
          *prev = new;
        }

        AVER(ISVALID(Block, new));
        ++span->blockCount;
      }

      AVER(ISVALID(Block, block));

      span->space += limit - base;

      return ErrSUCCESS;
    }

    prev = &block->next;
    block = block->next;
  }
  while(block != NULL);

  /* The freed area is in the span, but not within a block. */
  NOTREACHED;

  return ErrSUCCESS;
}


/*  == Allocate ==  */

static Error alloc(Addr *pReturn, Pool pool, Size size)
{
  Error e;
  Arena arena;
  Span span;
  PoolMV poolMV;
  Size segSize;

  AVER(pReturn != NULL);
  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassMVStruct);
  AVER(size > 0);

  poolMV = PARENT(PoolMVStruct, poolStruct, pool);

  size = AlignUp(pool->alignment, size);

  if(size <= poolMV->space)
  {
    span = poolMV->spans;

    while(span != NULL)
    {
      if(size <= span->space)
      {
        Addr new;

        if(SpanAlloc(&new, span, size, BLOCKPOOL(poolMV)))
        {
          poolMV->space -= size;
          AVER(IsAligned(pool->alignment, new));
          *pReturn = new;
          return ErrSUCCESS;
        }
      }

      span = span->next;
    }
  }

/*  There is no block large enough in any of the spans, so extend the
 *  pool with a new segment which will hold the requested allocation.
 *  Allocate a new span descriptor and initialize it to point at the
 *  segment.
 */

  e = PoolAlloc((Addr *)&span, SPANPOOL(poolMV), sizeof(SpanStruct));
  if(e != ErrSUCCESS)
    return e;

  if(size <= poolMV->extendBy)
    segSize = poolMV->extendBy;
  else
    segSize = size;

  arena = SpaceArena(PoolSpace(pool));
  segSize = AlignUp(ArenaGrain(arena), segSize);

  e = PoolSegAlloc(&span->seg, pool, segSize);
  if(e != ErrSUCCESS)
  {
    PoolFree(SPANPOOL(poolMV), (Addr)span, sizeof(SpanStruct));
    return e;
  }
  ArenaPut(arena, span->seg, ARENA_CLASS, (void *)span);


  span->next = poolMV->spans;
  span->base.base = span->base.limit = span->seg;
  span->limit.base =
    span->limit.limit = span->seg + ArenaSegSize(arena, span->seg);
  span->space = span->limit.base - span->base.limit;
  span->limit.next = NULL;
  span->base.next = &span->limit;
  span->blocks = &span->base;
  span->blockCount = 2;

  span->base.limit += size;
  span->space -= size;

  AVER(ISVALID(Span, span));

  poolMV->space += span->space;
  poolMV->spans = span;

  *pReturn = span->base.base;
  return ErrSUCCESS;
}


static void free_(Pool pool, Addr old, Size size)
{
  Addr base, limit;
  Arena arena;
  Span span;
  PoolMV poolMV;
  Error e;

  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassMVStruct);
  AVER(old != (Addr)0);
  AVER(size > 0);

  arena = SpaceArena(PoolSpace(pool));

  poolMV = PARENT(PoolMVStruct, poolStruct, pool);
  size = AlignUp(pool->alignment, size);
  base = old;
  limit = base + size;

  /* Map the pointer onto the segment which contains it, and thence */
  /* onto the span. */

  span = ArenaGet(arena, base, ARENA_CLASS);
  AVER(ISVALID(Span, span));
  
  /* the to be freed area should be within the span just found */
  AVER(span->base.base <= base && limit <= span->limit.limit);
  
  /* Unfortunately, if allocating the new block descriptor fails we */
  /* can't do anything, and the memory is lost.  See note 2. */
  e = SpanFree(span, base, limit, BLOCKPOOL(poolMV));
  if(e != ErrSUCCESS)
    poolMV->lost += size;
  else
    poolMV->space += size;
  
  return;                   /* should free spans.  See note 4. */
}


static Error describe(Pool pool, LibStream stream)
{
  PoolMV poolMV;
  Span span;
  Addr step, length;

  AVER(ISVALID(Pool, pool));
  AVER(pool->class == &PoolClassMVStruct);
  AVER(stream != NULL);

  poolMV = PARENT(PoolMVStruct, poolStruct, pool);

  LibFormat(stream,
    "  blockPool = %p  spanPool = %p\n"
    "  extendBy = %lX\n"
    "  avgSize  = %lX\n"
    "  maxSize  = %lX\n"
    "  space    = %lX\n",
    BLOCKPOOL(poolMV), SPANPOOL(poolMV),
    (unsigned long)poolMV->extendBy,
    (unsigned long)poolMV->avgSize,
    (unsigned long)poolMV->maxSize,
    poolMV->space);

  LibFormat(stream,
    "  Spans\n"
    "      desc      seg    space blockCount\n");
  span = poolMV->spans;
  while(span != NULL)
  {
    AVER(ISVALID(Span, span));

    LibFormat(stream, "  %8lX %8lX %8lX %d\n",
      (unsigned long)span,
      (unsigned long)span->seg,
      span->space, span->blockCount);

    span = span->next;
  }

  LibFormat(stream, "  Span allocation maps\n");

  step = pool->alignment;
  length = 0x40 * step;

  span = poolMV->spans;
  while(span != NULL)
  {
    Addr i, j;
    Block block;
    LibFormat(stream, "    Span %8lX\n", (unsigned long)span);

    block = span->blocks;
    AVER(block == &span->base); /* should be start sentinel */

    for(i=span->base.base; i<span->limit.limit; i+=length)
    {
      LibFormat(stream, "    %8lX ", (unsigned long)i);

      for(j=i; j<i+length && j<span->limit.limit; j+=step)
      {
        if(j == block->base) {
          if(j+step == block->limit)
            LibPutChar(stream, '@');
          else
            LibPutChar(stream, '[');
        }
        else if(j+step == block->limit)
          LibPutChar(stream, ']');
        else if(j > block->base && j < block->limit)
          LibPutChar(stream, '=');
        else
          LibPutChar(stream, '.');

        if(j >= block->limit)
        {
          block = block->next;
          AVER(block != NULL);  /* shouldn't pass limit sentinel */
        }
      }
      LibPutChar(stream, '\n');
    }

    span = span->next;
  }

  return ErrSUCCESS;
}

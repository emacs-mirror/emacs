/* impl.c.dbgpool: POOL DEBUG MIXIN
 *
 * $HopeName: MMsrc!dbgpool.c(trunk.3) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * .source: design.mps.object-debug
 */

#include "dbgpool.h"
#include "mpslib.h"
#include "mpm.h"
#include "mps.h"
#include <stdarg.h>


/* tagStruct -- tags for storing info baout allocated objects */

typedef struct tagStruct {
  /* We don't want to pay the expense of a sig in every tag */
  Addr addr;
  Size size;
  SplayNodeStruct splayNode;
  char userdata[1 /* actually variable length */];
} tagStruct;

#define SplayNode2Tag(node) PARENT(tagStruct, splayNode, (node))

typedef tagStruct *Tag;


/* tag init methods: copying the user-supplied data into the tag */

#define TagInitMethodCheck(f) \
  ((f) != NULL) /* that's the best we can do */

static void TagTrivInit(void* tag, va_list args)
{
  UNUSED(tag); UNUSED(args);
}


/* TagComp -- splay comparison function for address ordering of tags */

static Compare TagComp(void *key, SplayNode node)
{
  Addr addr1, addr2;

  addr1 = *(Addr *)key;
  addr2 = SplayNode2Tag(node)->addr;
  if (addr1 < addr2)
    return CompareLESS;
  else if (addr1 > addr2) {
    /* Check key is not inside the object of this tag */
    AVER_CRITICAL(AddrAdd(addr2, SplayNode2Tag(node)->size) <= addr1);
    return CompareGREATER;
  } else
    return CompareEQUAL;
}


/* PoolDebugMixinCheck -- check a PoolDebugMixin */

Bool PoolDebugMixinCheck(PoolDebugMixin debug)
{
  /* Nothing to check about fenceTemplate */
  /* Nothing to check about fenceSize */
  CHECKL(TagInitMethodCheck(debug->tagInit));
  /* Nothing to check about tagSize */
  CHECKD(Pool, debug->tagPool);
  CHECKL(CHECKTYPE(Addr, void*)); /* tagPool relies on this */
  /* Nothing to check about missingTags */
  CHECKL(SplayTreeCheck(&debug->index));
  UNUSED(debug); /* see impl.c.mpm.check.unused */
  return TRUE;
}


/* DebugPoolDebugMixin -- gets the debug mixin, if any */

#define DebugPoolDebugMixin(pool) (((pool)->class->debugMixin)(pool))


/* PoolNoDebugMixin -- debug mixin methods for pools with no mixin */

PoolDebugMixin PoolNoDebugMixin(Pool pool)
{
  AVERT(Pool, pool);
  return NULL;
}


/* PoolDebugOptionsCheck -- check a PoolDebugOptions */

static Bool PoolDebugOptionsCheck(PoolDebugOptions opt)
{
  CHECKL(opt != NULL);
  if (opt->fenceSize != 0) {
    CHECKL(opt->fenceTemplate != NULL);
    /* Nothing to check about fenceSize */
  }
  return TRUE;
}


/* DebugPoolInit -- init method for a debug pool
 *
 * Someday, this could be split into fence and tag init methods.
 */

static Res DebugPoolInit(Pool pool, va_list args)
{
  Res res;
  PoolDebugOptions options;
  PoolDebugMixin debug;
  TagInitMethod tagInit;
  Size tagSize;

  AVERT(Pool, pool);
  options = va_arg(args, PoolDebugOptions);
  AVERT(PoolDebugOptions, options);
  /* @@@@ Tag parameters should be taken from options, but tags have */
  /* not been published yet. */
  tagInit = NULL; tagSize = 0;

  res = (pool->class->super->init)(pool, args);
  if (res != ResOK)
    return res;

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);

  /* fencepost init */
  /* @@@@ This parses a user argument, options, so it should really */
  /* go through the MPS interface.  The template needs to be copied */
  /* into Addr memory, to avoid breaking design.mps.type.addr.use. */
  debug->fenceSize = options->fenceSize;
  if (debug->fenceSize != 0) {
    if (debug->fenceSize % PoolAlignment(pool) != 0) {
      res = ResPARAM;
      goto alignFail;
    }
    /* Fenceposting turns on tagging */
    if (tagInit == NULL) {
      tagSize = 0;
      tagInit = TagTrivInit;
    }
    debug->fenceTemplate = options->fenceTemplate;
  }
  
  /* tag init */
  debug->tagInit = tagInit;
  if (debug->tagInit != NULL) {
    debug->tagSize = tagSize + sizeof(tagStruct) - 1;
    /* This pool has to be like the arena control pool: the blocks */
    /* allocated must be accessible using void*. */
    res = PoolCreate(&debug->tagPool, PoolArena(pool), PoolClassMFS(),
                     debug->tagSize, debug->tagSize);
    if (res != ResOK)
      goto tagFail;
    debug->missingTags = 0;
    SplayTreeInit(&debug->index, TagComp, NULL);
  }

  debug->sig = PoolDebugMixinSig;
  AVERT(PoolDebugMixin, debug);
  return ResOK;

tagFail:
alignFail:
  (pool->class->super->finish)(pool);
  return res;
}


/* DebugPoolFinish -- finish method for a debug pool */

static void DebugPoolFinish(Pool pool)
{
  PoolDebugMixin debug;

  AVERT(Pool, pool);

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);
  AVERT(PoolDebugMixin, debug);
  if (debug->tagInit != NULL) {
    SplayTreeFinish(&debug->index);
    PoolDestroy(debug->tagPool);
  }
  (pool->class->super->finish)(pool);
}


/* FenceAlloc -- allocation wrapper for fenceposts
 *
 * Allocates an object, adding fenceposts on both sides.  Layout:
 *
 * |----------|-------------------------------------|------|----------|
 *   start fp              client object              slop    end fp
 *
 * slop is the extra allocation from rounding up the client request to
 * the pool's alignment.  The fenceposting code does this, so there's a
 * better chance of the end fencepost being flush with the next object
 * (can't be guaranteed, since the underlying pool could have allocated
 * an even larger block).  The alignment slop is filled from the
 * fencepost template as well (as much as fits, .fence.size guarantees
 * the template is larger).
 */

static Res FenceAlloc(Addr *aReturn, PoolDebugMixin debug, Pool pool,
                      Size size, Bool withReservoir)
{
  Res res;
  Addr new, clientNew;
  Size alignedSize;

  AVER(aReturn != NULL);
  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(Bool, withReservoir);

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  res = (pool->class->super->alloc)(&new, pool,
                                    alignedSize + 2*debug->fenceSize,
                                    withReservoir);
  if (res != ResOK)
    return res;
  clientNew = AddrAdd(new, debug->fenceSize);
  /* @@@@ shields? */
  /* start fencepost */
  AddrCopy(new, debug->fenceTemplate, debug->fenceSize);
  /* alignment slop */
  AddrCopy(AddrAdd(clientNew, size),
           debug->fenceTemplate, alignedSize - size);
  /* end fencepost */
  AddrCopy(AddrAdd(clientNew, alignedSize),
           debug->fenceTemplate, debug->fenceSize);

  *aReturn = clientNew;
  return res;
}


/* FenceCheck -- check fences of an object */

static Bool FenceCheck(PoolDebugMixin debug, Pool pool,
                       Addr obj, Size size)
{
  Size alignedSize;

  AVERT_CRITICAL(PoolDebugMixin, debug);
  AVERT_CRITICAL(Pool, pool);
  /* Can't check obj */

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  /* Compare this to the memcpy's in FenceAlloc */
  return (AddrComp(AddrSub(obj, debug->fenceSize), debug->fenceTemplate,
                   debug->fenceSize) == 0
          && AddrComp(AddrAdd(obj, size), debug->fenceTemplate,
                      alignedSize - size) == 0
          && AddrComp(AddrAdd(obj, alignedSize), debug->fenceTemplate,
                      debug->fenceSize) == 0);
}


/* FenceFree -- freeing wrapper for fenceposts */

static void FenceFree(PoolDebugMixin debug,
                      Pool pool, Addr old, Size size)
{
  Size alignedSize;

  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  /* Can't check old */
  AVER(size > 0);

  AVER(FenceCheck(debug, pool, old, size));

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  (pool->class->super->free)(pool, AddrSub(old, debug->fenceSize),
                             alignedSize + 2*debug->fenceSize);
}


/* TagAlloc -- tag allocation */

static Res TagAlloc(PoolDebugMixin debug,
                    Pool pool, Addr new, Size size, Bool withReservoir)
{
  Tag tag;
  Res res;

  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  AVER(size > 0);

  res = PoolAlloc((Addr*)&tag, debug->tagPool, debug->tagSize, FALSE);
  if (res != ResOK)
    if (withReservoir) { /* design.mps.object-debug.out-of-space */
      debug->missingTags++;
      return ResOK;
    } else {
      return res;
    }
  tag->addr = new; tag->size = size;
  /* In the future, we might call debug->tagInit here. */
  res = SplayTreeInsert(&debug->index, &tag->splayNode, (void *)&new);
  AVER(res == ResOK);
  return ResOK;
}


static void TagFree(PoolDebugMixin debug, Pool pool, Addr old, Size size)
{
  SplayNode node;
  Tag tag;
  Res res;

  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  AVER(size > 0);

  res = SplayTreeSearch(&node, &debug->index, (void *)&old);
  if (res != ResOK) {
    AVER(debug->missingTags > 0);
    debug->missingTags--;
    return;
  }
  tag = SplayNode2Tag(node);
  AVER(tag->size == size);
  res = SplayTreeDelete(&debug->index, node, (void *)&old);
  AVER(res == ResOK);
  PoolFree(debug->tagPool, (Addr)tag, debug->tagSize);
}


/* DebugPoolAlloc -- alloc method for a debug pool
 *
 * Eventually, tag init args will need to be handled somewhere here.
 */

static Res DebugPoolAlloc(Addr *aReturn,
                           Pool pool, Size size, Bool withReservoir)
{
  Res res;
  Addr new;
  PoolDebugMixin debug;

  AVER(aReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(Bool, withReservoir);

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);
  AVERT(PoolDebugMixin, debug);
  res = FenceAlloc(&new, debug, pool, size, withReservoir);
  if (res != ResOK)
    return res;
  res = TagAlloc(debug, pool, new, size, withReservoir);
  if (res != ResOK)
    goto tagFail;

  *aReturn = new;
  return res;

tagFail:
  FenceFree(debug, pool, new, size);
  return res;
}


/* DebugPoolFree -- free method for a debug pool */

static void DebugPoolFree(Pool pool, Addr old, Size size)
{
  PoolDebugMixin debug;

  AVERT(Pool, pool);
  /* Can't check old */
  AVER(size > 0);

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);
  AVERT(PoolDebugMixin, debug);
  TagFree(debug, pool, old, size);
  FenceFree(debug, pool, old, size);
}


/* TagWalk -- walk all object in the pool using tags */

typedef void (*ObjectsStepMethod)(Addr addr, Size size, Format fmt,
                                  Pool pool, void *tagData, void *p);

#define ObjectsStepMethodCheck(f) \
  ((f) != NULL) /* that's the best we can do */

static void TagWalk(Pool pool, ObjectsStepMethod step, void *p)
{
  SplayNode node;
  PoolDebugMixin debug;
  Addr dummy = NULL; /* Breaks design.mps.type.addr.use, but it's */
                     /* only temporary until SplayTreeFirst is fixed. */

  AVERT(Pool, pool);
  AVERT(ObjectsStepMethod, step);
  /* Can't check p */

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);
  AVERT(PoolDebugMixin, debug); 

  node = SplayTreeFirst(&debug->index, (void *)&dummy);
  while (node != NULL) {
    Tag tag = SplayNode2Tag(node);

    step(tag->addr, tag->size, NULL, pool, &tag->userdata, p);
    node = SplayTreeNext(&debug->index, node, (void *)&tag->addr);
  }
}


/* FenceCheckingStep -- step function for DebugPoolCheckFences */

static void FenceCheckingStep(Addr addr, Size size, Format fmt,
                              Pool pool, void *tagData, void *p)
{
  /* no need to check arguments checked in the caller */
  UNUSED(fmt); UNUSED(tagData);
  FenceCheck((PoolDebugMixin)p, pool, addr, size);
}


/* DebugPoolCheckFences -- check all the fenceposts in the pool */

static void DebugPoolCheckFences(Pool pool)
{
  PoolDebugMixin debug;

  AVERT(Pool, pool);
  debug = DebugPoolDebugMixin(pool);
  if (debug == NULL)
    return;
  AVERT(PoolDebugMixin, debug);

  TagWalk(pool, FenceCheckingStep, (void *)debug);
}


/* EnsureDebugClass -- make a debug subclass of the given class */

void EnsureDebugClass(PoolClassStruct *class, PoolClass super)
{
  *class = *super;  /* @@@@ this doesn't work multi-threaded! */
  class->super = super;
  class->init = DebugPoolInit;
  class->finish = DebugPoolFinish;
  class->alloc = DebugPoolAlloc;
  class->free = DebugPoolFree;
}


/* mps_pool_check_fenceposts -- check all the fenceposts in the pool */

void mps_pool_check_fenceposts(mps_pool_t mps_pool)
{
  Pool pool = (Pool)mps_pool;
  Arena arena;
  
  AVER(CHECKT(Pool, pool));
  arena = PoolArena(pool);

  ArenaEnter(arena);

  AVERT(Pool, pool);
  DebugPoolCheckFences(pool);

  ArenaLeave(arena);
}

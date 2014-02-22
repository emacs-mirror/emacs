/* dbgpool.c: POOL DEBUG MIXIN
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .source: design.mps.object-debug
 */

#include "dbgpool.h"
#include "poolmfs.h"
#include "splay.h"
#include "mpm.h"
#include <stdarg.h>

SRCID(dbgpool, "$Id$");


/* tagStruct -- tags for storing info about allocated objects */

typedef struct tagStruct {
  /* We don't want to pay the expense of a sig in every tag */
  Addr addr;
  Size size;
  TreeStruct treeStruct;
  char userdata[1 /* actually variable length */];
} tagStruct;

#define TagTree(tag)    (&(tag)->treeStruct)
#define TagOfTree(tree) PARENT(tagStruct, treeStruct, tree)

typedef tagStruct *Tag;


/* tag init methods: copying the user-supplied data into the tag */

#define TagInitMethodCheck(f) FUNCHECK(f)

static void TagTrivInit(void* tag, va_list args)
{
  UNUSED(tag); UNUSED(args);
}


/* TagComp -- splay comparison function for address ordering of tags */

static Compare TagCompare(Tree node, TreeKey key)
{
  Addr addr1, addr2;

  addr1 = *(Addr *)key;
  addr2 = TagOfTree(node)->addr;
  if (addr1 < addr2)
    return CompareLESS;
  else if (addr1 > addr2) {
    /* Check key is not inside the object of this tag */
    AVER_CRITICAL(AddrAdd(addr2, TagOfTree(node)->size) <= addr1);
    return CompareGREATER;
  } else
    return CompareEQUAL;
}


/* PoolDebugMixinCheck -- check a PoolDebugMixin */

Bool PoolDebugMixinCheck(PoolDebugMixin debug)
{
  /* Nothing to check about fenceTemplate */
  /* Nothing to check about fenceSize */
  /* Nothing to check about freeTemplate */
  /* Nothing to check about freeSize */
  if (debug->tagInit != NULL) {
    CHECKL(TagInitMethodCheck(debug->tagInit));
    /* Nothing to check about tagSize */
    CHECKD(Pool, debug->tagPool);
    CHECKL(COMPATTYPE(Addr, void*)); /* tagPool relies on this */
    /* Nothing to check about missingTags */
    CHECKL(SplayTreeCheck(&debug->index));
  }
  UNUSED(debug); /* see <code/mpm.c#check.unused> */
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

Bool PoolDebugOptionsCheck(PoolDebugOptions opt)
{
  CHECKL(opt != NULL);
  if (opt->fenceSize != 0) {
    CHECKL(opt->fenceTemplate != NULL);
    /* Nothing to check about fenceSize */
  }
  if (opt->freeSize != 0) {
    CHECKL(opt->freeTemplate != NULL);
    /* Nothing to check about freeSize */
  }
  return TRUE;
}


/* DebugPoolInit -- init method for a debug pool
 *
 * Someday, this could be split into fence and tag init methods.
 */

ARG_DEFINE_KEY(pool_debug_options, PoolDebugOptions);

static Res DebugPoolInit(Pool pool, ArgList args)
{
  Res res;
  PoolDebugOptions options;
  PoolDebugMixin debug;
  TagInitMethod tagInit;
  Size tagSize;
  ArgStruct arg;

  AVERT(Pool, pool);

  /* TODO: Split this structure into separate keyword arguments,
     now that we can support them. */
  ArgRequire(&arg, args, MPS_KEY_POOL_DEBUG_OPTIONS);
  options = (PoolDebugOptions)arg.val.pool_debug_options;
  
  AVERT(PoolDebugOptions, options);

  /* @@@@ Tag parameters should be taken from options, but tags have */
  /* not been published yet. */
  tagInit = NULL; tagSize = 0;

  res = SuperclassOfPool(pool)->init(pool, args);
  if (res != ResOK)
    return res;

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);

  /* fencepost init */
  /* @@@@ This parses a user argument, options, so it should really */
  /* go through the MPS interface.  The template needs to be copied */
  /* into Addr memory, to avoid breaking <design/type/#addr.use>. */
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

  /* free-checking init */
  /* @@@@ This parses a user argument, options, so it should really */
  /* go through the MPS interface.  The template needs to be copied */
  /* into Addr memory, to avoid breaking <design/type#addr.use>. */
  debug->freeSize = options->freeSize;
  if (debug->freeSize != 0) {
    if (PoolAlignment(pool) % debug->freeSize != 0) {
      res = ResPARAM;
      goto alignFail;
    }
    debug->freeTemplate = options->freeTemplate;
  }

  /* tag init */
  debug->tagInit = tagInit;
  if (debug->tagInit != NULL) {
    debug->tagSize = tagSize + sizeof(tagStruct) - 1;
    /* This pool has to be like the arena control pool: the blocks */
    /* allocated must be accessible using void*. */
    MPS_ARGS_BEGIN(pcArgs) {
      MPS_ARGS_ADD(pcArgs, MPS_KEY_EXTEND_BY, debug->tagSize); /* FIXME: Check this */
      MPS_ARGS_ADD(pcArgs, MPS_KEY_MFS_UNIT_SIZE, debug->tagSize);
      MPS_ARGS_DONE(pcArgs);
      res = PoolCreate(&debug->tagPool, PoolArena(pool), PoolClassMFS(), pcArgs);
    } MPS_ARGS_END(pcArgs);
    if (res != ResOK)
      goto tagFail;
    debug->missingTags = 0;
    SplayTreeInit(&debug->index, TagCompare, SplayTrivUpdate);
  }

  debug->sig = PoolDebugMixinSig;
  AVERT(PoolDebugMixin, debug);
  return ResOK;

tagFail:
alignFail:
  SuperclassOfPool(pool)->finish(pool);
  AVER(res != ResOK);
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
  SuperclassOfPool(pool)->finish(pool);
}


/* freeSplat -- splat free block with splat pattern
 *
 * If base is in a segment, the whole block has to be in it.
 */

static void freeSplat(PoolDebugMixin debug, Pool pool, Addr base, Addr limit)
{
  Addr p, next;
  Size freeSize = debug->freeSize;
  Arena arena;
  Seg seg = NULL;       /* suppress "may be used uninitialized" */
  Bool inSeg;

  AVER(base < limit);

  /* If the block is in a segment, make sure any shield is up. */
  arena = PoolArena(pool);
  inSeg = SegOfAddr(&seg, arena, base);
  if (inSeg) {
    AVER(limit <= SegLimit(seg));
    ShieldExpose(arena, seg);
  }
  /* Write as many copies of the template as fit in the block. */
  for (p = base, next = AddrAdd(p, freeSize);
       next <= limit && p < next /* watch out for overflow in next */;
       p = next, next = AddrAdd(next, freeSize))
    (void)AddrCopy(p, debug->freeTemplate, freeSize);
  /* Fill the tail of the block with a partial copy of the template. */
  if (next > limit || next < p)
    (void)AddrCopy(p, debug->freeTemplate, AddrOffset(p, limit));
  if (inSeg) {
    ShieldCover(arena, seg);
  }
}


/* freeCheck -- check free block for splat pattern */

static Bool freeCheck(PoolDebugMixin debug, Pool pool, Addr base, Addr limit)
{
  Addr p, next;
  Size freeSize = debug->freeSize;
  Res res;
  Arena arena;
  Seg seg = NULL;       /* suppress "may be used uninitialized" */
  Bool inSeg;

  AVER(base < limit);

  /* If the block is in a segment, make sure any shield is up. */
  arena = PoolArena(pool);
  inSeg = SegOfAddr(&seg, arena, base);
  if (inSeg) {
    AVER(limit <= SegLimit(seg));
    ShieldExpose(arena, seg);
  }
  /* Compare this to the AddrCopys in freeSplat. */
  /* Check the complete copies of the template in the block. */
  for (p = base, next = AddrAdd(p, freeSize);
       next <= limit && p < next /* watch out for overflow in next */;
       p = next, next = AddrAdd(next, freeSize))
    if (AddrComp(p, debug->freeTemplate, freeSize) != 0) {
      res = FALSE; goto done;
    }
  /* Check the partial copy of the template at the tail of the block. */
  if (next > limit || next < p)
    if (AddrComp(p, debug->freeTemplate, AddrOffset(p, limit)) != 0) {
      res = FALSE; goto done;
    }
  res = TRUE;

done:
  if (inSeg) {
    ShieldCover(arena, seg);
  }
  return res;
}


/* freeCheckAlloc -- allocation wrapper for free-checking */

static Res freeCheckAlloc(Addr *aReturn, PoolDebugMixin debug, Pool pool,
                          Size size, Bool withReservoir)
{
  Res res;
  Addr new;

  AVER(aReturn != NULL);

  res = SuperclassOfPool(pool)->alloc(&new, pool, size, withReservoir);
  if (res != ResOK)
    return res;
  if (debug->freeSize != 0)
    ASSERT(freeCheck(debug, pool, new, AddrAdd(new, size)),
           "free space corrupted on alloc");

  *aReturn = new;
  return res;
}


/* freeCheckFree -- freeing wrapper for free-checking */

static void freeCheckFree(PoolDebugMixin debug,
                          Pool pool, Addr old, Size size)
{
  if (debug->freeSize != 0)
    freeSplat(debug, pool, old, AddrAdd(old, size));
  SuperclassOfPool(pool)->free(pool, old, size);
}


/* fenceAlloc -- allocation wrapper for fenceposts
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

static Res fenceAlloc(Addr *aReturn, PoolDebugMixin debug, Pool pool,
                      Size size, Bool withReservoir)
{
  Res res;
  Addr new, clientNew;
  Size alignedSize;

  AVER(aReturn != NULL);

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  res = freeCheckAlloc(&new, debug, pool, alignedSize + 2*debug->fenceSize,
                       withReservoir);
  if (res != ResOK)
    return res;
  clientNew = AddrAdd(new, debug->fenceSize);
  /* @@@@ shields? */
  /* start fencepost */
  (void)AddrCopy(new, debug->fenceTemplate, debug->fenceSize);
  /* alignment slop */
  (void)AddrCopy(AddrAdd(clientNew, size),
                 debug->fenceTemplate, alignedSize - size);
  /* end fencepost */
  (void)AddrCopy(AddrAdd(clientNew, alignedSize),
                 debug->fenceTemplate, debug->fenceSize);

  *aReturn = clientNew;
  return res;
}


/* fenceCheck -- check fences of an object */

static Bool fenceCheck(PoolDebugMixin debug, Pool pool, Addr obj, Size size)
{
  Size alignedSize;

  AVERT_CRITICAL(PoolDebugMixin, debug);
  AVERT_CRITICAL(Pool, pool);
  /* Can't check obj */

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  /* @@@@ shields? */
  /* Compare this to the AddrCopys in fenceAlloc */
  return (AddrComp(AddrSub(obj, debug->fenceSize), debug->fenceTemplate,
                   debug->fenceSize) == 0
          && AddrComp(AddrAdd(obj, size), debug->fenceTemplate,
                      alignedSize - size) == 0
          && AddrComp(AddrAdd(obj, alignedSize), debug->fenceTemplate,
                      debug->fenceSize) == 0);
}


/* fenceFree -- freeing wrapper for fenceposts */

static void fenceFree(PoolDebugMixin debug,
                      Pool pool, Addr old, Size size)
{
  Size alignedSize;

  ASSERT(fenceCheck(debug, pool, old, size), "fencepost check on free");

  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  freeCheckFree(debug, pool, AddrSub(old, debug->fenceSize),
                alignedSize + 2*debug->fenceSize);
}


/* tagAlloc -- allocation wrapper for tagged pools */

static Res tagAlloc(PoolDebugMixin debug,
                    Pool pool, Addr new, Size size, Bool withReservoir)
{
  Tag tag;
  Res res;
  Bool b;
  Addr addr;

  UNUSED(pool);
  res = PoolAlloc(&addr, debug->tagPool, debug->tagSize, FALSE);
  if (res != ResOK) {
    if (withReservoir) { /* <design/object-debug/#out-of-space */
      debug->missingTags++;
      return ResOK;
    } else {
      return res;
    }
  }
  tag = (Tag)addr;
  tag->addr = new; tag->size = size;
  TreeInit(TagTree(tag));
  /* In the future, we might call debug->tagInit here. */
  b = SplayTreeInsert(&debug->index, TagTree(tag), &new);
  AVER(b);
  return ResOK;
}


/* tagFree -- deallocation wrapper for tagged pools */

static void tagFree(PoolDebugMixin debug, Pool pool, Addr old, Size size)
{
  Tree node;
  Tag tag;
  Bool b;

  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);
  AVER(size > 0);

  if (!SplayTreeFind(&node, &debug->index, &old)) {
    AVER(debug->missingTags > 0);
    debug->missingTags--;
    return;
  }
  tag = TagOfTree(node);
  AVER(tag->size == size);
  b = SplayTreeDelete(&debug->index, node, &old);
  AVER(b); /* expect tag to be in the tree */
  TreeFinish(node);
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
  Addr new = NULL; /* suppress "may be used uninitialized" warning */
  PoolDebugMixin debug;

  AVER(aReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);
  AVERT(Bool, withReservoir);

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);
  AVERT(PoolDebugMixin, debug);
  if (debug->fenceSize != 0)
    res = fenceAlloc(&new, debug, pool, size, withReservoir);
  else
    res = freeCheckAlloc(&new, debug, pool, size, withReservoir);
  if (res != ResOK)
    return res;
  /* Allocate object first, so it fits even when the tag doesn't. */
  if (debug->tagInit != NULL) {
    res = tagAlloc(debug, pool, new, size, withReservoir);
    if (res != ResOK)
      goto tagFail;
  }

  *aReturn = new;
  return res;

tagFail:
  fenceFree(debug, pool, new, size);
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

  if (debug->fenceSize != 0)
    fenceFree(debug, pool, old, size);
  else
    freeCheckFree(debug, pool, old, size);
  /* Free the object first, to get fences checked before tag. */
  if (debug->tagInit != NULL)
    tagFree(debug, pool, old, size);
}


/* TagWalk -- walk all objects in the pool using tags */

typedef void (*ObjectsStepMethod)(Addr addr, Size size, Format fmt,
                                  Pool pool, void *tagData, void *p);

#define ObjectsStepMethodCheck(f) \
  ((f) != NULL) /* that's the best we can do */

static void TagWalk(Pool pool, ObjectsStepMethod step, void *p)
{
  Tree node;
  PoolDebugMixin debug;

  AVERT(Pool, pool);
  AVERT(ObjectsStepMethod, step);
  /* Can't check p */

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);
  AVERT(PoolDebugMixin, debug);

  node = SplayTreeFirst(&debug->index);
  while (node != NULL) {
    Tag tag = TagOfTree(node);

    step(tag->addr, tag->size, NULL, pool, &tag->userdata, p);
    node = SplayTreeNext(&debug->index, &tag->addr);
  }
}


/* fenceCheckingStep -- step function for DebugPoolCheckFences */

static void fenceCheckingStep(Addr addr, Size size, Format fmt,
                              Pool pool, void *tagData, void *p)
{
  /* no need to check arguments checked in the caller */
  UNUSED(fmt); UNUSED(tagData);
  ASSERT(fenceCheck((PoolDebugMixin)p, pool, addr, size),
         "fencepost check requested by client");
}


/* DebugPoolCheckFences -- check all the fenceposts in the pool */

void DebugPoolCheckFences(Pool pool)
{
  PoolDebugMixin debug;

  AVERT(Pool, pool);
  debug = DebugPoolDebugMixin(pool);
  if (debug == NULL)
    return;
  AVERT(PoolDebugMixin, debug);

  if (debug->fenceSize != 0)
    TagWalk(pool, fenceCheckingStep, (void *)debug);
}


/* DebugPoolFreeSplat -- if in a free-checking debug pool, splat free block */

void DebugPoolFreeSplat(Pool pool, Addr base, Addr limit)
{
  PoolDebugMixin debug;

  AVERT(Pool, pool);
  AVER(PoolHasAddr(pool, base));
  AVER(PoolHasAddr(pool, AddrSub(limit, 1)));

  debug = DebugPoolDebugMixin(pool);
  if (debug != NULL) {
    AVERT(PoolDebugMixin, debug);
    if (debug->freeSize != 0)
      freeSplat(debug, pool, base, limit);
  }
}


/* DebugPoolFreeCheck -- if in a free-checking debug pool, check free block */

void DebugPoolFreeCheck(Pool pool, Addr base, Addr limit)
{
  PoolDebugMixin debug;

  AVERT(Pool, pool);
  AVER(PoolHasAddr(pool, base));
  AVER(PoolHasAddr(pool, AddrSub(limit, 1)));

  debug = DebugPoolDebugMixin(pool);
  if (debug != NULL) {
    AVERT(PoolDebugMixin, debug);
    if (debug->freeSize != 0)
      ASSERT(freeCheck(debug, pool, base, limit),
             "free space corrupted on release");
  }
}


/* freeCheckingStep -- step function for DebugPoolCheckFreeSpace */

static void freeCheckingStep(Addr base, Addr limit, Pool pool, void *p)
{
  /* no need to check arguments checked in the caller */
  ASSERT(freeCheck((PoolDebugMixin)p, pool, base, limit),
         "free space corrupted on client check");
}


/* DebugPoolCheckFreeSpace -- check free space in the pool for overwrites */

void DebugPoolCheckFreeSpace(Pool pool)
{
  PoolDebugMixin debug;

  AVERT(Pool, pool);
  debug = DebugPoolDebugMixin(pool);
  if (debug == NULL)
    return;
  AVERT(PoolDebugMixin, debug);

  if (debug->freeSize != 0)
    PoolFreeWalk(pool, freeCheckingStep, (void *)debug);
}


/* PoolClassMixInDebug -- mix in the debug support for class init */

void PoolClassMixInDebug(PoolClass class)
{
  /* Can't check class because it's not initialized yet */
  class->init = DebugPoolInit;
  class->finish = DebugPoolFinish;
  class->alloc = DebugPoolAlloc;
  class->free = DebugPoolFree;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

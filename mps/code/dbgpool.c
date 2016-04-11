/* dbgpool.c: POOL DEBUG MIXIN
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
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
#define TagOfTree(tree) TREE_ELT(tag, treeStruct, tree)

typedef tagStruct *Tag;


/* tag init methods: copying the user-supplied data into the tag */

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

static TreeKey TagKey(Tree node)
{
  return &TagOfTree(node)->addr;
}


/* PoolDebugMixinCheck -- check a PoolDebugMixin */

Bool PoolDebugMixinCheck(PoolDebugMixin debug)
{
  /* Nothing to check about fenceTemplate */
  /* Nothing to check about fenceSize */
  /* Nothing to check about freeTemplate */
  /* Nothing to check about freeSize */
  if (debug->tagInit != NULL) {
    CHECKL(FUNCHECK(debug->tagInit));
    /* Nothing to check about tagSize */
    CHECKD(Pool, debug->tagPool);
    CHECKL(COMPATTYPE(Addr, void*)); /* tagPool relies on this */
    /* Nothing to check about missingTags */
    CHECKD(SplayTree, &debug->index);
  }
  UNUSED(debug); /* see <code/mpm.c#check.unused> */
  return TRUE;
}


/* DebugPoolDebugMixin -- gets the debug mixin, if any */

#define DebugPoolDebugMixin(pool) (Method(Pool, pool, debugMixin)(pool))


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

ARG_DEFINE_KEY(POOL_DEBUG_OPTIONS, PoolDebugOptions);

static PoolDebugOptionsStruct debugPoolOptionsDefault = {
  "POST", 4, "DEAD", 4,
};

static Res DebugPoolInit(Pool pool, Arena arena, PoolClass class, ArgList args)
{
  Res res;
  PoolDebugOptions options = &debugPoolOptionsDefault;
  PoolDebugMixin debug;
  TagInitFunction tagInit;
  Size tagSize;
  ArgStruct arg;

  AVER(pool != NULL);
  AVERT(Arena, arena);
  AVERT(PoolClass, class);
  AVERT(ArgList, args);

  if (ArgPick(&arg, args, MPS_KEY_POOL_DEBUG_OPTIONS))
    options = (PoolDebugOptions)arg.val.pool_debug_options;
  
  AVERT(PoolDebugOptions, options);

  /* @@@@ Tag parameters should be taken from options, but tags have */
  /* not been published yet. */
  tagInit = NULL; tagSize = 0;

  res = SuperclassPoly(Pool, class)->init(pool, arena, class, args);
  if (res != ResOK)
    return res;

  SetClassOfPoly(pool, class);
  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);

  /* fencepost init */
  /* @@@@ This parses a user argument, options, so it should really */
  /* go through the MPS interface.  The template needs to be copied */
  /* into Addr memory, to avoid breaking <design/type/#addr.use>. */
  debug->fenceSize = options->fenceSize;
  if (debug->fenceSize != 0) {
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
    debug->freeTemplate = options->freeTemplate;
  }

  /* tag init */
  debug->tagInit = tagInit;
  if (debug->tagInit != NULL) {
    debug->tagSize = tagSize + sizeof(tagStruct) - 1;
    /* This pool has to be like the arena control pool: the blocks */
    /* allocated must be accessible using void*. */
    MPS_ARGS_BEGIN(pcArgs) {
      /* By setting EXTEND_BY to debug->tagSize we get the smallest
         possible extensions compatible with the tags, and so the
         least amount of wasted space. */
      MPS_ARGS_ADD(pcArgs, MPS_KEY_EXTEND_BY, debug->tagSize);
      MPS_ARGS_ADD(pcArgs, MPS_KEY_MFS_UNIT_SIZE, debug->tagSize);
      res = PoolCreate(&debug->tagPool, PoolArena(pool), PoolClassMFS(), pcArgs);
    } MPS_ARGS_END(pcArgs);
    if (res != ResOK)
      goto tagFail;
    debug->missingTags = 0;
    SplayTreeInit(&debug->index, TagCompare, TagKey, SplayTrivUpdate);
  }

  debug->sig = PoolDebugMixinSig;
  AVERT(PoolDebugMixin, debug);
  return ResOK;

tagFail:
  SuperclassPoly(Pool, class)->finish(pool);
  AVER(res != ResOK);
  return res;
}


/* DebugPoolFinish -- finish method for a debug pool */

static void DebugPoolFinish(Pool pool)
{
  PoolDebugMixin debug;
  PoolClass class;

  AVERT(Pool, pool);

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);
  AVERT(PoolDebugMixin, debug);
  if (debug->tagInit != NULL) {
    SplayTreeFinish(&debug->index);
    PoolDestroy(debug->tagPool);
  }
  class = ClassOfPoly(Pool, pool);
  SuperclassPoly(Pool, class)->finish(pool);
}


/* patternIterate -- call visitor for occurrences of pattern between
 * base and limit
 *
 * pattern is an arbitrary pattern that's size bytes long.
 *
 * Imagine that the entirety of memory were covered by contiguous
 * copies of pattern starting at address 0. Then call visitor for each
 * copy (or part) of pattern that lies between base and limit. In each
 * call, target is the address of the copy or part (where base <=
 * target < limit); source is the corresponding byte of the pattern
 * (where pattern <= source < pattern + size); and size is the length
 * of the copy or part.
 */

typedef Bool (*patternVisitor)(Addr target, ReadonlyAddr source, Size size);

static Bool patternIterate(ReadonlyAddr pattern, Size size,
                           Addr base, Addr limit, patternVisitor visitor)
{
  Addr p;

  AVER(pattern != NULL);
  AVER(0 < size);
  AVER(base != NULL);
  AVER(base <= limit);

  p = base;
  while (p < limit) {
    Addr end = AddrAdd(p, size);
    Addr rounded = AddrRoundUp(p, size);
    Size offset = (Word)p % size;
    if (end < p || rounded < p) {
      /* Address range overflow */
      break;
    } else if (p == rounded && end <= limit) {
      /* Room for a whole copy */
      if (!(*visitor)(p, pattern, size))
        return FALSE;
      p = end;
    } else if (p < rounded && rounded <= end && rounded <= limit) {
      /* Copy up to rounded */
      if (!(*visitor)(p, ReadonlyAddrAdd(pattern, offset),
                      AddrOffset(p, rounded)))
        return FALSE;
      p = rounded;
    } else {
      /* Copy up to limit */
      AVER(limit <= end);
      AVER(p == rounded || limit <= rounded);
      if (!(*visitor)(p, ReadonlyAddrAdd(pattern, offset),
                      AddrOffset(p, limit)))
        return FALSE;
      p = limit;
    }
  }

  return TRUE;
}


/* patternCopy -- copy pattern to fill a range
 *
 * Fill the range of addresses from base (inclusive) to limit
 * (exclusive) with copies of pattern (which is size bytes long).
 */

static Bool patternCopyVisitor(Addr target, ReadonlyAddr source, Size size)
{
  (void)AddrCopy(target, source, size);
  return TRUE;
}

static void patternCopy(ReadonlyAddr pattern, Size size, Addr base, Addr limit)
{
  (void)patternIterate(pattern, size, base, limit, patternCopyVisitor);
}


/* patternCheck -- check pattern against a range
 *
 * Compare the range of addresses from base (inclusive) to limit
 * (exclusive) with copies of pattern (which is size bytes long). The
 * copies of pattern must be arranged so that fresh copies start at
 * aligned addresses wherever possible.
 */

static Bool patternCheckVisitor(Addr target, ReadonlyAddr source, Size size)
{
  return AddrComp(target, source, size) == 0;
}

static Bool patternCheck(ReadonlyAddr pattern, Size size, Addr base, Addr limit)
{
  return patternIterate(pattern, size, base, limit, patternCheckVisitor);
}


/* debugPoolSegIterate -- iterate over a range of segments in an arena
 *
 * Expects to be called on a range corresponding to objects withing a
 * single pool.
 * 
 * NOTE: This relies on pools consistently using segments
 * contiguously.
 */

static void debugPoolSegIterate(Arena arena, Addr base, Addr limit,
                                void (*visitor)(Arena, Seg))
{
  Seg seg;

  if (SegOfAddr(&seg, arena, base)) {
    do {
      base = SegLimit(seg);
      (*visitor)(arena, seg);
    } while (base < limit && SegOfAddr(&seg, arena, base));
    AVER(base >= limit); /* shouldn't run out of segments */
  }
}

static void debugPoolShieldExpose(Arena arena, Seg seg)
{
  ShieldExpose(arena, seg);
}

static void debugPoolShieldCover(Arena arena, Seg seg)
{
  ShieldCover(arena, seg);
}


/* freeSplat -- splat free block with splat pattern */

static void freeSplat(PoolDebugMixin debug, Pool pool, Addr base, Addr limit)
{
  Arena arena;

  AVER(base < limit);

  /* If the block is in one or more segments, make sure the segments
     are exposed so that we can overwrite the block with the pattern. */
  arena = PoolArena(pool);
  debugPoolSegIterate(arena, base, limit, debugPoolShieldExpose);
  patternCopy(debug->freeTemplate, debug->freeSize, base, limit);
  debugPoolSegIterate(arena, base, limit, debugPoolShieldCover);
}


/* freeCheck -- check free block for splat pattern */

static Bool freeCheck(PoolDebugMixin debug, Pool pool, Addr base, Addr limit)
{
  Bool res;
  Arena arena;

  AVER(base < limit);

  /* If the block is in one or more segments, make sure the segments
     are exposed so we can read the pattern. */
  arena = PoolArena(pool);
  debugPoolSegIterate(arena, base, limit, debugPoolShieldExpose);
  res = patternCheck(debug->freeTemplate, debug->freeSize, base, limit);
  debugPoolSegIterate(arena, base, limit, debugPoolShieldCover);
  return res;
}


/* freeCheckAlloc -- allocation wrapper for free-checking */

static Res freeCheckAlloc(Addr *aReturn, PoolDebugMixin debug, Pool pool,
                          Size size)
{
  Res res;
  Addr new;
  PoolClass class;

  AVER(aReturn != NULL);

  class = ClassOfPoly(Pool, pool);
  res = SuperclassPoly(Pool, class)->alloc(&new, pool, size);
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
  PoolClass class;
  if (debug->freeSize != 0)
    freeSplat(debug, pool, old, AddrAdd(old, size));
  class = ClassOfPoly(Pool, pool);
  SuperclassPoly(Pool, class)->free(pool, old, size);
}


/* fenceAlloc -- allocation wrapper for fenceposts
 *
 * Allocates an object, adding fenceposts on both sides.  Layout:
 *
 * |----------|-------------------------------------|------|----------|
 *   start fp              client object              slop    end fp
 *
 * slop is the extra allocation from rounding up the client request to
 * the pool's alignment. The fenceposting code adds this slop so that
 * there's a better chance of the end fencepost being flush with the
 * next object (though it can't be guaranteed, since the underlying
 * pool could have allocated an even larger block). The alignment slop
 * is filled from the fencepost template as well.
 *
 * Keep in sync with fenceCheck.
 */

static Res fenceAlloc(Addr *aReturn, PoolDebugMixin debug, Pool pool,
                      Size size)
{
  Res res;
  Addr obj, startFence, clientNew, clientLimit, limit;
  Size alignedFenceSize, alignedSize;

  AVER(aReturn != NULL);
  AVERT(PoolDebugMixin, debug);
  AVERT(Pool, pool);

  alignedFenceSize = SizeAlignUp(debug->fenceSize, PoolAlignment(pool));
  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  res = freeCheckAlloc(&obj, debug, pool,
                       alignedSize + 2 * alignedFenceSize);
  if (res != ResOK)
    return res;

  startFence = obj;
  clientNew = AddrAdd(startFence, alignedFenceSize);
  clientLimit = AddrAdd(clientNew, size);
  limit = AddrAdd(clientNew, alignedSize + alignedFenceSize);

  /* @@@@ shields? */
  patternCopy(debug->fenceTemplate, debug->fenceSize, startFence, clientNew);
  patternCopy(debug->fenceTemplate, debug->fenceSize, clientLimit, limit);

  *aReturn = clientNew;
  return ResOK;
}


/* fenceCheck -- check fences of an object
 *
 * Keep in sync with fenceAlloc.
 */

static Bool fenceCheck(PoolDebugMixin debug, Pool pool, Addr obj, Size size)
{
  Addr startFence, clientNew, clientLimit, limit;
  Size alignedFenceSize, alignedSize;

  AVERT_CRITICAL(PoolDebugMixin, debug);
  AVERT_CRITICAL(Pool, pool);
  /* Can't check obj */

  alignedFenceSize = SizeAlignUp(debug->fenceSize, PoolAlignment(pool));
  alignedSize = SizeAlignUp(size, PoolAlignment(pool));

  startFence = AddrSub(obj, alignedFenceSize);
  clientNew = obj;
  clientLimit = AddrAdd(clientNew, size);
  limit = AddrAdd(clientNew, alignedSize + alignedFenceSize);

  /* @@@@ shields? */
  return patternCheck(debug->fenceTemplate, debug->fenceSize,
                      startFence, clientNew)
    && patternCheck(debug->fenceTemplate, debug->fenceSize,
                    clientLimit, limit);
}


/* fenceFree -- freeing wrapper for fenceposts */

static void fenceFree(PoolDebugMixin debug,
                      Pool pool, Addr old, Size size)
{
  Size alignedFenceSize, alignedSize;

  ASSERT(fenceCheck(debug, pool, old, size), "fencepost check on free");

  alignedFenceSize = SizeAlignUp(debug->fenceSize, PoolAlignment(pool));
  alignedSize = SizeAlignUp(size, PoolAlignment(pool));
  freeCheckFree(debug, pool, AddrSub(old, alignedFenceSize),
                alignedSize + 2 * alignedFenceSize);
}


/* tagAlloc -- allocation wrapper for tagged pools */

static Res tagAlloc(PoolDebugMixin debug,
                    Pool pool, Addr new, Size size)
{
  Tag tag;
  Res res;
  Bool b;
  Addr addr;

  UNUSED(pool);
  res = PoolAlloc(&addr, debug->tagPool, debug->tagSize);
  if (res != ResOK)
    return res;
  tag = (Tag)addr;
  tag->addr = new; tag->size = size;
  TreeInit(TagTree(tag));
  /* In the future, we might call debug->tagInit here. */
  b = SplayTreeInsert(&debug->index, TagTree(tag));
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
  AVER(tag->addr == old);
  b = SplayTreeDelete(&debug->index, node);
  AVER(b); /* expect tag to be in the tree */
  TreeFinish(node);
  PoolFree(debug->tagPool, (Addr)tag, debug->tagSize);
}


/* DebugPoolAlloc -- alloc method for a debug pool
 *
 * Eventually, tag init args will need to be handled somewhere here.
 */

static Res DebugPoolAlloc(Addr *aReturn, Pool pool, Size size)
{
  Res res;
  Addr new = NULL; /* suppress "may be used uninitialized" warning */
  PoolDebugMixin debug;

  AVER(aReturn != NULL);
  AVERT(Pool, pool);
  AVER(size > 0);

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);
  AVERT(PoolDebugMixin, debug);
  if (debug->fenceSize != 0)
    res = fenceAlloc(&new, debug, pool, size);
  else
    res = freeCheckAlloc(&new, debug, pool, size);
  if (res != ResOK)
    return res;
  /* Allocate object first, so it fits even when the tag doesn't. */
  if (debug->tagInit != NULL) {
    res = tagAlloc(debug, pool, new, size);
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

typedef void (*ObjectsVisitor)(Addr addr, Size size, Format fmt,
                               Pool pool, void *tagData, void *p);

static void TagWalk(Pool pool, ObjectsVisitor visitor, void *p)
{
  Tree node;
  PoolDebugMixin debug;

  AVERT(Pool, pool);
  AVER(FUNCHECK(visitor));
  /* Can't check p */

  debug = DebugPoolDebugMixin(pool);
  AVER(debug != NULL);
  AVERT(PoolDebugMixin, debug);

  node = SplayTreeFirst(&debug->index);
  while (node != TreeEMPTY) {
    Tag tag = TagOfTree(node);

    (*visitor)(tag->addr, tag->size, NULL, pool, &tag->userdata, p);
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

/* freelist.c: FREE LIST ALLOCATOR IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2013 Ravenbrook Limited.  See end of file for license.
 *
 * .sources: <design/freelist/>.
 */

#include "freelist.h"
#include "mpm.h"

SRCID(freelist, "$Id$");


/* See <design/freelist/#impl.grain.align> */
#define freelistMinimumAlignment ((Align)sizeof(void *))


/* See <design/freelist/#impl.grain> */
typedef struct FreelistGrainStruct {
  Addr next;
} FreelistGrainStruct;


/* FreelistGrain* -- Getters and setters for grains
 *
 * See <design/freelist/#impl.grain>.
 */
#define FreelistGrainBase(grain) ((Addr)(grain))
#define FreelistGrainLimit(fl, grain) \
  AddrAdd(FreelistGrainBase(grain), FreelistGrainSize(fl))
#define FreelistGrainSize(fl) ((fl)->alignment)
#define FreelistGrainNext(grain) \
  ((FreelistGrain)(((FreelistGrain)(grain))->next))
#define FreelistGrainSetNext(grain, _next) \
  BEGIN ((FreelistGrain)(grain))->next = (void *)(_next); END


/* FreelistGrainInit -- return grain for the range [base, limit). */

static FreelistGrain FreelistGrainInit(Freelist fl, Addr base, Addr limit)
{
  FreelistGrain grain = (FreelistGrain)base;
  AVER(AddrOffset(base, limit) == FreelistGrainSize(fl));
  FreelistGrainSetNext(grain, NULL);
  return grain;
}


/* See <design/freelist/#impl.block> */
typedef struct FreelistBlockStruct {
  Addr next;
  Addr limit;
} FreelistBlockStruct;


/* FreelistBlock* -- Getters and setters for blocks
 *
 * See <design/freelist/#impl.block>.
 */
#define FreelistBlockBase(block) ((Addr)(block))
#define FreelistBlockLimit(block) (((FreelistBlock)(block))->limit)
#define FreelistBlockSize(block) \
  AddrOffset(FreelistBlockBase(block), FreelistBlockLimit(block))
#define FreelistBlockNext(block) \
  ((FreelistBlock)(((FreelistBlock)(block))->next))
#define FreelistBlockSetNext(block, _next) \
  BEGIN ((FreelistBlock)(block))->next = (void *)(_next); END
#define FreelistBlockSetLimit(block, _limit)                            \
  BEGIN {                                                               \
    AVER(AddrOffset(block, _limit) >= sizeof(FreelistBlockStruct));     \
    ((FreelistBlock)(block))->limit = (void *)(_limit);                 \
  } END


/* FreelistBlockCheck -- check block. */

static Bool FreelistBlockCheck(FreelistBlock block)
{
  CHECKL(FreelistBlockBase(block) < FreelistBlockLimit(block));
  return TRUE;
}


/* FreelistBlockInit -- return block for the range [base, limit). */

static FreelistBlock FreelistBlockInit(Addr base, Addr limit)
{
  FreelistBlock block;
  AVER(AddrOffset(base, limit) >= sizeof(FreelistBlockStruct));
  block = (FreelistBlock)base;
  FreelistBlockSetNext(block, NULL);
  FreelistBlockSetLimit(block, limit);
  return block;
}


Bool FreelistCheck(Freelist fl)
{
  CHECKS(Freelist, fl);
  /* See <design/freelist/#impl.grain.align> */
  CHECKL(AlignIsAligned(fl->alignment, freelistMinimumAlignment));
  /* can't check blockList or grainList more */
  /* Checking blockListSize and grainListSize is too laborious without
     a List ADT */
  return TRUE;
}


Res FreelistInit(Freelist fl, Align alignment)
{
  /* See <design/freelist/#impl.grain.align> */
  if (!AlignIsAligned(alignment, freelistMinimumAlignment))
    return ResPARAM;

  fl->alignment = alignment;
  fl->blockList = NULL;
  fl->blockListSize = 0;
  fl->grainList = NULL;
  fl->grainListSize = 0;

  fl->sig = FreelistSig;
  AVERT(Freelist, fl);
  return ResOK;
}


void FreelistFinish(Freelist fl)
{
  AVERT(Freelist, fl);
  fl->sig = SigInvalid;

  fl->blockList = NULL;
  fl->grainList = NULL;
}


/* freelistGrainSetPrevNext -- make 'next' be the next grain in the
 * list after 'prev', or make it the first grain in the list if 'prev'
 * is NULL. Update the count of grains by 'delta'.
 */
static void freelistGrainSetPrevNext(Freelist fl, FreelistGrain prev,
                                     FreelistGrain next, int delta)
{
  if (prev) {
    FreelistGrainSetNext(prev, next);
  } else {
    fl->grainList = next;
  }
  if (delta < 0)
    AVER(fl->grainListSize >= -delta);
  fl->grainListSize += delta;
}


/* freelistBlockSetPrevNext -- make 'next' be the next block in the
 * list after 'prev', or make it the first block in the list if 'prev'
 * is NULL. Update the count of blocks by 'delta'.
 */
static void freelistBlockSetPrevNext(Freelist fl, FreelistBlock prev,
                                     FreelistBlock next, int delta)
{
  if (prev) {
    FreelistBlockSetNext(prev, next);
  } else {
    fl->blockList = next;
  }
  if (delta < 0)
    AVER(fl->blockListSize >= -delta);
  fl->blockListSize += delta;
}


Res FreelistInsert(Range rangeReturn, Freelist fl, Range range)
{
  FreelistGrain grainPrev, grainCur, grainNext, grainNew;
  FreelistBlock blockPrev, blockCur, blockNext, blockNew;
  Addr base, limit;
  Size size;
  Bool coalesceLeft, coalesceRight;
  Res res = ResOK;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, fl->alignment));

  size = RangeSize(range);
  AVER(size >= FreelistGrainSize(fl));
  base = RangeBase(range);
  limit = RangeLimit(range);

  /* In order to correctly detect erroneous input (that is, range
   * overlapping with some existing block or grain), we must check for
   * overlap with both the block list and the grain list *before*
   * trying to coalesce with either, lest we find ourselves in the
   * embarrassing position of having coalesced the range with a block
   * only to find that it overlaps with a grain (or vice versa).
   */
  grainPrev = NULL;
  grainCur = fl->grainList;
  grainNext = NULL;
  while (grainCur) {
    if (base < FreelistGrainLimit(fl, grainCur)
        && FreelistGrainBase(grainCur) < limit)
      return ResFAIL; /* range overlaps with grainCur */
    if (limit <= FreelistGrainBase(grainCur))
      break;
    /* The following test is a special case for grains. In the block
     * case we can safely wait until the next time around the loop
     * because if range coalesces on the left with blockPrev then
     * blockPrev->limit can be adjusted. But in the grain case if we
     * coalesce we remove the grain from the grain list, so we have to
     * ensure that we retain a pointer to the previous grain so that
     * we can update grainPrev->next.
     */
    if (base == FreelistGrainLimit(fl, grainCur))
      break;
    grainNext = FreelistGrainNext(grainCur);
    if (grainNext)
      AVER(FreelistGrainLimit(fl, grainCur) < FreelistGrainBase(grainNext));
    grainPrev = grainCur;
    grainCur = grainNext;
  }

  blockPrev = NULL;
  blockCur = fl->blockList;
  while (blockCur) {
    AVERT(FreelistBlock, blockCur);
    if (base < FreelistBlockLimit(blockCur)
        && FreelistBlockBase(blockCur) < limit)
      return ResFAIL; /* range overlaps with blockCur */
    if (limit <= FreelistBlockBase(blockCur))
      break;
    blockNext = FreelistBlockNext(blockCur);
    if (blockNext)
      AVER(FreelistBlockLimit(blockCur) < FreelistBlockBase(blockNext));
    blockPrev = blockCur;
    blockCur = blockNext;
  }

  /* Now we know that range does not overlap with any block or grain,
   * and if it coalesces then it does so with blockPrev on the left,
   * with blockCur on the right, with grainCur on either side, and/or
   * with grainNext on the right. Try the grains first.
   */
  if (grainCur && limit == FreelistGrainBase(grainCur)) {
    /* Coalesce with grainCur on the right and delete grainCur */
    limit = FreelistGrainLimit(fl, grainCur);
    freelistGrainSetPrevNext(fl, grainPrev, grainNext, -1);

  } else if (grainCur && base == FreelistGrainLimit(fl, grainCur)
             && grainNext && limit == FreelistGrainBase(grainNext)) {
    /* Coalesce with grainCur on the left and grainNext on the right
     * and delete both grains. */
    base = FreelistGrainBase(grainCur);
    limit = FreelistGrainLimit(fl, grainNext);
    freelistGrainSetPrevNext(fl, grainPrev, FreelistGrainNext(grainNext), -2);

  } else if (grainCur && base == FreelistGrainLimit(fl, grainCur)) {
    /* Coalesce with grainCur on the left and delete grainCur */
    base = FreelistGrainBase(grainCur);
    freelistGrainSetPrevNext(fl, grainPrev, grainNext, -1);
  }

  /* now try to coalesce with blocks */
  coalesceLeft = (blockPrev && base == FreelistBlockLimit(blockPrev));
  coalesceRight = (blockCur && limit == FreelistBlockBase(blockCur));

  if (coalesceLeft && coalesceRight) {
    base = FreelistBlockBase(blockPrev);
    limit = FreelistBlockLimit(blockCur);
    FreelistBlockSetLimit(blockPrev, limit);
    freelistBlockSetPrevNext(fl, blockPrev, blockNext, -1);

  } else if (coalesceLeft) {
    base = FreelistBlockBase(blockPrev);
    FreelistBlockSetLimit(blockPrev, limit);

  } else if (coalesceRight) {
    limit = FreelistBlockLimit(blockCur);
    blockCur = FreelistBlockInit(base, limit);
    FreelistBlockSetNext(blockCur, blockNext);

  } else if (size > FreelistGrainSize(fl)) {
    /* failed to coalesce: add new block */
    blockNew = FreelistBlockInit(base, limit);
    FreelistBlockSetNext(blockNew, blockCur);
    freelistBlockSetPrevNext(fl, blockPrev, blockNew, +1);

  } else {
    /* failed to coalesce: add new grain */
    grainNew = FreelistGrainInit(fl, base, limit);
    FreelistGrainSetNext(grainNew, grainNext);
    freelistGrainSetPrevNext(fl, grainPrev, grainNew, +1);
  }

  RangeInit(rangeReturn, base, limit);
  return ResOK;
}


/* freelistInsertGrain -- Insert a grain (that is known to be isolated
 * from all blocks and grains) into the free list.
 */
static void freelistInsertGrain(Freelist fl, FreelistGrain grain)
{
  FreelistGrain prev, cur, next;

  prev = NULL;
  cur = fl->grainList;
  while (cur) {
    if (prev)
      AVER(FreelistGrainLimit(fl, prev) < FreelistGrainBase(cur));
    if ((prev == NULL
         || FreelistGrainLimit(fl, prev) < FreelistGrainBase(grain))
        && FreelistGrainLimit(fl, grain) < FreelistGrainBase(cur))
      break;
    AVER(FreelistGrainLimit(fl, grain) != FreelistGrainBase(cur));
    AVER(FreelistGrainBase(grain) != FreelistGrainLimit(fl, cur));
    next = FreelistGrainNext(cur);
    prev = cur;
    cur = next;
  }
  
  FreelistGrainSetNext(grain, cur);
  freelistGrainSetPrevNext(fl, prev, grain, +1);
}


/* freelistDeleteFromBlock -- delete 'range' from 'block' (it is known
 * to be a subset of that block); update 'rangeReturn' to the original
 * range of 'block' and update the block list accordingly: 'prev' is
 * the block on the list just before 'block', or NULL if 'block' is
 * the first block on the list.
 */
static void freelistDeleteFromBlock(Range rangeReturn, Freelist fl,
                                    Range range, FreelistBlock prev,
                                    FreelistBlock block)
{
  FreelistBlock next, blockNew;
  Addr base, limit, blockBase, blockLimit;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, fl->alignment));
  AVER(prev == NULL || FreelistBlockNext(prev) == block);
  AVER(block != NULL);

  AVER(FreelistBlockBase(block) <= RangeBase(range));
  AVER(RangeLimit(range) <= FreelistBlockLimit(block));

  base = RangeBase(range);
  limit = RangeLimit(range);
  blockBase = FreelistBlockBase(block);
  blockLimit = FreelistBlockLimit(block);
  next = FreelistBlockNext(block);

  /* There are NINE (count 'em!) cases here, because on both sides
   * the deletion might leave no fragment, a grain, or a block. */

  if (base == blockBase && limit == blockLimit) {
    /* No fragment at left; no fragment at right. */
    freelistBlockSetPrevNext(fl, prev, next, -1);

  } else if (base == blockBase
             && FreelistGrainLimit(fl, limit) == blockLimit)
  {
    /* No fragment at left; grain at right. */
    freelistBlockSetPrevNext(fl, prev, next, -1);
    freelistInsertGrain(fl, FreelistGrainInit(fl, limit, blockLimit));

  } else if (base == blockBase) {
    /* No fragment at left; block at right. */
    block = FreelistBlockInit(limit, blockLimit);
    FreelistBlockSetNext(block, next);
    freelistBlockSetPrevNext(fl, prev, block, 0);

  } else if (FreelistGrainLimit(fl, blockBase) == base
             && limit == blockLimit)
  {
    /* Grain at left; no fragment at right. */
    freelistBlockSetPrevNext(fl, prev, next, -1);
    freelistInsertGrain(fl, FreelistGrainInit(fl, blockBase, base));

  } else if (limit == blockLimit) {        
    /* Block at left; no frament at right. */
    FreelistBlockSetLimit(block, base);

  } else if (FreelistGrainLimit(fl, blockBase) == base
             && FreelistGrainLimit(fl, limit) == blockLimit)
  {
    /* Grain at left; grain at right. */
    freelistBlockSetPrevNext(fl, prev, next, -1);
    freelistInsertGrain(fl, FreelistGrainInit(fl, blockBase, base));
    freelistInsertGrain(fl, FreelistGrainInit(fl, limit, blockLimit));

  } else if (FreelistGrainLimit(fl, blockBase) == base) {
    /* Grain at left; block at right. */
    block = FreelistBlockInit(limit, blockLimit);
    FreelistBlockSetNext(block, next);
    freelistBlockSetPrevNext(fl, prev, block, 0);
    freelistInsertGrain(fl, FreelistGrainInit(fl, blockBase, base));

  } else if (FreelistGrainLimit(fl, limit) == blockLimit) {
    /* Block at left; grain at right. */
    AVER(FreelistGrainLimit(fl, blockBase) < base);
    FreelistBlockSetLimit(block, base);
    freelistInsertGrain(fl, FreelistGrainInit(fl, limit, blockLimit));

  } else {
    /* Block at left; block at right. */
    FreelistBlockSetLimit(block, base);
    blockNew = FreelistBlockInit(limit, blockLimit);
    FreelistBlockSetNext(blockNew, next);
    freelistBlockSetPrevNext(fl, block, blockNew, +1);
  }

  RangeInit(rangeReturn, blockBase, blockLimit);
}


/* freelistDeleteFromBlockList -- if 'range' is found in the block
 * list, delete it, update 'rangeReturn' to the range of the block
 * that contained 'range' and return ResOK. Otherwise, return ResFAIL.
 * (This might mean that the range was not found at all in the block
 * list, or that it was only partially found.)
 */
static Res freelistDeleteFromBlockList(Range rangeReturn, Freelist fl,
                                       Range range)
{
  Res res;
  FreelistBlock prev, cur, next, blockNew;
  Addr base, limit;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);

  base = RangeBase(range);
  limit = RangeLimit(range);

  prev = NULL;
  cur = fl->blockList;
  while (cur) {
    Addr blockBase, blockLimit;
    AVERT(FreelistBlock, cur);
    blockBase = FreelistBlockBase(cur);
    blockLimit = FreelistBlockLimit(cur);

    if (limit <= blockBase)
      return ResFAIL; /* not found */
    if (base <= blockLimit) {
      if (base < blockBase || blockLimit < limit)
        return ResFAIL; /* partially overlapping */
      freelistDeleteFromBlock(rangeReturn, fl, range, prev, cur);
      return ResOK;
    }
    
    next = FreelistBlockNext(cur);
    prev = cur;
    cur = next;
  }

  /* Range not found in block list. */
  return ResFAIL;
}


/* freelistDeleteFromGrainList -- if 'range' is found in the grain
 * list, delete it, update 'rangeReturn' to the range of the grain
 * that contained 'range' and return ResOK. Otherwise, return ResFAIL.
 */
static Res freelistDeleteFromGrainList(Range rangeReturn, Freelist fl,
                                       Range range)
{
  Addr base, limit;
  FreelistGrain prev, cur, next;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  
  if (RangeSize(range) != FreelistGrainSize(fl))
    return ResFAIL;

  base = RangeBase(range);
  limit = RangeLimit(range);

  prev = NULL;
  cur = fl->grainList;
  while (cur) {
    Addr grainBase, grainLimit;
    grainBase = FreelistGrainBase(cur);
    grainLimit = FreelistGrainLimit(fl, cur);
    next = FreelistGrainNext(cur);

    if (prev != NULL)
      AVER(FreelistGrainLimit(fl, prev) < grainBase);
    if (limit <= grainBase)
      return ResFAIL; /* not found */
    if (base == grainBase && limit == grainLimit) {
      freelistGrainSetPrevNext(fl, prev, next, -1);
      RangeInit(rangeReturn, grainBase, grainLimit);
      return ResOK;
    }
    /* Partial overlap can't happen if everything's aligned. */
    AVER(base >= grainLimit);

    prev = cur;
    cur = next;
  }

  return ResFAIL; /* not found */
}


Res FreelistDelete(Range rangeReturn, Freelist fl, Range range)
{
  Res res;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, fl->alignment));

  if (RangeSize(range) == FreelistGrainSize(fl)) {
    res = freelistDeleteFromGrainList(rangeReturn, fl, range);
  } else {
    res = freelistDeleteFromBlockList(rangeReturn, fl, range);
  }
  return res;
}


void FreelistIterate(Freelist fl, FreelistIterateMethod iterate,
                     void *closureP, Size closureS)
{
  FreelistBlock blockPrev, blockCur, blockNext;
  FreelistGrain grainPrev, grainCur, grainNext;

  AVERT(Freelist, fl);
  AVER(FUNCHECK(iterate));

  blockPrev = NULL;
  blockCur = fl->blockList;
  grainPrev = NULL;
  grainCur = fl->grainList;
  while (blockCur || grainCur) {
    Bool delete = FALSE;
    RangeStruct range;
    Bool cont;
    if (blockCur == NULL || (void *)grainCur < (void *)blockCur) {
      AVER(grainCur);
      RangeInit(&range, FreelistGrainBase(grainCur),
                FreelistGrainLimit(fl, grainCur));
      cont = (*iterate)(&delete, &range, closureP, closureS);
      AVERT(Bool, cont);
      AVERT(Bool, delete);
      grainNext = FreelistGrainNext(grainCur);
      if (delete) {
        freelistGrainSetPrevNext(fl, grainPrev, grainNext, -1);
      } else {
        grainPrev = grainCur;
      }
      grainCur = grainNext;
    } else {
      AVER(blockCur);
      RangeInit(&range, FreelistBlockBase(blockCur),
                FreelistBlockLimit(blockCur));
      cont = (*iterate)(&delete, &range, closureP, closureS);
      AVERT(Bool, cont);
      AVERT(Bool, delete);
      blockNext = FreelistBlockNext(blockCur);
      if (delete) {
        freelistBlockSetPrevNext(fl, blockPrev, blockNext, -1);
      } else {
        blockPrev = blockCur;
      }
      blockCur = blockNext;
    }
    if (!cont)
      break;
  }
}


/* freelistFindDeleteFromBlock -- Find a chunk of 'size' bytes in
 * 'block' (which is known to be at least that big) and possibly
 * delete that chunk according to the instruction in 'findDelete'.
 * Return the range of that chunk in 'rangeReturn'. Return the
 * original range of the block in 'oldRangeReturn'. Update the block
 * list accordingly, using 'prev' which is the previous block in the
 * list, or NULL if 'block' is the first block in the list.
 */
static void freelistFindDeleteFromBlock(Range rangeReturn, Range oldRangeReturn,
                                        Freelist fl, Size size,
                                        FindDelete findDelete,
                                        FreelistBlock prev, FreelistBlock block)
{
  Bool callDelete = TRUE;
  Addr base, limit;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVER(SizeIsAligned(size, fl->alignment));
  AVERT(Freelist, fl);
  AVERT(FindDelete, findDelete);
  AVER(prev == NULL || FreelistBlockNext(prev) == block);
  AVER(block != NULL);
  AVER(FreelistBlockSize(block) >= size);
  
  base = FreelistBlockBase(block);
  limit = FreelistBlockLimit(block);

  switch (findDelete) {
  case FindDeleteNONE:
    callDelete = FALSE;
    break;

  case FindDeleteLOW:
    limit = AddrAdd(base, size);
    break;

  case FindDeleteHIGH:
    base = AddrSub(limit, size);
    break;

  case FindDeleteENTIRE:
    /* do nothing */
    break;

  default:
    NOTREACHED;
    break;
  }

  RangeInit(rangeReturn, base, limit);
  if (callDelete) {
    freelistDeleteFromBlock(oldRangeReturn, fl, rangeReturn, prev, block);
  } else {
    RangeInit(oldRangeReturn, base, limit);        
  }
}


/* freelistTakeGrain -- If there are any grains in the free list, find
 * the first grain; return its range in 'rangeReturn' and
 * 'oldRangeReturn'; possibly delete it from the list according to the
 * instruction in 'findDelete'; and return TRUE. If the grain list is
 * empty, return FALSE.
 */
static Bool freelistTakeGrain(Range rangeReturn, Range oldRangeReturn,
                              Freelist fl, FindDelete findDelete)
{
  FreelistGrain grain;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(FindDelete, findDelete);

  grain = fl->grainList;
  if (grain) {
    RangeInit(rangeReturn, FreelistGrainBase(grain),
              FreelistGrainLimit(fl, grain));
    RangeInit(oldRangeReturn, FreelistGrainBase(grain),
              FreelistGrainLimit(fl, grain));
    if (findDelete != FindDeleteNONE)
      freelistGrainSetPrevNext(fl, NULL, FreelistGrainNext(grain), -1);
    return TRUE;
  }

  return FALSE;
}


Bool FreelistFind(Range rangeReturn, Range oldRangeReturn,
                  Freelist fl, Size size, FindDelete findDelete)
{
  Res res;
  FreelistBlock blockPrev, blockCur, blockNext;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Freelist, fl);
  AVER(SizeIsAligned(size, fl->alignment));
  AVERT(FindDelete, findDelete);

  if (size == FreelistGrainSize(fl))
    return freelistTakeGrain(rangeReturn, oldRangeReturn, fl, findDelete);

  blockPrev = NULL;
  blockCur = fl->blockList;
  while (blockCur) {
    if (FreelistBlockSize(blockCur) >= size) {
      freelistFindDeleteFromBlock(rangeReturn, oldRangeReturn, fl, size,
                                  findDelete, blockPrev, blockCur);
      return TRUE;
    }
    blockNext = FreelistBlockNext(blockCur);
    blockPrev = blockCur;
    blockCur = blockNext;
  }

  return FALSE;
}


Bool FreelistFindLargest(Range rangeReturn, Range oldRangeReturn,
                         Freelist fl, FindDelete findDelete)
{
  Bool found = FALSE;
  Size size = 0;
  FreelistBlock blockPrev, blockCur, blockNext;
  FreelistBlock bestBlockPrev, bestBlockCur;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(FindDelete, findDelete);

  blockPrev = NULL;
  blockCur = fl->blockList;
  while (blockCur) {
    if (FreelistBlockSize(blockCur) > size) {
      found = TRUE;
      size = FreelistBlockSize(blockCur);
      bestBlockPrev = blockPrev;
      bestBlockCur = blockCur;
    }
    blockNext = FreelistBlockNext(blockCur);
    blockPrev = blockCur;
    blockCur = blockNext;
  }

  if (found) {
    /* No need to search the grain list. */
    freelistFindDeleteFromBlock(rangeReturn, oldRangeReturn, fl, size,
                                findDelete, bestBlockPrev, bestBlockCur);
    return TRUE;
  }

  return freelistTakeGrain(rangeReturn, oldRangeReturn, fl, findDelete);
}


/* freelistDescribeIterateMethod -- Iterate method for
 * FreelistDescribe. Writes a decription of the range into the stream
 * pointed to by 'closureP'.
 */
static Bool freelistDescribeIterateMethod(Bool *deleteReturn, Range range,
                                          void *closureP, Size closureS)
{
  Res res;
  mps_lib_FILE *stream = closureP;

  AVER(deleteReturn != NULL);
  AVERT(Range, range);
  AVER(stream != NULL);

  res = WriteF(stream,
               "  [$P,", (WriteFP)RangeBase(range),
               "$P)\n", (WriteFP)RangeLimit(range),
               NULL);

  *deleteReturn = FALSE;
  return res == ResOK;
}


Res FreelistDescribe(Freelist fl, mps_lib_FILE *stream)
{
  Res res;

  if (!TESTT(Freelist, fl)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "Freelist $P {\n", (WriteFP)fl,
               "  alignment = $U\n", (WriteFU)fl->alignment,
               "  blockListSize = $U\n", (WriteFU)fl->blockListSize,
               "  grainListSize = $U\n", (WriteFU)fl->grainListSize,
               NULL);

  FreelistIterate(fl, freelistDescribeIterateMethod, stream, 0);

  res = WriteF(stream, "}\n", NULL);
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

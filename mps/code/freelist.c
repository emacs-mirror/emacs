/* freelist.c: FREE LIST ALLOCATOR IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2013-2015 Ravenbrook Limited.  See end of file for license.
 *
 * .sources: <design/freelist/>.
 */

#include "freelist.h"
#include "mpm.h"
#include "range.h"

SRCID(freelist, "$Id$");


#define freelistOfLand(land) PARENT(FreelistStruct, landStruct, land)
#define freelistAlignment(fl) LandAlignment(FreelistLand(fl))


typedef union FreelistBlockUnion {
  struct FreelistBlockSmall {
    FreelistBlock next;    /* tagged with low bit 1 */
    /* limit is (char *)this + freelistAlignment(fl) */
  } small;
  struct FreelistBlockLarge {
    FreelistBlock next;    /* not tagged (low bit 0) */
    Addr limit;
  } large;
} FreelistBlockUnion;


/* freelistEND -- the end of a list
 *
 * The end of a list should not be represented with NULL, as this is
 * ambiguous. However, freelistEND is in fact a null pointer, for
 * performance. To check whether you have it right, try temporarily
 * defining freelistEND as ((FreelistBlock)2) or similar (it must be
 * an even number because of the use of a tag).
 */

#define freelistEND ((FreelistBlock)0)


/* freelistTag -- return the tag of word */

#define freelistTag(word) ((word) & 1)


/* freelistTagSet -- return word updated with the tag set */

#define freelistTagSet(word) ((FreelistBlock)((Word)(word) | 1))


/* freelistTagReset -- return word updated with the tag reset */

#define freelistTagReset(word) ((FreelistBlock)((Word)(word) & ~(Word)1))


/* freelistTagCopy -- return 'to' updated to have the same tag as 'from' */

#define freelistTagCopy(to, from) ((FreelistBlock)((Word)(to) | freelistTag((Word)(from))))


/* freelistBlockIsSmall -- return true if block is small, false if large */

#define freelistBlockIsSmall(block) freelistTag((Word)((block)->small.next))


/* freelistBlockBase -- return the base of a block. */

#define freelistBlockBase(block) ((Addr)(block))


/* freelistBlockNext -- return the next block in the list, or
 * freelistEND if there are no more blocks.
 */

#define freelistBlockNext(block) freelistTagReset((block)->small.next)


/* freelistBlockLimit -- return the limit of a block. */

static Addr freelistBlockLimit(Freelist fl, FreelistBlock block)
{
  AVERT(Freelist, fl);
  if (freelistBlockIsSmall(block)) {
    return AddrAdd(freelistBlockBase(block), freelistAlignment(fl));
  } else {
    return block->large.limit;
  }
}


/* FreelistBlockCheck -- check a block. */

ATTRIBUTE_UNUSED
static Bool FreelistBlockCheck(FreelistBlock block)
{
  CHECKL(block != NULL);
  /* block list is address-ordered */
  CHECKL(freelistBlockNext(block) == freelistEND
         || block < freelistBlockNext(block));
  CHECKL(freelistBlockIsSmall(block) || (Addr)block < block->large.limit);
  /* Would like to CHECKL(!freelistBlockIsSmall(block) ||
   * freelistBlockSize(fl, block) == freelistAlignment(fl)) but we
   * don't have 'fl' here. This is checked in freelistBlockSetLimit. */

  return TRUE;
}


/* freelistBlockSize -- return the size of a block. */

#define freelistBlockSize(fl, block) \
  AddrOffset(freelistBlockBase(block), freelistBlockLimit(fl, block))


/* freelistBlockSetNext -- update the next block in the list */

static void freelistBlockSetNext(FreelistBlock block, FreelistBlock next)
{
  AVERT(FreelistBlock, block);
  block->small.next = freelistTagCopy(next, block->small.next);
}


/* freelistBlockSetLimit -- update the limit of a block */

static void freelistBlockSetLimit(Freelist fl, FreelistBlock block, Addr limit)
{
  Size size;

  AVERT(Freelist, fl);
  AVERT(FreelistBlock, block);
  AVER(AddrIsAligned(limit, freelistAlignment(fl)));
  AVER(freelistBlockBase(block) < limit);

  size = AddrOffset(block, limit);
  if (size >= sizeof(block->large)) {
    block->large.next = freelistTagReset(block->large.next);
    block->large.limit = limit;
  } else {
    AVER(size >= sizeof(block->small));
    block->small.next = freelistTagSet(block->small.next);
    AVER(freelistBlockSize(fl, block) == freelistAlignment(fl));
  }
  AVER(freelistBlockLimit(fl, block) == limit);
}


/* freelistBlockInit -- initalize block storing the range [base, limit). */

static FreelistBlock freelistBlockInit(Freelist fl, Addr base, Addr limit)
{
  FreelistBlock block;

  AVERT(Freelist, fl);
  AVER(base != NULL);
  AVER(AddrIsAligned(base, freelistAlignment(fl)));
  AVER(base < limit);
  AVER(AddrIsAligned(limit, freelistAlignment(fl)));

  block = (FreelistBlock)base;
  block->small.next = freelistTagSet(freelistEND);
  freelistBlockSetLimit(fl, block, limit);
  AVERT(FreelistBlock, block);
  return block;
}


Bool FreelistCheck(Freelist fl)
{
  Land land;
  CHECKS(Freelist, fl);
  land = FreelistLand(fl);
  CHECKD(Land, land);
  CHECKL(AlignCheck(FreelistMinimumAlignment));
  CHECKL(sizeof(struct FreelistBlockSmall) < sizeof(struct FreelistBlockLarge));
  CHECKL(sizeof(struct FreelistBlockSmall) <= freelistAlignment(fl));
  /* See <design/freelist/#impl.grain.align> */
  CHECKL(AlignIsAligned(freelistAlignment(fl), FreelistMinimumAlignment));
  CHECKL((fl->list == freelistEND) == (fl->listSize == 0));
  CHECKL((fl->list == freelistEND) == (fl->size == 0));
  CHECKL(SizeIsAligned(fl->size, freelistAlignment(fl)));

  return TRUE;
}


static Res freelistInit(Land land, Arena arena, Align alignment, ArgList args)
{
  Freelist fl;
  LandClass super;
  Res res;

  AVER(land != NULL); /* FIXME: express intention */
  super = SUPERCLASS(Freelist);
  res = (*super->init)(land, arena, alignment, args);
  if (res != ResOK)
    return res;

  land->class = CLASS(Freelist);
  fl = MustBeA(Freelist, land);

  /* See <design/freelist/#impl.grain> */
  AVER(AlignIsAligned(LandAlignment(land), FreelistMinimumAlignment));

  fl->list = freelistEND;
  fl->listSize = 0;
  fl->size = 0;

  fl->sig = FreelistSig;
  AVERT(Freelist, fl);
  return ResOK;
}


static void freelistFinish(Land land)
{
  Freelist fl = MustBeA(Freelist, land);
  fl->sig = SigInvalid;
  fl->list = freelistEND;
  SUPERCLASS(Freelist)->finish(land); /* FIXME: Method call */
}


static Size freelistSize(Land land)
{
  Freelist fl;

  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  return fl->size;
}


/* freelistBlockSetPrevNext -- update list of blocks
 *
 * If prev and next are both freelistEND, make the block list empty.
 * Otherwise, if prev is freelistEND, make next the first block in the list.
 * Otherwise, if next is freelistEND, make prev the last block in the list.
 * Otherwise, make next follow prev in the list.
 * Update the count of blocks by 'delta'.
 *
 * It is tempting to try to simplify this code by putting a
 * FreelistBlockUnion into the FreelistStruct and so avoiding the
 * special case on prev. But the problem with that idea is that we
 * can't guarantee that such a sentinel would respect the isolated
 * range invariant (it would have to be at a lower address than the
 * first block in the free list, which the MPS has no mechanism to
 * enforce), and so it would still have to be special-cased.
 */

static void freelistBlockSetPrevNext(Freelist fl, FreelistBlock prev,
                                     FreelistBlock next, int delta)
{
  AVERT(Freelist, fl);

  if (prev == freelistEND) {
    fl->list = next;
  } else {
    /* Isolated range invariant (design.mps.freelist.impl.invariant). */
    AVER(next == freelistEND
         || freelistBlockLimit(fl, prev) < freelistBlockBase(next));
    freelistBlockSetNext(prev, next);
  }
  if (delta < 0) {
    AVER(fl->listSize >= (Count)-delta);
    fl->listSize -= (Count)-delta;
  } else {
    fl->listSize += (Count)delta;
  }
}


static Res freelistInsert(Range rangeReturn, Land land, Range range)
{
  Freelist fl;
  FreelistBlock prev, cur, next, new;
  Addr base, limit;
  Bool coalesceLeft, coalesceRight;

  AVER(rangeReturn != NULL);
  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, freelistAlignment(fl)));

  base = RangeBase(range);
  limit = RangeLimit(range);

  prev = freelistEND;
  cur = fl->list;
  while (cur != freelistEND) {
    if (base < freelistBlockLimit(fl, cur) && freelistBlockBase(cur) < limit)
      return ResFAIL; /* range overlaps with cur */
    if (limit <= freelistBlockBase(cur))
      break;
    next = freelistBlockNext(cur);
    if (next != freelistEND)
      /* Isolated range invariant (design.mps.freelist.impl.invariant). */
      AVER(freelistBlockLimit(fl, cur) < freelistBlockBase(next));
    prev = cur;
    cur = next;
  }

  /* Now we know that range does not overlap with any block, and if it
   * coalesces then it does so with prev on the left, and cur on the
   * right.
   */
  coalesceLeft = (prev != freelistEND && base == freelistBlockLimit(fl, prev));
  coalesceRight = (cur != freelistEND && limit == freelistBlockBase(cur));

  if (coalesceLeft && coalesceRight) {
    base = freelistBlockBase(prev);
    limit = freelistBlockLimit(fl, cur);
    freelistBlockSetLimit(fl, prev, limit);
    freelistBlockSetPrevNext(fl, prev, freelistBlockNext(cur), -1);

  } else if (coalesceLeft) {
    base = freelistBlockBase(prev);
    freelistBlockSetLimit(fl, prev, limit);

  } else if (coalesceRight) {
    next = freelistBlockNext(cur);
    limit = freelistBlockLimit(fl, cur);
    cur = freelistBlockInit(fl, base, limit);
    freelistBlockSetNext(cur, next);
    freelistBlockSetPrevNext(fl, prev, cur, 0);

  } else {
    /* failed to coalesce: add new block */
    new = freelistBlockInit(fl, base, limit);
    freelistBlockSetNext(new, cur);
    freelistBlockSetPrevNext(fl, prev, new, +1);
  }

  fl->size += RangeSize(range);
  RangeInit(rangeReturn, base, limit);
  return ResOK;
}


/* freelistDeleteFromBlock -- delete range from block
 *
 * range must be a subset of block. Update rangeReturn to be the
 * original range of block and update the block list accordingly: prev
 * is on the list just before block, or freelistEND if block is the
 * first block on the list.
 */

static void freelistDeleteFromBlock(Range rangeReturn, Freelist fl,
                                    Range range, FreelistBlock prev,
                                    FreelistBlock block)
{
  FreelistBlock next, new;
  Addr base, limit, blockBase, blockLimit;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, freelistAlignment(fl)));
  AVER(prev == freelistEND || freelistBlockNext(prev) == block);
  AVERT(FreelistBlock, block);
  AVER(freelistBlockBase(block) <= RangeBase(range));
  AVER(RangeLimit(range) <= freelistBlockLimit(fl, block));

  base = RangeBase(range);
  limit = RangeLimit(range);
  blockBase = freelistBlockBase(block);
  blockLimit = freelistBlockLimit(fl, block);
  next = freelistBlockNext(block);

  if (base == blockBase && limit == blockLimit) {
    /* No fragment at left; no fragment at right. */
    freelistBlockSetPrevNext(fl, prev, next, -1);

  } else if (base == blockBase) {
    /* No fragment at left; block at right. */
    block = freelistBlockInit(fl, limit, blockLimit);
    freelistBlockSetNext(block, next);
    freelistBlockSetPrevNext(fl, prev, block, 0);

  } else if (limit == blockLimit) {        
    /* Block at left; no fragment at right. */
    freelistBlockSetLimit(fl, block, base);

  } else {
    /* Block at left; block at right. */
    freelistBlockSetLimit(fl, block, base);
    new = freelistBlockInit(fl, limit, blockLimit);
    freelistBlockSetNext(new, next);
    freelistBlockSetPrevNext(fl, block, new, +1);
  }

  AVER(fl->size >= RangeSize(range));
  fl->size -= RangeSize(range);
  RangeInit(rangeReturn, blockBase, blockLimit);
}


static Res freelistDelete(Range rangeReturn, Land land, Range range)
{
  Freelist fl;
  FreelistBlock prev, cur, next;
  Addr base, limit;

  AVER(rangeReturn != NULL);
  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  AVERT(Range, range);

  base = RangeBase(range);
  limit = RangeLimit(range);

  prev = freelistEND;
  cur = fl->list;
  while (cur != freelistEND) {
    Addr blockBase, blockLimit;
    blockBase = freelistBlockBase(cur);
    blockLimit = freelistBlockLimit(fl, cur);

    if (limit <= blockBase)
      return ResFAIL; /* not found */
    if (base <= blockLimit) {
      if (base < blockBase || blockLimit < limit)
        return ResFAIL; /* partially overlapping */
      freelistDeleteFromBlock(rangeReturn, fl, range, prev, cur);
      return ResOK;
    }
    
    next = freelistBlockNext(cur);
    prev = cur;
    cur = next;
  }

  /* Range not found in block list. */
  return ResFAIL;
}


static Bool freelistIterate(Land land, LandVisitor visitor,
                            void *closure)
{
  Freelist fl;
  FreelistBlock cur, next;

  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  AVER(FUNCHECK(visitor));
  /* closure arbitrary */

  for (cur = fl->list; cur != freelistEND; cur = next) {
    RangeStruct range;
    Bool cont;
    /* .next.first: Take next before calling the visitor, in case the
     * visitor touches the block. */
    next = freelistBlockNext(cur);
    RangeInit(&range, freelistBlockBase(cur), freelistBlockLimit(fl, cur));
    cont = (*visitor)(land, &range, closure);
    if (!cont)
      return FALSE;
  }
  return TRUE;
}


static Bool freelistIterateAndDelete(Land land, LandDeleteVisitor visitor,
                                     void *closure)
{
  Freelist fl;
  FreelistBlock prev, cur, next;

  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  AVER(FUNCHECK(visitor));
  /* closure arbitrary */

  prev = freelistEND;
  cur = fl->list;
  while (cur != freelistEND) {
    Bool delete = FALSE;
    RangeStruct range;
    Bool cont;
    Size size;
    next = freelistBlockNext(cur); /* See .next.first. */
    size = freelistBlockSize(fl, cur);
    RangeInit(&range, freelistBlockBase(cur), freelistBlockLimit(fl, cur));
    cont = (*visitor)(&delete, land, &range, closure);
    if (delete) {
      freelistBlockSetPrevNext(fl, prev, next, -1);
      AVER(fl->size >= size);
      fl->size -= size;
    } else {
      prev = cur;
    }
    if (!cont)
      return FALSE;
    cur = next;
  }
  return TRUE;
}


/* freelistFindDeleteFromBlock -- delete size bytes from block
 *
 * Find a chunk of size bytes in block (which is known to be at least
 * that big) and possibly delete that chunk according to the
 * instruction in findDelete. Return the range of that chunk in
 * rangeReturn. Return the original range of the block in
 * oldRangeReturn. Update the block list accordingly, using prev,
 * which is previous in list or freelistEND if block is the first
 * block in the list.
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
  AVERT(Freelist, fl);
  AVER(SizeIsAligned(size, freelistAlignment(fl)));
  AVERT(FindDelete, findDelete);
  AVER(prev == freelistEND || freelistBlockNext(prev) == block);
  AVERT(FreelistBlock, block);
  AVER(freelistBlockSize(fl, block) >= size);
  
  base = freelistBlockBase(block);
  limit = freelistBlockLimit(fl, block);

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


static Bool freelistFindFirst(Range rangeReturn, Range oldRangeReturn,
                              Land land, Size size, FindDelete findDelete)
{
  Freelist fl;
  FreelistBlock prev, cur, next;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  AVER(SizeIsAligned(size, freelistAlignment(fl)));
  AVERT(FindDelete, findDelete);

  prev = freelistEND;
  cur = fl->list;
  while (cur != freelistEND) {
    if (freelistBlockSize(fl, cur) >= size) {
      freelistFindDeleteFromBlock(rangeReturn, oldRangeReturn, fl, size,
                                  findDelete, prev, cur);
      return TRUE;
    }
    next = freelistBlockNext(cur);
    prev = cur;
    cur = next;
  }

  return FALSE;
}


static Bool freelistFindLast(Range rangeReturn, Range oldRangeReturn,
                             Land land, Size size, FindDelete findDelete)
{
  Freelist fl;
  Bool found = FALSE;
  FreelistBlock prev, cur, next;
  FreelistBlock foundPrev = freelistEND, foundCur = freelistEND;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  AVER(SizeIsAligned(size, freelistAlignment(fl)));
  AVERT(FindDelete, findDelete);

  prev = freelistEND;
  cur = fl->list;
  while (cur != freelistEND) {
    if (freelistBlockSize(fl, cur) >= size) {
      found = TRUE;
      foundPrev = prev;
      foundCur = cur;
    }
    next = freelistBlockNext(cur);
    prev = cur;
    cur = next;
  }

  if (found)
    freelistFindDeleteFromBlock(rangeReturn, oldRangeReturn, fl, size,
                                findDelete, foundPrev, foundCur);

  return found;
}


static Bool freelistFindLargest(Range rangeReturn, Range oldRangeReturn,
                                Land land, Size size, FindDelete findDelete)
{
  Freelist fl;
  Bool found = FALSE;
  FreelistBlock prev, cur, next;
  FreelistBlock bestPrev = freelistEND, bestCur = freelistEND;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  AVERT(FindDelete, findDelete);

  prev = freelistEND;
  cur = fl->list;
  while (cur != freelistEND) {
    if (freelistBlockSize(fl, cur) >= size) {
      found = TRUE;
      size = freelistBlockSize(fl, cur);
      bestPrev = prev;
      bestCur = cur;
    }
    next = freelistBlockNext(cur);
    prev = cur;
    cur = next;
  }

  if (found)
    freelistFindDeleteFromBlock(rangeReturn, oldRangeReturn, fl, size,
                                findDelete, bestPrev, bestCur);

  return found;
}


static Res freelistFindInZones(Bool *foundReturn, Range rangeReturn,
                               Range oldRangeReturn, Land land, Size size,
                               ZoneSet zoneSet, Bool high)
{
  Freelist fl;
  LandFindMethod landFind;
  RangeInZoneSet search;
  Bool found = FALSE;
  FreelistBlock prev, cur, next;
  FreelistBlock foundPrev = freelistEND, foundCur = freelistEND;
  RangeStruct foundRange;

  AVER(FALSE); /* TODO: this code is completely untested! */
  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  /* AVERT(ZoneSet, zoneSet); */
  AVERT(Bool, high);

  landFind = high ? freelistFindLast : freelistFindFirst;
  search = high ? RangeInZoneSetLast : RangeInZoneSetFirst;

  if (zoneSet == ZoneSetEMPTY)
    goto fail;
  if (zoneSet == ZoneSetUNIV) {
    FindDelete fd = high ? FindDeleteHIGH : FindDeleteLOW;
    *foundReturn = (*landFind)(rangeReturn, oldRangeReturn, land, size, fd);
    return ResOK;
  }
  if (ZoneSetIsSingle(zoneSet) && size > ArenaStripeSize(LandArena(land)))
    goto fail;

  prev = freelistEND;
  cur = fl->list;
  while (cur != freelistEND) {
    Addr base, limit;
    if ((*search)(&base, &limit, freelistBlockBase(cur),
                  freelistBlockLimit(fl, cur),
                  LandArena(land), zoneSet, size))
    {
      found = TRUE;
      foundPrev = prev;
      foundCur = cur;
      RangeInit(&foundRange, base, limit);
      if (!high)
        break;
    }
    next = freelistBlockNext(cur);
    prev = cur;
    cur = next;
  }

  if (!found)
    goto fail;

  freelistDeleteFromBlock(oldRangeReturn, fl, &foundRange, foundPrev, foundCur);
  RangeCopy(rangeReturn, &foundRange);
  *foundReturn = TRUE;
  return ResOK;

fail:
  *foundReturn = FALSE;
  return ResOK;
}


/* freelistDescribeVisitor -- visitor method for freelistDescribe
 *
 * Writes a decription of the range into the stream pointed to by
 * closure.
 */

typedef struct FreelistDescribeClosureStruct {
  mps_lib_FILE *stream;
  Count depth;
} FreelistDescribeClosureStruct, *FreelistDescribeClosure;

static Bool freelistDescribeVisitor(Land land, Range range,
                                    void *closure)
{
  Res res;
  FreelistDescribeClosure my = closure;

  if (!TESTT(Land, land))
    return FALSE;
  if (!RangeCheck(range))
    return FALSE;
  if (my->stream == NULL)
    return FALSE;

  res = WriteF(my->stream, my->depth,
               "[$P,", (WriteFP)RangeBase(range),
               "$P)", (WriteFP)RangeLimit(range),
               " {$U}\n", (WriteFU)RangeSize(range),
               NULL);

  return res == ResOK;
}


static Res freelistDescribe(Land land, mps_lib_FILE *stream, Count depth)
{
  Freelist fl = CouldBeA(Freelist, land);
  Res res;
  Bool b;
  FreelistDescribeClosureStruct closure;

  if (!TESTC(Freelist, fl))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  /* FIXME: Should use the class from the land itself. */
  res = SUPERCLASS(Freelist)->describe(land, stream, depth);
  if (res != ResOK)
    return res;

  res = WriteF(stream, depth + 2,
               "listSize $U\n", (WriteFU)fl->listSize,
               "size     $U\n", (WriteFU)fl->size,
               NULL);

  closure.stream = stream;
  closure.depth = depth + 2;
  b = LandIterate(land, freelistDescribeVisitor, &closure);
  if (!b)
    return ResFAIL;

  return res;
}


DEFINE_CLASS(Land, Freelist, class)
{
  INHERIT_CLASS(class, Freelist, Land);
  class->size = sizeof(FreelistStruct);
  class->init = freelistInit;
  class->finish = freelistFinish;
  class->sizeMethod = freelistSize;
  class->insert = freelistInsert;
  class->delete = freelistDelete;
  class->iterate = freelistIterate;
  class->iterateAndDelete = freelistIterateAndDelete;
  class->findFirst = freelistFindFirst;
  class->findLast = freelistFindLast;
  class->findLargest = freelistFindLargest;
  class->findInZones = freelistFindInZones;
  class->describe = freelistDescribe;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2013-2015 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

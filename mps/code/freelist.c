/* freelist.c: FREE LIST ALLOCATOR IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2013-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .sources: <design/freelist/>.
 */

#include "freelist.h"
#include "mpm.h"
#include "range.h"

SRCID(freelist, "$Id$");


#define freelistOfLand(land) PARENT(FreelistStruct, landStruct, land)
#define freelistAlignment(fl) LandAlignment(&(fl)->landStruct)


typedef union FreelistBlockUnion {
  struct {
    FreelistBlock next;    /* tagged with low bit 1 */
    /* limit is (char *)this + freelistAlignment(fl) */
  } small;
  struct {
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


/* freelistMinimumAlignment -- the minimum allowed alignment for the
 * address ranges in a free list: see <design/freelist/#impl.grain.align>
 */

#define freelistMinimumAlignment ((Align)sizeof(FreelistBlock))


/* FreelistTag -- return the tag of word */

#define FreelistTag(word) ((word) & 1)


/* FreelistTagSet -- return word updated with the tag set */

#define FreelistTagSet(word) ((FreelistBlock)((Word)(word) | 1))


/* FreelistTagReset -- return word updated with the tag reset */

#define FreelistTagReset(word) ((FreelistBlock)((Word)(word) & ~(Word)1))


/* FreelistTagCopy -- return 'to' updated to have the same tag as 'from' */

#define FreelistTagCopy(to, from) ((FreelistBlock)((Word)(to) | FreelistTag((Word)(from))))


/* FreelistBlockIsSmall -- return true if block is small, false if large */

#define FreelistBlockIsSmall(block) FreelistTag((Word)((block)->small.next))


/* FreelistBlockBase -- return the base of a block. */

#define FreelistBlockBase(block) ((Addr)(block))


/* FreelistBlockLimit -- return the limit of a block. */

static Addr FreelistBlockLimit(Freelist fl, FreelistBlock block)
{
  AVERT(Freelist, fl);
  if (FreelistBlockIsSmall(block)) {
    return AddrAdd(FreelistBlockBase(block), freelistAlignment(fl));
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
  CHECKL(FreelistTagReset(block->small.next) == freelistEND
         || block < FreelistTagReset(block->small.next));
  CHECKL(FreelistBlockIsSmall(block) || (Addr)block < block->large.limit);

  return TRUE;
}


/* FreelistBlockNext -- return the next block in the list, or
 * freelistEND if there are no more blocks.
 */
static FreelistBlock FreelistBlockNext(FreelistBlock block)
{
  AVERT(FreelistBlock, block);
  return FreelistTagReset(block->small.next);
}


/* FreelistBlockSize -- return the size of a block. */

#define FreelistBlockSize(fl, block) \
  AddrOffset(FreelistBlockBase(block), FreelistBlockLimit(fl, block))


/* FreelistBlockSetNext -- update the next block in the list */

static void FreelistBlockSetNext(FreelistBlock block, FreelistBlock next)
{
  AVERT(FreelistBlock, block);
  block->small.next = FreelistTagCopy(next, block->small.next);
}


/* FreelistBlockSetLimit -- update the limit of a block */

static void FreelistBlockSetLimit(Freelist fl, FreelistBlock block, Addr limit)
{
  Size size;

  AVERT(Freelist, fl);
  AVERT(FreelistBlock, block);
  AVER(AddrIsAligned(limit, freelistAlignment(fl)));
  AVER(FreelistBlockBase(block) < limit);

  size = AddrOffset(block, limit);
  if (size >= sizeof(block->large)) {
    block->large.next = FreelistTagReset(block->large.next);
    block->large.limit = limit;
  } else {
    AVER(size >= sizeof(block->small));
    block->small.next = FreelistTagSet(block->small.next);
  }
  AVER(FreelistBlockLimit(fl, block) == limit);
}


/* FreelistBlockInit -- initalize block storing the range [base, limit). */

static FreelistBlock FreelistBlockInit(Freelist fl, Addr base, Addr limit)
{
  FreelistBlock block;

  AVERT(Freelist, fl);
  AVER(base != NULL);
  AVER(AddrIsAligned(base, freelistAlignment(fl)));
  AVER(base < limit);
  AVER(AddrIsAligned(limit, freelistAlignment(fl)));

  block = (FreelistBlock)base;
  block->small.next = FreelistTagSet(freelistEND);
  FreelistBlockSetLimit(fl, block, limit);
  AVERT(FreelistBlock, block);
  return block;
}


Bool FreelistCheck(Freelist fl)
{
  Land land;
  CHECKS(Freelist, fl);
  land = &fl->landStruct;
  CHECKD(Land, land);
  /* See <design/freelist/#impl.grain.align> */
  CHECKL(AlignIsAligned(freelistAlignment(fl), freelistMinimumAlignment));
  CHECKL((fl->list == freelistEND) == (fl->listSize == 0));
  CHECKL((fl->list == freelistEND) == (fl->size == 0));
  CHECKL(SizeIsAligned(fl->size, freelistAlignment(fl)));

  return TRUE;
}


static Res freelistInit(Land land, ArgList args)
{
  Freelist fl;
  LandClass super;
  Res res;

  AVERT(Land, land);
  super = LAND_SUPERCLASS(FreelistLandClass);
  res = (*super->init)(land, args);
  if (res != ResOK)
    return res;

  /* See <design/freelist/#impl.grain> */
  AVER(AlignIsAligned(LandAlignment(land), freelistMinimumAlignment));

  fl = freelistOfLand(land);
  fl->list = freelistEND;
  fl->listSize = 0;
  fl->size = 0;

  fl->sig = FreelistSig;
  AVERT(Freelist, fl);
  return ResOK;
}


static void freelistFinish(Land land)
{
  Freelist fl;

  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  fl->sig = SigInvalid;
  fl->list = freelistEND;
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

 * It is tempting to try to simplify this code by putting a
 * FreelistBlockUnion into the FreelistStruct and so avoiding the
 * special case on prev. But the problem with that idea is that we
 * can't guarantee that such a sentinel would respect the isolated
 * range invariant, and so it would still have to be special-cases.
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
         || FreelistBlockLimit(fl, prev) < FreelistBlockBase(next));
    FreelistBlockSetNext(prev, next);
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
    if (base < FreelistBlockLimit(fl, cur) && FreelistBlockBase(cur) < limit)
      return ResFAIL; /* range overlaps with cur */
    if (limit <= FreelistBlockBase(cur))
      break;
    next = FreelistBlockNext(cur);
    if (next != freelistEND)
      /* Isolated range invariant (design.mps.freelist.impl.invariant). */
      AVER(FreelistBlockLimit(fl, cur) < FreelistBlockBase(next));
    prev = cur;
    cur = next;
  }

  /* Now we know that range does not overlap with any block, and if it
   * coalesces then it does so with prev on the left, and cur on the
   * right.
   */
  coalesceLeft = (prev != freelistEND && base == FreelistBlockLimit(fl, prev));
  coalesceRight = (cur != freelistEND && limit == FreelistBlockBase(cur));

  if (coalesceLeft && coalesceRight) {
    base = FreelistBlockBase(prev);
    limit = FreelistBlockLimit(fl, cur);
    FreelistBlockSetLimit(fl, prev, limit);
    freelistBlockSetPrevNext(fl, prev, FreelistBlockNext(cur), -1);

  } else if (coalesceLeft) {
    base = FreelistBlockBase(prev);
    FreelistBlockSetLimit(fl, prev, limit);

  } else if (coalesceRight) {
    next = FreelistBlockNext(cur);
    limit = FreelistBlockLimit(fl, cur);
    cur = FreelistBlockInit(fl, base, limit);
    FreelistBlockSetNext(cur, next);
    freelistBlockSetPrevNext(fl, prev, cur, 0);

  } else {
    /* failed to coalesce: add new block */
    new = FreelistBlockInit(fl, base, limit);
    FreelistBlockSetNext(new, cur);
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
  AVER(prev == freelistEND || FreelistBlockNext(prev) == block);
  AVERT(FreelistBlock, block);
  AVER(FreelistBlockBase(block) <= RangeBase(range));
  AVER(RangeLimit(range) <= FreelistBlockLimit(fl, block));

  base = RangeBase(range);
  limit = RangeLimit(range);
  blockBase = FreelistBlockBase(block);
  blockLimit = FreelistBlockLimit(fl, block);
  next = FreelistBlockNext(block);

  if (base == blockBase && limit == blockLimit) {
    /* No fragment at left; no fragment at right. */
    freelistBlockSetPrevNext(fl, prev, next, -1);

  } else if (base == blockBase) {
    /* No fragment at left; block at right. */
    block = FreelistBlockInit(fl, limit, blockLimit);
    FreelistBlockSetNext(block, next);
    freelistBlockSetPrevNext(fl, prev, block, 0);

  } else if (limit == blockLimit) {        
    /* Block at left; no fragment at right. */
    FreelistBlockSetLimit(fl, block, base);

  } else {
    /* Block at left; block at right. */
    FreelistBlockSetLimit(fl, block, base);
    new = FreelistBlockInit(fl, limit, blockLimit);
    FreelistBlockSetNext(new, next);
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
    blockBase = FreelistBlockBase(cur);
    blockLimit = FreelistBlockLimit(fl, cur);

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


static Bool freelistIterate(Land land, LandVisitor visitor,
                            void *closureP, Size closureS)
{
  Freelist fl;
  FreelistBlock cur, next;

  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  AVER(FUNCHECK(visitor));
  /* closureP and closureS are arbitrary */

  for (cur = fl->list; cur != freelistEND; cur = next) {
    RangeStruct range;
    Bool cont;
    /* .next.first: Take next before calling the visitor, in case the
     * visitor touches the block. */
    next = FreelistBlockNext(cur);
    RangeInit(&range, FreelistBlockBase(cur), FreelistBlockLimit(fl, cur));
    cont = (*visitor)(land, &range, closureP, closureS);
    if (!cont)
      return FALSE;
  }
  return TRUE;
}


static Bool freelistIterateAndDelete(Land land, LandDeleteVisitor visitor,
                                     void *closureP, Size closureS)
{
  Freelist fl;
  FreelistBlock prev, cur, next;

  AVERT(Land, land);
  fl = freelistOfLand(land);
  AVERT(Freelist, fl);
  AVER(FUNCHECK(visitor));
  /* closureP and closureS are arbitrary */

  prev = freelistEND;
  cur = fl->list;
  while (cur != freelistEND) {
    Bool delete = FALSE;
    RangeStruct range;
    Bool cont;
    Size size;
    next = FreelistBlockNext(cur); /* See .next.first. */
    size = FreelistBlockSize(fl, cur);
    RangeInit(&range, FreelistBlockBase(cur), FreelistBlockLimit(fl, cur));
    cont = (*visitor)(&delete, land, &range, closureP, closureS);
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
  AVER(prev == freelistEND || FreelistBlockNext(prev) == block);
  AVERT(FreelistBlock, block);
  AVER(FreelistBlockSize(fl, block) >= size);
  
  base = FreelistBlockBase(block);
  limit = FreelistBlockLimit(fl, block);

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
    if (FreelistBlockSize(fl, cur) >= size) {
      freelistFindDeleteFromBlock(rangeReturn, oldRangeReturn, fl, size,
                                  findDelete, prev, cur);
      return TRUE;
    }
    next = FreelistBlockNext(cur);
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
    if (FreelistBlockSize(fl, cur) >= size) {
      found = TRUE;
      foundPrev = prev;
      foundCur = cur;
    }
    next = FreelistBlockNext(cur);
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
    if (FreelistBlockSize(fl, cur) >= size) {
      found = TRUE;
      size = FreelistBlockSize(fl, cur);
      bestPrev = prev;
      bestCur = cur;
    }
    next = FreelistBlockNext(cur);
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
    if ((*search)(&base, &limit, FreelistBlockBase(cur),
                  FreelistBlockLimit(fl, cur),
                  LandArena(land), zoneSet, size))
    {
      found = TRUE;
      foundPrev = prev;
      foundCur = cur;
      RangeInit(&foundRange, base, limit);
      if (!high)
        break;
    }
    next = FreelistBlockNext(cur);
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
 * closureP.
 */

static Bool freelistDescribeVisitor(Land land, Range range,
                                    void *closureP, Size closureS)
{
  Res res;
  mps_lib_FILE *stream = closureP;

  if (!TESTT(Land, land)) return FALSE;
  if (!RangeCheck(range)) return FALSE;
  if (stream == NULL) return FALSE;
  if (closureS != UNUSED_SIZE) return FALSE;

  res = WriteF(stream,
               "  [$P,", (WriteFP)RangeBase(range),
               "$P)", (WriteFP)RangeLimit(range),
               " {$U}\n", (WriteFU)RangeSize(range),
               NULL);

  return res == ResOK;
}


static Res freelistDescribe(Land land, mps_lib_FILE *stream)
{
  Freelist fl;
  Res res;
  Bool b;

  if (!TESTT(Land, land)) return ResFAIL;
  fl = freelistOfLand(land);
  if (!TESTT(Freelist, fl)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(stream,
               "Freelist $P {\n", (WriteFP)fl,
               "  listSize = $U\n", (WriteFU)fl->listSize,
               NULL);

  b = LandIterate(land, freelistDescribeVisitor, stream, UNUSED_SIZE);
  if (!b) return ResFAIL;

  res = WriteF(stream, "}\n", NULL);
  return res;
}


DEFINE_LAND_CLASS(FreelistLandClass, class)
{
  INHERIT_CLASS(class, LandClass);
  class->name = "FREELIST";
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
  AVERT(LandClass, class);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2013-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

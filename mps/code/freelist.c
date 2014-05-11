/* freelist.c: FREE LIST ALLOCATOR IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2013 Ravenbrook Limited.  See end of file for license.
 *
 * .sources: <design/freelist/>.
 */

#include "cbs.h"
#include "freelist.h"
#include "mpm.h"

SRCID(freelist, "$Id$");


typedef union FreelistBlockUnion {
  struct {
    FreelistBlock next;    /* tagged with low bit 1 */
    /* limit is (char *)this + fl->alignment */
  } small;
  struct {
    FreelistBlock next;
    Addr limit;
  } large;
} FreelistBlockUnion;


/* See <design/freelist/#impl.grain.align> */
#define freelistMinimumAlignment ((Align)sizeof(FreelistBlock))


#define FreelistTag(word) ((word) & 1)
#define FreelistTagSet(word) ((FreelistBlock)((Word)(word) | 1))
#define FreelistTagReset(word) ((FreelistBlock)((Word)(word) & ~(Word)1))
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
    return AddrAdd(FreelistBlockBase(block), fl->alignment);
  } else {
    return block->large.limit;
  }
}


/* FreelistBlockCheck -- check a block. */

static Bool FreelistBlockCheck(FreelistBlock block)
{
  CHECKL(block != NULL);
  /* block list is address-ordered */
  CHECKL(FreelistTagReset(block->small.next) == NULL
         || block < FreelistTagReset(block->small.next));
  CHECKL(FreelistBlockIsSmall(block) || (Addr)block < block->large.limit);

  return TRUE;
}


/* FreelistBlockNext -- return the next block in the list, or NULL if
 * there are no more blocks.
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
  AVER(AddrIsAligned(limit, fl->alignment));
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
  AVER(AddrIsAligned(base, fl->alignment));
  AVER(base < limit);
  AVER(AddrIsAligned(limit, fl->alignment));

  block = (FreelistBlock)base;
  block->small.next = FreelistTagSet(NULL);
  FreelistBlockSetLimit(fl, block, limit);
  AVERT(FreelistBlock, block);
  return block;
}


Bool FreelistCheck(Freelist fl)
{
  CHECKS(Freelist, fl);
  /* See <design/freelist/#impl.grain.align> */
  CHECKL(AlignIsAligned(fl->alignment, freelistMinimumAlignment));
  CHECKL((fl->list == NULL) == (fl->listSize == 0));
  return TRUE;
}


Res FreelistInit(Freelist fl, Align alignment)
{
  /* See <design/freelist/#impl.grain> */
  if (!AlignIsAligned(alignment, freelistMinimumAlignment))
    return ResPARAM;

  fl->alignment = alignment;
  fl->list = NULL;
  fl->listSize = 0;

  fl->sig = FreelistSig;
  AVERT(Freelist, fl);
  return ResOK;
}


void FreelistFinish(Freelist fl)
{
  AVERT(Freelist, fl);
  fl->sig = SigInvalid;
  fl->list = NULL;
}


/* freelistBlockSetPrevNext -- update list of blocks
 * If prev and next are both NULL, make the block list empty.
 * Otherwise, if prev is NULL, make next the first block in the list.
 * Otherwise, if next is NULL, make prev the last block in the list.
 * Otherwise, make next follow prev in the list.
 * Update the count of blocks by 'delta'.
 */
static void freelistBlockSetPrevNext(Freelist fl, FreelistBlock prev,
                                     FreelistBlock next, int delta)
{
  AVERT(Freelist, fl);

  if (prev) {
    AVER(next == NULL || FreelistBlockLimit(fl, prev) < FreelistBlockBase(next));
    FreelistBlockSetNext(prev, next);
  } else {
    fl->list = next;
  }
  if (delta < 0) {
    AVER(fl->listSize >= (Count)-delta);
    fl->listSize -= (Count)-delta;
  } else {
    fl->listSize += (Count)delta;
  }
}


Res FreelistInsert(Range rangeReturn, Freelist fl, Range range)
{
  FreelistBlock prev, cur, next, new;
  Addr base, limit;
  Bool coalesceLeft, coalesceRight;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, fl->alignment));

  base = RangeBase(range);
  limit = RangeLimit(range);

  prev = NULL;
  cur = fl->list;
  while (cur) {
    if (base < FreelistBlockLimit(fl, cur) && FreelistBlockBase(cur) < limit)
      return ResFAIL; /* range overlaps with cur */
    if (limit <= FreelistBlockBase(cur))
      break;
    next = FreelistBlockNext(cur);
    if (next)
      /* Isolated range invariant (design.mps.freelist.impl.invariant). */
      AVER(FreelistBlockLimit(fl, cur) < FreelistBlockBase(next));
    prev = cur;
    cur = next;
  }

  /* Now we know that range does not overlap with any block, and if it
   * coalesces then it does so with prev on the left, and cur on the
   * right.
   */
  coalesceLeft = (prev && base == FreelistBlockLimit(fl, prev));
  coalesceRight = (cur && limit == FreelistBlockBase(cur));

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

  RangeInit(rangeReturn, base, limit);
  return ResOK;
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
  FreelistBlock next, new;
  Addr base, limit, blockBase, blockLimit;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, fl->alignment));
  AVER(prev == NULL || FreelistBlockNext(prev) == block);
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

  RangeInit(rangeReturn, blockBase, blockLimit);
}


Res FreelistDelete(Range rangeReturn, Freelist fl, Range range)
{
  FreelistBlock prev, cur, next;
  Addr base, limit;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);

  base = RangeBase(range);
  limit = RangeLimit(range);

  prev = NULL;
  cur = fl->list;
  while (cur) {
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


void FreelistIterate(Freelist fl, FreelistIterateMethod iterate,
                     void *closureP, Size closureS)
{
  FreelistBlock prev, cur, next;

  AVERT(Freelist, fl);
  AVER(FUNCHECK(iterate));

  prev = NULL;
  cur = fl->list;
  while (cur) {
    Bool delete = FALSE;
    RangeStruct range;
    Bool cont;
    RangeInit(&range, FreelistBlockBase(cur), FreelistBlockLimit(fl, cur));
    cont = (*iterate)(&delete, &range, closureP, closureS);
    next = FreelistBlockNext(cur);
    if (delete) {
      freelistBlockSetPrevNext(fl, prev, next, -1);
    } else {
      prev = cur;
    }
    cur = next;
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
  AVERT(Freelist, fl);
  AVER(SizeIsAligned(size, fl->alignment));
  AVERT(FindDelete, findDelete);
  AVER(prev == NULL || FreelistBlockNext(prev) == block);
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


Bool FreelistFindFirst(Range rangeReturn, Range oldRangeReturn,
                       Freelist fl, Size size, FindDelete findDelete)
{
  FreelistBlock prev, cur, next;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Freelist, fl);
  AVER(SizeIsAligned(size, fl->alignment));
  AVERT(FindDelete, findDelete);

  prev = NULL;
  cur = fl->list;
  while (cur) {
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


Bool FreelistFindLast(Range rangeReturn, Range oldRangeReturn,
                      Freelist fl, Size size, FindDelete findDelete)
{
  Bool found = FALSE;
  FreelistBlock prev, cur, next;
  FreelistBlock foundPrev = NULL, foundCur = NULL;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Freelist, fl);
  AVER(SizeIsAligned(size, fl->alignment));
  AVERT(FindDelete, findDelete);

  prev = NULL;
  cur = fl->list;
  while (cur) {
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


Bool FreelistFindLargest(Range rangeReturn, Range oldRangeReturn,
                         Freelist fl, Size size, FindDelete findDelete)
{
  Bool found = FALSE;
  FreelistBlock prev, cur, next;
  FreelistBlock bestPrev = NULL, bestCur = NULL;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(FindDelete, findDelete);

  prev = NULL;
  cur = fl->list;
  while (cur) {
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


/* freelistDescribeIterateMethod -- Iterate method for
 * FreelistDescribe. Writes a decription of the range into the stream
 * pointed to by 'closureP'.
 */
static Bool freelistDescribeIterateMethod(Bool *deleteReturn, Range range,
                                          void *closureP, Size closureS)
{
  Res res;
  mps_lib_FILE *stream = closureP;
  Count depth = closureS;

  AVER(deleteReturn != NULL);
  AVERT(Range, range);
  AVER(stream != NULL);

  res = WriteF(depth, stream,
               "[$P,", (WriteFP)RangeBase(range),
               "$P)", (WriteFP)RangeLimit(range),
               " {$U}\n", (WriteFU)RangeSize(range),
               NULL);

  *deleteReturn = FALSE;
  return res == ResOK;
}


Res FreelistDescribe(Freelist fl, mps_lib_FILE *stream, Count depth)
{
  Res res;

  if (!TESTT(Freelist, fl)) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  res = WriteF(depth, stream,
               "Freelist $P {\n", (WriteFP)fl,
               "  alignment = $U\n", (WriteFU)fl->alignment,
               "  listSize = $U\n", (WriteFU)fl->listSize,
               NULL);

  FreelistIterate(fl, freelistDescribeIterateMethod, stream, depth + 2);

  res = WriteF(depth, stream, "} Freelist $P\n", (WriteFP)fl, NULL);
  return res;
}


/* freelistFlushIterateMethod -- Iterate method for
 * FreelistFlushToCBS. Attempst to insert the range into the CBS.
 */
static Bool freelistFlushIterateMethod(Bool *deleteReturn, Range range,
                                       void *closureP, Size closureS)
{
  Res res;
  RangeStruct newRange;
  CBS cbs;

  AVER(deleteReturn != NULL);
  AVERT(Range, range);
  AVER(closureP != NULL);
  UNUSED(closureS);

  cbs = closureP;
  res = CBSInsert(&newRange, cbs, range);
  if (res == ResOK) {
    *deleteReturn = TRUE;
    return TRUE;
  } else {
    *deleteReturn = FALSE;
    return FALSE;
  }
}


void FreelistFlushToCBS(Freelist fl, CBS cbs)
{
  AVERT(Freelist, fl);
  AVERT(CBS, cbs);

  FreelistIterate(fl, freelistFlushIterateMethod, cbs, 0);
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

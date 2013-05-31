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

#define FreelistBlock(addr) ((Addr *)(addr))

#define FreelistTag(addr) ((Word)(addr) & 1)
#define FreelistTagSet(addr) ((Addr)((Word)(addr) | 1))
#define FreelistTagReset(addr) ((Addr)((Word)(addr) & ~1))
#define FreelistTagCopy(to, from) ((Addr)((Word)(to) | FreelistTag(from)))

#define FreelistGrainSize(fl) ((fl)->alignment)


/* FreelistBlockLimit -- return the limit of a block. */

static Addr FreelistBlockLimit(Freelist fl, Addr addr)
{
  Addr *block = FreelistBlock(addr);
  if (FreelistTag(block[0])) {
    return AddrAdd(addr, FreelistGrainSize(fl));
  } else {
    return block[1];
  }
}


/* FreelistBlockNext -- return the next block in the list, or NULL if
 * there are no more blocks.
 */
static Addr FreelistBlockNext(Freelist fl, Addr addr)
{
  Addr *block = FreelistBlock(addr);
  return FreelistTagReset(block[0]);
}


/* FreelistBlockSize -- return the size of a block. */

#define FreelistBlockSize(fl, block) \
  AddrOffset(block, FreelistBlockLimit(fl, block))


/* FreelistBlockSetNext -- update the next block in the list */

static void FreelistBlockSetNext(Freelist fl, Addr addr, Addr next)
{
  Addr *block = FreelistBlock(addr);
  block[0] = FreelistTagCopy(next, block[0]);
}


/* FreelistBlockSetLimit -- update the limit of a block */

static void FreelistBlockSetLimit(Freelist fl, Addr addr, Addr limit)
{
  Addr *block = FreelistBlock(addr);
  Size size = size;
  if (size > FreelistGrainSize(fl)) {
    block[0] = FreelistTagReset(block[0]);
    block[1] = limit;
  } else {
    AVER(size == FreelistGrainSize(fl));
    block[0] = FreelistTagSet(block[0]);
  }
}


/* FreelistBlockInit -- return block for the range [base, limit). */

static Addr FreelistBlockInit(Freelist fl, Addr base, Addr limit)
{
  Addr *block = FreelistBlock(base);
  block[0] = NULL;
  FreelistBlockSetLimit(fl, base, limit);
  return base;
}


Bool FreelistCheck(Freelist fl)
{
  CHECKS(Freelist, fl);
  /* See <design/freelist/#impl.grain.align> */
  CHECKL(AlignIsAligned(fl->alignment, freelistMinimumAlignment));
  /* can't check list or listSize */
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


/* freelistBlockSetPrevNext -- make 'next' be the next block in the
 * list after 'prev', or make it the first block in the list if 'prev'
 * is NULL. Update the count of blocks by 'delta'.
 */
static void freelistBlockSetPrevNext(Freelist fl, Addr prev,
                                     Addr next, int delta)
{
  if (prev) {
    FreelistBlockSetNext(fl, prev, next);
  } else {
    fl->list = next;
  }
  if (delta < 0)
    AVER(fl->listSize >= -delta);
  fl->listSize += delta;
}


Res FreelistInsert(Range rangeReturn, Freelist fl, Range range)
{
  Addr prev, cur, next, new;
  Addr base, limit;
  Bool coalesceLeft, coalesceRight;
  Res res = ResOK;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, fl->alignment));
  AVER(RangeSize(range) >= FreelistGrainSize(fl));

  base = RangeBase(range);
  limit = RangeLimit(range);

  prev = NULL;
  cur = fl->list;
  while (cur) {
    if (base < FreelistBlockLimit(fl, cur) && cur < limit)
      return ResFAIL; /* range overlaps with cur */
    if (limit <= cur)
      break;
    next = FreelistBlockNext(fl, cur);
    if (next)
      /* Isolated range invariant (design.mps.freelist.impl.invariant). */
      AVER(FreelistBlockLimit(fl, cur) < next);
    prev = cur;
    cur = next;
  }

  /* Now we know that range does not overlap with any block, and if it
   * coalesces then it does so with prev on the left, and cur on the
   * right.
   */
  coalesceLeft = (prev && base == FreelistBlockLimit(fl, prev));
  coalesceRight = (cur && limit == cur);

  if (coalesceLeft && coalesceRight) {
    base = prev;
    limit = FreelistBlockLimit(fl, cur);
    FreelistBlockSetLimit(fl, prev, limit);
    freelistBlockSetPrevNext(fl, prev, next, -1);

  } else if (coalesceLeft) {
    base = prev;
    FreelistBlockSetLimit(fl, prev, limit);

  } else if (coalesceRight) {
    limit = FreelistBlockLimit(fl, cur);
    cur = FreelistBlockInit(fl, base, limit);
    FreelistBlockSetNext(fl, cur, next);

  } else {
    /* failed to coalesce: add new block */
    new = FreelistBlockInit(fl, base, limit);
    FreelistBlockSetNext(fl, new, cur);
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
                                    Range range, Addr prev, Addr block)
{
  Addr next, new;
  Addr base, limit, blockBase, blockLimit;

  AVER(rangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(Range, range);
  AVER(RangeIsAligned(range, fl->alignment));
  AVER(prev == NULL || FreelistBlockNext(fl, prev) == block);
  AVER(block != NULL);

  AVER(block <= RangeBase(range));
  AVER(RangeLimit(range) <= FreelistBlockLimit(fl, block));

  base = RangeBase(range);
  limit = RangeLimit(range);
  blockBase = block;
  blockLimit = FreelistBlockLimit(fl, block);
  next = FreelistBlockNext(fl, block);

  if (base == blockBase && limit == blockLimit) {
    /* No fragment at left; no fragment at right. */
    freelistBlockSetPrevNext(fl, prev, next, -1);

  } else if (base == blockBase) {
    /* No fragment at left; block at right. */
    block = FreelistBlockInit(fl, limit, blockLimit);
    FreelistBlockSetNext(fl, block, next);
    freelistBlockSetPrevNext(fl, prev, block, 0);

  } else if (limit == blockLimit) {        
    /* Block at left; no fragment at right. */
    FreelistBlockSetLimit(fl, block, base);

  } else {
    /* Block at left; block at right. */
    FreelistBlockSetLimit(fl, block, base);
    new = FreelistBlockInit(fl, limit, blockLimit);
    FreelistBlockSetNext(fl, new, next);
    freelistBlockSetPrevNext(fl, block, new, +1);
  }

  RangeInit(rangeReturn, blockBase, blockLimit);
}


Res FreelistDelete(Range rangeReturn, Freelist fl, Range range)
{
  Res res;
  Addr prev, cur, next, new;
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
    blockBase = cur;
    blockLimit = FreelistBlockLimit(fl, cur);

    if (limit <= blockBase)
      return ResFAIL; /* not found */
    if (base <= blockLimit) {
      if (base < blockBase || blockLimit < limit)
        return ResFAIL; /* partially overlapping */
      freelistDeleteFromBlock(rangeReturn, fl, range, prev, cur);
      return ResOK;
    }
    
    next = FreelistBlockNext(fl, cur);
    prev = cur;
    cur = next;
  }

  /* Range not found in block list. */
  return ResFAIL;
}


void FreelistIterate(Freelist fl, FreelistIterateMethod iterate,
                     void *closureP, Size closureS)
{
  Addr prev, cur, next;

  AVERT(Freelist, fl);
  AVER(FUNCHECK(iterate));

  prev = NULL;
  cur = fl->list;
  while (cur) {
    Bool delete = FALSE;
    RangeStruct range;
    Bool cont;
    RangeInit(&range, cur, FreelistBlockLimit(fl, cur));
    cont = (*iterate)(&delete, &range, closureP, closureS);
    next = FreelistBlockNext(fl, cur);
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
                                        Addr prev, Addr block)
{
  Bool callDelete = TRUE;
  Addr base, limit;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Freelist, fl);
  AVER(SizeIsAligned(size, fl->alignment));
  AVERT(FindDelete, findDelete);
  AVER(prev == NULL || FreelistBlockNext(fl, prev) == block);
  AVER(block != NULL);
  AVER(FreelistBlockSize(fl, block) >= size);
  
  base = block;
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


Bool FreelistFind(Range rangeReturn, Range oldRangeReturn,
                  Freelist fl, Size size, FindDelete findDelete)
{
  Res res;
  Addr prev, cur, next;

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
    next = FreelistBlockNext(fl, cur);
    prev = cur;
    cur = next;
  }

  return FALSE;
}


Bool FreelistFindLargest(Range rangeReturn, Range oldRangeReturn,
                         Freelist fl, FindDelete findDelete)
{
  Bool found = FALSE;
  Size size = 0;
  Addr prev, cur, next;
  Addr bestPrev, bestCur;

  AVER(rangeReturn != NULL);
  AVER(oldRangeReturn != NULL);
  AVERT(Freelist, fl);
  AVERT(FindDelete, findDelete);

  prev = NULL;
  cur = fl->list;
  while (cur) {
    if (FreelistBlockSize(fl, cur) > size) {
      found = TRUE;
      size = FreelistBlockSize(fl, cur);
      bestPrev = prev;
      bestCur = cur;
    }
    next = FreelistBlockNext(fl, cur);
    prev = cur;
    cur = next;
  }

  if (found) {
    freelistFindDeleteFromBlock(rangeReturn, oldRangeReturn, fl, size,
                                findDelete, bestPrev, bestCur);
    return TRUE;
  }

  return FALSE;
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
               "  listSize = $U\n", (WriteFU)fl->listSize,
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

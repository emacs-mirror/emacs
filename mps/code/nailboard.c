/* nailboard.c: NAILBOARD IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * .sources: <design/nailboard/>.
 */

#include "bt.h"
#include "check.h"
#include "mpm.h"
#include "nailboard.h"

SRCID(nailboard, "$Id$");

Bool NailboardCheck(Nailboard board)
{
  Index i;
  CHECKS(Nailboard, board);
  CHECKU(Arena, board->arena);
  CHECKL(RangeCheck(&board->range));
  CHECKL(0 < board->levelShift);
  for (i = 0; i < board->levels; ++i) {
    /* weak check for BTs @@@@ */
    CHECKL(board->level[i] != NULL);
  }
  /* distinctNails must be the same as the number of set bits in
   * level[0], but we don't want to check this as it's O(n).
   */
  CHECKL(board->distinctNails <= board->nails);
  CHECKL(BoolCheck(board->newNails));
  return TRUE;
}

/* nailboardStructSize -- return the size of the nailboard structure,
 * including its array of pointers to levels.
 */
static Size nailboardStructSize(Count levels)
{
  return sizeof(NailboardStruct) + sizeof(BT *) * (levels - 1);
}

/* nailboardLevelsSize -- return the total size of the bit tables for
 * all the levels in a nailboard with the given number of paramaters.
 */
static Size nailboardLevelsSize(Count nails, Count levels, Shift shift)
{
  Index i;
  Size size;
  AVER(nails >> ((levels - 1) * shift) != 0);
  AVER(nails >> (levels * shift) <= 1);
  size = 0;
  for (i = 0; i < levels; ++i) {
    size += BTSize(nails >> (i * shift));
  }
  return size;
}

/* NailboardCreate -- allocate a nailboard
 *
 * Allocate a nailboard in the control pool for arena, to cover the
 * range of addresses from base to limit (which must be non-empty). If
 * successful, set *boardReturn to point to the nailboard and return
 * ResOK. Otherwise, return a result code to indicate failure.
 * 
 * alignment specifies the granularity of the nails: that is, the
 * number of bytes covered by each nail.
 */
Res NailboardCreate(Nailboard *boardReturn, Arena arena, Align alignment,
                    Addr base, Addr limit)
{
  void *p;
  Nailboard board;
  Count nails, levels;
  Index i;
  Size structSize, levelsSize;
  Res res;
  Shift levelShift = MPS_WORD_SHIFT;

  AVER(boardReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Align, alignment);
  AVER(base < limit);
  AVER(AddrIsAligned(base, alignment));
  AVER(AddrIsAligned(limit, alignment));

  nails = AddrOffset(base, limit) / alignment;
  levels = SizeRoundUp(SizeLog2(nails) + 1, levelShift) / levelShift;
  AVER((nails >> ((levels - 1) * levelShift)) > 0);
  AVER((nails >> (levels * levelShift)) == 0);

  structSize = nailboardStructSize(levels);
  levelsSize = nailboardLevelsSize(nails, levels, levelShift);
  res = ControlAlloc(&p, arena, structSize + levelsSize, FALSE);
  if (res != ResOK)
    return res;

  board = p;
  board->arena = arena;
  board->nails = 0;
  board->distinctNails = 0;
  board->newNails = FALSE;
  board->levels = levels;
  board->alignShift = SizeLog2(alignment);
  board->levelShift = levelShift;
  RangeInit(&board->range, base, limit);

  AVER(nails == RangeSize(&board->range) >> board->alignShift);
  p = AddrAdd(p, structSize);
  for (i = 0; i < levels; ++i) {
    Count levelNails = nails >> (i * levelShift);
    AVER(levelNails > 0);
    board->level[i] = p;
    BTResRange(board->level[i], 0, levelNails);
    p = AddrAdd(p, BTSize(levelNails));
  }

  board->sig = NailboardSig;
  AVERT(Nailboard, board);
  *boardReturn = board;
  return ResOK;
}

/* NailboardDestroy -- destroy a nailboard */

void NailboardDestroy(Nailboard board)
{
  Arena arena;
  Count nails;
  Size structSize, levelsSize;

  AVERT(Nailboard, board);

  arena = board->arena;
  nails = RangeSize(&board->range) >> board->alignShift;
  structSize = nailboardStructSize(board->levels);
  levelsSize = nailboardLevelsSize(nails, board->levels, board->levelShift);

  board->sig = SigInvalid;
  ControlFree(arena, board, structSize + levelsSize);
}

/* NailboardClearNewNails -- clear the "new nails" flag */

void NailboardClearNewNails(Nailboard board)
{
  board->newNails = FALSE;
}

/* NailboardNewNails -- return the "new nails" flag
 *
 * Return TRUE if any new nails have been set in the nailboard since
 * the last call to NailboardClearNewNails (or since the nailboard was
 * created, if there have never been any such calls), FALSE otherwise.
 */
Bool NailboardNewNails(Nailboard board)
{
  return board->newNails;
}

/* nailboardIndex -- return the index of the nail corresponding to
 * addr in the given level.
 */
static Index nailboardIndex(Nailboard board, Index level, Addr addr)
{
  return AddrOffset(RangeBase(&board->range), addr)
    >> (board->alignShift + level * board->levelShift);
}

/* nailboardAddr -- return the address corresponding to the index in
 * the given level.
 */
static Addr nailboardAddr(Nailboard board, Index level, Index index)
{
  return AddrAdd(RangeBase(&board->range),
                 index << (board->alignShift + level * board->levelShift));
}

/* nailboardIndexRange -- update *ibaseReturn and *ilimitReturn to be
 * the indexes of the nail corresponding to base and limit
 * respectively, in the given level.
 */
static void nailboardIndexRange(Index *ibaseReturn, Index *ilimitReturn,
                                Nailboard board, Index level,
                                Addr base, Addr limit)
{
  *ibaseReturn = nailboardIndex(board, level, base);
  *ilimitReturn = nailboardIndex(board, level, AddrSub(limit, 1)) + 1;
}

/* NailboardGet -- return nail corresponding to address
 * 
 * Return the nail in the nailboard corresponding to the address addr.
 * It is an error if addr does not lie in the range of addresses
 * covered by the nailboard.
 */
Bool NailboardGet(Nailboard board, Addr addr)
{
  AVERT(Nailboard, board);
  AVER(RangeContains(&board->range, addr));
  return BTGet(board->level[0], nailboardIndex(board, 0, addr));
}

/* NailboardSet -- set nail corresponding to address
 *
 * Set the nail in the nailboard corresponding to the address addr.
 * Return the old nail at that position. It is an error if addr does
 * not lie in the range of addresses covered by the nailboard.
 */
Bool NailboardSet(Nailboard board, Addr addr)
{
  Bool isNew = FALSE;
  Index i;

  AVERT_CRITICAL(Nailboard, board);
  AVER_CRITICAL(RangeContains(&board->range, addr));

  ++ board->nails;
  for (i = 0; i < board->levels; ++i) {
    Index j = nailboardIndex(board, i, addr);
    if (BTGet(board->level[i], j)) {
      break;
    }
    BTSet(board->level[i], j);
    isNew = TRUE;
  }
  if (isNew) {
    board->newNails = TRUE;
    ++ board->distinctNails;
    return FALSE;
  }
  return TRUE;
}

/* NailboardSetRange -- set all nails in range
 *
 * Set all nails in the nailboard corresponding to the range between
 * base and limit. It is an error if any part of the range is not
 * covered by the nailboard, or if any nail in the range is set.
 */
void NailboardSetRange(Nailboard board, Addr base, Addr limit)
{
  Index i, ibase, ilimit;
  nailboardIndexRange(&ibase, &ilimit, board, 0, base, limit);
  AVER(BTIsResRange(board->level[0], ibase, ilimit));
  BTSetRange(board->level[0], ibase, ilimit);
  board->nails += ilimit - ibase;
  board->distinctNails += ilimit - ibase;
  for (i = 1; i < board->levels; ++i) {
    nailboardIndexRange(&ibase, &ilimit, board, i, base, limit);
    BTSetRange(board->level[i], ibase, ilimit);
  }
}

/* NailboardIsSetRange -- test if all nails are set in a range
 *
 * Return TRUE if all nails are set in the range between base and
 * limit, or FALSE if any nail is unset. It is an error if any part of
 * the range is not covered by the nailboard.
 */
Bool NailboardIsSetRange(Nailboard board, Addr base, Addr limit)
{
  Index ibase, ilimit;
  AVERT(Nailboard, board);
  nailboardIndexRange(&ibase, &ilimit, board, 0, base, limit);
  return board->distinctNails >= ilimit - ibase
    && BTIsSetRange(board->level[0], ibase, ilimit);
}

/* NailboardIsResRange -- test if all nails are reset in a range
 *
 * Return TRUE if no nails are set in the range between base and
 * limit, or FALSE if any nail is set. It is an error if any part of
 * the range is not covered by the nailboard.
 */
Bool NailboardIsResRange(Nailboard board, Addr base, Addr limit)
{
  Index i, ibase, ilimit;
  Addr leftLimit, rightBase;
  AVERT_CRITICAL(Nailboard, board);
  i = board->levels - 1;
  nailboardIndexRange(&ibase, &ilimit, board, i, base, limit);
  if (BTIsResRange(board->level[i], ibase, ilimit))
    return TRUE;
  if (ibase + 1 < ilimit - 1
      && !BTIsResRange(board->level[i], ibase + 1, ilimit - 1))
    return FALSE;
  leftLimit = nailboardAddr(board, i, ibase + 1);
  if (leftLimit > limit) leftLimit = limit;
  rightBase = nailboardAddr(board, i, ilimit - 1);
  if (rightBase < base) rightBase = base;

  /* Left splinter */
  i = board->levels - 1;
  while (i > 0) {
    i -= 1;
    nailboardIndexRange(&ibase, &ilimit, board, i, base, leftLimit);
    if (ibase + 1 < ilimit
        && !BTIsResRange(board->level[i], ibase + 1, ilimit))
      return FALSE;
    if (!BTGet(board->level[i], ibase))
      goto leftSplinterRes;
    leftLimit = nailboardAddr(board, i, ibase + 1);
    if (leftLimit > limit) leftLimit = limit;
  }
  return FALSE;
 leftSplinterRes:

  /* Right splinter */
  i = board->levels - 1;
  while (i > 0) {
    i -= 1;
    nailboardIndexRange(&ibase, &ilimit, board, i, rightBase, limit);
    if (ibase < ilimit - 1
        && !BTIsResRange(board->level[i], ibase, ilimit - 1))
      return FALSE;
    if (!BTGet(board->level[i], ilimit - 1))
      goto rightSplinterRes;
    rightBase = nailboardAddr(board, i, ilimit - 1);
    if (rightBase < base) rightBase = base;
  }
  return FALSE;
 rightSplinterRes:

  return TRUE;
}

/* NailboardIsResClientRange -- test if all nails are reset in a range
 *
 * As NailboardIsResRange, except that the addresses are client
 * addresses for objects with the given header size.
 */
Bool NailboardIsResClientRange(Nailboard board, Size headerSize, Addr base, Addr limit)
{
  return NailboardIsResRange(board, AddrSub(base, headerSize),
                             AddrSub(limit, headerSize));
}

Res NailboardDescribe(Nailboard board, mps_lib_FILE *stream)
{
  Count nails;
  Index i, j;

  if(!TESTT(Nailboard, board))
    return ResFAIL;
  if(stream == NULL)
    return ResFAIL;

  nails = RangeSize(&board->range) >> board->alignShift;
  for(i = 0; i < board->levels; ++i) {
    Count levelNails = nails >> (i * board->levelShift);
    Res res;
    res = WriteF(stream, "  Level $U: ", i, NULL);
    if(res != ResOK)
      return res;
    for (j = 0; j < levelNails; ++j) {
      char c = BTGet(board->level[i], j) ? '*' : '.';
      res = WriteF(stream, "$C", c, NULL);
      if(res != ResOK)
        return res;
    }
    res = WriteF(stream, "\n", NULL);
    if(res != ResOK)
      return res;
  }

  return ResOK;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

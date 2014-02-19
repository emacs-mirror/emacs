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
  CHECKL(board->levels <= NAILBOARD_MAX_LEVELS);
  CHECKL(0 < board->alignShift);
  CHECKL(0 < board->levelShift);
  for (i = 0; i < board->levels; ++i) {
    /* weak check for BTs @@@@ */
    CHECKL(board->level[i] != NULL);
  }
  /* distinctNails must be the same as the number of set bits in
   * level[0].mark, but we don't want to check this as it's O(n).
   */
  CHECKL(board->distinctNails <= board->nails);
  CHECKL(BoolCheck(board->newNails));
  return TRUE;
}

static Size nailboardSize(Count bits, Count levels, Shift shift)
{
  Index i;
  Size size;
  AVER(bits >> ((levels - 1) * shift) != 0);
  AVER(bits >> (levels * shift) == 0);
  size = 0;
  for (i = 0; i < levels; ++i) {
    size += BTSize(bits >> (i * shift));
  }
  return size;
}

Res NailboardCreate(Nailboard *boardReturn, Arena arena, Align alignment,
                    Addr base, Addr limit)
{
  void *p;
  Nailboard board;
  Count bits, levels;
  Index i;
  Size size;
  Res res;
  Shift levelShift = MPS_WORD_SHIFT;

  AVER(boardReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Align, alignment);
  AVER(base < limit);
  AVER(AddrIsAligned(base, alignment));
  AVER(AddrIsAligned(limit, alignment));

  bits = AddrOffset(base, limit) / alignment;
  levels = SizeRoundUp(SizeLog2(bits), levelShift) / levelShift;
  AVER(levels <= NAILBOARD_MAX_LEVELS);
  size = sizeof(NailboardStruct) + sizeof(BT*) * (levels - 1);
  res = ControlAlloc(&p, arena, size, FALSE);
  if(res != ResOK)
    goto failAllocNailboard;
  board = p;
  board->arena = arena;
  board->nails = (Count)0;
  board->distinctNails = (Count)0;
  board->newNails = FALSE;
  board->levels = levels;
  board->alignShift = SizeLog2(alignment);
  board->levelShift = levelShift;
  RangeInit(&board->range, base, limit);

  res = ControlAlloc(&p, arena, nailboardSize(bits, levels, levelShift), FALSE);
  if(res != ResOK)
    goto failAllocNails;

  AVER(bits == RangeSize(&board->range) >> board->alignShift);
  for (i = 0; i < levels; ++i) {
    AVER(bits > 0);
    board->level[i] = p;
    BTResRange(board->level[i], 0, bits);
    p = AddrAdd(p, BTSize(bits));
    bits >>= levelShift;
  }
  AVER(bits == 0);

  board->sig = NailboardSig;
  AVERT(Nailboard, board);
  *boardReturn = board;
  return ResOK;

failAllocNails:
  ControlFree(arena, board, sizeof(NailboardStruct));
failAllocNailboard:
  return res;
}

void NailboardDestroy(Nailboard board)
{
  Arena arena;
  Count bits;

  AVERT(Nailboard, board);

  arena = board->arena;
  bits = RangeSize(&board->range) >> board->alignShift;
  ControlFree(arena, board->level[0],
              nailboardSize(bits, board->levels, board->levelShift));
  board->sig = SigInvalid;
  ControlFree(arena, board, sizeof(NailboardStruct));
}

Align NailboardAlignment(Nailboard board)
{
  return (Align)1 << board->alignShift;
}

void NailboardClearNewNails(Nailboard board)
{
  board->newNails = FALSE;
}

Bool NailboardNewNails(Nailboard board)
{
  return board->newNails;
}

/* nailboardIndex -- return the index of the nail corresponding to
 * addr in the given level.
 */
static Index nailboardIndex(Nailboard board, Index level, Addr addr)
{
  AVERT(Nailboard, board);
  AVER(level < board->levels);
  AVER(RangeBase(&board->range) <= addr);
  AVER(addr <= RangeLimit(&board->range));

  return AddrOffset(RangeBase(&board->range), addr)
    >> (board->alignShift + level * board->levelShift);
}

/* nailboardIndexRange -- update *ibaseReturn and *ilimitReturn to be
 * the indexes of the nail corresponding to base and limit
 * respectively, in the given level.
 */
static void nailboardIndexRange(Index *ibaseReturn, Index *ilimitReturn,
                                Nailboard board, Index level,
                                Addr base, Addr limit)
{
  AVER(base < limit);

  *ibaseReturn = nailboardIndex(board, level, base);
  *ilimitReturn = nailboardIndex(board, level, AddrSub(limit, 1)) + 1;
}

Bool NailboardGet(Nailboard board, Addr addr)
{
  AVERT(Nailboard, board);
  AVER(RangeContains(&board->range, addr));
  return BTGet(board->level[0], nailboardIndex(board, 0, addr));
}

Bool NailboardSet(Nailboard board, Addr addr)
{
  Index i, j;

  AVERT(Nailboard, board);
  AVER(RangeContains(&board->range, addr));

  ++ board->nails;
  j = nailboardIndex(board, 0, addr);
  if (BTGet(board->level[0], j)) {
    return TRUE;
  }
  BTSet(board->level[0], j);
  board->newNails = TRUE;
  ++ board->distinctNails;
  for (i = 1; i < board->levels; ++i) {
    j = nailboardIndex(board, i, addr);
    if (BTGet(board->level[i], j)) {
      break;
    }
    BTSet(board->level[i], j);
  }
  return FALSE;
}

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

Bool NailboardIsSetRange(Nailboard board, Addr base, Addr limit)
{
  Index ibase, ilimit;
  AVERT(Nailboard, board);
  nailboardIndexRange(&ibase, &ilimit, board, 0, base, limit);
  return board->distinctNails >= ilimit - ibase
    && BTIsSetRange(board->level[0], ibase, ilimit);
}

Bool NailboardIsResRange(Nailboard board, Addr base, Addr limit)
{
  Index i;
  AVERT(Nailboard, board);
  i = board->levels;
  while (i > 0) {
    Index ibase, ilimit;
    i -= 1;
    nailboardIndexRange(&ibase, &ilimit, board, i, base, limit);
    if (BTIsResRange(board->level[i], ibase, ilimit))
      return TRUE;
  }
  return FALSE;
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

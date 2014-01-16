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
  CHECKS(Nailboard, board);
  CHECKU(Arena, board->arena);
  CHECKL(RangeCheck(&board->range));
  /* nails is >= number of set bits in mark, but we can't check this */
  /* We know that shift corresponds to pool->align. */
  CHECKL(BoolCheck(board->newMarks));
  CHECKL(board->distinctNails <= board->nails);
  /* weak check for BTs @@@@ */
  CHECKL(board->mark != NULL);
  return TRUE;
}

Res NailboardCreate(Nailboard *boardReturn, Arena arena, Align alignment,
                    Range range)
{
  void *p;
  Nailboard board;
  Count bits;
  Res res;

  AVER(boardReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Range, range);

  res = ControlAlloc(&p, arena, sizeof(NailboardStruct), FALSE);
  if(res != ResOK)
    goto failAllocNailboard;
  board = p;
  board->arena = arena;
  board->nails = (Count)0;
  board->distinctNails = (Count)0;
  board->newMarks = FALSE;
  board->markShift = SizeLog2((Size)alignment);
  RangeInitCopy(&board->range, range);
  bits = RangeSize(&board->range) >> board->markShift;
  res = ControlAlloc(&p, arena, BTSize(bits), FALSE);
  if(res != ResOK)
    goto failMarkTable;
  board->mark = p;
  BTResRange(board->mark, 0, bits);
  board->sig = NailboardSig;
  AVERT(Nailboard, board);
  *boardReturn = board;
  return ResOK;

failMarkTable:
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

  bits = RangeSize(&board->range) >> board->markShift;
  ControlFree(board->arena, board->mark, BTSize(bits));
  board->sig = SigInvalid;
  ControlFree(arena, board, sizeof(NailboardStruct));
}

Align NailboardAlignment(Nailboard board)
{
  return 1 << board->markShift;
}

/* nailboardIndex -- return the index of the nail corresponding to addr */

static Index nailboardIndex(Nailboard board, Addr addr)
{
  return AddrOffset(RangeBase(&board->range), addr) >> board->markShift;
}

Bool NailboardGet(Nailboard board, Addr addr)
{
  AVERT(Nailboard, board);
  AVER(RangeContains(&board->range, addr));
  return BTGet(board->mark, nailboardIndex(board, addr));
}

Bool NailboardSet(Nailboard board, Addr addr)
{
  Index i;

  AVERT(Nailboard, board);
  AVER(RangeContains(&board->range, addr));

  ++ board->nails;
  i = nailboardIndex(board, addr);
  if (!BTGet(board->mark, i)) {
    BTSet(board->mark, i);
    board->newMarks = TRUE;
    ++ board->distinctNails;
    return FALSE;
  }
  return TRUE;
}

void NailboardSetRange(Nailboard board, Range range)
{
  Index ibase, ilimit;

  AVERT(Nailboard, board);
  AVERT(Range, range);
  AVER(RangesNest(&board->range, range));

  ibase = nailboardIndex(board, RangeBase(range));
  ilimit = nailboardIndex(board, RangeLimit(range));

  AVER(BTIsResRange(board->mark, ibase, ilimit));
  BTSetRange(board->mark, ibase, ilimit);
  board->nails += ilimit - ibase;
  board->distinctNails += ilimit - ibase;
}


/* amcNailRangeIsMarked -- check that a range in the board is marked
 *
 * Like amcNailMarkRange, we take the arguments as referring to base
 * pointers and look at the bits of the corresponding client pointers.
 */
Bool NailboardIsSetRange(Nailboard board, Range range)
{
  Index ibase, ilimit;

  AVERT(Nailboard, board);
  AVERT(Range, range);
  AVER(RangesNest(&board->range, range));

  ibase = nailboardIndex(board, RangeBase(range));
  ilimit = nailboardIndex(board, RangeLimit(range));
  return board->distinctNails >= ilimit - ibase
    && BTIsSetRange(board->mark, ibase, ilimit);
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

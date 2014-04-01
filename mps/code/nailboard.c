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


/* Log2 of scale factor between levels. See <design/nailboard/#impl.scale>. */
#define LEVEL_SHIFT MPS_WORD_SHIFT


/* nailboardLevels -- return the number of levels in a nailboard with
 * the given number of nails.
 *
 * See <design/nailboard/#impl.table.last>
 */

static Count nailboardLevels(Count nails)
{
  return SizeRoundUp(SizeFloorLog2(nails) + 1, LEVEL_SHIFT) / LEVEL_SHIFT;
}


/* nailboardLevelBits -- return the number of bits in the bit table
 * for the given level.
 */

static Count nailboardLevelBits(Nailboard board, Index level)
{
  /* Use <= rather than < because of .check.levels. */
  AVER(level <= board->levels);
  return RangeSize(&board->range) >> (board->alignShift + level * LEVEL_SHIFT);
}

Bool NailboardCheck(Nailboard board)
{
  Index i;
  CHECKS(Nailboard, board);
  CHECKL(RangeCheck(&board->range));
  CHECKL(0 < board->levels);
  CHECKL(board->levels == nailboardLevels(nailboardLevelBits(board, 0)));
  CHECKL(nailboardLevelBits(board, board->levels - 1) != 0);
  CHECKL(nailboardLevelBits(board, board->levels) == 0); /* .check.levels */
  CHECKL(BoolCheck(board->newNails));
  for (i = 0; i < board->levels; ++i) {
    CHECKL(board->level[i] != NULL);
  }
  return TRUE;
}


/* nailboardStructSize -- return the size of the nailboard structure,
 * plus the array of pointers to levels.
 */

static Size nailboardStructSize(Count levels)
{
  return offsetof(NailboardStruct, level) + sizeof(BT *) * levels;
}


/* nailboardSize -- return the total size of the nailboard
 *
 * This is the size of the nailboard structure plus the combined sizes
 * of the bit tables.
 */

static Size nailboardSize(Count nails, Count levels)
{
  Index i;
  Size size;
  size = nailboardStructSize(levels);
  for (i = 0; i < levels; ++i) {
    size += BTSize(nails);
    nails >>= LEVEL_SHIFT;
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
  Shift alignShift;
  Count nails, levels;
  Index i;
  Res res;

  AVER(boardReturn != NULL);
  AVERT(Arena, arena);
  AVERT(Align, alignment);
  AVER(base < limit);
  AVER(AddrIsAligned(base, alignment));
  AVER(AddrIsAligned(limit, alignment));

  alignShift = SizeLog2((Size)alignment);
  nails = AddrOffset(base, limit) >> alignShift;
  levels = nailboardLevels(nails);
  res = ControlAlloc(&p, arena, nailboardSize(nails, levels), FALSE);
  if (res != ResOK)
    return res;

  board = p;
  RangeInit(&board->range, base, limit);
  board->levels = levels;
  board->alignShift = alignShift;
  board->newNails = FALSE;

  p = (char *)p + nailboardStructSize(levels);
  for (i = 0; i < levels; ++i) {
    AVER(nails > 0);
    board->level[i] = p;
    BTResRange(board->level[i], 0, nails);
    p = (char *)p + BTSize(nails);
    nails >>= LEVEL_SHIFT;
  }
  
  board->sig = NailboardSig;
  AVERT(Nailboard, board);
  *boardReturn = board;
  return ResOK;
}


/* NailboardDestroy -- destroy a nailboard */

void NailboardDestroy(Nailboard board, Arena arena)
{
  Count nails;
  Size size;

  AVERT(Nailboard, board);
  AVERT(Arena, arena);

  nails = nailboardLevelBits(board, 0);
  size = nailboardSize(nails, board->levels);

  board->sig = SigInvalid;
  ControlFree(arena, board, size);
}


/* NailboardClearNewNails -- clear the "new nails" flag */

void (NailboardClearNewNails)(Nailboard board)
{
  AVERT(Nailboard, board);
  NailboardClearNewNails(board);
}


/* NailboardNewNails -- return the "new nails" flag
 *
 * Return TRUE if any new nails have been set in the nailboard since
 * the last call to NailboardClearNewNails (or since the nailboard was
 * created, if there have never been any such calls), FALSE otherwise.
 */

Bool (NailboardNewNails)(Nailboard board)
{
  AVERT(Nailboard, board);
  return NailboardNewNails(board);
}


/* nailboardIndex -- return the index of the nail corresponding to
 * addr in the given level.
 */

static Index nailboardIndex(Nailboard board, Index level, Addr addr)
{
  return AddrOffset(RangeBase(&board->range), addr)
    >> (board->alignShift + level * LEVEL_SHIFT);
}


/* nailboardAddr -- return the address corresponding to the index in
 * the given level.
 */

static Addr nailboardAddr(Nailboard board, Index level, Index index)
{
  return AddrAdd(RangeBase(&board->range),
                 index << (board->alignShift + level * LEVEL_SHIFT));
}


/* nailboardIndexRange -- update *ibaseReturn and *ilimitReturn to be
 * the interval of indexes of nails in the given level, corresponding
 * to the interval of addresses base and limit. See
 * <design/nailboard/#.impl.isresrange.alignment>.
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
 *
 * This function is on the critical path because it is called for
 * every fix of an ambiguous reference to an address in an AMC pool.
 */

Bool NailboardSet(Nailboard board, Addr addr)
{
  Index i, j;

  AVERT_CRITICAL(Nailboard, board);
  AVER_CRITICAL(RangeContains(&board->range, addr));

  j = nailboardIndex(board, 0, addr);
  if (BTGet(board->level[0], j))
    return TRUE;
  board->newNails = TRUE;
  BTSet(board->level[0], j);

  for (i = 1; i < board->levels; ++i) {
    j = nailboardIndex(board, i, addr);
    if (BTGet(board->level[i], j))
      break;
    BTSet(board->level[i], j);
  }
  return FALSE;
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
 *
 * This function is not expected to be efficient because it is only
 * used in an AVER in AMCWhiten to check that the unused part of the
 * buffer for a nailboarded segment has in fact been nailed.
 */

Bool NailboardIsSetRange(Nailboard board, Addr base, Addr limit)
{
  Index ibase, ilimit;
  AVERT(Nailboard, board);
  nailboardIndexRange(&ibase, &ilimit, board, 0, base, limit);
  return BTIsSetRange(board->level[0], ibase, ilimit);
}


/* NailboardIsResRange -- test if all nails are reset in a range
 *
 * Return TRUE if no nails are set in the range between base and
 * limit, or FALSE if any nail is set. It is an error if any part of
 * the range is not covered by the nailboard.
 *
 * This function is on the critical path as it is called for every
 * object in every nailed segment. It must take time that is no more
 * than logarithmic in the size of the range.
 *
 * See <design/nailboard/#impl.isresrange>.
 */

Bool NailboardIsResRange(Nailboard board, Addr base, Addr limit)
{
  Index i, ibase, ilimit;
  Index j, jbase, jlimit;
  Addr leftLimit, rightBase;

  AVERT_CRITICAL(Nailboard, board);

  /* Descend levels until ibase and ilimit are two or more bits apart:
   * that is, until there is an "inner" part to the range. */
  i = board->levels;
  do {
    -- i;
    nailboardIndexRange(&ibase, &ilimit, board, i, base, limit);
    if (BTIsResRange(board->level[i], ibase, ilimit))
      /* The entire range was clear. This is expected to be the common
       * case. <design/nailboard/#impl.isresrange.empty> */
      return TRUE;
    if (i == 0)
      /* At level 0 there is only one nail per bit so the set bit is known
       * to be within the range. <design/nailboard/#impl.isresrange.level0> */
      return FALSE;
  } while (ibase + 1 >= ilimit - 1);

  /* At this point we know there is an "inner" part. Are there any
   * bits set in it? <design/nailboard#impl.isresrange.inner> */
  if (!BTIsResRange(board->level[i], ibase + 1, ilimit - 1))
    return FALSE;

  /* At this point we know that in level i, there is is a bit set at
   * ibase or at ilimit - 1 (or both), and everything between them is
   * reset. */
  AVER_CRITICAL(BTGet(board->level[i], ibase)
                || BTGet(board->level[i], ilimit - 1));

  /* Left splinter */
  for (j = i, jbase = ibase;;) {
    leftLimit = nailboardAddr(board, j, jbase + 1);
    AVER_CRITICAL(base < leftLimit);
    AVER_CRITICAL(leftLimit < limit);
    -- j;
    nailboardIndexRange(&jbase, &jlimit, board, j, base, leftLimit);
    if (jbase + 1 < jlimit && !BTIsResRange(board->level[j], jbase + 1, jlimit))
      return FALSE;    /* <design/nailboard/#impl.isresrange.inner> */
    if (!BTGet(board->level[j], jbase))
      break;
    if (j == 0)
      return FALSE;
  }

  /* Right splinter */
  for (j = i, jlimit = ilimit;;) {
    rightBase = nailboardAddr(board, j, jlimit - 1);
    AVER_CRITICAL(base < rightBase);
    AVER_CRITICAL(rightBase < limit);
    -- j;
    nailboardIndexRange(&jbase, &jlimit, board, j, rightBase, limit);
    if (jbase < jlimit - 1 && !BTIsResRange(board->level[j], jbase, jlimit - 1))
      return FALSE;    /* <design/nailboard/#impl.isresrange.inner> */
    if (!BTGet(board->level[j], jlimit - 1))
      break;
    if (j == 0)
      return FALSE;
  }

  return TRUE;
}


Res NailboardDescribe(Nailboard board, mps_lib_FILE *stream)
{
  Index i, j;
  Res res;

  if (!TESTT(Nailboard, board))
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream,
               "Nailboard $P\n{\n", (WriteFP)board,
               "  base: $P\n", (WriteFP)RangeBase(&board->range),
               "  limit: $P\n", (WriteFP)RangeLimit(&board->range),
               "  levels: $U\n", (WriteFU)board->levels,
               "  newNails: $S\n", board->newNails ? "TRUE" : "FALSE",
               "  alignShift: $U\n", (WriteFU)board->alignShift,
               NULL);
  if (res != ResOK)
    return res;

  for(i = 0; i < board->levels; ++i) {
    Count levelNails = nailboardLevelBits(board, i);
    Count resetNails = BTCountResRange(board->level[i], 0, levelNails);
    res = WriteF(stream, "  Level $U ($U bits, $U set): ",
                 i, levelNails, levelNails - resetNails, NULL);
    if (res != ResOK)
      return res;
    for (j = 0; j < levelNails; ++j) {
      char c = BTGet(board->level[i], j) ? '*' : '.';
      res = WriteF(stream, "$C", c, NULL);
      if (res != ResOK)
        return res;
    }
    res = WriteF(stream, "\n", NULL);
    if (res != ResOK)
      return res;
  }
  res = WriteF(stream, "}\n", NULL);
  if (res != ResOK)
    return res;

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

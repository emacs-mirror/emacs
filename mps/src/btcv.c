/*  impl.c.btss: BIT TABLE COVERAGE TEST
 *
 *  $HopeName: !btcv.c(trunk.4) $
 * Copyright (C) 2000 Harlequin Ltd.  All rights reserved.
 *
 * .readership: MPS developers
 *
 * .coverage: Direct coverage of BTFind*ResRange*, BTRangesSame,
 * BTISResRange, BTIsSetRange, BTCopyRange, BTCopyOffsetRange.
 * Reasonable coverage of BTCopyInvertRange, BTResRange,
 * BTSetRange, BTRes, BTSet, BTCreate, BTDestroy.
 */


#include "mpm.h"
#include "mpsavm.h"
#include "mps.h"
#include "testlib.h"

#include <stdio.h>
#include <stdlib.h>

SRCID(btcv, "$HopeName: !btcv.c(trunk.4) $");


static void die_bool(Bool res, Bool expected, const char *s)
{
  die_expect((mps_res_t)res, (mps_res_t)expected, s);
}


/* bt*Symmetric -- Symmetric operations on bit tables
 *
 * The operations take 2 bit tables, btlo & bthi.
 * They perform the equivalent BT* operation on btlo, and
 * a reflected operation on the bits of bthi from the opposite
 * direction.
 */

#define btReflectIndex(btSize, i) (btSize - (i) - 1)
#define btReflectLimit(btSize, i) (btSize - (i))


static void btSetSymmetric(BT btlo, BT bthi, Count btSize, Index i)
{
  BTSet(btlo, i);
  BTSet(bthi, btReflectIndex(btSize, i));
}

static void btResSymmetric(BT btlo, BT bthi, Count btSize, Index i)
{
  BTRes(btlo, i);
  BTRes(bthi, btReflectIndex(btSize, i));
}

static void btSetRangeSymmetric(BT btlo, BT bthi, Count btSize,
                                Index base, Index limit)
{
  BTSetRange(btlo, base, limit);
  BTSetRange(bthi, btReflectLimit(btSize, limit), btReflectLimit(btSize, base));
}

static void btResRangeSymmetric(BT btlo, BT bthi, Count btSize,
                                Index base, Index limit)
{
  BTResRange(btlo, base, limit);
  BTResRange(bthi, btReflectLimit(btSize, limit), btReflectLimit(btSize, base));
}


typedef Bool (*BTFinderFn)(Index *, Index *, BT, Index, Index, unsigned long);


/* btTestSingleRange -- Test expectations for calls to BTFind*ResRange*
 *
 */

static void btTestSingleRange(BTFinderFn finder, BT bt,
                              Index base, Index limit,
                              unsigned long length,
                              Bool expect,
                              Index expectBase, Index expectLimit)
{
   Bool found;
   Index foundBase, foundLimit;

   found = finder(&foundBase, &foundLimit, bt, base, limit, length);
   die_bool(found, expect, "FindResRange result");
   if (expect) {
     die_bool(foundBase == expectBase, TRUE, "FindResRange base");
     die_bool(foundLimit == expectLimit, TRUE, "FindResRange limit");
   }
}


/* btTestResRange -- Test expectations for calls to BTFindShortResRange
 *
 * Symmetrically call BTFindShortResRange / BTFindShortResRangeHigh
 * and test the expected results
 */

static void btTestResRange(BT btlo, BT bthi, Count btSize,
                           Index base, Index limit,
                           unsigned long length,
                           Bool expect,
                           Index expectBase, Index expectLimit)
{
  btTestSingleRange(BTFindShortResRange, btlo,
                    base, limit,
                    length, expect,
                    expectBase, expectLimit);

  btTestSingleRange(BTFindShortResRangeHigh, bthi,
                    btReflectLimit(btSize, limit),
                    btReflectLimit(btSize, base),
                    length, expect,
                    btReflectLimit(btSize, expectLimit),
                    btReflectLimit(btSize, expectBase));
}


/* btTestLongResRange -- Test expectations for calls to BTFindLongResRange
 *
 * Symmetrically call BTFindLongResRange / BTFindLongResRangeHigh
 * and test the expected results
 */

static void btTestLongResRange(BT btlo, BT bthi, Count btSize,
                               Index base, Index limit,
                               unsigned long length,
                               Bool expect,
                               Index expectBase, Index expectLimit)
{
  btTestSingleRange(BTFindLongResRange, btlo,
                    base, limit,
                    length, expect,
                    expectBase, expectLimit);

  btTestSingleRange(BTFindLongResRangeHigh, bthi,
                    btReflectLimit(btSize, limit),
                    btReflectLimit(btSize, base),
                    length, expect,
                    btReflectLimit(btSize, expectLimit),
                    btReflectLimit(btSize, expectBase));
}


/* btAllResTest -- tests with only a reset range
 *
 * Test finding reset ranges in an all-reset table.
 */

static void btAllResTest(BT btlo, BT bthi, Count btSize,
                         Index base, Index limit,
                         unsigned long length)
{
  btResRangeSymmetric(btlo, bthi, btSize, 0, btSize);
  btTestResRange(btlo, bthi, btSize, base, limit, length,
                 TRUE, base, base + length);
  btTestLongResRange(btlo, bthi, btSize, base, limit, length,
                     TRUE, base, limit);
}


/* btNoResTest -- tests with no reset ranges
 *
 * Test finding reset ranges in an all-set search area of a table.
 * Reset the area outside the search to ensure it doesn't get found
 * by mistake.
 */

static void btNoResTest(BT btlo, BT bthi, Count btSize,
                        Index base, Index limit,
                        unsigned long length)
{
  btResRangeSymmetric(btlo, bthi, btSize, 0, btSize);
  btSetRangeSymmetric(btlo, bthi, btSize, base, limit);
  btTestResRange(btlo, bthi, btSize, base, limit, length,
                 FALSE, 0, 0);
  btTestLongResRange(btlo, bthi, btSize, base, limit, length,
                     FALSE, 0, 0);
}


/* btResAndFindTest -- Test finding ranges of given size
 *
 * Resets the range between resBase & resLimit, and then attempts
 * to find it by searching in the range between base & limit.
 * Expect to find the range if it's long enough,
 */

static void btResAndFindTest(BT btlo, BT bthi, Count btSize,
                             Index base, Index limit,
                             Index resBase, Index resLimit,
                             unsigned long length)
{
  btResRangeSymmetric(btlo, bthi, btSize, resBase, resLimit);
  if ((resLimit - resBase) < length) {
    btTestResRange(btlo, bthi, btSize, base, limit, length,
                   FALSE, 0, 0);
    btTestLongResRange(btlo, bthi, btSize, base, limit, length,
                       FALSE, 0, 0);
  } else {
    btTestResRange(btlo, bthi, btSize, base, limit, length,
                   TRUE, resBase, resBase + length);
    btTestLongResRange(btlo, bthi, btSize, base, limit, length,
                       TRUE, resBase, resLimit);
  }
}



/* btSingleResTest -- tests with a single reset range
 *
 * Test finding single ranges of various sizes
 */

static void btSingleResTest(BT btlo, BT bthi, Count btSize,
                            Index base, Index limit,
                            unsigned long length)
{
  unsigned long resLen;
  /* choose varying range lengths from too short to longer than needed */
  for (resLen = length - 1; resLen <= length + 1; resLen++) {
    if ((resLen > 0) && (resLen < (limit - base -2))) {
      /* place the ranges both near the beginning & near the end */
      /* of the search space */
      Index resBase, resLimit;
      for (resBase = base; resBase <= base +2; resBase++) {
        btResRangeSymmetric(btlo, bthi, btSize, 0, btSize);
        btSetRangeSymmetric(btlo, bthi, btSize, base, limit);
        btResAndFindTest(btlo, bthi, btSize, base, limit,
                         resBase, resBase + resLen, length);
      }
      for (resLimit = limit; resLimit >= limit -2; resLimit--) {
        btResRangeSymmetric(btlo, bthi, btSize, 0, btSize);
        btSetRangeSymmetric(btlo, bthi, btSize, base, limit);
        btResAndFindTest(btlo, bthi, btSize, base, limit,
                         resLimit - resLen, resLimit, length);
      }
    }
  }
}



/* btDoubleResTest -- Test finding double ranges of various sizes
 *
 * Set up 2 ranges with various relative positions. The first
 * range is always too small.
 */


/* Constants describing the type of arrangement of the 2 ranges */
enum {
  ArrangeGAP1 = 0,
  ArrangeGAP2 = 1,
  ArrangeSPREAD = 2,
  ArrangeMAX
};

typedef unsigned Arrangement;

/* Choose a limit for reset range 1 */
static Index btArrangeRes1(Arrangement arrange,
                           Index base, Index res2Base,
                           unsigned long length)
{
  switch (arrange) {

  case ArrangeGAP1: {
      /* Gap between ranges is of length 1 */
      return res2Base - 1;
    }

  case ArrangeGAP2: {
      /* Gap between ranges is of length 2 */
      return res2Base - 2;
    }

  case ArrangeSPREAD: {
      /* range 1 starts as far before range 2 as possible */
      return base + length;
    }

  default:
    NOTREACHED;
    return 0; /* keep the compiler happy */
  }
}

/* Constants describing the type of pattern for the first range */
enum {
  PatternLEN1 = 0,
  PatternSETMID = 1,
  PatternJUSTSMALL = 2,
  PatternMAX
};

typedef unsigned Pattern;

/* Choose a limit for reset range 1 */
static void btResetFirstRange(BT btlo, BT bthi, Count btSize,
                              Index res1Limit,
                              unsigned long length,
                              Pattern pattern)
{
  switch (pattern) {

  case PatternLEN1: {
      /* First range is a single reset bit */
      btResSymmetric(btlo, bthi, btSize, res1Limit-1);
      return;
    }

  case PatternSETMID: {
      /* Actually make 2 ranges here by setting a bit in the middle */
      Index mid = res1Limit - length + (length / 2);
      btResRangeSymmetric(btlo, bthi, btSize, res1Limit-length, res1Limit);
      btSetSymmetric(btlo, bthi, btSize, mid);
      return;
    }

  case PatternJUSTSMALL: {
      /* Range of (length - 1) */
      btResRangeSymmetric(btlo, bthi, btSize,
                          1 + res1Limit - length, res1Limit);
      return;
    }

  default:
    NOTREACHED;
  }
}


static void btDoubleResTest(BT btlo, BT bthi, Count btSize,
                            Index base, Index limit,
                            unsigned long length)
{
  unsigned long res2Len;

  if (length < 2)
    return; /* no possibility of making the first range too small */

  /* choose varying range lengths for second res range */
  for (res2Len = length - 1; res2Len <= length + 1; res2Len++) {
    if ((res2Len > 0) && (res2Len < (limit - base -2))) {
      Index res2Limit;
      /* place the second ranges near the end of the search space */
      for (res2Limit = limit; res2Limit >= limit-8; res2Limit--) {
        Index res2Base = res2Limit - res2Len;
        Arrangement arrange;
        /* Pick one of a number of possible arrangements of the ranges */
        for (arrange = ArrangeGAP1; arrange < ArrangeMAX; arrange++) {
          Index res1Limit = btArrangeRes1(arrange, base, res2Base, length);
          Pattern pat;
          /* Pick one of a number of pattern types for range 1 */
          for (pat = PatternLEN1; pat < PatternMAX; pat++) {
            btResRangeSymmetric(btlo, bthi, btSize, 0, btSize);
            btSetRangeSymmetric(btlo, bthi, btSize, base, limit);
            btResetFirstRange(btlo, bthi, btSize, res1Limit, length, pat);
            /* Set up range 2 and expect to find it when searching */
            btResAndFindTest(btlo, bthi, btSize, base, limit,
                             res2Base, res2Limit, length);
          }
        }
      }
    }
  }
}


/* btFindRangeTests -- Test BTFind*ResRange*
 *
 * Run a variety of FindResRange tests with different table patterns.
 */

static void btFindRangeTests(BT btlo, BT bthi, Count btSize,
                             Index base, Index limit,
                             unsigned long length)
{
  btAllResTest(btlo, bthi, btSize, base, limit, length);
  btNoResTest(btlo, bthi, btSize, base, limit, length);
  btSingleResTest(btlo, bthi, btSize, base, limit, length);
  btDoubleResTest(btlo, bthi, btSize, base, limit, length);
}



/* btIsRangeTests -- Test BTIsResRange & BTIsSetRange
 *
 * Test ranges which are all reset or set apart from single
 * bits near to the base and limit (both inside and outside
 * the range).
 *
 * Test BTRangesSame by using the same bit patterns and comparing
 * with an appropriate all-set or all-reset table.
 *
 * These tests also test BTCopyInvertRange
 */

static void btIsRangeTests(BT bt1, BT bt2, Count btSize,
                           Index base, Index limit)
{
  Index minBase, maxLimit, b, l;

  if (base > 0) {
    minBase = base - 1;
  } else {
    minBase = 0;
  }

  if (limit < btSize) {
    maxLimit = limit + 1;
  } else {
    maxLimit = btSize;
  }

  for (b = minBase; b <= base+1; b++) {
    for (l = maxLimit; l >= limit-1; l--) {
      /* test a table which is all reset apart from a set bit */
      /* near each of the base and limit of the range in question */
      Bool outside; /* true if set bits are both outside test range */

      outside = (b < base) && (l > limit);
      BTResRange(bt1, 0, btSize);
      BTSet(bt1, b);
      BTSet(bt1, l - 1);

      /* invert the table for the inverse test */
      BTCopyInvertRange(bt1, bt2, 0, btSize);

      /* Check it with BTIsResRange, and the inverse with BTIsSetRange */
      die_bool(BTIsResRange(bt1, base, limit), outside, "BTISResRange");
      die_bool(BTIsSetRange(bt2, base, limit), outside, "BTISSetRange");

      /* Check the same range with BTRangesSame on an empty table */
      BTResRange(bt2, 0, btSize);
      die_bool(BTRangesSame(bt1, bt2, base, limit), outside, "BTRangeSame");

      /* Check the inverse with BTRangesSame on a full table */
      BTCopyInvertRange(bt1, bt2, 0, btSize);
      BTSetRange(bt1, 0, btSize);
      die_bool(BTRangesSame(bt1, bt2, base, limit), outside, "BTRangeSame");
    }
  }
}


/* btCopyTests -- Test BTCopyRange & BTCopyOffsetRange
 *
 * Test copying ranges which are all reset or set apart from
 * single bits near to the base and limit (both inside and outside
 * the range).
 *
 */

static void btCopyTests(BT bt1, BT bt2, Count btSize,
                        Index base, Index limit)
{
  Index minBase, maxLimit, b, l;

  if (base > 0) {
    minBase = base - 1;
  } else {
    minBase = 0;
  }

  if (limit < btSize) {
    maxLimit = limit + 1;
  } else {
    maxLimit = btSize;
  }

  for (b = minBase; b <= base+1; b++) {
    for (l = maxLimit; l >= limit-1; l--) {
      /* initialize a table which is all reset apart from a set bit */
      /* near each of the base and limit of the range in question */
      Bool outside; /* true if set bits are both outside test range */

      outside = (b < base) && (l > limit);
      BTResRange(bt1, 0, btSize);
      BTSet(bt1, b);
      BTSet(bt1, l - 1);

      /* check copying the region to the bottom of the other table */
      BTCopyOffsetRange(bt1, bt2, base, limit, 0, limit - base);
      die_bool(BTIsResRange(bt2, 0, limit - base), outside, "BTIsResRange");

      /* check copying the region to the top of the other table */
      BTCopyOffsetRange(bt1, bt2,
                        base, limit, btSize + base - limit, btSize);
      die_bool(BTIsResRange(bt2, btSize + base - limit, btSize), outside,
               "BTIsResRange");

      /* check copying the region to the same place in the other table */
      BTCopyOffsetRange(bt1, bt2, base, limit, base, limit);
      die_bool(BTIsResRange(bt2, base, limit), outside, "BTIsResRange");

      /* copy the range and check its the same */
      BTCopyRange(bt1, bt2, base, limit);
      die_bool(BTRangesSame(bt1, bt2, base, limit), TRUE, "BTRangeSame");

      /* invert the table, then copy it and check it again */
      BTCopyInvertRange(bt2, bt1, 0, btSize);
      BTCopyRange(bt1, bt2, base, limit);
      die_bool(BTRangesSame(bt1, bt2, base, limit), TRUE, "BTRangeSame");
    }
  }
}



/* btTests --  Do all the tests
 */

static void btTests(BT btlo, BT bthi, Count btSize)
{
  Index base, limit;

  /* Perform lots of tests over different subranges */
  for (base = 0; base < MPS_WORD_WIDTH; base++) {
    for (limit = btSize; limit > (btSize-MPS_WORD_WIDTH); limit--) {
      /* Perform Is*Range tests over those subranges */
      btIsRangeTests(btlo, bthi, btSize, base, limit);

      /* Perform Copy*Range tests over those subranges */
      btCopyTests(btlo, bthi, btSize, base, limit);

      /* Perform FindResRange tests with different lengths */
      btFindRangeTests(btlo, bthi, btSize, base, limit, 1);
      btFindRangeTests(btlo, bthi, btSize, base, limit, 2);
      btFindRangeTests(btlo, bthi, btSize, base, limit, MPS_WORD_WIDTH - 1);
      btFindRangeTests(btlo, bthi, btSize, base, limit, MPS_WORD_WIDTH);
      btFindRangeTests(btlo, bthi, btSize, base, limit, MPS_WORD_WIDTH + 1);
      btFindRangeTests(btlo, bthi, btSize, base, limit, limit - base -1);
      btFindRangeTests(btlo, bthi, btSize, base, limit, limit - base);
    }
  }
}


/* Start the world */
int main(int argc, char *argv[])
{
  mps_arena_t mpsArena;
  Arena arena; /* the arena which we use to allocate the BT */
  BT btlo, bthi;
  Count btSize;

  /* tests need 4 whole words plus a few extra bits */
  btSize = MPS_WORD_WIDTH * 4 + 10;

  testlib_unused(argc);
  testlib_unused(argv);

  die((mps_res_t)mps_arena_create(&mpsArena, mps_arena_class_vm()),
      "Failed to create arena");
  arena = (Arena)mpsArena; /* avoid pun */

  die((mps_res_t)BTCreate(&btlo, arena, btSize),
      "failed to create low bit table");

  die((mps_res_t)BTCreate(&bthi, arena, btSize),
      "failed to create high bit table");

  btTests(btlo, bthi, btSize);

  printf("\nNo problems detected.\n");
  return 0;
}

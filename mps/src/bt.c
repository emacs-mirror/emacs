/* impl.c.bt: BIT TABLES
 *
 * $HopeName: MMsrc!bt.c(trunk.7) $
 * Copyright (C) 1997 Harlequin Group, all rights reserved
 *
 * READERSHIP
 *
 * .readership: Any MPS developer
 *
 * DESIGN
 *
 * .design: see design.mps.bt
 *
 * PURPOSE
 *
 * .purpose: see design.mps.bt
 */


#include "mpm.h"

SRCID(bt, "$HopeName: MMsrc!bt.c(trunk.7) $");


/* BTCheck -- check the validity of a bit table
 *
 * There's not much that can be checked at present.  This is
 * discussed in review.impl.c.bt.4.
 */

static Bool BTCheck(BT bt)
{
  AVER(bt != NULL);
  AVER(AddrIsAligned((Addr)bt, sizeof(Word)));
  return TRUE;
}


/* design.mps.bt.fun.size */
Size (BTSize)(unsigned long n)
{
  /* check that the expression used in rounding up doesn't overflow */
  AVER(n+MPS_WORD_WIDTH-1 > n);

  return BTSize(n);
}
  

/* design.mps.bt.fun.get */
int (BTGet)(BT t, Index i)
{
  AVER(BTCheck(t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  return BTGet(t, i);
}
  

/* design.mps.bt.fun.set */
void (BTSet)(BT t, Index i)
{
  AVER(BTCheck(t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  BTSet(t, i);
}


/* design.mps.bt.fun.res */
void (BTRes)(BT t, Index i)
{
  AVER(BTCheck(t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  BTRes(t, i);
}


/* design.mps.bt.fun.set-range */
void BTSetRange(BT t, Index i, Index j)
{
  AVER(BTCheck(t));
  AVER(i < j);

  while(i < j) {
    BTSet(t, i);
    ++i;
  }
}


/* BTIsResRange -- test whether a range of bits is all reset
 *
 * See design.mps.bt.fun.is-reset-range.
 */

Bool BTIsResRange(BT bt, Index base, Index limit)
{
  Index i;

  AVER(BTCheck(bt));
  AVER(base < limit);
  /* Can't check range of base or limit */

  i = base;
  while(i < limit) {
    if(BTGet(bt, i))
      return FALSE;
    ++i;
  }
  AVER(i == limit);

  return TRUE;
}


/* BTIsSetRange -- test whether a range of bits is all set
 *
 * See design.mps.bt.fun.is-set-range.
 */

Bool BTIsSetRange(BT bt, Index base, Index limit)
{
  Index i;

  AVER(BTCheck(bt));
  AVER(base < limit);
  /* Can't check range of base or limit */

  i = base;
  while(i < limit) {
    if(!BTGet(bt, i))
      return FALSE;
    ++i;
  }
  AVER(i == limit);

  return TRUE;
}


/* design.mps.bt.fun.res-range */
void BTResRange(BT t, Index i, Index j)
{
  AVER(BTCheck(t));
  AVER(i < j);

  while(i < j) {
    BTRes(t, i);
    ++i;
  }
}


/* BTFindResRange -- find a reset range of bits in a bit table
 *
 * See design.mps.bt.fun.find-res-range.
 */

static Bool BTFindResRange(Index *baseReturn, Index *limitReturn,
                           BT bt,
                           Index searchBase, Index searchLimit,
                           unsigned long minLength,
                           unsigned long maxLength)
{
  unsigned long base;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(bt != NULL);
  AVER(searchBase < searchLimit);
  AVER(minLength <= maxLength);
  AVER(minLength <= searchLimit - searchBase);
  AVER(minLength > 0);
  AVER(maxLength <= searchLimit - searchBase);
  AVER(maxLength > 0);

  /* design.mps.bt.fun.find-res.outer-loop */
  base = searchBase;
  while(base < searchLimit) {
    if(!BTGet(bt, base)) {
      /* design.mps.bt.fun.find-res.enter */
      /* base now marks the beginning of a run */
      unsigned long limit = base;
      unsigned long stopLimit = base + maxLength;
      if(stopLimit > searchLimit)
        stopLimit = searchLimit;
      do {
	/* design.mps.bt.fun.find-res.inner-loop */
        ++limit;
      } while(limit < stopLimit && !BTGet(bt, limit));
      if(limit - base >= minLength) {
	/* design.mps.bt.fun.find-res.success */
        /* found sufficiently long run */
        *baseReturn = base;
        *limitReturn = limit;
        return TRUE;
      }
      /* design.mps.bt.fun.find-res.continue */
      /* wasn't long enough */
      base = limit;
    } else {             /* necessary, consider j == s */
      /* design.mps.bt.fun.find-res.outer-loop */
      ++base;
    }
  }
  AVER(base == searchLimit);

  return FALSE;
}


/* BTFindLongResRange -- find long range of reset bits in a bit table
 *
 * See design.mps.bt.fun.find-long-res-range.
 */

Bool BTFindLongResRange(Index *baseReturn, Index *limitReturn,
                        BT bt,
                        Index searchBase, Index searchLimit,
                        unsigned long length)
{
  /* All parameters are checked by BTFindResRange. */
  return BTFindResRange(baseReturn, limitReturn,
                        bt,
                        searchBase, searchLimit,
                        length, searchLimit - searchBase);
}


/* BTFindShortResRange -- find short range of reset bits in a bit table
 *
 * See design.mps.bt.fun.find-short-res-range.
 */

Bool BTFindShortResRange(Index *baseReturn, Index *limitReturn,
                         BT bt,
                         Index searchBase, Index searchLimit,
                         unsigned long length)
{
  /* All parameters are checked by BTFindResRange. */
  return BTFindResRange(baseReturn, limitReturn,
                        bt,
                        searchBase, searchLimit,
                        length, length);
}

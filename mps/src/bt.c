/* impl.c.bt: BIT TABLES
 *
 * $HopeName: MMsrc!bt.c(trunk.5) $
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

SRCID(bt, "$HopeName: MMsrc!bt.c(trunk.5) $");


/* design.mps.bt.fun.size */
Size BTSize(unsigned long n)
{
  /* check that the expression used in rounding up doesn't overflow */
  AVER(n+MPS_WORD_WIDTH-1 > n);

  return (n+MPS_WORD_WIDTH-1)/MPS_WORD_WIDTH*sizeof(Word);
}
  

/* design.mps.bt.fun.get */
int (BTGet)(BT t, Index i)
{
  AVER(t != NULL);
  AVER(AddrIsAligned((Addr)t, sizeof *t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  return BTGet(t, i);
}
  

/* design.mps.bt.fun.set */
void (BTSet)(BT t, Index i)
{
  AVER(t != NULL);
  AVER(AddrIsAligned((Addr)t, sizeof *t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  BTSet(t, i);
}


/* design.mps.bt.fun.res */
void (BTRes)(BT t, Index i)
{
  AVER(t != NULL);
  AVER(AddrIsAligned((Addr)t, sizeof *t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  BTRes(t, i);
}


/* design.mps.bt.fun.set-range */
void BTSetRange(BT t, Index i, Index j)
{
  AVER(t != NULL);
  AVER(AddrIsAligned((Addr)t, sizeof *t));
  AVER(i < j);

  while(i < j) {
    BTSet(t, i);
    ++i;
  }
}


/* design.mps.bt.fun.res-range */
void BTResRange(BT t, Index i, Index j)
{
  AVER(t != NULL);
  AVER(AddrIsAligned((Addr)t, sizeof *t));
  AVER(i < j);

  while(i < j) {
    BTRes(t, i);
    ++i;
  }
}

/* design.mps.bt.fun.find-res-range */
Bool BTFindResRange(Index *baseReturn, Index *limitReturn,
                    BT bt,
                    Index searchBase, Index searchLimit,
                    unsigned long length)
{
  unsigned long base;

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(bt != NULL);
  AVER(searchBase < searchLimit);
  AVER(length <= searchLimit - searchBase);
  AVER(length > 0);

  /* design.mps.bt.fun.find-res.outer-loop */
  base = searchBase;
  while(base < searchLimit) {
    if(!BTGet(bt, base)) {
      /* design.mps.bt.fun.find-res.enter */
      /* base now marks the beginning of a run */
      unsigned long limit = base;
      do {
	/* design.mps.bt.fun.find-res.inner-loop */
        ++limit;
      } while(limit < searchLimit && !BTGet(bt, limit));
      if(limit - base >= length) {
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

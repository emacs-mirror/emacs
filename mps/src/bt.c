/* impl.c.bt: BIT TABLES
 *
 * $HopeName: MMsrc!bt.c(trunk.4) $
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

SRCID(bt, "$HopeName: MMsrc!bt.c(trunk.4) $");


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
Bool BTFindResRange(Index *baseIndexReturn, Index *limitIndexReturn,
                    BT table, unsigned long tableSize,
		    unsigned long runLength)
{
  Index i = 0;

  AVER(baseIndexReturn != NULL);
  AVER(limitIndexReturn != NULL);
  AVER(table != NULL);
  AVER(AddrIsAligned((Addr)table, sizeof *table));
  AVER(runLength <= tableSize);
  AVER(runLength > 0);

  /* design.mps.bt.fun.find-res.outer-loop */
  while(i < tableSize) {
    if(!BTGet(table, i)) {
      /* design.mps.bt.fun.find-res.enter */
      /* i now marks the beginning of a run */
      Index j = i;

      do {
	/* design.mps.bt.fun.find-res.inner-loop */
        ++j;
      } while(j < tableSize && !BTGet(table, j));
      /* j now marks the end of a run */
      if(j - i >= runLength) {
	/* design.mps.bt.fun.find-res.success */
        /* found sufficiently long run */
        *limitIndexReturn = j;
        *baseIndexReturn = i;
        return TRUE;
      }
      /* design.mps.bt.fun.find-res.continue */
      /* wasn't long enough */
      AVER(i < j);
      i = j;
    } else {
      /* design.mps.bt.fun.find-res.outer-loop */
      ++i;
    }
  }
  AVER(i == tableSize);

  return FALSE;
}

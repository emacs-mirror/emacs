/* impl.c.bt: BIT TABLES
 *
 * $HopeName: MMsrc!bt.c(trunk.3) $
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
 * The Bit Tables modules is provides centrally functions for
 * manipulating tables of bits.  Such tables are often used to provide some
 * sort of mapping or set functionality (for example, a table of mark bits
 * in a mark and sweep pool may use bit tables).
 */


#include "mpm.h"

SRCID(bt, "$HopeName: MMsrc!bt.c(trunk.3) $");


Size BTSize(unsigned long n)
{
  AVER(n+MPS_WORD_WIDTH-1 > n);

  return (n+MPS_WORD_WIDTH-1)/MPS_WORD_WIDTH*sizeof(Word);
}
  
int (BTGet)(BT t, Index i)
{
  AVER(t != NULL);
  /* Can't check i */

  return BTGet(t, i);
}
  
void (BTSet)(BT t, Index i)
{
  AVER(t != NULL);
  /* Can't check i */

  BTSet(t, i);
}

void (BTRes)(BT t, Index i)
{
  AVER(t != NULL);
  /* Can't check i */

  BTRes(t, i);
}

void BTSetRange(BT t, Index i, Index j)
{
  AVER(t != NULL);
  AVER(i < j);

  for( ; i < j; ++i) {
    BTSet(t, i);
  }
}

void BTResRange(BT t, Index i, Index j)
{
  AVER(t != NULL);
  AVER(i < j);

  for( ; i < j; ++i) {
    BTRes(t, i);
  }
}

Bool BTFindResRange(Index *iReturn, Index *jReturn,
                    BT t, unsigned long s, unsigned long n)
{
  unsigned long i = 0;

  AVER(iReturn != NULL);
  AVER(jReturn != NULL);
  AVER(t != NULL);
  AVER(n <= s);
  AVER(n > 0);

  /* remember, s is the size of the table */
  while(i < s) {
    if(!BTGet(t, i)) {
      unsigned long j = i;
      do {
        ++j;
      } while(j < s && !BTGet(t, j));
      if(j - i >= n) {
        /* found sufficiently long run */
        *jReturn = j;
        *iReturn = i;
        return TRUE;
      }
      i = j;
      continue;         /* necessary, consider j == s */
    }
    /* wasn't long enough */
    ++i;
  }
  AVER(i == s);

  return FALSE;
}


/* impl.c.bt: BIT TABLES
 *
 * $HopeName: MMsrc!bt.c(trunk.16) $
 * Copyright (C) 1998.  Harlequin Group plc.  All rights reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer
 *
 * DESIGN
 *
 * .design: see design.mps.bt
 */

#include "mpm.h"


SRCID(bt, "$HopeName: MMsrc!bt.c(trunk.16) $");


/* is the whole word of bits at this index set? */

#define BTIsWordSet(bt,i) ((bt)[(i)>>MPS_WORD_SHIFT] == ~(Word)0)

/* align bit-table indices up and down */

#define BTIndexAlignUp(index) ((Index)SizeAlignUp((index), MPS_WORD_WIDTH))
#define BTIndexAlignDown(index) ((Index)SizeAlignDown((index), MPS_WORD_WIDTH))


/* AMSBTCreate -- allocate a BT from the control pool
 * 
 * See design.mps.bt.if.create
 */

Res BTCreate(BT *btReturn, Arena arena, Count length)
{
  Res res;
  BT bt;
  void *p;

  AVER(btReturn != NULL);
  AVERT(Arena, arena);
  AVER(length > 0);

  res = ArenaAlloc(&p, arena, BTSize(length));
  if(res != ResOK)
    return res;
  bt = (BT)p;

  *btReturn = bt;
  return ResOK;
}

/* BTDestroy -- free a BT to the control pool.
 * 
 * See design.mps.bt.if.destroy
 */

void BTDestroy(BT bt, Arena arena, Count length)
{
  AVER(bt != NULL);
  AVERT(Arena, arena);
  AVER(length > 0);
  
  ArenaFree(arena, bt, BTSize(length));
}

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
size_t (BTSize)(unsigned long n)
{
  /* check that the expression used in rounding up doesn't overflow */
  AVER(n+MPS_WORD_WIDTH-1 > n);

  return BTSize(n);
}
  

/* design.mps.bt.fun.get */
Bool (BTGet)(BT t, Index i)
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
void BTResRange(BT t, Index base, Index limit)
{
  Index bitIndex, innerBase, innerLimit;

  AVER(BTCheck(t));
  AVER(base < limit);

  /* We determine the maximal inner range that has word-aligned */
  /* base and limit.  We then reset the lead and trailing bits as */
  /* bits, and the rest as words. */
     
  innerBase = BTIndexAlignUp(base);
  innerLimit = BTIndexAlignDown(limit);

  if(innerBase >= innerLimit) { /* no inner range */
    for(bitIndex = base; bitIndex < limit; ++bitIndex)
      BTRes(t, bitIndex);
  } else {
    Index wordIndex, wordBase, wordLimit;

    wordBase = innerBase >> MPS_WORD_SHIFT;
    wordLimit = innerLimit >> MPS_WORD_SHIFT;

    for(bitIndex = base; bitIndex < innerBase; ++bitIndex) 
      BTRes(t, bitIndex);

    for(wordIndex = wordBase; wordIndex < wordLimit; ++wordIndex)
      t[wordIndex] = (Word)0;

    for(bitIndex = innerLimit; bitIndex < limit; ++bitIndex)
      BTRes(t, bitIndex);
  }
}


/* BTFindResRange -- find a reset range of bits in a bit table,
 * starting at the low end of the search range.
 *
 * See design.mps.bt.fun.find-res-range.
 */

static Bool BTFindResRange(Index *baseReturn, Index *limitReturn,
                           BT bt,
                           Index searchBase, Index searchLimit,
                           unsigned long minLength, unsigned long maxLength)
{
  Index base;   /* base of each candidate range */
  Index limit;  /* limit of each candidate range */
  Index i;      /* current index to check */
  unsigned long length; /* length of a successful candidate */

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(bt != NULL);
  AVER(searchBase < searchLimit);
  AVER(minLength > 0);
  AVER(minLength <= maxLength);
  AVER(maxLength <= searchLimit - searchBase);

  base = searchBase;
  while (base <= searchLimit - minLength) {
    limit = base + minLength;
    i = limit - 1;                      /* top index in candidate range */
    if (BTIsWordSet(bt,i)) {            /* skip to the next word */
      base = BTIndexAlignUp(limit);
      if (base < limit) /* overflow case */
	return FALSE;
    } else {                           /* check the candidate range */
      while (!BTGet(bt, i)) {
        if (i == base) {               /* candidate range succeeds */
          length = minLength;
	  /* try to extend to maxLength */
	  while ((length < maxLength) &&
		 (limit < searchLimit) &&
                 !BTGet(bt,limit)) {
	    ++ length;
	    ++ limit;
	  }
          *baseReturn = base;
	  *limitReturn = limit;
          return TRUE;
        }
        -- i;
      }
      base = i + 1;                 /* Skip to reset or unknown bit */
    }
  }
  /* failure */
  return FALSE;
}


/* BTFindResRangeHigh -- find a reset range of bits in a bit table,
 * starting at the high end of the search range.
 *
 * See design.mps.bt.fun.find-res-range.
 */

static Bool BTFindResRangeHigh(Index *baseReturn, Index *limitReturn,
                               BT bt,
                               Index searchBase, Index searchLimit,
                               unsigned long minLength,
			       unsigned long maxLength)
{
  Index base;   /* base of each candidate range */
  Index limit;  /* limit of each candidate range */
  Index i;      /* current index to check */
  unsigned long length; /* length of a successful candidate */

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVER(bt != NULL);
  AVER(searchBase < searchLimit);
  AVER(minLength > 0);
  AVER(minLength <= maxLength);
  AVER(maxLength <= searchLimit - searchBase);

  limit = searchLimit;
  while (limit >= searchBase + minLength) {
    base = limit - minLength;
    i = base;                           /* bottom index in candidate range */
    if (BTIsWordSet(bt,i)) {            /* skip to the next word */
      limit = BTIndexAlignDown(base);
    } else {                            /* check the candidate range */
      while (!BTGet(bt, i)) {
	++ i;
        if (i == limit) {               /* candidate range succeeds */
          length = minLength;
	  /* try to extend to maxLength */
	  while ((length < maxLength) &&
		 (base > searchBase) &&
                 !BTGet(bt, base - 1)) {
	    ++ length;
	    -- base;
	  }
          *baseReturn = base;
	  *limitReturn = limit;
          return TRUE;
        }
      }
      limit = i;                        /* skip to reset or unknown bit */
    }
  }
  /* failure */
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


/* BTFindLongResRangeHigh -- find long range of reset bits in a bit table
 *
 * See design.mps.bt.fun.find-long-res-range-high.
 */

Bool BTFindLongResRangeHigh(Index *baseReturn, Index *limitReturn,
                            BT bt,
                            Index searchBase, Index searchLimit,
                          unsigned long length)
{
  /* All parameters are checked by BTFindResRangeHigh. */
  return BTFindResRangeHigh(baseReturn, limitReturn,
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

/* BTFindShortResRangeHigh -- find short range of reset bits in a bit table,
 * starting to look from the top of the search range.
 *
 * See design.mps.bt.fun.find-short-res-range-high.
 */

Bool BTFindShortResRangeHigh(Index *baseReturn, Index *limitReturn,
			     BT bt,
			     Index searchBase, Index searchLimit,
			     unsigned long length)
{
  /* All parameters are checked by BTFindResRangeHigh. */
  return BTFindResRangeHigh(baseReturn, limitReturn,
			    bt,
			    searchBase, searchLimit,
			    length, length);
}

/* BTRangesSame -- check that a range of bits in two BTs are the same.
 * 
 * See design.mps.bt.if.ranges-same
 */
 
Bool BTRangesSame(BT comparand, BT comparator, Index base, Index limit)
{
  Index i;

  AVER(BTCheck(comparand));
  AVER(BTCheck(comparator));
  AVER(base < limit);
  i = base;
  while(i < limit) {
    if(BTGet(comparand, i) != BTGet(comparator, i))
      return FALSE;
    ++ i;
  }
  return TRUE;
}

/* BTCopyInvertRange -- copy a range of bits from one BT to another,
 * inverting them as you go.
 * 
 * See design.mps.bt.if.copy-invert-range
 */

void BTCopyInvertRange(BT fromBT, BT toBT, Index base, Index limit)
{
  Index bitIndex, innerBase, innerLimit;

  AVER(BTCheck(fromBT));
  AVER(BTCheck(toBT));
  AVER(fromBT != toBT);
  AVER(base < limit);

  /* We determine the maximal inner range that has word-aligned */
  /* base and limit.  We then copy the lead and trailing bits as */
  /* bits, and the rest as words. */

  innerBase = BTIndexAlignUp(base);
  innerLimit = BTIndexAlignDown(limit); 

  if(innerBase >= innerLimit) { /* no inner range */
    for(bitIndex = base; bitIndex < limit; ++bitIndex)
     if(BTGet(fromBT, bitIndex))
       BTRes(toBT, bitIndex);
     else
       BTSet(toBT, bitIndex);
  } else {
    Index wordIndex, wordBase, wordLimit;
  
    wordBase = innerBase >> MPS_WORD_SHIFT;
    wordLimit = innerLimit >> MPS_WORD_SHIFT;

    for(bitIndex = base; bitIndex < innerBase; ++bitIndex) {
      if(BTGet(fromBT, bitIndex))
        BTRes(toBT, bitIndex);
      else
        BTSet(toBT, bitIndex);
    }

    for(wordIndex = wordBase; wordIndex < wordLimit; ++wordIndex)
      toBT[wordIndex] = ~fromBT[wordIndex];
      
    for(bitIndex = innerLimit; bitIndex < limit; ++bitIndex) {
      if(BTGet(fromBT, bitIndex))
        BTRes(toBT, bitIndex);
      else
        BTSet(toBT, bitIndex);
    }
  }
}


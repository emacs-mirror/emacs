/* impl.c.bt: BIT TABLES
 *
 * $HopeName: MMsrc!bt.c(trunk.18) $
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


SRCID(bt, "$HopeName: MMsrc!bt.c(trunk.18) $");


/* is the whole word of bits at this index set? */

#define BTIsWordSet(bt,i) ((bt)[(i)>>MPS_WORD_SHIFT] == ~(Word)0)

/* align bit-table indices up and down */

#define BTIndexAlignUp(index) ((Index)SizeAlignUp((index), MPS_WORD_WIDTH))
#define BTIndexAlignDown(index) ((Index)SizeAlignDown((index), MPS_WORD_WIDTH))

/* return a word mask of bits set only from base and above */
#define BTMaskLow(base) (~(Word)0 << (base))

/* return a word mask of bits set only below limit */
#define BTMaskHigh(limit) (~(Word)0 >> (MPS_WORD_WIDTH - (limit)))

/* return a word mask of bits set only in requested range */
#define BTMask(base,limit) (BTMaskHigh((limit)) & BTMaskLow((base)))

/*  ((~(Word)0 >> (MPS_WORD_WIDTH - ((limit) - (base)))) << (base)) */

/* return word and bit indexes from index */

#define BTWordIndex(index) ((index) >> MPS_WORD_SHIFT)
#define BTBitIndex(index) ((index) & (MPS_WORD_WIDTH - 1))


/* Predicate to determine whether a range is sufficiently small
 * that it's not worth trying to separate words and odd bits.
 * The choice of what counts as "sufficiently small" is made
 * for efficiency reasons. Empirical evidence indicates that
 * a good choice is ranges of size 6 or less.
 */

#define BTIsSmallRange(base,limit) ((base) + 6 >= (limit))


/* ACT_ON_RANGE -- macro to act on a base-limit range
 * Three actions should be provided:
 *   - single_action(btIndex) - operates on a single bit
 *   - bits_action(wordIndex, base, limit) -- operates on part-words
 *   - word_action(wordIndex) -- Operates on full words in range
 * WORD_ACTIONs should not use break or continue.
 *
 * If the range is small enough it will be processed a single
 * bit at a time. Larger ranges are processed as words where
 * possible, and part-words for boundary bits.
 */

#define ACT_ON_RANGE(base,limit,single_action, \
                     bits_action,word_action) \
  BEGIN \
    Index actInnerBase = BTIndexAlignUp((base)); \
\
  if (BTIsSmallRange(base, limit)) { \
    /* Small ranges are processed most efficiently bit-by-bit */ \
    Index actBit; \
    for(actBit = base; actBit < limit; ++actBit) { \
      single_action(actBit); \
    } \
  } else if (actInnerBase > limit) { /* no inner range */ \
      AVER(base < limit); /* caught by small range case */ \
      bits_action(BTWordIndex((base)), \
                  BTBitIndex((base)), \
                  BTBitIndex((limit))); \
    } else { \
      Index actInnerLimit = BTIndexAlignDown((limit)); \
      Index actWordIndex, actWordBase, actWordLimit; \
\
      actWordBase = BTWordIndex(actInnerBase); \
      actWordLimit = BTWordIndex(actInnerLimit); \
\
      if(base < actInnerBase) { \
        bits_action(actWordBase-1, \
                    BTBitIndex((base)), \
                    MPS_WORD_WIDTH); \
      } \
\
      for(actWordIndex = actWordBase; actWordIndex < actWordLimit; \
          ++actWordIndex) { \
        word_action(actWordIndex); \
      } \
\
      if(limit > actInnerLimit) { \
        bits_action(actWordLimit, 0, BTBitIndex((limit))); \
      } \
    } \
  END

/* ACT_ON_RANGE_HIGH -- macro to act on a base-limit range 
 * in reverse order. Usage as for ACT_ON_RANGE
 */
#define ACT_ON_RANGE_HIGH(base,limit,single_action, \
                          bits_action,word_action) \
  BEGIN \
    Index actInnerBase = BTIndexAlignUp((base)); \
\
  if (BTIsSmallRange(base, limit)) { \
    /* Small ranges are processed most efficiently bit-by-bit */ \
    Index actBit; \
    for(actBit = limit; actBit > base; --actBit) { \
      single_action(actBit - 1); \
    } \
  } else if(actInnerBase > limit) { /* no inner range */ \
      AVER(base < limit); /* caught by small range case */ \
      bits_action(BTWordIndex((base)), \
                  BTBitIndex((base)), \
                  BTBitIndex((limit))); \
    } else { \
      Index actInnerLimit = BTIndexAlignDown((limit)); \
      Index actWordIndex, actWordBase, actWordLimit; \
\
      actWordBase = BTWordIndex(actInnerBase); \
      actWordLimit = BTWordIndex(actInnerLimit); \
\
      if(limit > actInnerLimit) { \
        bits_action(actWordLimit, 0, BTBitIndex((limit))); \
      } \
\
      for(actWordIndex = actWordLimit; actWordIndex > actWordBase; \
          --actWordIndex) { \
        word_action(actWordIndex-1); \
      } \
\
      if(base < actInnerBase) { \
        bits_action(actWordBase-1, \
		    BTBitIndex((base)), \
                    MPS_WORD_WIDTH); \
      } \
\
    } \
  END



/* BTCreate -- allocate a BT from the control pool
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
void BTSetRange(BT t, Index base, Index limit)
{
  AVER(BTCheck(t));
  AVER(base < limit);

#define SINGLE_SET_RANGE(i) \
  BTSet(t, (i))
#define BITS_SET_RANGE(i,base,limit) \
  t[(i)] |= BTMask((base),(limit))
#define WORD_SET_RANGE(i) \
  t[(i)] = ~(Word)(0)

  ACT_ON_RANGE(base, limit, SINGLE_SET_RANGE, 
               BITS_SET_RANGE, WORD_SET_RANGE);
}


/* BTIsResRange -- test whether a range of bits is all reset
 *
 * See design.mps.bt.fun.is-reset-range.
 */
Bool BTIsResRange(BT bt, Index base, Index limit)
{
  AVER(BTCheck(bt));
  AVER(base < limit);
  /* Can't check range of base or limit */

#define SINGLE_IS_RES_RANGE(i) \
  if (BTGet(bt, (i))) return FALSE
#define BITS_IS_RES_RANGE(i,base,limit) \
  if((bt[(i)] & BTMask((base),(limit))) != (Word)0) return FALSE
#define WORD_IS_RES_RANGE(i) \
  if(bt[(i)] != (Word)0) return FALSE
 
  ACT_ON_RANGE(base, limit, SINGLE_IS_RES_RANGE, 
               BITS_IS_RES_RANGE, WORD_IS_RES_RANGE);
  return TRUE;
}


/* BTIsSetRange -- test whether a range of bits is all set
 *
 * See design.mps.bt.fun.is-set-range.
 */
Bool BTIsSetRange(BT bt, Index base, Index limit)
{
  AVER(BTCheck(bt));
  AVER(base < limit);
  /* Can't check range of base or limit */

#define SINGLE_IS_SET_RANGE(i) \
  if (!BTGet(bt, (i))) return FALSE
#define BITS_IS_SET_RANGE(i,base,limit) \
  BEGIN \
    Word bactMask = BTMask((base),(limit)); \
    if((bt[(i)] & bactMask) != bactMask) \
      return FALSE; \
  END
#define WORD_IS_SET_RANGE(i) \
  if(bt[(i)] != ~(Word)0) return FALSE

  ACT_ON_RANGE(base, limit, SINGLE_IS_SET_RANGE, 
               BITS_IS_SET_RANGE, WORD_IS_SET_RANGE);
  return TRUE;
}


/* design.mps.bt.fun.res-range */
void BTResRange(BT t, Index base, Index limit)
{
  AVER(BTCheck(t));
  AVER(base < limit);

#define SINGLE_RES_RANGE(i) \
  BTRes(t, (i))
#define BITS_RES_RANGE(i,base,limit) \
  t[(i)] &= ~(BTMask((base),(limit)))
#define WORD_RES_RANGE(i) t[(i)] = (Word)(0)

  ACT_ON_RANGE(base, limit, SINGLE_RES_RANGE, 
               BITS_RES_RANGE, WORD_RES_RANGE);
}


/* BTFindSet -- find the lowest set bit in a range in a bit table.
 * Sets foundReturn to false if the range is entirely reset;
 * in this case indexReturn is unset. Sets foundReturn to true
 * otherwise.
 *
 * Implemented as a macro for efficiency reasons.
 * The macro internally uses the label btFindSetLabel.
 * If the macro must be used more than once within a function
 * this label must be redefined to avoid a nameclash. E.g.
 *    #define btFindSetLabel uniqueLabel
 *    BTFindSet(...)
 *    #undef btFindSetLabel
 */

#define BTFindSet(foundReturn,indexReturn,bt,base,limit)\
  BEGIN \
    Bool *bfsFoundReturn = (foundReturn); \
    Index *bfsIndexReturn = (indexReturn); \
    BT bfsBt = (bt); \
    ACT_ON_RANGE((base), (limit), SINGLE_FIND_SET,  \
                 BITS_FIND_SET, WORD_FIND_SET); \
    *bfsFoundReturn = FALSE; \
btFindSetLabel:; \
  END

#define SINGLE_FIND_SET(i)  \
  if (BTGet(bfsBt, (i))) { \
    *bfsIndexReturn = (i); \
    *bfsFoundReturn = TRUE; \
    goto btFindSetLabel; \
  } 
#define BITS_FIND_SET(wi,base,limit)  \
  BEGIN \
    Index bactWi = (wi); \
    ACTION_FIND_SET(bactWi, bfsBt[bactWi], (base), (limit)); \
  END
#define WORD_FIND_SET(wi) \
  BEGIN \
    Index wactWi = (wi); \
    ACTION_FIND_SET(wactWi, bfsBt[wactWi], 0, MPS_WORD_WIDTH); \
  END
#define ACTION_FIND_SET(wi,word,base,limit) \
  ACTION_FIND_SET_BIT((wi),(word),(base),(limit),btFindSetLabel)

/* Helper macro to find the low bit in a range of a word.
 * Works by first shifting the base of the range to the low
 * bits of the word. Then loops performing a binary chop
 * over the data looking to see if a bit is set in the lower
 * half. If not, it must be in the upper half which is then 
 * shifted down. The loop completes after using a chop unit 
 * of a single single bit.
 */
#define ACTION_FIND_SET_BIT(wi,word,base,limit,label) \
  BEGIN \
    /* no need to mask the low bits which are shifted */ \
    Index actionIndex = (base); \
    Word actionWord = ((word) & BTMaskHigh((limit))) >> actionIndex; \
    Count actionMaskWidth = (MPS_WORD_WIDTH >> 1); \
    Word actionMask = ~(Word)0 >> (MPS_WORD_WIDTH-actionMaskWidth); \
    if (actionWord != (Word)0) { \
      while(actionMaskWidth != (Count)0) { \
        if((actionWord & actionMask) == (Word)0) { \
          actionIndex += actionMaskWidth; \
          actionWord >>= actionMaskWidth; \
        } \
        actionMaskWidth >>= 1; \
        actionMask >>= actionMaskWidth; \
      } \
      *bfsIndexReturn = ((wi) << MPS_WORD_SHIFT) | actionIndex; \
      *bfsFoundReturn = TRUE; \
      goto label; \
    } \
  END


/* BTFindRes -- find the lowest reset bit in a range in a bit table.
 * Usage as for BTFindSet
 *
 * Internally uses the label btFindResLabel
 * which must be redefined to avoid a nameclash if the macro is 
 * used twice in a function scope.
 */

#define BTFindRes(foundReturn,indexReturn,bt,base,limit)\
  BEGIN \
    Bool *bfsFoundReturn = (foundReturn); \
    Index *bfsIndexReturn = (indexReturn); \
    BT bfsBt = (bt); \
    ACT_ON_RANGE((base), (limit), SINGLE_FIND_RES,  \
                 BITS_FIND_RES, WORD_FIND_RES); \
    *bfsFoundReturn = FALSE; \
btFindResLabel:; \
  END

#define SINGLE_FIND_RES(i)  \
  if (!BTGet(bfsBt, (i))) { \
    *bfsIndexReturn = (i); \
    *bfsFoundReturn = TRUE; \
    goto btFindResLabel; \
  } 
#define BITS_FIND_RES(wi,base,limit)  \
  BEGIN \
    Index bactWi = (wi); \
    ACTION_FIND_RES(bactWi,bfsBt[bactWi], (base), (limit)); \
  END
#define WORD_FIND_RES(wi) \
  BEGIN \
    Index wactWi = (wi); \
    ACTION_FIND_RES(wactWi, bfsBt[wactWi], 0, MPS_WORD_WIDTH); \
  END
#define ACTION_FIND_RES(wi,word,base,limit) \
  ACTION_FIND_SET_BIT((wi),~(word),(base),(limit),btFindResLabel)


/* BTFindSetHigh -- find the highest set bit in a range in a bit table.
 * Usage as for BTFindSet
 *
 * Internally uses the label btFindSetHighLabel
 * which must be redefined to avoid a nameclash if the macro is 
 * used twice in a function scope.
 */

#define BTFindSetHigh(foundReturn,indexReturn,bt,base,limit)\
  BEGIN \
    Bool *bfsFoundReturn = (foundReturn); \
    Index *bfsIndexReturn = (indexReturn); \
    BT bfsBt = (bt); \
    ACT_ON_RANGE_HIGH((base), (limit), SINGLE_FIND_SET_HIGH, \
                      BITS_FIND_SET_HIGH, WORD_FIND_SET_HIGH); \
    *bfsFoundReturn = FALSE; \
btFindSetHighLabel:; \
  END

#define SINGLE_FIND_SET_HIGH(i)  \
  if (BTGet(bfsBt, (i))) { \
    *bfsIndexReturn = (i); \
    *bfsFoundReturn = TRUE; \
    goto btFindSetHighLabel; \
  } 
#define BITS_FIND_SET_HIGH(wi,base,limit) \
  BEGIN \
    Index bactWi = (wi); \
    ACTION_FIND_SET_HIGH(bactWi, bfsBt[bactWi], (base), (limit)); \
  END
#define WORD_FIND_SET_HIGH(wi) \
  BEGIN \
    Index wactWi = (wi); \
    ACTION_FIND_SET_HIGH(wactWi, (bfsBt[wactWi]), 0, MPS_WORD_WIDTH); \
  END
#define ACTION_FIND_SET_HIGH(wi,word,base,limit) \
  ACTION_FIND_SET_BIT_HIGH((wi),(word),(base),(limit),btFindSetHighLabel)

/* Helper macro to find the high bit in a range of a word.
 * Essentially a mirror image of ACTION_FIND_SET
 */
#define ACTION_FIND_SET_BIT_HIGH(wi,word,base,limit,label) \
  BEGIN \
    /* no need to mask the high bits which are shifted */ \
    Index actionShift = MPS_WORD_WIDTH - (limit);  \
    Index actionIndex = MPS_WORD_WIDTH - 1 - actionShift; \
    Word actionWord = ((word) & BTMaskLow((base))) << actionShift; \
    Count actionMaskWidth = (MPS_WORD_WIDTH >> 1); \
    Word actionMask = ~(Word)0 << (MPS_WORD_WIDTH-actionMaskWidth); \
    if (actionWord != (Word)0) { \
      while(actionMaskWidth != (Count)0) { \
        if((actionWord & actionMask) == (Word)0) { \
          actionIndex -= actionMaskWidth; \
          actionWord <<= actionMaskWidth; \
        } \
        actionMaskWidth >>= 1; \
        actionMask <<= actionMaskWidth; \
      } \
      *bfsIndexReturn = ((wi) << MPS_WORD_SHIFT) | actionIndex; \
      *bfsFoundReturn = TRUE; \
      goto label; \
    } \
  END
  

/* BTFindResHigh -- find the highest reset bit in a bit table range 
 * Usage as for BTFindSet
 *
 * Internally uses the label btFindSetHighLabel
 * which must be redefined to avoid a nameclash if the macro is 
 * used twice in a function scope.
 */

#define BTFindResHigh(foundReturn,indexReturn,bt,base,limit)\
  BEGIN \
    Bool *bfsFoundReturn = (foundReturn); \
    Index *bfsIndexReturn = (indexReturn); \
    BT bfsBt = (bt); \
    ACT_ON_RANGE_HIGH((base), (limit), SINGLE_FIND_RES_HIGH, \
                      BITS_FIND_RES_HIGH, WORD_FIND_RES_HIGH); \
    *bfsFoundReturn = FALSE; \
btFindResHighLabel:; \
  END

#define SINGLE_FIND_RES_HIGH(i)  \
  if (!BTGet(bfsBt, (i))) { \
    *bfsIndexReturn = (i); \
    *bfsFoundReturn = TRUE; \
    goto btFindResHighLabel; \
  } 
#define BITS_FIND_RES_HIGH(wi,base,limit) \
  BEGIN \
    Index bactWi = (wi); \
    ACTION_FIND_RES_HIGH(bactWi, bfsBt[bactWi], (base), (limit)); \
  END
#define WORD_FIND_RES_HIGH(wi) \
  BEGIN \
    Index wactWi = (wi); \
    ACTION_FIND_RES_HIGH(wactWi, (bfsBt[wactWi]), 0, MPS_WORD_WIDTH); \
  END
#define ACTION_FIND_RES_HIGH(wi,word,base,limit) \
  ACTION_FIND_SET_BIT_HIGH((wi),~(word),(base),(limit),btFindResHighLabel)


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
  Index resBase;   /* base of each candidate range when looking for start */
  Index resLimit;  /* limit of each candidate range when looking for start */
  Index resIndex;  /* index of first reset bit found */

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(BT, bt);
  AVER(searchBase < searchLimit);
  AVER(minLength > 0);
  AVER(minLength <= maxLength);
  AVER(maxLength <= searchLimit - searchBase);

  resBase = searchBase;
  resLimit = searchLimit - minLength;
  while (resBase < resLimit) {
    Bool foundRes = FALSE;
    /* Look for the start of a range */
    BTFindRes(&foundRes, &resIndex, bt, resBase, resLimit);
    if (foundRes) {
      Index minLimit = resIndex + minLength;
      Index setIndex;  /* index of first set bit found */
      Index setBase;   /* base of search for set bit */
      Index setLimit;  /* limit search for set bit */
      Bool foundSet = FALSE;

      /* Look to see if there's a minimum range */
      BTFindSetHigh(&foundSet, &setIndex, bt, resIndex+1, minLimit);
      if (!foundSet) {
        /* found minimum range. extend it. */
        foundSet = FALSE;
        setBase = minLimit;
        setLimit = resIndex + maxLength;
        if (setLimit > searchLimit)
          setLimit = searchLimit;
        if (setLimit > setBase)
          BTFindSet(&foundSet, &setIndex, bt, setBase, setLimit);
        if (!foundSet)
          setIndex = setLimit;
        
        AVER(setIndex - resIndex >= minLength);
        AVER(setIndex - resIndex <= maxLength);
        *baseReturn = resIndex;
        *limitReturn = setIndex;
        return TRUE;

      } else {
        /* range was too small. Try again */
        resBase = setIndex + 1;
      }

    } else {
      /* failure */
      return FALSE;
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
  Index resBase;   /* base of each candidate range when looking for start */
  Index resLimit;  /* limit of each candidate range when looking for start */
  Index resIndex;  /* index of first reset bit found */

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(BT, bt);
  AVER(searchBase < searchLimit);
  AVER(minLength > 0);
  AVER(minLength <= maxLength);
  AVER(maxLength <= searchLimit - searchBase);

  resBase = searchBase + minLength -1;
  resLimit = searchLimit;
  while (resLimit > resBase) {
    Bool foundRes = FALSE;
    /* Look for the start of a range */
    BTFindResHigh(&foundRes, &resIndex, bt, resBase, resLimit);
    if (foundRes) { 
      Index minBase = resIndex - minLength + 1;
      Index setIndex;  /* index of first set bit found */
      Index setBase;   /* base of search for set bit */
      Index setLimit;  /* limit search for set bit */
      Index baseIndex; /* base of range found */
      Index limitIndex = resIndex + 1; /* limit of range found */
      Bool foundSet = FALSE;

      /* Look to see if there's a minimum range */
      BTFindSet(&foundSet, &setIndex, bt, minBase, resIndex);
      if (!foundSet) {
        /* found minimum range. extend it. */
        foundSet = FALSE;
        setLimit = minBase;
        if ((searchBase + maxLength) > limitIndex)
          setBase = searchBase;
        else
          setBase  = limitIndex - maxLength;
        if (setLimit > setBase)
          BTFindSetHigh(&foundSet, &setIndex, bt, setBase, setLimit);
        if (foundSet)
          baseIndex = setIndex+1;
        else
          baseIndex = setBase;
        
        AVER(limitIndex - baseIndex >= minLength);
        AVER(limitIndex - baseIndex <= maxLength);
        *baseReturn = baseIndex;
        *limitReturn = limitIndex;
        return TRUE;

      } else {
        /* range was too small. Try again */
        resLimit = setIndex;
      }

    } else {
      /* failure */
      return FALSE;
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
  AVER(BTCheck(comparand));
  AVER(BTCheck(comparator));
  AVER(base < limit);

#define SINGLE_RANGES_SAME(i) \
  if(BTGet(comparand, (i)) != BTGet(comparator, (i))) \
      return FALSE
#define BITS_RANGES_SAME(i,base,limit) \
  BEGIN \
    Index bactI = (i); \
    Word bactMask = BTMask((base),(limit)); \
    if((comparand[bactI] & (bactMask)) != \
       (comparator[bactI] & (bactMask))) \
      return FALSE; \
  END
#define WORD_RANGES_SAME(i) \
  BEGIN \
    Index wactI = (i); \
    if((comparand[wactI]) != (comparator[wactI])) \
      return FALSE; \
  END
 
  ACT_ON_RANGE(base, limit, SINGLE_RANGES_SAME, 
               BITS_RANGES_SAME, WORD_RANGES_SAME);
  return TRUE;
}

/* BTCopyInvertRange -- copy a range of bits from one BT to another,
 * inverting them as you go.
 * 
 * See design.mps.bt.if.copy-invert-range
 */

void BTCopyInvertRange(BT fromBT, BT toBT, Index base, Index limit)
{
  AVER(BTCheck(fromBT));
  AVER(BTCheck(toBT));
  AVER(fromBT != toBT);
  AVER(base < limit);

#define SINGLE_COPY_INVERT_RANGE(i) \
  if(BTGet(fromBT, (i))) \
    BTRes(toBT, (i)); \
  else \
    BTSet(toBT, (i)) 
#define BITS_COPY_INVERT_RANGE(i,base,limit) \
  BEGIN \
    Index bactI = (i); \
    Word bactMask = BTMask((base),(limit)); \
    toBT[bactI] = \
      (toBT[bactI] & ~bactMask) | (~fromBT[bactI] & bactMask); \
  END
#define WORD_COPY_INVERT_RANGE(i) \
  BEGIN \
    Index wactI = (i); \
    toBT[wactI] = ~fromBT[wactI]; \
  END
 
  ACT_ON_RANGE(base, limit, SINGLE_COPY_INVERT_RANGE, 
               BITS_COPY_INVERT_RANGE, WORD_COPY_INVERT_RANGE);
}


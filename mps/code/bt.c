/* impl.c.bt: BIT TABLES
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
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


SRCID(bt, "$Id$");


/* BTIndexAlignUp, BTIndexAlignDown -- Align bit-table indices
 *
 * Align bit-table indices up and down to word boundaries
 */

#define BTIndexAlignUp(index) (IndexAlignUp((index), MPS_WORD_WIDTH))
#define BTIndexAlignDown(index) (IndexAlignDown((index), MPS_WORD_WIDTH))


/* BTMask -- generate sub-word masks
 *
 * Create a mask with only specified bits set
 */

/* Return a word mask of bits set only from base and above */
#define BTMaskLow(base) (~(Word)0 << (base))

/* Return a word mask of bits set only below limit */
#define BTMaskHigh(limit) (~(Word)0 >> (MPS_WORD_WIDTH - (limit)))

/* Return a word mask of bits set only in requested range */
#define BTMask(base,limit) (BTMaskHigh((limit)) & BTMaskLow((base)))


/* BTWordIndex, BTBitIndex -- Decode BT indexes
 *
 * Return word and bit indexes from index
 */

#define BTWordIndex(index) ((index) >> MPS_WORD_SHIFT)
#define BTBitIndex(index) ((index) & (MPS_WORD_WIDTH - 1))


/* BTIsSmallRange -- test range size
 *
 * Predicate to determine whether a range is sufficiently small
 * that it's not worth trying to separate words and odd bits.
 * The choice of what counts as "sufficiently small" is made
 * for efficiency reasons. Empirical evidence indicates that
 * a good choice is ranges of size 6 or less.
 */

#define BTIsSmallRange(base,limit) ((base) + 6 >= (limit))


/* ACT_ON_RANGE -- macro to act on a base-limit range
 *
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
    if (BTIsSmallRange((base), (limit))) { \
      /* Small ranges are processed most efficiently bit-by-bit */ \
      Index actBit; \
      for (actBit = (base); actBit < (limit); ++actBit) { \
        single_action(actBit); \
      } \
    } else { \
      Index actInnerBase = BTIndexAlignUp((base)); \
      if (actInnerBase > (limit)) { /* no inner range */ \
        AVER((base) < (limit)); /* caught by small range case */ \
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
        if ((base) < actInnerBase) { \
          bits_action(actWordBase-1, \
                      BTBitIndex((base)), \
                      MPS_WORD_WIDTH); \
        } \
\
        for (actWordIndex = actWordBase; actWordIndex < actWordLimit; \
            ++actWordIndex) { \
          word_action(actWordIndex); \
        } \
\
        if ((limit) > actInnerLimit) { \
          bits_action(actWordLimit, 0, BTBitIndex((limit))); \
        } \
      } \
    } \
  END


/* ACT_ON_RANGE_HIGH -- macro to act on a base-limit range
 *
 * in reverse order. Usage as for ACT_ON_RANGE
 */

#define ACT_ON_RANGE_HIGH(base,limit,single_action, \
                          bits_action,word_action) \
  BEGIN \
    if (BTIsSmallRange((base), (limit))) { \
      /* Small ranges are processed most efficiently bit-by-bit */ \
      Index actBit; \
      for (actBit = (limit); actBit > (base); --actBit) { \
        single_action(actBit - 1); \
      } \
    } else { \
      Index actInnerBase = BTIndexAlignUp((base)); \
      if (actInnerBase > (limit)) { /* no inner range */ \
        AVER((base) < (limit)); /* caught by small range case */ \
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
        if ((limit) > actInnerLimit) { \
          bits_action(actWordLimit, 0, BTBitIndex((limit))); \
        } \
\
        for (actWordIndex = actWordLimit; actWordIndex > actWordBase; \
            --actWordIndex) { \
          word_action(actWordIndex-1); \
        } \
\
        if ((base) < actInnerBase) { \
          bits_action(actWordBase-1, \
                      BTBitIndex((base)), \
                      MPS_WORD_WIDTH); \
        } \
      } \
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

  res = ControlAlloc(&p, arena, BTSize(length),
                     /* withReservoirPermit */ FALSE);
  if (res != ResOK)
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
 
  ControlFree(arena, bt, BTSize(length));
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


/* BTSize -- return the size of a BT
 *
 * See design.mps.bt.fun.size
 */

size_t (BTSize)(unsigned long n)
{
  /* check that the expression used in rounding up doesn't overflow */
  AVER(n+MPS_WORD_WIDTH-1 > n);

  return BTSize(n);
}
 

/* BTGet -- get a bit from a BT
 *
 * See design.mps.bt.fun.get
 */

Bool (BTGet)(BT t, Index i)
{
  AVER(BTCheck(t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  return BTGet(t, i);
}
 

/* BTSet -- set a bit in a BT
 *
 * See design.mps.bt.fun.set
 */

void (BTSet)(BT t, Index i)
{
  AVER(BTCheck(t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  BTSet(t, i);
}


/* BTRes -- reset a bit in a BT
 *
 * design.mps.bt.fun.res
 */

void (BTRes)(BT t, Index i)
{
  AVER(BTCheck(t));
  /* Can't check i */

  /* see macro in impl.h.mpm */
  BTRes(t, i);
}


/* BTSetRange -- set a range of bits in a BT
 *
 * design.mps.bt.fun.set-range
 */

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
  if ((bt[(i)] & BTMask((base),(limit))) != (Word)0) return FALSE
#define WORD_IS_RES_RANGE(i) \
  if (bt[(i)] != (Word)0) return FALSE

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
    if ((bt[(i)] & bactMask) != bactMask) \
      return FALSE; \
  END
#define WORD_IS_SET_RANGE(i) \
  if (bt[(i)] != ~(Word)0) return FALSE

  ACT_ON_RANGE(base, limit, SINGLE_IS_SET_RANGE,
               BITS_IS_SET_RANGE, WORD_IS_SET_RANGE);
  return TRUE;
}


/* BTResRange -- reset a range of bits in a BT
 *
 * design.mps.bt.fun.res-range
 */

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
 *
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


/* ACTION_FIND_SET_BIT -- Find first set bit in a range
 *
 * Helper macro to find the low bit in a range of a word.
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
      while (actionMaskWidth != (Count)0) { \
        if ((actionWord & actionMask) == (Word)0) { \
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
 *
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
 *
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


/* ACTION_FIND_SET_BIT_HIGH -- Find highest set bit in a range
 *
 * Helper macro to find the high bit in a range of a word.
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
      while (actionMaskWidth != (Count)0) { \
        if ((actionWord & actionMask) == (Word)0) { \
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
 

/* BTFindResHigh -- find the highest reset bit in a range
 *
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


/* BTFindResRange -- find a reset range of bits in a bit table
 *
 * Starts searching at the low end of the search range.
 *
 * See design.mps.bt.fun.find-res-range.
 */

static Bool BTFindResRange(Index *baseReturn, Index *limitReturn,
                           BT bt,
                           Index searchBase, Index searchLimit,
                           unsigned long minLength, unsigned long maxLength)
{
  Bool foundRes;         /* true if a reset bit is found */
  Index resBase;         /* base of a candidate reset range */
  Index unseenBase;      /* base of testing so far */
  Index minLimit;        /* limit of minimal acceptable range */
  Index resLimit;        /* limit of search for a candidate range */

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(BT, bt);
  AVER(searchBase < searchLimit);
  AVER(minLength > 0);
  AVER(minLength <= maxLength);
  AVER(maxLength <= searchLimit - searchBase);

  foundRes = FALSE;     /* don't know first reset bit */
  minLimit = 0;         /* avoid spurious compiler warning */
  resBase = searchBase; /* haven't seen anything yet */
  unseenBase = searchBase;  /* haven't seen anything yet */
  resLimit = searchLimit - minLength + 1;

  while (resBase < resLimit) {
    Index setIndex;  /* index of last set bit found */
    Bool foundSet = FALSE; /* true if a set bit is found */

    /* Find the first reset bit if it's not already known */
    if (!foundRes) {
      BTFindRes(&foundRes, &resBase, bt, unseenBase, resLimit);
      if (!foundRes) {
        /* failure */
        return FALSE;
      }
      unseenBase = resBase + 1;
      minLimit = resBase + minLength;
    }

    /* Look to see if there is any set bit in the minimum range */
    BTFindSetHigh(&foundSet, &setIndex, bt, unseenBase, minLimit);
    if (!foundSet) {
      /* Found minimum range. Extend it. */
      Index setBase;   /* base of search for set bit */
      Index setLimit;  /* limit search for set bit */
      foundSet = FALSE;
      setBase = minLimit;
      setLimit = resBase + maxLength;
      if (setLimit > searchLimit)
        setLimit = searchLimit;
      if (setLimit > setBase)
        BTFindSet(&foundSet, &setIndex, bt, setBase, setLimit);
      if (!foundSet)
        setIndex = setLimit;
       
      AVER(setIndex - resBase >= minLength);
      AVER(setIndex - resBase <= maxLength);
      *baseReturn = resBase;
      *limitReturn = setIndex;
      return TRUE;
       
    } else {
      /* Range was too small. Try again */
      unseenBase = minLimit;
      resBase = setIndex + 1;
      if (resBase != minLimit) {
        /* Already found the start of next candidate range */
        minLimit = resBase + minLength;
      } else {
        foundRes = FALSE;
      }
    }
  }

  /* failure */
  return FALSE;
}


/* BTFindResRangeHigh -- find a reset range of bits in a bit table
 *
 * Starts searching at the high end of the search range.
 *
 * See design.mps.bt.fun.find-res-range.
 */

static Bool BTFindResRangeHigh(Index *baseReturn, Index *limitReturn,
                               BT bt,
                               Index searchBase, Index searchLimit,
                               unsigned long minLength,
			       unsigned long maxLength)
{
  Bool foundRes;         /* true if a reset bit is found */
  Index resLimit;        /* limit of a candidate reset range */
  Index resIndex;        /* index of highest reset bit found */
  Index unseenLimit;     /* limit of testing so far */
  Index minBase;         /* base of minimal acceptable range */
  Index resBase;         /* base of search for a candidate range */

  AVER(baseReturn != NULL);
  AVER(limitReturn != NULL);
  AVERT(BT, bt);
  AVER(searchBase < searchLimit);
  AVER(minLength > 0);
  AVER(minLength <= maxLength);
  AVER(maxLength <= searchLimit - searchBase);

  foundRes = FALSE;     /* don't know first reset bit */
  minBase = 0;          /* avoid spurious compiler warning */
  resLimit = searchLimit;    /* haven't seen anything yet */
  unseenLimit = searchLimit; /* haven't seen anything yet */
  resBase = searchBase + minLength -1;

  while (resLimit > resBase) {
    Index setIndex;  /* index of first set bit found */
    Bool foundSet = FALSE; /* true if a set bit is found */

    /* Find the first reset bit if it's not already known */
    if (!foundRes) {
      /* Look for the limit of a range */
      BTFindResHigh(&foundRes, &resIndex, bt, resBase, unseenLimit);
      if (!foundRes) {
        /* failure */
        return FALSE;
      }
      resLimit = resIndex + 1;
      unseenLimit = resIndex;
      minBase = resLimit - minLength;
    }

    /* Look to see if there is any set bit in the minimum range */
    BTFindSet(&foundSet, &setIndex, bt, minBase, unseenLimit);
    if (!foundSet) {
      /* Found minimum range. Extend it. */
      Index setBase;   /* base of search for set bit */
      Index setLimit;  /* limit search for set bit */
      Index baseIndex; /* base of reset range found */
      foundSet = FALSE;
      setLimit = minBase;
      if ((searchBase + maxLength) > resLimit)
        setBase = searchBase;
      else
        setBase  = resLimit - maxLength;
      if (setLimit > setBase)
        BTFindSetHigh(&foundSet, &setIndex, bt, setBase, setLimit);
      if (foundSet)
        baseIndex = setIndex+1;
      else
        baseIndex = setBase;
     
      AVER(resLimit - baseIndex >= minLength);
      AVER(resLimit - baseIndex <= maxLength);
      *baseReturn = baseIndex;
      *limitReturn = resLimit;
      return TRUE;
     
    } else {
      /* Range was too small. Try again */
      unseenLimit = minBase;
      resLimit = setIndex;
      if (resLimit != minBase) {
        /* Already found the start of next candidate range */
        minBase = resLimit - minLength;
      } else {
        foundRes = FALSE;
      }
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

/* BTFindShortResRangeHigh -- find short range of reset bits in a bit table
 *
 * Starts looking from the top of the search range.
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
  if (BTGet(comparand, (i)) != BTGet(comparator, (i))) \
      return FALSE
#define BITS_RANGES_SAME(i,base,limit) \
  BEGIN \
    Index bactI = (i); \
    Word bactMask = BTMask((base),(limit)); \
    if ((comparand[bactI] & (bactMask)) != \
       (comparator[bactI] & (bactMask))) \
      return FALSE; \
  END
#define WORD_RANGES_SAME(i) \
  BEGIN \
    Index wactI = (i); \
    if ((comparand[wactI]) != (comparator[wactI])) \
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
  if (BTGet(fromBT, (i))) \
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


/* BTCopyRange -- copy a range of bits from one BT to another
 *
 * See design.mps.bt.if.copy-range
 */

void BTCopyRange(BT fromBT, BT toBT, Index base, Index limit)
{
  AVER(BTCheck(fromBT));
  AVER(BTCheck(toBT));
  AVER(fromBT != toBT);
  AVER(base < limit);

#define SINGLE_COPY_RANGE(i) \
  if (BTGet(fromBT, (i))) \
    BTSet(toBT, (i)); \
  else \
    BTRes(toBT, (i))
#define BITS_COPY_RANGE(i,base,limit) \
  BEGIN \
    Index bactI = (i); \
    Word bactMask = BTMask((base),(limit)); \
    toBT[bactI] = \
      (toBT[bactI] & ~bactMask) | (fromBT[bactI] & bactMask); \
  END
#define WORD_COPY_RANGE(i) \
  BEGIN \
    Index wactI = (i); \
    toBT[wactI] = fromBT[wactI]; \
  END

  ACT_ON_RANGE(base, limit, SINGLE_COPY_RANGE,
               BITS_COPY_RANGE, WORD_COPY_RANGE);
}


/* BTCopyOffsetRange -- copy a range of bits from one BT to an
 * offset range in another BT
 *
 * .slow: Can't always use ACT_ON_RANGE because word alignment
 * may differ for each range. We could try to be smart about
 * detecting similar alignment - but we don't.
 *
 * See design.mps.bt.if.copy-offset-range
 */

void BTCopyOffsetRange(BT fromBT, BT toBT,
                       Index fromBase, Index fromLimit,
                       Index toBase, Index toLimit)
{
  Index fromBit, toBit;

  AVER(BTCheck(fromBT));
  AVER(BTCheck(toBT));
  AVER(fromBT != toBT);
  AVER(fromBase < fromLimit);
  AVER(toBase < toLimit);
  AVER((fromLimit - fromBase) == (toLimit - toBase));

  for (fromBit = fromBase, toBit = toBase;
       fromBit < fromLimit;
       ++fromBit, ++toBit) {
    if (BTGet(fromBT, fromBit))
      BTSet(toBT, toBit);
    else
      BTRes(toBT, toBit);
  }
}



/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

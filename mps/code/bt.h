/* impl.h.bt: Bit Table Interface
 *
 * $Id: bt.h,v 1.2 2002/02/01 13:52:04 pekka Exp $
 * $HopeName: MMsrc!bt.h(trunk.2) $
 * Copyright (C) 2002 Global Graphics Software.
 *
 * .source: design.mps.bt.  */

#ifndef bt_h
#define bt_h

#include "mpmtypes.h"


/* design.mps.bt.if.size */
extern size_t (BTSize)(unsigned long length);
#define BTSize(n) (((n) + MPS_WORD_WIDTH-1) / MPS_WORD_WIDTH * sizeof(Word))


/* design.mps.bt.if.get */
extern Bool (BTGet)(BT bt, Index index);
#define BTGet(a, i) \
  ((Bool)(((a)[((i) >> MPS_WORD_SHIFT)] \
           >> ((i) & ~((Word)-1 << MPS_WORD_SHIFT))) \
          & (Word)1))

/* design.mps.bt.if.set */
extern void (BTSet)(BT bt, Index index);
#define BTSet(a, i) \
  BEGIN \
    (a)[((i)>>MPS_WORD_SHIFT)] |= (Word)1<<((i)&~((Word)-1<<MPS_WORD_SHIFT)); \
  END

/* design.mps.bt.if.res */
extern void (BTRes)(BT bt, Index index);
#define BTRes(a, i) \
  BEGIN \
    (a)[((i)>>MPS_WORD_SHIFT)] &= \
      ~((Word)1 << ((i) & ~((Word)-1<<MPS_WORD_SHIFT))); \
  END


extern Res BTCreate(BT *btReturn, Arena arena, Count length);
extern void BTDestroy(BT bt, Arena arena, Count length);

extern void BTSetRange(BT bt, Index base, Index limit);
extern Bool BTIsSetRange(BT bt, Index base, Index limit);
extern void BTResRange(BT bt, Index base, Index limit);
extern Bool BTIsResRange(BT bt, Index base, Index limit);

extern Bool BTFindShortResRange(Index *baseReturn, Index *limitReturn,
                                BT bt, Index searchBase, Index searchLimit,
                                unsigned long length);
extern Bool BTFindShortResRangeHigh(Index *baseReturn, Index *limitReturn,
                                    BT bt, Index searchBase, Index searchLimit,
                                    unsigned long length);
extern Bool BTFindLongResRange(Index *baseReturn, Index *limitReturn,
                               BT bt, Index searchBase, Index searchLimit,
                               unsigned long length);
extern Bool BTFindLongResRangeHigh(Index *baseReturn, Index *limitReturn,
                                   BT bt, Index searchBase, Index searchLimit,
                                   unsigned long length);

extern Bool BTRangesSame(BT BTx, BT BTy, Index base, Index limit);

extern void BTCopyInvertRange(BT fromBT, BT toBT, Index base, Index limit);
extern void BTCopyRange(BT fromBT, BT toBT, Index base, Index limit);
extern void BTCopyOffsetRange(BT fromBT, BT toBT,
                              Index fromBase, Index fromLimit,
                              Index toBase, Index toLimit);

extern Count BTCountResRange(BT bt, Index base, Index limit);


#endif /* bt_h */

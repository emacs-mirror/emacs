/* bt.h: Bit Table Interface
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .source: design.mps.bt.  */

#ifndef bt_h
#define bt_h

#include "mpmtypes.h"


/* design.mps.bt.if.size */
extern size_t (BTSize)(unsigned long length);
#define BTSize(n) (((n) + MPS_WORD_WIDTH-1) / MPS_WORD_WIDTH * sizeof(Word))


/* <design/bt/#if.get> */
extern Bool (BTGet)(BT bt, Index index);
#define BTGet(a, i) \
  ((Bool)(((a)[((i) >> MPS_WORD_SHIFT)] \
           >> ((i) & ~((Word)-1 << MPS_WORD_SHIFT))) \
          & (Word)1))

/* <design/bt/#if.set> */
extern void (BTSet)(BT bt, Index index);
#define BTSet(a, i) \
  BEGIN \
    (a)[((i)>>MPS_WORD_SHIFT)] |= (Word)1<<((i)&~((Word)-1<<MPS_WORD_SHIFT)); \
  END

/* <design/bt/#if.res> */
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

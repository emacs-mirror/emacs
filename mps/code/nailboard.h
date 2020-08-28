/* nailboard.h: NAILBOARD INTERFACE
 *
 * $Id$
 * Copyright (c) 2014-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/nailboard>.
 */

#ifndef nailboard_h
#define nailboard_h

#include "mpmtypes.h"
#include "range.h"

typedef struct NailboardStruct *Nailboard;

/* NOTE: we could reduce the size of this structure using bitfields.
 * levels can be at most MPS_WORD_WIDTH / LEVEL_SHIFT + 1, which is 11
 * on 64-bit, so it would fit in 4 bits. (Or it could be recalculated
 * from range each time it's needed.) alignShift is at most
 * MPS_WORD_SHIFT so would fit in 3 bits. (Or it could be supplied in
 * each function call by the owner.) newNails would fit in 1 bit.
 */
typedef struct NailboardStruct {
  Sig sig;
  RangeStruct range;   /* range of addresses covered by nailboard */
  Count levels;        /* number of levels */
  Shift alignShift;    /* shift due to address alignment */
  Bool newNails;       /* set to TRUE if a new nail is set */
  BT level[1];         /* bit tables for each level */
} NailboardStruct;

#define NailboardSig ((Sig)0x5194A17B) /* SIGnature NAILBoard */

#define NailboardClearNewNails(board) ((board)->newNails = FALSE)
#define NailboardNewNails(board) RVALUE((board)->newNails)

extern Bool NailboardCheck(Nailboard board);
extern Res NailboardCreate(Nailboard *boardReturn, Arena arena, Align alignment, Addr base, Addr limit);
extern void NailboardDestroy(Nailboard board, Arena arena);
extern void (NailboardClearNewNails)(Nailboard board);
extern Bool (NailboardNewNails)(Nailboard board);
extern Bool NailboardGet(Nailboard board, Addr addr);
extern Bool NailboardSet(Nailboard board, Addr addr);
extern void NailboardSetRange(Nailboard board, Addr base, Addr limit);
extern Bool NailboardIsSetRange(Nailboard board, Addr base, Addr limit);
extern Bool NailboardIsResRange(Nailboard board, Addr base, Addr limit);
extern Res NailboardDescribe(Nailboard board, mps_lib_FILE *stream, Count depth);

#endif /* nailboard.h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

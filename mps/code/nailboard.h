/* nailboard.h: NAILBOARD INTERFACE
 *
 * $Id$
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/nailboard/>.
 */

#ifndef nailboard_h
#define nailboard_h

#include "mpmtypes.h"
#include "range.h"

typedef struct NailboardStruct *Nailboard;

typedef struct NailboardStruct {
  Sig sig;
  Arena arena;
  RangeStruct range; /* range covered by nailboard */
  Shift markShift;  /* to convert offset into bit index for mark */
  BT mark;          /* mark table used to record ambiguous fixes */
  Count nails;      /* no. of ambigFixes, not necessarily distinct */
  Count distinctNails; /* number of distinct ambigFixes */
  Bool newNails;    /* set to TRUE if a new nail is set */
} NailboardStruct;

#define NailboardSig ((Sig)0x5194A17B) /* SIGnature NAILBoard */

extern Bool NailboardCheck(Nailboard board);
extern Res NailboardCreate(Nailboard *boardReturn, Arena arena, Align alignment, Addr base, Addr limit);
extern void NailboardDestroy(Nailboard board);
extern Align NailboardAlignment(Nailboard board);
extern void NailboardClearNewNails(Nailboard board);
extern Bool NailboardNewNails(Nailboard board);
extern Bool NailboardGet(Nailboard board, Addr addr);
extern Bool NailboardSet(Nailboard board, Addr addr);
extern void NailboardSetRange(Nailboard board, Addr base, Addr limit);
extern Bool NailboardIsSetRange(Nailboard board, Addr base, Addr limit);
extern Bool NailboardIsResRange(Nailboard board, Addr base, Addr limit);

#endif /* nailboard.h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

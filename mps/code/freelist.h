/* freelist.h: FREE LIST ALLOCATOR INTERFACE
 *
 * $Id$
 * Copyright (c) 2013 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/freelist/>.
 */

#ifndef freelist_h
#define freelist_h

#include "mpmtypes.h"
#include "range.h"

#define FreelistSig ((Sig)0x519F6331) /* SIGnature FREEL */

typedef struct FreelistStruct *Freelist;
typedef union FreelistBlockUnion *FreelistBlock;

typedef Bool (*FreelistIterateMethod)(Bool *deleteReturn, Range range,
                                      void *closureP, Size closureS);

typedef struct FreelistStruct {
  Sig sig;
  Align alignment;
  FreelistBlock list;
  Count listSize;
} FreelistStruct;

extern Bool FreelistCheck(Freelist fl);
extern Res FreelistInit(Freelist fl, Align alignment);
extern void FreelistFinish(Freelist fl);

extern Res FreelistInsert(Range rangeReturn, Freelist fl, Range range);
extern Res FreelistDelete(Range rangeReturn, Freelist fl, Range range);
extern Res FreelistDescribe(Freelist fl, mps_lib_FILE *stream);

extern void FreelistIterate(Freelist abq, FreelistIterateMethod iterate,
                            void *closureP, Size closureS);

extern Bool FreelistFindFirst(Range rangeReturn, Range oldRangeReturn,
                              Freelist fl, Size size, FindDelete findDelete);
extern Bool FreelistFindLast(Range rangeReturn, Range oldRangeReturn,
                             Freelist fl, Size size, FindDelete findDelete);
extern Bool FreelistFindLargest(Range rangeReturn, Range oldRangeReturn,
                                Freelist fl, Size size, FindDelete findDelete);

extern void FreelistFlushToLand(Freelist fl, Land land);

#endif /* freelist.h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

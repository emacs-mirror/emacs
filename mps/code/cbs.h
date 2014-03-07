/* cbs.h: CBS -- Coalescing Block Structure
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/cbs/>.
 */

#ifndef cbs_h
#define cbs_h

#include "arg.h"
#include "meter.h"
#include "mpmtypes.h"
#include "range.h"
#include "splay.h"


typedef struct CBSStruct *CBS;
typedef Bool (*CBSVisitor)(CBS cbs, Range range,
                                 void *closureP, Size closureS);


#define CBSSig ((Sig)0x519CB599) /* SIGnature CBS */

typedef struct CBSStruct {
  SplayTreeStruct tree;
  Count treeSize;
  Pool blockPool;
  Align alignment;
  Bool fastFind;
  Bool inCBS; /* prevent reentrance */
  /* meters for sizes of search structures at each op */
  METER_DECL(treeSearch);
  Sig sig; /* sig at end because embeded */
} CBSStruct;

extern Bool CBSCheck(CBS cbs);

extern Res CBSInit(Arena arena, CBS cbs, void *owner,
                   Align alignment, Bool fastFind, ArgList args);
extern void CBSFinish(CBS cbs);

extern Res CBSInsert(Range rangeReturn, CBS cbs, Range range);
extern Res CBSDelete(Range rangeReturn, CBS cbs, Range range);
extern void CBSIterate(CBS cbs, CBSVisitor visitor,
                       void *closureP, Size closureS);

extern Res CBSDescribe(CBS cbs, mps_lib_FILE *stream);

extern Bool CBSFindFirst(Range rangeReturn, Range oldRangeReturn,
                         CBS cbs, Size size, FindDelete findDelete);
extern Bool CBSFindLast(Range rangeReturn, Range oldRangeReturn,
                        CBS cbs, Size size, FindDelete findDelete);
extern Bool CBSFindLargest(Range rangeReturn, Range oldRangeReturn,
                           CBS cbs, Size size, FindDelete findDelete);


#endif /* cbs_h */


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

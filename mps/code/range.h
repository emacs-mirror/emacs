/* range.h: ADDRESS RANGE INTERFACE
 *
 * $Id$
 * Copyright (c) 2013 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Representation of address ranges.
 *
 * .design: <design/range/>
 */

#ifndef range_h
#define range_h

#include "mpmtypes.h"


/* Prototypes */

#define RangeBase(range) ((range)->base)
#define RangeLimit(range) ((range)->limit)
#define RangeSetBase(range, addr) BEGIN ((range)->base = (addr)); END
#define RangeSetLimit(range, addr) BEGIN ((range)->limit = (addr)); END
#define RangeSize(range) (AddrOffset(RangeBase(range), RangeLimit(range)))
#define RangeContains(range, addr) ((range)->base <= (addr) && (addr) < (range)->limit)
#define RangeIsEmpty(range) (RangeSize(range) == 0)

extern void RangeInit(Range range, Addr base, Addr limit);
extern void RangeInitSize(Range range, Addr base, Size size);
extern void RangeFinish(Range range);
extern Res RangeDescribe(Range range, mps_lib_FILE *stream, Count depth);
extern Bool RangeCheck(Range range);
extern Bool RangeIsAligned(Range range, Align align);
extern Bool RangesOverlap(Range range1, Range range2);
extern Bool RangesNest(Range outer, Range inner);
extern Bool RangesEqual(Range range1, Range range2);
extern Addr (RangeBase)(Range range);
extern Addr (RangeLimit)(Range range);
extern void (RangeSetBase)(Range range, Addr addr);
extern void (RangeSetLimit)(Range range, Addr addr);
extern Size (RangeSize)(Range range);
extern void RangeCopy(Range to, Range from);

/* RangeStruct -- address range */

typedef struct RangeStruct {
  Addr base;
  Addr limit;
} RangeStruct;

#endif /* range_h */


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

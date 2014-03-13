/* range.c: ADDRESS RANGE IMPLEMENTATION
 *
 * $Id$
 * Copyright (c) 2013 Ravenbrook Limited.  See end of file for license.
 *
 * .design: <design/range/>
 */

#include "check.h"
#include "mpm.h"
#include "range.h"

SRCID(range, "$Id$");


Bool RangeCheck(Range range)
{
  CHECKS(Range, range);
  CHECKL(range->base != NULL);
  CHECKL(range->base <= range->limit);

  return TRUE;
}

void RangeInit(Range range, Addr base, Addr limit)
{
  AVER(range != NULL);
  AVER(base != NULL);
  AVER(base <= limit);

  range->base = base;
  range->limit = limit;

  range->sig = RangeSig;
  AVERT(Range, range);
}

void RangeFinish(Range range)
{
  AVERT(Range, range);
  range->sig = SigInvalid;

  range->base = range->limit = NULL;
}

Res RangeDescribe(Range range, mps_lib_FILE *stream)
{
  Res res;

  AVERT(Range, range);
  AVER(stream != NULL);

  res = WriteF(stream,
               "Range $P\n{\n", (WriteFP)range,
               "  base: $P\n", (WriteFP)RangeBase(range),
               "  limit: $P\n", (WriteFP)RangeLimit(range),
               "  size: $U\n", (WriteFU)RangeSize(range),
               "}\n", NULL);
  if (res != ResOK) {
    return res;
  }

  return ResOK;
}

Bool RangesOverlap(Range range1, Range range2)
{
  AVERT(Range, range1);
  AVERT(Range, range2);
  return RangeBase(range1) < RangeLimit(range2)
      && RangeBase(range2) < RangeLimit(range1);
}

Bool RangesNest(Range outer, Range inner)
{
  AVERT(Range, outer);
  AVERT(Range, inner);
  return RangeBase(outer) <= RangeBase(inner)
      && RangeLimit(inner) <= RangeLimit(outer);
}

Bool RangesEqual(Range range1, Range range2)
{
  AVERT(Range, range1);
  AVERT(Range, range2);
  return RangeBase(range1) == RangeBase(range2)
      && RangeLimit(range1) == RangeLimit(range2);
}

Bool RangeIsAligned(Range range, Align alignment)
{
  AVERT(Range, range);
  return AddrIsAligned(RangeBase(range), alignment)
      && AddrIsAligned(RangeLimit(range), alignment);
}

Addr (RangeBase)(Range range) {
  AVERT(Range, range);
  return RangeBase(range);
}

Addr (RangeLimit)(Range range) {
  AVERT(Range, range);
  return RangeLimit(range);
}

Size (RangeSize)(Range range) {
  AVERT(Range, range);
  return RangeSize(range);
}

void RangeCopy(Range to, Range from)
{
  AVERT(Range, from);
  RangeInit(to, RangeBase(from), RangeLimit(from));
}


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

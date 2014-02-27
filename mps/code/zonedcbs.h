/* zonedcbs.h: ZONE COALESCING BLOCK STRUCTURE INTERFACE
 *
 * $Id$
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 *
 * A Zone CBS is like a CBS but allows for efficient allocation in zones.
 * Allocation in zones gives control over some parts of an object's address,
 * so that we can later apply fast filters on the critical path.
 * The Zone CBS is mainly used by the arena to allocate blocks to pools.
 */

#ifndef zonedcbs_h
#define zonedcbs_h

#include "mpm.h"
#include "range.h"


typedef struct ZonedCBSStruct *ZonedCBS;

/* ZoneCBSStruct is in mpmst.h because it is inlined in the ArenaStruct. */


/* Basically the same interface as for CBS.  See <code/cbs.h>. */

extern Res ZonedCBSInit(ZonedCBS zcbs, Arena arena, Pool blockPool,
                        Align alignment);
extern void ZonedCBSFinish(ZonedCBS zcbs);
extern Bool ZonedCBSCheck(ZonedCBS zcbs);

extern Res ZonedCBSInsert(Range rangeReturn, ZonedCBS zcbs, Range range);
extern Res ZonedCBSDelete(Range oldRange, ZonedCBS zcbs, Range range);

extern Res ZonedCBSFind(Range rangeReturn,
                        Range oldRangeReturn,
                        ZonedCBS zcbs,
                        ZoneSet zones,
                        Size size,
                        FindDelete findDelete,
                        Bool high);

/*
extern Bool ZonedCBSFindFirst(Range rangeReturn, Range oldRangeReturn,
                              ZonedCBS zcbs, ZoneSet zones,
                              Size size, FindDelete findDelete);
extern Bool ZonedCBSFindLast(Range rangeReturn, Range oldRangeReturn,
                              ZonedCBS zcbs, ZoneSet zones,
                              Size size, FindDelete findDelete);
 */


#endif /* zonedcbs_h */

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

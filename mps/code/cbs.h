/* impl.h.cbs: CBS -- Coalescing Block Structure
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .source: <design/cbs/>.
 */

#ifndef cbs_h
#define cbs_h

#include "meter.h"
#include "splay.h"
#include "mpmtypes.h"


typedef struct CBSStruct *CBS;
typedef struct CBSBlockStruct *CBSBlock;
typedef void (*CBSChangeSizeMethod)(CBS cbs, CBSBlock block,
              Size oldSize, Size newSize);
typedef Bool (*CBSIterateMethod)(CBS cbs, CBSBlock block, void *closureP);


/* See <design/cbs/#impl.low-mem.inline.block> */
typedef void **CBSEmergencyBlock; /* next, limit */

/* See <design/cbs/#impl.low-mem.inline.block> */
typedef void **CBSEmergencyGrain; /* next */


#define CBSSig ((Sig)0x519CB599) /* SIGnature CBS */

typedef struct CBSStruct {
  SplayTreeStruct splayTree;
  Count splayTreeSize;
  Pool blockPool;
  CBSChangeSizeMethod new;
  CBSChangeSizeMethod delete;
  CBSChangeSizeMethod grow;
  CBSChangeSizeMethod shrink;
  Size minSize;
  Align alignment;
  Bool mayUseInline;
  Bool fastFind;
  Bool inCBS; /* prevent reentrance */
  CBSEmergencyBlock emergencyBlockList;
  Count eblSize;
  CBSEmergencyGrain emergencyGrainList;
  Count eglSize;
  /* meters for sizes of search structures at each op */
  METER_DECL(splaySearch);
  METER_DECL(eblSearch);
  METER_DECL(eglSearch);
  Sig sig; /* sig at end because embeded */
} CBSStruct;

typedef struct CBSBlockStruct {
  SplayNodeStruct splayNode;
  Addr base;
  Addr limit;
  Size maxSize; /* accurate maximum block size of sub-tree */
} CBSBlockStruct;


extern Bool CBSCheck(CBS cbs);
extern Bool CBSBlockCheck(CBSBlock block);

extern Res CBSInit(Arena arena, CBS cbs, void *owner,
                   CBSChangeSizeMethod new,
                   CBSChangeSizeMethod delete,
                   CBSChangeSizeMethod grow,
                   CBSChangeSizeMethod shrink,
                   Size minSize,
                   Align alignment,
                   Bool mayUseInline,
                   Bool fastFind);
extern void CBSFinish(CBS cbs);

extern Res CBSInsert(CBS cbs, Addr base, Addr limit);
extern Res CBSInsertReturningRange(Addr *baseReturn, Addr *limitReturn,
                                   CBS cbs, Addr base, Addr limit);
extern Res CBSDelete(CBS cbs, Addr base, Addr limit);
extern void CBSIterate(CBS cbs, CBSIterateMethod iterate, void *closureP);
extern void CBSIterateLarge(CBS cbs, CBSIterateMethod iterate, void *closureP);
extern void CBSSetMinSize(CBS cbs, Size minSize);

extern Res CBSDescribe(CBS cbs, mps_lib_FILE *stream);
extern Res CBSBlockDescribe(CBSBlock block, mps_lib_FILE *stream);

/* CBSBlockBase -- See <design/cbs/#function.cbs.block.base> */
#define CBSBlockBase(block) ((block)->base)
/* CBSBlockLimit -- See <design/cbs/#function.cbs.block.limit> */
#define CBSBlockLimit(block) ((block)->limit)
#define CBSBlockSize(block) AddrOffset((block)->base, (block)->limit)
extern Size (CBSBlockSize)(CBSBlock block);

typedef unsigned CBSFindDelete;
enum {
  CBSFindDeleteNONE = 1,/* don't delete after finding */
  CBSFindDeleteLOW,     /* delete precise size from low end */
  CBSFindDeleteHIGH,    /* delete precise size from high end */
  CBSFindDeleteENTIRE   /* delete entire range */
};

extern Bool CBSFindFirst(Addr *baseReturn, Addr *limitReturn,
                         CBS cbs, Size size, CBSFindDelete findDelete);
extern Bool CBSFindLast(Addr *baseReturn, Addr *limitReturn,
                        CBS cbs, Size size, CBSFindDelete findDelete);

extern Bool CBSFindLargest(Addr *baseReturn, Addr *limitReturn,
                           CBS cbs, CBSFindDelete findDelete);


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

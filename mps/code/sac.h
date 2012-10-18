/* sac.h: SEGREGATED ALLOCATION CACHES INTERFACE
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */

#ifndef sac_h
#define sac_h

#include "mpmtypes.h"
#include "mpm.h" /* for PoolArena */


#define sacClassLIMIT ((Count)8)


/* SAC -- the real segregated allocation caches */

#define SACSig ((Sig)0x5195AC99) /* SIGnature SAC */

typedef struct SACStruct *SAC;

typedef struct SACStruct {
  Sig sig;
  Pool pool;
  Count classesCount;  /* number of classes */
  Index middleIndex;   /* index of the middle */
  mps_sac_s esac_s;     /* variable length, must be last */
} SACStruct;

#define SACOfExternalSAC(esac) PARENT(SACStruct, esac_s, esac)

#define ExternalSACOfSAC(sac) (&((sac)->esac_s))

#define SACArena(sac) PoolArena((sac)->pool)


/* SACClasses -- structure for specifying classes in the cache */
/* .sacc: This structure must match <code/mps.h#sacc>. */

typedef struct mps_sac_classes_s *SACClasses;


extern Res SACCreate(SAC *sac_o, Pool pool, Count classesCount,
                     SACClasses classes);
extern void SACDestroy(SAC sac);
extern Res SACFill(Addr *p_o, SAC sac, Size size, Bool hasReservoirPermit);
extern void SACEmpty(SAC sac, Addr p, Size size);
extern void SACFlush(SAC sac);


#endif /* sac_h */


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

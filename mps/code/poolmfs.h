/*  poolmfs.h: MANUAL FIXED SMALL UNIT POOL
 *
 *  $Id$
 *
 *  Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 *  The MFS pool is used to manage small fixed-size chunks of memory.  It
 *  stores control structures in the memory it manages, rather than to one
 *  side.  It therefore achieves better locality for small objects, but
 *  wastes memory for large objects.  It should not be used unless you are
 *  packing a reasonable number of objects into an arena grain.
 *
 *  Create and Init take the following arguments:
 *
 *    Size extendBy
 *
 *  extendBy is the default number of bytes reserved by the pool at a time.
 *  A large size will make allocation cheaper but have a higher resource
 *  overhead.  A typical value might be 65536.  See note 2.
 *
 *    Size unitSize
 *
 *  unitSize is the size in bytes of the objects you with to allocate.  It
 *  must be larger than the minimum unit size returned by GetInfo, and not
 *  larger than extendBy.
 */

#ifndef poolmfs_h
#define poolmfs_h

#include "mpm.h"
#include "mpscmfs.h"

typedef struct MFSStruct *MFS;

#define MFSPool(mfs) (&(mfs)->poolStruct)

extern PoolClass PoolClassMFS(void);

extern Bool MFSCheck(MFS mfs);

extern const struct mps_key_s _mps_key_MFSExtendSelf;
#define MFSExtendSelf (&_mps_key_MFSExtendSelf)
#define MFSExtendSelf_FIELD b

extern void MFSExtend(Pool pool, Addr base, Size size);

typedef void MFSExtentVisitor(Pool pool, Addr base, Size size,
                              void *closure);
extern void MFSFinishExtents(Pool pool, MFSExtentVisitor visitor,
                             void *closure);

#endif /* poolmfs_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

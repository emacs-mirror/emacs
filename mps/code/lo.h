/*  impl.h.lo
 *
 *                   LEAF OBJECT POOL CLASS
 *
 *  $Id$
 *
 *  Copyright (c) 2001 Ravenbrook Limited.
 *
 *  The Leaf Object PoolClass is an automatically managed (ie garbage
 *  collected) pool for managing "leaf" objects.  Leaf objects are
 *  objects that have no references or no references that need tracing
 *  (ie the objects they refer too are non-moving and are manually
 *  managed).
 *
 *  This Class has the following features:
 *
 *  Approximately 6% (asymptotically) space overhead on managed objects.
 *
 *  Automatically reclaims memory used by objects no longer reachable
 *  from the roots.
 *
 *  Non-moving.  References to objects in this pool will never change
 *  due to "fixing".
 *
 *  Buffers will always "commit".  When allocating using a buffer,
 *  commit will never fail.
 *
 *  The following caveat applies:
 *
 *  Space and time performance will degrade when fragmentation
 *  increases.
 *
 */

#ifndef lo_h
#define lo_h

#include "mpm.h"

typedef struct LOStruct *LO;

extern PoolClass PoolClassLO(void);

#endif /* lo_h */


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

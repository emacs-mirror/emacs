/* lock.h: RECURSIVE LOCKS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 */

#ifndef lock_h
#define lock_h

#include "mpm.h"


#define LockSig         ((Sig)0x51970CC9) /* SIGnature LOCK */


/* LockSize -- Return the size of a LockStruct
 *
 * Supports allocation of locks.
 */

extern size_t LockSize(void);


/* LockInitGlobal -- initialize global locks */

extern void LockInitGlobal(void);


/*  LockInit/Finish
 *
 *  lock points to the allocated lock structure.  A lock has no
 *  owner after initialisation.
 */

extern void LockInit(Lock lock);
extern void LockFinish(Lock lock);


/*  LockClaimRecursive
 *
 *  This is called to increase the number of claims on the lock.
 *  LockClaimRecursive will wait until the lock is not owned by another
 *  thread and return with the lock owned.
 *  This can be called recursively.
 */

extern void LockClaimRecursive(Lock lock);


/*  LockReleaseRecursive
 *
 *  This is called to reduce the number of claims on the lock.
 *  If the number of claims drops to zero, ownership is relinquished.
 *  This must not be called without possession of the lock.
 */

extern void LockReleaseRecursive(Lock lock);


/*  LockClaim
 *
 *  This may only be used when the lock is not already owned by
 *  the calling thread.
 *  When used it behaves like LockClaimRecursive, but must be
 *  matched by a call to LockRelease.
 */

extern void LockClaim(Lock lock);


/*  LockRelease
 *
 *  This must only be used to release a Lock symmetrically
 *  with LockClaim.  It therefore should only be called with
 *  a single claim.
 */

extern void LockRelease(Lock lock);


/*  LockCheck -- Validation */

extern Bool LockCheck(Lock lock);


/* LockIsHeld -- test whether lock is held by any thread */

extern Bool LockIsHeld(Lock lock);


/*  == Global locks == */


/*  LockClaimGlobalRecursive
 *
 *  This is called to increase the number of claims on the recursive
 *  global lock.  LockClaimRecursive will wait until the lock is not
 *  owned by another thread and return with the lock owned.
 *  This can be called recursively.
 */

extern void LockClaimGlobalRecursive(void);


/*  LockReleaseGlobalRecursive
 *
 *  This is called to reduce the number of claims on the recursive
 *  global lock. If the number of claims drops to zero, ownership
 *  is relinquished. This must not be called without possession of
 *  the lock.
 */

extern void LockReleaseGlobalRecursive(void);


/*  LockClaimGlobal
 *
 *  This is called to claim the binary global lock, and may only be
 *  used if that lock is not already owned by the calling thread.
 *  It must be matched by a call to LockReleaseGlobal.
 */

extern void LockClaimGlobal(void);


/*  LockReleaseGlobal
 *
 *  This must only be used to release the binary global lock
 *  symmetrically with LockClaimGlobal.
 *  It therefore should only be called with a single claim.
 */

extern void LockReleaseGlobal(void);


#endif /* lock_h */


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

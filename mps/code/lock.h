/* lock.h: RECURSIVE LOCKS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .description: [@@@@ Should be combined with <design/lock/>]
 *  This defines the type Lock, which supports simple recursive
 *  locking.  Locking ensures that only a single thread may be running
 *  with a lock held.  By claiming a lock in some code, this ensures
 *  that only one thread can be running in that code at a time.  This
 *  in turn can be used to protect different threads from trying to
 *  read or update data structures which are in a transitional state.
 *
 *  At most one thread may own a lock at a time.  A lock is initialised
 *  without an owner.  A lock should not have an owner when it is
 *  finished.  Claiming the lock will wait until the lock is not owned
 *  by another thread and then cause the current thread to become the
 *  owner.  Releasing the the lock will relinquish ownership if the
 *  number of releases matches the number of claims.
 *
 *  To use a lock a structure of type LockStruct must be allocated.
 *  This is defined in <code/lockst.h>.  Sources which allocate such a
 *  structure will need to include "lockst.h".  A lock of type Lock is
 *  a pointer to such an allocated structure.
 *
 *  A lock must be Inited before use and should be Finished after use,
 *  using LockInit and LockFinish.
 *
 *  LockClaimRecursive & LockReleaseRecursive are for claiming and
 *  releasing the lock.  These may be used recursively.
 *
 *  There is a limit on the number of recursive claims which
 *  depends on the implementation.  See issue.lock-claim-limit.
 *
 *  LockClaim and LockRelease are the same as the Recursive versions,
 *  except that LockClaim may only be used by a thread that doesn't
 *  already own the lock, and LockRelease may only be used to release
 *  a lock with one claim.  LockClaim and LockRelease if used, must
 *  be used symmetrically in pairs.
 *
 *  There are two intended uses.  Here is an example:
 *  #include "lock.h"
 *  #include "lockst.h"
 *  static LockStruct lockStruct;
 *  binaryUse()
 *  { ;; lock not owned by this thread.
 *    LockClaim(&lockStruct);
 *    ;; lock owned by this thread.
 *    ;; Cannot call binaryUse() at this point.
 *    ;; only one thread at a time may be at this point.
 *    LockRelease(&lockStruct);
 *    ;; lock not owned by this thread.
 *  }
 *
 *  recursiveUse()
 *  { ;; lock may already be owned by this thread.
 *    LockClaimRecursive(&lockStruct);
 *    ;; lock held by this thread.
 *    ;; only one thread at a time may be at this point.
 *    LockReleaseRecursive(&lockStruct);
 *    ;; lock owned by this thread if it was before.
 *  }
 *  LockInit(&lockStruct) must be called before calling binaryUse()
 *  or recursiveUse().
 *  LockFinish(&lockStruct) should be called when lock is no longer
 *  needed.
 *  recursiveUse() may be called by both functions.
 *  binaryUse() may only be called where lock is known not to be
 *  already owned by this thread.  In particular, it may not be
 *  called by recursiveUse().
 *
 *  LockClaimGlobalRecursive & LockReleaseGlobalRecursive are
 *  similar to LockClaimRecursive & LockReleaseRecursive
 *  except that they lock an implicit global lock. This may be
 *  used for locking access to data structures which are global,
 *  such as class objects.
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


#if defined(LOCK)
/* Nothing to do: functions declared in all lock configurations. */
#elif defined(LOCK_NONE)
#define LockSize() MPS_PF_ALIGN
#define LockInit(lock) UNUSED(lock)
#define LockFinish(lock) UNUSED(lock)
#define LockClaimRecursive(lock) UNUSED(lock)
#define LockReleaseRecursive(lock) UNUSED(lock)
#define LockClaim(lock) UNUSED(lock)
#define LockRelease(lock) UNUSED(lock)
#define LockCheck(lock) ((void)lock, TRUE)
#define LockClaimGlobalRecursive()
#define LockReleaseGlobalRecursive()
#define LockClaimGlobal()
#define LockReleaseGlobal()
#else
#error "No lock configuration."
#endif  /* LOCK */


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

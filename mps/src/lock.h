/* impl.h.lock: RECURSIVE LOCKS
 *
 * $HopeName: MMsrc!lock.h(trunk.5) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * .description: [@@@@ Should be combined with design.mps.lock]
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
 *  This is defined in impl.h.lockst.  Sources which allocate such a
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
 *  LockClaim and LockReleaseMPM are the same as the Recursive versions,
 *  except that LockClaim may only be used by a thread that doesn't
 *  already own the lock, and LockReleaseMPM may only be used to release
 *  a lock with one claim.  LockClaim and LockReleaseMPM if used, must
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
 *    LockReleaseMPM(&lockStruct);
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


#if defined(THREAD_MULTI)


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
 *  matched by a call to LockReleaseMPM.
 */

extern void LockClaim(Lock lock);


/*  LockReleaseMPM
 *
 *  This must only be used to release a Lock symmetrically
 *  with LockClaim.  It therefore should only be called with
 *  a single claim.
 */

extern void LockReleaseMPM(Lock lock);


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


#elif defined(THREAD_SINGLE)


#define LockSize() MPS_PF_ALIGN
#define LockInit(lock) UNUSED(lock)
#define LockFinish(lock) UNUSED(lock)
#define LockClaimRecursive(lock) UNUSED(lock)
#define LockReleaseRecursive(lock) UNUSED(lock)
#define LockClaim(lock) UNUSED(lock)
#define LockReleaseMPM(lock) UNUSED(lock)
#define LockCheck(lock) ((void)lock, TRUE)
#define LockClaimGlobalRecursive()
#define LockReleaseGlobalRecursive()
#define LockClaimGlobal()
#define LockReleaseGlobal()


#else

#error "No threading defined."

#endif


#endif /* lock_h */

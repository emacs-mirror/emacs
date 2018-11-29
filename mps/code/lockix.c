/* lockix.c: RECURSIVE LOCKS FOR POSIX SYSTEMS
 *
 * $Id$
 * Copyright (c) 2001-2018 Ravenbrook Limited.  See end of file for license.
 *
 * .posix: The implementation uses a POSIX interface, and should be reusable
 * for many Unix-like operating systems.
 *
 * .freebsd: This implementation supports FreeBSD (platform
 * MPS_OS_FR).
 *
 * .darwin: This implementation supports Darwin (macOS) (platform
 * MPS_OS_XC).
 *
 * .design: These locks are implemented using mutexes.
 *
 * .recursive: Mutexes support both non-recursive and recursive
 * locking, but only at initialization time.  This doesn't match the
 * API of MPS Lock module, which chooses at locking time, so all locks
 * are made (non-recursive) errorchecking.  Recursive locks are
 * implemented by checking the error code.
 *
 * .claims: During use the claims field is updated to remember the
 * number of claims acquired on a lock.  This field must only be
 * modified while we hold the mutex.
 *
 * .from: This was copied from the FreeBSD implementation (lockfr.c)
 * which was itself a cleaner version of the LinuxThreads
 * implementation (lockli.c).
 */

#include "mpm.h"

#if !defined(MPS_OS_FR) && !defined(MPS_OS_LI) && !defined(MPS_OS_XC)
#error "lockix.c is specific to MPS_OS_FR, MPS_OS_LI or MPS_OS_XC"
#endif

#include "lock.h"

#include <pthread.h> /* see .feature.li in config.h */
#include <semaphore.h>
#include <errno.h>

SRCID(lockix, "$Id$");

#if defined(LOCK)

/* LockStruct -- the MPS lock structure
 *
 * .lock.posix: Posix lock structure; uses a mutex.
 */

typedef struct LockStruct {
  Sig sig;                      /* <design/sig> */
  unsigned long claims;         /* # claims held by owner */
  pthread_mutex_t mut;          /* the mutex itself */
} LockStruct;


/* LockSize -- size of a LockStruct */

size_t (LockSize)(void)
{
  return sizeof(LockStruct);
}


/* LockCheck -- check a lock */

Bool (LockCheck)(Lock lock)
{
  CHECKS(Lock, lock);
  /* While claims can't be very large, I don't dare to put a limit on it. */
  /* There's no way to test the mutex, or check if it's held by somebody. */
  return TRUE;
}


/* LockInit -- initialize a lock */

void (LockInit)(Lock lock)
{
  pthread_mutexattr_t attr;
  int res;

  AVER(lock != NULL);
  lock->claims = 0;
  res = pthread_mutexattr_init(&attr);
  AVER(res == 0);
  res = pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
  AVER(res == 0);
  res = pthread_mutex_init(&lock->mut, &attr);
  AVER(res == 0);
  res = pthread_mutexattr_destroy(&attr);
  AVER(res == 0);
  lock->sig = LockSig;
  AVERT(Lock, lock);
}


/* LockFinish -- finish a lock */

void (LockFinish)(Lock lock)
{
  int res;

  AVERT(Lock, lock);
  /* Lock should not be finished while held */
  AVER(lock->claims == 0);
  res = pthread_mutex_destroy(&lock->mut);
  AVER(res == 0);
  lock->sig = SigInvalid;
}


/* LockClaim -- claim a lock (non-recursive) */

void (LockClaim)(Lock lock)
{
  int res;

  AVERT(Lock, lock);

  res = pthread_mutex_lock(&lock->mut);
  /* pthread_mutex_lock will error if we own the lock already. */
  AVER(res == 0); /* <design/check/#.common> */

  /* This should be the first claim.  Now we own the mutex */
  /* it is ok to check this. */
  AVER(lock->claims == 0);
  lock->claims = 1;
}


/* LockRelease -- release a lock (non-recursive) */

void (LockRelease)(Lock lock)
{
  int res;

  AVERT(Lock, lock);
  AVER(lock->claims == 1);  /* The lock should only be held once */
  lock->claims = 0;  /* Must set this before releasing the lock */
  res = pthread_mutex_unlock(&lock->mut);
  /* pthread_mutex_unlock will error if we didn't own the lock. */
  AVER(res == 0);
}


/* LockClaimRecursive -- claim a lock (recursive) */

void (LockClaimRecursive)(Lock lock)
{
  int res;

  AVERT(Lock, lock);

  res = pthread_mutex_lock(&lock->mut);
  /* pthread_mutex_lock will return: */
  /*     0 if we have just claimed the lock */
  /*     EDEADLK if we own the lock already. */
  AVER((res == 0) == (lock->claims == 0));
  AVER((res == EDEADLK) == (lock->claims > 0));

  ++lock->claims;
  AVER(lock->claims > 0);
}


/* LockReleaseRecursive -- release a lock (recursive) */

void (LockReleaseRecursive)(Lock lock)
{
  int res;

  AVERT(Lock, lock);
  AVER(lock->claims > 0);
  --lock->claims;
  if (lock->claims == 0) {
    res = pthread_mutex_unlock(&lock->mut);
    /* pthread_mutex_unlock will error if we didn't own the lock. */
    AVER(res == 0);
  }
}


/* LockIsHeld -- test whether lock is held */

Bool (LockIsHeld)(Lock lock)
{
  AVERT(Lock, lock);
  if (pthread_mutex_trylock(&lock->mut) == 0) {
    Bool claimed = lock->claims > 0;
    int res = pthread_mutex_unlock(&lock->mut);
    AVER(res == 0);
    return claimed;
  }
  return TRUE;
}


/* Global locks
 *
 * .global: The two "global" locks are statically allocated normal locks.
 */

static LockStruct globalLockStruct;
static LockStruct globalRecLockStruct;
static Lock globalLock = &globalLockStruct;
static Lock globalRecLock = &globalRecLockStruct;
static pthread_once_t isGlobalLockInit = PTHREAD_ONCE_INIT;

void LockInitGlobal(void)
{
  LockInit(globalLock);
  LockInit(globalRecLock);
}


/* LockClaimGlobalRecursive -- claim the global recursive lock */

void (LockClaimGlobalRecursive)(void)
{
  int res;

  /* Ensure the global lock has been initialized */
  res = pthread_once(&isGlobalLockInit, LockInitGlobal);
  AVER(res == 0);
  LockClaimRecursive(globalRecLock);
}


/* LockReleaseGlobalRecursive -- release the global recursive lock */

void (LockReleaseGlobalRecursive)(void)
{
  LockReleaseRecursive(globalRecLock);
}


/* LockClaimGlobal -- claim the global non-recursive lock */

void (LockClaimGlobal)(void)
{
  int res;

  /* Ensure the global lock has been initialized */
  res = pthread_once(&isGlobalLockInit, LockInitGlobal);
  AVER(res == 0);
  LockClaim(globalLock);
}


/* LockReleaseGlobal -- release the global non-recursive lock */

void (LockReleaseGlobal)(void)
{
  LockRelease(globalLock);
}


/* LockSetup -- one-time lock initialization */

void LockSetup(void)
{
  /* Claim all locks before a fork; release in the parent;
     reinitialize in the child <design/thread-safety#.sol.fork.lock> */
  pthread_atfork(GlobalsClaimAll, GlobalsReleaseAll, GlobalsReinitializeAll);
}


#elif defined(LOCK_NONE)
#include "lockan.c"
#else
#error "No lock configuration."
#endif


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

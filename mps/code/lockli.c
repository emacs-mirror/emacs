/* impl.c.lockli: RECURSIVE LOCKS FOR POSIX SYSTEMS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .linux: This implementation currently just supports LinuxThreads
 * (platform MPS_OS_LI), Single Unix i/f.
 *
 * .posix: In fact, the implementation should be reusable for most POSIX
 * implementations, but may need some customization for each.
 *
 * .design: These locks are implemented using mutexes.
 *
 * .recursive: Mutexes support both non-recursive and recursive locking, but
 * only at initialization time.  This doesn't match the API of MPS Lock module,
 * which chooses at locking time, so all locks are made (non-recursive)
 * errorchecking.  Recursive locks are implemented by checking the error
 * code.
 *
 * .claims: During use the claims field is updated to remember the number of
 * claims acquired on a lock.  This field must only be modified
 * while we hold the mutex. 
 */

#define _XOPEN_SOURCE 500
#include <pthread.h>
#include <semaphore.h>
#include <errno.h>

#include "mpmtypes.h"
#include "lock.h"
#include "config.h"


#ifndef MPS_OS_LI
#error "lockli.c is specific to LinuxThreads but MPS_OS_LI not defined"
#endif

SRCID(lockli, "$Id$");


/* LockAttrSetRecursive -- Set mutexattr to permit recursive locking
 *
 * There's a standard way to do this - but early LinuxThreads doesn't
 * quite follow the standard.  Some other implementations might not
 * either.
 */

#ifdef OLD_LINUXTHREADS

#define LockAttrSetRecursive(attrptr) \
  pthread_mutexattr_setkind_np(attrptr, PTHREAD_MUTEX_ERRORCHECK_NP)
   
#else

#define LockAttrSetRecursive(attrptr) \
  pthread_mutexattr_settype(attrptr, PTHREAD_MUTEX_ERRORCHECK)

#endif


/* LockStruct -- the MPS lock structure
 *
 * .lock.posix: Posix lock structure; uses a mutex.
 */

typedef struct LockStruct {
  Sig sig;                      /* design.mps.sig */
  unsigned long claims;         /* # claims held by owner */
  pthread_mutex_t mut;          /* the mutex itself */
} LockStruct;


/* LockSize -- size of a LockStruct */

size_t LockSize(void)
{
  return sizeof(LockStruct);
}


/* LockCheck -- check a lock */

Bool LockCheck(Lock lock)
{
  CHECKS(Lock, lock);
  /* While claims can't be very large, I don't dare to put a limit on it. */
  /* There's no way to test the mutex, or check if it's held by somebody. */
  return TRUE;
}


/* LockInit -- initialize a lock */

void LockInit(Lock lock)
{
  pthread_mutexattr_t attr;
  int res;

  AVER(lock != NULL);
  lock->claims = 0;
  res = pthread_mutexattr_init(&attr);
  AVER(res == 0);
  res = LockAttrSetRecursive(&attr);
  AVER(res == 0);
  res = pthread_mutex_init(&lock->mut, &attr);
  AVER(res == 0);
  res = pthread_mutexattr_destroy(&attr);
  AVER(res == 0);
  lock->sig = LockSig;
  AVERT(Lock, lock);
}


/* LockFinish -- finish a lock */

void LockFinish(Lock lock)
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

void LockClaim(Lock lock)
{
  int res;

  AVERT(Lock, lock);

  res = pthread_mutex_lock(&lock->mut);
  /* pthread_mutex_lock will error if we own the lock already. */
  AVER(res == 0);

  /* This should be the first claim.  Now we own the mutex */
  /* it is ok to check this. */
  AVER(lock->claims == 0);
  lock->claims = 1;
}


/* LockReleaseMPM -- release a lock (non-recursive) */

void LockReleaseMPM(Lock lock)
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

void LockClaimRecursive(Lock lock)
{
  int res;

  AVERT(Lock, lock);

  res = pthread_mutex_lock(&lock->mut);
  /* pthread_mutex_lock will return: */
  /*     0 if we have just claimed the lock */
  /*     EDEADLK if we own the lock already. */
  AVER((res == 0 && lock->claims == 0)  || 
       (res == EDEADLK && lock->claims > 0));

  ++lock->claims;
  AVER(lock->claims > 0);
}


/* LockReleaseRecursive -- release a lock (recursive) */

void LockReleaseRecursive(Lock lock)
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


/* Global locks
 *
 * .global: The two "global" locks are statically allocated normal locks.
 */

static LockStruct globalLockStruct;
static LockStruct globalRecLockStruct;
static Lock globalLock = &globalLockStruct;
static Lock globalRecLock = &globalRecLockStruct;
static pthread_once_t isGlobalLockInit = PTHREAD_ONCE_INIT;

static void globalLockInit(void)
{
  LockInit(globalLock);
  LockInit(globalRecLock);
}


/* LockClaimGlobalRecursive -- claim the global recursive lock */

void LockClaimGlobalRecursive(void)
{
  int res;

  /* Ensure the global lock has been initialized */
  res = pthread_once(&isGlobalLockInit, globalLockInit);
  AVER(res == 0);
  LockClaimRecursive(globalRecLock);
}


/* LockReleaseGlobalRecursive -- release the global recursive lock */

void LockReleaseGlobalRecursive(void)
{
  LockReleaseRecursive(globalRecLock);
}


/* LockClaimGlobal -- claim the global non-recursive lock */

void LockClaimGlobal(void)
{
  int res;

  /* Ensure the global lock has been initialized */
  res = pthread_once(&isGlobalLockInit, globalLockInit);
  AVER(res == 0);
  LockClaim(globalLock);
}


/* LockReleaseGlobal -- release the global non-recursive lock */

void LockReleaseGlobal(void)
{
  LockReleaseMPM(globalLock);
}


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

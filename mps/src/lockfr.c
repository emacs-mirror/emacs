/* impl.c.lockfr: RECURSIVE LOCKS FOR POSIX SYSTEMS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .freebsd: This implementation supports FreeBSD (platform
 * MPS_OS_FR).
 *
 * .posix: In fact, the implementation should be reusable for most POSIX
 * implementations, but may need some customization for each.
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
 * modified while we hold the mutex.  */

#include <pthread.h>
#include <semaphore.h>
#include <errno.h>

#include "mpmtypes.h"
#include "lock.h"
#include "config.h"


#ifndef MPS_OS_FR
#error "lockfr.c is FreeBSD specific but MPS_OS_FR not defined"
#endif

SRCID(lockfr, "$Id$");


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

/* impl.c.lockli: RECURSIVE LOCKS FOR POSIX SYSTEMS
 *
 * $HopeName: $
 * Copyright (C) 2000 Harlequin Ltd.  All rights reserved.
 *
 *  .linux: This implementation currently just supports LinuxThreads 
 *  (platform MPS_OS_LI)
 *
 *  .posix: In fact, the implementation should be reusable for most POSIX
 *  implementations, but may need some customization for each. 
 *
 *  .design: These locks are implemented using mutexes.
 *
 *  .recursive: Mutexes support both non-recursive and recursive locking, but 
 *  only at initialization time.  This doesn't match the API of MPS Lock module, 
 *  which chooses at locking time, so all locks are made (non recursive)
 *  errorchecking.  Recursive locks are implemented by checking the error
 *  code.
 *
 *  .claims: During use the claims field is updated to remember the number of
 *  claims acquired on a lock.  This field must only be modified
 *  while we hold the mutex.  
 */

#include "mpm.h"

/* .linux */
#ifndef MPS_OS_LI
#error "lockli.c is specific to LinuxThreads but MPS_OS_LI not defined"
#endif

#include <pthread.h>
#include <semaphore.h>

SRCID(lockli, "$HopeName: $");


/* LockAttrSetRecursive -- Set mutexattr to permimt recursive locking
 *
 * There's a standard way to do this - but LinuxThreads doesn't quite follow 
 * the standard. Some other implementations might not either. Keep the code
 * general to permit future reuse. (.posix)
 */

#ifdef MPS_OS_LI

#define LockAttrSetRecursive(attrptr) \
  (pthread_mutexattr_setkind_np((attrptr), PTHREAD_MUTEX_ERRORCHECK_NP))
    
#else

#define LockAttrSetRecursive(attrptr) \
  (pthread_mutexattr_settype((attrptr), PTHREAD_MUTEX_ERRORCHECK))

#endif


/* .lock.posix: Posix lock structure; uses a mutex */
typedef struct LockStruct {
  Sig sig;                      /* design.mps.sig */
  unsigned long claims;         /* # claims held by owner */
  pthread_mutex_t mut;          /* the mutex itself */
} LockStruct;


size_t LockSize(void)
{
  return sizeof(LockStruct);
}

Bool LockCheck(Lock lock)
{
  CHECKS(Lock, lock);
  return TRUE;
}

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


/* Global locking is performed by normal locks. 
 * A separate lock structure is used for recursive and 
 * non-recursive locks so that each may be differently ordered
 * with respect to client-allocated locks.
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

void LockClaimGlobalRecursive(void)
{
  int res;

  /* Ensure the global lock has been initialized */
  res = pthread_once(&isGlobalLockInit, globalLockInit);
  AVER(res == 0);
  LockClaimRecursive(globalRecLock);
}

void LockReleaseGlobalRecursive(void)
{
  LockReleaseRecursive(globalRecLock);
}

void LockClaimGlobal(void)
{
  int res;

  /* Ensure the global lock has been initialized */
  res = pthread_once(&isGlobalLockInit, globalLockInit);
  AVER(res == 0);
  LockClaim(globalLock);
}

void LockReleaseGlobal(void)
{
  LockReleaseMPM(globalLock);
}


/* impl.c.lockan: ANSI RECURSIVE LOCKS
 *
 * $HopeName: MMsrc!lockan.c(trunk.9) $
 * Copyright (C) 1998 Harlequin Limited.  All rights reserved.
 *
 * .purpose: This is a trivial implementation of recursive locks
 * that assumes we are not running in a multi-threaded environment.
 * This provides stubs for the locking code where locking is not
 * applicable.  The stubs provide some amount of checking.
 *
 * .limit: The limit on the number of recursive claims is ULONG_MAX.
 */

#include "lock.h"
#include "mpmtypes.h"

SRCID(lockan, "$HopeName: MMsrc!lockan.c(trunk.9) $");


typedef struct LockStruct {     /* ANSI fake lock structure */
  Sig sig;                      /* design.mps.sig */
  unsigned long claims;         /* # claims held by owner */
} LockStruct;


size_t (LockSize)(void)
{
  return sizeof(LockStruct);
}

Bool (LockCheck)(Lock lock)
{
  CHECKS(Lock, lock);
  return TRUE;
}


void (LockInit)(Lock lock)
{
  AVER(lock != NULL);
  lock->claims = 0;
  lock->sig = LockSig;
  AVERT(Lock, lock);
}

void (LockFinish)(Lock lock)
{
  AVERT(Lock, lock);
  AVER(lock->claims == 0);
  lock->sig = SigInvalid;
}


void (LockClaim)(Lock lock)
{
  AVERT(Lock, lock);
  AVER(lock->claims == 0);
  lock->claims = 1;
}

void (LockReleaseMPM)(Lock lock)
{
  AVERT(Lock, lock);
  AVER(lock->claims == 1);
  lock->claims = 0;
}

void (LockClaimRecursive)(Lock lock)
{
  AVERT(Lock, lock);
  ++lock->claims;
  AVER(lock->claims>0);
}

void (LockReleaseRecursive)(Lock lock)
{
  AVERT(Lock, lock);
  AVER(lock->claims > 0);
  --lock->claims;
}


/* Global locking is performed by normal locks. 
 * A separate lock structure is used for recursive and 
 * non-recursive locks so that each may be differently ordered
 * with respect to client-allocated locks.
 */

static LockStruct globalLockStruct = {
  LockSig,
  0
};

static LockStruct globalRecursiveLockStruct = {
  LockSig,
  0
};

static Lock globalLock = &globalLockStruct;

static Lock globalRecLock = &globalRecursiveLockStruct;


void (LockClaimGlobalRecursive)(void)
{
  LockClaimRecursive(globalRecLock);
}

void (LockReleaseGlobalRecursive)(void)
{
  LockReleaseRecursive(globalRecLock);
}

void (LockClaimGlobal)(void)
{
  LockClaim(globalLock);
}

void (LockReleaseGlobal)(void)
{
  LockReleaseMPM(globalLock);
}

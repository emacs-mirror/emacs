/* impl.c.lockan: ANSI RECURSIVE LOCKS
 *
 * $HopeName$
 * Copyright (C) 1995, 1998 Harlequin Group plc.  All rights reserved.
 *
 * .purpose: This is a trivial implementation of recursive locks
 * that assumes we are not running in a multi-threaded environment.
 * This provides stubs for the locking code where locking is not
 * applicable.  The stubs provide some amount of checking.
 *
 * .limit: The limit on the number of recursive claims is ULONG_MAX.
 */

#include "mpm.h"

SRCID(lockan, "$HopeName$");


Bool LockCheck(Lock lock)
{
  CHECKS(Lock, lock);
  return TRUE;
}

void LockInit(Lock lock)
{
  AVER(lock != NULL);
  lock->claims = 0;
  lock->sig = LockSig;
  AVERT(Lock, lock);
}

void LockFinish(Lock lock)
{
  AVERT(Lock, lock);
  AVER(lock->claims == 0);
  lock->sig = SigInvalid;
}

void LockClaim(Lock lock)
{
  AVERT(Lock, lock);
  AVER(lock->claims == 0);
  lock->claims = 1;
}

void LockReleaseMPM(Lock lock)
{
  AVERT(Lock, lock);
  AVER(lock->claims == 1);
  lock->claims = 0;
}

void LockClaimRecursive(Lock lock)
{
  AVERT(Lock, lock);
  ++lock->claims;
  AVER(lock->claims>0);
}

void LockReleaseRecursive(Lock lock)
{
  AVERT(Lock, lock);
  AVER(lock->claims > 0);
  --lock->claims;
}


/* Global locking is performed by a normal lock. */

static LockStruct globalLockStruct = {
  LockSig,
  0
};

static Lock globalLock = &globalLockStruct;

void LockClaimGlobalRecursive(void)
{
  LockClaimRecursive(globalLock);
}

void LockReleaseGlobalRecursive(void)
{
  LockReleaseRecursive(globalLock);
}

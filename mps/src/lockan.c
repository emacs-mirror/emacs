/*  impl.c.lockan
 *
 *                  ANSI RECURSIVE LOCKS
 *
 *  $HopeName: MMsrc!lockan.c(MMdevel_restr.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a trivial implemenation of recursive locks
 *  that assumes we are not running in a multi-threaded
 *  environment.
 *
 *  This provides stubs for the locking code where locking
 *  is not applicable.  The stubs provide some amount of
 *  checking.
 *
 *  The limit on the number of recursive claims is ULONG_MAX.
 */

#include "mpm.h"

SRCID(lockan, "$HopeName: MMsrc!lockan.c(MMdevel_restr.2) $");

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

void LockRelease(Lock lock)
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

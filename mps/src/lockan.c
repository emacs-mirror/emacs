/*  impl.c.lockan
 *
 *                  ANSI RECURSIVE LOCKS
 *
 *  $HopeName: MMsrc!lockan.c(trunk.3) $
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

#include "std.h"
#include "lock.h"
#include "lockst.h"

SRCID("$HopeName: MMsrc!lockan.c(trunk.3) $");

#ifdef DEBUG

Bool LockIsValid(Lock lock, ValidationType validParam)
{
  AVER(lock->sig == LockSig);
  return TRUE;
}  

#endif

void LockInit(Lock lock)
{
  AVER(lock != NULL);
  lock->claims = 0; 
  lock->sig = LockSig;
  AVER(ISVALID(Lock, lock));
}

void LockFinish(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  AVER(lock->claims == 0);
  lock->sig = SigInvalid;
}

void LockClaim(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  AVER(lock->claims == 0);
  lock->claims = 1;        
}

void LockRelease(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  AVER(lock->claims == 1);
  lock->claims = 0;
}

void LockClaimRecursive(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  ++lock->claims;
  AVER(lock->claims>0);
}

void LockReleaseRecursive(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  AVER(lock->claims > 0);
  --lock->claims;
}

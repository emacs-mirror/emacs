/*  impl.c.locknt
 *
 *                  RECURSIVE LOCKS IN WIN32
 *
 *  $HopeName: MMsrc!locknt.c(trunk.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  These are implemented using critical sections.
 *  See the section titled "Synchronization functions" in the Groups
 *  chapter of the Microsoft Win32 API Programmer's Reference.
 *  The "Synchronization" section of the Overview is also relevant.
 *
 *  Critical sections support recursive locking, so the implementation
 *  could be trivial.  This implementation counts the claims to provide
 *  extra checking.
 *
 *  The limit on the number of recursive claims is the max of
 *  ULONG_MAX and the limit imposed by critical sections, which
 *  is believed to be about UCHAR_MAX.
 *
 *  During use the claims field is updated to remember the number of
 *  claims acquired on a lock.  This field must only be modified
 *  while we are inside the critical section.
 */

#include "std.h"
#include "lock.h"
#include "lockst.h"

#ifndef MPS_OS_NT
#error "locknt.c is specific to Win32 but MPS_OS_NT not defined"
#endif

#include <windows.h>

SRCID("$HopeName");

static SigStruct LockSigStruct;

#ifdef DEBUG

Bool LockIsValid(Lock lock, ValidationType validParam)
{
  AVER(ISVALIDNESTED(Sig, &LockSigStruct));
  AVER(lock->sig == &LockSigStruct);
  return TRUE;
}  

#endif

void LockInit(Lock lock)
{
  AVER(lock != NULL);
  lock->claims = 0; 
  InitializeCriticalSection(&lock->cs);
  SigInit(&LockSigStruct, "Lock");
  lock->sig = &LockSigStruct;
  AVER(ISVALID(Lock, lock));
}

void LockFinish(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  /* Lock should not be finished while held */
  AVER(lock->claims == 0);
  DeleteCriticalSection(&lock->cs);
  lock->sig = SigInvalid;
}

void LockClaim(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  EnterCriticalSection(&lock->cs);
  /* This should be the first claim.  Now we are inside the
   * critical section it is ok to check this. */
  AVER(lock->claims == 0);
  lock->claims = 1;        
}

void LockRelease(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  AVER(lock->claims == 1);  /* The lock should only be held once */
  lock->claims = 0;  /* Must set this before leaving CS */
  LeaveCriticalSection(&lock->cs);
}

void LockClaimRecursive(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  EnterCriticalSection(&lock->cs);
  ++lock->claims;
  AVER(lock->claims > 0);
}

void LockReleaseRecursive(Lock lock)
{
  AVER(ISVALID(Lock, lock));
  AVER(lock->claims > 0);
  --lock->claims;
  LeaveCriticalSection(&lock->cs);
}

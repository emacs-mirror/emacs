/* lockan.c: ANSI RECURSIVE LOCKS
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
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

SRCID(lockan, "$Id$");


typedef struct LockStruct {     /* ANSI fake lock structure */
  Sig sig;                      /* <design/sig> */
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

void (LockRelease)(Lock lock)
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

Bool (LockIsHeld)(Lock lock)
{
  AVERT(Lock, lock);
  return lock->claims > 0;
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

void LockInitGlobal(void)
{
  globalLock->claims = 0;
  LockInit(globalLock);
  globalRecLock->claims = 0;
  LockInit(globalRecLock);
}

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
  LockRelease(globalLock);
}

void LockSetup(void)
{
  /* Nothing to do as ANSI platform does not have fork(). */
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

/* lockcov.c: LOCK COVERAGE TEST
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
#include "testlib.h"
#include "mpslib.h"
#include <stdlib.h>             /* for malloc & free */


int main(int argc, char *argv[])
{
  Lock a = malloc(LockSize());
  Lock b = malloc(LockSize());
  testlib_unused(argc);

  Insist(a != NULL);
  Insist(b != NULL);

  LockInit(a);
  LockInit(b);
  LockClaimGlobal();
  LockClaim(a);
  LockClaimRecursive(b);
  LockClaimGlobalRecursive();
  LockReleaseGlobal();
  LockClaimGlobal();
  LockReleaseMPM(a);
  LockClaimGlobalRecursive();
  LockReleaseGlobal();
  LockClaimRecursive(b);
  LockFinish(a);
  LockReleaseRecursive(b);
  LockReleaseRecursive(b);
  LockFinish(b);
  LockInit(a);
  LockClaim(a);
  LockClaimRecursive(a);
  LockReleaseGlobalRecursive();
  LockReleaseRecursive(a);
  LockReleaseMPM(a);
  LockFinish(a);
  LockReleaseGlobalRecursive();
  free(a);
  free(b);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

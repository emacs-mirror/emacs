/* impl.c.lockcov: LOCK COVERAGE TEST
 *
 * $HopeName: MMsrc!lockcov.c(trunk.4) $
 * Copyright (C) 1997 Harlequin Limited.  All rights reserved.
 */

#include "mpm.h"
#include "testlib.h"
#include <stdlib.h>             /* for malloc & free */


int main(void)
{
  Lock a = malloc(LockSize());
  Lock b = malloc(LockSize());

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

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}

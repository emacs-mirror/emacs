/* impl.c.lockcov
 *                     LOCK COVERAGE TEST
 *
 * $HopeName: !lockcov.c(trunk.4) $
 */

#include "mpm.h"
#include <stdlib.h>             /* for malloc & free */

int main(void){
  Lock a = malloc(LockSize());
  Lock b = malloc(LockSize());

  AVER(a != NULL);
  AVER(b != NULL);

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

  return 0;
}

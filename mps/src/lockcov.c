/* impl.c.lockcov
 *                     LOCK COVERAGE TEST
 *
 * $HopeName: MMsrc!lockcov.c(MMdevel_restr.2) $
 */

#include "mpm.h"

int main(void){
  LockStruct a,b;

  LockInit(&a);
  LockInit(&b);
  LockClaim(&a);
  LockClaimRecursive(&b);
  LockRelease(&a);
  LockClaimRecursive(&b);
  LockFinish(&a);
  LockReleaseRecursive(&b);
  LockReleaseRecursive(&b);
  LockFinish(&b);
  LockInit(&a);
  LockClaim(&a);
  LockClaimRecursive(&a);
  LockReleaseRecursive(&a);
  LockRelease(&a);
  LockFinish(&a);

  return 0;
}

/* impl.c.lockcov
 *                     LOCK COVERAGE TEST
 *
 * $HopeName$
 */

#include "std.h"
#include "lock.h"
#include "lockst.h"

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

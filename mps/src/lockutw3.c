/* impl.c.lockutw3: LOCK COVERAGE TEST
 *
 * $HopeName: !lockutw3.c(trunk.8) $
 */

#include "mpm.h"
#include "testlib.h"

#ifndef MPS_OS_W3
#error "Relies on Win32 threads"
#endif

#include "mpswin.h"

SRCID(lockutw3, "$HopeName: !lockutw3.c(trunk.8) $");


static Lock lock;
unsigned long shared,tmp;


void incR(unsigned long i)
{
  LockClaimRecursive(lock);
  if(i<100) {
    while(i--) {
      tmp=shared;
      shared=tmp+1;
    }
  } else {
    incR(i>>1);
    incR(i+1>>1);
  }
  LockReleaseRecursive(lock);
}

void inc(unsigned long i)
{
  incR(i+1>>1);
  i>>=1;
  while(i){
    LockClaim(lock);
    if(i>10000){
      incR(5000);
      i-=5000;
    }
    tmp=shared;
    shared=tmp+1;
    i--;
    LockReleaseMPM(lock);
  }
}

#define COUNT 100000l
DWORD WINAPI thread0(void *p)
{
  (void)p;
  inc(COUNT);
  return 0;
}

int main(void)
{
  DWORD id;
  HANDLE t[10];
  unsigned i,nthreads;

  nthreads = 4;

  lock = malloc(LockSize());
  AVER(lock != NULL);

  LockInit(lock);

  shared = 0;

  for(i=0;i<nthreads;i++)
    t[i] = CreateThread(NULL,0,thread0,NULL,0,&id);

  for(i=0;i<nthreads;i++)
    WaitForSingleObject(t[i], INFINITE);

  AVER(shared == nthreads*COUNT);

  LockFinish(lock);

  return 0;
}

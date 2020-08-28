/* lockut.c: LOCK UTILIZATION TEST
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#include "mps.h"
#include "mpsavm.h"
#include "mpscmfs.h"
#include "mpm.h"
#include "testlib.h"
#include "testthr.h"

#include <stdio.h> /* printf */


#define nTHREADS 4

static Lock lock;
static unsigned long shared, tmp;


static void incR(unsigned long i)
{
  LockClaimRecursive(lock);
  if (i < 100) {
    while(i--) {
      tmp = shared;
      shared = tmp + 1;
    }
  } else {
    incR(i >> 1);
    incR( (i+1) >> 1);
  }
  LockReleaseRecursive(lock);
}


static void inc(unsigned long i)
{
  incR( (i+1) >>1);
  i >>= 1;
  while (i) {
    LockClaim(lock);
    if (i > 10000) {
      incR(5000);
      i -= 5000;
    }
    tmp = shared;
    shared = tmp+1;
    i--;
    LockRelease(lock);
  }
}


#define COUNT 100000l
static void *thread0(void *p)
{
  unsigned i;
  testlib_unused(p);
  LockClaimGlobal();
  LockReleaseGlobal();
  for (i = 0; i < COUNT; ++i)
    LockClaimGlobalRecursive();
  for (i = 0; i < COUNT; ++i)
    LockReleaseGlobalRecursive();
  inc(COUNT);
  return NULL;
}


int main(int argc, char *argv[])
{
  mps_arena_t arena;
  mps_pool_t pool;
  mps_addr_t p;
  testthr_t t[10];
  unsigned i;

  testlib_init(argc, argv);

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
      "arena_create");
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_MFS_UNIT_SIZE, LockSize());
    die(mps_pool_create_k(&pool, arena, mps_class_mfs(), args), "pool_create");
  } MPS_ARGS_END(args);

  die(mps_alloc(&p, pool, LockSize()), "alloc");
  lock = p;
  Insist(lock != NULL);

  LockInit(lock);
  UNUSED(argc);

  shared = 0;

  for(i = 0; i < nTHREADS; i++)
    testthr_create(&t[i], thread0, NULL);

  for(i = 0; i < nTHREADS; i++)
    testthr_join(&t[i], NULL);

  Insist(shared == nTHREADS*COUNT);

  LockFinish(lock);

  mps_free(pool, lock, LockSize());
  mps_pool_destroy(pool);
  mps_arena_destroy(arena);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

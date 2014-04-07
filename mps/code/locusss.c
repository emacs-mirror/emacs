/* locusss.c: LOCUS STRESS TEST
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 */

#include "mpscmvff.h"
#include "mpscmv.h"
#include "mpslib.h"
#include "mpsavm.h"
#include "testlib.h"
#include "mpslib.h"
#include "mps.h"

#include <stdlib.h>
#include <stdarg.h>


/* some constants */

#define TRUE  1
#define FALSE 0

#define iterationCount 30          /* number of iterations */
#define contigAllocs 8             /* number of allocs each iteration */
#define chunkSize ((size_t)65536)  /* our allocation chunk size */

#define smallArenaSize \
  ((size_t)(chunkSize * iterationCount * contigAllocs * 2))


#define AddressOffset(b, l) \
  ((size_t)((char *)(l) - (char *)(b)))


/* PoolStat -- maintain data about contiguous allocations */

typedef struct PoolStatStruct *PoolStat;

typedef struct PoolStatStruct {
  mps_pool_t pool;  /* the pool being measured */
  size_t objSize;   /* size of each allocation */
  mps_addr_t min;   /* lowest address lock allocated to the pool */
  mps_addr_t max;   /* highest address lock allocated to the pool */
  int ncCount;      /* count of non-contiguous allocations */
  int aCount;       /* count of allocations */
  int fCount;       /* count of frees */
} PoolStatStruct;



static mps_addr_t allocObject(mps_pool_t pool, size_t size)
{
  mps_addr_t addr;
  die(mps_alloc(&addr, pool, size),
      "Allocate Object");
  return addr;
}

static void recordNewObjectStat(PoolStat stat, mps_addr_t obj)
{
  stat->aCount++;
  if (obj < stat->min) {
    if (AddressOffset(obj, stat->min) > stat->objSize) {
      stat->ncCount++;
    }
    stat->min = obj;
  } else if (obj > stat->max) {
    if (AddressOffset(stat->max, obj) > stat->objSize) {
      stat->ncCount++;
    }
    stat->max = obj;
  }
}

static void recordFreedObjectStat(PoolStat stat)
{
  stat->fCount++;
}


static void poolStatInit(PoolStat stat, mps_pool_t pool, size_t objSize)
{
  mps_addr_t s1, s2, s3;

  stat->pool = pool;
  stat->objSize = objSize;
  stat->ncCount = 0;
  stat->aCount = 0;
  stat->fCount = 0;

  /* allocate 3 half-size sentinel objects, freeing the middle one */
  /* to leave a bit of space for the control pool */
  s1 = allocObject(pool, objSize / 2);
  stat->min = s1;
  stat->max = s1;
  stat->aCount++;

  s2 = allocObject(pool, objSize / 2);
  recordNewObjectStat(stat, s2);
  s3 = allocObject(pool, objSize / 2);
  recordNewObjectStat(stat, s3);

  mps_free(pool, s2, objSize / 2);
  recordFreedObjectStat(stat);

}


static mps_res_t allocMultiple(PoolStat stat)
{
  mps_addr_t objects[contigAllocs];
  size_t i;

  /* allocate a few objects, and record stats for them */
  for (i = 0; i < contigAllocs; i++) {
    mps_addr_t obj;
    mps_res_t res = mps_alloc(&obj, stat->pool, stat->objSize);
    if (res != MPS_RES_OK)
      return res;
    recordNewObjectStat(stat, obj);
    objects[i] = obj;
  }

  /* free one of the objects, to make the test more interesting */
  i = rnd() % contigAllocs;
  mps_free(stat->pool, objects[i], stat->objSize);
  recordFreedObjectStat(stat);

  return MPS_RES_OK;
}


/* reportResults - print a report on a PoolStat */

static void reportResults(PoolStat stat, const char *name)
{
  printf("\nResults for ");
  printf("%s", name);
  printf("\n");
  printf("   Allocated  %"PRIuLONGEST" objects\n", (ulongest_t)stat->aCount);
  printf("   Freed      %"PRIuLONGEST" objects\n", (ulongest_t)stat->fCount);
  printf("   There were %lu non-contiguous allocations\n",
         (unsigned long)stat->ncCount);
  printf("   Address range from %p to %p\n", stat->min, stat->max);
  printf("\n");
}


static void testInArena(mps_arena_t arena,
                        mps_bool_t failcase,
                        mps_bool_t usefulFailcase)
{
  mps_pool_t lopool, hipool, temppool;
  PoolStatStruct lostruct;  /* stats about lopool */
  PoolStatStruct histruct;  /* stats about lopool */
  PoolStatStruct tempstruct;  /* stats about temppool */
  PoolStat lostat = &lostruct;
  PoolStat histat = &histruct;
  PoolStat tempstat = &tempstruct;
  int i;

  die(mps_pool_create(&hipool, arena, mps_class_mvff(),
                      chunkSize, chunkSize, (size_t)1024,
                      TRUE, TRUE, TRUE),
      "Create HI MFFV");

  die(mps_pool_create(&lopool, arena, mps_class_mvff(),
                      chunkSize, chunkSize, (size_t)1024,
                      FALSE, FALSE, TRUE),
      "Create LO MFFV");

  die(mps_pool_create(&temppool, arena, mps_class_mv(),
                      chunkSize, chunkSize, chunkSize),
      "Create TEMP");

  if(failcase) {
    if(usefulFailcase) {
      /* describe a useful failure case */
    } else {
      /* describe a misleading failure case */
    }
  }

  poolStatInit(lostat, lopool, chunkSize);
  poolStatInit(histat, hipool, chunkSize);
  poolStatInit(tempstat, temppool, chunkSize);

  /* iterate, allocating objects */
  for (i=0; i<iterationCount; ++i) {
    mps_res_t res;
    res = allocMultiple(lostat);
    if (res != MPS_RES_OK)
      break;
    res = allocMultiple(histat);
    if (res != MPS_RES_OK)
      break;
    res = allocMultiple(tempstat);
    if (res != MPS_RES_OK)
      break;
  }

  /* report results */
  reportResults(lostat, "the low MVFF pool");
  reportResults(histat, "the high MVFF pool");
  reportResults(tempstat, "the temp pool");

  mps_pool_destroy(hipool);
  mps_pool_destroy(lopool);
  mps_pool_destroy(temppool);

}

static void runArenaTest(size_t size,
                         mps_bool_t failcase,
                         mps_bool_t usefulFailcase)
{
  mps_arena_t arena;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, size);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_ZONED, FALSE);
    die(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
        "mps_arena_create");
  } MPS_ARGS_END(args);

  die(mps_arena_commit_limit_set(arena, size - chunkSize),
      "mps_arena_commit_limit_set");

  testInArena(arena, failcase, usefulFailcase);

  mps_arena_destroy(arena);

}


int main(int argc, char *argv[])
{
  testlib_init(argc, argv);

  printf("\nRunning test with no information about peak usage.\n");
  runArenaTest(smallArenaSize, FALSE, FALSE);
  /*
  printf("\nRunning test with useful information about peak usage.\n");
  runArenaTest(smallArenaSize, TRUE, TRUE);
  printf("\nRunning test with misleading information about peak usage.\n");
  runArenaTest(smallArenaSize, TRUE, FALSE);
  */

  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

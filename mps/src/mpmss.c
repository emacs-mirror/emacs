/*  impl.c.mpmss: MPM STRESS TEST
 *
 * $HopeName: MMsrc!mpmss.c(trunk.17) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

#include "mps.h"
#include "mpscmv.h"
#include "mpslib.h"
#include "mpsavm.h"
#include "testlib.h"
#include "mpsavm.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


/* @@@@ Hack due to missing mpscmfs.h */
extern mps_class_t PoolClassMFS(void);


#define testArenaSIZE   ((((size_t)64)<<20) - 4)
#define smallArenaSIZE  ((((size_t)1)<<20) - 4)
#define TEST_SET_SIZE 200
#define TEST_LOOPS 10


static mps_res_t stress(mps_class_t class, mps_arena_t arena,
                        size_t (*size)(int i), ...)
{
  mps_res_t res;
  mps_pool_t pool;
  va_list arg;
  int i, k;
  int *ps[TEST_SET_SIZE];
  size_t ss[TEST_SET_SIZE];

  va_start(arg, size);
  res = mps_pool_create_v(&pool, arena, class, arg);
  va_end(arg);
  if(res != MPS_RES_OK)
    return res;


  /* allocate a load of objects */
  for(i=0; i<TEST_SET_SIZE; ++i) {
    ss[i] = (*size)(i);

    res = mps_alloc((mps_addr_t *)&ps[i], pool, ss[i]);
    if(res != MPS_RES_OK)
      return res;
    *ps[i] = 1; /* Write something, so it gets swap. */

    if(i && i%4==0) putchar('\n');
    printf("%8lX %6lX ", (unsigned long)ps[i], (unsigned long)ss[i]);
  }
  putchar('\n');

  for (k=0; k<TEST_LOOPS; ++k) {
    /* shuffle all the objects */
    for(i=0; i<TEST_SET_SIZE; ++i) {
      int j = rand()%(TEST_SET_SIZE-i);
      void *tp;
      size_t ts;
      
      tp = ps[j]; ts = ss[j];
      ps[j] = ps[i]; ss[j] = ss[i];
      ps[i] = tp; ss[i] = ts;
    }
    /* free half of the objects */
    /* upper half, as when allocating them again we want smaller objects */
    /* see randomSize() */
    for(i=TEST_SET_SIZE/2; i<TEST_SET_SIZE; ++i) {
      mps_free(pool, (mps_addr_t)ps[i], ss[i]);
      /*    if(i == TEST_SET_SIZE/2)
            PoolDescribe((Pool)pool, mps_lib_stdout); */
    }
    /* allocate some new objects */
    for(i=TEST_SET_SIZE/2; i<TEST_SET_SIZE; ++i) {
      ss[i] = (*size)(i);
      res = mps_alloc((mps_addr_t *)&ps[i], pool, ss[i]);
      if(res != MPS_RES_OK) return res;
      
      if(i && i%4==0) putchar('\n');
      printf("%8lX %6lX ", (unsigned long)ps[i], (unsigned long)ss[i]);
    }
    putchar('\n');
  }
    
  mps_pool_destroy(pool);

  return MPS_RES_OK;
}


#define max(a, b) (((a) > (b)) ? (a) : (b))


static size_t randomSize(int i)
{
  /* Make the range large enough to span three pages in the segment table: */
  /* 160 segments/page, page size max 0x2000. */
  size_t maxSize = 2 * 160 * 0x2000;
  /* Reduce by a factor of 2 every 10 cycles.  Total allocation about 40 MB. */
  return rnd() % max((maxSize >> (i / 10)), 2) + 1;
}


static size_t fixedSizeSize = 0;

static size_t fixedSize(int i)
{
  testlib_unused(i);
  return fixedSizeSize;
}


static int test_in_arena(mps_arena_t arena)
{
  fixedSizeSize = 13;
  die(stress(PoolClassMFS(),
             arena, fixedSize, (size_t)100000, fixedSizeSize),
      "stress MFS");

  die(stress(mps_class_mv(),
             arena, randomSize, (size_t)65536,
             (size_t)32, (size_t)65536), "stress MV");

  return 0;
}

int main(void)
{
  mps_arena_t arena;
  int i;

  /* Randomize the random number generator a bit. */
  for(i = time(NULL) % 67; i > 0; --i) rnd();

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");

  test_in_arena(arena);

  mps_arena_destroy(arena);

  die(mps_arena_create(&arena, mps_arena_class_vm(), smallArenaSIZE),
      "mps_arena_create");

  test_in_arena(arena);

  mps_arena_destroy(arena);

  return 0;
}

/* impl.c.sacss: SAC MANUAL ALLOC STRESS TEST
 *
 * $HopeName: MMsrc!sacss.c(trunk.2) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 */


#include "mpscmv.h"
#include "mpscmvff.h"
#include "mpslib.h"
#include "mpsavm.h"
#include "mps.h"

#include "testlib.h"

#include <stdio.h>
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif
#include <stdlib.h>
#include <stdarg.h>
#ifdef MPS_OS_IA
struct itimerspec; /* stop complaints from time.h */
#endif
#include <time.h>


#define TRUE  1
#define FALSE 0

#define testArenaSIZE   ((((size_t)64)<<20) - 4)
#define testSetSIZE 200
#define testLOOPS 10

#define topClassSIZE 0xA00
#define classCOUNT 4


static mps_res_t make(mps_addr_t *p, mps_sac_t sac, size_t size)
{
  mps_res_t res;

  MPS_SAC_ALLOC(res, *p, sac, size, FALSE);
  return res;
}


static mps_res_t stress(mps_class_t class, mps_arena_t arena,
                        size_t classes_count, mps_sac_classes_t classes,
                        size_t (*size)(int i), ...)
{
  mps_res_t res;
  mps_pool_t pool;
  mps_sac_t sac;
  va_list arg;
  int i, k;
  int *ps[testSetSIZE];
  size_t ss[testSetSIZE];

  va_start(arg, size);
  res = mps_pool_create_v(&pool, arena, class, arg);
  va_end(arg);
  if (res != MPS_RES_OK)
    return res;

  die(mps_sac_create(&sac, pool, classes_count, classes), "SACCreate");

  /* allocate a load of objects */
  for (i = 0; i < testSetSIZE; ++i) {
    ss[i] = (*size)(i);

    res = make((mps_addr_t *)&ps[i], sac, ss[i]);
    if (res != MPS_RES_OK)
      return res;
    if (ss[i] >= sizeof(ps[i]))
      *ps[i] = 1; /* Write something, so it gets swap. */
  }

  mps_pool_check_fenceposts(pool);

  for (k = 0; k < testLOOPS; ++k) {
    /* shuffle all the objects */
    for (i=0; i<testSetSIZE; ++i) {
      int j = rnd()%(testSetSIZE-i);
      void *tp;
      size_t ts;
      
      tp = ps[j]; ts = ss[j];
      ps[j] = ps[i]; ss[j] = ss[i];
      ps[i] = tp; ss[i] = ts;
    }
    if (k == (testLOOPS / 2)) mps_sac_flush(sac);
    /* free half of the objects */
    /* upper half, as when allocating them again we want smaller objects */
    /* see randomSize() */
    switch (k % 2) {
    case 0: {
      for (i=testSetSIZE/2; i<testSetSIZE; ++i)
        MPS_SAC_FREE(sac, (mps_addr_t)ps[i], ss[i]);
    } break;
    case 1: {
      for (i=testSetSIZE/2; i<testSetSIZE; ++i)
        mps_sac_free(sac, (mps_addr_t)ps[i], ss[i]);
    } break;
    }
    /* allocate some new objects */
    for (i=testSetSIZE/2; i<testSetSIZE; ++i) {
      ss[i] = (*size)(i);
      switch (k % 2) {
      case 0: {
        res = make((mps_addr_t *)&ps[i], sac, ss[i]);
      } break;
      case 1: {
        res = mps_sac_alloc((mps_addr_t *)&ps[i], sac, ss[i], FALSE);
      } break;
      }      
      if (res != MPS_RES_OK) return res;
    }
  }
    
  mps_sac_destroy(sac);
  mps_pool_destroy(pool);

  return MPS_RES_OK;
}


#define max(a, b) (((a) > (b)) ? (a) : (b))


static size_t randomSize8(int i)
{
  size_t maxSize = 2 * 160 * 0x2000;
  size_t size;

  /* Reduce by a factor of 2 every 10 cycles.  Total allocation about 40 MB. */
  size = rnd() % max((maxSize >> (i / 10)), 2) + 1;
  return size;
}


static mps_pool_debug_option_s debugOptions = { (void *)"postpost", 8 };

static mps_sac_classes_t classes = { {8, 1, 1}, {16, 1, 2}, {136, 9, 5},
                                     {topClassSIZE, 9, 4} };

static int testInArena(mps_arena_t arena)
{
  printf("MVFF\n\n");
  die(stress(mps_class_mvff(), arena, classCOUNT, classes, randomSize8,
             (size_t)65536, (size_t)32, (size_t)4, TRUE, TRUE, TRUE),
      "stress MVFF");
  printf("MV debug\n\n");
  die(stress(mps_class_mv_debug(), arena, classCOUNT, classes, randomSize8,
             &debugOptions, (size_t)65536, (size_t)32, (size_t)65536),
      "stress MV debug");
  printf("MV\n\n");
  die(stress(mps_class_mv(), arena, classCOUNT, classes, randomSize8,
             (size_t)65536, (size_t)32, (size_t)65536),
      "stress MV");
  return 0;
}


int main(int argc, char **argv)
{
  mps_arena_t arena;

  randomize(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vmnz(), testArenaSIZE),
      "mps_arena_create");
  testInArena(arena);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}

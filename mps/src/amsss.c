/* impl.c.amsss: POOL CLASS AMS STRESS TEST
 *
 * $HopeName$
 * Copyright (C) 1998.  Harlequin Group plc.  All rights reserved.
 *
 * .design: Adapted from amcss.c, but not counting collections, just
 * total size of objects allocated (because epoch doesn't increment
 * when AMS is collected).
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include "testlib.h"
#include "mps.h"
#include "mpscams.h"
#include "mpsavm.h"
#include "fmtdy.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


#define exactRootsCOUNT 50
#define ambigRootsCOUNT 50
/* Even this much takes 20 min to run in variety CI on gaia. */ 
#define totalSizeMAX    800 * (size_t)1024
#define totalSizeSTEP   200 * (size_t)1024
#define objNULL         ((mps_addr_t)0xDECEA5ED)
#define testArenaSIZE   ((size_t)16<<20)
#define initTestFREQ    6000


static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];
static size_t totalSize = 0;


static mps_addr_t make(void)
{
  size_t length = rnd() % 20, size = (length+2) * sizeof(mps_word_t);
  mps_addr_t p;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, p, ap, size);
    if(res)
      die(res, "MPS_RESERVE_BLOCK");
    res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
    if(res)
      die(res, "dylan_init");
  } while(!mps_commit(ap, p, size));

  totalSize += size;
  return p;
}


static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_root_t exactRoot, ambigRoot;
  size_t lastStep = 0;
  unsigned long i;
  mps_ap_t busy_ap;
  mps_addr_t busy_init;

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(mps_fmt_create_A(&format, arena, dylan_fmt_A()), "fmt_create");

  die(mps_pool_create(&pool, arena, mps_class_ams(), format),
      "pool_create(ams)");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, MPS_RANK_EXACT), "BufferCreate 2");

  for(i = 0; i < exactRootsCOUNT; ++i)
    exactRoots[i] = objNULL;
  for(i = 0; i < ambigRootsCOUNT; ++i)
    ambigRoots[i] = (mps_addr_t)rnd();

  die(mps_root_create_table(&exactRoot, arena,
                            MPS_RANK_EXACT, (mps_rm_t)0,
                            &exactRoots[0], exactRootsCOUNT),
                            "root_create_table(exact)");
  die(mps_root_create_table(&ambigRoot, arena,
                            MPS_RANK_AMBIG, (mps_rm_t)0,
                            &ambigRoots[0], ambigRootsCOUNT),
                            "root_create_table(ambig)");

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, 64), "mps_reserve busy");

  i = 0;
  while(totalSize < totalSizeMAX) {
    size_t r;

    if(totalSize > lastStep + totalSizeSTEP) {
      lastStep = totalSize;
      printf("\nSize %lu bytes, %lu objects.\n",
             (unsigned long)totalSize, i);
      fflush(stdout);
      for(r = 0; r < exactRootsCOUNT; ++r)
        assert(exactRoots[r] == objNULL ||
               dylan_check(exactRoots[r]));
    }

    if(rnd() & 1)
      exactRoots[rnd() % exactRootsCOUNT] = make();
    else
      ambigRoots[rnd() % ambigRootsCOUNT] = make();

    r = rnd() % exactRootsCOUNT;
    if(exactRoots[r] != objNULL)
      assert(dylan_check(exactRoots[r]));

    if(rnd() % initTestFREQ == 0)
      *(int*)busy_init = -1; /* check that the buffer is still there */

    ++i;
    if (i % 256 == 0) {
      printf(".");
      fflush(stdout);
    }
  }

  (void)mps_commit(busy_ap, busy_init, 64);
  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);

  return NULL;
}


int main(void)
{
  mps_arena_t arena;
  mps_thr_t thread;
  void *r;

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}

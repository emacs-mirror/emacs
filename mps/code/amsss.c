/* impl.c.amsss: POOL CLASS AMS STRESS TEST
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .design: Adapted from amcss.c, but not counting collections, just
 * total size of objects allocated (because epoch doesn't increment
 * when AMS is collected).
 */

#include "fmtdy.h"
#include "testlib.h"
#include "mpscams.h"
#include "mpsavm.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#include "mps.h"
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>


#define exactRootsCOUNT 50
#define ambigRootsCOUNT 100
/* This is enough for three GCs. */
#define totalSizeMAX    800 * (size_t)1024
#define totalSizeSTEP   200 * (size_t)1024
/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL         ((mps_addr_t)0xDECEA5ED)
#define testArenaSIZE   ((size_t)16<<20)
#define initTestFREQ    6000
static mps_gen_param_s testChain[1] = { { 160, 0.90 } };


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
  mps_chain_t chain;
  mps_root_t exactRoot, ambigRoot;
  size_t lastStep = 0, i, r;
  unsigned long objs;
  mps_ap_t busy_ap;
  mps_addr_t busy_init;

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(mps_fmt_create_A(&format, arena, dylan_fmt_A()), "fmt_create");
  die(mps_chain_create(&chain, arena, 1, testChain), "chain_create");
  die(mps_pool_create(&pool, arena, mps_class_ams(), format, chain),
      "pool_create(ams)");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, MPS_RANK_EXACT), "BufferCreate 2");

  for(i = 0; i < exactRootsCOUNT; ++i)
    exactRoots[i] = objNULL;
  for(i = 0; i < ambigRootsCOUNT; ++i)
    ambigRoots[i] = (mps_addr_t)rnd();

  die(mps_root_create_table_masked(&exactRoot, arena,
                                   MPS_RANK_EXACT, (mps_rm_t)0,
                                   &exactRoots[0], exactRootsCOUNT,
                                   (mps_word_t)1),
      "root_create_table(exact)");
  die(mps_root_create_table(&ambigRoot, arena,
                            MPS_RANK_AMBIG, (mps_rm_t)0,
                            &ambigRoots[0], ambigRootsCOUNT),
      "root_create_table(ambig)");

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, 64), "mps_reserve busy");

  objs = 0;
  while(totalSize < totalSizeMAX) {
    if(totalSize > lastStep + totalSizeSTEP) {
      lastStep = totalSize;
      printf("\nSize %lu bytes, %lu objects.\n",
             (unsigned long)totalSize, objs);
      fflush(stdout);
      for(i = 0; i < exactRootsCOUNT; ++i)
        cdie(exactRoots[i] == objNULL || dylan_check(exactRoots[i]),
             "all roots check");
    }

    r = (size_t)rnd();
    if(r & 1) {
      i = (r >> 1) % exactRootsCOUNT;
      if(exactRoots[i] != objNULL)
        cdie(dylan_check(exactRoots[i]), "dying root check");
      exactRoots[i] = make();
      if(exactRoots[(exactRootsCOUNT-1) - i] != objNULL)
        dylan_write(exactRoots[(exactRootsCOUNT-1) - i],
                    exactRoots, exactRootsCOUNT);
    } else {
      i = (r >> 1) % ambigRootsCOUNT;
      ambigRoots[(ambigRootsCOUNT-1) - i] = make();
      /* Create random interior pointers */
      ambigRoots[i] = (mps_addr_t)((char *)(ambigRoots[i/2]) + 1);
    }

    if(rnd() % initTestFREQ == 0)
      *(int*)busy_init = -1; /* check that the buffer is still there */

    ++objs;
    if (objs % 256 == 0) {
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
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);

  return NULL;
}


int main(int argc, char **argv)
{
  mps_arena_t arena;
  mps_thr_t thread;
  void *r;

  randomize(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}

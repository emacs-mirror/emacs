/* impl.c.amcss: POOL CLASS AMC STRESS TEST
 *
 * $HopeName: MMsrc!amcss.c(trunk.20) $
 * Copyright (C) 1996, 1998 Harlequin Group, all rights reserved
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "fmtdy.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


#define testArenaSIZE     ((size_t)64<<20)
#define exactRootsCOUNT   50
#define ambigRootsCOUNT   50
#define collectionsCOUNT  5
#define objNULL           ((mps_addr_t)0xDECEA5ED)

static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exactRoots[exactRootsCOUNT];
static mps_addr_t ambigRoots[ambigRootsCOUNT];


static mps_addr_t make(void)
{
  size_t length = rnd() % 20, size = (length+2)*sizeof(mps_word_t);
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

  return p;
}


static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_root_t exactRoot, ambigRoot;
  unsigned long i;
  mps_word_t collections;

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(mps_fmt_create_A(&format, arena, dylan_fmt_A()), "fmt_create");

  die(mps_pool_create(&pool, arena, mps_class_amc(), format),
      "pool_create(amc)");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");

  die(mps_root_create_table(&exactRoot, arena,
                            MPS_RANK_EXACT, (mps_rm_t)0,
                            &exactRoots[0], exactRootsCOUNT),
                            "root_create_table(exact)");

  die(mps_root_create_table(&ambigRoot, arena,
                            MPS_RANK_AMBIG, (mps_rm_t)0,
                            &ambigRoots[0], ambigRootsCOUNT),
                            "root_create_table(ambig)");

  for(i=0; i<exactRootsCOUNT; ++i)
    exactRoots[i] = objNULL;

  for(i=0; i<ambigRootsCOUNT; ++i)
    ambigRoots[i] = (mps_addr_t)rnd();

  collections = 0;

  i = 0;
  while(collections < collectionsCOUNT) {
    unsigned c;
    size_t r;

    c = mps_collections(arena);

    if(collections != c) {
      collections = c;
      printf("\nCollection %u, %lu objects.\n", c, i);
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

    if(i % 1000 == 0) {
      putchar('.');
      fflush(stdout);
    }

    ++i;
  }

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
      "arena_create\n");
  die(mps_thread_reg(&thread, arena), "thread_reg");
  mps_tramp(&r, test, arena, 0);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}

/* impl.c.amcss: POOL CLASS AMC STRESS TEST
 *
 * $HopeName: MMsrc!amcss.c(trunk.11) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved
 */

#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "fmtdy.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#ifdef MPS_OS_SU
#include "ossu.h"
#endif

#define NR_EXACT_ROOTS  50
#define NR_AMBIG_ROOTS  50
#define FIELDS_MAX      2000
#define OBJECTS         40000
#define OBJNULL         ((mps_addr_t)0xDECEA5ED)

static mps_pool_t pool;
static mps_ap_t ap;
static mps_addr_t exact_roots[NR_EXACT_ROOTS];
static mps_addr_t ambig_roots[NR_AMBIG_ROOTS];

static mps_addr_t make(void)
{
  size_t length = rnd() % 20, size = (length+2)*sizeof(mps_word_t);
  mps_addr_t p;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, p, ap, size);
    if(res) die(res, "MPS_RESERVE_BLOCK");
    res = dylan_init(p, size, exact_roots, NR_EXACT_ROOTS);
    if(res) die(res, "dylan_init");
  } while(!mps_commit(ap, p, size));

  return p;
}

static void *test(void *arg, size_t s)
{
  mps_space_t space;
  mps_fmt_t format;
  mps_root_t exact_root, ambig_root;
  mps_word_t i;
  mps_word_t collections;

  space = (mps_space_t)arg;
  UNUSED(s);

  die(mps_fmt_create_A(&format, space, dylan_fmt_A()), "fmt_create");

  die(mps_pool_create(&pool, space, mps_class_amc(), format),
      "pool_create(amc)");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");

  die(mps_root_create_table(&exact_root, space,
                            MPS_RANK_EXACT, (mps_rm_t)0,
                            &exact_roots[0], NR_EXACT_ROOTS),
                            "root_create_table(exact)");

  die(mps_root_create_table(&ambig_root, space,
                            MPS_RANK_AMBIG, (mps_rm_t)0,
                            &ambig_roots[0], NR_AMBIG_ROOTS),
                            "root_create_table(ambig)");

  for(i=0; i<NR_EXACT_ROOTS; ++i)
    exact_roots[i] = OBJNULL;

  for(i=0; i<NR_AMBIG_ROOTS; ++i)
    ambig_roots[i] = (mps_addr_t)rnd();

  collections = 0;

  for(i=0; i<OBJECTS; ++i) {
    unsigned c;
    size_t r;

    c = mps_collections(space);

    if(collections != c) {
      collections = c;
      printf("\nCollection %u, %lu objects.\n",
             c, (unsigned long)i);
      for(r=0; r<NR_EXACT_ROOTS; ++r)
        assert(dylan_check(exact_roots[r]));
    }

    if(rnd() & 1)
      exact_roots[rnd() % NR_EXACT_ROOTS] = make();
    else
      ambig_roots[rnd() % NR_AMBIG_ROOTS] = make();

    r = rnd() % NR_EXACT_ROOTS;
    if(exact_roots[r] != OBJNULL)
      assert(dylan_check(exact_roots[r]));
  }

  mps_ap_destroy(ap);
  mps_root_destroy(exact_root);
  mps_root_destroy(ambig_root);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);

  return NULL;
}

int main(void)
{
  mps_space_t space;
  mps_thr_t thread;
  void *r;

  die(mps_space_create(&space), "space_create");
  die(mps_thread_reg(&thread, space), "thread_reg");
  mps_tramp(&r, test, space, 0);
  mps_thread_dereg(thread);
  mps_space_destroy(space);

  return 0;
}

/* impl.c.mpsicv: MPSI COVERAGE TEST
 *
 * $HopeName: MMsrc!mpsicv.c(MMdevel_restr.2) $
 * Copyright (C) 1996 Harlequin Group, all rights reserved
 */

#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpscmv.h"
#include "fmtdy.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#define NR_EXACT_ROOTS  50
#define NR_AMBIG_ROOTS  50
#define FIELDS_MAX      2000
#define OBJECTS         4000
#define OBJNULL         ((mps_addr_t)0xDECEA5ED)

static mps_pool_t amcpool;
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

static mps_addr_t make_no_inline(void)
{
  size_t length = rnd() % 20, size = (length+2)*sizeof(mps_word_t);
  mps_addr_t p;
  mps_res_t res;

  do {
    res = (mps_reserve)(&p, ap, size);
    if(res) die(res, "(mps_reserve)");
    res = dylan_init(p, size, exact_roots, NR_EXACT_ROOTS);
    if(res) die(res, "dylan_init");
  } while(!(mps_commit)(ap, p, size));

  return p;
}

static void alloc_v_test(mps_pool_t pool, ...)
{
  void *p;
  size_t size = 32;
  va_list args;

  va_start(args, pool);
  die(mps_alloc_v(&p, pool, size, args), "alloc_v");
  va_end(args);
  mps_free(pool, p, size);
}

static void pool_create_v_test(mps_space_t space, ...)
{
  va_list args;

  va_start(args, space);
  die(mps_pool_create_v(&amcpool, space, mps_class_amc(), args),
      "pool_create_v(amc)");
  va_end(args);
}

static void ap_create_v_test(mps_pool_t pool, ...)
{
  mps_ap_t apt;
  va_list args;

  va_start(args, pool);
  die(mps_ap_create_v(&apt, pool, args), "ap_create_v");
  va_end(args);
  mps_ap_destroy(apt);
}

static mps_res_t root_single(mps_ss_t ss, void *p, size_t s)
{
  UNUSED(s);
  return mps_fix(ss, (mps_addr_t)p);
}

static void *test(void *arg, size_t s)
{
  mps_space_t space;
  mps_fmt_t format;
  mps_root_t exact_root, ambig_root;
  mps_root_t single_root, fmt_root;
  mps_word_t i;
  mps_word_t collections;
  mps_pool_t mv;
  mps_addr_t alloced_obj;
  size_t asize = 32;  /* size of alloced obj */
  mps_addr_t obj;
  mps_ld_s ld;

  space = (mps_space_t)arg;
  UNUSED(s);

  die(mps_fmt_create_A(&format, space, dylan_fmt_A()), "fmt_create");

  die(mps_pool_create(&mv, space, mps_class_mv(), 0x10000, 32, 0x10000),
      "pool_create(mv)");

  pool_create_v_test(space, format); /* creates amc pool */

  ap_create_v_test(amcpool);

  die(mps_ap_create(&ap, amcpool), "ap_create");

  for(i=0; i<NR_EXACT_ROOTS; ++i)
    exact_roots[i] = OBJNULL;

  for(i=0; i<NR_AMBIG_ROOTS; ++i)
    ambig_roots[i] = (mps_addr_t)rnd();


  die(mps_root_create_table(&exact_root, space,
                            MPS_RANK_EXACT, (mps_rm_t)0,
                            &exact_roots[0], NR_EXACT_ROOTS),
                            "root_create_table(exact)");

  die(mps_root_create_table(&ambig_root, space,
                            MPS_RANK_AMBIG, (mps_rm_t)0,
                            &ambig_roots[0], NR_AMBIG_ROOTS),
                            "root_create_table(ambig)");

  obj = OBJNULL;

  die(mps_root_create(&single_root, space,
                      MPS_RANK_EXACT, (mps_rm_t)0,
                      &root_single, &obj, 0),
                      "root_create(single)");

  /* test non-inlined reserve/commit */
  obj = make_no_inline();

  die(mps_alloc(&alloced_obj, mv, asize), "mps_alloc");

  die(dylan_init(alloced_obj, asize, exact_roots, NR_EXACT_ROOTS),
    "dylan_init(alloced_obj)");

  die(mps_root_create_fmt(&fmt_root, space,
                          MPS_RANK_EXACT, (mps_rm_t)0,
                          dylan_fmt_A()->scan,
                          alloced_obj,
                          (mps_addr_t)(((char*)alloced_obj)+asize)),
                          "root_create_fmt");

  mps_ld_reset(&ld, space);
  mps_ld_add(&ld, space, obj);

  if(mps_ld_isstale(&ld, space, obj)) {
    mps_ld_reset(&ld, space);
    mps_ld_add(&ld, space, obj);
  }

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

  mps_free(mv, alloced_obj, 32);
  alloc_v_test(mv);
  mps_pool_destroy(mv);
  mps_ap_destroy(ap);
  mps_root_destroy(fmt_root);
  mps_root_destroy(single_root);
  mps_root_destroy(exact_root);
  mps_root_destroy(ambig_root);
  mps_pool_destroy(amcpool);
  mps_fmt_destroy(format);

  return NULL;
}

int main(void)
{
  mps_space_t space;
  mps_thr_t thread;
  mps_root_t reg_root;
  void *r;
  void *marker = &marker;

  (void)mps_assert_install(mps_assert_default());
  die(mps_space_create(&space), "space_create");
  die(mps_thread_reg(&thread, space), "thread_reg");

  die(mps_root_create_reg(&reg_root, space,
                          MPS_RANK_AMBIG, (mps_rm_t)0,
                          thread, &mps_stack_scan_ambig, marker),
                          "root_create_reg");

  (mps_tramp)(&r, test, space, 0);  /* non-inlined trampoline */
  mps_tramp(&r, test, space, 0);
  mps_root_destroy(reg_root);
  mps_thread_dereg(thread);
  mps_space_destroy(space);

  return 0;
}

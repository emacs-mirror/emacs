/* impl.c.mpsicv: MPSI COVERAGE TEST
 *
 * $HopeName: MMsrc!mpsicv.c(trunk.15) $
 * Copyright (C) 1996, 1997, 1998 Harlequin Group plc.  All rights reserved.
 */

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "mpscmv.h"
#include "fmtdy.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#include "mps.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <string.h>
#include <assert.h>

#define exactRootsCOUNT  50
#define ambigRootsCOUNT  50
#define OBJECTS          4000
#define patternFREQ      100

/* objNULL needs to be odd so that it's ignored in exactRoots. */
#define objNULL         ((mps_addr_t)0xDECEA5ED)
#define FILLER_OBJECT_SIZE 1024


static mps_pool_t amcpool;
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
    if(res) die(res, "MPS_RESERVE_BLOCK");
    res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
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
    res = dylan_init(p, size, exactRoots, exactRootsCOUNT);
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


static void pool_create_v_test(mps_arena_t arena, ...)
{
  va_list args;

  va_start(args, arena);
  die(mps_pool_create_v(&amcpool, arena, mps_class_amc(), args),
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
  testlib_unused(s);
  return mps_fix(ss, (mps_addr_t *)p);
}


/* == arena_commit_test ==
 *
 * intended to test:
 *   MPS_RES_COMMIT_LIMIT
 *   mps_arena_commit_limit
 *   mps_arena_commit_limit_set
 *   mps_arena_committed
 *   mps_arena_reserved
 * incidentally tests:
 *   mps_alloc
 *   mps_class_mv
 *   mps_pool_create
 *   mps_pool_destroy
 */
static void arena_commit_test(mps_arena_t arena)
{
  mps_pool_t pool;
  size_t committed;
  size_t reserved;
  size_t limit;
  void *p;
  mps_res_t res;

  reserved = mps_arena_reserved(arena);
  committed = mps_arena_committed(arena);
  if(reserved < committed) {
    fprintf(stderr, "Error: amount returned by mps_arena_reserved is\n"
		    "less than amount returned by mps_arena_committed.\n");
    abort();
  }
  die(mps_pool_create(&pool, arena, mps_class_mv(),
		      0x1000, 1024, 16384), "commit pool create");
  limit = mps_arena_commit_limit(arena);
  mps_arena_commit_limit_set(arena, committed);
  while((res = mps_alloc(&p, pool, FILLER_OBJECT_SIZE)) == MPS_RES_OK)
    ;
  if(res != MPS_RES_COMMIT_LIMIT) {
    fprintf(stderr, "Unexpected: Allocation failed for reason other than "
		    "MPS_RES_COMMIT_LIMIT, res = %d\n", res);
  }
  mps_arena_commit_limit_set(arena, limit);
  res = mps_alloc(&p, pool, FILLER_OBJECT_SIZE);
  if(res != MPS_RES_OK) {
    fprintf(stderr, "Unexpected: Allocation failed after raising "
		    " commit_limit, res = %d\n", res);
  }
  mps_pool_destroy(pool);
}


static void *test(void *arg, size_t s)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_root_t exactRoot, ambigRoot, singleRoot, fmtRoot;
  unsigned long i;
  size_t j;
  mps_word_t collections;
  mps_pool_t mv;
  mps_addr_t alloced_obj;
  size_t asize = 32;  /* size of alloced obj */
  mps_addr_t obj;
  mps_ld_s ld;
  mps_alloc_pattern_t ramp = mps_alloc_pattern_ramp();

  arena = (mps_arena_t)arg;
  testlib_unused(s);

  die(dylan_fmt(&format, arena), "fmt_create");

  die(mps_pool_create(&mv, arena, mps_class_mv(), 0x10000, 32, 0x10000),
      "pool_create(mv)");
  
  pool_create_v_test(arena, format); /* creates amc pool */

  ap_create_v_test(amcpool);

  die(mps_ap_create(&ap, amcpool), "ap_create");

  for(j = 0; j < exactRootsCOUNT; ++j)
    exactRoots[j] = objNULL;
  for(j = 0; j < ambigRootsCOUNT; ++j)
    ambigRoots[j] = (mps_addr_t)rnd();

  die(mps_root_create_table_masked(&exactRoot, arena,
                                   MPS_RANK_EXACT, (mps_rm_t)0,
                                   &exactRoots[0], exactRootsCOUNT,
                                   (mps_word_t)1),
      "root_create_table(exact)");
  die(mps_root_create_table(&ambigRoot, arena,
                            MPS_RANK_AMBIG, (mps_rm_t)0,
                            &ambigRoots[0], ambigRootsCOUNT),
      "root_create_table(ambig)");

  obj = objNULL;

  die(mps_root_create(&singleRoot, arena,
                      MPS_RANK_EXACT, (mps_rm_t)0,
                      &root_single, &obj, 0),
      "root_create(single)");

  /* test non-inlined reserve/commit */
  obj = make_no_inline();

  die(mps_alloc(&alloced_obj, mv, asize), "mps_alloc");
  die(dylan_init(alloced_obj, asize, exactRoots, exactRootsCOUNT),
      "dylan_init(alloced_obj)");

  die(mps_root_create_fmt(&fmtRoot, arena,
                          MPS_RANK_EXACT, (mps_rm_t)0,
                          dylan_fmt_A()->scan,
                          alloced_obj,
                          (mps_addr_t)(((char*)alloced_obj)+asize)),
      "root_create_fmt");

  mps_ld_reset(&ld, arena);
  mps_ld_add(&ld, arena, obj);

  if(mps_ld_isstale(&ld, arena, obj)) {
    mps_ld_reset(&ld, arena);
    mps_ld_add(&ld, arena, obj);
  }

  collections = mps_collections(arena);

  for(i = 0; i < OBJECTS; ++i) {
    unsigned c;
    size_t r;

    c = mps_collections(arena);

    if(collections != c) {
      collections = c;
      printf("\nCollection %u, %lu objects.\n", c, i);
      for(r = 0; r < exactRootsCOUNT; ++r)
        assert(exactRoots[r] == objNULL || dylan_check(exactRoots[r]));
    }

    if(rnd() % patternFREQ == 0)
      switch(rnd() % 4) {
      case 0: case 1: mps_ap_alloc_pattern_begin(ap, ramp); break;
      case 2: mps_ap_alloc_pattern_end(ap, ramp); break;
      case 3: mps_ap_alloc_pattern_reset(ap); break;
      }

    if(rnd() & 1)
      exactRoots[rnd() % exactRootsCOUNT] = make();
    else
      ambigRoots[rnd() % ambigRootsCOUNT] = make();

    r = rnd() % exactRootsCOUNT;
    if(exactRoots[r] != objNULL)
      assert(dylan_check(exactRoots[r]));
  }

  arena_commit_test(arena);

  mps_arena_collect(arena);

  mps_free(mv, alloced_obj, 32);
  alloc_v_test(mv);
  mps_pool_destroy(mv);
  mps_ap_destroy(ap);
  mps_root_destroy(fmtRoot);
  mps_root_destroy(singleRoot);
  mps_root_destroy(exactRoot);
  mps_root_destroy(ambigRoot);
  mps_pool_destroy(amcpool);
  mps_fmt_destroy(format);

  return NULL;
}


#define TEST_ARENA_SIZE              ((size_t)16<<20)


int main(void)
{
  mps_arena_t arena;
  mps_thr_t thread;
  mps_root_t reg_root;
  void *r;
  void *marker = &marker;

  (void)mps_assert_install(mps_assert_default());
  die(mps_arena_create(&arena, mps_arena_class_vm(), TEST_ARENA_SIZE),
      "arena_create");
  die(mps_thread_reg(&thread, arena), "thread_reg");

  die(mps_root_create_reg(&reg_root, arena,
                          MPS_RANK_AMBIG, (mps_rm_t)0,
                          thread, &mps_stack_scan_ambig,
                          marker, (size_t)0),
      "root_create_reg");

  (mps_tramp)(&r, test, arena, 0);  /* non-inlined trampoline */
  mps_tramp(&r, test, arena, 0);
  mps_root_destroy(reg_root);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);

  fprintf(stderr, "\nConclusion:  Failed to find any defects.\n");
  return 0;
}

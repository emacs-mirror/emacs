/* $HopeName$
TEST_HEADER
 summary = Collect with a fully initialised (but not committed) buffer
 language = c
 link = testlib.o
END_HEADER
 * Copyright (C) 1998 Harlequin Group, all rights reserved
 */

#include <stdio.h>

#include "testlib.h"
#include "mps.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "mpstd.h"
#ifdef MPS_OS_W3
#include "mpsw3.h"
#endif
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


#define testArenaSIZE     ((size_t)64<<20)
/* objSIZE should be such that when this size is requested in a reserve */
/* the buffer gets filled with exactly this much memory */
#define objSIZE           8192

static mps_ap_t ap;

static mps_res_t simple_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  return MPS_RES_OK;
}

static mps_addr_t simple_skip(mps_addr_t limit)
{
  return (void *)((char *)limit + objSIZE);
}

static void simple_fwd(mps_addr_t old, mps_addr_t new)
{
  ((mps_addr_t *)old)[1] = new;
  ((mps_word_t *)old)[0] = 1;
}

static mps_addr_t simple_is_fwd(mps_addr_t obj)
{
  if(*(mps_word_t *)obj) {
    return ((mps_addr_t *)obj)[1];
  } else {
    return NULL;
  }
}

static void simple_pad(mps_addr_t addr, size_t size)
{
}

struct mps_fmt_A_s simple_fmt_A = {
  4,
  &simple_scan,
  &simple_skip,
  NULL,
  &simple_fwd,
  &simple_is_fwd,
  &simple_pad
};


static mps_addr_t make(void)
{
  size_t size = objSIZE;
  mps_addr_t p;
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, p, ap, size);
    if(res)
      die(res, "MPS_RESERVE_BLOCK");
    *(mps_word_t *)p = 0;
  } while(!mps_commit(ap, p, size));

  return p;
}

static void test_stepper(mps_addr_t object, void *p, size_t s)
{
  (*(unsigned long *)p)++;
  testlib_unused(s);
  testlib_unused(object);
}


static void *test(void *arg, size_t s)
{
  mps_addr_t busy_init;
  mps_ap_t busy_ap;
  mps_arena_t arena;
  mps_fmt_t format;
  mps_pool_t pool;
  unsigned long i;

  arena = (mps_arena_t)arg;
  (void)s; /* unused */

  die(mps_fmt_create_A(&format, arena, &simple_fmt_A), "fmt_create");

  die(mps_pool_create(&pool, arena, mps_class_amc(), format),
      "pool_create(amc)");

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, MPS_RANK_EXACT), "BufferCreate");

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, objSIZE), "mps_reserve busy");

  /* now simulate first part of commit */
  busy_ap->init = busy_ap->alloc;

  for(i = 0; i < 100000; ++i) {
    make();
  }

  /* now simulate rest of commit */
  busy_ap->limit != 0 || mps_ap_trip(busy_ap, busy_init, objSIZE);

  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);
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

/* 
TEST_HEADER
 id = $Id$
 summary = Collect with a fully initialised (but not committed) buffer
 language = c
 link = testlib.o
END_HEADER
 */

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

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

static void simple_copy(mps_addr_t obj, mps_addr_t to)
{
 error("copy method not implemented in this test");
}


struct mps_fmt_A_s simple_fmt_A = {
  4,
  &simple_scan,
  &simple_skip,
  &simple_copy,
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


static void test(void)
{
  mps_addr_t busy_init;
  mps_ap_t busy_ap;
  mps_arena_t arena;
  mps_thr_t thread;
  mps_fmt_t format;
  mps_chain_t chain;
  mps_pool_t pool;
  unsigned long i;

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "arena_create");

  die(mps_thread_reg(&thread, arena), "thread_reg");

  die(mps_fmt_create_A(&format, arena, &simple_fmt_A), "fmt_create");
  die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

  die(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
      "create pool");

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");
  die(mps_ap_create(&busy_ap, pool, mps_rank_exact()), "BufferCreate");

  /* create an ap, and leave it busy */
  die(mps_reserve(&busy_init, busy_ap, objSIZE), "mps_reserve busy");

  /* now simulate first part of commit */
  busy_ap->init = busy_ap->alloc;

  for(i = 0; i < 100000; ++i) {
    make();
  }

  /* now simulate rest of commit */
  (void)(busy_ap->limit != 0 || mps_ap_trip(busy_ap, busy_init, objSIZE));

  mps_ap_destroy(busy_ap);
  mps_ap_destroy(ap);
  mps_pool_destroy(pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);
  mps_thread_dereg(thread);
  mps_arena_destroy(arena);
}


int main(void)
{
  easy_tramp(test);

  pass();
  return 0;
}

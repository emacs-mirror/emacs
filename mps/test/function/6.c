/* 
TEST_HEADER
 id = $Id$
 summary = (regression test ) Keep resetting lds in managed memory while doing allocation. The idea is to force a collection so that the ld will be protected when you try to reset it.
 language = c
 link = testlib.o newfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "newfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;

 mycell *a, *b, *c;
 mycell *p;
 mps_ld_t ld;

 int i, j;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
      "create pool");

 cdie(mps_ap_create(&ap, pool, mps_rank_exact()), "create ap");

  a = allocone(ap, 100);
  b = a;
  c = a;

 for (j = 1; j < 100; j++) {
  comment("%i of 100", j);
  p = allocdumb(ap, sizeof(mycell));
  ld = (mps_ld_t) getdata(p);
 
  b = a;
  c = a;

  for (i = 1; i < 100; i++) {
    mps_ld_reset(ld, arena);

    UC;
    c = allocone(ap, 200);
    UC;
    setref(b, 0, c);
    UC;
    b = c;
  }
 }

 mps_arena_park(arena);
 mps_arena_park(arena);
 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 run_test(test);
 pass();
 return 0;
}

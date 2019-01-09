/* 
TEST_HEADER
 id = $Id$
 summary = test running out of memory while scanning roots
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define MEG (1024uL*1024uL)
#define THIRTY_MEG (30uL*MEG)

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


static void test(void *stack_pointer)
{
 int j;
 mps_ap_t ap;
 mps_arena_t arena;
 mps_fmt_t format;
 mps_chain_t chain;
 mps_pool_t pool;
 mps_root_t root1;
 mps_thr_t thread;

 /* create an arena that can't grow beyond 30 MB */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)THIRTY_MEG),
      "create arena");
 cdie(mps_arena_commit_limit_set(arena, (size_t)THIRTY_MEG),
      "commit limit set");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_table(&root1, arena, mps_rank_exact(), 0,
                            (mps_addr_t*)&exfmt_root, 1),
      "create table root");

 cdie(mps_fmt_create_A(&format, arena, &fmtA),
      "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
     "create pool");

 cdie(mps_ap_create(&ap, pool, mps_rank_exact()),
      "create ap");

 /* allocate a 16 MB live object */

 allocdumb(ap, 16*MEG, mps_rank_exact());

 comment("collect world...");

 for (j=0; j<1000; j++) {
  mps_arena_collect(arena);
 }

 mps_arena_park(arena);
 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_root_destroy(root1);
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

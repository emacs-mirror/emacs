/* 
TEST_HEADER
 id = $Id$
 summary = weak refs to nailed amc objects are not splatted
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "rankfmt.h"
#include "mpsavm.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t poolamc, poolawl;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t apamc, apawl, apweak;

 mycell *a, *b;

 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 die(mps_thread_reg(&thread, arena), "register thread");
 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&poolamc, arena, mps_class_amc(), format, chain),
     "create pool(amc)");

 cdie(mps_pool_create(&poolawl, arena, mps_class_awl(), format, getassociated),
      "create pool(awl)");

 cdie(mps_ap_create(&apweak, poolawl, mps_rank_weak()),
      "create ap(weak)");

 cdie(mps_ap_create(&apawl, poolawl, mps_rank_exact()),
      "create ap(awl)");
 
 cdie(mps_ap_create(&apamc, poolamc, mps_rank_exact()),
      "create ap(amc)");

 b = allocone(apamc, 1, mps_rank_exact());
 a = allocone(apweak, 1, mps_rank_weak());

 mps_arena_park(arena);
 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 mps_ap_destroy(apweak);
 comment("Destroyed aps.");
 /* buffered segments aren't condemned! */

 setref(a, 0, b);
 mps_arena_collect(arena);
 asserts(getref(a, 0) == b, "Reference changed or was splatted.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
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

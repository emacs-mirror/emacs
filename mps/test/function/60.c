/* 
TEST_HEADER
 id = $Id$
 summary = loops an AMC and an AWL pool
 language = c
 link = testlib.o awlfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpscawl.h"
#include "awlfmt.h"

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t poolamc1, poolawl2;
 mps_thr_t thread;
 mps_root_t root;

 mps_chain_t chain;
 mps_fmt_t format;
 mps_ap_t ap1, ap2;

 mycell *a, *b;

 int i;
 int j;

 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(
  mps_pool_create(&poolamc1, arena, mps_class_amc(), format, chain),
  "create pool");

 cdie(
  mps_pool_create(&poolawl2, arena, mps_class_awl(), format, getassociated),
  "create pool");

 cdie(
  mps_ap_create(&ap1, poolamc1, mps_rank_exact()),
  "create ap");

 cdie(
  mps_ap_create(&ap2, poolawl2, mps_rank_exact()),
  "create ap");

 for (j=1; j<=10; j++)
 {
  comment("%i of 10.", j);

  for (i=1; i<=1000; i++)
  {
  UC;
   a = allocone(ap1, 50, 1);
   b = allocone(ap2, 50, 1);
   setref(a, 0, b);
   setref(b, 0, a);
  UC;
  }
 DC;
 DMC;
 }

 mps_arena_park(arena);
 mps_ap_destroy(ap1);
 mps_ap_destroy(ap2);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc1);
 mps_pool_destroy(poolawl2);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_chain_destroy(chain);
 comment("Destroyed chain.");

 mps_root_destroy(root);
 comment("Destroyed root.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");

}

int main(void)
{
 run_test(test);
 pass();
 return 0;
}

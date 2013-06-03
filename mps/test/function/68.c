/* 
TEST_HEADER
 id = $Id$
 summary = keep calling arena_collect
 language = c
 link = testlib.o exfmt.o
 manual = true
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t poolamc;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_chain_t chain;
 mps_fmt_t format;
 mps_ap_t apamc;

 size_t size0, size1;
 mycell *a, *b;

 int i;
 int j;

 RC;

 deathcomments = 0;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1, arena, mps_rank_ambig(), 0, &exfmt_root, 1),
  "create exfmt root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(
  mps_pool_create(&poolamc, arena, mps_class_amc(), format, chain),
  "create pool");

 cdie(
  mps_ap_create(&apamc, poolamc, mps_rank_exact()),
  "create ap");

 b = allocone(apamc, 1, 1);

 for (j=1; j<500; j++) {
  a = allocone(apamc, 1000, 1);
  setref(a, 0, b);
  b = a;
 }

 comment("Collecting");

 size0 = mps_arena_committed(arena);

 while(1) {
   mps_arena_collect(arena);
   size1 = mps_arena_committed(arena);
   report("diff", "%lu", (unsigned long) (size1-size0));
 }

 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_chain_destroy(chain);
 comment("Destroyed chain.");

 mps_root_destroy(root);
 mps_root_destroy(root1);
 comment("Destroyed roots.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

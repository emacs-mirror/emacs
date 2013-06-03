/* 
TEST_HEADER
 id = $Id$
 summary = loops wthin an AMC pool
 language = c
 link = testlib.o awlfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "awlfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t poolamc1, poolamc2;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap1, ap2;

 mycell *a, *b;

 int i;
 int j;

 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 die(mps_thread_reg(&thread, arena), "register thread");
 die(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
                         mps_stack_scan_ambig, stackpointer, 0),
     "create root");

 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&poolamc1, arena, mps_class_amc(), format, chain),
     "create pool(1)");
 die(mmqa_pool_create_chain(&poolamc2, arena, mps_class_amc(), format, chain),
     "create pool(2)");

 cdie(
  mps_ap_create(&ap1, poolamc1, mps_rank_exact()),
  "create ap");

 cdie(
  mps_ap_create(&ap2, poolamc2, mps_rank_exact()),
  "create ap");

 for (j = 1; j < 100; j++) {
  comment("%i of 100.", j);

  for (i = 1; i < 10000; i++) {
   UC;
   a = allocone(ap1, 2, 1);
   b = allocone(ap1, 2, 1);
   setref(a, 0, b);
   setref(b, 0, a);
   UC;
  }
  DC;
  DMC;
 }

 mps_ap_destroy(ap1);
 mps_ap_destroy(ap2);
 mps_pool_destroy(poolamc1);
 mps_pool_destroy(poolamc2);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

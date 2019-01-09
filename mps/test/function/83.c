/* 
TEST_HEADER
 id = $Id$
 summary = test for bug with segment summaries
 language = c
 link = testlib.o awlfmt.o
END_HEADER
*/

/*
This bug, suggested by drj, turned out not to exist. The problem
would have occurred when a nailed, buffered segment was scanned,
and the summary would be wrongly set.  But in fact all buffered
segments are scanned in their entirety anyway, so no problem
arises.
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpscawl.h"
#include "awlfmt.h"
#include "mpsavm.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


mps_addr_t temp_root;


static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool1, pool2;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap1, ap2, ap;

 mycell *a, *b = NULL, *c, *d;

 int i;

 alloccomments = 1;
 fixcomments = 1;
 deathcomments = 1;
 formatcomments = 1;
 fixcomments = 1;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 die(mps_thread_reg(&thread, arena), "register thread");

 die(mps_root_create_table(&root, arena, mps_rank_ambig(), 0, &temp_root, 1),
     "create temp root");

 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&pool1, arena, mps_class_amc(), format, chain),
     "create pool(amc)");

 die(mps_pool_create(&pool2, arena, mps_class_awl(), format, getassociated),
     "create pool(awl)");

 die(mps_ap_create(&ap1, pool1, mps_rank_exact()),
     "create ap(amc)");

 die(mps_ap_create(&ap2, pool2, mps_rank_exact()),
     "create ap(awl)");

 ap=ap1;

 mps_arena_park(arena);
 
 c = allocone(ap, 1, 1);

 for (i=0; i<20; i++) {
  if (i==10) {
   comment("b...");
   b = allocone(ap, 1, 1);
  }
  a = allocone(ap, 1, 1);
  setref(a, 0, c);
  if (i==10) {
   comment("switch ap");
   ap = ap2;
  }
  d = allocone(ap, 1000, 1);
  c = a;
 }
 setref(b, 0, c);

 temp_root = NULL;

 mps_arena_collect(arena);
 mps_arena_release(arena);

 report("d", "%p", d);


 mps_arena_park(arena);
 mps_ap_destroy(ap1);
 mps_ap_destroy(ap2);
 mps_pool_destroy(pool1);
 mps_pool_destroy(pool2);
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

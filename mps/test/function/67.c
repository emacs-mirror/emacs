/* 
TEST_HEADER
 id = $Id$
 summary = test for non-bug with mark-sweep scanning in AWL
 language = c
 link = testlib.o exfmt.o
END_HEADER

 I wrote this test with drj when we were convinced there was
 a bug in pool scanning because of ScanStateSetSummary setting
 unfixed to be potentially bigger than the segment summary.  But
 it turned out not to be a problem, 'cos ScanStateSetSummary is
 called (at least by AWL) _before_ scanning is done.
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "exfmt.h"
#include "mpsavm.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t poolamc, poolawl;
 mps_thr_t thread;
 mps_root_t root, root1, root2;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t apamc, apawl;

 mycell *a[3];

 int j;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 die(mps_thread_reg(&thread, arena), "register thread");
 die(mps_root_create_table(&root, arena, mps_rank_exact(), 0,
                           (mps_addr_t*)&a[0], 2),
     "create table root");

 cdie(
  mps_root_create_table(&root2, arena, mps_rank_ambig(), 0, (mps_addr_t*)&a[2], 1),
  "ambig table root");

 cdie(
  mps_root_create_table(&root1, arena, mps_rank_ambig(), 0, &exfmt_root, 1),
  "create exfmt root");

 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&poolamc, arena, mps_class_amc(), format, chain),
     "create pool");

 cdie(
  mps_pool_create(&poolawl, arena, mps_class_awl(), format, getassociated),
  "create pool");

 cdie(
  mps_ap_create(&apawl, poolawl, mps_rank_exact()),
  "create ap");

 cdie(
  mps_ap_create(&apamc, poolamc, mps_rank_exact()),
  "create ap");

 a[1] = allocone(apawl, 2, 1);
 a[0] = allocone(apawl, 2, 1);

 a[2] = allocone(apamc, 2, 1);
 setref(a[0], 0, a[2]);

 mps_ap_destroy(apawl);
 comment("Destroyed awl ap");

 for(j=0; j<100; j++) {
  allocdumb(apamc, 1024*256, 1);
 }

 mps_ap_destroy(apamc);
 comment("Destroyed amc ap.");

 mps_root_destroy(root1);
 mps_root_destroy(root2);
 comment("Destroyed ambiguous roots.");

 a[1] = a[0];
 comment("Now to try arena_collect:");
 mps_arena_collect(arena);

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
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary =  check tagged roots are scanned correctly
 language = c
 link = testlib.o exfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "exfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;
 mps_root_t root2;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;

 mycell *z[100];

 int i;

 formatcomments = 1;
 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 die(mps_root_create_table_masked(&root, arena, mps_rank_exact(),
                                  0, (mps_addr_t*)&z[0], 100, 0x4),
     "create table root");
 die(mps_root_create_table(&root2, arena, mps_rank_ambig(), 0, &exfmt_root, 1),
     "create exfmt root");

 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
     "create pool(amc)");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 for (i=0; i<100; i++) {
  comment("%i of 10.", i);
  UC;
  z[i] = allocone(ap, 1, 1);
  if (i % 8 == 0) { z[i] = (mycell *) ((int)z[i] + 4); } /* error to scan this! */
 }

 for (i=0; i<1000; i++) {
   z[0] = allocone(ap, 1000, 0);
 }
 DC;
 DMC;

 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_root_destroy(root2);
 comment("Destroyed roots.");
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

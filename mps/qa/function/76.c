/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!76.c(trunk.4) $
 summary = destroy space when messages are on the queue
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "mpsclo.h"
#include "rankfmt.h"


void *stackpointer;

mps_arena_t arena;

int final_count = 0;


static void test(void) {
 mps_pool_t poolamc, poolawl, poollo;
 mps_thr_t thread;
 mps_root_t root0, root1;

 mps_fmt_t format;
 mps_ap_t apamc, apawl, aplo;

 mycell *a, *b, *c, *d;

 long int j;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)1024*1024*30),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_reg(&root0, arena, MPS_RANK_AMBIG, 0, thread,
                          mps_stack_scan_ambig, stackpointer, 0),
      "create root");
 
 cdie(mps_root_create_table(&root1, arena, MPS_RANK_AMBIG, 0,
                            (mps_addr_t*)&exfmt_root, 1),
      "create table root");

 cdie(mps_fmt_create_A(&format, arena, &fmtA),
      "create format");

 cdie(mps_pool_create(&poolamc, arena, mps_class_amc(), format),
      "create pool");

 cdie(mps_pool_create(&poolawl, arena, mps_class_awl(), format),
      "create pool");

 cdie(mps_pool_create(&poollo, arena, mps_class_lo(), format),
      "create pool");

 cdie(mps_ap_create(&apawl, poolawl, MPS_RANK_WEAK),
      "create ap");

 cdie(mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
      "create ap");
 
 cdie(mps_ap_create(&aplo, poollo, MPS_RANK_EXACT),
      "create ap");

 mps_message_type_enable(arena, mps_message_type_finalization());

 /* register loads of objects for finalization (1000*4) */

 a = allocone(apamc, 2, 1);
 b = a;

 for (j=0; j<1000; j++) {
  a = allocone(apamc, 2, MPS_RANK_EXACT);
  c = allocone(apawl, 2, MPS_RANK_WEAK);
  d = allocone(aplo, 2, MPS_RANK_EXACT); /* rank irrelevant here! */
  mps_finalize(arena, (mps_addr_t*)&a);
  mps_finalize(arena, (mps_addr_t*)&c);
  mps_finalize(arena, (mps_addr_t*)&d);
  mps_finalize(arena, (mps_addr_t*)&d);
  final_count += 4;
 }

 /* throw them all away and collect everything */

 a = NULL;
 b = NULL;
 c = NULL;
 d = NULL;

 mps_root_destroy(root0);
 mps_root_destroy(root1);
 comment("Destroyed roots.");

 mps_arena_collect(arena);

 while(mps_message_poll(arena) == 0) {
  a = allocdumb(apawl, 1024, MPS_RANK_WEAK);
  a = allocdumb(apamc, 1024, MPS_RANK_EXACT);
  a = allocdumb(aplo,  1024, MPS_RANK_EXACT);
  mps_arena_collect(arena);
 }

 /* how many are left? (n.b. ideally this would be 0 but
    there's no guarantee)
    */

 /* now to test leaving messages open for a long time! */

 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 mps_ap_destroy(aplo);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 mps_pool_destroy(poollo);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void) {
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $HopeName$
 summary = test for non-bug with mark-sweep scanning in AWL
 language = c
 link = testlib.o exfmt.o
END_HEADER

 I wrote this test with drj when we were convinced there was
 a bug in pool scanning because of ScanStateSetSummary setting
 unfixed to be potentially bigger than the segment summary. But
 it turned out not to be a problem, 'cos ScanStateSetSummary is
 called (at least by AWL) _before_ scanning is done.

*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "exfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolamc, poolawl;
 mps_thr_t thread;
 mps_root_t root, root1, root2;

 mps_fmt_t format;
 mps_ap_t apamc, apawl;

 mycell *a[3];

 int j;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_table(&root, space, MPS_RANK_EXACT, 0, &a[0], 2),
  "create table root");

 cdie(
  mps_root_create_table(&root2, space, MPS_RANK_AMBIG, 0, &a[2], 1),
  "ambig table root");

 cdie(
  mps_root_create_table(&root1, space, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "create exfmt root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&poolamc, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_pool_create(&poolawl, space, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_ap_create(&apawl, poolawl, MPS_RANK_EXACT),
  "create ap");

 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
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
 mps_arena_collect(space);

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 comment("Destroyed exact roots.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_space_destroy(space);
 comment("Destroyed space.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

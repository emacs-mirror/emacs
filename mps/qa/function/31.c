/* 
TEST_HEADER
 id = $HopeName$
 summary = regression test for request.dylan.170459 (scanning and buffers in AWL)
 language = c
 link = testlib.o awlfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "awlfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolamc, poolawl;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t apamc, apawl;

 mycell *a, *b, *c;

 int i;
 int j;

 alloccomments = 1;
 fixcomments = 1;
 deathcomments = 1;
 formatcomments = 1;
 fixcomments = 1;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

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

 a = allocone(apawl, 1, 1);
 b = allocone(apawl, 1, 1);
 c = allocone(apawl, 1000, 1);

 setref(b, 0, c);
 setref(c, 0, b);

 for(i=0; i<1000; i++) {
  b = allocdumb(apamc, 0x400*1024, 0);
  comment("%i of 1000.", j);
 }

 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 comment("Destroyed root.");

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

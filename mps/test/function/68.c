/* 
TEST_HEADER
 id = $HopeName$
 summary = keep calling arena_collect
 language = c
 link = testlib.o exfmt.o
 manual = true
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolamc;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_ap_t apamc;

 size_t size0, size1;
 mycell *a, *b;

 int i;
 int j;

 RC;

 deathcomments = 0;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

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
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");

 b = allocone(apamc, 1, 1);

 for (j=1; j<500; j++) {
  a = allocone(apamc, 1000, 1);
  setref(a, 0, b);
  b = a;
 }

 comment("Collecting");

 size0 = mps_arena_committed(space);

 while(1) {
   mps_arena_collect(space);
   size1 = mps_arena_committed(space);
   report("diff", "%lu", (unsigned long) (size1-size0));
 }

 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 mps_root_destroy(root1);
 comment("Destroyed roots.");

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
 return 0;
}

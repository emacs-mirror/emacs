/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!83.c(trunk.2) $
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


void *stackpointer;

mps_addr_t temp_root;


static void test(void)
{
 mps_space_t space;
 mps_pool_t pool1, pool2;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap1, ap2, ap;

 mycell *a, *b = NULL, *c, *d;

 int i;

 alloccomments = 1;
 fixcomments = 1;
 deathcomments = 1;
 formatcomments = 1;
 fixcomments = 1;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_table(&root, space, MPS_RANK_AMBIG, 0, &temp_root, 1),
  "create temp root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool1, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_pool_create(&pool2, space, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap1, pool1, MPS_RANK_EXACT),
  "create ap");

 cdie(
  mps_ap_create(&ap2, pool2, MPS_RANK_EXACT),
  "create ap");

 ap=ap1;

 mps_arena_park(space);
 
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

 mps_arena_collect(space);

 mps_arena_release(space);

 report("d", "%p", d);


 mps_ap_destroy(ap1);
 mps_ap_destroy(ap2);
 comment("Destroyed aps.");

 mps_pool_destroy(pool1);
 mps_pool_destroy(pool2);
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

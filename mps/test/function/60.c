/* 
TEST_HEADER
 id = $HopeName$
 summary = loops an AMC and an AWL pool
 language = c
 link = testlib.o awlfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpscawl.h"
#include "awlfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolamc1, poolawl2;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap1, ap2;

 mycell *a, *b;

 int i;
 int j;

 RC;

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
  mps_pool_create(&poolamc1, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_pool_create(&poolawl2, space, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap1, poolamc1, MPS_RANK_EXACT),
  "create ap");

 cdie(
  mps_ap_create(&ap2, poolawl2, MPS_RANK_EXACT),
  "create ap");

 for (j=1; j<100; j++)
 {
  comment("%i of 100.", j);

  for (i=1; i<10000; i++)
  {
  UC;
   a = allocone(ap1, 100, 1);
   b = allocone(ap2, 100, 1);
   setref(a, 0, b);
   setref(b, 0, a);
  UC;
  }
 DC;
 DMC;
 }

 mps_ap_destroy(ap1);
 mps_ap_destroy(ap2);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc1);
 mps_pool_destroy(poolawl2);
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

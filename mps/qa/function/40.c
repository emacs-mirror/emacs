/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!40.c(trunk.4) $
 summary =  check tagged roots are scanned correctly
 language = c
 link = testlib.o exfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

void *stackpointer;


static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;
 mps_root_t root2;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *z[100];

 int i;

 formatcomments = 1;


 RC;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

/*
 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");
*/

 cdie(
  mps_root_create_table_masked(&root, space, MPS_RANK_EXACT,
   0, (mps_addr_t*)&z[0], 100, 0x4),
  "create table root");

 cdie(
  mps_root_create_table(&root2, space, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "create exfmt root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

 for (i=0; i<100; i++) {
  comment("%i of 10.", i);
  UC;
  z[i] = allocone(ap, 1, 1);
  if (i % 8 == 0) { z[i] = (mycell *) ((int) z[i] + 4); } /* error to scan this! */
 }

 for (i=0; i<1000; i++) {
   z[0] = allocone(ap, 1000, 0);
 }
 DC;
 DMC;

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 mps_root_destroy(root2);
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
 pass();
 return 0;
}

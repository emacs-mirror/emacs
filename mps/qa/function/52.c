/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!52.c(trunk.4) $
 summary = provoke mad behaviour by constant allocation
 language = c
 link = rankfmt.o testlib.o
END_HEADER
*/

/* this is based on MMQA_test_function!4.c, but with no user-input
*/

#include <time.h>
#include "testlib.h"
#include "mpscamc.h"
#include "rankfmt.h"

void *stackpointer;


static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a[3];

 int i,j,k;
 clock_t time0, time1;

 formatcomments = 1;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_table(&root, space, MPS_RANK_AMBIG, 0, (mps_addr_t*)&a[0], 3),
  "create table root");
 
 cdie(
  mps_root_create_table(&root1, space, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "exfmt root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

 time0 = clock();
 asserts(time0 != -1, "processor time not available");

 for (k=0; k<100; k++)
 {

  for (j=0; j<100; j++)
  {
   a[0] = allocone(ap, 50, MPS_RANK_EXACT);
   a[1] = a[0];

   for (i=1; i<100; i++)
   {
     a[2] = allocone(ap, 50, MPS_RANK_EXACT);
     setref(a[1], 0, a[2]);
     a[1] = a[2];
   }
  }

  time1 = clock();
  comment("%d: %i", k, (int) (100*(time1-time0)/CLOCKS_PER_SEC));
  time0 = time1;
 }

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

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
 pass();
 return 0;
}

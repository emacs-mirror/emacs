/* 
TEST_HEADER
 id = $HopeName$
 summary = ensure allocation in MV pool causes collection
 language = c
 link = newfmt.o testlib.o
OUTPUT_SPEC
 diff23 < 5
END_HEADER
*/

/* this is same as test 3.c, with input of 0x4000
   and a limit of 100 not 10000 iterations
*/

#include "testlib.h"
#include "mpscmv.h"
#include "mpscamc.h"
#include "newfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolMV, poolAMC;
 mps_thr_t thread;

 mps_fmt_t format;
 mps_ap_t ap;
 mps_res_t r;

 mycell *a;
 mps_addr_t p;

 int i;
 int s1, s2, s3;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 formatcomments = 0;

 cdie(
  mps_pool_create(&poolAMC, space, mps_class_amc(), format),
  "create AMC pool");

 cdie(
  mps_ap_create(&ap, poolAMC, MPS_RANK_EXACT),
  "create ap");

 comment("Sizes in megabytes:");

 die(
  mps_pool_create(&poolMV, space, mps_class_mv(),
   1024*128, 1024*64, 1024*1024),
  "create MV pool");
 i=0;
 while ((r=mps_alloc(&p, poolMV, 1024*1024)) == 0) i++;
 report("refuse1", "%s", err_text(r));
 report("size1", "%i", i);
 s1 = i;
 mps_pool_destroy(poolMV);

 die(
  mps_pool_create(&poolMV, space, mps_class_mv(),
   1024*128, 1024*64, 1024*1024),
  "create MV pool");
 i=0;
 while ((r=mps_alloc(&p, poolMV, 1024*1024)) == 0) i++;
 report("refuse2", "%s", err_text(r));
 report("size2", "%i", i);
 s2 = i;
 mps_pool_destroy(poolMV);

 a = allocdumb(ap, 1024*1024*30); /* allocate 30 M object */

 die(
  mps_pool_create(&poolMV, space, mps_class_mv(),
   1024*128, 1024*64, 1024*1024),
  "create MV pool");
 i=0;
 while ((r=mps_alloc(&p, poolMV, 1024*1024)) == 0) i++;
 report("refuse4", "%s", err_text(r));
 report("size3", "%i", i);
 s3 = i;

 report("diff12", "%i", s1-s2);
 report("diff23", "%i", s2-s3);

 for(i=0; i<10; i++) {
  r=mps_alloc(&p, poolMV, 1024*1024);
  report("refuse4", "%s", err_text(r));
 }
 mps_pool_destroy(poolMV);

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(poolAMC);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

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

/* 
TEST_HEADER
 id = $HopeName$
 summary = try to provoke request.dylan.170463 using AMCZ pool
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
 mps_pool_t poolamc, poollo;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t apamc, aplo;

 mycell *a[100], *b;

 int i;
 int j;
 int k,z;

 alloccomments = 1;
 formatcomments = 1;

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
  mps_pool_create(&poollo, space, mps_class_amcz(), format),
  "create pool");

 cdie(
  mps_ap_create(&aplo, poollo, MPS_RANK_EXACT),
  "create ap");

 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");

 for(i=0; i<100; i++) {
  a[i] = allocone(aplo, 6, 1);
 }

 for(i=0; i<10000; i++) {
  j = ranint(100);
  comment("New object %i", j);
  a[j] = allocone(aplo, 5+ranint(50), 1);
  k = ranint(50);
  z = ranint(5);
  comment("setting %i (%p) %i", k, a[k], z);
 setref(a[k], z, a[j]);
  b = allocdumb(apamc, 0x400*64, 0);
 }

 mps_ap_destroy(aplo);
 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poollo);
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

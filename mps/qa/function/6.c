/* 
TEST_HEADER
 id = $HopeName$
 summary = (regression test ) Keep resetting lds in managed memory while doing allocation. The idea is to force a collection so that the ld will be protected when you try to reset it.
 language = c
 link = testlib.o newfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "newfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a, *b, *c;
 mycell *p;
 mps_ld_t ld;

 int i, j;

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
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

  a = allocone(ap, 100);
  b = a;
  c = a;

 for (j=1; j<100; j++)
 {
  comment("%i of 100", j);
  p = allocdumb(ap, sizeof(mps_ld_s));
  ld = (mps_ld_t) getdata(p);
 
  b = a;
  c = a;

  for (i=1; i<100; i++)
  {
  mps_ld_reset(ld, space);

  UC;
   c = allocone(ap, 200);
  UC;
   setref(b, 0, c);
  UC;
   b = c;
  }
 }

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

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

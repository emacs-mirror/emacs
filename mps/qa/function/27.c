/* 
TEST_HEADER
 id = $HopeName$
 summary = mutually referring objects should die
 language = c
 link = testlib.o newfmt.o
END_HEADER
*/


#include "testlib.h"
#include "mpscamc.h"
#include "newfmt.h"

void *stackpointer;

mps_ap_t ap;
mycell *p;
size_t s;
int nrefs;

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;

 int h;
 mycell *p, *q, *r;

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

 cdie(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "create ap");

 r = allocone(ap, 1000);

 for (h=0; h<10000; h++) {
  if (h % 10 == 0) {
   report("iter", "%i", h);
  }
  q = allocone(ap, 1000);
  p = allocone(ap, 1000);
  setref(p, 1, q);
  r = allocone(ap, 1000);
  setref(q, 50, r);
  setref(r, 1, p);

 }

 mps_ap_destroy(ap);

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

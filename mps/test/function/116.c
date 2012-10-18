/* 
TEST_HEADER
 id = $HopeName$
 summary = allocation shouldn't fail if there's garbage to collect
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"

void *stackpointer;

mps_pool_t poolmv;
mps_space_t space;

static void test(void)
{
 mps_pool_t pool;
 mps_thr_t thread;

 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap, ap2;

 mycell *a, *b;

 mps_res_t res;
 int i;

/* create an arena that can't grow beyond 30 M */

 cdie(mps_arena_create(&space, mps_arena_class_vm(), (size_t) (1024*1024*30)),
  "create arena");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
  mps_stack_scan_ambig, stackpointer, 0), "create root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

/* allocate until full */


 i = 0;

 b = NULL;

 while (allocrone(&a, ap, 128, MPS_RANK_EXACT) == MPS_RES_OK) {
  i++;
  setref(a, 0, b);
  b = a;
 }

 comment("%d objs allocated.", i);

/* try to allocate 10 times */

 cdie(mps_ap_create(&ap2, pool, MPS_RANK_EXACT), "create second ap");
 mps_ap_destroy(ap);

 for (i = 0; i < 10; i++) {
  res = allocrone(&a, ap2, 128, MPS_RANK_EXACT);
  report("predie", "%s", err_text(res));
 }

/* now let everything die, and try to allocate 10 times */

 mps_root_destroy(root);

 for (i = 0; i < 10; i++) {
  res = allocrone(&a, ap2, 128, MPS_RANK_EXACT);
  report("postdie", "%s", err_text(res));
 }

 mps_ap_destroy(ap2);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
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

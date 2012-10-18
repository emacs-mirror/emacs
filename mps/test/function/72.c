/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!72.c(trunk.4) $
 summary = request.dylan.170439 (detect bad pointers)
 language = c
 link = testlib.o exfmt.o
OUTPUT_SPEC
 assert = true
 assertfile P= trace.c
 assertline = 963
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

void *stackpointer;
mycell *z;

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a, *b;

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

 a = allocone(ap, 1024, 1);
 z = a;

 b = allocone(ap, 1024, 1);
 setref(b, 0, a);

 a = allocdumb(ap, 1024*64, 1);
 a = allocdumb(ap, 1024*64, 1);

 comment("Collecting...");
 mps_arena_collect(space);
 asserts(z != a, "Didn't move!");

 comment("Writing bad pointer...");

 b->data.ref[0].addr = z;
 mps_arena_collect(space);
 comment("Bad pointer not spotted in collection");

 fail();
 
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

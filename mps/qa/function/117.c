/* 
TEST_HEADER
 id = $HopeName$
 summary = should collect objects on buffered segs (request.dylan.160064)
 language = c
 link = testlib.o rankfmt.o
OUTPUT_SPEC
 drop12 > 10000000
 drop13 > 10000000
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"

void *stackpointer;

mps_space_t space;

static void test(void)
{
 mps_pool_t pool;
 mps_thr_t thread;

 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a, *b;

 size_t x, y, z;

 int i;

/* create an arena that can't grow beyond 128 M */

 cdie(mps_arena_create(&space, mps_arena_class_vm(), (size_t) (1024*1024*128)),
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

/* allocate a jolly big chain of objects */

 b = allocone(ap, 4, MPS_RANK_EXACT);

 for (i = 0; i < 256*1024; i++) {
  a = allocone(ap, 4, MPS_RANK_EXACT);
  setref(a, 0, b);
  b = a;
 }

 comment("%d objs allocated.", i);

 mps_arena_collect(space);
 x = mps_arena_committed(space);
 report("livesize", "%d", x);

/* now let everything die, by destroying the only root and calling
   collect-world */

 mps_root_destroy(root);
 mps_arena_collect(space);
 y = mps_arena_committed(space);
 report("rootless", "%d", y);

 mps_ap_destroy(ap);
 mps_arena_collect(space);
 z = mps_arena_committed(space);
 report("apless", "%d", z);

 report("drop12", "%d", x - y);
 report("drop13", "%d", x - z);
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

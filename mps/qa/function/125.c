/* 
TEST_HEADER
 id = $HopeName$
 summary = extensible arena test (small objects)
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
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

 mycell *a, *b;

 int j;

/* create an arena that can't grow beyond 1 M */

 cdie(mps_arena_create(&space, mps_arena_class_vm(), (size_t) (1024*1024*1)),
  "create arena");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1,space,MPS_RANK_AMBIG,0,&exfmt_root,1),
  "create table root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

 comment("ready");

 b = allocone(ap, 2, MPS_RANK_EXACT);

/* allocate lots of little objects */

 while (mps_arena_committed(space) < 1024ul*1024ul*20) {
  comment("reserved %ld, committed %ld",
   mps_arena_reserved(space), mps_arena_committed(space));
  for (j=0; j<10000; j++) {
   a = allocone(ap, 2, MPS_RANK_EXACT);
   setref(a, 0, b);
   b = a;
   a = allocone(ap, 1, MPS_RANK_EXACT);
   setref(b, 1, a);
  }
  mps_arena_collect(space);
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

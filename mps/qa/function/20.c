/* 
TEST_HEADER
 id = $HopeName$
 summary = create lots of formats at once (and cause to run out of memory)
 language = c
 link = testlib.o newfmt.o
OUTPUT_SPEC
 errtext = create format: MEMORY
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpscmv.h"
#include "newfmt.h"

void *stackpointer;

static void test(void) {
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_fmt_t format;
 mps_root_t root;
 mps_addr_t q;

 int p;

 die(mps_space_create(&space), "create");
 die(mps_thread_reg(&thread, space), "register thread");
 die(mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
  mps_stack_scan_ambig, stackpointer, 0), "create root");

 die(mps_pool_create(&pool, space, mps_class_mv(),
  1024*32, 1024*16, 1024*256), "pool");

 while (mps_alloc(&q, pool, 64*1024)==MPS_RES_OK);
 p=0;

 while (1) {
  p++;
  die(mps_fmt_create_A(&format, space, &fmtA), "create format");
  report("format", "%i", p);
 }

 asserts(1, "Unreachable!");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

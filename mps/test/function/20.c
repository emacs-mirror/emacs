/* 
TEST_HEADER
 id = $Id$
 summary = create lots of formats at once (and cause to run out of memory)
 language = c
 link = testlib.o newfmt.o
OUTPUT_SPEC
 errtext = create format: COMMIT_LIMIT
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpscmv.h"
#include "newfmt.h"

void *stackpointer;

static void test(void) {
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_fmt_t format;
 mps_root_t root;
 mps_addr_t q;

 int p;

 die(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create");
 die(mps_arena_commit_limit_set(arena, 1ul << 30), "commit_limit_set");
 die(mps_thread_reg(&thread, arena), "register thread");
 die(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
  mps_stack_scan_ambig, stackpointer, 0), "create root");

 die(mps_pool_create(&pool, arena, mps_class_mv(),
  1024*32, 1024*16, 1024*256), "pool");

 while (mps_alloc(&q, pool, 64*1024)==MPS_RES_OK);
 p=0;

 while (1) {
  p++;
  die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
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

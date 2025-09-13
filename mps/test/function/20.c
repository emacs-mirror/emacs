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
#include "mpscmvff.h"
#include "newfmt.h"

static void test(void *stack_pointer)
{
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
 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

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
 run_test(test);
 pass();
 return 0;
}

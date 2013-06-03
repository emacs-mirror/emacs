/* 
TEST_HEADER
 id = $Id$
 summary = mps_arena_commit_limit and small arena chunks
 language = c
 link = testlib.o
OUTPUT_SPEC
 count < 10
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"
#include "mpscmv.h"

void *stackpointer;

mps_arena_t arena;
mps_thr_t thread;
mps_pool_t pool;

static void test(void) {
 int i;
 mps_addr_t a;

/* create an arena with chunk size of 2 M */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*20)),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

/* set the commit limit to 10MB */

 report_res("commit0",
  mps_arena_commit_limit_set(arena, (size_t) (1024*1024*10)));


/* create a pool */

 cdie(mps_pool_create(&pool, arena, mps_class_mv(), (size_t) 64, (size_t) 64, (size_t) 64), "pool create");
 
 for (i=0; i<200; i++) {
  report("count", "%i", i);
  die(mps_alloc(&a, pool, (size_t) 1024*1024), "alloc");
 }

 mps_pool_destroy(pool);

 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena.");

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

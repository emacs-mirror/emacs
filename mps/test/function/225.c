/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!21.c(trunk.4) $
 summary = EPDL allocate large promise, make it small, repeat
 language = c
 link = testlib.o
 harness = 2.5
 parameters = EXTENDBY=65536 AVGSIZE=32 ALIGN=4 PROMISE=64 ITERATE=2000
END_HEADER
*/

#include "testlib.h"
#include "mpscepdl.h"
#include "mpsavm.h"

#define VMNZSIZE ((size_t) 30*1024*1024)

static void test(void) {
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t q;
 int p;

 die(mps_arena_create(&arena, mps_arena_class_vmnz(), VMNZSIZE), "create");
 die(mps_arena_commit_limit_set(arena, VMNZSIZE), "commit limit");

 die(mps_pool_create(&pool, arena, mps_class_epdl(),
  EXTENDBY, AVGSIZE, ALIGN), "pool create");

 for (p=0; p<ITERATE; p++) {
  die(mps_alloc(&q, pool, PROMISE*1024), "alloc");
  q = (mps_addr_t) ((char *) q + 8);
  mps_free(pool, q, PROMISE*1024-8);
  report("promise", "%i", p);
 }

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void) {
 easy_tramp(test);
 pass();
 return 0;
}

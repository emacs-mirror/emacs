/* 
TEST_HEADER
 id = $Id$
 summary = allocate large promise, make it small, repeat
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"

static void test(void) {
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t q;
 int p;

 die(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create");

 die(mps_pool_create(&pool, arena, mps_class_mv(),
                     (size_t)(1024*32), (size_t)(1024*16), (size_t)(1024*256)),
     "create MV pool");

 for (p=0; p<2000; p++) {
  die(mps_alloc(&q, pool, 1024*1024), "alloc");
  q = (mps_addr_t) ((char *) q + 8);
  mps_free(pool, q, 256*1024-8);
  report("promise", "%i", p);
 }
}

int main(void) {
 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = allocate large promise, make it small, repeat interleaved
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"

static void test(void) {
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t q, r;
 int p;

 die(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create");

 die(mps_pool_create(&pool, arena, mps_class_mv(),
                    (size_t)(1024*32), (size_t)(1024*16), (size_t)(1024*256)),
     "create MV pool");

 die(mps_alloc(&q, pool, 1024*1024), "alloc");

 for (p=0; p<2000; p++) {
  report("promise", "%i", p);
  die(mps_alloc(&r, pool, 1024*1024), "alloc");
  mps_free(pool, q, 256*1024-MPS_PF_ALIGN);
  q = (mps_addr_t) ((char *) r + MPS_PF_ALIGN);
 }
}

int main(void) {
 easy_tramp(test);
 pass();
 return 0;
}

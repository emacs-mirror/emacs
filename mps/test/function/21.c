/* 
TEST_HEADER
 id = $HopeName$
 summary = allocate large promise, make it small, repeat
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"

static void test(void) {
 mps_space_t space;
 mps_pool_t pool;
 mps_addr_t q;
 int p;

 die(mps_space_create(&space), "create");

 die(mps_pool_create(&pool, space, mps_class_mv(),
  1024*32, 1024*16, 1024*256), "pool");

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

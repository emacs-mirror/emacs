/* 
TEST_HEADER
 id = $HopeName$
 summary = -1 as third argument to mps_alloc (MV)
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

 cdie(mps_space_create(&space), "create");

 cdie(mps_pool_create(&pool, space, mps_class_mv(),
  1024*32, 1024*16, 1024*256), "pool");

 cdie(mps_alloc(&q, pool, (size_t) -1), "alloc");

 mps_pool_destroy(pool);
 mps_space_destroy(space);
}

int main(void) {
 easy_tramp(test);
 pass();
 return 0;
}

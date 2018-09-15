/* 
TEST_HEADER
 id = $Id$
 summary = very large number as third argument to mps_alloc (MVFF)
 language = c
 link = testlib.o
OUTPUT_SPEC
 error = true
 errtext = alloc: RESOURCE
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"

static void test(void) {
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t q;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create");

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 cdie(mps_alloc(&q, pool, (size_t)-1 - mmqaArenaSIZE), "alloc");

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void) {
 easy_tramp(test);
 return 0;
}

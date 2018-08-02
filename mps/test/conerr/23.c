/* 
TEST_HEADER
 id = $Id$
 summary = double free
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poolmvff.c
 assertcond = res == ResOK
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t obj;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 cdie(mps_alloc(&obj, pool, 152), "allocate");

 mps_free(pool, obj, 152);
 mps_free(pool, obj, 152);

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

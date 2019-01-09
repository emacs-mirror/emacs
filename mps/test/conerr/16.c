/* 
TEST_HEADER
 id = $Id$
 summary = destroy a pool twice
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = TESTT(Pool, pool)
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 mps_pool_destroy(pool);
 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void)
{
 run_test(test);
 return 0;
}

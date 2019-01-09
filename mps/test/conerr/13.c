/* 
TEST_HEADER
 id = $Id$
 summary = create a pool in an uncreated arena
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= global.c
 assertcond = TESTT(Arena, arena)
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena = malloc(4096);
 mps_pool_t pool;

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void)
{
 run_test(test);
 return 0;
}

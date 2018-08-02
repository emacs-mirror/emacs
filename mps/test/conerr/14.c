/* 
TEST_HEADER
 id = $Id$
 summary = create a pool in a destroyed arena
 language = c
 link = testlib.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 mps_arena_destroy(arena);

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

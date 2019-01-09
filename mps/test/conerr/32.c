/* 
TEST_HEADER
 id = $Id$
 summary = create AP in a pool that doesn't support it
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poolabs.c
 assertcond = unreachable code
END_HEADER
*/

#include "testlib.h"
#include "mpscmfs.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_ap_t ap;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_pool_create(&pool, arena, mps_class_mfs(), 64), "create pool");

 cdie(mps_ap_create(&ap, pool, mps_rank_exact()), "create ap");

 mps_pool_destroy(pool);
 comment("Destroyed pool");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 run_test(test);
 return 0;
}

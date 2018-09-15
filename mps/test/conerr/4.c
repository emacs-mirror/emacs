/* 
TEST_HEADER
 id = $Id$
 summary = destroy an arena which contains a pool
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= global.c
 assertcond = RingLength(&arenaGlobals->poolRing) == arenaGlobals->systemPools
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");
 
 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 mps_arena_destroy(arena);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

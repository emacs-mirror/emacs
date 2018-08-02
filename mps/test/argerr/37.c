/* 
TEST_HEADER
 id = $Id$
 summary = wrong size_t to free (MVFF)
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= pool.c
 assertcond = (alignedLimit) <= _ch->limit
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"
#include "arg.h"

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t a;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 die(mps_alloc(&a, pool, 8), "alloc a");
 mps_free(pool, a, HIGHBIT_SIZE+8);

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = free though not allocated
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= tract.c
 assertcond = found
END_HEADER
*/

#include <stdlib.h>

#include "testlib.h"
#include "mpscmvff.h"

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t obj = malloc(512);

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 mps_free(pool, obj, 512);

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

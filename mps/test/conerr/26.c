/* 
TEST_HEADER
 id = $Id$
 summary = free in the wrong pool
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= pool.c
 assertcond = PoolHasRange(pool, old, AddrAdd(old, size))
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool0;
 mps_pool_t pool1;
 mps_addr_t obj;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_pool_create_k(&pool0, arena, mps_class_mv(), mps_args_none),
      "create pool 0");

 cdie(mps_pool_create_k(&pool1, arena, mps_class_mv(), mps_args_none),
      "create pool 1");

 cdie(mps_alloc(&obj, pool0, 152), "allocate in 0");

 mps_free(pool1, obj, 512);
 comment("Freed in 1.");

}

int main(void)
{
 easy_tramp(test);
 return 0;
}

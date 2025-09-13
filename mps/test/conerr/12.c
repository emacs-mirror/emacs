/* 
TEST_HEADER
 id = $Id$
 summary = destroy a format though attached to a pool
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= format.c
 assertcond = format->poolCount == 0
END_HEADER
*/

#include "testlib.h"
#include "mpsclo.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_fmt_t format;
 mps_pool_t pool;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_fmt_create_k(&format, arena, mps_args_none), "create format");

 cdie(mps_pool_create(&pool, arena, mps_class_lo(), format), "create pool");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 run_test(test);
 return 0;
}

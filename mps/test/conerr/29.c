/* 
TEST_HEADER
 id = $Id$
 summary = create an AP in a destroyed pool
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = TESTT(Pool, pool)
END_HEADER
*/

#include "testlib.h"
#include "mpsclo.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_fmt_t format;
 mps_ap_t ap;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_fmt_create_k(&format, arena, mps_args_none), "create format");

 cdie(mps_pool_create(&pool, arena, mps_class_lo(), format), "create pool");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 cdie(mps_ap_create(&ap, pool, mps_rank_exact()), "create ap");

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 run_test(test);
 return 0;
}

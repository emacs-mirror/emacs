/* 
TEST_HEADER
 id = $Id$
 summary = free in pool not supporting free
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poolabs.c
 assertcond = unreachable code
END_HEADER
*/

#include "testlib.h"
#include "mpsclo.h"

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_fmt_t format;
 mps_ap_t ap;
 mps_addr_t obj;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_fmt_create_k(&format, arena, mps_args_none), "create format");

 cdie(mps_pool_create(&pool, arena, mps_class_lo(), format), "create pool");

 cdie(mps_ap_create(&ap, pool), "create ap");

 cdie(mps_reserve(&obj, ap, 256), "reserve");
 (void)mps_commit(ap, &obj, 256);

 mps_free(pool, obj, 256);
 comment("Freed.");

 mps_free(pool, obj, 256);
 comment("Freed again.");

 mps_pool_destroy(pool);
 comment("Destroyed pool");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

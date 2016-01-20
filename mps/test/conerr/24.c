/* 
TEST_HEADER
 id = $Id$
 summary = alloc in pool not supporting alloc
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
 mps_addr_t obj;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(
  mps_fmt_create_k(&format, arena, mps_args_none),
  "create format");

 cdie(
  mps_pool_create(&pool, arena, mps_class_lo(), format),
  "create pool");

 cdie(mps_alloc(&obj, pool, 152), "allocate");

 mps_free(pool, obj, 152);
 comment("Freed.");

 mps_free(pool, obj, 152);
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

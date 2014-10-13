/* 
TEST_HEADER
 id = $Id$
 summary = create a pool with a format in the wrong arena
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpsclo.h"

static void test(void)
{
 mps_arena_t arena0;
 mps_arena_t arena1;
 mps_pool_t pool;

 mps_fmt_t format;

 cdie(mps_arena_create(&arena0, mps_arena_class_vm(), mmqaArenaSIZE), "create arena 0");
 cdie(mps_arena_create(&arena1, mps_arena_class_vm(), mmqaArenaSIZE), "create arena 1");

 cdie(
  mps_fmt_create_k(&format, arena0, mps_args_none),
  "create format in arena 0");

 cdie(
  mps_pool_create(&pool, arena1, mps_class_lo(), format),
  "create pool in arena 1");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_arena_destroy(arena0);
 comment("Destroyed arena 0.");
 mps_arena_destroy(arena1);
 comment("Destroyed arena 1.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

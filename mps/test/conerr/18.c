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

static void zilch(void)
{
}


static mps_addr_t myskip(mps_addr_t object)
{
 return *(mps_addr_t *)object;
}


static void test(void)
{
 mps_arena_t arena;
 mps_arena_t arena1;
 mps_pool_t pool;

 mps_fmt_t format;
 mps_fmt_A_s fmtA;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena 0");
 cdie(mps_arena_create(&arena1), "create arena 1");

 fmtA.align = (mps_align_t) 1;
 fmtA.scan  = &zilch;
 fmtA.skip  = &myskip;
 fmtA.copy  = &zilch;
 fmtA.fwd   = &zilch;
 fmtA.isfwd = &zilch;
 fmtA.pad   = &zilch;

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format in arena 0");

 cdie(
  mps_pool_create(&pool, arena1, mps_class_lo(), format),
  "create pool in arena 1");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_arena_destroy(arena);
 comment("Destroyed arena 0.");
 mps_arena_destroy(arena1);
 comment("Destroyed arena 1.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

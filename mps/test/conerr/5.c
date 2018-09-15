/* 
TEST_HEADER
 id = $Id$
 summary = destroy an arena which contains a format
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= global.c
 assertcond = RingIsSingle(&arena->formatRing)
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
 mps_arena_t arena;
 mps_fmt_t format;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_fmt_create_k(&format, arena, mps_args_none), "create format");

 mps_arena_destroy(arena);
 comment("Destroy arena.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

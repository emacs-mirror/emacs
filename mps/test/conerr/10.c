/* 
TEST_HEADER
 id = $Id$
 summary = destroy a format though uncreated
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = TESTT(Format, format)
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
 mps_arena_t arena;
 mps_fmt_t format = (mps_fmt_t)&format;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

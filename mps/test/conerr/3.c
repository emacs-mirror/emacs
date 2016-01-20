/* 
TEST_HEADER
 id = $Id$
 summary = destroy an arena which isn't an arena
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= global.c
 assertcond = TESTT(Arena, arena)
END_HEADER
*/

#include "mpmst.h"
#include "testlib.h"

static void test(void)
{
 char buf[sizeof(ArenaStruct)];
 mps_arena_t arena;

 arena = (void *)buf;
 mps_arena_destroy(arena);
 comment("Destroy arena.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

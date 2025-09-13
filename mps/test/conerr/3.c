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

static void test(void *stack_pointer)
{
 char buf[sizeof(ArenaStruct)];
 mps_arena_t arena;

 arena = (void *)buf;
 mps_arena_destroy(arena);
 comment("Destroy arena.");
}

int main(void)
{
 run_test(test);
 return 0;
}

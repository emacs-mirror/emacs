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

#include "testlib.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;

 arena = malloc(4096);
 mps_arena_destroy(arena);
 comment("Destroy arena.");
}

int main(void)
{
 run_test(test);
 return 0;
}

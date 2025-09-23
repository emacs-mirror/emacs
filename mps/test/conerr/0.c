/* 
TEST_HEADER
 id = $Id$
 summary = create an arena and then destroy it, twice!
 language = c
 link = testlib.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "Create arena");
 mps_arena_destroy(arena);
 comment("Destroy arena once.");
 mps_arena_destroy(arena);
 comment("Destroy arena twice!");
}

int main(void)
{
 run_test(test);
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = create an arena and see how much mem is taken
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"


static void test(void *stack_pointer)
{
 mps_arena_t arena;

 comment("Entered trampoline");
 mmqa_pause(10);
 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");
 comment("Created arena");
 report("committed", "%ul", mps_arena_committed(arena));
 report("reserved",  "%ul", mps_arena_reserved(arena));
 mmqa_pause(10);
 mps_arena_destroy(arena);
 comment("Destroyed arena");
 mmqa_pause(10);
}


int main(void)
{
 comment("Started");
 mmqa_pause(10);
 run_test(test);
 comment("Left trampoline");
 mmqa_pause(10);
 pass();
 return 0;
}

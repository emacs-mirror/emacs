/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!70.c(trunk.3) $
 summary = create a space and see how much mem is taken
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"


static void test(void)
{
 mps_space_t space;

 comment("Entered trampoline");
 mmqa_pause(10);
 cdie(mps_space_create(&space), "create space");
 comment("Created space");
 report("committed", "%ul", mps_arena_committed(space));
 report("reserved",  "%ul", mps_arena_reserved(space));
 mmqa_pause(10);
 mps_space_destroy(space);
 comment("Destroyed space");
 mmqa_pause(10);
}


int main(void)
{
 comment("Started");
 mmqa_pause(10);
 easy_tramp(test);
 comment("Left trampoline");
 mmqa_pause(10);
 pass();
 return 0;
}

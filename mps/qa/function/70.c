/* 
TEST_HEADER
 id = $HopeName$
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
 pause(10);
 cdie(mps_space_create(&space), "create space");
 comment("Created space");
 report("committed", "%ul", mps_arena_committed(space));
 report("reserved",  "%ul", mps_arena_reserved(space));
 pause(10);
 mps_space_destroy(space);
 comment("Destroyed space");
 pause(10);
}

int main(void)
{
 comment("Started");
 pause(10);
 easy_tramp(test);
 comment("Left trampoline");
 pause(10);
 pass();
 return 0;
}

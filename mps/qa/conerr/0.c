/* 
TEST_HEADER
 id = $HopeName$
 summary = create a space and then destroy it, twice!
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
 mps_space_t space;

 cdie(mps_space_create(&space), "Create space");
 mps_space_destroy(space);
 comment("Destroy space once.");
 mps_space_destroy(space);
 comment("Destroy space twice!");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

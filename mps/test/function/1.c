/* 
TEST_HEADER
 id = $HopeName$
 summary = create a space and then destroy it
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
 mps_space_t space;

 cdie(mps_space_create(&space), "create space");
 mps_space_destroy(space);
}

int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $HopeName$
 summary = create 1000 spaces and destroy each one just before the next is created
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
 mps_space_t space;

 int p;

 die(mps_space_create(&space), "create");

 for (p=0; p<1000; p++)
 {
  mps_space_destroy(space);
  die(mps_space_create(&space), "create");
  comment("%i", p);
 }
}

int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $HopeName$
 summary = create 1000 spaces and destroy each one just after the next is created
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
 mps_space_t space;
 mps_space_t space1;

 int p;

 die(mps_space_create(&space1), "create");

 for (p=0; p<1000; p++)
 {
  die(mps_space_create(&space), "create");
  comment("%i", p);
  mps_space_destroy(space1);
  space1=space;
 }
}

int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

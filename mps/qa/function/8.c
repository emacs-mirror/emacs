/* test create spaces at once until an error results!
   language c
   link testlib.o
   space>10
*/

#include "testlib.h"

static void test(void)
{
 mps_space_t space;

 int p;

 p=0;

 while (1)
 {
  die(mps_space_create(&space), "create");
  p = p+1;
  report("space", "%i", p);
 }
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

/* test destroy a space without creating it
   language c
   link testlib.o
*/

#include "testlib.h"

static void test(void)
{
 mps_space_t space;

 mps_space_destroy(space);
 comment("Destroy space.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

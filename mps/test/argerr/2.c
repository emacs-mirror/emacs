/* 
TEST_HEADER
 id = $HopeName$
 summary = destroy a space with an null space_t
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

static void test(void)
{
 mps_space_t space;

 cdie(mps_space_create(&space),
      "Create space");
 mps_space_destroy(NULL);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

/* TEST_HEADER
 summary = destroy a space with an unaligned space_t
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
 mps_space_destroy(UNALIGNED);
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

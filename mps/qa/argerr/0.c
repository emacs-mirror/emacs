/* test create a space with a NULL space_t
   language c
   link testlib.o
*/

#include "testlib.h"
#include "arg.h"

static void test(void)
{
 adie(mps_space_create(NULL),
      "Create space");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

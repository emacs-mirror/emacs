/* 
TEST_HEADER
 id = $HopeName$
 summary = reset ld in destroyed space
 language = c
 link = myfmt.o testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

static void test(void)
{
 mps_space_t space;
 mps_ld_s ld;

 cdie(mps_space_create(&space), "create space");

 mps_space_destroy(space);
 comment("Destroyed space.");

 mps_ld_reset(&ld, space);
 comment("Reset ld."); 
}

int main(void)
{
 easy_tramp(test);
 return 0;
}


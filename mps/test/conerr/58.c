/* 
TEST_HEADER
 id = $HopeName$
 summary = isstale on ld in wrong space
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
 mps_space_t space1;
 mps_ld_s ld;

 cdie(mps_space_create(&space), "create space");
 cdie(mps_space_create(&space1), "create space 1");

 mps_ld_reset(&ld, space);
 comment("Reset ld."); 

 report("isstale", "%d", mps_ld_isstale(&ld, space1, &space));

 mps_space_destroy(space);
 comment("Destroyed space.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}


/* 
TEST_HEADER
 id = $HopeName$
 summary = add to ld in destroyed space
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

 mps_ld_reset(&ld, space);
 comment("Reset ld."); 

 mps_ld_add(&ld, space, &space);
 comment("Added to ld.");

 report("isstale", "%d", mps_ld_isstale(&ld, space, &space));

 mps_space_destroy(space);
 comment("Destroyed space.");

 mps_ld_add(&ld, space, &space);
 comment("Added to ld.");

}

int main(void)
{
 easy_tramp(test);
 return 0;
}


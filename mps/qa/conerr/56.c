/* test isstale in destroyed space
   language c
   link myfmt.o testlib.o
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

 report("isstale2", "%d", mps_ld_isstale(&ld, space, &space));
}

int main(void)
{
 easy_tramp(test);
 return 0;
}


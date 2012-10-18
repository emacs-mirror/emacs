/* 
TEST_HEADER
 id = $HopeName$
 summary = add to ld without resetting
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

/*
 mps_ld_reset(&ld, space);
 comment("Reset ld."); 
*/

 mps_ld_add(&ld, space, &space);
 comment("Added to ld.");

 mps_space_destroy(space);
 comment("Destroyed space.");

}

int main(void)
{
 easy_tramp(test);
 return 0;
}


/* $HopeName$
TEST_HEADER
 summary = destroy a format though attached to a pool
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpsclo.h"

static void zilch(void)
{
}


static mps_addr_t myskip(mps_addr_t object)
{
 return object;
}

static void test(void)
{
 mps_space_t space;
 mps_fmt_t format;
 mps_fmt_A_s fmtA;
 mps_pool_t pool;

 cdie(mps_space_create(&space), "create space");

 fmtA.align = (mps_align_t) 1;
 fmtA.scan  = &zilch;
 fmtA.skip  = &myskip;
 fmtA.copy  = &zilch;
 fmtA.fwd   = &zilch;
 fmtA.isfwd = &zilch;
 fmtA.pad   = &zilch;

 cdie(
  mps_fmt_create_A(&format, space, &fmtA), 
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_lo(), format),
  "create pool");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_space_destroy(space);
 comment("Destroyed space.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

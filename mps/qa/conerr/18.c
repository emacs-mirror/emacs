/* 
TEST_HEADER
 id = $HopeName$
 summary = create a pool with a format in the wrong space
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
 return *(mps_addr_t *)object;
}


static void test(void)
{
 mps_space_t space;
 mps_space_t space1;
 mps_pool_t pool;

 mps_fmt_t format;
 mps_fmt_A_s fmtA;

 cdie(mps_space_create(&space), "create space 0");
 cdie(mps_space_create(&space1), "create space 1");

 fmtA.align = (mps_align_t) 1;
 fmtA.scan  = &zilch;
 fmtA.skip  = &myskip;
 fmtA.copy  = &zilch;
 fmtA.fwd   = &zilch;
 fmtA.isfwd = &zilch;
 fmtA.pad   = &zilch;

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format in space 0");

 cdie(
  mps_pool_create(&pool, space1, mps_class_lo(), format),
  "create pool in space 1");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_space_destroy(space);
 comment("Destroyed space 0.");
 mps_space_destroy(space1);
 comment("Destroyed space 1.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

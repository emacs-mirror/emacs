/* 
TEST_HEADER
 id = $HopeName$
 summary = create an AP in an uncreated pool
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
 mps_pool_t pool;

 mps_fmt_t format;
 mps_fmt_A_s fmtA;
 mps_ap_t ap;

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

/*
 cdie(
  mps_pool_create(&pool, space, mps_class_lo(), format),
  "create pool");
*/

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_space_destroy(space);
 comment("Destroyed space.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

/* 
TEST_HEADER
 id = $HopeName$
 summary = create an AP with a rank not supported by its pool
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"

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
 mps_addr_t p;

 cdie(mps_space_create(&space), "create space");

 fmtA.align = (mps_align_t) 4;
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
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_AMBIG),
  "create ap");

do
 {
  cdie(mps_reserve(&p, ap, 0x100), "Reserve: ");
 }
 while (!mps_commit(ap, p, 0x100));
 comment("Committed.");

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

/* 
TEST_HEADER
 id = $HopeName$
 summary = destroy an uncreated pool
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 size_t extendBy;
 size_t avgSize;
 size_t maxSize;

 extendBy = (size_t) 4096;
 avgSize  = (size_t) 32;
 maxSize  = (size_t) 65536;

 cdie(mps_space_create(&space), "create space");

/*
 cdie(
  mps_pool_create(&pool, space, mps_class_mv(),
   extendBy, avgSize, maxSize),
  "create pool");
*/

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

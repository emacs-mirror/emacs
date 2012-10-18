/* 
TEST_HEADER
 id = $HopeName$
 summary = free in the wrong pool
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
 mps_pool_t pool1;
 size_t extendBy;
 size_t avgSize;
 size_t maxSize;

 mps_addr_t obj;

 extendBy = (size_t) 4096;
 avgSize  = (size_t) 32;
 maxSize  = (size_t) 65536;

 cdie(mps_space_create(&space), "create space");

 cdie(
  mps_pool_create(&pool, space, mps_class_mv(),
   extendBy, avgSize, maxSize),
  "create pool 0");

 cdie(
  mps_pool_create(&pool1, space, mps_class_mv(),
   extendBy, avgSize, maxSize),
  "create pool 1");

 cdie(mps_alloc(&obj, pool, 152), "allocate in 0");

 mps_free(pool, obj, 512);
 comment("Freed in 1.");

}

int main(void)
{
 easy_tramp(test);
 return 0;
}

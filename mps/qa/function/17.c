/* 
TEST_HEADER
 id = $HopeName$
 summary = create and destroy lots of pools (interleaved)
 language = c
 link = testlib.o newfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "newfmt.h"

static void test(void) {
 mps_space_t space;
 mps_pool_t pool;
 mps_pool_t pool1;
 mps_thr_t thread;
 mps_fmt_t format;

 int p;

 die(mps_space_create(&space), "create");
 die(mps_thread_reg(&thread, space), "register thread");
 die(mps_fmt_create_A(&format, space, &fmtA), "create format");

 die(mps_pool_create(&pool1, space, mps_class_amc(), format),
  "create pool");

 for (p=0; p<10000; p++) {
  die(mps_pool_create(&pool, space, mps_class_amc(), format),
   "create pool");
  comment("%i", p);
  mps_pool_destroy(pool1);
  pool1=pool;
 }

 mps_pool_destroy(pool);
 mps_fmt_destroy(format);
 mps_thread_dereg(thread);
 mps_space_destroy(space);
}

int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

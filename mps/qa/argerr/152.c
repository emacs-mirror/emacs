/* TEST_HEADER
 summary = 2nd arg to mps_free not at start of an object
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscmfs.h"
#include "arg.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;

 size_t mysize;
 mps_addr_t a;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 mysize = 16;

 cdie(
  mps_pool_create(
     &pool, space, mps_class_mfs(), mysize, mysize),
  "create pool");

 cdie(mps_alloc(&a, pool, mysize), "alloc");
 mps_free(pool, (mps_addr_t) ((char *)a+8), mysize);
 error("free");
 mps_pool_destroy(pool);

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

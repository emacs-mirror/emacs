/* 
TEST_HEADER
 id = $HopeName$
 summary = wrong 3rd arg to mps_free
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

 mysize = 8;

 cdie(
  mps_pool_create(
     &pool, space, mps_class_mfs(), (size_t) 8, mysize),
  "create pool");

 mps_alloc(&a, pool, mysize);
 mps_free(pool, a, mysize+8);
 mps_pool_destroy(pool);

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

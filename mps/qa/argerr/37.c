/* 
TEST_HEADER
 id = $HopeName$
 summary = wrong size_t to free (MV)
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"
#include "arg.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;

 mps_addr_t a;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_pool_create(
     &pool, space, mps_class_mv(),
     (size_t) 4096, (size_t) 32, (size_t) 64*1024),
  "create pool");

 die(mps_alloc(&a, pool, 8),
     "alloc a");
 mps_free(pool, a, HIGHBIT_SIZE+8);
 mps_pool_destroy(pool);

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
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
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;

 size_t mysize;
 mps_addr_t a;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 mysize = 16;

 cdie(
  mps_pool_create(
     &pool, arena, mps_class_mfs(), mysize, mysize),
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

/* 
TEST_HEADER
 id = $Id$
 summary = NULL 1st arg to mps_free
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

 mysize = 8;

 cdie(
  mps_pool_create(
     &pool, arena, mps_class_mfs(), (size_t) 8, mysize),
  "create pool");

 mps_alloc(&a, pool, mysize);
 mps_free(NULL, a, mysize);
 mps_pool_destroy(pool);

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

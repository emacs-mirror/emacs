/* 
TEST_HEADER
 id = $Id$
 summary = highbit set 3rd arg to mps_free (MFS)
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= pool.c
 assertcond = (alignedLimit) <= _ch->limit
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
 mps_free(pool, a, HIGHBIT_SIZE+mysize);
 mps_pool_destroy(pool);

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

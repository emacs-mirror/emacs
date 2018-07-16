/* 
TEST_HEADER
 id = $Id$
 summary = extendBy > maxSize for pool_create (MV)
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poolmvff.c
 assertcond = extendBy <= maxSize
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"
#include "arg.h"

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_pool_create(
     &pool, arena, mps_class_mv(),
     (size_t) 33, (size_t) 32, (size_t) 32),
  "create pool");

 mps_pool_destroy(pool);

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = zero extendBy for pool_create (MFS)
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poolmfs.c
 assertcond = extendBy > 0
END_HEADER
*/

#include "testlib.h"
#include "mpscmfs.h"
#include "arg.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_pool_create(
     &pool, arena, mps_class_mfs(), (size_t) 0, (size_t) 32),
  "create pool");

 mps_pool_destroy(pool);

}

int main(void)
{
 run_test(test);
 return 0;
}

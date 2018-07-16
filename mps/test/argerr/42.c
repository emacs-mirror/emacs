/* 
TEST_HEADER
 id = $Id$
 summary = zero avgSize for pool_create (MV)
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= poolmvff.c
 assertcond = avgSize > 0
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

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(
  mps_pool_create(
     &pool, arena, mps_class_mv(),
     (size_t) 32, (size_t) 0, (size_t) 32),
  "create pool");

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = allocate large object, free its middle, repeat
 language = c
 link = testlib.o
 parameters = OBJECTS=2000
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_addr_t q;
 int p;

 die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none), "create");

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 for (p=0; p<OBJECTS; p++) {
  die(mps_alloc(&q, pool, 1024), "alloc");
  q = (mps_addr_t) ((char *) q + MPS_PF_ALIGN);
  mps_free(pool, q, 256-MPS_PF_ALIGN);
 }

 mps_pool_destroy(pool);
 mps_arena_destroy(arena);
}

int main(void) {
 run_test(test);
 pass();
 return 0;
}

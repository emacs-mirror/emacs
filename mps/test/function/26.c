/* 
TEST_HEADER
 id = $Id$
 summary = try to allocate as large an object as will succeed
 language = c
 link = testlib.o
OUTPUT_SPEC
 maxsize > 100000000
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"

mps_arena_t arena;
mps_pool_t pool;
mps_addr_t q;

static mps_res_t trysize(size_t try) {
 mps_res_t res;

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 comment("Trying %x", try);

 res = mps_alloc(&q, pool, try);
 mps_pool_destroy(pool);

 comment("%s", err_text(res));

 return res;
}

static void test(void *stack_pointer)
{
 size_t inf, sup, try;

 die(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create");

 inf = 0;
 sup = 1024*1024*1000; /* i.e. 1 gigabyte */

 while (sup-inf > 1) {
  try = inf + (sup-inf)/2;
  if (trysize(try) == MPS_RES_OK) {
   inf = try;
  } else {
   sup = try;
  }
 }

 for (try = inf-4; try < inf+4; try++)
  (void) trysize(try);

 report("maxsize", "%lu", (unsigned long) inf);
}

int main(void) {
 run_test(test);
 pass();
 return 0;
}

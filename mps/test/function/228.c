/* 
TEST_HEADER
 id = $Id$
 summary = can't register unfinalizable objects for finalization
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= global.c
 assertcond = PoolHasAttr(refpool, AttrGC)
END_HEADER
*/

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"

static void test(void)
{
  mps_arena_t arena;
  mps_pool_t pool;
  mps_addr_t p;

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
      "arena_create");
  die(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none),
      "pool_create");
  die(mps_alloc(&p, pool, 4096), "alloc");
  die(mps_finalize(arena, &p), "finalize");

  mps_pool_destroy(pool);
  mps_arena_destroy(arena);
}


int main(void)
{
  easy_tramp(test);
  pass();
  return 0;
}

/* 
TEST_HEADER
 id = $Id: //info.ravenbrook.com/project/mps/master/test/argerr/99.c#4 $
 summary = finalize address in manually managed pool
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= global.c
 assertcond = PoolHasAttr(refpool, AttrGC)
END_HEADER
*/

#include "testlib.h"
#include "mps.h"
#include "mpscmvff.h"

static void test(void)
{
  void *p;
  mps_arena_t arena;
  mps_pool_t pool;
  cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
       "create arena");
  cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none),
       "create pool");
  cdie(mps_alloc(&p, pool, 16), "alloc");
  mps_finalize(arena, &p);
  mps_pool_destroy(pool);
  mps_arena_destroy(arena);
}

int main(void)
{
  easy_tramp(test);
  return 0;
}

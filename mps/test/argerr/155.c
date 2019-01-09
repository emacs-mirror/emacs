/* 
TEST_HEADER
 id = $Id: //info.ravenbrook.com/project/mps/master/test/argerr/99.c#4 $
 summary = finalize address not managed by the arena
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= global.c
 assertcond = PoolOfAddr(&refpool, arena, (Addr)obj)
END_HEADER
*/

#include "testlib.h"
#include "mps.h"

static void test(void *stack_pointer)
{
  void *p = &p;
  mps_arena_t arena;
  cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
       "create arena");
  mps_finalize(arena, &p);
  mps_arena_destroy(arena);
}

int main(void)
{
  run_test(test);
  return 0;
}

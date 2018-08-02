/* 
TEST_HEADER
 id = $Id: //info.ravenbrook.com/project/mps/master/test/function/234.c#1 $
 summary = regression test for job004102
 language = c
 link = testlib.o
 parameters = GRAINSIZE=4096
END_HEADER
*/

#include "mpsavm.h"
#include "mpscmvff.h"
#include "testlib.h"

static void test(void)
{
  mps_arena_t arena;
  mps_pool_t pool;
  void *addr[GRAINSIZE];
  size_t i;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_GRAIN_SIZE, GRAINSIZE);
    MPS_ARGS_ADD(args, MPS_KEY_SPARE_COMMIT_LIMIT, 0);
    cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
         "create arena");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, GRAINSIZE);
    MPS_ARGS_ADD(args, MPS_KEY_SPARE, 0);
    cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), args),
         "create pool");
  } MPS_ARGS_END(args);

  /* Allocate objects, each consisting of one arena grain. */
  for (i = 0; i < GRAINSIZE; ++i) {
    die(mps_alloc(&addr[i], pool, GRAINSIZE), "alloc");
  }

  /* 1. The MVFF pool was configured to keep no spare memory, so the
     freed memory will be returned to the arena immediately and added
     to the free land.

     2. We are freeing every other object, and so each object is an
     isolated continguous free range and so requires an additional CBS
     block.

     3. Eventually the free land's block pool runs out of memory and
     requires extending.

     4. The arena was configured to have no spare committed memory,
     and no additional memory can be mapped, because the commit limit
     is set to the committed memory. This means that the arena will
     have to "steal" a grain from the freed memory to extend the block
     pool.

     5. The freed memory consists of a single grain, so after stealing
     a grain there is nothing left to add to the free land. This is
     the case we are testing. */

  for (i = 0; i < GRAINSIZE; i += 2) {
    mps_arena_commit_limit_set(arena, mps_arena_committed(arena));
    mps_free(pool, addr[i], GRAINSIZE);
  }

  mps_pool_destroy(pool);
  mps_arena_destroy(arena);
}

int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

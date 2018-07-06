/* 
TEST_HEADER
 id = $Id$
 summary = MFS can allocate when UNIT_SIZE == EXTEND_BY
 language = c
 link = testlib.o
END_HEADER
*/

#include "mpm.h"
#include "mpscmfs.h"
#include "testlib.h"

static void test(void)
{
  size_t i;
  for (i = 0; i < 20; ++i) {
    size_t unitSize = 1 << i;
    mps_arena_t arena;
    mps_pool_t pool;
    mps_addr_t p;

    MPS_ARGS_BEGIN(args) {
      die(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
          "arena_create");
    } MPS_ARGS_END(args);

    MPS_ARGS_BEGIN(args) {
      MPS_ARGS_ADD(args, MPS_KEY_MFS_UNIT_SIZE, unitSize);
      MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, unitSize);
      die(mps_pool_create_k(&pool, arena, mps_class_mfs(), args),
          "pool_create");
    } MPS_ARGS_END(args);

    die(mps_alloc(&p, pool, unitSize), "alloc");

    mps_pool_destroy(pool);
    mps_arena_destroy(arena);
  }
}

int main(void)
{
  easy_tramp(test);
  pass();
  return 0;
}

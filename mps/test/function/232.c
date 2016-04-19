/* 
TEST_HEADER
 id = $Id: //info.ravenbrook.com/project/mps/branch/2015-08-11/compact/test/function/229.c#1 $
 summary = test arena extension and compaction
 language = c
 link = testlib.o
 parameters = SIZE=1024*1024 ITERATIONS=100
END_HEADER
*/

#include "mpm.h"
#include "mpscmvff.h"
#include "testlib.h"

static void check_chunks(mps_arena_t arena, unsigned expected)
{
  unsigned chunks = (unsigned)RingLength(ArenaChunkRing((Arena)arena));
  asserts(chunks == expected, "expected %u chunks, got %u", expected, chunks);
}

static void test(void)
{
  mps_arena_t arena;
  mps_pool_t pool;
  mps_addr_t block[ITERATIONS];
  unsigned i;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, SIZE);
    die(mps_arena_create_k(&arena, mps_arena_class_vm(), args), "arena_create");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_SPARE, 0);
    die(mps_pool_create_k(&pool, arena, mps_class_mvff(), args), "pool_create");
  } MPS_ARGS_END(args);
  check_chunks(arena, 1);

  for (i = 0; i < ITERATIONS; ++i) {
    die(mps_alloc(&block[i], pool, SIZE), "mps_alloc");
    check_chunks(arena, i + 2);
  }

  for (i = ITERATIONS; i > 0; --i) {
    mps_free(pool, block[i - 1], SIZE);
    mps_arena_collect(arena); /* ensure ArenaCompact is called */
    check_chunks(arena, i);
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

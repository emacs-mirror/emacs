/* 
TEST_HEADER
 id = $Id$
 summary = negative value for MPS_KEY_SPARE
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= arena.c
 assertcond = 0.0 <= arena->spare
END_HEADER
*/

#include "testlib.h"
#include "mps.h"

static void test(void *stack_pointer)
{
  mps_arena_t arena;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_SPARE, -0.5);
    cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), args), "arena");
  } MPS_ARGS_END(args);
  mps_arena_destroy(arena);
}

int main(void)
{
  run_test(test);
  return 0;
}

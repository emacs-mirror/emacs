/* 
TEST_HEADER
 id = $Id$
 summary = large value for MPS_KEY_SPARE
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= arena.c
 assertcond = arena->spare <= 1.0
END_HEADER
*/

#include "testlib.h"
#include "mps.h"

static void test(void)
{
  mps_arena_t arena;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_SPARE, 1.1);
    cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), args), "arena");
  } MPS_ARGS_END(args);
  mps_arena_destroy(arena);
}

int main(void)
{
  easy_tramp(test);
  return 0;
}

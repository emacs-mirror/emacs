/* 
TEST_HEADER
 id = $Id$
 summary = large argument to mps_arena_spare_set
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= arena.c
 assertcond = spare <= 1.0
END_HEADER
*/

#include "testlib.h"
#include "mps.h"

static void test(void *stack_pointer)
{
  mps_arena_t arena;
  cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
       "arena");
  mps_arena_spare_set(arena, 1.1);
  mps_arena_destroy(arena);
}

int main(void)
{
  run_test(test);
  return 0;
}

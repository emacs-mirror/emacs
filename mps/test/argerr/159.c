/* 
TEST_HEADER
 id = $Id$
 summary = negative argument to mps_arena_spare_set
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= arena.c
 assertcond = 0.0 <= spare
END_HEADER
*/

#include "testlib.h"
#include "mps.h"

static void test(void)
{
  mps_arena_t arena;
  cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
       "arena");
  mps_arena_spare_set(arena, -0.5);
  mps_arena_destroy(arena);
}

int main(void)
{
  easy_tramp(test);
  return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = destroy an arena without creating it
 language = c
 link = testlib.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
  mps_arena_t arena = (mps_arena_t)1;

 mps_arena_destroy(arena);
 comment("Destroy arena.");
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

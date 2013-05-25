/* 
TEST_HEADER
 id = $Id$
 summary = create an arena and then destroy it
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
 mps_arena_t arena;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");
 mps_arena_destroy(arena);
}

int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

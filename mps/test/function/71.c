/* 
TEST_HEADER
 id = $Id$
 summary = create 1000 spaces and destroy each one just before the next is created
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"

static void test(void)
{
 mps_arena_t arena;

 int p;

 die(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create");

 for (p=0; p<1000; p++)
 {
  mps_arena_destroy(arena);
  die(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create");
  comment("%i", p);
 }
}

int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

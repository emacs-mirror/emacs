/* 
TEST_HEADER
 id = $Id$
 summary = create many arenas and destroy each one just after the next is created
 language = c
 link = testlib.o
 parameters = ARENAS=1000
END_HEADER
*/

#include "testlib.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_arena_t arena1;

 int p;

 for (p=0; p<ARENAS; p++)
 {
  die(mps_arena_create(&arena, mps_arena_class_vm(), mps_args_none), "create");
  if (p > 0)
   mps_arena_destroy(arena1);
  arena1=arena;
 }
 mps_arena_destroy(arena1);
}

int main(void)
{
 run_test(test);
 pass();
 return 0;
}

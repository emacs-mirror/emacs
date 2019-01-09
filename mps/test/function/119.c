/* 
TEST_HEADER
 id = $Id$
 summary = create a VM arena with non-aligned size
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"

mps_arena_t arena;

static void test(void *stack_pointer)
{

/* create an arena that can't grow beyond 128 M */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*10+1)),
  "create arena");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");

}

int main(void)
{
 run_test(test);
 pass();
 return 0;
}

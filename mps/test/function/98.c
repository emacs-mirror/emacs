/* 
TEST_HEADER
 id = $Id$
 summary = create arenas at once until an error results!
 language = c
 link = testlib.o
OUTPUT_SPEC
 arena > 10
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"

static void test(void)
{
 mps_arena_t arena;

 int p;

 p=0;

 while (1)
 {
  die(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*10)), "create");
  p = p+1;
  report("arena", "%i", p);
 }
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

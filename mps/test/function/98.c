/* 
TEST_HEADER
 id = $HopeName$
 summary = create arenas at once until an error results!
 language = c
 link = testlib.o
OUTPUT_SPEC
 space > 10
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"

static void test(void)
{
 mps_space_t space;

 int p;

 p=0;

 while (1)
 {
  die(mps_arena_create(&space, mps_arena_class_vm(), (size_t) (1024*1024*10)), "create");
  p = p+1;
  report("space", "%i", p);
 }
}

int main(void)
{
 easy_tramp(test);
 return 0;
}

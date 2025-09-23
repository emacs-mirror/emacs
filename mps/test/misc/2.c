/* 
TEST_HEADER
 id = $Id$
 summary = access negative location.
 language = c
 link = testlib.o
 parameters = NUM=1
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 char *p;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), 64*1024uL*1024uL),
      "create arena");

 p = (char *)-NUM;
 *p = 0;
 comment("%p", *p);
}

int main(void)
{
 run_test(test);
 return 0;
}


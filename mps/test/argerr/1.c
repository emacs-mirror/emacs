/* 
TEST_HEADER
 id = $Id$
 summary = create an arena with an unaligned arena_t
 language = c
 link = testlib.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

static void test(void *stack_pointer)
{
 adie(mps_arena_create(UNALIGNED, mps_arena_class_vm(), mmqaArenaSIZE),
      "Create arena");
}

int main(void)
{
 run_test(test);
 return 0;
}

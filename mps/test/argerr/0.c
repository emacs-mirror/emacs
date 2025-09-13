/* 
TEST_HEADER
 id = $Id$
 summary = create an arena with a NULL arena_t
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = mps_arena_o != NULL
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

static void test(void *stack_pointer)
{
 adie(mps_arena_create(NULL, mps_arena_class_vm(), mmqaArenaSIZE),
      "Create arena");
}

int main(void)
{
 run_test(test);
 return 0;
}

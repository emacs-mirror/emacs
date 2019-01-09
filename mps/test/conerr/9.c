/* 
TEST_HEADER
 id = $Id$
 summary = create a format in a destroyed arena
 language = c
 link = testlib.o
OUTPUT_SPEC
 abort = true
END_HEADER
*/

#include "testlib.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_fmt_t format;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 mps_arena_destroy(arena);
 comment("Destroy arena.");

 cdie(mps_fmt_create_k(&format, arena, mps_args_none), "create format");

}

int main(void)
{
 run_test(test);
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = UNALIGNED arena for mps_root_create_thread
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
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_thread(&root, UNALIGNED, thread, stack_pointer), "thread root");
}

int main(void)
{
 run_test(test);
 return 0;
}

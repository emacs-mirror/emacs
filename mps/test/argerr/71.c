/* 
TEST_HEADER
 id = $Id$
 summary = UNALIGNED arena_t to thread_reg
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

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, UNALIGNED), "register thread");

}

int main(void)
{
 run_test(test);
 return 0;
}

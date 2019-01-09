/* 
TEST_HEADER
 id = $Id$
 summary = < AMBIG rank for mps_root_create_reg (with stack scan ambig)
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = rank == mps_rank_ambig()
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

 cdie(mps_root_create_reg(&root, arena, mps_rank_ambig()-1, 0, 
                      thread, mps_stack_scan_ambig, stack_pointer, 0),
      "root create");

}

int main(void)
{
 run_test(test);
 return 0;
}

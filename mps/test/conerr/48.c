/* 
TEST_HEADER
 id = $Id$
 summary = deregister thread twice
 language = c
 link = myfmt.o testlib.o
OUTPUT_SPEC
 assert = true
 assertcond = SigCheck Thread: thread
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_thr_t thread;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_thread_dereg(thread);
 comment("Deregistered thread again.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 run_test(test);
 return 0;
}


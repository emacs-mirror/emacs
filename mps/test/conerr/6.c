/* 
TEST_HEADER
 id = $Id$
 summary = destroy an arena which contains a root
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= global.c
 assertcond = RingIsSingle(&arenaGlobals->rootRing)
END_HEADER
*/

#include "testlib.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");
 
 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroy arena.");
}

int main(void)
{
 run_test(test);
 return 0;
}

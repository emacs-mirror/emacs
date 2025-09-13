/* 
TEST_HEADER
 id = $Id$
 summary = create register root without registering thread
 language = c
 link = myfmt.o testlib.o
OUTPUT_SPEC
 assert_or_abort = true
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_thr_t thread = (mps_thr_t)1;
 mps_root_t root;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

/*
 cdie(mps_thread_reg(&thread, arena), "register thread");
*/

 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 mps_root_destroy(root);
 comment("Destroyed root.");

/*
 mps_thread_dereg(thread);
 comment("Deregistered thread.");
*/

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 run_test(test);
 return 0;
}


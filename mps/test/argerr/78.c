/* 
TEST_HEADER
 id = $Id$
 summary = null ld for ld_add
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 assertfile P= ld.c
 assertcond = ld != NULL
END_HEADER
*/

#include "testlib.h"
#include "arg.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_ld_s ld;
 mps_thr_t thread;
 mps_addr_t p;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 mps_ld_reset(&ld, arena);

 mps_ld_add(NULL, arena, &p);
}

int main(void)
{
 run_test(test);
 return 0;
}

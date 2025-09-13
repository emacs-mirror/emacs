/* 
TEST_HEADER
 id = $Id$
 summary = NULL 1st arg to fmt_create_A
 language = c
 link = testlib.o newfmt.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = mps_fmt_o != NULL
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "arg.h"
#include "newfmt.h"

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(
  mps_fmt_create_A(NULL, arena, &fmtA),
  "create format");

}

int main(void)
{
 run_test(test);
 return 0;
}

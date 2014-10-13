/* 
TEST_HEADER
 id = $Id$
 summary = NULL arg to fmt_destroy
 language = c
 link = testlib.o newfmt.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = TESTT(Format, format)
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "arg.h"
#include "newfmt.h"

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");
 
 mps_fmt_destroy(NULL);

}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 return 0;
}

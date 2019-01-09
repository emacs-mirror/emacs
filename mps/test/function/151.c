/* 
TEST_HEADER
 id = $Id$
 summary = SNC pop-to-NULL test (request.dylan.170602)
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscsnc.h"
#include "mpsavm.h"
#include "rankfmt.h"

#define THIRTY_MEG (30uL*1024ul*1024ul)
#define OBJSIZE (1024*1024*1)
#define ITERATIONS (100)

static void test(void *stack_pointer)
{
 int i = 0;
 mps_ap_t sap;
 mps_arena_t arena;
 mps_fmt_t format;
 mps_pool_t spool;
 mps_thr_t thread;
 mps_frame_t frame;

/* create an arena that can't grow beyond 30 M */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)THIRTY_MEG),
  "create arena");
 cdie(mps_arena_commit_limit_set(arena, (size_t)THIRTY_MEG),
  "commit limit set");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&spool, arena, mps_class_snc(), format),
  "create SNC pool");

 cdie(
  mps_ap_create(&sap, spool, mps_rank_exact()),
  "create ap");

/* repeatedly push, alloc 1MB object, and pop to first stack frame.
   This shouldn't use much more than 1MB of memory.
*/

 for (i=0; i < ITERATIONS; i++) {
  die(mps_ap_frame_push(&frame, sap), "push");
  (void)allocdumb(sap, OBJSIZE, mps_rank_exact());
  die(mps_ap_frame_pop(sap, frame), "pop");
  comment("%i of %i", i, ITERATIONS);
 }

 mps_arena_park(arena);
 mps_ap_destroy(sap);
 comment("Destroyed ap.");

 mps_pool_destroy(spool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");

}

int main(void)
{
 run_test(test);
 pass();
 return 0;
}

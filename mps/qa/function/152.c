/* 
TEST_HEADER
 id = $HopeName$
 summary = SNC scanning test
 language = c
 link = testlib.o rankfmt.o
OUTPUT_SPEC
 inc1 = 1
 inc2 = 0
 result = pass
END_HEADER
*/

/* This test was derived from MMQA_test_function!148.c */

/* This test uses an AMC pool and an SNC pool, with objects in the SNC
   pool keeping objects in the ANC pool alive. No ambiguous roots, so we
   can tell exactly what's been scanned and check that the SNC pool is
   working properly.
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpscsnc.h"
#include "mpsavm.h"
#include "rankfmt.h"

#define ARENA_SIZE (100ul*1024ul*1024ul)
#define BIGSIZE (10ul*1024*1024)
#define SMALLSIZE (4096)

static void test(void)
{
 mps_ap_t ap, sap;
 mps_arena_t arena;
 mps_fmt_t format;
 mps_pool_t pool, spool;
 mps_thr_t thread;
 mps_frame_t frame1;
 mycell *p, *q;
 size_t com, com1;

 formatcomments=1;
 alloccomments=1;
 fixcomments=1;

/* create an arena (no particular size limit) */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)ARENA_SIZE),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

/* because we know objects in the stack pool don't move, 
   we can do without roots. Hooray!
*/

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");
 cdie(
  mps_pool_create(&pool, arena, mps_class_amc(), format),
  "create AMC pool");
 cdie(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "create ap");
 cdie(
  mps_pool_create(&spool, arena, mps_class_snc(), format),
  "create SNC pool");
 cdie(mps_ap_create(&sap, spool, MPS_RANK_EXACT), "create ap");

 /* push, alloc, check object is scanned */

 com = mps_arena_committed(arena);
 report("com", "%ld", com);
 cdie(mps_ap_frame_push(&frame1, sap), "push");
 p = allocone(sap, 2, MPS_RANK_EXACT);
 q = allocdumb(ap, BIGSIZE, MPS_RANK_EXACT);
 setref(p, 0, q);
 q = allocdumb(ap, SMALLSIZE, MPS_RANK_EXACT);
 report("com", "%ld", mps_arena_committed(arena));
 comment("collect...");
 mps_arena_collect(arena);
 com1 = mps_arena_committed(arena);
 mps_arena_release(arena);
 report("com", "%ld", com1);
 report("inc1", "%d", (com1-com)/BIGSIZE);

 /* pop, check object isn't scanned */

 cdie(mps_ap_frame_pop(sap, frame1), "pop");
 p = allocone(sap, 2, MPS_RANK_EXACT);
 comment("collect...");
 mps_arena_collect(arena);
 com1 = mps_arena_committed(arena);
 mps_arena_release(arena);
 report("com", "%ld", com1);
 report("inc2", "%ld", (com1-com)/BIGSIZE);

 mps_ap_destroy(ap);
 mps_ap_destroy(sap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
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
 easy_tramp(test);
 pass();
 return 0;
}

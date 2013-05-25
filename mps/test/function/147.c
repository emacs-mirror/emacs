/* 
TEST_HEADER
 id = $Id$
 summary = SNC simple test
 language = c
 link = testlib.o rankfmt.o
 parameters = MAXFRAMES=100 MAXLEVOBJS=20 ITERATIONS=10000
END_HEADER
*/

#include "testlib.h"
#include "mpscsnc.h"
#include "mpsavm.h"
#include "rankfmt.h"

#define THIRTY_MEG (30uL*1024ul*1024ul)

static void test(void)
{
 int i = 0, f = 0;
 mps_ap_t sap;
 mps_arena_t arena;
 mps_fmt_t format;
 mps_pool_t spool;
 mps_thr_t thread;
 mps_frame_t frames[MAXFRAMES];
 int nobj[MAXFRAMES+1];
 mycell *p;

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

/* repeatedly push and pop stack frames, and allocate objects in them
   at random. Parameters ensure that max allocation can't get too high.
*/

 for (i=0; i < ITERATIONS; i++) {
  switch (ranint(12)) {
  case 1: case 2: case 3: case 4: case 5:
   if (f < MAXFRAMES) {
    die(mps_ap_frame_push(&frames[f], sap), "push");
    comment("push %i", f);
    f++;
    nobj[f] = 0;
   }
   break;
  case 6: case 7: case 8: case 9: case 10:
   if (nobj[f] < MAXLEVOBJS) {
    p = allocone(sap, 16, mps_rank_exact());
    setref(p, 0, NULL);
    nobj[f]++;
   }
   break;
  case 11:
   if (f>0) {
    f -= 1+ranint(1+ranint(f)); /* new f is in [0, old f) */
    die(mps_ap_frame_pop(sap, frames[f]), "pop");
    comment("pop %i", f);
   }
   break;
  }
 }

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
 easy_tramp(test);
 pass();
 return 0;
}

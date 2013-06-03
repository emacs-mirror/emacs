/*
TEST_HEADER
 id = $Id$
 summary = SNC low-memory reservoir tests with commit limit
 language = c
 harness = 2.1
 link = testlib.o rankfmt.o
OUTPUT_SPEC
 lim0 = 0
 avail0 = 0
 lim1 > 5000000
 lim1 < 6000000
 defecit1 = 0
 lim2 > 1045
 lim2 < 32768
 defecit2 = 0
 defecit3 > 8000000
 spill3 <= 0
 spill4 <= 0
 grow4 > 500000
 allocfail < 20
 failres = COMMIT_LIMIT
 spill5 <= 0
 grow5 = 0
 avail5 > 1500000
 allocfail2 > 10000
 failres2 = MEMORY
 shrink6 > 1000000
 spill6 <= 0
 completed = yes
END_HEADER
*/

#include "testlib.h"
#include "mpscsnc.h"
#include "mpsavm.h"
#include "rankfmt.h"

#define ARENA_SIZE ((size_t) 1024*1024*30)

void *stackpointer;

mps_arena_t arena;
mps_pool_t poolsnc;
mps_pool_t poolmv;
mps_thr_t thread;
mps_root_t root;

mps_fmt_t format;
mps_ap_t apsnc;

mps_root_t root;

static void test(void) {

 mycell *p, *q;
 int i;
 mps_res_t res;
 size_t lim0, avail0, lim1, avail1, commit1, lim2, avail2, commit2;
 size_t lim3, avail3, commit3, lim4, avail4, commit4;
 size_t lim5, avail5, commit5, lim6, avail6, commit6;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), ARENA_SIZE),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
  mps_stack_scan_ambig, stackpointer, 0), "create stack root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&poolsnc, arena, mps_class_snc(), format),
  "create pool");

 cdie(
  mps_ap_create(&apsnc, poolsnc, mps_rank_exact()),
  "create ap");

 report("lim0", "%d", lim0 = mps_reservoir_limit(arena));
 report("avail0",  "%d", avail0 = mps_reservoir_available(arena));
 mps_reservoir_limit_set(arena, (size_t) 0);

 mps_reservoir_limit_set(arena, (size_t) (5ul*1024*1024));
 report("lim1", "%d", lim1 = mps_reservoir_limit(arena));
 report("avail1",  "%d", avail1 = mps_reservoir_available(arena));
 report("commit1", "%d", commit1 = arena_committed_and_used(arena));
 report("defecit1", "%d", lim1-avail1);

 mps_reservoir_limit_set(arena, (size_t) (1045));
 report("lim2", "%d", lim2 = mps_reservoir_limit(arena));
 report("avail2",  "%d", avail2 = mps_reservoir_available(arena));
 report("commit2", "%d", commit2 = arena_committed_and_used(arena));
 report("defecit2", "%d", lim2-avail2);

/* set commit limit to whatever is currently committed plus 1 MB
*/

 die(mps_arena_commit_limit_set(arena, arena_committed_and_used(arena)+1024*1024), "commit limit set");
 mps_reservoir_limit_set(arena, (size_t) (10ul*1024*1024));
 report("lim3", "%d", lim3 = mps_reservoir_limit(arena));
 report("avail3",  "%d", avail3 = mps_reservoir_available(arena));
 report("commit3", "%d", commit3 = arena_committed_and_used(arena));
 report("defecit3", "%d", lim3-avail3);
 report("spill3", "%d", commit3-mps_arena_commit_limit(arena));

/* now raise it by 1/2 MB -- reservoir should grow
*/

 die(mps_arena_commit_limit_set(arena, arena_committed_and_used(arena)+512*1024), "commit limit set");
 report("lim4", "%d", lim4 = mps_reservoir_limit(arena));
 report("avail4",  "%d", avail4 = mps_reservoir_available(arena));
 report("commit4", "%d", commit4 = arena_committed_and_used(arena));
 report("grow4", "%d", avail4-avail3);
 report("spill4", "%d", commit4-mps_arena_commit_limit(arena));

/* try some allocation -- more than a small amount should fail
*/

 i = -1;
 p = NULL;
 res = MPS_RES_OK;
 while (res == MPS_RES_OK) {
  res = allocrone(&q, apsnc, 10, mps_rank_exact());
  if (res == MPS_RES_OK) {
   setref(q, 0, p);
   p = q;
  }
  i++;
 }
 report("allocfail", "%d", i);
 report_res("failres", res);
 
/* available shouldn't have changed since before allocation
*/

 report("lim5", "%d", lim5 = mps_reservoir_limit(arena));
 report("avail5",  "%d", avail5 = mps_reservoir_available(arena));
 report("commit5", "%d", commit5 = arena_committed_and_used(arena));
 report("grow5", "%d", avail5-avail4);
 report("spill5", "%d", commit5-mps_arena_commit_limit(arena));

/* try some allocation from reservoir -- not much should fail
*/

 i = -1;
 res = MPS_RES_OK;
 while (res == MPS_RES_OK) {
  res = reservoir_allocrone(&q, apsnc, 10, mps_rank_exact());
  if (res == MPS_RES_OK) {
   setref(q, 0, p);
   p = q;
  }
  i++;
 }
 report("allocfail2", "%d", i);
 report_res("failres2", res);
 
/* available should have changed now
*/

 report("lim6", "%d", lim6 = mps_reservoir_limit(arena));
 report("avail6",  "%d", avail6 = mps_reservoir_available(arena));
 report("commit6", "%d", commit6 = arena_committed_and_used(arena));
 report("spill6", "%d", commit6-mps_arena_commit_limit(arena));
 report("shrink6", "%d", avail5-avail6);
 
 mps_root_destroy(root);
 comment("Destroyed root.");

 mps_ap_destroy(apsnc);
 comment("Destroyed ap.");

 mps_pool_destroy(poolsnc);
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
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 report("completed", "yes");
 return 0;
}

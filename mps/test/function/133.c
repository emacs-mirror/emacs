/* 
TEST_HEADER
 id = $Id$
 summary = low-memory reservoir tests with commit limit, part II
 language = c
 link = testlib.o rankfmt.o
OUTPUT_SPEC
 allocfail3 > 3000
 failres3 = COMMIT_LIMIT
 spill8 <= 0
 spill9 <= 0
 grow9 > 1000000
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"

#define ARENA_SIZE ((size_t) 1024*1024*30)

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

void *stackpointer;

mps_arena_t arena;
mps_pool_t poolamc;
mps_pool_t poolmv;
mps_thr_t thread;
mps_root_t root;

 mps_chain_t chain;
mps_fmt_t format;
mps_ap_t apamc;

mps_root_t root;

static void test(void) {

 mycell *p, *q;
 int i;
 mps_res_t res;
 size_t lim7, avail7, commit7, lim8, avail8, commit8;
 size_t lim9, avail9, commit9;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), ARENA_SIZE),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
  mps_stack_scan_ambig, stackpointer, 0), "create stack root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(
  mps_pool_create(&poolamc, arena, mps_class_amc(), format, chain),
  "create pool");

 cdie(
  mps_ap_create(&apamc, poolamc, mps_rank_exact()),
  "create ap");

 mps_arena_commit_limit_set(arena, mps_arena_committed(arena)+1024*1024);

 mps_reservoir_limit_set(arena, 0);
 report("lim7", "%d", lim7 = mps_reservoir_limit(arena));
 report("avail7",  "%d", avail7 = mps_reservoir_available(arena));
 report("commit7", "%d", commit7 = mps_arena_committed(arena));
 
/* available should be zero, but should be able to allocate with
   reservoir permit, until commit_limit is reached
*/

 i = -1;
 p = NULL;
 res = MPS_RES_OK;
 while (res == MPS_RES_OK) {
  res = reservoir_allocrone(&q, apamc, 10, mps_rank_exact());
  if (res == MPS_RES_OK) {
   setref(q, 0, p);
   p = q;
  }
  i++;
 }
 report("allocfail3", "%d", i);
 report_res("failres3", res);

/* should be none left to make available */

 mps_reservoir_limit_set(arena, 10ul*1024*1024);
 report("lim8", "%d", lim8 = mps_reservoir_limit(arena));
 report("avail8",  "%d", avail8 = mps_reservoir_available(arena));
 report("commit8", "%d", commit8 = mps_arena_committed(arena));
 report("spill8", "%d", commit8-mps_arena_commit_limit(arena));

/* throw away objects and collect world */

 p = NULL;
 q = NULL;
 mps_root_destroy(root);
 mps_arena_collect(arena);

/* available should have gone up now */

 report("lim9", "%d", lim9 = mps_reservoir_limit(arena));
 report("avail9",  "%d", avail9 = mps_reservoir_available(arena));
 report("commit9", "%d", commit9 = mps_arena_committed(arena));
 report("grow9", "%d", avail9-avail8);
 report("spill9", "%d", commit9-mps_arena_commit_limit(arena));
 
/* destroy everything remaining
*/

 mps_arena_park(arena);
 mps_ap_destroy(apamc);
 comment("Destroyed ap.");

 mps_pool_destroy(poolamc);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_chain_destroy(chain);
 comment("Destroyed chain.");

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
 report("result", "pass");
 return 0;
}

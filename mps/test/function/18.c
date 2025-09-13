/* 
TEST_HEADER
 id = $Id$
 summary = create lots of pools at once (and cause to run out of memory)
 language = c
 link = testlib.o newfmt.o
OUTPUT_SPEC
 errtext = create AMC pool: COMMIT_LIMIT
END_HEADER
*/

#include "testlib.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpscmvff.h"
#include "newfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_fmt_t format;
 mps_chain_t chain;
 mps_root_t root;
 mps_addr_t q;
 mps_res_t res;
 int p;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");
 die(mps_arena_commit_limit_set(arena, 1ul << 30), "commit_limit_set");
 die(mps_thread_reg(&thread, arena), "register thread");
 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root"); die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 die(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none), "pool");

 do {
  res = mps_alloc(&q, pool, 64*1024);
 } while (res==MPS_RES_OK);

 p = 0;

 while (1) {
  p++;
  die(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
      "create AMC pool");
  report("pool", "%i", p);
 }

 asserts(1, "Unreachable!");
}


int main(void)
{
 run_test(test);
 pass();
 return 0;
}

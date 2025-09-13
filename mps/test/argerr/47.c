/* 
TEST_HEADER
 id = $Id$
 summary = NULL pool to mps_ap_create
 language = c
 link = testlib.o newfmt.o
OUTPUT_SPEC
 assert = true
 assertfile P= mpsi.c
 assertcond = TESTT(Pool, pool)
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "newfmt.h"
#include "arg.h"

#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_chain_t chain;
 mps_fmt_t format;
 mps_ap_t ap;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 formatcomments = 0;

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(
  mps_pool_create(&pool, arena, mps_class_amc(), format, chain),
  "create pool");

 cdie(
  mps_ap_create(&ap, NULL, mps_rank_exact()),
  "create ap");

}

int main(void)
{
 run_test(test);
 return 0;
}

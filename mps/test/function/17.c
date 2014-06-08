/* 
TEST_HEADER
 id = $Id$
 summary = create and destroy lots of pools (interleaved)
 language = c
 link = testlib.o newfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "newfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_pool_t pool1;
 mps_thr_t thread;
 mps_fmt_t format;
 mps_chain_t chain;

 int p;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");
 die(mps_thread_reg(&thread, arena), "register thread");
 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&pool1, arena, mps_class_amc(), format, chain),
     "create pool");

 for (p = 0; p < 10000; p++) {
   die(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
       "create pool");
   comment("%i", p);
   mps_pool_destroy(pool1);
   pool1=pool;
 }

 mps_arena_park(arena);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}


int main(void)
{
 easy_tramp(test);
 pass();
 return 0;
}

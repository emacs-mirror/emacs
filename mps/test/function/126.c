/* 
TEST_HEADER
 id = $Id$
 summary = extensible arena test (huge objects)
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;

 mycell *b;

 /* create an arena that can't grow beyond 1 Mb */
 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*1)),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(mps_root_create_table(&root1, arena, mps_rank_ambig(), 0,
                            (mps_addr_t*)&exfmt_root, 1),
      "create table root");

 cdie(mps_fmt_create_A(&format, arena, &fmtA),
      "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
     "create pool");

 cdie(mps_ap_create(&ap, pool, mps_rank_exact()),
      "create ap");

 comment("ready");

 comment("reserved %ld, committed %ld",
         mps_arena_reserved(arena), mps_arena_committed(arena));

 b = allocdumb(ap, 1024ul*1024ul*40, mps_rank_exact());
 comment("alloc 40 MB at %p", b);

 comment("reserved %ld, committed %ld",
         mps_arena_reserved(arena), mps_arena_committed(arena));

 b = allocdumb(ap, 1024ul*1024ul*40, mps_rank_exact());
 comment("alloc 80 MB at %p", b);

 comment("reserved %ld, committed %ld",
         mps_arena_reserved(arena), mps_arena_committed(arena));

 mps_arena_collect(arena);
 comment("collected");

 comment("reserved %ld, committed %ld",
         mps_arena_reserved(arena), mps_arena_committed(arena));

 mps_arena_park(arena);
 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_root_destroy(root1);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 run_test(test);
 pass();
 return 0;
}

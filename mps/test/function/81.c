/* 
TEST_HEADER
 id = $Id$
 summary = run out of memory while collecting
 language = c
 link = testlib.o fastfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;

 mycell *a, *b;

 int j;

 /* create an arena that can't grow beyond 15 Mb */
 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)(1024*1024*15)),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
                          mps_stack_scan_ambig, stackpointer, 0),
      "create root");

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

 b = allocone(ap, 100, mps_rank_exact());

 /* allocate at least 100 * 10K = 10M */

 for (j = 0; j < 100; j++) {
  comment("%i of 100.", j);
  a = allocone(ap, 2, mps_rank_exact());
  setref(a, 0, b);
  b = a;
  a = allocdumb(ap, (size_t) (1024*100), mps_rank_exact());
  setref(b, 1, a);
 }

 /* now collect world. There's isn't enough space to copy everything! */

 mps_arena_collect(arena);

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
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = lots of allocation to provoke mmap error in sunos
 language = c
 link = myfmt.o testlib.o
END_HEADER
*/

/* this is same as test 3.c, with input of 0x4000
   and a limit of 100 not 10000 iterations
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "myfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


static void test(void *stack_pointer)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;

 mycell *a,*b,*c;
 size_t inpsize;

 int i,j;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)(1024*1024*20)),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(mps_root_create_thread(&root, arena, thread, stack_pointer), "thread root");
 cdie(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 formatcomments = 0;

 cdie(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
      "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 inpsize=0x4000;

 for (j=0; j<100; j++) {
   a = allocone(ap, 0, NULL, NULL, inpsize);
   b = a;

   for (i=1; i<100; i++) {
     c = allocone(ap, i, NULL, NULL, inpsize);
     b->ref[0] = c;
     b = c;
   }

   comment("%d: %p", j, a);
 }

 mps_arena_park(arena);
 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
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

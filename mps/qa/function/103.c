/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!103.c(trunk.3) $
 summary = more low memory tests with AMC (using MV)
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpscmv.h"
#include "mpsavm.h"
#include "rankfmt.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;

mps_pool_t poolmv;
mps_arena_t arena;


static void fillup(void)
{
 size_t size;
 mps_addr_t a;
 char *b;

 mps_pool_create(&poolmv, arena, mps_class_mv(), 64, 64, 64);
 size=1024ul*1024ul;
 while (size) {
  while (mps_alloc(&a, poolmv, size)==MPS_RES_OK) {
   for(b=a; b<(char *)a+size; b++) {
    *b = 97;
   }
  }
  size = size / 2;
 }
}


static void empty(void)
{
 mps_pool_destroy(poolmv);
}


static void test(void)
{
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;

 mycell *a, *b;
 mps_addr_t addr;

 mps_res_t res;
 int j;

 /* create an arena that can't grow beyond 30 M */
 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*30)),
      "create arena");
 cdie(mps_arena_commit_limit_set(arena, (size_t) (1024*1024*30)), "limit");

 die(mps_thread_reg(&thread, arena), "register thread");
 die(mps_root_create_reg(&root, arena, MPS_RANK_AMBIG, 0, thread,
                         mps_stack_scan_ambig, stackpointer, 0),
     "create root");

 cdie(mps_root_create_table(&root1, arena, MPS_RANK_AMBIG, 0,
                            (mps_addr_t *)&exfmt_root, 1),
      "create table root");

 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
     "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

/* allocate 16 M of (live) stuff */

 b = allocone(ap, 2, MPS_RANK_EXACT);
 for (j=0; j<160; j++) {
  a = allocone(ap, 2, MPS_RANK_EXACT);
  setref(a, 0, b);
  b = allocdumb(ap, 1024*100, MPS_RANK_EXACT);
  setref(a, 1, b);
  b = a;
 }

 comment("created 16M of live objects");

 for (j=0; j<1000; j++) {
  res=allocrdumb(&a, ap, 1024*1024, MPS_RANK_EXACT);
 }

 fillup();

 comment("finalizing...");

 addr = a;
 for (j=0; j<100; j++) {
  comment(err_text(mps_finalize(arena, addr)));
 }

 comment("try to make collectm by allocating another 1G...");
 
 empty();

 for (j=0; j<1000*1024; j++) {
  res=allocrdumb(&a, ap, 1024, MPS_RANK_EXACT);
  if (res == MPS_RES_OK) {
   comment("%i ok", j);
  } else {
   break;
  }
 }

 comment("collect world...");

 for (j=0; j<100; j++) {
  mps_arena_collect(arena);
 }

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

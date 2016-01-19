/* 
TEST_HEADER
 id = $Id$
 summary = ensure allocation in MV pool causes collection
 language = c
 link = newfmt.o testlib.o
OUTPUT_SPEC
 diff23 < 5
END_HEADER
*/

/* this is same as test 3.c, with input of 0x4000
   and a limit of 100 not 10000 iterations
*/

#include "testlib.h"
#include "mpscmv.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "newfmt.h"

#define EXTEND_BY ((size_t)(1024*128))
#define MEAN_SIZE ((size_t)(1024*64))
#define MAX_SIZE ((size_t)(1024*1024))
#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t poolMV, poolAMC;
 mps_thr_t thread;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t ap;
 mps_res_t r;

 mycell *a;
 mps_addr_t p;

 int i;
 int s1, s2, s3;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 formatcomments = 0;

 die(mmqa_pool_create_chain(&poolAMC, arena, mps_class_amc(), format, chain),
      "create pool");

 cdie(
  mps_ap_create(&ap, poolAMC, mps_rank_exact()),
  "create ap");

 comment("Sizes in megabytes:");

 die(mps_pool_create(&poolMV, arena, mps_class_mv(),
                     EXTEND_BY, MEAN_SIZE, MAX_SIZE),
     "create MV pool");
 i = 0;
 while ((r=mps_alloc(&p, poolMV, 1024*1024)) == 0) i++;
 report("refuse1", "%s", err_text(r));
 report("size1", "%i", i);
 s1 = i;
 mps_pool_destroy(poolMV);

 die(mps_pool_create(&poolMV, arena, mps_class_mv(),
                     EXTEND_BY, MEAN_SIZE, MAX_SIZE),
     "create MV pool");
 i = 0;
 while ((r=mps_alloc(&p, poolMV, 1024*1024)) == 0) i++;
 report("refuse2", "%s", err_text(r));
 report("size2", "%i", i);
 s2 = i;
 mps_pool_destroy(poolMV);

 a = allocdumb(ap, 1024*1024*30); /* allocate 30 M object */

 die(mps_pool_create(&poolMV, arena, mps_class_mv(),
                     EXTEND_BY, MEAN_SIZE, MAX_SIZE),
     "create MV pool");
 i=0;
 while ((r=mps_alloc(&p, poolMV, 1024*1024)) == 0) i++;
 report("refuse3", "%s", err_text(r));
 report("size3", "%i", i);
 s3 = i;

 report("diff12", "%i", s1-s2);
 report("diff23", "%i", s2-s3);

 for(i = 0; i < 10; i++) {
  r = mps_alloc(&p, poolMV, 1024*1024);
  report("refuse4", "%s", err_text(r));
 }

 mps_arena_park(arena);
 mps_pool_destroy(poolMV);

 mps_ap_destroy(ap);

 mps_pool_destroy(poolAMC);
 comment("Destroyed pool.");

 mps_chain_destroy(chain);
 mps_fmt_destroy(format);
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

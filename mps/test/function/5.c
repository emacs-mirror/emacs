/* 
TEST_HEADER
 id = $Id$
 summary = (regression test) have an ambiguous reference to a reserved but not committed object, and then allocate lots more with another AP, to make it collect
 language = c
 link = testlib.o newfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "newfmt.h"


#define OBJ_SIZE (MPS_PF_ALIGN*32)
#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t apA;
 mps_ap_t apB;

 mps_addr_t p;
 mycell *q;

 int i;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");
 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 formatcomments = 0;

 cdie(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
      "create pool");

 cdie(
  mps_ap_create(&apA, pool, mps_rank_exact()), "create apA");
 cdie(
  mps_ap_create(&apB, pool, mps_rank_exact()), "create apB");


 die(mps_reserve(&p, apA, OBJ_SIZE), "Reserve: ");

 for (i=1; i<10000; i++)
 {
  allocone(apB, 100);
 }

 q = p;
 q->data.tag = MCdata;
 q->data.id = 0;
 q->data.numrefs = 0;
 q->data.size = OBJ_SIZE;
 (void) mps_commit(apA, p, OBJ_SIZE);

 mps_ap_destroy(apA);
 comment("Destroyed apA.");
 mps_ap_destroy(apB);
 comment("Destroyed apB.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_chain_destroy(chain);
 comment("Destroyed chain.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 comment("Destroyed root.");

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
 pass();
 return 0;
}

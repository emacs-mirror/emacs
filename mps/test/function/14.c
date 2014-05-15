/* 
TEST_HEADER
 id = $Id$
 summary = regression test for bug when commit fails
 language = c
 link = testlib.o newfmt.o
END_HEADER
*/

/*
   This is a regression test for a bug found by test f12.
   Get a segment with an ambiguous reference to it, and get
   an object X with only exact references to it. Reserve an object Y
   and init it to point to the object X. Then cause collections
   until X gets moved. Reference in Y is out-of-date. Commit Y
   (should fail) and then cause collection, hoping to trick MM
   into scanning Y.
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "newfmt.h"


void *stackpointer;


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_ap_t apA, apB;
 mps_fmt_t format;
 mps_chain_t chain;

 mycell *ambigref;

 size_t bytes;
 size_t alignment;
 mps_addr_t q;

 int i;

 formatcomments = 1;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
                          mps_stack_scan_ambig, stackpointer, 0),
      "create root");

 cdie(mps_fmt_create_A(&format, arena, &fmtA), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(mmqa_pool_create_chain(&pool, arena, mps_class_amc(), format, chain),
      "create pool");

 die(mps_ap_create(&apA, pool, mps_rank_exact()), "create apA");
 die(mps_ap_create(&apB, pool, mps_rank_exact()), "create apB");

 bytes = offsetof(struct data, ref) + sizeof(struct refitem);
 alignment = MPS_PF_ALIGN;
 bytes = (bytes+alignment-1)&~(alignment-1);
 die(mps_reserve(&q, apB, bytes), "reserve: ");

 comment("Reserve");
 ambigref = q;
 ambigref->data.tag = MCdata;
 ambigref->data.id = MCerrorid;
 ambigref->data.numrefs = 0;
 ambigref->data.size = bytes;

 comment("Midallocation");
 for(i = 0; i < 40; i++) {
  allocdumb(apA, 1024*256);
 }

 comment("Commit");
 asserts(mps_commit(apB, q, bytes) == 0, "Commit succeeded!");

 comment("Postallocation");
 for(i = 0; i < 40; i++) {
  allocdumb(apA, 1024*256);
 }

 comment("Finished");

 mps_arena_park(arena);
 mps_ap_destroy(apA);
 mps_ap_destroy(apB);

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
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

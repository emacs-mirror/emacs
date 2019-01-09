/* 
TEST_HEADER
 id = $Id$
 summary = test of location dependencies
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscmvff.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"
#include "mpsavm.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };

#define MAXLDS 100


mps_arena_t arena;
static mycell *obj_table[MAXLDS];
static mps_ld_t lds[MAXLDS];


static void checklds(void)
{
 int i;

 for (i=0; i < MAXLDS; i++) {
  if (obj_table[i]->data.copycount != 0) {
   asserts(mps_ld_isstale(lds[i], arena, (mps_addr_t) obj_table[i]),
           "%d isn't stale but should be", i);
   if (ranint(4) == 0) {
    obj_table[i]->data.copycount = 0;
    mps_ld_reset(lds[i], arena);
    comment("reset %d", i);
    mps_ld_add(lds[i], arena, (mps_addr_t) obj_table[i]);
   }
  }
 }
}


static void test(void *stack_pointer)
{
 mps_pool_t poolmvff, poolawl, poolamc;
 mps_thr_t thread;
 mps_root_t root0, root1, root2;

 mps_addr_t p;
 mps_fmt_t format;
 mps_chain_t chain;
 mps_ap_t apawl, apamc;

 mycell *a, *b;

 int i,j;

 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t)1024*1024*30),
      "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_table(&root0, arena, mps_rank_ambig(), 0,
                            (mps_addr_t*)&exfmt_root, 1),
      "create exfmt root");
 cdie(mps_root_create_table(&root2, arena, mps_rank_exact(), 0,
                            (mps_addr_t *)obj_table, MAXLDS),
      "create table root");

 cdie(mps_root_create_thread(&root1, arena, thread, stack_pointer), "thread root");
 cdie(mps_fmt_create_A(&format, arena, &fmtA),
      "create format");

 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 cdie(mps_pool_create(&poolawl, arena, mps_class_awl(), format, getassociated),
      "create awl pool");

 cdie(mps_pool_create_k(&poolmvff, arena, mps_class_mvff(), mps_args_none),
      "create MVFF pool");

 cdie(mps_ap_create(&apawl, poolawl, mps_rank_exact()),
      "create ap");

 /* First we'll use only pool classes MV and AWL. So LDs shouldn't */
 /* go stale at all. */

 b = allocone(apawl, 5, mps_rank_exact());

 for (i=0; i < MAXLDS; i++) {
  comment("%d", i);
  mps_alloc(&p, poolmvff, sizeof(mps_ld_s));
  a = allocone(apawl, 5, mps_rank_exact());
  setref(a, 0, b);
  b = a;
  a = allocdumb(apawl, 256*1024, mps_rank_exact());
  comment("alloc");
  lds[i] = p;
  mps_ld_reset(lds[i], arena);
  comment("reset");
  if (i>0) {
   mps_ld_add(lds[i], arena, (mps_addr_t) b);
  }
  comment("add");
 }

 for (i=0; i < MAXLDS; i++) {
  comment("%d", i);
  asserts(mps_ld_isstale(lds[i], arena, p) == 0,
          "%d stale but shouldn't be", i);
 }

 /* Then use AMC, so object do move and LDs go stale. */

 die(mmqa_pool_create_chain(&poolamc, arena, mps_class_amc(), format, chain),
     "create pool");

 cdie(
  mps_ap_create(&apamc, poolamc, mps_rank_exact()),
  "create ap");

 /* Allocate MAXLDS objects, and make each LD depend on the corresponding */
 /* object. */

 for (i=0; i < MAXLDS; i++) {
  comment("%d", i);
  obj_table[i] = allocone(apamc, ranint(100), mps_rank_exact());
  mps_ld_add(lds[i], arena, (mps_addr_t) obj_table[i]);
 }

 for (i=0; i < 1000; i++) {
  comment("%d of 1000", i);
  checklds();
  for (j=0; j < 8; j++) {
   a = allocdumb(apamc, 32*1024, mps_rank_exact());
  }
 }

 mps_arena_park(arena);
 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 mps_pool_destroy(poolmvff);
 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_chain_destroy(chain);
 mps_fmt_destroy(format);

 mps_root_destroy(root0);
 mps_root_destroy(root1);
 mps_root_destroy(root2);
 comment("Destroyed roots.");

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

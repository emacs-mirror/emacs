/* 
TEST_HEADER
 id = $HopeName$
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
#include "newfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_ap_t apA, apB;
 mps_fmt_t format;

 mycell *ambigref;

 size_t bytes;
 size_t alignment;
 mps_addr_t q;

 int i;

 formatcomments = 1;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 die(mps_ap_create(&apA, pool, MPS_RANK_EXACT), "create apA");
 die(mps_ap_create(&apB, pool, MPS_RANK_EXACT), "create apB");

 bytes = offsetof(struct data, ref)+sizeof(struct refitem);
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
 for(i=0; i<40; i++)
 {
  allocdumb(apA, 1024*256);
 }

 comment("Commit");
 asserts(mps_commit(apB, q, bytes)==0, "Commit succeeded!");

 comment("Postallocation");
 for(i=0; i<40; i++)
 {
  allocdumb(apA, 1024*256);
 }

 comment("Finished");

 mps_ap_destroy(apA);
 mps_ap_destroy(apB);

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 comment("Destroyed root.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_space_destroy(space);
 comment("Destroyed space.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $HopeName$
 summary = regression test for scan of invalid obj after I=A
 language = c
 link = testlib.o newfmt.o
END_HEADER
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

 mps_word_t i;

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

 i = mps_collections(space)+2;

 while ((unsigned) mps_collections(space)<i)
 {
  allocdumb(apA, 1024*256);
 }

 apB->init = apB->alloc;

 while (mps_collections(space)<i+2)
 {
  allocdumb(apA, 1024*256);
 }

 asserts((apB->limit != 0 || mps_ap_trip(apB, q, bytes))==0,
  "Commit succeeded!");

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

/* 
TEST_HEADER
 id = $HopeName$
 summary = regression test for AWl bug (request.dylan.160094
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "mpsavm.h"
#include "rankfmt.h"

void *stackpointer;

mycell *a, *b;

static void test(void)
{
 mps_arena_t space;
 mps_pool_t poolamc, poolawl;
 mps_thr_t thread;
 mps_root_t root, rootb;

 mps_fmt_t format;
 mps_ap_t apamc, apawl;

 unsigned int i, c;

 cdie(mps_arena_create(&space, mps_arena_class_vm(), (size_t) (60ul*1024*1024)), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(mps_root_create_table(&root, space, MPS_RANK_AMBIG, 0, &b, 1),
  "creat root");

 cdie(mps_root_create_table(&rootb, space, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "create root b");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&poolamc, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_pool_create(&poolawl, space, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_ap_create(&apawl, poolawl, MPS_RANK_WEAK),
  "create ap");

 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");

 formatcomments = 0;

 b = allocone(apamc, 1024, MPS_RANK_EXACT);

 c = mps_collections(space);

 for (i=1; i<100; i++)
 {
  comment("%i of 100.", i);
  while (mps_collections(space) == c) {
   a = allocone(apamc, 1024, MPS_RANK_EXACT);
   if (ranint(5)) {
    setref(a, 0, b);
   }
   b = a;
  }
  c = mps_collections(space);
  a = allocone(apawl, 1, MPS_RANK_WEAK);
  a->data.id = 0;
  setref(a, 0, b);
 }

 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 mps_root_destroy(rootb);
 comment("Destroyed roots.");

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

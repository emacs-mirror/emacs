/* 
TEST_HEADER
 id = $HopeName$
 summary = test awl and amc pools, checking ranks work properly
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "rankfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolamc, poolawl;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t apamc, apawl, apweak;

 mycell *a, *b, *c, *d, *e, *f, *g;

 int i;
 int j;

 RC;

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
  mps_pool_create(&poolamc, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_pool_create(&poolawl, space, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_ap_create(&apweak, poolawl, MPS_RANK_WEAK),
  "create ap");

 cdie(
  mps_ap_create(&apawl, poolawl, MPS_RANK_EXACT),
  "create ap");
 
 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");

 b = allocone(apamc, 1, 1);

 for (j=1; j<10; j++)
 {
  comment("%i of 10.", j);
  UC;
  a = allocone(apawl, 5, MPS_RANK_EXACT);
  setref(b, 0, a);
  b = a;
  c = a;
  d = a;
  e = a;
  f = a;
  g = a;

  for (i=1; i<1000; i++)
  {
  UC;
   if (ranint(3) == 0) {
   c = allocone(apawl, 500, MPS_RANK_EXACT);
   } else if (ranint(2) == 0) {
   c = allocone(apweak, 500, MPS_RANK_WEAK);
   } else {
   c = allocone(apamc, 500, MPS_RANK_EXACT);
   }
   if (ranint(8) == 0) d = c;
   if (ranint(8) == 0) e = c;
   if (ranint(8) == 0) f = c;
   if (ranint(8) == 0) g = c;
  UC;
   setref(b, 0, c);
  UC;
   setref(c, 1, d);
  UC;
   setref(c, 2, e);
  UC;
   setref(c, 3, f);
  UC;
   setref(c, 4, g);
  UC;
   b = c;
  }
 DC;
 DMC;
 }

 mps_ap_destroy(apawl);
 mps_ap_destroy(apamc);
 mps_ap_destroy(apweak);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 mps_pool_destroy(poolawl);
 comment("Destroyed pools.");

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

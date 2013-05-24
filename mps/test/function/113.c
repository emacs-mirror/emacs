/* 
TEST_HEADER
 id = $HopeName$
 summary = AWL and AWL performance
 language = c
 link = testlib.o fastfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "mpsclo.h"
#include "fastfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolamc, poolawl;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_ap_t apamc, apawl;

 mycell *a, *b, *c, *d, *e, *f, *g;

 int i;
 int j;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1,space,mps_rank_ambig(),0,&exfmt_root,1),
  "create table root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&poolamc, space, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_pool_create(&poolawl, space, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_ap_create(&apawl, poolawl, mps_rank_exact()),
  "create ap");

 cdie(
  mps_ap_create(&apamc, poolamc, mps_rank_exact()),
  "create ap");

 b = allocone(apamc, 1, mps_rank_exact());

 for (j=1; j<100; j++)
 {
  comment("%i of 100.", j);
  a = allocone(apamc, 5, mps_rank_exact());
  b = a;
  c = a;
  d = a;
  e = a;
  f = a;
  g = a;

  for (i=1; i<5000; i++)
  {
   c = allocone(apamc, 20, mps_rank_exact());
   d = allocone(apawl, 20, mps_rank_exact());
   if (ranint(8) == 0) e = c;
   if (ranint(8) == 0) f = c;
   if (ranint(8) == 0) g = c;
   setref(b, 0, c);
   setref(c, 1, d);
   setref(c, 2, e);
   setref(c, 3, f);
   setref(c, 4, g);
   b = c;
  }
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
 mps_root_destroy(root1);
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

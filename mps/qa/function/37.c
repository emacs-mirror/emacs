/* $HopeName$
TEST_HEADER
 summary =  check exfmt works.
 language = c
 link = testlib.o exfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;
 mps_root_t root2;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *z[10];

 mycell **a = &z[0];
 mycell **b = &z[1];
 mycell **c = &z[2];
 mycell **d = &z[3];
 mycell **e = &z[4];
 mycell **f = &z[5];
 mycell **g = &z[6];

 int i;
 int j;

 RC;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

/*
 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");
*/

 cdie(
  mps_root_create_table(&root, space, MPS_RANK_AMBIG, 0, &z[0], 10),
  "create table root");

 cdie(
  mps_root_create_table(&root2, space, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "create exfmt root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

 for (j=1; j<10; j++)
 {
  comment("%i of 10.", j);
  UC;
  *a = allocone(ap, 5, 1);
  *b = *a;
  *c = *a;
  *d = *a;
  *e = *a;
  *f = *a;
  *g = *a;

  for (i=1; i<1000; i++)
  {
  UC;
   *c = allocone(ap, 1000, 1);
   if (ranint(8) == 0) *d = *c;
   if (ranint(8) == 0) *e = *c;
   if (ranint(8) == 0) *f = *c;
   if (ranint(8) == 0) *g = *c;
  UC;
   setref(*b, 0, *c);
  UC;
   setref(*c, 1, *d);
  UC;
   setref(*c, 2, *e);
  UC;
   setref(*c, 3, *f);
  UC;
   setref(*c, 4, *g);
  UC;
   *b = *c;
  }
 checkfrom(*a);
 }
 DC;
 DMC;

 checkfrom(*a);

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 mps_root_destroy(root2);
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

/* 
TEST_HEADER
 id = $HopeName$
 summary = mps_park and mps_amc_apply
 language = c
 link = testlib.o exfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

#define MAGICSIZE ((size_t) 10342)

void *stackpointer;
long int appcount;
long int apppadcount;

static void test_apply(mps_addr_t addr, void *V, size_t S) {
 mycell *a;

 asserts(S == MAGICSIZE, "Size didn't get passed!");
 a = addr;
 asserts(((a->tag) & 0x3) == MCdata || ((a->tag) & 0x3) == MCpad,
         "apply on non-data and non-pad object at %p", addr);
 if (((a->tag) & 0x3) == MCdata) {
  appcount += 1;
 } else {
  apppadcount +=1;
 }
}

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolamc;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_ap_t apamc;

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
  mps_root_create_table(&root1, space, MPS_RANK_AMBIG, 0, &exfmt_root, 1),
  "create exfmt root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&poolamc, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");

 b = allocone(apamc, 1, 1);

 for (j=1; j<10; j++) {
  comment("%i of 10.", j);
  a = allocone(apamc, 5, 1);
  b = a;
  c = a;
  d = a;
  e = a;
  f = a;
  g = a;
  for (i=1; i<1000; i++) {
   c = allocone(apamc, 1000, 1);
   if (ranint(8) == 0) d = c;
   if (ranint(8) == 0) e = c;
   if (ranint(8) == 0) f = c;
   if (ranint(8) == 0) g = c;
   setref(c, 0, b);
   setref(c, 1, d);
   setref(c, 2, e);
   setref(c, 3, f);
   setref(c, 4, g);
   b = c;
  }
  comment("parking...");
  mps_arena_park(space);
  for (i=1; i<1000; i++) {
   c = allocone(apamc, 1000, 1);
   if (ranint(8) == 0) d = c;
   if (ranint(8) == 0) e = c;
   if (ranint(8) == 0) f = c;
   if (ranint(8) == 0) g = c;
   setref(c, 0, b);
   setref(c, 1, d);
   setref(c, 2, e);
   setref(c, 3, f);
   setref(c, 4, g);
   b = c;
  }
  a = c;
  b = c;
  d = c;
  e = c;
  f = c;
  g = c;
  mps_arena_collect(space);
  comment("calling amc_apply:");
  checkfrom(c);
  appcount = 0;
  apppadcount = 0;
  mps_amc_apply(poolamc, &test_apply, NULL, MAGICSIZE);
  comment("finished amc_apply.");
  report("appcount", "%ld", appcount);
  report("apppadcount", "%ld", apppadcount);
  mps_arena_release(space);
  comment("released.");
  RC;
 }

 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
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
 report("result", "unknown");
 return 0;
}

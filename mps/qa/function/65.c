/* 
TEST_HEADER
 id = $HopeName$
 summary = sort-of-leak in arena_collect
 language = c
 link = testlib.o exfmt.o
OUTPUT_SPEC
 junksize = 0
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "exfmt.h"

#define MAGICSIZE ((size_t) 10342)

void *stackpointer;
long int appcount;
long int apppadcount;
long int cutoff_id;
size_t junk_size;

static void test_apply(mps_addr_t addr, void *V, size_t S) {
 mycell *a;
 long int id;

 asserts(S == MAGICSIZE, "Size didn't get passed!");
 a = addr;
 asserts(((a->tag) & 0x3) == MCdata || ((a->tag) & 0x3) == MCpad,
         "apply on non-data and non-pad object at %p", addr);
 if (((a->tag) & 0x3) == MCdata) {
  appcount += 1;
  id = getid(a);
  if (id < cutoff_id) {
   junk_size += a->data.size;
  }
 } else {
  apppadcount +=1;
 }
}

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolamc;
 mps_thr_t thread;
 mps_root_t root, root2, root3, root4, root5, root6, root7, root1;

 mps_fmt_t format;
 mps_ap_t apamc;

 typedef mycell * myroot;
 myroot a, b, c, d, e, f, g;

 int i;
 int j;

 RC;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_table(&root1, space, MPS_RANK_EXACT, 0, &a, 1),
  "root");
 cdie(
  mps_root_create_table(&root2, space, MPS_RANK_EXACT, 0, &b, 1),
  "root");
 cdie(
  mps_root_create_table(&root3, space, MPS_RANK_EXACT, 0, &c, 1),
  "root");
 cdie(
  mps_root_create_table(&root4, space, MPS_RANK_EXACT, 0, &d, 1),
  "root");
 cdie(
  mps_root_create_table(&root5, space, MPS_RANK_EXACT, 0, &e, 1),
  "root");
 cdie(
  mps_root_create_table(&root6, space, MPS_RANK_EXACT, 0, &f, 1),
  "root");
 cdie(
  mps_root_create_table(&root7, space, MPS_RANK_EXACT, 0, &g, 1),
  "root");

 cdie(
  mps_root_create_table(&root, space, MPS_RANK_EXACT, 0, &exfmt_root, 1),
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

 comment("parking...");
 mps_arena_park(space);
 
 b = allocone(apamc, 1, 1);

 for (j=1; j<10; j++) {
  comment("%i of 10.", j);
  a = allocone(apamc, 5, 1);
  cutoff_id = getid(a);
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
  exfmt_root = c;
  mps_arena_collect(space);
  comment(" reserved: %lu", (unsigned long) mps_arena_reserved(space));
  comment("committed: %lu", (unsigned long) mps_arena_committed(space));
  comment("calling amc_apply:");
  checkfrom(c);
  appcount = 0;
  apppadcount = 0;
  junk_size = 0;
  mps_amc_apply(poolamc, &test_apply, NULL, MAGICSIZE);
  comment("finished amc_apply");
  report("junksize", "%lu", (unsigned long) junk_size);
  report("appcount", "%ld", appcount);
  report("apppadcount", "%ld", apppadcount);
  RC;
 }

 mps_arena_release(space);
 comment("released.");

 mps_ap_destroy(apamc);
 comment("Destroyed aps.");

 mps_pool_destroy(poolamc);
 comment("Destroyed pools.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 mps_root_destroy(root1);
 mps_root_destroy(root2);
 mps_root_destroy(root3);
 mps_root_destroy(root4);
 mps_root_destroy(root5);
 mps_root_destroy(root6);
 mps_root_destroy(root7);
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

/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!44.c(trunk.4) $
 summary = clamp and space_collect tests
 language = c
 harness = 2.1
 link = testlib.o exfmt.o
OUTPUT_SPEC
 diff0 > 0
 diff1 > 0
 completed = yes
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "mpscamc.h"
#include "exfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t poolamc, poolawl;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_ap_t apamc, apawl;

 size_t size0, size1;
 mycell *a, *b, *c, *d, *e, *f, *g;

 int i;
 int j;

 RC;

 deathcomments = 0;

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
  mps_pool_create(&poolawl, space, mps_class_awl(), format),
  "create pool");

 cdie(
  mps_ap_create(&apawl, poolawl, MPS_RANK_WEAK),
  "create ap");

 cdie(
  mps_ap_create(&apamc, poolamc, MPS_RANK_EXACT),
  "create ap");

 b = allocone(apamc, 1, 1);

 for (j=1; j<10; j++) {
  comment("%i of 10.", j);
  a = allocone(apawl, 5, 1);
  setref(b, 0, a);
  b = a;
  c = a;
  d = a;
  e = a;
  f = a;
  g = a;
  for (i=1; i<1000; i++) {
   if (i%100 == 0) {
    comment("   %i", i);
   }
   if (ranint(2)) {
    c = allocone(apamc, 1000, 1);
   } else {
    c = allocone(apawl, 1000, 1);
   }
   if (ranint(8) == 0) d = c;
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
  if (j==3) {
   DC;
   comment("...collecting:");
   RC;
   size0 = arena_committed_and_used(space);
   mps_arena_collect(space);
   size1 = arena_committed_and_used(space);
   report("sizebefore0", "%lu", (unsigned long) size0);
   report("sizeafter0", "%lu", (unsigned long) size1);
   report("diff0", "%lu", (unsigned long) size0-size1);
   DC;
   mps_arena_release(space);
   comment("...released");
  }
  DC;
  comment("clamping...");
  mps_arena_park(space);
  RC;
  for (i=1; i<1000; i++) {
   if (i%100 == 0) {
    comment("   %i", i);
   }
   if (ranint(2)) {
    c = allocone(apamc, 1000, 1);
   } else {
    c = allocone(apawl, 1000, 1);
   }
   if (ranint(8) == 0) d = c;
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
  asserts(counters[COPY_COUNT]==0, "Eppur si muove!");
  DC;
  if (j==9) {
   comment("collecting...");
   size0 = arena_committed_and_used(space);
   mps_arena_collect(space);
   size1 = arena_committed_and_used(space);
   report("sizebefore1", "%lu", (unsigned long) size0);
   report("sizeafter1", "%lu", (unsigned long) size1);
   report("diff1", "%lu", (unsigned long) size0-size1);
   DC;
  }
  mps_arena_release(space);
  comment("released.");
  RC;
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

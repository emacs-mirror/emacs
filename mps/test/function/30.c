/* 
TEST_HEADER
 id = $Id$
 summary = test my format for checking the graph
 language = c
 link = testlib.o awlfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscawl.h"
#include "awlfmt.h"
#include "mpsavm.h"


void *stackpointer;


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a, *b, *c, *d, *e, *f, *g;

 int i;
 int j;

 RC;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 die(mps_thread_reg(&thread, arena), "register thread");
 die(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
                         mps_stack_scan_ambig, stackpointer, 0),
     "create root");

 cdie(mps_fmt_create_A(&format, arena, &fmtA), "create format");

 die(mps_pool_create(&pool, arena, mps_class_awl(), format, getassociated),
     "create pool");

 cdie(mps_ap_create(&ap, pool, mps_rank_exact()), "create ap");

 for (j = 1; j < 100; j++) {
  comment("%i of 100.", j);
  UC;
  a = allocone(ap, 5, 1);
  b = a;
  c = a;
  d = a;
  e = a;
  f = a;
  g = a;

  for (i = 1; i < 100; i++) {
  UC;
   c = allocone(ap, 1000, 1);
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
 }
 DC;
 DMC;

 checkfrom(a);

 mps_arena_park(arena);
 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}


int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

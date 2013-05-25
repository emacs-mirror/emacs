/* 
TEST_HEADER
 id = $Id$
 summary = test my format for format_a
 language = c
 link = myfmt.o testlib.o
 manual = true
END_HEADER
*/

#include <stdio.h>
#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a,*b,*c;
 size_t inpsize;

 int i;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE), "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(
  mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, arena, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, arena, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 scanf("%lx", &inpsize);

 a = allocone(ap, 0, NULL, NULL, inpsize);
 comment("%x\n", (int) a);
 b = a;

 for (i=1; i<100; i++)
 {
  c = allocone(ap, i, NULL, NULL, inpsize);
  b->ref[0] = c;
  b = c;
  comment("%d: %lx\n", i, (mps_word_t) b);
/*  getchar();
*/
 }

 b = a;
 while (b != c)
 {
  comment("%ld: %x\n", b->data, (int) b);
  b = b->ref[0];
 }

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

 mps_fmt_destroy(format);
 comment("Destroyed format.");

 mps_root_destroy(root);
 comment("Destroyed root.");

 mps_thread_dereg(thread);
 comment("Deregistered thread.");

 mps_arena_destroy(arena);
 comment("Destroyed arena.");
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
}

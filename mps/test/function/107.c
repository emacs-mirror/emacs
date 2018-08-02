/* 
TEST_HEADER
 id = $Id$
 summary = big allocation with an AMCZ pool
 language = c
 link = lofmt.o testlib.o
 parameters = ITERATIONS=10000
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "lofmt.h"
#include <string.h>
#include "mpsavm.h"


#define genCOUNT (3)

static mps_gen_param_s testChain[genCOUNT] = {
  { 6000, 0.90 }, { 8000, 0.65 }, { 16000, 0.50 } };


void *stackpointer;
mps_ap_t ap;


static locell *string_ch(char* x)
{
 size_t len;
 locell *y;

 len = strlen(x);

 y = alloclo(ap, len+1);
 memcpy(y->data.data, x, len+1);
 y->data.len = len;

 return y;
}


static locell *conc(locell *x, locell *y)
{
 size_t l, m;
 locell *z;

 l = x->data.len;
 m = y->data.len;

 z = alloclo(ap, l+m+1);
 memcpy(z->data.data, x->data.data, l);
 memcpy(&(z->data.data[l]), y->data.data, m);
 z->data.data[l+m] = '\0';
 z->data.len = l+m;

 return z;
}


static void test(void)
{
 mps_arena_t arena;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_chain_t chain;

 locell *a;
 int i;

 alloclocomments = 0;
 allowlocopies = 0;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), mmqaArenaSIZE),
      "create arena");

 die(mps_thread_reg(&thread, arena), "register thread");
 die(mps_root_create_reg(&root, arena, mps_rank_ambig(), 0, thread,
                         mps_stack_scan_ambig, stackpointer, 0),
     "create root");

 die(mps_fmt_create_A(&format, arena, &fmtLO), "create format");
 cdie(mps_chain_create(&chain, arena, genCOUNT, testChain), "chain_create");

 die(mmqa_pool_create_chain(&pool, arena, mps_class_amcz(), format, chain),
     "create pool");

 cdie(
  mps_ap_create(&ap, pool, mps_rank_exact()),
  "create ap");

 a = string_ch("Hello there");
 (void)string_ch("Wibble wobble foo");
 (void)string_ch("Ba ");

 for (i=0; i<ITERATIONS; i++) {
  a = conc(string_ch("B"), a);
  (void)conc(string_ch("Hello there"), string_ch(" folks!"));
  (void)alloclo(ap, 0x4000);
 }

 mps_arena_park(arena);
 mps_ap_destroy(ap);
 mps_pool_destroy(pool);
 mps_chain_destroy(chain);
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

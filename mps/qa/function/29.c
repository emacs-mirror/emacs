/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!29.c(trunk.4) $
 summary = big allocation with an LO pool
 language = c
 link = lofmt.o testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpsclo.h"
#include "lofmt.h"
#include <string.h>

void *stackpointer;
mps_ap_t ap;

#define MAXLEN 1000000;


static locell *string_ch(char* x) {
 size_t len;
 locell *y;

 len = strlen(x);

 y = alloclo(ap, len+1);
 memcpy(y->data.data, x, len+1);
 y->data.len = len;

 return y;
}


static locell *conc(locell *x, locell *y) {
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


static void test(void) {
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;

 locell *a,*b,*c,*z;
 int i;

 alloclocomments = 0;
 allowlocopies = 0;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtLO),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_lo(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

 a = string_ch("Hello there");
 b = string_ch("Wibble wobble foo");
 c = string_ch("Ba ");

 for (i=0; i<10000; i++) {
  a = conc(string_ch("B"), a);
  c = conc(string_ch("Hello there"), string_ch(" folks!"));
  z = alloclo(ap, 0x4000);
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

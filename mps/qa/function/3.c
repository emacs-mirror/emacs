/* 
TEST_HEADER
 id = $HopeName$
 summary = allocate 100 items, throw away and repeat
 language = c
 link = myfmt.o testlib.o
 manual = true
END_HEADER
*/

/* you have to type in the size (in hex) of the items allocated */
/* try with 0x4000 for a bug after 35 iterations */
/* successful for 10, as a leak test */

#include "testlib.h"
#include "mpscamc.h"
#include "myfmt.h"

void *stackpointer;

static void test(void)
{
 mps_space_t space;
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a,*b,*c;
 size_t inpsize;

 int i,j;

 cdie(mps_space_create(&space), "create space");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 formatcomments = 0;

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

 scanf("%lx", &inpsize);

 for (j=0; j<10000; j++)
 {
 a = allocone(ap, 0, NULL, NULL, inpsize);
 b = a;

 for (i=1; i<100; i++)
 {
  c = allocone(ap, i, NULL, NULL, inpsize);
  b->ref[0] = c;
  b = c;
 }

 comment("%d: %x", j, (int) a);
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
 return 0;
}

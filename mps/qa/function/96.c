/* $HopeName: MMQA_test_function!96.c(trunk.2) $
TEST_HEADER
 summary = low memory tests with AMC (and using MV)
 language = c
 link = testlib.o rankfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscamc.h"
#include "mpscmv.h"
#include "mpsavm.h"
#include "rankfmt.h"

void *stackpointer;

mps_pool_t poolmv;
mps_space_t space;

static void fillup(void) {
 size_t size;
 mps_addr_t a;
 char *b;

 mps_pool_create(&poolmv, space, mps_class_mv(), 64, 64, 64);
 size=1024ul*1024ul;
 while (size) {
  while (mps_alloc(&a, poolmv, size)==MPS_RES_OK) {
   for(b=a; b<(char *)a+size; b++) {
    *b = 97;
   }
  }
  size = size / 2;
 }
}

static void empty(void) {
 mps_pool_destroy(poolmv);
}

static void test(void)
{
 mps_pool_t pool;
 mps_thr_t thread;
 mps_root_t root, root1;

 mps_fmt_t format;
 mps_ap_t ap;

 mycell *a, *b;
 mps_addr_t addr;

 mps_res_t res;
 int j;

/* create an arena that can't grow beyond 30 M */

 cdie(mps_arena_create(&space, mps_arena_class_vm(), (size_t) (1024*1024*30)),
  "create arena");

 cdie(mps_arena_commit_limit_set(space, (size_t) (1024*1024*30)), "limit");

 cdie(mps_thread_reg(&thread, space), "register thread");

 cdie(
  mps_root_create_reg(&root, space, MPS_RANK_AMBIG, 0, thread,
   mps_stack_scan_ambig, stackpointer, 0),
  "create root");

 cdie(
  mps_root_create_table(&root1,space,MPS_RANK_AMBIG,0,&exfmt_root,1),
  "create table root");

 cdie(
  mps_fmt_create_A(&format, space, &fmtA),
  "create format");

 cdie(
  mps_pool_create(&pool, space, mps_class_amc(), format),
  "create pool");

 cdie(
  mps_ap_create(&ap, pool, MPS_RANK_EXACT),
  "create ap");

/* allocate 16 M of (live) stuff */

 b = allocone(ap, 2, MPS_RANK_EXACT);
 for (j=0; j<160; j++) {
  a = allocone(ap, 2, MPS_RANK_EXACT);
  setref(a, 0, b);
  b = allocdumb(ap, 1024*100, MPS_RANK_EXACT);
  setref(a, 1, b);
  b = a;
 }

 comment("created 16M of live objects");


/*
 for (j=0; j<1000; j++) {
  res=allocrdumb(&a, ap, 1024*1024, MPS_RANK_EXACT);
 }
*/

 fillup();

 comment("finalizing...");

/*
 addr = a;
 for (j=0; j<100; j++) {
  comment(err_text(mps_finalize(space, addr)));
 }
*/

 comment("try to make collectm by allocating another 1G...");
 
 empty();

 for (j=0; j<1000*1024; j++) {
  res=allocrdumb(&a, ap, 1024, MPS_RANK_EXACT);
  if (res == MPS_RES_OK) {
   comment("%i ok", j);
  } else {
   break;
  }
 }

 comment("collect world...");

 for (j=0; j<10; j++) {
  mps_arena_collect(space);
 }

 mps_ap_destroy(ap);
 comment("Destroyed ap.");

 mps_pool_destroy(pool);
 comment("Destroyed pool.");

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

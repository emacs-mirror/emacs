/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!88.c(trunk.2) $
 summary = EPVM allocate and collect_world
 language = c
 link = testlib.o epvmfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscepvm.h"
#include "mpsavm.h"
#include "epvmfmt.h"

#define MAX_SAVE 32 
#define INIT_SAVE 0 

void *stackpointer;

psobj *a[100];

static void test(void)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_root_t root;

 mps_fmt_t format;

 mps_pool_t pool1;
 mps_epvm_save_level_t lev1;
 mps_ap_t ap1s;
 
 int i, j;

 alloccomments = 1;

 /* create an arena that can't grow beyond 64M */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*64)),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_root_create_table(&root, arena, MPS_RANK_AMBIG, 0,
                            (mps_addr_t *)&a[0], 100),
  "create root");

 cdie(mps_fmt_create_A(&format, arena, &fmtepvm), "create format");

 cdie(mps_pool_create(&pool1, arena, mps_class_epvm(),
   format, MAX_SAVE, INIT_SAVE), "create pool1");
 
 cdie(mps_ap_create(&ap1s, pool1, 0), "create ap1s");

 lev1 = INIT_SAVE;

 for (i=0; i<10; i++) {
  for (j=0; j<10000; j++) {
   a[ranint(100)] = allocepvm(ap1s, (size_t) (8+8*ranint(16)));
  }
  mps_arena_collect(arena);
  comment("collected %i", i);
 }

 mps_ap_destroy(ap1s);
 mps_pool_destroy(pool1);
 mps_fmt_destroy(format);
 mps_root_destroy(root);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
 comment("Destroyed arena");

}


int main(void) {
 void *m;
 stackpointer=&m;

 easy_tramp(test);
 pass();
 return 0;
}


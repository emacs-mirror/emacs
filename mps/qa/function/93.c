/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!93.c(trunk.3) $
 summary = EPVM time allocations with checklevels
 language = c
 link = testlib.o epvmfmt.o
END_HEADER
*/

#include "testlib.h"
#include "mpscepvm.h"
#include "mpsavm.h"
#include "epvmfmt.h"
#include <time.h>

#define MAX_SAVE 1
#define INIT_SAVE 0 

void *stackpointer;

mps_word_t CheckLevel;

#define CheckNONE 0
#define CheckSHALLOW 1
#define CheckDEEP 2

static void timealloc(mps_pool_t pool, mps_ap_t ap, size_t size, int num, int step) {
 int i, j;
 clock_t t0, t1;
 int secs;
 psobj *a;

 mps_epvm_save(pool);
 t0 = clock();
 for (j=0; j<step; j++) {
  for (i=0; i<num; i++) {
   a = allocepvm(ap, size);
  }
  t1 = clock();
  secs = 100*(t1-t0)/CLOCKS_PER_SEC;
  comment("%i, %i, %i", (j+1)*num, size*8, secs);
 }
 mps_epvm_restore(pool, 0);

}


static void test(void)
{
 mps_arena_t arena;
 mps_thr_t thread;
 mps_fmt_t format;
 mps_pool_t pool1;
 mps_ap_t ap1s;
 
 size_t size;

 asserts(clock() != -1, "processor time not available");

/* create an arena that can't grow beyond 64M */

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*64)),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_fmt_create_A(&format, arena, &fmtepvm), "create format");

 cdie(mps_pool_create(&pool1, arena, mps_class_epvm(),
   format, MAX_SAVE, INIT_SAVE), "create pool1");
 
 cdie(mps_ap_create(&ap1s, pool1, 0), "create ap1s");

 for (size=1; size<32; size+=1) {
  for (CheckLevel = CheckNONE; CheckLevel <= CheckDEEP; CheckLevel ++) {
   timealloc(pool1, ap1s, size, 10000, 1);
  }
  CheckLevel = CheckDEEP;
 }
 
 mps_ap_destroy(ap1s);
 mps_pool_destroy(pool1);
 mps_fmt_destroy(format);
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

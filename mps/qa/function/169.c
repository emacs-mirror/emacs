/* 
TEST_HEADER
 id = $HopeName: MMQA_test_function!154.c(trunk.1) $
 summary = EPVM allocate smaller segment when big one doesn't fit
 language = c
 link = testlib.o epvmfmt.o
 harness = 2.0
 parameters = RAISE=16384
END_HEADER
*/

#include "testlib.h"
#include "mpscepvm.h"
#include "mpsavm.h"
#include "epvmfmt.h"

#define MAX_SAVE 1000 
#define INIT_SAVE 12

void *stackpointer;

static void test(void)
{
 mps_arena_t arena;
 mps_thr_t thread;

 mps_fmt_t format;

 mps_pool_t pool1;
 mps_ap_t ap1s;
 
 int j;

 psobj *a;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*64)),
  "create arena");

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_fmt_create_A(&format, arena, &fmtepvm), "create format");

 cdie(mps_pool_create(&pool1, arena, mps_class_epvm(),
   format, MAX_SAVE, INIT_SAVE), "create pool1");
 
 cdie(mps_ap_create(&ap1s, pool1, 0), "create ap1s");

 /* allocate a bit to get us off the initial segment */

 for (j=0; j<1024; j++) {
  a = allocepvm(ap1s, 32);
 }

 comment("initial allocation");

 /* set commit limit to allow no more */

 cdie(mps_arena_commit_limit_set(arena, mps_arena_committed(arena)),
  "commit limit set");

 /* and allocate 4KB objects until full */
 while(allocrepvm(&a, ap1s, 4096/8) == MPS_RES_OK) {};

 /* raise commit limit by RAISE (default is 16KB) */

 cdie(mps_arena_commit_limit_set(arena, mps_arena_committed(arena)+RAISE),
  "commit limit raise");

 /* and try to allocate one more object */

 a = allocepvm(ap1s, 4096/8);

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




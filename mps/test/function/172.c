/* 
TEST_HEADER
 id = $HopeName$
 summary = Intern and Label telemetry events
 language = c
 link = testlib.o
 stdin = af_six
OUTPUT_SPEC
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscmv.h"
#include "mpsavm.h"

void *stackpointer;
mps_arena_t arena;

static void test(void)
{
 mps_thr_t thread;
 mps_pool_t pool;
 mps_addr_t a;
 int i;

 char str[] = " 0  MMQA interned symbol\t212 œ%)(*œ!)(  ";

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*10)),
  "create space");

 (void) mps_telemetry_control(0x7F, 0x7F);

 cdie(mps_thread_reg(&thread, arena), "register thread");

 cdie(mps_pool_create(&pool,arena,mps_class_mv(), 65536, 32, 65536),
  "create pool");

 mps_telemetry_label(NULL, mps_telemetry_intern("FOO"));

 for (i = 0; i < 20; i++) {
  die(mps_alloc(&a, pool, 64), "alloc");
 }

 for (i = 0; i < 10; i++) {
  die(mps_alloc(&a, pool, 128), "alloc");
  mps_telemetry_label(a, mps_telemetry_intern(str));
  str[1]++;
 }

 for (i = 0; i < 20; i++) {
  die(mps_alloc(&a, pool, 64), "alloc");
 }

 mps_pool_destroy(pool);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

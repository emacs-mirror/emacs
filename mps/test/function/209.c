/* 
TEST_HEADER
 id = $HopeName$
 summary = EPDR allocate far too much test
 language = c
 link = testlib.o
OUTPUT_SPEC
 res = MEMORY
 result = pass
END_HEADER
*/

#include "testlib.h"
#include "mpscepdl.h"
#include "mpsavm.h"

#define EXTENDBY 8192
#define AVGSIZE 8192
#define ALIGN 8
#define SIZE 4096

void *stackpointer;
mps_arena_t arena;

static void test(void)
{
 mps_thr_t thread;
 mps_pool_t pool;
 mps_res_t res;
 mps_addr_t a;
 int *b;
 int i;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*300)),
  "create space");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 die(
  mps_pool_create(&pool, arena, mps_class_epdr(),
                  EXTENDBY, AVGSIZE, ALIGN),
  "create EPDR pool");

 i = 0;

 while (MPS_RES_OK == (res = mps_alloc(&a, pool, SIZE))) {
  b = a;
  *b = 152;
  i++;
 }

 report("total", "%d", i);
 report_res("res", res);

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

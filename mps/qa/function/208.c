/* 
TEST_HEADER
 id = $HopeName$
 summary = EPDR pool-destroy speed test
 language = c
 link = testlib.o
END_HEADER
*/

#include "testlib.h"
#include "mpscepdl.h"
#include "mpsavm.h"

#define EXTENDBY 8192
#define AVGSIZE 8192
#define ALIGN 8
#define SIZE 8192

#define ITER 5000
#define NALLOC 8192 
#define SALLOC 1 

void *stackpointer;
mps_arena_t arena;

static void alloclots(mps_pool_t pool, int n) {
 int i;
 mps_addr_t a;
 for (i = 0; i < n; i++) {
  die(mps_alloc(&a, pool, SIZE), "alloc failed");
 }
}

static void test(void)
{
 mps_thr_t thread;
 mps_pool_t pool1, pool2;
 int i;

 cdie(mps_arena_create(&arena, mps_arena_class_vm(), (size_t) (1024*1024*100)),
  "create space");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 die(
  mps_pool_create(&pool1, arena, mps_class_epdr(),
                  EXTENDBY, AVGSIZE, ALIGN),
  "create EPDR pool");
 alloclots(pool1, NALLOC);

 for (i = 0; i < ITER; i++) {
  die(mps_pool_create(&pool2, arena, mps_class_epdr(),
    EXTENDBY, AVGSIZE, ALIGN), "pool create");
  alloclots(pool2, SALLOC);
  mps_pool_destroy(pool2);
 }

 mps_pool_destroy(pool1);

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

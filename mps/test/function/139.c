/* 
TEST_HEADER
 id = $Id$
 summary = MVFF alloc from emergency list test
 language = c
 link = testlib.o
END_HEADER
*/

/* MVFF will put free blocks on emergency lists if it cannot allocate
   in the CBS. There is a bug in anchovy.1 in which the emergency lists
   are not checked when allocating, so some allocs will fail needlessly.
*/ 

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"

#define COMLIMIT1 (10*1024*1024)
#define COMLIMIT2 (12*1024*1024)
#define SMALLSIZE (8)
#define NSMALL (4*1024)
#define BIGSIZE (64)
#define EXTENDBY (8*1024)
#define MAXLARGE (1000*1024)

void *stackpointer;
mps_arena_t arena;

static mps_addr_t
  largeObjects[MAXLARGE],
  smallObjects[NSMALL];

static void test(void) {
 mps_thr_t thread;
 mps_pool_t pool;
 unsigned int i;
 unsigned long nLarge;

 cdie(mps_arena_create(&arena, mps_arena_class_vmnz(),
  (size_t) (1024*1024*50)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 mps_arena_commit_limit_set(arena, COMLIMIT1);

 die(
  mps_pool_create(&pool, arena, mps_class_mvff(),
                  EXTENDBY, 8, 8, 0, 0, 1),
  "create MVFF pool");

 for (i = 0; i < NSMALL; i++) {
  die(mps_alloc(&smallObjects[i], pool, SMALLSIZE), "small alloc failed");
 }

 nLarge = 0;
 while (mps_alloc(&largeObjects[nLarge], pool, BIGSIZE) == MPS_RES_OK) {
  nLarge ++;
 }

 report("nLarge", "%lu", nLarge);

 for (i = 0; i < NSMALL; i += 2) {
  mps_free(pool, smallObjects[i], SMALLSIZE);
 }

 comment("Freed every other small object.");

 /* The CBS should be in emergency mode now. */

 mps_free(pool, largeObjects[3], BIGSIZE);
 die(mps_alloc(&largeObjects[3], pool, BIGSIZE), "free and alloc failed");

 mps_pool_destroy(pool);
 mps_thread_dereg(thread);
 mps_arena_destroy(arena);
}

int main(void) {
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

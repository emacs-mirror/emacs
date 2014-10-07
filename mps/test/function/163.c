/* 
TEST_HEADER
 id = $Id$
 summary = MVFF emergency list test, using _grain_ list
 language = c
 link = testlib.o
END_HEADER
*/

/* MVFF will put free blocks on emergency lists if it cannot allocate
   in the CBS. There is a bug in anchovy.1 in which having more than one
   block on the emergency lists that's bigger than a segment will cause
   an assertion to fire when the emergency lists are flushed.

   - set commit limit
   - allocate (a) lots of small objects
              (b) lots of big objects (until can do no more)
   - free a goodly number of non-adjacent small objects, so that
     we start to use emergency lists
   - free lots of non-adjacent big objects
   - raise commit limit
   - free or alloc to make CBS flush lists
*/ 

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"

#define COMLIMIT1 (10*1024*1024)
#define COMLIMIT2 (12*1024*1024)
#define SMALLSIZE (4)
#define NSMALL (4*1024)
#define BIGSIZE (8*1024)
#define EXTENDBY (8*1024)
#define MAXLARGE (10*1024)

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

 cdie(mps_arena_create(&arena, mps_arena_class_vm(),
  (size_t) (1024*1024*50)), "create arena");
 cdie(mps_thread_reg(&thread, arena), "register thread");

 mps_arena_commit_limit_set(arena, COMLIMIT1);

 die(
  mps_pool_create(&pool, arena, mps_class_mvff(),
                  EXTENDBY, 8, MPS_PF_ALIGN, 0, 0, 1),
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

 for (i = 0; i < nLarge; i += 2) {
  mps_free(pool, largeObjects[i], BIGSIZE);
 }

 comment("Freed every other large object.");

 /* Now there should be lots of big blocks on the CBS. */

 mps_arena_commit_limit_set(arena, COMLIMIT2);

 comment("Raised the commit limit. Will attempt free and alloc.");
 mps_free(pool, largeObjects[1], BIGSIZE);
 die(mps_alloc(&largeObjects[0], pool, BIGSIZE), "alloc failed");

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

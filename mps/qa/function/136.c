/* $HopeName: MMQA_test_function!207.c(trunk.2) $
TEST_HEADER
 summary = MVFF low-memory test; reusing space in other pool
 language = c
 link = testlib.o
END_HEADER
*/

/* Purpose:
 * This is a grey-box test intended to expose problems in the
 * interaction between MVFF and CBS, whereby MVFF can't return
 * segments to the arena when CBS can't allocate control blocks.
 *
 * This problem is believed to occur in release.epcore.minnow.1.
 *
 *
 * Strategy:
 *   - Set low commit limit.
 *   - Allocate large objects in MVFF until we run out of memory.
 *   - Free one large object.
 *   - Allocate small objects in MVFF until we run out of memory.
 *   - Free every other small object.
 * At this point, the CBS should have run out of control blocks.
 *   - Free every other large object.
 *   - Allocate in another pool.
 */ 

#include <time.h>
#include "testlib.h"
#include "mpscmvff.h"
#include "mpscmv.h"
#include "mpsavm.h"

#define MAXSMALLOBJECTS 100000
#define MAXLARGEOBJECTS 100000
#define SMALLOBJECTSIZE 8
#define LARGEOBJECTSIZE 65536

void *stackpointer;
mps_space_t space;

int comments = 0;

static mps_addr_t
  largeObjects[MAXLARGEOBJECTS],
  smallObjects[MAXSMALLOBJECTS];

static void do_test(size_t extendBy, size_t avgSize, size_t align,
                    int slotHigh, int arenaHigh, int firstFit)
{
 mps_pool_t pool, pool2;
 mps_res_t res;
 mps_addr_t p;
 int i;
 size_t nLargeObjects = 0, nSmallObjects = 0;
 size_t largeObjectSize, smallObjectSize;

  largeObjectSize = extendBy;
  smallObjectSize = align;

  die(
  mps_pool_create(&pool, space, mps_class_mvff(),
                  extendBy, avgSize, align, slotHigh, arenaHigh, firstFit),
  "create MVFF pool");

  die(
  mps_pool_create(&pool2, space, mps_class_mv(),
                  extendBy, avgSize, /* maxSize */ extendBy),
  "create MV pool");

  /* First we allocate large objects until we run out of memory. */
  for(i = 0; i < MAXLARGEOBJECTS; i++) {
    res = mps_alloc(&p, pool, largeObjectSize);
    if(res != MPS_RES_OK)
      break;
    largeObjects[nLargeObjects] = p;
    ++nLargeObjects;
  }
  asserts(res != MPS_RES_OK, "Suceeded in creating %lu objects of size %lu",
    (unsigned long)MAXLARGEOBJECTS, (unsigned long)largeObjectSize);
  asserts(nLargeObjects > 0, "Couldn't create even one object of size %lu",
    (unsigned long)largeObjectSize);

  /* Then we free one to make sure we can allocate some small objects */
  mps_free(pool, largeObjects[nLargeObjects - 1], largeObjectSize);
  --nLargeObjects;

  comment("Allocated %lu objects of size %lu",
    (unsigned long)nLargeObjects, (unsigned long)largeObjectSize);

  /* Then we allocate lots of small objects. */
  for(i = 0; i < MAXSMALLOBJECTS; i++) {
    res = mps_alloc(&p, pool, smallObjectSize);
    if(res != MPS_RES_OK)
      break;
    smallObjects[nSmallObjects] = p;
    ++nSmallObjects;
  }
  asserts(res != MPS_RES_OK, "Suceeded in creating %lu objects of size %lu",
    (unsigned long)MAXSMALLOBJECTS, (unsigned long)smallObjectSize);

  comment("Allocated %lu objects of size %lu",
    (unsigned long)nSmallObjects, (unsigned long)smallObjectSize);

  /* Then we free every other small object */
  for(i = 0; i < nSmallObjects; i += 2) {
    mps_free(pool, smallObjects[i], smallObjectSize);
    smallObjects[i] = (mps_addr_t)0;
  }

  /* The CBS should be in emergency mode now. */

  /* Then we free every other large object */
  for(i = 0; i < nLargeObjects; i += 2) {
    mps_free(pool, largeObjects[i], largeObjectSize);
    largeObjects[i] = (mps_addr_t)0;
  }

  /* Then we allocate in another pool. */
  res = mps_alloc(&p, pool2, largeObjectSize);
  asserts(res == MPS_RES_OK, 
    "Couldn't allocate one object of size %lu in second pool",
    (unsigned long)largeObjectSize);

  mps_pool_destroy(pool);
  mps_pool_destroy(pool2);
}

static void test(void)
{
  mps_thr_t thread;
  int symm;
  size_t comlimit;
  mps_bool_t slotHigh, arenaHigh, firstFit;

 cdie(mps_arena_create(&space, mps_arena_class_vm(), (size_t) (1024*1024*50)), "create space");
 cdie(mps_thread_reg(&thread, space), "register thread");

 for (comlimit = 512 *1024; comlimit >= 64 * 1024; comlimit -= 4*1024) {
  mps_arena_commit_limit_set(space, comlimit);
  report("limit", "%x", comlimit);
  symm = ranint(8);
  slotHigh = (symm >> 2) & 1;
  arenaHigh = (symm >> 1) & 1;
  firstFit = (symm & 1);

  do_test(4096, 8, 8, slotHigh, arenaHigh, firstFit);
 }

 mps_thread_dereg(thread);
 mps_arena_destroy(space);
}

int main(void)
{
 void *m;
 stackpointer=&m; /* hack to get stack pointer */

 easy_tramp(test);
 pass();
 return 0;
}

/* 
TEST_HEADER
 id = $Id$
 summary = MVFF low-memory test; failover of CBS to freelist
 language = c
 link = testlib.o
OUTPUT_SPEC
 assert = true
 limit < 160000
END_HEADER
*/

/* Purpose:
 *
 * This tests that the MVFF can continue to return blocks to the arena
 * even if its CBS can no longer allocate control blocks (by failing
 * over to use the freelist).
 *
 * This failed to work in release.epcore.anchovy.1.
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

#include "testlib.h"
#include "mpscmvff.h"
#include "mpsavm.h"


#define MAXSMALLOBJECTS (100000ul)
#define MAXLARGEOBJECTS (100000ul)


void *stackpointer;
mps_arena_t arena;

static mps_addr_t
  largeObjects[MAXLARGEOBJECTS],
  smallObjects[MAXSMALLOBJECTS];


static void do_test(size_t extendBy, size_t avgSize, size_t align,
                    int slotHigh, int arenaHigh, int firstFit)
{
  mps_pool_t pool, pool2;
  mps_res_t res = MPS_RES_OK; /* suppress warning */
  mps_addr_t p;
  unsigned int i;
  unsigned long nLargeObjects = 0, nSmallObjects = 0;
  size_t largeObjectSize, smallObjectSize;

  largeObjectSize = extendBy;
  smallObjectSize = align;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, extendBy);
    MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, avgSize);
    MPS_ARGS_ADD(args, MPS_KEY_MVFF_ARENA_HIGH, arenaHigh);
    MPS_ARGS_ADD(args, MPS_KEY_MVFF_SLOT_HIGH, slotHigh);
    MPS_ARGS_ADD(args, MPS_KEY_MVFF_FIRST_FIT, firstFit);
    /* Set SPARE to 0 as we want this pool to return memory to the
       arena as soon as it is freed so we can allocate it elsewhere. */
    MPS_ARGS_ADD(args, MPS_KEY_SPARE, 0.0);
    die(mps_pool_create_k(&pool, arena, mps_class_mvff(), args),
        "create MVFF pool");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, extendBy);
    die(mps_pool_create_k(&pool2, arena, mps_class_mvff(), args),
        "create second MVFF pool");
  } MPS_ARGS_END(args);

  /* Allocate one small object in pool2 so that its CBS gets some
     initial memory. */
  res = mps_alloc(&p, pool2, 8);
  asserts(res == MPS_RES_OK,
          "Couldn't allocate first object of size %lu in second pool",
          (unsigned long)8);

  /* First we allocate large objects until we run out of memory. */
  for(i = 0; i < MAXLARGEOBJECTS; i++) {
    res = mps_alloc(&p, pool, largeObjectSize);
    if (res != MPS_RES_OK)
      break;
    largeObjects[nLargeObjects] = p;
    ++nLargeObjects;
  }
  asserts(res != MPS_RES_OK, 
          "Unexpectedly managed to create %lu objects of size %lu",
          MAXLARGEOBJECTS, largeObjectSize);
  if (nLargeObjects < 2) {
    /* Need two large objects for the rest of the test to work */
    goto done;
  }

  /* Then we free one to make sure we can allocate some small objects */
  mps_free(pool, largeObjects[nLargeObjects - 1], largeObjectSize);
  --nLargeObjects;

  comment("Allocated %lu objects of size %lu",
    nLargeObjects, largeObjectSize);

  /* Then we allocate lots of small objects. */
  for(i = 0; i < MAXSMALLOBJECTS; i++) {
    res = mps_alloc(&p, pool, smallObjectSize);
    if (res != MPS_RES_OK)
      break;
    smallObjects[nSmallObjects] = p;
    ++nSmallObjects;
  }
  asserts(res != MPS_RES_OK, 
          "Unexpectedly managed to create %lu objects of size %lu",
          MAXSMALLOBJECTS, smallObjectSize);

  comment("Allocated %lu objects of size %lu",
          nSmallObjects, smallObjectSize);

  /* Then we free every other small object */
  for(i = 0; i < nSmallObjects; i += 2) {
    mps_free(pool, smallObjects[i], smallObjectSize);
    smallObjects[i] = (mps_addr_t)0;
  }

  /* MVFF should be failing over from the CBS to the freelist now. */
  res = mps_alloc(&p, pool2, largeObjectSize);
  asserts(res != MPS_RES_OK, "unexpectedly have some memory left");

  /* Then we free every other large object */
  for(i = 0; i < nLargeObjects; i += 2) {
    mps_free(pool, largeObjects[i], largeObjectSize);
    largeObjects[i] = (mps_addr_t)0;
  }

  /* Then we allocate in another pool. */
  res = mps_alloc(&p, pool2, largeObjectSize);
  asserts(res == MPS_RES_OK, 
          "Couldn't allocate second object of size %lu in second pool",
          (unsigned long)largeObjectSize);

 done:
  mps_pool_destroy(pool);
  mps_pool_destroy(pool2);
}


static void test(void)
{
  mps_thr_t thread;
  int symm;
  size_t grainSize = 4096;
  size_t comlimit;
  mps_bool_t slotHigh, arenaHigh, firstFit;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, 1024*1024*50);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_GRAIN_SIZE, grainSize);
    cdie(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
         "create arena");
  } MPS_ARGS_END(args);

 cdie(mps_thread_reg(&thread, arena), "register thread");

 for (comlimit = 128 * grainSize; comlimit > 0; comlimit -= grainSize) {
   mps_arena_commit_limit_set(arena, comlimit);
   report("limit", "%d", comlimit);
   symm = ranint(8);
   slotHigh = (symm >> 2) & 1;
   arenaHigh = (symm >> 1) & 1;
   firstFit = (symm & 1);

   do_test(grainSize, 8, 8, slotHigh, arenaHigh, firstFit);
 }

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

/* impl.c.arenacv: ARENA COVERAGE TEST
 *
 * $HopeName: MMsrc!arenacv.c(trunk.7) $
 * Copyright (C) 1997 Harlequin Group, all rights reserved
 *
 * .readership: MPS developers
 * .coverage: At the moment, we're only trying to cover the new code
 * (partial mapping of the page table).
 * .note.seg-size: If the page size is divisible by sizeof(SegStruct), many
 * test cases end up being essentially identical -- there just aren't that
 * many different cases then.
 * .improve.gap-below: Could test different-sized gaps below the segment
 * being allocated; this requires using two adjacent zones.
 */

#include <stdio.h>
#include <stdlib.h>
#include "mpstd.h"

#include "mpm.h"
#include "testlib.h"
#include "mpsavm.h"
#include "mpsacl.h"
#include "mpsaan.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


#define segsSIZE 500


static void testit(ArenaClass class, ...)
{
  Arena arena; Pool pool;
  Seg offsetSeg, gapSeg, newSeg, topSeg;
  Size pageSize;
  Count segsPerPage, offset, gap, new;
  int i;
  SegPrefStruct pref = *SegPrefDefault();
  RefSet refSet = (RefSet)2;
  va_list args;

  va_start(args, class);
  die(ArenaCreateV(&arena, class, args), "ArenaCreate");
  va_end(args);

  die(PoolCreate(&pool, arena, PoolClassMV(),
		 (Size)65536, (Size)32, (Size)65536),
      "PoolCreate");

  pageSize = ArenaAlign(arena);
  segsPerPage = pageSize / sizeof(SegStruct);
  printf("%ld segments per page in the page table.\n", (long)segsPerPage);

  /* Testing the behaviour with various sizes of gaps in the page table. */

  /* Assume the allocation strategy is first-fit.  The idea of the tests is */
  /* to allocate a range of pages, then deallocate a gap in the middle, */
  /* then allocate a new segment that fits in the gap with various amounts */
  /* left over.  Like this: */
  /* |-offsetSeg-||----gapSeg----||-topSeg-| */
  /* |-offsetSeg-||-newSeg-|      |-topSeg-| */
  /* This is done with three different sizes of offsetSeg, in two different */
  /* zones to ensure that all page boundary cases are tested. */
  for(i = 0; i < 2; ++i) { /* zone loop */
    for(offset = 0; offset <= 2*segsPerPage; offset += segsPerPage) {
      if(offset != 0)
        die(SegAlloc(&offsetSeg, &pref, offset * pageSize, pool,
                     /* withReservoirPermit */ FALSE),
            "offsetSeg");
      for(gap = segsPerPage+1; gap <= 3 * (segsPerPage+1);
          gap += (segsPerPage+1)) {
        die(SegAlloc(&gapSeg, &pref, gap * pageSize, pool,
                     /* withReservoirPermit */ FALSE),
            "gapSeg");
        die(SegAlloc(&topSeg, &pref, pageSize, pool,
                     /* withReservoirPermit */ FALSE),
            "topSeg");
        SegFree(gapSeg);
        for(new = 1; new <= gap; new += segsPerPage) {
          Seg seg;

          die(SegAlloc(&newSeg, &pref, new * pageSize, pool,
                       /* withReservoirPermit */ FALSE),
              "newSeg");

          /* Test segment iterators */
          die(SegFirst(&seg, arena) ? ResOK : ResFAIL, "first");
          die(SegNext(&seg, arena, SegBase(seg)) ? ResOK : ResFAIL,
              "second");
          die(SegNext(&seg, arena, SegBase(seg)) ? ResOK : ResFAIL,
              "third");
          /* There are at least three segments */
          SegNext(&seg, arena, SegBase(seg));

          SegFree(newSeg);
        }

      SegFree(topSeg);
      }
      if(offset != 0) {
	/* Test size functions */
	Addr base, limit;
	Size size;

	base = SegBase(offsetSeg);
	limit = SegLimit(offsetSeg);
	size = SegSize(offsetSeg);
	die(size == AddrOffset(base, limit) ? ResOK : ResFAIL, "size");

	SegFree(offsetSeg);
      }
    }
    SegPrefExpress(&pref, SegPrefRefSet, &refSet);
  }

  PoolDestroy(pool);
  ArenaDestroy(arena);
}


#define TEST_ARENA_SIZE              ((Size)16<<20)


int main(void)
{
  void *block;

  testit((ArenaClass)mps_arena_class_vm(), TEST_ARENA_SIZE);

  testit((ArenaClass)mps_arena_class_an(), TEST_ARENA_SIZE);

  block = malloc(TEST_ARENA_SIZE);
  die(block == NULL ? ResFAIL : ResOK, "malloc");
  testit((ArenaClass)mps_arena_class_cl(), TEST_ARENA_SIZE, (Addr)block);

  fprintf(stderr, "Conclusion:  Failed to find any defects.\n");
  return 0;
}

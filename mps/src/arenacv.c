/* impl.c.arenacv: ARENA COVERAGE TEST
 *
 * $HopeName: MMsrc!arenacv.c(MMdevel_partial_page.2) $
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
 * .warning.sunOS: Allocating and deallocating too many segments will cause
 * SunOS (4.1.2) to start to return -1 from vmunmap.  I didn't do exhaustive
 * tests, but going over 1000 is dangerous.
 */

#include <stdio.h>
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif

#include "mpm.h"
#include "testlib.h"


#define segsSIZE 500


int main(void)
{
  Space space; Pool pool;
  Seg offsetSeg, gapSeg, newSeg, topSeg;
  Size pageSize;
  Count segsPerPage, offset, gap, new;
  int i;
  SegPrefStruct pref = *SegPrefDefault();
  RefSet refSet = (RefSet)2;

  die(SpaceCreate(&space, (Addr)0, ARENA_SIZE), "SpaceCreate");
  die(PoolCreate(&pool, space, PoolClassMV(),
		 (Size)65536, (Size)32, (Size)65536),
      "PoolCreate");

  pageSize = ArenaAlign(space);
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
	die(SegAlloc(&offsetSeg, &pref, space, offset * pageSize, pool),
	    "offsetSeg");
      for(gap = segsPerPage+1; gap <= 3 * (segsPerPage+1);
	  gap += (segsPerPage+1)) {
	die(SegAlloc(&gapSeg, &pref, space, gap * pageSize, pool),
	    "gapSeg");
	die(SegAlloc(&topSeg, &pref, space, pageSize, pool),
	    "topSeg");
	SegFree(space, gapSeg);
	for(new = 1; new <= gap; new += segsPerPage) {
	  die(SegAlloc(&newSeg, &pref, space, new * pageSize, pool),
	      "newSeg");
	  SegFree(space, newSeg);
	}
	SegFree(space, topSeg);
      }
      if(offset != 0) SegFree(space, offsetSeg);
    }
    SegPrefExpress(&pref, SegPrefRefSet, &refSet);
  }

  PoolDestroy(pool);
  SpaceDestroy(space);
  fprintf(stderr, "Conclusion:  Failed to find any defects.\n");
  return 0;
}

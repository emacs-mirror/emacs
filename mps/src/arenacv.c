/* impl.c.arenacv: ARENA COVERAGE TEST
 *
 * $HopeName: !arenacv.c(trunk.12) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 *
 * .readership: MPS developers
 * .coverage: At the moment, we're only trying to cover the new code
 * (partial mapping of the page table and vm overflow).
 * .note.tract-size: If the page size is divisible by sizeof(TractStruct), many
 * test cases end up being essentially identical -- there just aren't that
 * many different cases then.
 * .improve.gap-below: Could test different-sized gaps below the tract
 * being allocated; this requires using two adjacent zones.
 */

#include <stdio.h>
#include <stdlib.h>
#include "mpstd.h"

#include "mpm.h"
#include "testlib.h"
#include "mpsavm.h"
#include "mpsacl.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


#define tractsSIZE 500



/* testAllocAndIterate  -- Test arena allocation and iteration
 *
 * .tract-seg: Test allocation and iteration, using both low-level
 * tracts and higher-level segments. To do this, contrive a set of
 * allocation and iteration functions which are interchangeable.
 *
 */

/* Type definitions for the interchangability interface */


/* AllocInfo -- interchangeable info about  allocated regions */

typedef struct AllocInfoStruct *AllocInfo;

typedef struct AllocInfoStruct {
  union {
    struct {
      Addr base;
      Size size;
      Pool pool;
    } tractData;
    struct {
      Seg seg;
    } segData;
  } the;
} AllocInfoStruct;

typedef Res (*AllocFun)(AllocInfoStruct *aiReturn, SegPref pref,
                        Size size, Pool pool);

typedef void (*FreeFun)(AllocInfo ai);

typedef Bool (*FirstFun)(AllocInfoStruct *aiReturn, Arena arena);

typedef Bool (*NextFun)(AllocInfoStruct *nextReturn, AllocInfo ai,
                        Arena arena);

typedef Count (*UnitsFun)(Count pages);

typedef void (*TestFun)(AllocInfo ai, Arena arena);

typedef void (*CopyFun)(AllocInfoStruct *toReturn, AllocInfo from);


/*  AllocatorClass -- encapsulates an allocation mechanism */

typedef struct AllocatorClassStruct *AllocatorClass;

typedef struct AllocatorClassStruct {
  AllocFun alloc;         /* allocation method */
  FreeFun free;           /* deallocation method */
  FirstFun first;         /* find first block for iteration */
  NextFun next;           /* find next block for iteration */
  UnitsFun units;         /* number of iteration objects for pages */
  TestFun test;           /* consistency check a region */
  CopyFun copy;           /* copy an AllocationInfo object */
} AllocatorClassStruct;


/* Implementation of the tract-based interchangability interface */

static Res allocAsTract(AllocInfoStruct *aiReturn, SegPref pref,
                        Size size, Pool pool)
{
  Res res;
  Addr base;
  res = ArenaAlloc(&base, pref, size, pool, FALSE);
  if (ResOK == res) {
    aiReturn->the.tractData.base = base;
    aiReturn->the.tractData.size = size;
    aiReturn->the.tractData.pool = pool;
  }
  return res;
}

static void freeAsTract(AllocInfo ai)
{
  ArenaFree(ai->the.tractData.base,
            ai->the.tractData.size,
            ai->the.tractData.pool);
}

static Bool firstAsTract(AllocInfoStruct *aiReturn, Arena arena)
{
  Bool res;
  Tract tract;
  res = TractFirst(&tract, arena);
  if (res) {
    aiReturn->the.tractData.base = TractBase(tract);
    aiReturn->the.tractData.size = ArenaAlign(arena);;
    aiReturn->the.tractData.pool = TractPool(tract);
  }
  return res;
}

static Bool nextAsTract(AllocInfoStruct *nextReturn, AllocInfo ai,
                        Arena arena)
{
  Bool res;
  Tract tract;
  res = TractNext(&tract, arena, ai->the.tractData.base);
  if (res) {
    nextReturn->the.tractData.base = TractBase(tract);
    nextReturn->the.tractData.size = ArenaAlign(arena);;
    nextReturn->the.tractData.pool = TractPool(tract);
  }
  return res;
}

static Count unitsAsTract(Count pages)
{
  return pages; /* one tract for each page */
}


static void testAsTract(AllocInfo ai, Arena arena)
{
  /* Test TractOfAddr */
  Tract tract;
  Addr base;
  Bool found;

  found = TractOfAddr(&tract, arena, ai->the.tractData.base);
  die(found ? ResOK : ResFAIL, "TractOfAddr");
  base = TractBase(tract);
  die(base == ai->the.tractData.base ? ResOK : ResFAIL, "base");

}

static void copyAsTract(AllocInfoStruct *toReturn, AllocInfo from)
{
  toReturn->the.tractData.base = from->the.tractData.base;
  toReturn->the.tractData.size = from->the.tractData.size;
  toReturn->the.tractData.pool = from->the.tractData.pool;
}

static AllocatorClassStruct allocatorTractStruct = {
  allocAsTract,
  freeAsTract,
  firstAsTract,
  nextAsTract,
  unitsAsTract,
  testAsTract,
  copyAsTract
};


/* Implementation of the segment-based interchangability interface */

static Res allocAsSeg(AllocInfoStruct *aiReturn, SegPref pref,
                      Size size, Pool pool)
{
  Res res;
  Seg seg;
  res = SegAlloc(&seg, EnsureSegClass(), pref, size, pool, FALSE);
  if (ResOK == res) {
    aiReturn->the.segData.seg = seg;
  }
  return res;
}

static void freeAsSeg(AllocInfo ai)
{
  SegFree(ai->the.segData.seg);
}

static Bool firstAsSeg(AllocInfoStruct *aiReturn, Arena arena)
{
  Bool res;
  Seg seg;
  res = SegFirst(&seg, arena);
  if (res) {
    aiReturn->the.segData.seg = seg;
  }
  return res;
}

static Bool nextAsSeg(AllocInfoStruct *nextReturn, AllocInfo ai,
                      Arena arena)
{
  Bool res;
  Seg seg;
  res = SegNext(&seg, arena, SegBase(ai->the.segData.seg));
  if (res) {
    nextReturn->the.segData.seg = seg;
  }
  return res;
}

static Count unitsAsSeg(Count pages)
{
  if (0 == pages)
    return 0; /* can't have a zero length seg */
  else
    return 1; /* one seg no matter how many pages */
}

static void testAsSeg(AllocInfo ai, Arena arena)
{
  /* Test size functions */
  Seg seg = ai->the.segData.seg;
  Addr base, limit;
  Size size;

  UNUSED(arena);
  base = SegBase(seg);
  limit = SegLimit(seg);
  size = SegSize(seg);
  die(size == AddrOffset(base, limit) ? ResOK : ResFAIL, "size");
}

static void copyAsSeg(AllocInfoStruct *toReturn, AllocInfo from)
{
  toReturn->the.segData.seg = from->the.segData.seg;
}

static AllocatorClassStruct allocatorSegStruct = {
  allocAsSeg,
  freeAsSeg,
  firstAsSeg,
  nextAsSeg,
  unitsAsSeg,
  testAsSeg,
  copyAsSeg
};


/* The main function can use either tracts or segs */

static void testAllocAndIterate(Arena arena, Pool pool,
                                Size pageSize, Count numPerPage,
                                AllocatorClass allocator)
{
  AllocInfoStruct offsetRegion, gapRegion, newRegion, topRegion;
  SegPrefStruct pref = *SegPrefDefault();
  Count offset, gap, new;
  RefSet refSet = (RefSet)2;
  int i;

  /* Testing the behaviour with various sizes of gaps in the page table. */

  /* Assume the allocation strategy is first-fit.  The idea of the tests is */
  /* to allocate a region of memory, then deallocate a gap in the middle, */
  /* then allocate a new region that fits in the gap with various amounts */
  /* left over.  Like this: */
  /* |-offsetRegion-||----gapRegion----||-topRegion-| */
  /* |-offsetRegion-||-newRegion-|      |-topRegion-| */
  /* This is done with three different sizes of offsetRegion, in two */
  /*  different zones to ensure that all page boundary cases are tested. */
  for(i = 0; i < 2; ++i) { /* zone loop */
    for(offset = 0; offset <= 2*numPerPage; offset += numPerPage) {
      if(offset != 0)
        die(allocator->alloc(&offsetRegion, &pref, offset * pageSize, pool),
            "offsetRegion");
      for(gap = numPerPage+1; gap <= 3 * (numPerPage+1);
          gap += (numPerPage+1)) {
        die(allocator->alloc(&gapRegion, &pref, gap * pageSize, pool),
            "gapRegion");
        die(allocator->alloc(&topRegion, &pref, pageSize, pool),
            "topRegion");
        allocator->free(&gapRegion);
        for(new = 1; new <= gap; new += numPerPage) {
          AllocInfoStruct thisRegion, nextRegion;
          Count regionNum, expected;
          Res enoughRegions;

          die(allocator->alloc(&newRegion, &pref, new * pageSize, pool),
              "newRegion");

          /* Test iterators */
          die(allocator->first(&thisRegion, arena) ? ResOK : ResFAIL, "first");
          regionNum = 1;
          while (allocator->next(&nextRegion, &thisRegion, arena)) {
            regionNum++;
            allocator->copy(&thisRegion, &nextRegion);
          }

          /* Should be able to iterate over at least offset, new, top */
          expected =
            allocator->units(offset) +
            allocator->units(new) +
            allocator->units(1);

          if (regionNum >= expected)
            enoughRegions = ResOK;
          else
            enoughRegions = ResFAIL;

          die(enoughRegions, "Not enough regions");

          allocator->free(&newRegion);
        }

      allocator->free(&topRegion);
      }
      if(offset != 0) {
        allocator->test(&offsetRegion, arena);
	allocator->free(&offsetRegion);
      }
    }
    SegPrefExpress(&pref, SegPrefRefSet, &refSet);
  }

}


static void testPageTable(ArenaClass class, ...)
{
  Arena arena; Pool pool;
  Size pageSize;
  Count tractsPerPage;
  va_list args;

  va_start(args, class);
  die(ArenaCreateV(&arena, class, args), "ArenaCreate");
  va_end(args);

  die(PoolCreate(&pool, arena, PoolClassMV(),
		 (Size)65536, (Size)32, (Size)65536),
      "PoolCreate");

  pageSize = ArenaAlign(arena);
  tractsPerPage = pageSize / sizeof(TractStruct);
  printf("%ld tracts per page in the page table.\n", (long)tractsPerPage);

  /* test tract allocation and iteration */
  testAllocAndIterate(arena, pool, pageSize, tractsPerPage,
                      &allocatorTractStruct);

  /* test segment allocation and iteration */
  testAllocAndIterate(arena, pool, pageSize, tractsPerPage,
                      &allocatorSegStruct);

  PoolDestroy(pool);
  ArenaDestroy(arena);
}


static Res makeArena(Arena *arenaOut, ArenaClass class, ...)
{
  va_list args;
  Res res;

  va_start(args, class);
  res = ArenaCreateV(arenaOut, class, args);
  va_end(args);
  return res;
}


/* testSize -- test arena size overflow
 *
 * Just try allocating larger arenas, doubling the size each time, until
 * it fails, then check the error code.
 */

static void testSize(Size size)
{
  ArenaClass class = (ArenaClass)mps_arena_class_vm();
  Arena arena;
  Res res;

  do {
    res = makeArena(&arena, class, size);
    if (res == ResOK)
      ArenaDestroy(arena);
    else
      die((res == ResRESOURCE) ? ResOK : res, "right error code");
    size *= 2;
  } while (size == 0);
}


#define TEST_ARENA_SIZE              ((Size)16<<22)


int main(void)
{
  void *block;

  testPageTable((ArenaClass)mps_arena_class_vm(), TEST_ARENA_SIZE);
  testPageTable((ArenaClass)mps_arena_class_vmnz(), TEST_ARENA_SIZE);

  block = malloc(TEST_ARENA_SIZE);
  die(block == NULL ? ResFAIL : ResOK, "malloc");
  testPageTable((ArenaClass)mps_arena_class_cl(), TEST_ARENA_SIZE,
                (Addr)block);

  testSize(TEST_ARENA_SIZE);

  fflush(stdout); /* synchronize */
  fprintf(stderr, "Conclusion:  Failed to find any defects.\n");
  return 0;
}

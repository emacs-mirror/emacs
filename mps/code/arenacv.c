/* arenacv.c: ARENA COVERAGE TEST
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .coverage: At the moment, we're only trying to cover the new code
 * (partial mapping of the page table and vm overflow).
 *
 * .note.tract-size: If the page size is divisible by sizeof(TractStruct), many
 * test cases end up being essentially identical -- there just aren't that
 * many different cases then.
 *
 * .improve.gap-below: Could test different-sized gaps below the tract
 * being allocated; this requires using two adjacent zones.
 */

#include "mpm.h"
#include "poolmv.h"
#include "testlib.h"
#include "mpslib.h"
#include "mpsavm.h"
#include "mpsacl.h"

#include <stdio.h> /* printf */
#include <stdlib.h> /* malloc */


#define tractsSIZE 500


/* testAllocAndIterate  -- Test arena allocation and iteration
 *
 * .tract-seg: Test allocation and iteration, using both low-level
 * tracts and higher-level segments. To do this, contrive a set of
 * allocation and iteration functions which are interchangeable.
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


/* tractSearchInChunk -- find a tract in a chunk
 *
 * .tract-search: Searches for a tract in the chunk starting at page
 * index i, return FALSE if there is none.
 */

static Bool tractSearchInChunk(Tract *tractReturn, Chunk chunk, Index i)
{
  AVER_CRITICAL(chunk->allocBase <= i);
  AVER_CRITICAL(i <= chunk->pages);

  while (i < chunk->pages
         && !(BTGet(chunk->allocTable, i)
              && PageIsAllocated(ChunkPage(chunk, i)))) {
    ++i;
  }
  if (i == chunk->pages)
    return FALSE;
  AVER(i < chunk->pages);
  *tractReturn = PageTract(ChunkPage(chunk, i));
  return TRUE;
}


/* tractSearch -- find next tract above address
 *
 * Searches for the next tract in increasing address order.
 * The tract returned is the next one along from addr (i.e.,
 * it has a base address bigger than addr and no other tract
 * with a base address bigger than addr has a smaller base address).
 *
 * Returns FALSE if there is no tract to find (end of the arena).
 */

static Bool tractSearch(Tract *tractReturn, Arena arena, Addr addr)
{
  Bool b;
  Chunk chunk;
  Tree tree;

  b = ChunkOfAddr(&chunk, arena, addr);
  if (b) {
    Index i;

    i = INDEX_OF_ADDR(chunk, addr);
    /* There are fewer pages than addresses, therefore the */
    /* page index can never wrap around */
    AVER_CRITICAL(i+1 != 0);

    if (tractSearchInChunk(tractReturn, chunk, i+1)) {
      return TRUE;
    }
  }
  while (TreeFindNext(&tree, ArenaChunkTree(arena), TreeKeyOfAddrVar(addr),
                      ChunkCompare))
  {
    chunk = ChunkOfTree(tree);
    addr = chunk->base;
    /* Start from allocBase to skip the tables. */
    if (tractSearchInChunk(tractReturn, chunk, chunk->allocBase)) {
      return TRUE;
    }
  }
  return FALSE;
}


/* Implementation of the tract-based interchangability interface */

static Res allocAsTract(AllocInfoStruct *aiReturn, SegPref pref,
                        Size size, Pool pool)
{
  Res res;
  Addr base;
  res = ArenaAlloc(&base, pref, size, pool, FALSE);
  if (res == ResOK) {
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
  res = tractSearch(&tract, arena, 0);
  if (res) {
    aiReturn->the.tractData.base = TractBase(tract);
    aiReturn->the.tractData.size = ArenaGrainSize(arena);;
    aiReturn->the.tractData.pool = TractPool(tract);
  }
  return res;
}

static Bool nextAsTract(AllocInfoStruct *nextReturn, AllocInfo ai,
                        Arena arena)
{
  Bool res;
  Tract tract;
  res = tractSearch(&tract, arena, ai->the.tractData.base);
  if (res) {
    nextReturn->the.tractData.base = TractBase(tract);
    nextReturn->the.tractData.size = ArenaGrainSize(arena);;
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
  cdie(found, "TractOfAddr");
  base = TractBase(tract);
  cdie(base == ai->the.tractData.base, "base");
 
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
  res = SegAlloc(&seg, SegClassGet(), pref, size, pool, FALSE, argsNone);
  if (res == ResOK) {
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
  res = SegNext(&seg, arena, ai->the.segData.seg);
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
  cdie(size == AddrOffset(base, limit), "size");
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
  SegPrefStruct pref;
  Count offset, gap, new;
  ZoneSet zone = (ZoneSet)2;
  int i;

  SegPrefInit(&pref);
  
  /* Testing the behaviour with various sizes of gaps in the page table. */

  /* Assume the allocation strategy is first-fit.  The idea of the tests is */
  /* to allocate a region of memory, then deallocate a gap in the middle, */
  /* then allocate a new region that fits in the gap with various amounts */
  /* left over.  Like this: */
  /* |-offsetRegion-||----gapRegion----||-topRegion-| */
  /* |-offsetRegion-||-newRegion-|      |-topRegion-| */
  /* This is done with three different sizes of offsetRegion, in two */
  /* different zones to ensure that all page boundary cases are tested. */
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
          cdie(allocator->first(&thisRegion, arena), "first");
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
    SegPrefExpress(&pref, SegPrefZoneSet, &zone);
  }
}


static void testPageTable(ArenaClass class, Size size, Addr addr, Bool zoned)
{
  Arena arena; Pool pool;
  Size pageSize;
  Count tractsPerPage;
  
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, size);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_CL_BASE, addr);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_ZONED, zoned);
    die(ArenaCreate(&arena, class, args), "ArenaCreate");
  } MPS_ARGS_END(args);

  die(PoolCreate(&pool, arena, PoolClassMV(), argsNone), "PoolCreate");

  pageSize = ArenaGrainSize(arena);
  tractsPerPage = pageSize / sizeof(TractStruct);
  printf("%ld tracts per page in the page table.\n", (long)tractsPerPage);

  /* test tract allocation and iteration */
  testAllocAndIterate(arena, pool, pageSize, tractsPerPage,
                      &allocatorTractStruct);

  /* test segment allocation and iteration */
  testAllocAndIterate(arena, pool, pageSize, tractsPerPage,
                      &allocatorSegStruct);

  die(ArenaDescribe(arena, mps_lib_get_stdout(), 0), "ArenaDescribe");
  die(ArenaDescribeTracts(arena, mps_lib_get_stdout(), 0),
      "ArenaDescribeTracts");

  PoolDestroy(pool);
  ArenaDestroy(arena);
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
    MPS_ARGS_BEGIN(args) {
      MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, size);
      res = ArenaCreate(&arena, class, args);
    } MPS_ARGS_END(args);
    if (res == ResOK)
      ArenaDestroy(arena);
    else
      die((res == ResRESOURCE) ? ResOK : res, "right error code");
    size *= 2;
  } while (size == 0);
}


#define TEST_ARENA_SIZE              ((Size)16<<22)


int main(int argc, char *argv[])
{
  void *block;

  testlib_init(argc, argv);

  testPageTable((ArenaClass)mps_arena_class_vm(), TEST_ARENA_SIZE, 0, TRUE);
  testPageTable((ArenaClass)mps_arena_class_vm(), TEST_ARENA_SIZE, 0, FALSE);

  block = malloc(TEST_ARENA_SIZE);
  cdie(block != NULL, "malloc");
  testPageTable((ArenaClass)mps_arena_class_cl(), TEST_ARENA_SIZE, block, FALSE);

  testSize(TEST_ARENA_SIZE);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* sparerrtest.c: SPARE RANGE RELEASE TEST
 *
 * $Id$
 * Copyright (c) 2026 Ravenbrook Limited.  See end of file for license.
 *
 * This tests the spareRangeRelease function in arenavm.c.  In
 * particular the case where LandDelete fails because the block pool is
 * full.
 *
 */

#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"
#include "mpslib.h"
#include "range.h"
#include "testlib.h"

#include <math.h>
#include <stdbool.h>
#include <stdio.h>

#if defined(MPS_BUILD_GC) && (__GNUC__ >= 12)
#pragma GCC diagnostic ignored "-Waggregate-return"
#endif

static RangeStruct allocPages(Pool pool, Count n)
{
  Size grainSize = ArenaGrainSize(PoolArena(pool));
  LocusPrefStruct* locus = LocusPrefDefault();
  Size size = grainSize * n;
  Addr addr;
  RangeStruct range;
  die(ArenaAlloc(&addr, locus, size, pool), "ArenaAlloc");
  RangeInitSize(&range, addr, size);
  return range;
}

static void freeRange(Pool pool, RangeStruct range)
{
  ArenaFree(RangeBase(&range), RangeSize(&range), pool);
}

static Index pageIndex(Addr addr, RangeStruct range, Arena arena)
{
  Size grainSize = ArenaGrainSize(arena);
  AVER(range.base <= addr);
  AVER(addr <= range.limit);
  AVER(AddrIsAligned(addr, grainSize));
  return AddrOffset(range.base, addr) / grainSize;
}

static void printRange(const char* s,
                       RangeStruct r,
                       RangeStruct r0,
                       Arena arena)
{
  Size grainSize = ArenaGrainSize(arena);
#define ZU PRIuLONGEST
  fprintf(stderr,
          "%s%p %p %" ZU " (%" ZU ")"
          " %" ZU " %" ZU "\n",
          s,
          (void*)RangeBase(&r),
          (void*)RangeLimit(&r),
          RangeSize(&r),
          RangeSize(&r) / grainSize,
          pageIndex(RangeBase(&r), r0, arena),
          pageIndex(RangeLimit(&r), r0, arena));
#undef ZU
}

static void freeLandDeletePage(Arena arena,
                               RangeStruct range,
                               Index page)
{
  Size grainSize = ArenaGrainSize(arena);
  Addr b, l;
  b = AddrAdd(range.base, grainSize * page);
  l = AddrAdd(b, grainSize);
  ArenaFreeLandDelete(arena, b, l);
}

static void test(Pool pool)
{
  Arena arena = PoolArena(pool);
  RangeStruct r0, r1, r2, r3;
  r0 = allocPages(pool, 20);
  freeRange(pool, r0); /* move some memory to spareLand */
  Insist(mps_arena_spare_committed(arena) > 10 * ArenaGrainSize(arena));
  printRange("r0: ", r0, r0, arena);
  /* Reserve page 2 to force a node split later. */
  freeLandDeletePage(arena, r0, 2);
  /* Here page 1 should still be available. */
  r1 = allocPages(pool, 1);
  printRange("r1: ", r1, r0, arena);
  Insist(pageIndex(r1.base, r0, arena) == 1);
  Insist(pageIndex(r1.limit, r0, arena) == 2);
  freeRange(pool, r1);
  ArenaDrainCBSBlockPool(arena, FALSE, TRUE);
  /* This allocates pages 1, 3, and 4.  A node of freeLand's splay tree
     must be split and because the CBS block pool is full, page 1 is
     used to extend that pool. */
  r2 = allocPages(pool, 2);
  printRange("r2: ", r2, r0, arena);
  Insist(pageIndex(r2.base, r0, arena) == 3);
  Insist(pageIndex(r2.limit, r0, arena) == 5);
  r3 = allocPages(pool, 1);
  /* Page 1 should no longer be available, as it is used for the CBS
     block pool. */
  Insist(pageIndex(r3.base, r0, arena) == 5);
  Insist(pageIndex(r3.limit, r0, arena) == 6);
  printRange("r3: ", r3, r0, arena);
  freeRange(pool, r2);
  freeRange(pool, r3);
}

#include "fmtscheme.h"

static void test_main(void* marker)
{
  mps_fmt_t obj_fmt;
  UNUSED(marker);

  die(mps_arena_create_k(&scheme_arena,
                         mps_arena_class_vm(),
                         mps_args_none),
      "mps_arena_create_k");

  scheme_fmt(&obj_fmt);

  MPS_ARGS_BEGIN(args)
  {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, obj_fmt);
    die(
      mps_pool_create_k(&obj_pool, scheme_arena, mps_class_amc(), args),
      "mps_pool_create_k");
  }
  MPS_ARGS_END(args);

  test(obj_pool);

  mps_arena_park(scheme_arena);
  mps_pool_destroy(obj_pool);
  mps_fmt_destroy(obj_fmt);
  mps_arena_destroy(scheme_arena);
}

int main(int argc, char* argv[])
{
  void* marker = &marker;
  testlib_init(argc, argv);

  test_main(marker);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2026 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* landtest.c: LAND TEST
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * The MPS contains two land implementations:
 *
 * 1. the CBS (Coalescing Block Structure) module maintains blocks in
 * a splay tree for fast access with a cost in storage;
 *
 * 2. the Freelist module maintains blocks in an address-ordered
 * singly linked list for zero storage overhead with a cost in
 * performance.
 */

#include "cbs.h"
#include "freelist.h"
#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpstd.h"
#include "testlib.h"

#include <stdarg.h>
#include <stdlib.h>
#include <time.h>

SRCID(landtest, "$Id$");


#define ArraySize ((Size)123456)

/* CBS is much faster than Freelist, so we apply more operations to
 * the former. */
#define nCBSOperations ((Size)125000)
#define nFLOperations ((Size)12500)

static Count NAllocateTried, NAllocateSucceeded, NDeallocateTried,
  NDeallocateSucceeded;

static int verbose = 0;

typedef struct TestStateStruct {
  Align align;
  BT allocTable;
  Addr block;
  Land land;
} TestStateStruct, *TestState;

typedef struct CheckTestClosureStruct {
  TestState state;
  Addr limit;
  Addr oldLimit;
} CheckTestClosureStruct, *CheckTestClosure;


static Addr (addrOfIndex)(TestState state, Index i)
{
  return AddrAdd(state->block, (i * state->align));
}


static Index (indexOfAddr)(TestState state, Addr a)
{
  return (Index)(AddrOffset(state->block, a) / state->align);
}


static void describe(TestState state) {
  die(LandDescribe(state->land, mps_lib_get_stdout()), "LandDescribe");
}


static Bool checkVisitor(Bool *deleteReturn, Land land, Range range,
                         void *closureP, Size closureS)
{
  Addr base, limit;
  CheckTestClosure cl = closureP;

  Insist(deleteReturn != NULL);
  testlib_unused(land);
  testlib_unused(closureS);
  Insist(cl != NULL);

  base = RangeBase(range);
  limit = RangeLimit(range);

  if (base > cl->oldLimit) {
    Insist(BTIsSetRange(cl->state->allocTable,
                        indexOfAddr(cl->state, cl->oldLimit),
                        indexOfAddr(cl->state, base)));
  } else { /* must be at start of table */
    Insist(base == cl->oldLimit);
    Insist(cl->oldLimit == cl->state->block);
  }
 
  Insist(BTIsResRange(cl->state->allocTable,
                      indexOfAddr(cl->state, base),
                      indexOfAddr(cl->state, limit)));

  cl->oldLimit = limit;

  return TRUE;
}

static void check(TestState state)
{
  CheckTestClosureStruct closure;

  closure.state = state;
  closure.limit = addrOfIndex(state, ArraySize);
  closure.oldLimit = state->block;

  LandIterate(state->land, checkVisitor, (void *)&closure, 0);

  if (closure.oldLimit == state->block)
    Insist(BTIsSetRange(state->allocTable, 0,
                        indexOfAddr(state, closure.limit)));
  else if (closure.limit > closure.oldLimit)
    Insist(BTIsSetRange(state->allocTable,
                        indexOfAddr(state, closure.oldLimit),
                        indexOfAddr(state, closure.limit)));
  else
    Insist(closure.oldLimit == closure.limit);
}


static Word fbmRnd(Word limit)
{
  /* Not very uniform, but never mind. */
  return (Word)rnd() % limit;
}


/* nextEdge -- Finds the next transition in the bit table
 *
 * Returns the index greater than <base> such that the
 * range [<base>, <return>) has the same value in the bit table,
 * and <return> has a different value or does not exist.
 */

static Index nextEdge(BT bt, Size size, Index base)
{
  Index end;
  Bool baseValue;

  Insist(bt != NULL);
  Insist(base < size);

  baseValue = BTGet(bt, base);

  for(end = base + 1; end < size && BTGet(bt, end) == baseValue; end++)
    NOOP;

  return end;
}


/* lastEdge -- Finds the previous transition in the bit table
 *
 * Returns the index less than <base> such that the range
 * [<return>, <base>] has the same value in the bit table,
 * and <return>-1 has a different value or does not exist.
 */

static Index lastEdge(BT bt, Size size, Index base)
{
  Index end;
  Bool baseValue;

  Insist(bt != NULL);
  Insist(base < size);

  baseValue = BTGet(bt, base);

  for(end = base; end > (Index)0 && BTGet(bt, end - 1) == baseValue; end--)
    NOOP;

  return end;
}


/* randomRange -- picks random range within table
 *
 * The function first picks a uniformly distributed <base> within the table.
 *
 * It then scans forward a binary exponentially distributed
 * number of "edges" in the table (that is, transitions between set and
 * reset) to get <end>.  Note that there is a 50% chance that <end> will
 * be the next edge, a 25% chance it will be the edge after, etc., until
 * the end of the table.
 *
 * Finally it picks a <limit> uniformly distributed in the range
 * [base+1, limit].
 *
 * Hence there is a somewhat better than 50% chance that the range will be
 * all either set or reset.
 */

static void randomRange(Addr *baseReturn, Addr *limitReturn, TestState state)
{
  Index base;   /* the start of our range */
  Index end;    /* an edge (i.e. different from its predecessor) */
                /* after base */
  Index limit;  /* a randomly chosen value in (base, limit]. */

  base = fbmRnd(ArraySize);

  do {
    end = nextEdge(state->allocTable, ArraySize, base);
  } while(end < ArraySize && fbmRnd(2) == 0); /* p=0.5 exponential */

  Insist(end > base);

  limit = base + 1 + fbmRnd(end - base);

  *baseReturn = addrOfIndex(state, base);
  *limitReturn = addrOfIndex(state, limit);
}


static void allocate(TestState state, Addr base, Addr limit)
{
  Res res;
  Index ib, il;                  /* Indexed for base and limit */
  Bool isFree;
  RangeStruct range, oldRange;
  Addr outerBase, outerLimit;    /* interval containing [ib, il) */

  ib = indexOfAddr(state, base);
  il = indexOfAddr(state, limit);

  isFree = BTIsResRange(state->allocTable, ib, il);

  NAllocateTried++;

  if (isFree) {
    Size left, right, total;       /* Sizes of block and two fragments */

    outerBase =
      addrOfIndex(state, lastEdge(state->allocTable, ArraySize, ib));
    outerLimit =
      addrOfIndex(state, nextEdge(state->allocTable, ArraySize, il - 1));

    left = AddrOffset(outerBase, base);
    right = AddrOffset(limit, outerLimit);
    total = AddrOffset(outerBase, outerLimit);

    /* TODO: check these values */
    testlib_unused(left);
    testlib_unused(right);
    testlib_unused(total);
  } else {
    outerBase = outerLimit = NULL;
  }

  RangeInit(&range, base, limit);
  res = LandDelete(&oldRange, state->land, &range);

  if (verbose) {
    printf("allocate: [%p,%p) -- %s\n",
           (void *)base, (void *)limit, isFree ? "succeed" : "fail");
    describe(state);
  }

  if (!isFree) {
    die_expect((mps_res_t)res, MPS_RES_FAIL,
               "Succeeded in deleting allocated block");
  } else { /* isFree */
    die_expect((mps_res_t)res, MPS_RES_OK,
               "failed to delete free block");
    Insist(RangeBase(&oldRange) == outerBase);
    Insist(RangeLimit(&oldRange) == outerLimit);
    NAllocateSucceeded++;
    BTSetRange(state->allocTable, ib, il);
  }
}


static void deallocate(TestState state, Addr base, Addr limit)
{
  Res res;
  Index ib, il;
  Bool isAllocated;
  Addr outerBase = base, outerLimit = limit; /* interval containing [ib, il) */
  RangeStruct range, freeRange; /* interval returned by the manager */

  ib = indexOfAddr(state, base);
  il = indexOfAddr(state, limit);

  isAllocated = BTIsSetRange(state->allocTable, ib, il);

  NDeallocateTried++;

  if (isAllocated) {
    Size left, right, total;       /* Sizes of block and two fragments */

    /* Find the free blocks adjacent to the allocated block */
    if (ib > 0 && !BTGet(state->allocTable, ib - 1)) {
      outerBase =
        addrOfIndex(state, lastEdge(state->allocTable, ArraySize, ib - 1));
    } else {
      outerBase = base;
     }

    if (il < ArraySize && !BTGet(state->allocTable, il)) {
      outerLimit =
        addrOfIndex(state, nextEdge(state->allocTable, ArraySize, il));
    } else {
      outerLimit = limit;
    }

    left = AddrOffset(outerBase, base);
    right = AddrOffset(limit, outerLimit);
    total = AddrOffset(outerBase, outerLimit);

    /* TODO: check these values */
    testlib_unused(left);
    testlib_unused(right);
    testlib_unused(total);
  }

  RangeInit(&range, base, limit);
  res = LandInsert(&freeRange, state->land, &range);

  if (verbose) {
    printf("deallocate: [%p,%p) -- %s\n",
           (void *)base, (void *)limit, isAllocated ? "succeed" : "fail");
    describe(state);
  }

  if (!isAllocated) {
    die_expect((mps_res_t)res, MPS_RES_FAIL,
               "succeeded in inserting non-allocated block");
  } else { /* isAllocated */
    die_expect((mps_res_t)res, MPS_RES_OK,
               "failed to insert allocated block");

    NDeallocateSucceeded++;
    BTResRange(state->allocTable, ib, il);
    Insist(RangeBase(&freeRange) == outerBase);
    Insist(RangeLimit(&freeRange) == outerLimit);
  }
}


static void find(TestState state, Size size, Bool high, FindDelete findDelete)
{
  Bool expected, found;
  Index expectedBase, expectedLimit;
  RangeStruct foundRange, oldRange;
  Addr remainderBase, remainderLimit;
  Addr origBase, origLimit;
  Size oldSize, newSize;

  origBase = origLimit = NULL;
  expected = (high ? BTFindLongResRangeHigh : BTFindLongResRange)
               (&expectedBase, &expectedLimit, state->allocTable,
                (Index)0, (Index)ArraySize, (Count)size);

  if (expected) {
    oldSize = (expectedLimit - expectedBase) * state->align;
    remainderBase = origBase = addrOfIndex(state, expectedBase);
    remainderLimit = origLimit = addrOfIndex(state, expectedLimit);

    switch(findDelete) {
    case FindDeleteNONE: {
      /* do nothing */
    } break;
    case FindDeleteENTIRE: {
      remainderBase = remainderLimit;
    } break;
    case FindDeleteLOW: {
      expectedLimit = expectedBase + size;
      remainderBase = addrOfIndex(state, expectedLimit);
    } break;
    case FindDeleteHIGH: {
      expectedBase = expectedLimit - size;
      remainderLimit = addrOfIndex(state, expectedBase);
    } break;
    }

    if (findDelete != FindDeleteNONE) {
      newSize = AddrOffset(remainderBase, remainderLimit);
    }

    /* TODO: check these values */
    testlib_unused(oldSize);
    testlib_unused(newSize);
  }

  found = (high ? LandFindLast : LandFindFirst)
    (&foundRange, &oldRange, state->land, size * state->align, findDelete);

  if (verbose) {
    printf("find %s %lu: ", high ? "last" : "first",
           (unsigned long)(size * state->align));
    if (expected) {
      printf("expecting [%p,%p)\n",
             (void *)addrOfIndex(state, expectedBase),
             (void *)addrOfIndex(state, expectedLimit));
    } else {
      printf("expecting this not to be found\n");
    }
    if (found) {
      printf("  found [%p,%p)\n", (void *)RangeBase(&foundRange),
             (void *)RangeLimit(&foundRange));
    } else {
      printf("  not found\n");
    }
  }

  Insist(found == expected);

  if (found) {
    Insist(expectedBase == indexOfAddr(state, RangeBase(&foundRange)));
    Insist(expectedLimit == indexOfAddr(state, RangeLimit(&foundRange)));

    if (findDelete != FindDeleteNONE) {
      Insist(RangeBase(&oldRange) == origBase);
      Insist(RangeLimit(&oldRange) == origLimit);
      BTSetRange(state->allocTable, expectedBase, expectedLimit);
    }
  }

  return;
}

static void test(TestState state, unsigned n) {
  Addr base, limit;
  unsigned i;
  Size size;
  Bool high;
  FindDelete findDelete = FindDeleteNONE;

  BTSetRange(state->allocTable, 0, ArraySize); /* Initially all allocated */
  check(state);
  for(i = 0; i < n; i++) {
    switch(fbmRnd(3)) {
    case 0:
      randomRange(&base, &limit, state);
      allocate(state, base, limit);
      break;
    case 1:
      randomRange(&base, &limit, state);
      deallocate(state, base, limit);
      break;
    case 2:
      size = fbmRnd(ArraySize / 10) + 1;
      high = fbmRnd(2) ? TRUE : FALSE;
      switch(fbmRnd(6)) {
      case 0:
      case 1:
      case 2: findDelete = FindDeleteNONE; break;
      case 3: findDelete = FindDeleteLOW; break;
      case 4: findDelete = FindDeleteHIGH; break;
      case 5: findDelete = FindDeleteENTIRE; break;
      }
      find(state, size, high, findDelete);
      break;
    default:
      cdie(0, "invalid rnd(3)");
      return;
    }
    if ((i + 1) % 1000 == 0)
      check(state);
  }
}

#define testArenaSIZE   (((size_t)4)<<20)

extern int main(int argc, char *argv[])
{
  mps_arena_t mpsArena;
  Arena arena;
  TestStateStruct state;
  void *p;
  Addr dummyBlock;
  BT allocTable;
  CBSStruct cbsStruct;
  FreelistStruct flStruct;
  Land land;
  Align align;

  testlib_init(argc, argv);
  align = (1 << rnd() % 4) * MPS_PF_ALIGN;

  NAllocateTried = NAllocateSucceeded = NDeallocateTried =
    NDeallocateSucceeded = 0;

  die(mps_arena_create(&mpsArena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  arena = (Arena)mpsArena; /* avoid pun */

  die((mps_res_t)BTCreate(&allocTable, arena, ArraySize),
      "failed to create alloc table");

  /* We're not going to use this block, but I feel unhappy just */
  /* inventing addresses. */
  die((mps_res_t)ControlAlloc(&p, arena, ArraySize * align,
                              /* withReservoirPermit */ FALSE),
      "failed to allocate block");
  dummyBlock = p; /* avoid pun */

  if (verbose) {
    printf("Allocated block [%p,%p)\n", (void*)dummyBlock,
           (char *)dummyBlock + ArraySize);
  }

  land = &cbsStruct.landStruct;
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, CBSFastFind, TRUE);
    die((mps_res_t)LandInit(land, CBSLandClassGet(), arena, align, NULL, args),
        "failed to initialise CBS");
  } MPS_ARGS_END(args);
  state.align = align;
  state.block = dummyBlock;
  state.allocTable = allocTable;
  state.land = land;
  test(&state, nCBSOperations);
  LandFinish(land);

  land = &flStruct.landStruct;
  die((mps_res_t)LandInit(land, FreelistLandClassGet(), arena, align, NULL,
                          mps_args_none),
      "failed to initialise Freelist");
  state.land = land;
  test(&state, nFLOperations);
  LandFinish(land);

  mps_arena_destroy(arena);

  printf("\nNumber of allocations attempted: %"PRIuLONGEST"\n",
         (ulongest_t)NAllocateTried);
  printf("Number of allocations succeeded: %"PRIuLONGEST"\n",
         (ulongest_t)NAllocateSucceeded);
  printf("Number of deallocations attempted: %"PRIuLONGEST"\n",
         (ulongest_t)NDeallocateTried);
  printf("Number of deallocations succeeded: %"PRIuLONGEST"\n",
         (ulongest_t)NDeallocateSucceeded);
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

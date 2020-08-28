/* landtest.c: LAND TEST
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * Test all three Land implementations against duplicate operations on
 * a bit-table.
 *
 * Test the "steal" operations on a CBS.
 */

#include "cbs.h"
#include "failover.h"
#include "freelist.h"
#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpstd.h"
#include "poolmfs.h"
#include "testlib.h"

#include <stdio.h> /* printf */

SRCID(landtest, "$Id$");


#define ArraySize ((Size)123456)

/* CBS is much faster than Freelist, so we apply more operations to
 * the former. */
#define nCBSOperations ((Size)125000)
#define nFLOperations ((Size)12500)
#define nFOOperations ((Size)12500)

static Count NAllocateTried, NAllocateSucceeded, NDeallocateTried,
  NDeallocateSucceeded;

static int verbose = 0;

typedef struct TestStateStruct {
  Align align;
  BT allocTable;
  Addr block;
  Size size;
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


static void describe(TestState state)
{
  die(LandDescribe(state->land, mps_lib_get_stdout(), 0), "LandDescribe");
}


static Bool checkVisitor(Land land, Range range, void *closure)
{
  Addr base, limit;
  CheckTestClosure cl = closure;

  testlib_unused(land);
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
  Bool b;

  closure.state = state;
  closure.limit = addrOfIndex(state, state->size);
  closure.oldLimit = state->block;

  b = LandIterate(state->land, checkVisitor, &closure);
  Insist(b);

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
 * It then scans forward a binary exponentially distributed number of
 * "edges" in the table (that is, transitions between set and reset)
 * to get <end>. Note that there is a 50% chance that <end> will be
 * the next edge, a 25% chance it will be the edge after, etc., until
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

  base = fbmRnd(state->size);

  do {
    end = nextEdge(state->allocTable, state->size, base);
  } while (end < state->size && fbmRnd(2) == 0); /* p=0.5 exponential */

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
    outerBase =
      addrOfIndex(state, lastEdge(state->allocTable, state->size, ib));
    outerLimit =
      addrOfIndex(state, nextEdge(state->allocTable, state->size, il - 1));
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
    /* Find the free blocks adjacent to the allocated block */
    if (ib > 0 && !BTGet(state->allocTable, ib - 1)) {
      outerBase =
        addrOfIndex(state, lastEdge(state->allocTable, state->size, ib - 1));
    } else {
      outerBase = base;
     }

    if (il < state->size && !BTGet(state->allocTable, il)) {
      outerLimit =
        addrOfIndex(state, nextEdge(state->allocTable, state->size, il));
    } else {
      outerLimit = limit;
    }
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
  Addr origBase, origLimit;

  origBase = origLimit = NULL;
  expected = (high ? BTFindLongResRangeHigh : BTFindLongResRange)
               (&expectedBase, &expectedLimit, state->allocTable,
                (Index)0, (Index)state->size, (Count)size);

  if (expected) {
    origBase = addrOfIndex(state, expectedBase);
    origLimit = addrOfIndex(state, expectedLimit);

    switch(findDelete) {
    case FindDeleteNONE:
      /* do nothing */
      break;
    case FindDeleteENTIRE:
      break;
    case FindDeleteLOW:
      expectedLimit = expectedBase + size;
      break;
    case FindDeleteHIGH:
      expectedBase = expectedLimit - size;
      break;
    default:
      cdie(0, "invalid findDelete");
      break;
    }
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
}

static void test(TestState state, unsigned n, unsigned operations)
{
  Addr base, limit;
  unsigned i;
  Size size;
  Bool high;
  FindDelete findDelete = FindDeleteNONE;

  BTSetRange(state->allocTable, 0, state->size); /* Initially all allocated */
  check(state);
  for(i = 0; i < n; i++) {
    switch (fbmRnd(operations)) {
    case 0:
      randomRange(&base, &limit, state);
      allocate(state, base, limit);
      break;
    case 1:
      randomRange(&base, &limit, state);
      deallocate(state, base, limit);
      break;
    case 2:
      size = fbmRnd(state->size / 10) + 1;
      high = fbmRnd(2) ? TRUE : FALSE;
      switch(fbmRnd(6)) {
      default: findDelete = FindDeleteNONE; break;
      case 3: findDelete = FindDeleteLOW; break;
      case 4: findDelete = FindDeleteHIGH; break;
      case 5: findDelete = FindDeleteENTIRE; break;
      }
      find(state, size, high, findDelete);
      break;
    default:
      cdie(0, "invalid operation");
      return;
    }
    if ((i + 1) % 1000 == 0)
      check(state);
  }
}

#define testArenaSIZE   (((size_t)4)<<20)

static void test_land(void)
{
  static const struct {
    LandClass (*klass)(void);
    unsigned operations;
  } cbsConfig[] = {
    {CBSClassGet, 2},
    {CBSFastClassGet, 3},
    {CBSZonedClassGet, 3},
  };
  mps_arena_t mpsArena;
  Arena arena;
  TestStateStruct state;
  void *p;
  MFSStruct blockPool;
  CBSStruct cbsStruct;
  FreelistStruct flStruct;
  FailoverStruct foStruct;
  Land cbs = CBSLand(&cbsStruct);
  Land fl = FreelistLand(&flStruct);
  Land fo = FailoverLand(&foStruct);
  Pool mfs = MFSPool(&blockPool);
  size_t i;

  state.size = ArraySize;
  state.align = (1 << rnd() % 4) * MPS_PF_ALIGN;

  NAllocateTried = NAllocateSucceeded = NDeallocateTried =
    NDeallocateSucceeded = 0;

  die(mps_arena_create(&mpsArena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  arena = (Arena)mpsArena; /* avoid pun */

  die((mps_res_t)BTCreate(&state.allocTable, arena, state.size),
      "failed to create alloc table");

  die((mps_res_t)ControlAlloc(&p, arena, (state.size + 1) * state.align),
      "failed to allocate block");
  state.block = AddrAlignUp(p, state.align);

  if (verbose) {
    printf("Allocated block [%p,%p)\n", (void *)state.block,
           (void *)AddrAdd(state.block, state.size));
  }

  /* 1. Test CBS */

  for (i = 0; i < NELEMS(cbsConfig); ++i) {
    MPS_ARGS_BEGIN(args) {
      die((mps_res_t)LandInit(cbs, cbsConfig[i].klass(), arena, state.align,
                              NULL, args),
          "failed to initialise CBS");
    } MPS_ARGS_END(args);
    state.land = cbs;
    test(&state, nCBSOperations, cbsConfig[i].operations);
    LandFinish(cbs);
  }

  /* 2. Test Freelist */

  die((mps_res_t)LandInit(fl, CLASS(Freelist), arena, state.align,
                          NULL, mps_args_none),
      "failed to initialise Freelist");
  state.land = fl;
  test(&state, nFLOperations, 3);
  LandFinish(fl);

  /* 3. Test CBS-failing-over-to-Freelist (always failing over on
   * first iteration, never failing over on second; see fotest.c for a
   * test case that randomly switches fail-over on and off)
   */

  for (i = 0; i < 2; ++i) {
      MPS_ARGS_BEGIN(piArgs) {
        MPS_ARGS_ADD(piArgs, MPS_KEY_MFS_UNIT_SIZE, sizeof(CBSFastBlockStruct));
        MPS_ARGS_ADD(piArgs, MPS_KEY_EXTEND_BY, ArenaGrainSize(arena));
        MPS_ARGS_ADD(piArgs, MFSExtendSelf, i != 0);
        die(PoolInit(mfs, arena, PoolClassMFS(), piArgs), "PoolInit");
      } MPS_ARGS_END(piArgs);

      MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, CBSBlockPool, mfs);
        die((mps_res_t)LandInit(cbs, CLASS(CBSFast), arena, state.align,
                                NULL, args),
            "failed to initialise CBS");
      } MPS_ARGS_END(args);

      die((mps_res_t)LandInit(fl, CLASS(Freelist), arena, state.align,
                              NULL, mps_args_none),
          "failed to initialise Freelist");
      MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, FailoverPrimary, cbs);
        MPS_ARGS_ADD(args, FailoverSecondary, fl);
        die((mps_res_t)LandInit(fo, CLASS(Failover), arena, state.align,
                                NULL, args),
            "failed to initialise Failover");
      } MPS_ARGS_END(args);

      state.land = fo;
      test(&state, nFOOperations, 3);
      LandFinish(fo);
      LandFinish(fl);
      LandFinish(cbs);
      PoolFinish(mfs);
  }

  ControlFree(arena, p, (state.size + 1) * state.align);
  mps_arena_destroy(arena);

  printf("Number of allocations attempted: %"PRIuLONGEST"\n",
         (ulongest_t)NAllocateTried);
  printf("Number of allocations succeeded: %"PRIuLONGEST"\n",
         (ulongest_t)NAllocateSucceeded);
  printf("Number of deallocations attempted: %"PRIuLONGEST"\n",
         (ulongest_t)NDeallocateTried);
  printf("Number of deallocations succeeded: %"PRIuLONGEST"\n",
         (ulongest_t)NDeallocateSucceeded);
}

static void shuffle(Addr *addr, size_t n)
{
  size_t i;
  for (i = 0; i < n; ++i) {
    size_t j = rnd() % (n - i);
    Addr tmp = addr[j];
    addr[j] = addr[i];
    addr[i] = tmp;
  }
}

static void test_steal(void)
{
  mps_arena_t mpsArena;
  Arena arena;
  MFSStruct mfs;                /* stores blocks for the CBS */
  Pool pool = MFSPool(&mfs);
  CBSStruct cbs;                /* allocated memory land */
  Land land = CBSLand(&cbs);
  Addr base;
  Addr addr[4096];
  Size grainSize;
  size_t i, n = NELEMS(addr), stolenInsert = 0, missingDelete = 0;

  MPS_ARGS_BEGIN(args) {
    die(mps_arena_create_k(&mpsArena, mps_arena_class_vm(), args), "arena");
  } MPS_ARGS_END(args);
  arena = (Arena)mpsArena; /* avoid pun */
  grainSize = ArenaGrainSize(arena);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_MFS_UNIT_SIZE, sizeof(RangeTreeStruct));
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, grainSize);
    MPS_ARGS_ADD(args, MFSExtendSelf, FALSE);
    die(PoolInit(pool, arena, CLASS(MFSPool), args), "pool");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, CBSBlockPool, pool);
    die(LandInit(land, CLASS(CBS), arena, grainSize, NULL, args),
        "land");
  } MPS_ARGS_END(args);

  /* Allocate a range of grains. */
  die(ArenaAlloc(&base, LocusPrefDefault(), grainSize * n, pool), "alloc");
  for (i = 0; i < n; ++i)
    addr[i] = AddrAdd(base, i * grainSize);

  /* Shuffle the grains. */
  shuffle(addr, n);

  /* Insert grains into the land in shuffled order. */
  for (i = 0; i < n; ++i) {
    RangeStruct range, origRange, containingRange;
    RangeInitSize(&range, addr[i], grainSize);
    RangeCopy(&origRange, &range);
    die(LandInsertSteal(&containingRange, land, &range), "steal");
    if (!RangesEqual(&origRange, &range))
      ++ stolenInsert;
  }

  /* Shuffle grains again. */
  shuffle(addr, n);

  /* Delete unstolen grains from the land in shuffled order. */
  for (i = 0; i < n; ++i) {
    RangeStruct range, containingRange;
    Res res;
    RangeInitSize(&range, addr[i], grainSize);
    res = LandDeleteSteal(&containingRange, land, &range);
    if (res == ResOK) {
      ArenaFree(addr[i], grainSize, pool);
    } else {
      Insist(res == ResFAIL);     /* grain was stolen */
      ++ missingDelete;
    }
  }

  Insist(LandSize(land) == 0);
  LandFinish(land);
  Insist(PoolFreeSize(pool) == PoolTotalSize(pool));
  PoolFinish(pool);
  mps_arena_destroy(arena);
  Insist(stolenInsert <= missingDelete);
  Insist(missingDelete < n);
  printf("Stolen on insert: %"PRIuLONGEST"\n", (ulongest_t)stolenInsert);
  printf("Missing on delete: %"PRIuLONGEST"\n", (ulongest_t)missingDelete);
}

int main(int argc, char *argv[])
{
  testlib_init(argc, argv);
  test_land();
  test_steal();
  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
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

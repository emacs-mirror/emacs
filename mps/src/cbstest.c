/*  impl.c.cbstest: COALESCING BLOCK STRUCTURE TEST
 *
 *  $HopeName: !cbstest.c(trunk.8) $
 * Copyright (C) 1999.  Harlequin Limited.  All rights reserved.
 */

#include "cbs.h"
#include "mpm.h"
#include "mpsavm.h"
#include "mps.h"
#include "testlib.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

SRCID(cbstest, "$HopeName: !cbstest.c(trunk.8) $");


#define ArraySize ((Size)123456)
#define NOperations ((Size)125000)
#define MinSize ((Size)120) /* Arbitrary size */
#define Alignment ((Align)sizeof(void *))


static Count NAllocateTried, NAllocateSucceeded, NDeallocateTried,
  NDeallocateSucceeded, NNewBlocks, NDeleteBlocks, NGrowBlocks,
  NShrinkBlocks;


/* Used to predict which callbacks will be called, and with which values. */
/* At most one callback of each type will be called. */
typedef struct CallbackPredictionStruct {
  Bool shouldBeCalled;
  Size oldSize;
  Addr base;
  Addr limit;
} CallbackPredictionStruct, *CallbackPrediction;

static CallbackPredictionStruct CallbackNew;
static CallbackPredictionStruct CallbackDelete;
static CallbackPredictionStruct CallbackGrow;
static CallbackPredictionStruct CallbackShrink;


typedef struct CheckCBSClosureStruct {
  BT allocTable;
  Addr base;
  Addr limit;
  Addr oldLimit;
} CheckCBSClosureStruct, *CheckCBSClosure;


static Addr (AddrOfIndex)(Addr block, Index i) {
  return AddrAdd(block, (i * Alignment));
}

static Index (IndexOfAddr)(Addr block, Addr a) {
  return (Index)(AddrOffset(block, a) / Alignment);
}


/* This function encapsulates the common tests for the callbacks. */
static void testCallback(CBS cbs, CBSBlock cbsBlock,
                         Size oldSize, Size newSize,
                         CallbackPrediction prediction) {
  AVERT(CBS, cbs);
  AVERT(CBSBlock, cbsBlock);
  AVER(prediction->shouldBeCalled);
  AVER(oldSize == prediction->oldSize);

  if(newSize == 0) {
    AVER(prediction->base == 0);
    AVER(prediction->limit == 0);
  } else {
    AVER(CBSBlockSize(cbsBlock) == newSize);
    AVER(newSize == AddrOffset(prediction->base, prediction->limit));
    AVER(CBSBlockBase(cbsBlock) == prediction->base);
    AVER(CBSBlockLimit(cbsBlock) == prediction->limit);
  }

  prediction->shouldBeCalled = FALSE;
}


static void cbsNewCallback(CBS cbs, CBSBlock cbsBlock,
                           Size oldSize, Size newSize) {
  testCallback(cbs, cbsBlock, oldSize, newSize, &CallbackNew);
  AVER(oldSize < cbs->minSize);
  AVER(newSize >= cbs->minSize);

  NNewBlocks++;
}

static void cbsDeleteCallback(CBS cbs, CBSBlock cbsBlock,
                              Size oldSize, Size newSize) {
  testCallback(cbs, cbsBlock, oldSize, newSize, &CallbackDelete);
  AVER(oldSize >= cbs->minSize);
  AVER(newSize < cbs->minSize);

  NDeleteBlocks++;
}

static void cbsGrowCallback(CBS cbs, CBSBlock cbsBlock,
                            Size oldSize, Size newSize) {
  testCallback(cbs, cbsBlock, oldSize, newSize, &CallbackGrow);
  AVER(oldSize >= cbs->minSize);
  AVER(newSize >= cbs->minSize);
  AVER(oldSize < newSize);

  NGrowBlocks++;
}

static void cbsShrinkCallback(CBS cbs, CBSBlock cbsBlock,
                              Size oldSize, Size newSize) {
  testCallback(cbs, cbsBlock, oldSize, newSize, &CallbackShrink);
  AVER(oldSize >= cbs->minSize);
  AVER(newSize >= cbs->minSize);
  AVER(oldSize > newSize);

  NShrinkBlocks++;
}

static Bool checkCBSAction(CBS cbs, CBSBlock cbsBlock,
                           void *p, unsigned long s) {
  Addr base, limit;
  CheckCBSClosure closure = (CheckCBSClosure)p;

  /* Don't need to check cbs every time */
  UNUSED(cbs);
  AVER(closure != NULL);
  AVER(s == 0);

  base = CBSBlockBase(cbsBlock);
  limit = CBSBlockLimit(cbsBlock);

  if(base > closure->oldLimit) {
    AVER(BTIsSetRange(closure->allocTable,
                      IndexOfAddr(closure->base, closure->oldLimit),
                      IndexOfAddr(closure->base, base)));
  } else { /* must be at start of table */
    AVER(base == closure->oldLimit);
    AVER(closure->oldLimit == closure->base);
  }

  AVER(BTIsResRange(closure->allocTable,
                    IndexOfAddr(closure->base, base),
                    IndexOfAddr(closure->base, limit)));


  closure->oldLimit = limit;

  return TRUE;
}

static void checkCBS(CBS cbs, BT allocTable, Addr dummyBlock) {
  CheckCBSClosureStruct closure;

  closure.allocTable = allocTable;
  closure.base = dummyBlock;
  closure.limit = AddrOfIndex(closure.base, ArraySize);
  closure.oldLimit = closure.base;

  CBSIterate(cbs, checkCBSAction, (void *)&closure, (unsigned long)0);

  if(closure.oldLimit == closure.base)
    AVER(BTIsSetRange(allocTable, 0,
                      IndexOfAddr(dummyBlock, closure.limit)));
  else if(closure.limit > closure.oldLimit)
    AVER(BTIsSetRange(allocTable,
                      IndexOfAddr(dummyBlock, closure.oldLimit),
                      IndexOfAddr(dummyBlock, closure.limit)));
  else
    AVER(closure.oldLimit == closure.limit);
}

/* Not very uniform, but never mind. */
static Word cbsRnd(Word limit) {
  return (Word)rnd() % limit;
}


/* nextEdge -- Finds the next transition in the bit table
 *
 * Returns the index greater than <base> such that the
 * range [<base>, <return>) has the same value in the bit table,
 * and <return> has a different value or does not exist.
 */

static Index nextEdge(BT bt, Size size, Index base) {
  Index end;
  Bool baseValue;

  AVER(bt != NULL);
  AVER(base < size);

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

static Index lastEdge(BT bt, Size size, Index base) {
  Index end;
  Bool baseValue;

  AVER(bt != NULL);
  AVER(base < size);

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

static void randomRange(Addr *baseReturn,
                        Addr *limitReturn,
                        BT allocTable,
                        Addr block) {
  Index base;   /* the start of our range */
  Index end;    /* an edge (i.e. different from its predecessor) */
                /* after base */
  Index limit;  /* a randomly chosen value in (base, limit]. */

  base = cbsRnd(ArraySize);

  do {
    end = nextEdge(allocTable, ArraySize, base);
  } while(end < ArraySize && cbsRnd(2) == 0); /* p=0.5 exponential */

  AVER(end > base);

  limit = base + 1 + cbsRnd(end - base);

  *baseReturn = AddrOfIndex(block, base);
  *limitReturn = AddrOfIndex(block, limit);
}


/* Set callback expectations */

static void clearExpectations(void) {
  CallbackNew.shouldBeCalled = FALSE;
  CallbackDelete.shouldBeCalled = FALSE;
  CallbackGrow.shouldBeCalled = FALSE;
  CallbackShrink.shouldBeCalled = FALSE;
}

static void expectCallback(CallbackPrediction prediction,
                           Size oldSize, Addr base, Addr limit) {
  AVER(prediction->shouldBeCalled == FALSE);
  AVER(base == (Addr)0 || limit > base);
  AVER(oldSize != (Size)0 || base != (Addr)0);
  AVER(base != (Addr)0 || limit == (Addr)0);

  prediction->shouldBeCalled = TRUE;
  prediction->oldSize = oldSize;
  prediction->base = base;
  prediction->limit = limit;
}

static void checkExpectations(void) {
  AVER(!CallbackNew.shouldBeCalled);
  AVER(!CallbackDelete.shouldBeCalled);
  AVER(!CallbackGrow.shouldBeCalled);
  AVER(!CallbackShrink.shouldBeCalled);
}


static void allocate(CBS cbs, Addr block, BT allocTable,
                     Addr base, Addr limit) {
  Res res;
  Index ib, il;                  /* Indexed for base and limit */
  Bool isFree;

  ib = IndexOfAddr(block, base);
  il = IndexOfAddr(block, limit);

  isFree = BTIsResRange(allocTable, ib, il);

  /*
  printf("allocate: [%p, %p) -- %s\n",
         base, limit, isFree ? "succeed" : "fail");
  */

  NAllocateTried++;

  if(isFree) {
    Addr outerBase, outerLimit;    /* interval containing [ib, il) */
    Size left, right, total;       /* Sizes of block and two fragments */

    outerBase =
      AddrOfIndex(block, lastEdge(allocTable, ArraySize, ib));
    outerLimit =
      AddrOfIndex(block, nextEdge(allocTable, ArraySize, il - 1));

    left = AddrOffset(outerBase, base);
    right = AddrOffset(limit, outerLimit);
    total = AddrOffset(outerBase, outerLimit);

    /* based on detailed knowledge of CBS behaviour */
    checkExpectations();
    if(total >= MinSize && left < MinSize && right < MinSize) {
      if(left == (Size)0 && right == (Size)0) {
        expectCallback(&CallbackDelete, total, (Addr)0, (Addr)0);
      } else if(left >= right) {
        expectCallback(&CallbackDelete, total, outerBase, base);
      } else {
        expectCallback(&CallbackDelete, total, limit, outerLimit);
      }
    } else if(left >= MinSize && right >= MinSize) {
      if(left >= right) {
        expectCallback(&CallbackShrink, total, outerBase, base);
        expectCallback(&CallbackNew, (Size)0, limit, outerLimit);
      } else {
        expectCallback(&CallbackNew, (Size)0, outerBase, base);
        expectCallback(&CallbackShrink, total, limit, outerLimit);
      }
    } else if(total >= MinSize) {
      if(left >= right) {
        AVER(left >= MinSize);
        AVER(right < MinSize);
        expectCallback(&CallbackShrink, total, outerBase, base);
      } else {
        AVER(left < MinSize);
        AVER(right >= MinSize);
        expectCallback(&CallbackShrink, total, limit, outerLimit);
      }
    }
  }

  res = CBSDelete(cbs, base, limit);

  if(!isFree) {
    die_expect((mps_res_t)res, MPS_RES_FAIL,
               "Succeeded in deleting allocated block");
  } else { /* isFree */
    die_expect((mps_res_t)res, MPS_RES_OK,
               "failed to delete free block");
    NAllocateSucceeded++;
    BTSetRange(allocTable, ib, il);
    checkExpectations();
  }
}


static void deallocate(CBS cbs, Addr block, BT allocTable,
                       Addr base, Addr limit) {
  Res res;
  Index ib, il;
  Bool isAllocated;
  Addr outerBase = base, outerLimit = limit; /* interval containing [ib, il) */
  Addr freeBase, freeLimit;                  /* interval returned by CBS */

  ib = IndexOfAddr(block, base);
  il = IndexOfAddr(block, limit);

  isAllocated = BTIsSetRange(allocTable, ib, il);

  /*
  printf("deallocate: [%p, %p) -- %s\n",
         base, limit, isAllocated ? "succeed" : "fail");
  */

  NDeallocateTried++;

  if(isAllocated) {
    Size left, right, total;       /* Sizes of block and two fragments */

    /* Find the free blocks adjacent to the allocated block */
    if(ib > 0 && !BTGet(allocTable, ib - 1)) {
      outerBase =
        AddrOfIndex(block, lastEdge(allocTable, ArraySize, ib - 1));
    } else {
      outerBase = base;
     }

    if(il < ArraySize && !BTGet(allocTable, il)) {
      outerLimit =
        AddrOfIndex(block, nextEdge(allocTable, ArraySize, il));
    } else {
      outerLimit = limit;
    }

    left = AddrOffset(outerBase, base);
    right = AddrOffset(limit, outerLimit);
    total = AddrOffset(outerBase, outerLimit);

    /* based on detailed knowledge of CBS behaviour */
    checkExpectations();
    if(total >= MinSize && left < MinSize && right < MinSize) {
      if(left >= right)
        expectCallback(&CallbackNew, left, outerBase, outerLimit);
      else
        expectCallback(&CallbackNew, right, outerBase, outerLimit);
    } else if(left >= MinSize && right >= MinSize) {
      if(left >= right) {
        expectCallback(&CallbackDelete, right, (Addr)0, (Addr)0);
        expectCallback(&CallbackGrow, left, outerBase, outerLimit);
      } else {
        expectCallback(&CallbackDelete, left, (Addr)0, (Addr)0);
        expectCallback(&CallbackGrow, right, outerBase, outerLimit);
      }
    } else if(total >= MinSize) {
      if(left >= right) {
        AVER(left >= MinSize);
        AVER(right < MinSize);
        expectCallback(&CallbackGrow, left, outerBase, outerLimit);
      } else {
        AVER(left < MinSize);
        AVER(right >= MinSize);
        expectCallback(&CallbackGrow, right, outerBase, outerLimit);
      }
    }
  }

  res = CBSInsertReturningRange(&freeBase, &freeLimit, cbs, base, limit);

  if(!isAllocated) {
    die_expect((mps_res_t)res, MPS_RES_FAIL,
               "succeeded in inserting non-allocated block");
  } else { /* isAllocated */
    die_expect((mps_res_t)res, MPS_RES_OK,
               "failed to insert allocated block");

    NDeallocateSucceeded++;
    BTResRange(allocTable, ib, il);
    checkExpectations();
    AVER(freeBase == outerBase);
    AVER(freeLimit == outerLimit);
  }
}


static void find(CBS cbs, void *block, BT alloc, Size size, Bool high,
                 CBSFindDelete findDelete)
{
  Bool expected, found;
  Index expectedBase, expectedLimit;
  Addr foundBase, foundLimit, remainderBase, remainderLimit;
  Size oldSize, newSize;

  checkExpectations();

  expected = (high ? BTFindLongResRangeHigh : BTFindLongResRange)
               (&expectedBase, &expectedLimit, alloc,
                (Index)0, (Index)ArraySize, (unsigned long)size);

  if(expected) {
    oldSize = (expectedLimit - expectedBase) * Alignment;
    remainderBase = AddrOfIndex(block, expectedBase);
    remainderLimit = AddrOfIndex(block, expectedLimit);

    switch(findDelete) {
    case CBSFindDeleteNONE: {
      /* do nothing */
    } break;
    case CBSFindDeleteENTIRE: {
      remainderBase = remainderLimit;
    } break;
    case CBSFindDeleteLOW: {
      expectedLimit = expectedBase + size;
      remainderBase = AddrOfIndex(block, expectedLimit);
    } break;
    case CBSFindDeleteHIGH: {
      expectedBase = expectedLimit - size;
      remainderLimit = AddrOfIndex(block, expectedBase);
    } break;
    }

    if(findDelete != CBSFindDeleteNONE) {
      newSize = AddrOffset(remainderBase, remainderLimit);

      if(oldSize >= MinSize) {
        if(newSize == 0)
          expectCallback(&CallbackDelete, oldSize, (Addr)0, (Addr)0);
        else if(newSize < MinSize)
          expectCallback(&CallbackDelete, oldSize,
                         remainderBase, remainderLimit);
        else
          expectCallback(&CallbackShrink, oldSize,
                         remainderBase, remainderLimit);
      }
    }
  }

  found = (high ? CBSFindLast : CBSFindFirst)
    (&foundBase, &foundLimit, cbs, size * Alignment, findDelete);

  AVER(found == expected);

  if(found) {
    AVER(expectedBase == IndexOfAddr(block, foundBase));
    AVER(expectedLimit == IndexOfAddr(block, foundLimit));
    checkExpectations();

    if(findDelete != CBSFindDeleteNONE)
      BTSetRange(alloc, expectedBase, expectedLimit);
  }

  return;
}


extern int main(int argc, char *argv[])
{
  int i;
  Addr base, limit;
  mps_arena_t mpsArena;
  Arena arena; /* the arena which we use to allocate the BT */
  CBSStruct cbsStruct;
  CBS cbs;
  void *p;
  Addr dummyBlock;
  BT allocTable;
  Size size;
  Bool high;
  CBSFindDelete findDelete = CBSFindDeleteNONE;

  testlib_unused(argc);
  testlib_unused(argv);

  NAllocateTried = NAllocateSucceeded = NDeallocateTried =
    NDeallocateSucceeded = NNewBlocks = NDeleteBlocks =
    NGrowBlocks = NShrinkBlocks = 0;

  clearExpectations();

  die((mps_res_t)mps_arena_create(&mpsArena, mps_arena_class_vm()),
      "Failed to create arena");
  arena = (Arena)mpsArena; /* avoid pun */

  die((mps_res_t)BTCreate(&allocTable, arena, ArraySize),
      "failed to create alloc table");

  die((mps_res_t)CBSInit(arena, &cbsStruct, NULL, &cbsNewCallback,
                         &cbsDeleteCallback, &cbsGrowCallback,
                         &cbsShrinkCallback, MinSize,
                         Alignment, TRUE, TRUE),
      "failed to initialise CBS");
  cbs = &cbsStruct;

  BTSetRange(allocTable, 0, ArraySize); /* Initially all allocated */

  /* We're not going to use this block, but I feel unhappy just */
  /* inventing addresses. */
  die((mps_res_t)ControlAlloc(&p, arena, ArraySize * Alignment,
                              /* withReservoirPermit */ FALSE),
      "failed to allocate block");
  dummyBlock = (Addr)p; /* avoid pun */

  printf("Allocated block [%p, %p)\n", dummyBlock,
         (char *)dummyBlock + ArraySize);

  checkCBS(cbs, allocTable, dummyBlock);
  for(i = 0; i < NOperations; i++) {
    switch(cbsRnd(3)) {
    case 0: {
      randomRange(&base, &limit, allocTable, dummyBlock);
      allocate(cbs, dummyBlock, allocTable, base, limit);
    } break;
    case 1: {
      randomRange(&base, &limit, allocTable, dummyBlock);
      deallocate(cbs, dummyBlock, allocTable, base, limit);
    } break;
    case 2: {
      size = cbsRnd(ArraySize / 10) + 1;
      high = cbsRnd(2) ? TRUE : FALSE;
      switch(cbsRnd(6)) {
      case 0:
      case 1:
      case 2: findDelete = CBSFindDeleteNONE; break;
      case 3: findDelete = CBSFindDeleteLOW; break;
      case 4: findDelete = CBSFindDeleteHIGH; break;
      case 5: findDelete = CBSFindDeleteENTIRE; break;
      }
      find(cbs, dummyBlock, allocTable, size, high, findDelete);
    } break;
    }
    if(i % 5000 == 0)
      checkCBS(cbs, allocTable, dummyBlock);
  }

  checkExpectations();

  /* CBSDescribe prints a very long line. */
  /* CBSDescribe(cbs, mps_lib_get_stdout()); */

  printf("\nNumber of allocations attempted: %ld\n", NAllocateTried);
  printf("Number of allocations succeeded: %ld\n", NAllocateSucceeded);
  printf("Number of deallocations attempted: %ld\n", NDeallocateTried);
  printf("Number of deallocations succeeded: %ld\n", NDeallocateSucceeded);
  printf("Number of new large blocks: %ld\n", NNewBlocks);
  printf("Number of deleted large blocks: %ld\n", NDeleteBlocks);
  printf("Number of grown large blocks: %ld\n", NGrowBlocks);
  printf("Number of shrunk large blocks: %ld\n", NShrinkBlocks);
  printf("\nNo problems detected.\n");
  return 0;
}

/*  impl.c.cbstest: COALESCING BLOCK STRUCTURE TEST
 *
 *  $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */

#include "cbs.h"
#include "mpm.h"
#include "mpsavm.h"
#include "mps.h"
#include "testlib.h"

#include <stdlib.h>
#include <stdarg.h>
#include "mpstd.h"
#ifdef MPS_OS_IA
struct itimerspec; /* stop complaints from time.h */
#endif
#include <time.h>

SRCID(cbstest, "$Id$");


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


static Addr (addrOfIndex)(Addr block, Index i)
{
  return AddrAdd(block, (i * Alignment));
}


static Index (indexOfAddr)(Addr block, Addr a)
{
  return (Index)(AddrOffset(block, a) / Alignment);
}


/* This function encapsulates the common tests for the callbacks. */

static void testCallback(CBS cbs, CBSBlock cbsBlock,
                         Size oldSize, Size newSize,
                         CallbackPrediction prediction)
{
  Insist(CBSCheck(cbs));
  Insist(CBSBlockCheck(cbsBlock));
  Insist(prediction->shouldBeCalled);
  Insist(oldSize == prediction->oldSize);

  if (newSize == 0) {
    Insist(prediction->base == 0);
    Insist(prediction->limit == 0);
  } else {
    Insist(CBSBlockSize(cbsBlock) == newSize);
    Insist(newSize == AddrOffset(prediction->base, prediction->limit));
    Insist(CBSBlockBase(cbsBlock) == prediction->base);
    Insist(CBSBlockLimit(cbsBlock) == prediction->limit);
  }

  prediction->shouldBeCalled = FALSE;
}


static void cbsNewCallback(CBS cbs, CBSBlock cbsBlock,
                           Size oldSize, Size newSize)
{
  testCallback(cbs, cbsBlock, oldSize, newSize, &CallbackNew);
  Insist(oldSize < cbs->minSize);
  Insist(newSize >= cbs->minSize);

  NNewBlocks++;
}


static void cbsDeleteCallback(CBS cbs, CBSBlock cbsBlock,
                              Size oldSize, Size newSize)
{
  testCallback(cbs, cbsBlock, oldSize, newSize, &CallbackDelete);
  Insist(oldSize >= cbs->minSize);
  Insist(newSize < cbs->minSize);

  NDeleteBlocks++;
}


static void cbsGrowCallback(CBS cbs, CBSBlock cbsBlock,
                            Size oldSize, Size newSize)
{
  testCallback(cbs, cbsBlock, oldSize, newSize, &CallbackGrow);
  Insist(oldSize >= cbs->minSize);
  Insist(newSize >= cbs->minSize);
  Insist(oldSize < newSize);

  NGrowBlocks++;
}


static void cbsShrinkCallback(CBS cbs, CBSBlock cbsBlock,
                              Size oldSize, Size newSize)
{
  testCallback(cbs, cbsBlock, oldSize, newSize, &CallbackShrink);
  Insist(oldSize >= cbs->minSize);
  Insist(newSize >= cbs->minSize);
  Insist(oldSize > newSize);

  NShrinkBlocks++;
}


static Bool checkCBSAction(CBS cbs, CBSBlock cbsBlock, void *p)
{
  Addr base, limit;
  CheckCBSClosure closure = (CheckCBSClosure)p;

  /* Don't need to check cbs every time */
  UNUSED(cbs);
  Insist(closure != NULL);

  base = CBSBlockBase(cbsBlock);
  limit = CBSBlockLimit(cbsBlock);

  if (base > closure->oldLimit) {
    Insist(BTIsSetRange(closure->allocTable,
                      indexOfAddr(closure->base, closure->oldLimit),
                      indexOfAddr(closure->base, base)));
  } else { /* must be at start of table */
    Insist(base == closure->oldLimit);
    Insist(closure->oldLimit == closure->base);
  }
 
  Insist(BTIsResRange(closure->allocTable,
                      indexOfAddr(closure->base, base),
                      indexOfAddr(closure->base, limit)));


  closure->oldLimit = limit;

  return TRUE;
}


static void checkCBS(CBS cbs, BT allocTable, Addr dummyBlock)
{
  CheckCBSClosureStruct closure;

  closure.allocTable = allocTable;
  closure.base = dummyBlock;
  closure.limit = addrOfIndex(closure.base, ArraySize);
  closure.oldLimit = closure.base;

  CBSIterate(cbs, checkCBSAction, (void *)&closure);

  if (closure.oldLimit == closure.base)
    Insist(BTIsSetRange(allocTable, 0,
                        indexOfAddr(dummyBlock, closure.limit)));
  else if (closure.limit > closure.oldLimit)
    Insist(BTIsSetRange(allocTable,
                        indexOfAddr(dummyBlock, closure.oldLimit),
                        indexOfAddr(dummyBlock, closure.limit)));
  else
    Insist(closure.oldLimit == closure.limit);
}


static Word cbsRnd(Word limit)
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

static void randomRange(Addr *baseReturn, Addr *limitReturn,
                        BT allocTable, Addr block)
{
  Index base;   /* the start of our range */
  Index end;    /* an edge (i.e. different from its predecessor) */
                /* after base */
  Index limit;  /* a randomly chosen value in (base, limit]. */

  base = cbsRnd(ArraySize);

  do {
    end = nextEdge(allocTable, ArraySize, base);
  } while(end < ArraySize && cbsRnd(2) == 0); /* p=0.5 exponential */

  Insist(end > base);

  limit = base + 1 + cbsRnd(end - base);

  *baseReturn = addrOfIndex(block, base);
  *limitReturn = addrOfIndex(block, limit);
}


/* Set callback expectations */

static void clearExpectations(void)
{
  CallbackNew.shouldBeCalled = FALSE;
  CallbackDelete.shouldBeCalled = FALSE;
  CallbackGrow.shouldBeCalled = FALSE;
  CallbackShrink.shouldBeCalled = FALSE;
}

static void expectCallback(CallbackPrediction prediction,
                           Size oldSize, Addr base, Addr limit)
{
  Insist(prediction->shouldBeCalled == FALSE);
  Insist(base == (Addr)0 || limit > base);
  Insist(oldSize != (Size)0 || base != (Addr)0);
  Insist(base != (Addr)0 || limit == (Addr)0);

  prediction->shouldBeCalled = TRUE;
  prediction->oldSize = oldSize;
  prediction->base = base;
  prediction->limit = limit;
}


static void checkExpectations(void)
{
  Insist(!CallbackNew.shouldBeCalled);
  Insist(!CallbackDelete.shouldBeCalled);
  Insist(!CallbackGrow.shouldBeCalled);
  Insist(!CallbackShrink.shouldBeCalled);
} 


static void allocate(CBS cbs, Addr block, BT allocTable,
                     Addr base, Addr limit)
{
  Res res;
  Index ib, il;                  /* Indexed for base and limit */
  Bool isFree;

  ib = indexOfAddr(block, base);
  il = indexOfAddr(block, limit);

  isFree = BTIsResRange(allocTable, ib, il);
 
  /*
  printf("allocate: [%p, %p) -- %s\n",
         base, limit, isFree ? "succeed" : "fail");
  */

  NAllocateTried++;

  if (isFree) {
    Addr outerBase, outerLimit;    /* interval containing [ib, il) */
    Size left, right, total;       /* Sizes of block and two fragments */

    outerBase =
      addrOfIndex(block, lastEdge(allocTable, ArraySize, ib));
    outerLimit =
      addrOfIndex(block, nextEdge(allocTable, ArraySize, il - 1));

    left = AddrOffset(outerBase, base);
    right = AddrOffset(limit, outerLimit);
    total = AddrOffset(outerBase, outerLimit);

    /* based on detailed knowledge of CBS behaviour */
    checkExpectations();
    if (total >= MinSize && left < MinSize && right < MinSize) {
      if (left == (Size)0 && right == (Size)0) {
        expectCallback(&CallbackDelete, total, (Addr)0, (Addr)0);
      } else if (left >= right) {
        expectCallback(&CallbackDelete, total, outerBase, base);
      } else {
        expectCallback(&CallbackDelete, total, limit, outerLimit);
      }
    } else if (left >= MinSize && right >= MinSize) {
      if (left >= right) {
        expectCallback(&CallbackShrink, total, outerBase, base);
        expectCallback(&CallbackNew, (Size)0, limit, outerLimit);
      } else {
        expectCallback(&CallbackNew, (Size)0, outerBase, base);
        expectCallback(&CallbackShrink, total, limit, outerLimit);
      }
    } else if (total >= MinSize) {
      if (left >= right) {
        Insist(left >= MinSize);
        Insist(right < MinSize);
        expectCallback(&CallbackShrink, total, outerBase, base);
      } else {
        Insist(left < MinSize);
        Insist(right >= MinSize);
        expectCallback(&CallbackShrink, total, limit, outerLimit);
      }
    }
  }

  res = CBSDelete(cbs, base, limit);

  if (!isFree) {
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
                       Addr base, Addr limit)
{
  Res res;
  Index ib, il;
  Bool isAllocated;
  Addr outerBase = base, outerLimit = limit; /* interval containing [ib, il) */
  Addr freeBase, freeLimit;                  /* interval returned by CBS */

  ib = indexOfAddr(block, base);
  il = indexOfAddr(block, limit);

  isAllocated = BTIsSetRange(allocTable, ib, il);

  /*
  printf("deallocate: [%p, %p) -- %s\n",
         base, limit, isAllocated ? "succeed" : "fail");
  */

  NDeallocateTried++;

  if (isAllocated) {
    Size left, right, total;       /* Sizes of block and two fragments */

    /* Find the free blocks adjacent to the allocated block */
    if (ib > 0 && !BTGet(allocTable, ib - 1)) {
      outerBase =
        addrOfIndex(block, lastEdge(allocTable, ArraySize, ib - 1));
    } else {
      outerBase = base;
     }

    if (il < ArraySize && !BTGet(allocTable, il)) {
      outerLimit =
        addrOfIndex(block, nextEdge(allocTable, ArraySize, il));
    } else {
      outerLimit = limit;
    }

    left = AddrOffset(outerBase, base);
    right = AddrOffset(limit, outerLimit);
    total = AddrOffset(outerBase, outerLimit);

    /* based on detailed knowledge of CBS behaviour */
    checkExpectations();
    if (total >= MinSize && left < MinSize && right < MinSize) {
      if (left >= right)
        expectCallback(&CallbackNew, left, outerBase, outerLimit);
      else
        expectCallback(&CallbackNew, right, outerBase, outerLimit);
    } else if (left >= MinSize && right >= MinSize) {
      if (left >= right) {
        expectCallback(&CallbackDelete, right, (Addr)0, (Addr)0);
        expectCallback(&CallbackGrow, left, outerBase, outerLimit);
      } else {
        expectCallback(&CallbackDelete, left, (Addr)0, (Addr)0);
        expectCallback(&CallbackGrow, right, outerBase, outerLimit);
      }
    } else if (total >= MinSize) {
      if (left >= right) {
        Insist(left >= MinSize);
        Insist(right < MinSize);
        expectCallback(&CallbackGrow, left, outerBase, outerLimit);
      } else {
        Insist(left < MinSize);
        Insist(right >= MinSize);
        expectCallback(&CallbackGrow, right, outerBase, outerLimit);
      }
    }
  }

  res = CBSInsertReturningRange(&freeBase, &freeLimit, cbs, base, limit);

  if (!isAllocated) {
    die_expect((mps_res_t)res, MPS_RES_FAIL,
               "succeeded in inserting non-allocated block");
  } else { /* isAllocated */
    die_expect((mps_res_t)res, MPS_RES_OK,
               "failed to insert allocated block");

    NDeallocateSucceeded++;
    BTResRange(allocTable, ib, il);
    checkExpectations();
    Insist(freeBase == outerBase);
    Insist(freeLimit == outerLimit);
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

  if (expected) {
    oldSize = (expectedLimit - expectedBase) * Alignment;
    remainderBase = addrOfIndex(block, expectedBase);
    remainderLimit = addrOfIndex(block, expectedLimit);

    switch(findDelete) {
    case CBSFindDeleteNONE: {
      /* do nothing */
    } break;
    case CBSFindDeleteENTIRE: {
      remainderBase = remainderLimit;
    } break;
    case CBSFindDeleteLOW: {
      expectedLimit = expectedBase + size;
      remainderBase = addrOfIndex(block, expectedLimit);
    } break;
    case CBSFindDeleteHIGH: {
      expectedBase = expectedLimit - size;
      remainderLimit = addrOfIndex(block, expectedBase);
    } break;
    }

    if (findDelete != CBSFindDeleteNONE) {
      newSize = AddrOffset(remainderBase, remainderLimit);

      if (oldSize >= MinSize) {
        if (newSize == 0)
          expectCallback(&CallbackDelete, oldSize, (Addr)0, (Addr)0);
        else if (newSize < MinSize)
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

  Insist(found == expected);

  if (found) {
    Insist(expectedBase == indexOfAddr(block, foundBase));
    Insist(expectedLimit == indexOfAddr(block, foundLimit));
    checkExpectations();

    if (findDelete != CBSFindDeleteNONE)
      BTSetRange(alloc, expectedBase, expectedLimit);
  }

  return;
}


#define testArenaSIZE   (((size_t)4)<<20)

extern int main(int argc, char *argv[])
{
  int i;
  Addr base, limit;
  mps_arena_t mpsArena;
  Arena arena; /* the ANSI arena which we use to allocate the BT */
  CBSStruct cbsStruct;
  CBS cbs;
  void *p;
  Addr dummyBlock;
  BT allocTable;
  Size size;
  Bool high;
  CBSFindDelete findDelete = CBSFindDeleteNONE;

  randomize(argc, argv);

  NAllocateTried = NAllocateSucceeded = NDeallocateTried =
    NDeallocateSucceeded = NNewBlocks = NDeleteBlocks =
    NGrowBlocks = NShrinkBlocks = 0;

  clearExpectations();

  die(mps_arena_create(&mpsArena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
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
    if (i % 5000 == 0)
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


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

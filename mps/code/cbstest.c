/* cbstest.c: COALESCING BLOCK STRUCTURE TEST
 *
 *  $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 */

#include "cbs.h"
#include "mpm.h"
#include "mpsavm.h"
#include "mps.h"
#include "testlib.h"

#include <stdlib.h>
#include <stdarg.h>
#include "mpstd.h"
#include <time.h>

SRCID(cbstest, "$Id$");


#define ArraySize ((Size)123456)
#define NOperations ((Size)125000)
#define Alignment ((Align)sizeof(void *))


static Count NAllocateTried, NAllocateSucceeded, NDeallocateTried,
  NDeallocateSucceeded;


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


static Bool checkCBSAction(CBS cbs, Range range, void *closureP, Size closureS)
{
  Addr base, limit;
  CheckCBSClosure closure = (CheckCBSClosure)closureP;

  /* Don't need to check cbs every time */
  UNUSED(cbs);
  UNUSED(closureS);
  Insist(closure != NULL);

  base = RangeBase(range);
  limit = RangeLimit(range);

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

  CBSIterate(cbs, checkCBSAction, (void *)&closure, 0);

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


static void allocate(CBS cbs, Addr block, BT allocTable,
                     Addr base, Addr limit)
{
  Res res;
  Index ib, il;                  /* Indexed for base and limit */
  Bool isFree;
  RangeStruct range, oldRange;
  Addr outerBase, outerLimit;    /* interval containing [ib, il) */

  ib = indexOfAddr(block, base);
  il = indexOfAddr(block, limit);

  isFree = BTIsResRange(allocTable, ib, il);
 
  /*
  printf("allocate: [%p, %p) -- %s\n",
         base, limit, isFree ? "succeed" : "fail");
  */

  NAllocateTried++;

  if (isFree) {
    Size left, right, total;       /* Sizes of block and two fragments */

    outerBase =
      addrOfIndex(block, lastEdge(allocTable, ArraySize, ib));
    outerLimit =
      addrOfIndex(block, nextEdge(allocTable, ArraySize, il - 1));

    left = AddrOffset(outerBase, base);
    right = AddrOffset(limit, outerLimit);
    total = AddrOffset(outerBase, outerLimit);

    /* TODO: check these values */
    UNUSED(left);
    UNUSED(right);
    UNUSED(total);
  }

  RangeInit(&range, base, limit);
  res = CBSDelete(&oldRange, cbs, &range);

  if (!isFree) {
    die_expect((mps_res_t)res, MPS_RES_FAIL,
               "Succeeded in deleting allocated block");
  } else { /* isFree */
    die_expect((mps_res_t)res, MPS_RES_OK,
               "failed to delete free block");
    Insist(RangeBase(&oldRange) == outerBase);
    Insist(RangeLimit(&oldRange) == outerLimit);
    NAllocateSucceeded++;
    BTSetRange(allocTable, ib, il);
  }
}


static void deallocate(CBS cbs, Addr block, BT allocTable,
                       Addr base, Addr limit)
{
  Res res;
  Index ib, il;
  Bool isAllocated;
  Addr outerBase = base, outerLimit = limit; /* interval containing [ib, il) */
  RangeStruct range, freeRange; /* interval returned by CBS */

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

    /* TODO: check these values */
    UNUSED(left);
    UNUSED(right);
    UNUSED(total);
  }

  RangeInit(&range, base, limit);
  res = CBSInsert(&freeRange, cbs, &range);

  if (!isAllocated) {
    die_expect((mps_res_t)res, MPS_RES_FAIL,
               "succeeded in inserting non-allocated block");
  } else { /* isAllocated */
    die_expect((mps_res_t)res, MPS_RES_OK,
               "failed to insert allocated block");

    NDeallocateSucceeded++;
    BTResRange(allocTable, ib, il);
    Insist(RangeBase(&freeRange) == outerBase);
    Insist(RangeLimit(&freeRange) == outerLimit);
  }
}


static void find(CBS cbs, void *block, BT alloc, Size size, Bool high,
                 FindDelete findDelete)
{
  Bool expected, found;
  Index expectedBase, expectedLimit;
  RangeStruct foundRange, oldRange;
  Addr remainderBase, remainderLimit;
  Addr origBase, origLimit;
  Size oldSize, newSize;

  origBase = origLimit = NULL;
  expected = (high ? BTFindLongResRangeHigh : BTFindLongResRange)
               (&expectedBase, &expectedLimit, alloc,
                (Index)0, (Index)ArraySize, (Count)size);

  if (expected) {
    oldSize = (expectedLimit - expectedBase) * Alignment;
    remainderBase = origBase = addrOfIndex(block, expectedBase);
    remainderLimit = origLimit = addrOfIndex(block, expectedLimit);

    switch(findDelete) {
    case FindDeleteNONE: {
      /* do nothing */
    } break;
    case FindDeleteENTIRE: {
      remainderBase = remainderLimit;
    } break;
    case FindDeleteLOW: {
      expectedLimit = expectedBase + size;
      remainderBase = addrOfIndex(block, expectedLimit);
    } break;
    case FindDeleteHIGH: {
      expectedBase = expectedLimit - size;
      remainderLimit = addrOfIndex(block, expectedBase);
    } break;
    }

    if (findDelete != FindDeleteNONE) {
      newSize = AddrOffset(remainderBase, remainderLimit);
    }

    /* TODO: check these values */
    UNUSED(oldSize);
    UNUSED(newSize);
  }

  found = (high ? CBSFindLast : CBSFindFirst)
    (&foundRange, &oldRange, cbs, size * Alignment, findDelete);

  Insist(found == expected);

  if (found) {
    Insist(expectedBase == indexOfAddr(block, RangeBase(&foundRange)));
    Insist(expectedLimit == indexOfAddr(block, RangeLimit(&foundRange)));

    if (findDelete != FindDeleteNONE) {
      Insist(RangeBase(&oldRange) == origBase);
      Insist(RangeLimit(&oldRange) == origLimit);
      BTSetRange(alloc, expectedBase, expectedLimit);
    }
  }

  return;
}


#define testArenaSIZE   (((size_t)4)<<20)

extern int main(int argc, char *argv[])
{
  unsigned i;
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
  FindDelete findDelete = FindDeleteNONE;

  randomize(argc, argv);

  NAllocateTried = NAllocateSucceeded = NDeallocateTried =
    NDeallocateSucceeded = 0;

  die(mps_arena_create(&mpsArena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  arena = (Arena)mpsArena; /* avoid pun */

  die((mps_res_t)BTCreate(&allocTable, arena, ArraySize),
      "failed to create alloc table");

  die((mps_res_t)CBSInit(arena, &cbsStruct, NULL, Alignment, TRUE,
                         mps_args_none),
      "failed to initialise CBS");
  cbs = &cbsStruct;

  BTSetRange(allocTable, 0, ArraySize); /* Initially all allocated */

  /* We're not going to use this block, but I feel unhappy just */
  /* inventing addresses. */
  die((mps_res_t)ControlAlloc(&p, arena, ArraySize * Alignment,
                              /* withReservoirPermit */ FALSE),
      "failed to allocate block");
  dummyBlock = (Addr)p; /* avoid pun */

  printf("Allocated block [%p, %p)\n", (void*)dummyBlock,
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
      case 2: findDelete = FindDeleteNONE; break;
      case 3: findDelete = FindDeleteLOW; break;
      case 4: findDelete = FindDeleteHIGH; break;
      case 5: findDelete = FindDeleteENTIRE; break;
      }
      find(cbs, dummyBlock, allocTable, size, high, findDelete);
    } break;
    }
    if (i % 5000 == 0)
      checkCBS(cbs, allocTable, dummyBlock);
  }

  /* CBSDescribe prints a very long line. */
  /* CBSDescribe(cbs, mps_lib_get_stdout()); */

  printf("\nNumber of allocations attempted: %ld\n", NAllocateTried);
  printf("Number of allocations succeeded: %ld\n", NAllocateSucceeded);
  printf("Number of deallocations attempted: %ld\n", NDeallocateTried);
  printf("Number of deallocations succeeded: %ld\n", NDeallocateSucceeded);
  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

/*  impl.c.cbstest: COALESCING BLOCK STRUCTURE TEST
 *
 *  $HopeName: MMsrc!cbstest.c(MMdevel_gavinm_splay.5) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>

#include "mpm.h"
#include "mps.h"
#include "mpsaan.h" /* ANSI arena for BTCreate and BTDestroy */
#include "testlib.h"

#ifdef MPS_OS_SU
#include "ossu.h"
#endif /* MPS_OS_SU */


SRCID(cbstest, "$HopeName: MMsrc!cbstest.c(MMdevel_gavinm_splay.5) $");

#define ArraySize ((size_t)123456)
#define nOperations ((size_t)125000)
#define SuccessRatio ((long)200) /* Ratio of failures attempted */
#define minSize ((Size)120) /* Arbitrary size */
#define addr_of_index(i) ((mps_addr_t)((char *)block + (i)))
#define index_of_addr(a) ((long)((char *)(a) - (char *)block))
#define ErrorExit(message) MPS_BEGIN printf(message "\n"); exit(1); MPS_END

static BT alloc; /* the BT which we will use for alloc */
static Arena arena; /* the ANSI arena which we use to allocate the BT */
static CBSStruct cbsStruct;
static void *block;
static long nAllocateTried, nAllocateSucceeded, nDeallocateTried,
  nDeallocateSucceeded, nNewBlocks;

typedef struct check_cbs_closure_s {
  Addr base;
  Addr limit;
  Addr oldLimit;
} check_cbs_closure_s, *check_cbs_closure_t;

static void cbs_new_callback(CBS cbs, CBSBlock cbsBlock) {
  AVERT(CBS, cbs);
  AVERT(CBSBlock, cbsBlock);

  AVER(CBSBlockSize(cbsBlock) >= minSize);

  nNewBlocks++;
}

static Bool check_cbs_action(CBS cbs, CBSBlock cbsBlock,
			     void *p, unsigned long s) {
  Addr base, limit;
  check_cbs_closure_t closure = (check_cbs_closure_t)p;

  /* Don't need to check cbs every time */
  UNUSED(cbs);
  AVER(closure != NULL);
  AVER(s == 0);

  base = CBSBlockBase(cbsBlock);
  limit = CBSBlockLimit(cbsBlock);

  if(closure->oldLimit == NULL) {
    if(base > closure->base)
      AVER(BTIsSetRange(alloc, 0, index_of_addr(base)));
    AVER(base >= closure->base);
  } else {
    AVER(base > closure->oldLimit);
    AVER(BTIsSetRange(alloc, index_of_addr(closure->oldLimit), 
		      index_of_addr(base)));
  }
  AVER(BTIsResRange(alloc, index_of_addr(base), index_of_addr(limit)));


  closure->oldLimit = limit;

  return TRUE;
}

static void check_cbs(CBS cbs) {
  check_cbs_closure_s closure;

  closure.base = (Addr)block;
  closure.limit = AddrAdd(closure.base, ArraySize);
  closure.oldLimit = NULL;

  CBSIterate(cbs, check_cbs_action, (void *)&closure, (unsigned long)0);

  if(closure.oldLimit ==NULL)
    AVER(BTIsSetRange(alloc, 0, index_of_addr(closure.limit)));
  else if(closure.limit > closure.oldLimit)
    AVER(BTIsSetRange(alloc, index_of_addr(closure.oldLimit),
		      index_of_addr(closure.limit)));
}

/* Not very uniform, but never mind. */
static long random(long limit) {
  return rnd() % limit;
}

static unsigned long next_edge(BT bt, size_t size, unsigned long base) {
  unsigned long end;
  Bool baseValue;

  AVER(bt != NULL);
  AVER(base < size);

  baseValue = BTGet(bt, base);

  for(end = base + 1; end < size && BTGet(bt, end) == baseValue; end++) 
    NOOP;

  return end;
}

static void random_range(mps_addr_t *base_return, mps_addr_t *limit_return) {
  /* base will be the start of our range */
  /* end is an edge (i.e. different from its predecessor) after base */
  /* such that there are an exponentially distributed number of edges */
  /* between base and end. */
  /* limit is a randomly chosen value in (base, limit]. */
  /* The returned range is the addresses corresponding to [base, limit). */

  unsigned long base, end, limit;

  base = random(ArraySize);

  do {
    end = next_edge(alloc, ArraySize, base);
  } while(end < ArraySize && random(2) == 0); /* p=0.5 exponential */

  AVER(end > base);

  limit = base + 1 + random(end - base);

  *base_return = addr_of_index(base);
  *limit_return = addr_of_index(limit);
}

static void allocate(mps_addr_t base, mps_addr_t limit) {
  mps_res_t res;
  long ib, il;
  Bool isRes;

  ib = index_of_addr(base);
  il = index_of_addr(limit);

  isRes = BTIsResRange(alloc, ib, il);

  nAllocateTried++;

  res = CBSDelete(&cbsStruct, base, limit);

  if(res == ResOK) {
    if(!isRes)  
      ErrorExit("succeeded in deleting non-alloced block");
    else {
      nAllocateSucceeded++;
      BTSetRange(alloc, ib, il);
    }
  } else if(res == ResFAIL) {
    if(isRes)
      ErrorExit("failed to delete alloced block");
  } else {
    ErrorExit("Unexpected result");
  }
}

static void deallocate(mps_addr_t base, mps_addr_t limit) {
  mps_res_t res;
  long ib, il;
  Bool isSet;

  ib = index_of_addr(base);
  il = index_of_addr(limit);

  isSet = BTIsSetRange(alloc, ib, il);

  nDeallocateTried++;

  res = CBSInsert(&cbsStruct, base, limit);

  if(res == ResOK) {
    if(!isSet)  
      ErrorExit("succeeded in inserting non-free block");
    else {
      nDeallocateSucceeded++;
      BTResRange(alloc, ib, il);
    }
  } else if(res == ResFAIL) {
    if(isSet)
      ErrorExit("failed to insert free block");
  } else {
    ErrorExit("Unexpected result");
  }
}

extern int main(int argc, char *argv[])
{
  mps_res_t res;
  int i;
  mps_addr_t base, limit;

  testlib_unused(argc); testlib_unused(argv);

  nAllocateTried = nAllocateSucceeded = nDeallocateTried = 
    nDeallocateSucceeded = nNewBlocks = 0;

  res = mps_arena_create((mps_arena_t *)&arena,
                         mps_arena_class_an());
  if (res != MPS_RES_OK) 
    ErrorExit("failed to create ANSI arena.");
  res = BTCreate(&alloc, arena, ArraySize);
  if (res != MPS_RES_OK) 
    ErrorExit("failed to create bit table.");

  res = CBSInit(arena, &cbsStruct, &cbs_new_callback, NULL, minSize, TRUE);
  if(res != MPS_RES_OK)
    ErrorExit("failed to initialise CBS.");

  BTSetRange(alloc, 0, ArraySize); /* Initially all allocated */
  /* We're not going to use this block, but I feel unhappy just */
  /* inventing addresses. */
  res = ArenaAlloc(&block, arena, ArraySize);
  if(res != MPS_RES_OK)
    ErrorExit("failed to allocate block");
  printf("Allocated block [%p, %p)\n", block, (char *)block + ArraySize);

  for(i = 0; i < nOperations; i++) {
    random_range(&base, &limit);
    if(random(2) == 1) {
      allocate(base, limit);
    } else {
      deallocate(base, limit);
    }
    if(i % 5000 == 0)
      check_cbs(&cbsStruct);
  }

  CBSDescribe(&cbsStruct, mps_lib_get_stdout());

  printf("\nNumber of allocations attempted: %ld\n", nAllocateTried);
  printf("Number of allocations succeeded: %ld\n", nAllocateSucceeded);
  printf("Number of deallocations attempted: %ld\n", nDeallocateTried);
  printf("Number of deallocations succeeded: %ld\n", nDeallocateSucceeded);
  printf("Number of new large blocks: %ld\n", nNewBlocks);
  printf("\nNo problems detected.\n");
  return 0;
}

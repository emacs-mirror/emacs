/* impl.c.abqtest: AVAILABLE BLOCK QUEUE TEST
 *
 * $HopeName: !abqtest.c(trunk.4) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 */

#include "abq.h"
#include "cbs.h"
#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "testlib.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <math.h>


SRCID(abqtest, "$HopeName: !abqtest.c(trunk.4) $");


static ABQStruct abq; /* the ABQ which we will use */
static Size abqSize; /* the size of the current ABQ */

#define ABQ_SIZE 10
#define TEST_ITER 10000


static unsigned long abqRnd(unsigned long n)
{
  return rnd()%n;
}

static int pushee = 1;
static int popee = 1;
static int deleted = 0;

typedef struct TestStruct *Test;

typedef struct TestStruct
{
  Test next;
  int id;
  CBSBlockStruct cbsBlockStruct;
} TestStruct;

static CBSBlock TestCBSBlock(Test t)
{
  return &t->cbsBlockStruct;
}

static Test CBSBlockTest(CBSBlock c)
{
  return PARENT(TestStruct, cbsBlockStruct, c);
}

static Test testBlocks = NULL;

static CBSBlock CreateCBSBlock(int no)
{
  Test b = malloc(sizeof(TestStruct));
  AVER(b != NULL);

  b->next = testBlocks;
  b->id = no;
  b->cbsBlockStruct.base = 0;
  b->cbsBlockStruct.limit = 0;

  testBlocks = b;

  return TestCBSBlock(b);
}


static void DestroyCBSBlock(CBSBlock c)
{
  Test b = CBSBlockTest(c);

  if (b == testBlocks)
    testBlocks = b->next;
  else {
    Test prev;

    for (prev = testBlocks; prev != 0; prev = prev->next)
      if (prev->next == b) {
        prev->next = b->next;
        break;
      }
  }

  free(b);
}


static void step(void)
{
  Res res;
  CBSBlock a;

  switch (abqRnd(9)) {
    case 0: case 1: case 2: case 3:
  push:
      res = ABQPush(&abq, CreateCBSBlock(pushee));
      if (res != ResOK) {
        goto pop;
      }
      pushee++;
      break;
    case 5: case 6: case 7: case 8:
  pop:
      res = ABQPop(&abq, &a);
      if (res != ResOK){
        goto push;
      }
      if (popee == deleted) {
      	popee++;
        deleted = 0;
      }
      AVER(CBSBlockTest(a)->id == popee);
      popee++;
      DestroyCBSBlock(a);
      break;
    default:
      if (!deleted & (pushee > popee)) {
        Test b;

        deleted = abqRnd (pushee - popee) + popee;
        for (b = testBlocks; b != NULL; b = b->next)
          if (b->id == deleted)
            break;
        AVER(b != NULL);
        res = ABQDelete(&abq, TestCBSBlock(b));
        AVER(res == ResOK);
      }
  }
}

extern int main(void)
{
  mps_res_t res;
  mps_arena_t arena;
  int i;

  abqSize = 0;

  res = mps_arena_create(&arena, mps_arena_class_vm());
  if (res != MPS_RES_OK) {
    printf("failed to create VM arena.\n");
    return 1;
  }

  res = ABQInit((Arena)arena, &abq, NULL, ABQ_SIZE);
  if (res == ResOK) {
    abqSize = ABQ_SIZE;
  } else {
    printf("ABQCreate returned %d\n",res);
    return 1;
  }

  for (i = 0; i < TEST_ITER; i++) {
    step();
  }

  printf("All tests passed.\n");
  return 0;
}

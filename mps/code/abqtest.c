/* abqtest.c: AVAILABLE BLOCK QUEUE TEST
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#include "abq.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpscmfs.h"
#include "mpstd.h"
#include "testlib.h"

#include <stdio.h> /* printf */


SRCID(abqtest, "$Id$");

static mps_pool_t pool;
static ABQStruct abq; /* the ABQ which we will use */
static Size abqSize; /* the size of the current ABQ */

#define ABQ_SIZE 10
#define TEST_ITER 10000


static unsigned long abqRnd(unsigned long n)
{
  return rnd()%n;
}


static unsigned pushee = 1;
static unsigned popee = 1;
static unsigned deleted = 0;


typedef struct TestBlockStruct *TestBlock;

typedef struct TestBlockStruct
{
  TestBlock next;
  unsigned id;
  Addr base;
  Addr limit;
} TestBlockStruct;


static TestBlock testBlocks = NULL;


static TestBlock CreateTestBlock(unsigned no)
{
  TestBlock b;
  mps_addr_t p;

  die(mps_alloc(&p, pool, sizeof(TestBlockStruct)), "alloc");

  b = p;
  b->next = testBlocks;
  b->id = no;
  b->base = 0;
  b->limit = 0;

  testBlocks = b;

  return b;
}


static void DestroyTestBlock(TestBlock b)
{
  if (b == testBlocks)
    testBlocks = b->next;
  else {
    TestBlock prev;

    for (prev = testBlocks; prev != 0; prev = prev->next)
      if (prev->next == b) {
        prev->next = b->next;
        break;
      }
  }

  mps_free(pool, b, sizeof(TestBlockStruct));
}

typedef struct TestClosureStruct *TestClosure;
typedef struct TestClosureStruct {
  TestBlock b;
  Res res;
} TestClosureStruct;

static Bool TestDeleteCallback(Bool *deleteReturn, void *element,
                               void *closure)
{
  TestBlock *a = (TestBlock *)element;
  TestClosure cl = (TestClosure)closure;
  if (*a == cl->b) {
    *deleteReturn = TRUE;
    cl->res = ResOK;
  } else {
    *deleteReturn = FALSE;
  }
  return TRUE;
}


static void step(void)
{
  TestBlock a;

  switch (abqRnd(9)) {
    case 0: case 1: case 2: case 3:
  push:
      a = CreateTestBlock(pushee);
      if (!ABQPush(&abq, &a)) {
        goto pop;
      }
      pushee++;
      break;
    case 5: case 6: case 7: case 8:
  pop:
      if (!ABQPop(&abq, &a)) {
        goto push;
      }
      if (popee == deleted) {
        popee++;
        deleted = 0;
      }
      cdie(a->id == popee, "pop");
      popee++;
      DestroyTestBlock(a);
      break;
    default:
      if (!deleted && (pushee > popee)) {
        TestBlock b;
        TestClosureStruct cl;
        deleted = (unsigned)abqRnd (pushee - popee) + popee;
        for (b = testBlocks; b != NULL; b = b->next)
          if (b->id == deleted)
            break;
        cdie(b != NULL, "found to delete");
        cl.b = b;
        cl.res = ResFAIL;
        ABQIterate(&abq, TestDeleteCallback, &cl);
        cdie(cl.res == ResOK, "ABQIterate");
      }
  }
}

int main(int argc, char *argv[])
{
  mps_arena_t arena;
  int i;

  testlib_init(argc, argv);

  abqSize = 0;

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
      "mps_arena_create");

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_MFS_UNIT_SIZE, sizeof(TestBlockStruct));
    die(mps_pool_create_k(&pool, arena, mps_class_mfs(), args), "pool_create");
  } MPS_ARGS_END(args);

  die(ABQInit((Arena)arena, &abq, NULL, ABQ_SIZE, sizeof(TestBlock)),
      "ABQInit");

  abqSize = ABQ_SIZE;

  for (i = 0; i < TEST_ITER; i++) {
    step();
  }

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

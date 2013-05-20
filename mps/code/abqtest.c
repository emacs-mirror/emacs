/* abqtest.c: AVAILABLE BLOCK QUEUE TEST
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 */

#include "abq.h"
#include "cbs.h"
#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "testlib.h"
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "mpstd.h"
#ifdef MPS_OS_IA
struct itimerspec; /* stop complaints from time.h */
#endif
#include <time.h>
#include <math.h>


SRCID(abqtest, "$Id$");


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
  TestBlock b = malloc(sizeof(TestBlockStruct));
  cdie(b != NULL, "malloc");

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

  free(b);
}

typedef struct TestClosureStruct *TestClosure;
typedef struct TestClosureStruct {
  TestBlock b;
  Res res;
} TestClosureStruct;

static Res TestDeleteCallback(ABQDisposition *dispositionReturn, Addr element,
                              void *closureP)
{
  TestBlock *a = (TestBlock *)element;
  TestClosure cl = (TestClosure)closureP;
  if (*a == cl->b) {
    *dispositionReturn = ABQDispositionDELETE;
    cl->res = ResOK;
    return ResFAIL;
  } else {
    *dispositionReturn = ABQDispositionKEEP;
    return ResOK;
  }
}


static void step(void)
{
  Res res;
  TestBlock a;

  switch (abqRnd(9)) {
    case 0: case 1: case 2: case 3:
  push:
      a = CreateTestBlock(pushee);
      res = ABQPush(&abq, (Addr)&a);
      if (res != ResOK) {
        goto pop;
      }
      pushee++;
      break;
    case 5: case 6: case 7: case 8:
  pop:
      res = ABQPop(&abq, (Addr)&a);
      if (res != ResOK){
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
      if (!deleted & (pushee > popee)) {
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


#define testArenaSIZE   (((size_t)4)<<20)

extern int main(int argc, char *argv[])
{
  mps_arena_t arena;
  int i;

  randomize(argc, argv);

  abqSize = 0;

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");

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

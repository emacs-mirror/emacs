/* impl.c.abqtest: AVAILABLE BLOCK QUEUE TEST
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 */

#include "abq.h"
#include "cbs.h"
#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "testlib.h"
#include <stdlib.h>
#include <stdarg.h>
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
  cdie(b != NULL, "malloc");

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
      cdie(CBSBlockTest(a)->id == popee, "pop");
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
        cdie(b != NULL, "found to delete");
        res = ABQDelete(&abq, TestCBSBlock(b));
        cdie(res == ResOK, "ABQDelete");
      }
  }
}


#define testArenaSIZE   (((size_t)4)<<20)

extern int main(int argc, char *argv[])
{
  Res res;
  mps_arena_t arena;
  int i;

  randomize(argc, argv);

  abqSize = 0;

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");

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

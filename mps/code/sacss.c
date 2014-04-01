/* sacss.c: SAC MANUAL ALLOC STRESS TEST
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 */

#include "mpscmv.h"
#include "mpscmvff.h"
#include "mpslib.h"
#include "mpsavm.h"
#include "mps.h"

#include "testlib.h"
#include "mpslib.h"

#include <stdio.h>
#include "mpstd.h"
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>


#define TRUE  1
#define FALSE 0

#define testArenaSIZE   ((((size_t)64)<<20) - 4)
#define testSetSIZE 200
#define testLOOPS 10

#define topClassSIZE 0xA00
#define classCOUNT 4


/* make -- allocate an object */

static mps_res_t make(mps_addr_t *p, mps_sac_t sac, size_t size)
{
  mps_res_t res;

  MPS_SAC_ALLOC(res, *p, sac, size, FALSE);
  return res;
}


/* stress -- create a pool of the requested type and allocate in it */

static mps_res_t stress(mps_class_t class,
                        size_t classes_count, mps_sac_classes_s *classes,
                        size_t (*size)(size_t i), mps_arena_t arena, ...)
{
  mps_res_t res;
  mps_pool_t pool;
  mps_sac_t sac;
  va_list arg;
  size_t i, k;
  int *ps[testSetSIZE];
  size_t ss[testSetSIZE];

  va_start(arg, arena);
  res = mps_pool_create_v(&pool, arena, class, arg);
  va_end(arg);
  if (res != MPS_RES_OK)
    return res;

  die(mps_sac_create(&sac, pool, classes_count, classes), "SACCreate");

  /* allocate a load of objects */
  for (i = 0; i < testSetSIZE; ++i) {
    ss[i] = (*size)(i);

    res = make((mps_addr_t *)&ps[i], sac, ss[i]);
    if (res != MPS_RES_OK)
      return res;
    if (ss[i] >= sizeof(ps[i]))
      *ps[i] = 1; /* Write something, so it gets swap. */
  }

  mps_pool_check_fenceposts(pool);

  for (k = 0; k < testLOOPS; ++k) {
    /* shuffle all the objects */
    for (i=0; i<testSetSIZE; ++i) {
      int j = (int)(rnd()%(unsigned)(testSetSIZE-i));
      void *tp;
      size_t ts;
     
      tp = ps[j]; ts = ss[j];
      ps[j] = ps[i]; ss[j] = ss[i];
      ps[i] = tp; ss[i] = ts;
    }
    if (k == (testLOOPS / 2)) mps_sac_flush(sac);
    /* free half of the objects */
    /* upper half, as when allocating them again we want smaller objects */
    /* see randomSize() */
    switch (k % 2) {
    case 0: {
      for (i=testSetSIZE/2; i<testSetSIZE; ++i)
        MPS_SAC_FREE(sac, (mps_addr_t)ps[i], ss[i]);
    } break;
    case 1: {
      for (i=testSetSIZE/2; i<testSetSIZE; ++i)
        mps_sac_free(sac, (mps_addr_t)ps[i], ss[i]);
    } break;
    }
    /* allocate some new objects */
    for (i=testSetSIZE/2; i<testSetSIZE; ++i) {
      ss[i] = (*size)(i);
      switch (k % 2) {
      case 0: {
        res = make((mps_addr_t *)&ps[i], sac, ss[i]);
      } break;
      case 1: {
        res = mps_sac_alloc((mps_addr_t *)&ps[i], sac, ss[i], FALSE);
      } break;
      }     
      if (res != MPS_RES_OK) return res;
    }
  }
   
  mps_sac_destroy(sac);
  mps_pool_destroy(pool);

  return MPS_RES_OK;
}


/* randomSize8 -- produce sizes both latge and small */

static size_t randomSize8(size_t i)
{
  size_t maxSize = 2 * 160 * 0x2000;
  size_t size;

  /* Reduce by a factor of 2 every 10 cycles.  Total allocation about 40 MB. */
  size = rnd() % max((maxSize >> (i / 10)), 2) + 1;
  return size;
}


/* testInArena -- test all the pool classes in the given arena */

static mps_pool_debug_option_s debugOptions = {
  /* .fence_template = */   (const void *)"postpostpostpost",
  /* .fence_size = */       MPS_PF_ALIGN,
  /* .free_template = */    (const void *)"DEAD",
  /* .free_size = */        4
};

static mps_sac_classes_s classes[4] = {
  {MPS_PF_ALIGN, 1, 1}, 
  {MPS_PF_ALIGN * 2, 1, 2},
  {128 + MPS_PF_ALIGN, 9, 5},
  {topClassSIZE, 9, 4} 
};

static void testInArena(mps_arena_t arena)
{
  printf("MVFF\n\n");
  die(stress(mps_class_mvff(), classCOUNT, classes, randomSize8, arena,
             (size_t)65536, (size_t)32, (mps_align_t)MPS_PF_ALIGN, TRUE, TRUE, TRUE),
      "stress MVFF");
  printf("MV debug\n\n");
  die(stress(mps_class_mv_debug(), classCOUNT, classes, randomSize8, arena,
             &debugOptions, (size_t)65536, (size_t)32, (size_t)65536),
      "stress MV debug");
  printf("MV\n\n");
  die(stress(mps_class_mv(), classCOUNT, classes, randomSize8, arena,
             (size_t)65536, (size_t)32, (size_t)65536),
      "stress MV");
}


int main(int argc, char *argv[])
{
  mps_arena_t arena;

  testlib_init(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  testInArena(arena);
  mps_arena_destroy(arena);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, testArenaSIZE);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_ZONED, FALSE);
    die(mps_arena_create_k(&arena, mps_arena_class_vm(), args),
        "mps_arena_create");
  } MPS_ARGS_END(args);
  testInArena(arena);
  mps_arena_destroy(arena);

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

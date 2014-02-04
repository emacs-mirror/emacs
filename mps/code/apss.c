/* apss.c: AP MANUAL ALLOC STRESS TEST
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 */


#include "mpscmv.h"
#include "mpscmvff.h"
#include "mpscmvt.h"
#include "mpslib.h"
#include "mpsacl.h"
#include "mpsavm.h"

#include "testlib.h"
#include "mpslib.h"

#include <stdarg.h>
#include <stdlib.h> /* malloc */


#define testArenaSIZE   ((((size_t)3)<<24) - 4)
#define testSetSIZE 200
#define testLOOPS 10


/* make -- allocate one object */

static mps_res_t make(mps_addr_t *p, mps_ap_t ap, size_t size)
{
  mps_res_t res;

  do {
    MPS_RESERVE_BLOCK(res, *p, ap, size);
    if(res != MPS_RES_OK)
      return res;
  } while(!mps_commit(ap, *p, size));

  return MPS_RES_OK;
}


/* stress -- create a pool of the requested type and allocate in it */

static mps_res_t stress(mps_class_t class, size_t (*size)(unsigned long i),
                        mps_arena_t arena, ...)
{
  mps_res_t res = MPS_RES_OK;
  mps_pool_t pool;
  mps_ap_t ap;
  va_list arg;
  unsigned long i, k;
  int *ps[testSetSIZE];
  size_t ss[testSetSIZE];

  va_start(arg, arena);
  res = mps_pool_create_v(&pool, arena, class, arg);
  va_end(arg);
  if (res != MPS_RES_OK)
    return res;

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");

  /* allocate a load of objects */
  for (i=0; i<testSetSIZE; ++i) {
    ss[i] = (*size)(i);

    res = make((mps_addr_t *)&ps[i], ap, ss[i]);
    if (res != MPS_RES_OK)
      goto allocFail;
    if (ss[i] >= sizeof(ps[i]))
      *ps[i] = 1; /* Write something, so it gets swap. */
  }

  mps_pool_check_fenceposts(pool);

  for (k=0; k<testLOOPS; ++k) {
    /* shuffle all the objects */
    for (i=0; i<testSetSIZE; ++i) {
      unsigned long j = rnd()%(testSetSIZE-i);
      void *tp;
      size_t ts;
     
      tp = ps[j]; ts = ss[j];
      ps[j] = ps[i]; ss[j] = ss[i];
      ps[i] = tp; ss[i] = ts;
    }
    /* free half of the objects */
    /* upper half, as when allocating them again we want smaller objects */
    /* see randomSize() */
    for (i=testSetSIZE/2; i<testSetSIZE; ++i) {
      mps_free(pool, (mps_addr_t)ps[i], ss[i]);
      /* if (i == testSetSIZE/2) */
      /*   PoolDescribe((Pool)pool, mps_lib_stdout); */
    }
    /* allocate some new objects */
    for (i=testSetSIZE/2; i<testSetSIZE; ++i) {
      ss[i] = (*size)(i);
      res = make((mps_addr_t *)&ps[i], ap, ss[i]);
      if (res != MPS_RES_OK)
        goto allocFail;
    }
  }

allocFail:
  mps_ap_destroy(ap);
  mps_pool_destroy(pool);

  return res;
}


#define max(a, b) (((a) > (b)) ? (a) : (b))

#define alignUp(w, a)       (((w) + (a) - 1) & ~((size_t)(a) - 1))


/* randomSizeAligned -- produce sizes both large and small,
 * aligned by platform alignment */

static size_t randomSizeAligned(unsigned long i)
{
  size_t maxSize = 2 * 160 * 0x2000;
  /* Reduce by a factor of 2 every 10 cycles.  Total allocation about 40 MB. */
  return alignUp(rnd() % max((maxSize >> (i / 10)), 2) + 1, MPS_PF_ALIGN);
}


static mps_pool_debug_option_s bothOptions8 = {
  /* .fence_template = */   (const void *)"postpost",
  /* .fence_size = */       8,
  /* .free_template = */    (const void *)"DEAD",
  /* .free_size = */        4
};

static mps_pool_debug_option_s bothOptions16 = {
  /* .fence_template = */   (const void *)"postpostpostpost",
  /* .fence_size = */       16,
  /* .free_template = */    (const void *)"DEAD",
  /* .free_size = */        4
};

static mps_pool_debug_option_s fenceOptions = {
  /* .fence_template = */   (const void *)"\0XXX ''\"\"'' XXX\0",
  /* .fence_size = */       16,
  /* .free_template = */    NULL,
  /* .free_size = */        0
};

/* testInArena -- test all the pool classes in the given arena */

static void testInArena(mps_arena_t arena, mps_pool_debug_option_s *options)
{
  mps_res_t res;

  /* IWBN to test MVFFDebug, but the MPS doesn't support debugging APs, */
  /* yet (MV Debug works here, because it fakes it through PoolAlloc). */
  printf("MVFF\n");
  res = stress(mps_class_mvff(), randomSizeAligned, arena,
               (size_t)65536, (size_t)32, (mps_align_t)MPS_PF_ALIGN, TRUE, TRUE, TRUE);
  if (res == MPS_RES_COMMIT_LIMIT) return;
  die(res, "stress MVFF");

  printf("MV debug\n");
  res = stress(mps_class_mv_debug(), randomSizeAligned, arena,
               options, (size_t)65536, (size_t)32, (size_t)65536);
  if (res == MPS_RES_COMMIT_LIMIT) return;
  die(res, "stress MV debug");

  printf("MV\n");
  res = stress(mps_class_mv(), randomSizeAligned, arena,
               (size_t)65536, (size_t)32, (size_t)65536);
  if (res == MPS_RES_COMMIT_LIMIT) return;
  die(res, "stress MV");

  printf("MVT\n");
  res = stress(mps_class_mvt(), randomSizeAligned, arena,
               (size_t)8, (size_t)32, (size_t)65536, (mps_word_t)4,
               (mps_word_t)50);
  if (res == MPS_RES_COMMIT_LIMIT) return;
  die(res, "stress MVT");
}


int main(int argc, char *argv[])
{
  mps_arena_t arena;
  mps_pool_debug_option_s *bothOptions;
  
  bothOptions = MPS_PF_ALIGN == 8 ? &bothOptions8 : &bothOptions16;

  randomize(argc, argv);
  mps_lib_assert_fail_install(assert_die);

  die(mps_arena_create(&arena, mps_arena_class_vm(), 2*testArenaSIZE),
      "mps_arena_create");
  die(mps_arena_commit_limit_set(arena, testArenaSIZE), "commit limit");
  testInArena(arena, &fenceOptions);
  mps_arena_destroy(arena);

#if 0 /* FIXME: Restore when arena can take an option */
  die(mps_arena_create(&arena, mps_arena_class_vmnz(), 2*testArenaSIZE),
      "mps_arena_create");
  testInArena(arena, bothOptions);
  mps_arena_destroy(arena);
#endif

  die(mps_arena_create(&arena, mps_arena_class_cl(),
                       testArenaSIZE, malloc(testArenaSIZE)),
      "mps_arena_create");
  testInArena(arena, bothOptions);
  mps_arena_destroy(arena);

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

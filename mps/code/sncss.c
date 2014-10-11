/* framess.c: ALLOCATION FRAME STRESS TEST
 *
 * $Id$
 * Copyright (c) 2014 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
#include "mpscmv.h"
#include "mpscmvt.h"
#include "mpscmvff.h"
#include "mpscsnc.h"
#include "mpsavm.h"
#include "mps.h"
#include "testlib.h"

#include <stdio.h> /* printf */


/* make -- allocate one object, and if it's big enough, store the size
 * in the first word, for the benefit of the object format */

static mps_res_t make(mps_addr_t *p, mps_ap_t ap, size_t size)
{
  mps_addr_t obj;
  mps_res_t res;

  do {
    res = mps_reserve(&obj, ap, size);
    if(res != MPS_RES_OK)
      return res;
    if(size >= sizeof size)
      *(size_t *)obj = size;
  } while(!mps_commit(ap, *p, size));

  *p = obj;     
  return MPS_RES_OK;
}


/* Simple format for the SNC pool. Each object starts with a word
   giving its length. */

static mps_res_t fmtScan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  testlib_unused(ss);
  testlib_unused(base);
  testlib_unused(limit);
  return MPS_RES_OK;
}

static mps_addr_t fmtSkip(mps_addr_t addr)
{
  size_t *obj = addr;
  return (char *)addr + *obj;
}

static void fmtPad(mps_addr_t addr, size_t size)
{
  size_t *obj = addr;
  *obj = size;
}

static void test(mps_pool_class_t pool_class)
{
  size_t i, j;
  mps_align_t align;
  mps_arena_t arena;
  mps_fmt_t fmt;
  mps_pool_t pool;
  struct ap_s {
    mps_ap_t ap;
    size_t frames;
    mps_frame_t frame[20];
    size_t alloc[21];
  } aps[3];

  align = sizeof(void *) << (rnd() % 4);

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
      "mps_arena_create");

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, fmtScan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, fmtSkip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, fmtPad);
    die(mps_fmt_create_k(&fmt, arena, args), "fmt_create");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ALIGN, align);
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
    die(mps_pool_create_k(&pool, arena, pool_class, args), "pool_create");
  } MPS_ARGS_END(args);

  for (i = 0; i < NELEMS(aps); ++i) {
    struct ap_s *a = &aps[i];
    die(mps_ap_create_k(&a->ap, pool, mps_args_none), "ap_create");
    a->frames = 0;
    a->alloc[0] = 0;
  }

  for (i = 0; i < 100000; ++i) {
    size_t k = rnd() % NELEMS(aps);
    struct ap_s *a = &aps[k];
    if (rnd() % 100 == 0) {
      j = rnd() % NELEMS(a->frame);
      if (j < a->frames) {
        a->frames = j;
        mps_ap_frame_pop(a->ap, a->frame[j]);
        printf("%lu: pop %lu\n", (unsigned long)k, (unsigned long)j);
      } else {
        mps_ap_frame_push(&a->frame[a->frames], a->ap);
        printf("%lu: push %lu\n", (unsigned long)k, (unsigned long)a->frames);
        ++ a->frames;
        a->alloc[a->frames] = 0;
      }
    } else {
      size_t size = alignUp(1 + rnd() % 128, align);
      mps_addr_t p;
      make(&p, a->ap, size);
      a->alloc[a->frames] += size;
    }
  }

  {
    size_t alloc = 0;
    size_t unused = 0;
    for (i = 0; i < NELEMS(aps); ++i) {
      struct ap_s *a = &aps[i];
      for (j = 0; j <= a->frames; ++j) {
        alloc += a->alloc[j];
      }
      unused += AddrOffset(a->ap->init, a->ap->limit);
    }
    printf("alloc=%lu unused=%lu, total=%lu free=%lu a+u+f=%lu\n",
           (unsigned long)alloc,
           (unsigned long)unused,
           (unsigned long)mps_pool_total_size(pool),
           (unsigned long)mps_pool_free_size(pool),
           (unsigned long)(alloc + unused + mps_pool_free_size(pool)));
  }

  for (i = 0; i < NELEMS(aps); ++i) {
    mps_ap_destroy(aps[i].ap);
  }
  mps_pool_destroy(pool);
  mps_fmt_destroy(fmt);
  mps_arena_destroy(arena);
}

int main(int argc, char *argv[])
{
  testlib_init(argc, argv);

  test(mps_class_snc());

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

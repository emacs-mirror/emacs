/* sncss.c: SNC STRESS TEST
 *
 * $Id$
 * Copyright (c) 2014-2020 Ravenbrook Limited.  See end of file for license.
 */

#include "mpm.h"
#include "mpscmvt.h"
#include "mpscmvff.h"
#include "mpscsnc.h"
#include "mpsavm.h"
#include "mps.h"
#include "testlib.h"

#include <stdio.h> /* printf */


/* Simple format for the SNC pool. */

typedef struct obj_s {
  size_t size;
  int pad;
} obj_s, *obj_t;

/* make -- allocate one object, and if it's big enough, store the size
 * in the first word, for the benefit of the object format */

static mps_res_t make(mps_addr_t *p, mps_ap_t ap, size_t size)
{
  mps_addr_t addr;
  mps_res_t res;

  do {
    obj_t obj;
    res = mps_reserve(&addr, ap, size);
    if (res != MPS_RES_OK)
      return res;
    obj = addr;
    obj->size = size;
    obj->pad = 0;
  } while (!mps_commit(ap, addr, size));

  *p = addr;
  return MPS_RES_OK;
}

static mps_res_t fmtScan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  testlib_unused(ss);
  testlib_unused(base);
  testlib_unused(limit);
  return MPS_RES_OK;
}

static mps_addr_t fmtSkip(mps_addr_t addr)
{
  obj_t obj = addr;
  return (char *)addr + obj->size;
}

static void fmtPad(mps_addr_t addr, size_t size)
{
  obj_t obj = addr;
  obj->size = size;
  obj->pad = 1;
}

typedef struct env_s {
  size_t obj;
  size_t pad;
} env_s, *env_t;

static void fmtVisitor(mps_addr_t object, mps_fmt_t format,
                       mps_pool_t pool, void *p, size_t s)
{
  env_t env = p;
  obj_t obj = object;
  testlib_unused(format);
  testlib_unused(pool);
  testlib_unused(s);
  if (obj->pad)
    env->pad += obj->size;
  else
    env->obj += obj->size;
}

#define AP_MAX 3                /* Number of allocation points */
#define DEPTH_MAX 20            /* Maximum depth of frame push */

typedef struct ap_s {
  mps_ap_t ap;                  /* An allocation point on an ANC pool */
  size_t depth;                 /* Number of frames pushed */
  size_t alloc[DEPTH_MAX + 1];  /* Total allocation at each depth */
  size_t push[DEPTH_MAX];       /* Total allocation when we pushed */
  mps_frame_t frame[DEPTH_MAX]; /* The frame pointers at each depth */
} ap_s, *ap_t;

static void test(mps_pool_class_t pool_class)
{
  size_t i, j;
  mps_align_t align;
  mps_arena_t arena;
  mps_fmt_t fmt;
  mps_pool_t pool;
  ap_s aps[AP_MAX];

  align = sizeof(obj_s) << (rnd() % 4);

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none),
      "mps_arena_create");

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ALIGN, align);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN, fmtScan);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP, fmtSkip);
    MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD, fmtPad);
    die(mps_fmt_create_k(&fmt, arena, args), "fmt_create");
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_FORMAT, fmt);
    die(mps_pool_create_k(&pool, arena, pool_class, args), "pool_create");
  } MPS_ARGS_END(args);

  for (i = 0; i < NELEMS(aps); ++i) {
    ap_t a = &aps[i];
    die(mps_ap_create_k(&a->ap, pool, mps_args_none), "ap_create");
    a->depth = 0;
    a->alloc[0] = 0;
  }

  for (i = 0; i < 1000000; ++i) {
    size_t k = rnd() % NELEMS(aps);
    ap_t a = &aps[k];
    if (rnd() % 10 == 0) {
      j = rnd() % NELEMS(a->frame);
      if (j < a->depth) {
        a->depth = j;
        mps_ap_frame_pop(a->ap, a->frame[j]);
        a->alloc[j] = a->push[j];
      } else {
        a->push[a->depth] = a->alloc[a->depth];
        mps_ap_frame_push(&a->frame[a->depth], a->ap);
        ++ a->depth;
        a->alloc[a->depth] = 0;
      }
    } else {
      size_t size = alignUp(1 + rnd() % 128, align);
      mps_addr_t p;
      make(&p, a->ap, size);
      a->alloc[a->depth] += size;
    }
  }

  {
    env_s env = {0, 0};
    size_t alloc = 0;
    size_t free = mps_pool_free_size(pool);
    size_t total = mps_pool_total_size(pool);

    for (i = 0; i < NELEMS(aps); ++i) {
      ap_t a = &aps[i];
      for (j = 0; j <= a->depth; ++j) {
        alloc += a->alloc[j];
      }
    }

    mps_arena_formatted_objects_walk(arena, fmtVisitor, &env, 0);

    printf("alloc=%lu obj=%lu pad=%lu free=%lu total=%lu\n",
           (unsigned long)alloc,
           (unsigned long)env.obj,
           (unsigned long)env.pad,
           (unsigned long)free,
           (unsigned long)total);
    Insist(alloc == env.obj);
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
 * Copyright (C) 2014-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
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

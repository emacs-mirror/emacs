/* fotest.c: FAIL-OVER TEST
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * This tests fail-over behaviour in low memory situations. The MVFF
 * and MVT pool classes normally maintain their list of free blocks in
 * a Coalescing Block Structure (CBS), but if the CBS cannot handle a
 * request due to running out of memory, they fall back to a Freelist
 * (which has zero memory overhead, at some cost in performance).
 *
 * This is a white box test: it monkey-patches the MFS pool's alloc
 * method with a method that always returns a memory error code.
 */


#include "mpscmvff.h"
#include "mpscmvt.h"
#include "mpsavm.h"

#include "testlib.h"

#include "cbs.h"
#include "mpm.h"
#include "mpmst.h"
#include "mpmtypes.h"
#include "poolmfs.h"

#include <stdio.h> /* printf */


#define testArenaSIZE   ((((size_t)3)<<24) - 4)
#define testSetSIZE 200 /* TODO: 10 * arena grain size / sizeof cbs_struct */
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


/* The original alloc method on the MFS pool. */
static PoolAllocMethod mfs_alloc;


/* Are we currently in a part of the test that is allowed to fail in the case
 * where we run out of memory? This controls the behaviour of oomAlloc. */
static Bool simulate_allocation_failure = FALSE;

/* How many times has oomAlloc failed on purpose. */
static unsigned long failure_count = 0;

/* oomAlloc -- allocation function that reliably fails
 *
 * Returns a randomly chosen memory error code (and increments
 * `failure_count`) if `simulate_allocation_failure`. The point is to verify
 * that none of these errors affects the caller. */

static Res oomAlloc(Addr *pReturn, Pool pool, Size size)
{
  if (simulate_allocation_failure) {
    /* Simulate a single failure in order to enforce the fail-over behaviour. */
    ++ failure_count;
    simulate_allocation_failure = 0;
    switch (rnd() % 3) {
    case 0:
      return ResRESOURCE;
    case 1:
      return ResMEMORY;
    default:
      return ResCOMMIT_LIMIT;
    }
  } else {
    /* Failure here is allowed, so attempt allocation as normal.
     * (see job004041 and job004104). */
    return mfs_alloc(pReturn, pool, size);
  }
}


/* stress -- create an allocation point and allocate in it */

static mps_res_t stress(size_t (*size)(unsigned long, mps_align_t),
                        mps_align_t alignment, mps_pool_t pool)
{
  mps_res_t res = MPS_RES_OK;
  mps_ap_t ap;
  unsigned long i, k;
  int *ps[testSetSIZE];
  size_t ss[testSetSIZE];

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");

  /* allocate a load of objects */
  for (i=0; i<testSetSIZE; ++i) {
    mps_addr_t obj;
    ss[i] = (*size)(i, alignment);
    res = make(&obj, ap, ss[i]);
    if (res != MPS_RES_OK)
      goto allocFail;
    ps[i] = obj;
    if (ss[i] >= sizeof(ps[i]))
      *ps[i] = 1; /* Write something, so it gets swap. */
  }

  failure_count = 0;

  for (k=0; k<testLOOPS; ++k) {
    /* Use oomAlloc for the first iteration and then with 0.5 probability. */
    CLASS_STATIC(MFSPool).alloc = (k>0 && rnd() % 2) ? mfs_alloc : oomAlloc;

    /* shuffle all the objects */
    for (i=0; i<testSetSIZE; ++i) {
      unsigned long j = i + rnd()%(testSetSIZE-i);
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
      simulate_allocation_failure = TRUE;
      mps_free(pool, (mps_addr_t)ps[i], ss[i]);
      simulate_allocation_failure = FALSE;
      /* if (i == testSetSIZE/2) */
      /*   PoolDescribe((Pool)pool, mps_lib_stdout); */
    }

    /* allocate some new objects */
    for (i=testSetSIZE/2; i<testSetSIZE; ++i) {
      mps_addr_t obj;
      ss[i] = (*size)(i, alignment);
      res = make(&obj, ap, ss[i]);
      if (res != MPS_RES_OK)
        goto allocFail;
      ps[i] = obj;
    }
  }
  CLASS_STATIC(MFSPool).alloc = mfs_alloc;

  Insist(failure_count > 0);

allocFail:
  mps_ap_destroy(ap);

  return res;
}


/* randomSizeAligned -- produce sizes both large and small,
 * aligned by platform alignment */

static size_t randomSizeAligned(unsigned long i, mps_align_t alignment)
{
  size_t maxSize = 2 * 160 * 0x2000;
  /* Reduce by a factor of 2 every 10 cycles.  Total allocation about 40 MB. */
  return alignUp(rnd() % max((maxSize >> (i / 10)), 2) + 1, alignment);
}

int main(int argc, char *argv[])
{
  mps_arena_t arena;
  mps_pool_t pool;
  mps_align_t alignment;

  testlib_init(argc, argv);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  mfs_alloc = CLASS_STATIC(MFSPool).alloc;
  alignment = sizeof(void *) << (rnd() % 4);
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_EXTEND_BY, (64 + rnd() % 64) * 1024);
    MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, (1 + rnd() % 8) * 8);
    MPS_ARGS_ADD(args, MPS_KEY_ALIGN, alignment);
    MPS_ARGS_ADD(args, MPS_KEY_MVFF_ARENA_HIGH, rnd() % 2);
    MPS_ARGS_ADD(args, MPS_KEY_MVFF_SLOT_HIGH, rnd() % 2);
    MPS_ARGS_ADD(args, MPS_KEY_MVFF_FIRST_FIT, rnd() % 2);
    die(mps_pool_create_k(&pool, arena, mps_class_mvff(), args), "create MVFF");
  } MPS_ARGS_END(args);
  die(stress(randomSizeAligned, alignment, pool), "stress MVFF");
  mps_pool_destroy(pool);
  mps_arena_destroy(arena);

  die(mps_arena_create(&arena, mps_arena_class_vm(), testArenaSIZE),
      "mps_arena_create");
  alignment = sizeof(void *) << (rnd() % 4);
  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ALIGN, alignment);
    MPS_ARGS_ADD(args, MPS_KEY_MIN_SIZE, (1 + rnd() % 4) * 4);
    MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, (1 + rnd() % 8) * 16);
    MPS_ARGS_ADD(args, MPS_KEY_MAX_SIZE, (1 + rnd() % 4) * 1024);
    MPS_ARGS_ADD(args, MPS_KEY_MVT_RESERVE_DEPTH, (1 + rnd() % 64) * 16);
    MPS_ARGS_ADD(args, MPS_KEY_MVT_FRAG_LIMIT, (rnd() % 101) / 100.0);
    die(mps_pool_create_k(&pool, arena, mps_class_mvt(), args), "create MVT");
  } MPS_ARGS_END(args);
  die(stress(randomSizeAligned, alignment, pool), "stress MVT");
  mps_pool_destroy(pool);
  mps_arena_destroy(arena);

  printf("%s: Conclusion: Failed to find any defects.\n", argv[0]);
  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

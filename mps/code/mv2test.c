/* mv2test.c: POOLMVT STRESS TEST
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 */

#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <time.h>

#include "mpm.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpscmvt.h"
#include "mpslib.h"
#include "mpstd.h"
#include "testlib.h"

/* expdev() -- exponentially distributed random deviates
 *
 * From <http://cfatab.harvard.edu/nr/bookcpdf/c7-2.pdf>
 *
 * Returns an exponentially distributed, positive, random deviate of
 * unit mean, using rnd_double() as the source of uniform deviates.
 */

static double expdev(void)
{
  double dum;
  do
    dum=rnd_double();
  while (dum == 0.0);
  return (float)-log(dum);
}


static size_t size_min;
static size_t size_mean;
static size_t size_max;
static int verbose = 0;
static mps_pool_t pool;

static size_t randomSize(unsigned long i)
{
  /* Distribution centered on mean.  Verify that allocations
     below min and above max are handled correctly */
  size_t s = (size_max - size_mean)/4;
  size_t m = size_mean;
  double r;
  double x;

  testlib_unused(i);

  /* per SGR */
  do {
    r = expdev();
    x = (double)s * sqrt(2 * r);
    x += (double)m;
  } while (x <= 1.0);

  return (size_t)x;

}


#define testArenaSIZE   ((size_t)64<<20)
#define TEST_SET_SIZE 1234
#define TEST_LOOPS 27

static mps_res_t make(mps_addr_t *p, mps_ap_t ap, size_t size, mps_align_t align)
{
  mps_res_t res;

  size = alignUp(size, align);

  do {
    MPS_RESERVE_BLOCK(res, *p, ap, size);
    if(res != MPS_RES_OK)
      return res;
  } while(!mps_commit(ap, *p, size));

  return MPS_RES_OK;
}


static mps_res_t stress(mps_arena_t arena, mps_align_t align,
                        size_t (*size)(unsigned long i),
                        mps_pool_class_t pool_class, mps_arg_s args[])
{
  mps_res_t res;
  mps_ap_t ap;
  unsigned long i, k;
  int *ps[TEST_SET_SIZE];
  size_t ss[TEST_SET_SIZE];

  res = mps_pool_create_k(&pool, arena, pool_class, args);
  if (res != MPS_RES_OK)
    return res;

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");

  /* allocate a load of objects */
  for(i=0; i<TEST_SET_SIZE; ++i) {
    mps_addr_t obj;
    ss[i] = (*size)(i);
    res = make(&obj, ap, ss[i], align);
    if (res != MPS_RES_OK) {
      ss[i] = 0;
    } else {
      ps[i]= obj;
      *ps[i] = 1; /* Write something, so it gets swap. */
    }

    if (verbose) {
      if (i && i%4==0)
        putchar('\n');
      printf("%"PRIwWORD PRIXLONGEST" %6"PRIXLONGEST" ",
             (ulongest_t)ps[i], (ulongest_t)ss[i]);
    }
    if (i == 100) {
      PoolDescribe(pool, mps_lib_get_stdout(), 0);
    }
  }
  if (verbose) {
    putchar('\n');
  }

  for (k=0; k<TEST_LOOPS; ++k) {
    unsigned long x = rnd()%(TEST_SET_SIZE-1);
    /* shuffle all the objects */
    for(i=0; i<TEST_SET_SIZE; ++i) {
      unsigned long j = rnd()%(TEST_SET_SIZE-i);
      void *tp;
      size_t ts;

      tp = ps[j]; ts = ss[j];
      ps[j] = ps[i]; ss[j] = ss[i];
      ps[i] = tp; ss[i] = ts;
    }
    /* free some of the objects */

    for(i=x; i<TEST_SET_SIZE; ++i) {
      if (ss[i] > 0) {
        mps_free(pool, (mps_addr_t)ps[i], ss[i]);
        ss[i] = 0;
      }
    }
    /* allocate some new objects */
    for(i=x; i<TEST_SET_SIZE; ++i) {
      mps_addr_t obj;
      size_t s = (*size)(i);
      res = make(&obj, ap, s, align);
      if(res != MPS_RES_OK)
        break;
      ps[i] = obj;
      ss[i] = s;

      if (verbose) {
        if (i && i%4==0)
          putchar('\n');
        printf("%"PRIwWORD PRIXLONGEST" %6"PRIXLONGEST" ",
               (ulongest_t)ps[i], (ulongest_t)ss[i]);
      }
    }
    if (verbose)
      putchar('\n');
  }

  mps_ap_destroy(ap);
  mps_pool_destroy(pool);

  return MPS_RES_OK;
}


static void test_in_arena(mps_arena_class_t arena_class, mps_arg_s *arena_args)
{
  mps_arena_t arena;

  die(mps_arena_create_k(&arena, arena_class, arena_args),
      "mps_arena_create");

  size_min = MPS_PF_ALIGN;
  size_mean = 42;
  size_max = 8192;

  MPS_ARGS_BEGIN(args) {
    mps_align_t align = sizeof(void *) << (rnd() % 4);
    MPS_ARGS_ADD(args, MPS_KEY_ALIGN, align);
    MPS_ARGS_ADD(args, MPS_KEY_MIN_SIZE, size_min);
    MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, size_mean);
    MPS_ARGS_ADD(args, MPS_KEY_MAX_SIZE, size_max);
    MPS_ARGS_ADD(args, MPS_KEY_MVT_RESERVE_DEPTH, TEST_SET_SIZE/2);
    MPS_ARGS_ADD(args, MPS_KEY_MVT_FRAG_LIMIT, 0.3);
    die(stress(arena, align, randomSize, mps_class_mvt(), args), "stress MVT");
  } MPS_ARGS_END(args);

  mps_arena_destroy(arena);
}


int main(int argc, char *argv[])
{
  testlib_init(argc, argv);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, testArenaSIZE);
    test_in_arena(mps_arena_class_vm(), args);
  } MPS_ARGS_END(args);

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, testArenaSIZE);
    MPS_ARGS_ADD(args, MPS_KEY_ARENA_ZONED, FALSE);
    test_in_arena(mps_arena_class_vm(), args);
  } MPS_ARGS_END(args);

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

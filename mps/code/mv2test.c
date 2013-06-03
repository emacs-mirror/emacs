/* mv2test.c: POOLMVT STRESS TEST
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 */

#include <stdio.h>
#include <stdarg.h>
#include "mpstd.h"
#include <time.h>

#include "mpscmvt.h"
#include "mps.h"

typedef mps_word_t mps_count_t;  /* machine word (target dep.) */

#include "mpslib.h"
#include "mpsavm.h"
#include "testlib.h"

#include <math.h>

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


#define max(a, b) (((a) > (b)) ? (a) : (b))

static size_t min;
static size_t mean;
static size_t max;
static int verbose = 0;
static mps_pool_t pool;

static size_t randomSize(unsigned long i)
{
  /* Distribution centered on mean.  Verify that allocations
     below min and above max are handled correctly */
  size_t s = (max - mean)/4;
  size_t m = mean;
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

#define alignUp(w, a) (((w) + (a) - 1) & ~((size_t)(a) - 1))

static mps_res_t make(mps_addr_t *p, mps_ap_t ap, size_t size)
{
  mps_res_t res;

  size = alignUp(size, MPS_PF_ALIGN);
 
  do {
    MPS_RESERVE_BLOCK(res, *p, ap, size);
    if(res != MPS_RES_OK)
      return res;
  } while(!mps_commit(ap, *p, size));

  return MPS_RES_OK;
}


static mps_res_t stress(mps_class_t class, mps_arena_t arena,
                        size_t (*size)(unsigned long i), mps_arg_s args[])
{
  mps_res_t res;
  mps_ap_t ap;
  unsigned long i, k;
  int *ps[TEST_SET_SIZE];
  size_t ss[TEST_SET_SIZE];

  res = mps_pool_create_k(&pool, arena, class, args);
  if(res != MPS_RES_OK) return res;

  die(mps_ap_create(&ap, pool, mps_rank_exact()), "BufferCreate");

  /* allocate a load of objects */
  for(i=0; i<TEST_SET_SIZE; ++i) {
    ss[i] = (*size)(i);

    res = make((mps_addr_t *)&ps[i], ap, ss[i]);
    if(res != MPS_RES_OK)
      ss[i] = 0;
    else
      *ps[i] = 1; /* Write something, so it gets swap. */

    if (verbose) {
      if(i && i%4==0) putchar('\n');
      printf("%"PRIwWORD PRIXLONGEST" %6"PRIXLONGEST" ",
             (ulongest_t)ps[i], (ulongest_t)ss[i]);
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
      size_t s = (*size)(i);
      res = make((mps_addr_t *)&ps[i], ap, s);
      if(res != MPS_RES_OK)
        break;
      ss[i] = s;
     
      if (verbose) {
        if(i && i%4==0) putchar('\n');
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


static void stress_with_arena_class(mps_arena_class_t aclass)
{
  mps_arena_t arena;

  die(mps_arena_create(&arena, aclass, testArenaSIZE),
      "mps_arena_create");

  min = MPS_PF_ALIGN;
  mean = 42;
  max = 8192;

  MPS_ARGS_BEGIN(args) {
    MPS_ARGS_ADD(args, MPS_KEY_MIN_SIZE, min);
    MPS_ARGS_ADD(args, MPS_KEY_MEAN_SIZE, mean);
    MPS_ARGS_ADD(args, MPS_KEY_MAX_SIZE, max);
    MPS_ARGS_ADD(args, MPS_KEY_MVT_RESERVE_DEPTH, TEST_SET_SIZE/2);
    MPS_ARGS_ADD(args, MPS_KEY_MVT_FRAG_LIMIT, 0.3);
    MPS_ARGS_DONE(args);
    die(stress(mps_class_mvt(), arena, randomSize, args), "stress MVT");
  } MPS_ARGS_END(args);

  mps_arena_destroy(arena);

  return;
}


int main(int argc, char *argv[])
{
  randomize(argc, argv);

  stress_with_arena_class(mps_arena_class_vm());
  stress_with_arena_class(mps_arena_class_vmnz());

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

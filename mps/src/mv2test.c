/*  impl.c.poolmv2ss: POOLMV2 STRESS TEST
 *
 * $HopeName: !mv2test.c(trunk.4) $
 * Copyright (C) 1998. Harlequin Group plc. All rights reserved.
 */


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "mpstd.h"
#ifdef MPS_OS_IA
struct itimerspec; /* stop complaints from time.h */
#endif
#include <time.h>

#ifdef MPS_OS_SU
#include "ossu.h"
#endif

#include "mpscmv2.h"

#include "mps.h"

typedef MPS_T_WORD mps_count_t;  /* machine word (target dep.) */

#include "mpslib.h"
#include "mpsavm.h"
#include "testlib.h"

/* --- to get to describe */
#include "mpm.h"

#include <math.h>


/*
 * From <http://cfatab.harvard.edu/nr/bookcpdf/c7-1.pdf>
 *
 * "Minimal" random number generator of Park and Miller with
 * Bays-Durham shuffle and added safeguards. Returns a uniform random
 * deviate between 0.0 and 1.0 (exclusive of the endpoint
 * values). Call with idum a negative integer to initialize;
 * thereafter, do not alter idum between successive deviates in a
 * sequence. RNMX should approximate the largest floating value that
 * is less than 1.
 */
#define IA 16807
#define IM 2147483647
#define AM (1.0F/IM)
#define IQ 127773
#define IR 2836
#define NTAB 32
#define NDIV (1+(IM-1)/NTAB)
#define EPS 1.2e-7F
#define RNMX (1.0F-EPS)
static float ran1(long *idum)
{
  int j;
  long k;
  static long iy=0;
  static long iv[NTAB];
  float temp;
  if (*idum <= 0 || !iy) {      /* Initialize. */
    if (-(*idum) < 1)           /* Be sure to prevent idum = 0. */
      *idum=1;
    else
      *idum = -(*idum);
    for (j=NTAB+7;j>=0;j--) {   /* Load the shuffle table (after 8
                                   warm-ups). */
      k=(*idum)/IQ;
      *idum=IA*(*idum-k*IQ)-IR*k;
      if (*idum < 0)
        *idum += IM;
      if (j < NTAB)
        iv[j] = *idum;
    }
    iy=iv[0];
  }
  k=(*idum)/IQ;                 /* Start here when not initializing. */
  *idum=IA*(*idum-k*IQ)-IR*k;   /* Compute idum=(IA*idum) % IM without
                                   overflows by Schrage's method. */
  if (*idum < 0) *idum += IM;
  j=iy/NDIV;                    /* Will be in the range 0..NTAB-1. */
  iy=iv[j];                     /* Output previously stored value and
                                   refill the shuffle table. */
  iv[j] = *idum;
  if ((temp=AM*(float)iy) > RNMX) /* Because users don't expect endpoint
                                     values. */
    return RNMX;
  else
    return temp;
}


/*
 * From <http://cfatab.harvard.edu/nr/bookcpdf/c7-2.pdf>
 *
 * Returns an exponentially distributed, positive, random deviate of
 * unit mean, using ran1(idum) as the source of uniform deviates.
 */

static float expdev(long *idum)
{
  float dum;
  do
    dum=ran1(idum);
  while (dum == 0.0);
  return (float)-log(dum);
}


#ifdef ndef
/*
 From: Leva, Joseph L., A fast normal random number generator, ACM Transactions on
 Mathematical Software Vol. 18, No. 4 (Dec. 1992), Pages 449-453
*/

static double nrnd(void)
{
  double m = (double)((unsigned)-1);
  double u;
  double v;
  double twor = 1.7156;		/* 2 * sqrt(2.0 / exp(1.0)) */
  double s = 0.449871;
  double t = -0.386595;
  double a = 0.19600;
  double b = 0.25472;
  double r1 = 0.27597;
  double r2 = 0.27846;
  double x, y, Q;

reject:
  u = (double)rnd()/m;
  v = (double)rnd()/m;
  v = twor * (v - 0.5);
  x = u - s;
  y = fabs(v) - t;
  Q = x * x + y * (a * y - b * x);

  if (Q < r1)
    goto accept;
  if (Q > r2)
    goto reject;
  if (v * v > -4 * u * u * log(u))
    goto reject;
accept:
  return v / u;
}
#endif /* ndef */


#define max(a, b) (((a) > (b)) ? (a) : (b))

static size_t min;
static size_t mean;
static size_t max;
static int verbose = 0;
static mps_pool_t pool;


extern void DescribeIt(void);

void DescribeIt(void)
{
  PoolDescribe((Pool)pool, (mps_lib_FILE *)stderr);
}


static size_t randomSize(int i)
{
  /* Distribution centered on mean.  Verify that allocations
     below min and above max are handled correctly */
  static long seed = 7472366;
  size_t s = (max - mean)/4;
  size_t m = mean;
  double r;
  double x;

  testlib_unused(i);

  /* per SGR */
  do {
    r = expdev(&seed);
    x = (double)s * sqrt(2 * r);
    x += (double)m;
  } while (x <= 1.0);

  return (size_t)x;

}


#define testArenaSIZE   ((size_t)64<<20)
#define TEST_SET_SIZE 1234
#define TEST_LOOPS 27

static mps_res_t make(mps_addr_t *p, mps_ap_t ap, size_t size)
{
  mps_res_t res;

  /* --- align */
  size = ((size+7)/8)*8;

  do {
    MPS_RESERVE_BLOCK(res, *p, ap, size);
    if(res != MPS_RES_OK)
      return res;
  } while(!mps_commit(ap, *p, size));

  return MPS_RES_OK;
}


static mps_res_t stress(mps_class_t class, mps_arena_t arena,
                        size_t (*size)(int i), ...)
{
  mps_res_t res;
  mps_ap_t ap;
  va_list arg;
  int i, k;
  int *ps[TEST_SET_SIZE];
  size_t ss[TEST_SET_SIZE];

  va_start(arg, size);
  res = mps_pool_create_v(&pool, arena, class, arg);
  va_end(arg);
  if(res != MPS_RES_OK) return res;

  die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "BufferCreate");

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
      printf("%8lX %6lX ", (unsigned long)ps[i], (unsigned long)ss[i]);
    }
  }
  if (verbose) {
    putchar('\n');
  }

  for (k=0; k<TEST_LOOPS; ++k) {
    int x = rand()%(TEST_SET_SIZE-1);
    /* shuffle all the objects */
    for(i=0; i<TEST_SET_SIZE; ++i) {
      int j = rand()%(TEST_SET_SIZE-i);
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
        printf("%8lX %6lX ", (unsigned long)ps[i], (unsigned long)ss[i]);
      }
    }
    if (verbose)
      putchar('\n');
  }

  PoolDescribe((Pool)pool, mps_lib_stdout);

  mps_ap_destroy(ap);
  mps_pool_destroy(pool);

  return MPS_RES_OK;
}


static void stress_with_arena_class(mps_arena_class_t aclass)
{
  mps_arena_t arena;

  die(mps_arena_create(&arena, aclass, testArenaSIZE),
      "mps_arena_create");

  min = 8;
  mean = 42;
  max = 8192;

  die(stress(mps_class_mv2(), arena, randomSize,
             min,               /* min_size */
             mean,              /* median_size */
             max,               /* maximum_size */
             (mps_count_t)TEST_SET_SIZE/2, /* reserve_depth */
             30                 /* fragmentation_limit */
             ),
      "stress MV2");

  mps_arena_destroy(arena);

  return;
}


int main(void)
{
  stress_with_arena_class(mps_arena_class_vm());
  stress_with_arena_class(mps_arena_class_vmnz());
  /* mps_arena_class_cl? */

  return 0;
}

/* impl.c.testlib: TEST LIBRARY
 *
 * $HopeName: MMsrc!testlib.c(trunk.17) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * .purpose: A library of functions that may be of use to unit tests.
 */

#include "testlib.h"
#include "mps.h"
#include "mpm.h"
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#ifdef MPS_OS_IA
struct itimerspec; /* stop complaints from time.h */
#endif
#include <time.h>


/* rnd -- a random number generator
 *
 * I nabbed it from "ML for the Working Programmer"
 * Originally from:
 * Stephen K Park & Keith W Miller (1988). Random number generators:
 * good one are to find.  Communications of the ACM, 31:1192-1201
 */

unsigned long rnd(void)
{
  static unsigned long seed = 1;
  double s;
  s = seed;
  s *= 16807.0;
  s = fmod(s, 2147483647.0);  /* 2^31 - 1 */
  seed = (unsigned long)s;
  return seed;
}


/* randomize -- randomize the generator, or initialize to replay */

void randomize(int argc, char **argv)
{
  int i, k, n;

  if(argc > 1) {
    n = sscanf(argv[1], "%d", &k);
    die((n == 1) ? MPS_RES_OK : MPS_RES_FAIL, "randomize");
  } else {
    k = time(NULL) % 32000;
    printf("Randomizing %d times.\n", k);
  }

  /* Randomize the random number generator a bit. */
  for (i = k; i > 0; --i)
    rnd();
}


/* die -- Test a return code, and exit on error */

void die(mps_res_t res, const char *s)
{
  if(res != MPS_RES_OK)
  {
    fflush(stdout); /* synchronize */
    fprintf(stderr, "\n%s: %d\n", s, res);
    exit(1);
  }
}


/* die_expect -- Test a return code, and exit on unexpected result */

void die_expect(mps_res_t res, mps_res_t expected, const char *s)
{
  if(res != expected)
  {
    fflush(stdout); /* synchronize */
    fprintf(stderr, "\n%s: %d\n", s, res);
    exit(1);
  }
}


/* adjust_collection_freq -- multiply all collection frequencies by
 *                           a given factor
 *
 * If sizes are adjusted too low, they are corrected so that all are
 * non-zero and noticeably larger than the ones for lower generations.
 */

#define multSIZE(size, mult) ((size) = (unsigned long)((size) * (mult)))
#define sizeLIMIT 63uL

void adjust_collection_freq(double multiplier)
{
  multSIZE(TraceGen0Size, multiplier);
  if(TraceGen0Size < sizeLIMIT)
    TraceGen0Size = sizeLIMIT;
  multSIZE(TraceGen1Size, multiplier);
  if(TraceGen1Size <= sizeLIMIT)
    TraceGen1Size = sizeLIMIT;
  multSIZE(TraceGen2Size, multiplier);
  if(TraceGen2Size <= sizeLIMIT)
    TraceGen2Size = sizeLIMIT;
  multSIZE(TraceGen0RampmodeSize, multiplier);
  if(TraceGen0RampmodeSize < sizeLIMIT)
    TraceGen0RampmodeSize = sizeLIMIT;
  multSIZE(TraceGen1RampmodeSize, multiplier);
  if(TraceGen1RampmodeSize <= sizeLIMIT)
    TraceGen1RampmodeSize = sizeLIMIT;
  multSIZE(TraceRampGenSize, multiplier);
  if(TraceRampGenSize <= sizeLIMIT)
    TraceRampGenSize = sizeLIMIT;
  multSIZE(TraceGen2RampmodeSize, multiplier);
  if(TraceGen2RampmodeSize <= sizeLIMIT)
    TraceGen2RampmodeSize = sizeLIMIT;
}

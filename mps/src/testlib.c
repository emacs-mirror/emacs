/* impl.c.testlib: TEST LIBRARY
 *
 * $HopeName: MMsrc!testlib.c(trunk.22) $
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * .purpose: A library of functions that may be of use to unit tests.
 */

#include "testlib.h"
#include "mps.h"
#include "mpm.h"
#include <math.h>
#include <stdlib.h>
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


/* verror -- die with message */

void verror(const char *format, va_list args)
{
  fflush(stdout); /* synchronize */
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
  exit(1);
}


/* error -- die with message */

void error(const char *format, ...)
{
 va_list args;

 va_start(args, format);
 verror(format, args);
 va_end(args);
}


/* die -- Test a return code, and exit on error */

void die(mps_res_t res, const char *s)
{
  if (res != MPS_RES_OK) {
    error("\n%s: %d\n", s, res);
  }
}


/* die_expect -- Test a return code, and exit on unexpected result */

void die_expect(mps_res_t res, mps_res_t expected, const char *s)
{
  if (res != expected) {
    error("\n%s: %d\n", s, res);
  }
}


/* cdie -- Test a C boolean, and exit on error */

void cdie(int res, const char *s)
{
  if (!res) {
    error("\n%s: %d\n", s, res);
  }
}

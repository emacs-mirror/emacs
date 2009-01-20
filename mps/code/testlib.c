/* testlib.c: TEST LIBRARY
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .purpose: A library of functions that may be of use to unit tests.
 */

#include "testlib.h"
#include "mps.h"
#include "misc.h" /* for NOOP */
#include <math.h>
#include <stdlib.h>
#include <limits.h>
#ifdef MPS_OS_IA
struct itimerspec; /* stop complaints from time.h */
#endif
#include <time.h>

#ifdef MPS_BUILD_MV
/* MSVC warning 4702 = unreachable code
 * 
 * job000605: believed needed to prevent VC7 warning 
 * for error() below, in which va_end is mandated by 
 * ISO C (C99:7.15.1) even though it is unreachable.
 */
#pragma warning(disable: 4702)
/* MSVC warning 4996 = stdio / C runtime 'unsafe' */
/* Objects to: sscanf.  See job001934. */
#pragma warning( disable : 4996 )
#endif


/* rnd -- a random number generator
 *
 * I nabbed it from "ML for the Working Programmer", originally from:
 * Stephen K Park & Keith W Miller (1988). Random number generators:
 * good ones are hard to find.  Communications of the ACM, 31:1192-1201.
 *
 * This is a 'full-period' generator: all values from [1..(mod-1)] 
 * are returned once, and then the generator cycles round again.
 * (The value 0 is not part of the cycle and is never returned). That 
 * means the period = mod-1, ie. 2147483646.
 *
 * This "x 16807 mod 2^31-1" generator has been very well studied.  
 * It is not the 'best' (most random) simple generator, but it is 
 * free of all vices we might care about for this application, so 
 * we'll keep it simple and stick with this, thanks.
 *
 * There are faster implementations of this generator, summarised 
 * briefly here:
 *  - L. Schrage (1979 & 1983) showed how to do all the calculations 
 *    within 32-bit integers.  See also Numerical recipes in C.
 *  - David Carta (1990) showed how to also eliminate division.  
 */

static unsigned long seed = 1;
/* 2^31 - 1 == (1UL<<31) - 1 == 2147483647 */
#define RND_MOD_2_31_1       2147483647
#define RND_MOD_2_31_1_FLOAT 2147483647.0
unsigned long rnd(void)
{
  double s;

  s = seed;
  s *= 16807.0;
  s = fmod(s, RND_MOD_2_31_1_FLOAT);  /* 2^31 - 1 */
  seed = (unsigned long)s;
  return seed;
  /* Have you modified this code?  Run rnd_verify() please!  RHSK */
}

/* rnd_verify -- verify that rnd() returns the correct results */
static void rnd_verify(void)
{
  unsigned long orig_seed = seed;
  unsigned long i;

  /* This test is from:
   * Stephen K Park & Keith W Miller (1988). Random number generators:
   * good ones are hard to find.  Communications of the ACM, 31:1192-1201.
   * Note: The Park-Miller paper uses 1-based indices.
   */
  i = 1;
  seed = 1;
  for(i = 2; i < 10001; i += 1) {
    (void)rnd();
  }
  Insist(i == 10001);
  Insist(rnd() == 1043618065);

  /* from Robin Whittle's compendium at 
   * http://www.firstpr.com.au/dsp/rand31/
   * Observe wrap-around after 'full-period' (which excludes 0).
   * Note: uses 0-based indices, ie. rnd_0 = 1, rnd_1 = 16807, ...
   */
  i = 2147483645;
  seed = 1407677000;
  i += 1;
  Insist(i == (1UL<<31) - 2 ); /* 'full-period' excludes 0 */
  Insist(rnd() == 1);  /* wrap-around */

  seed = orig_seed;
}

/* rnd_addr -- a random address generator
 *
 * rnd gives 31 random bits, we run it repeatedly to get enough bits.
 */

#define ADDR_BITS (sizeof(mps_addr_t) * CHAR_BIT)

mps_addr_t rnd_addr(void)
{
  mps_word_t res;
  unsigned bits;

  for (bits = 0, res = 0; bits < ADDR_BITS;
       bits += 31, res = res << 31 | (mps_word_t)rnd())
    NOOP;
  return (mps_addr_t)res;
}


/* randomize -- randomize the generator, or initialize to replay */

void randomize(int argc, char **argv)
{
  int n;
  unsigned long seed0;
  int i;

  if (argc > 1) {
    n = sscanf(argv[1], "%lu", &seed0);
    Insist(n == 1);
    printf("randomize(): resetting initial seed to: %lu.\n", seed0);
    rnd_verify();  /* good to verify occasionally: slip it in here */
  } else {
    /* time_t uses an arbitrary encoding, but hopefully the low order */
    /* 31 bits will have at least one bit changed from run to run. */
    seed0 = 1 + time(NULL) % (RND_MOD_2_31_1 - 1);
    printf("randomize(): choosing initial seed: %lu.\n", seed0);
  }

  Insist(seed0 < RND_MOD_2_31_1);  /* 2^31 - 1 */
  Insist(seed0 != 0);
  seed = seed0;

  /* The 'random' seed is taken from time(), which may simply be a 
   * count of seconds: therefore successive runs may start with 
   * nearby seeds, possibly differing only by 1.  So the first value
   * returned by rnd() may differ by only 16807.  It is conceivable 
   * that some tests might be able to 'spot' this pattern (for 
   * example: by using the first rnd() value, mod 100M and rounded 
   * to multiple of 128K, as arena size).
   * 
   * So to mix it up a bit, we do a few iterations now.  How many?  
   * Very roughly, 16807^2 is of the same order as 2^31, so two 
   * iterations would make the characteristic difference similar to 
   * the period.  Hey, let's go wild and do 10.
   */
  for(i = 0; i < 10; i += 1) {
    (void)rnd();
  }
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


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002, 2008 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

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
 * We use the (Multiplicative) Linear Congruential Generator
 *   Xn = a * Xn-1 mod m
 * with: m = 2147483647 (2^31 - 1, a Mersenne prime), and a = 48271.  
 * This is a 'full-period' generator: all values from [1..(mod-1)], 
 * or 0x00000001 to 0x7ffffffe inclusive, are returned once, and then 
 * the cycle begins again.  (The value 0 is not part of the cycle and 
 * is never returned).  So the period = mod-1, ie. 2147483646.
 *
 * This generator is extremely simple and has been very well studied.  
 * It is free of major vices we might care about for this application.
 * In particular, as m is prime, low order bits are random.  Therefore 
 * to roll an N-sided die (N << m), "rnd() % N" is acceptable, giving 
 * a value in [0..N-1].
 *
 * It was popularised by the much-cited Park & Miller paper:
 *   Stephen K Park & Keith W Miller (1988). Random number generators:
 *   good ones are hard to find.  Communications of the ACM, 
 *   31:1192-1201.
 * The recommended multiplier a was later updated from 16807 to 48271:
 *   Stephen K Park, Keith W Miller, Paul K. Stockmeyer (1993). 
 *   Technical Correspondence.  Communications of the ACM, 36:105-110.
 *
 * (Many more elaborate generators have been invented.  The next simple 
 * step would be to combine with the MLCG m = 2147483399 a = 40692, to 
 * make the period "about 74 quadrillion".  See the summary of chapter 
 * 3 in Knuth's "The Art of Computer Programming".)
 *
 * This (fast) implementation uses the identity:
 *   0x80000000 == 0x7FFFFFFF + 0x00000001
 * noted by David Carta (1990), where 0x7FFFFFFF == 2^31-1 == m, which 
 * means that bits above the first 31 can simply be shifted >> 31 and 
 * added, preserving Xn mod m.  To remain within 32-bit unsigned 
 * arithmetic when multiplying the previous seed (31 bits) by a (16 
 * bits), the seed is split into bottom and top halves; bits above 
 * the first 31 are simply "top >> 16".  (Code by RHSK, inspired by 
 * Robin Whittle's article at "http://www.firstpr.com.au/dsp/rand31/").
 *
 * Slower implementations, used for verification:
 * rnd_verify_schrage uses the method of L. Schrage (1979 & 1983), 
 *   namely splitting the seed by q, where q = m div a.
 * rnd_verify_float simply uses floating point arithmetic.
 */

static unsigned long seed = 1;
#define R_m 2147483647UL
#define R_a 48271UL
unsigned long rnd(void)
{
  /* requires m == 2^31-1, a < 2^16 */
  unsigned long bot = R_a * (seed & 0x7FFF);
  unsigned long top = R_a * (seed >> 15);
  seed = bot + ((top & 0xFFFF) << 15) + (top >> 16);
  if(seed > R_m)
    seed -= R_m;
  return seed;
  /* Have you modified this code?  Run rnd_verify(3) please!  RHSK */
}

static unsigned long seed_verify_schrage = 1;
#define R_q (R_m / R_a)
#define R_r (R_m % R_a)
static unsigned long rnd_verify_schrage(void)
{
  /* requires m < 2^31, q > r; see Park & Miller (1988) */
  unsigned long alpha = R_a * (seed_verify_schrage % R_q);  /* < m */
  unsigned long beta = R_r * (seed_verify_schrage / R_q);  /* < m */
  seed_verify_schrage = alpha - beta;
  if(alpha < beta)
    seed_verify_schrage += R_m;
  return seed_verify_schrage;
}

static unsigned long seed_verify_float = 1;
#define R_m_float 2147483647.0
#define R_a_float 48271.0
static unsigned long rnd_verify_float(void)
{
  double s;
  s = seed_verify_float;
  s *= R_a_float;
  s = fmod(s, R_m_float);
  seed_verify_float = (unsigned long)s;
  return seed_verify_float;
}

/* rnd_verify -- verify that rnd() returns the correct results
 *
 * depth = how much time to spend verifying
 * 0: very quick -- just verify the next rnd() value
 * 1: quick -- verify the first 10000 calls from seed = 1
 * 2: slow (< 5 minutes) -- run the fast generator for a full cycle
 * 3: very slow (several hours) -- verify a full cycle
 */
void rnd_verify(int depth)
{
  unsigned long orig_seed = seed;
  unsigned long i;
  unsigned long r;
  
  /* 0: the next value from rnd() matches rnd_verify_*() */
  if(depth >= 0) {
    seed_verify_schrage = seed;
    seed_verify_float = seed;
    r = rnd();
    Insist(rnd_verify_schrage() == r);
    Insist(rnd_verify_float() == r);
  }

  /* 1: first 10000 (from Park & Miller, note: 1-based indexing!) */
  if(depth >= 1) {
    i = 1;
    seed = 1;
    seed_verify_schrage = 1;
    seed_verify_float = 1;
    for(i = 2; i <= 10001; i += 1) {
      r = rnd();
      Insist(rnd_verify_schrage() == r);
      Insist(rnd_verify_float() == r);
    }
    printf("%lu\n", r);
    /* Insist(r == 1043618065UL); -- correct value for a = 16807 */
    Insist(r == 399268537UL);  /* correct for a = 48271 */
  }

  /* 1: observe wrap-around (note: 0-based indexing) */
  if(depth >= 1) {
    i = 2147483645UL;
    /* seed = 1407677000UL; -- correct value for a = 16807 */
    seed = 1899818559UL;  /* correct for a = 48271 */
    seed_verify_schrage = 1899818559UL;
    seed_verify_float = 1899818559UL;
    i += 1;
    Insist(i == (1UL<<31) - 2 ); /* 'full-period' excludes 0 */
    Insist(rnd() == 1);  /* wrap-around */
    Insist(rnd_verify_schrage() == 1);
    Insist(rnd_verify_float() == 1);
  }

  /* 2 & 3: Full cycle (3 => verifying each value) */
  if(depth >= 2) {
    unsigned long r1;
    i = 0;
    seed = 1;
    seed_verify_schrage = 1;
    seed_verify_float = 1;
    while(1) {
      i += 1;
      r = rnd();
      if(depth >= 3) {
        Insist(rnd_verify_schrage() == r);
        Insist(rnd_verify_float() == r);
      }
      if(r == 1) {
        printf("Wrap at i=%lu, r=%lu, r(i-1)=%lu.\n",
               i, r, r1);
        break;
      } else {
        r1 = r;
      }
    }
  }

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
  } else {
    /* time_t uses an arbitrary encoding, but hopefully the low order */
    /* 31 bits will have at least one bit changed from run to run. */
    seed0 = 1 + time(NULL) % (R_m - 1);
    printf("randomize(): choosing initial seed: %lu.\n", seed0);
  }

  Insist(seed0 < R_m);
  Insist(seed0 != 0);
  seed = seed0;

  rnd_verify(0);
  Insist(seed == seed0);

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

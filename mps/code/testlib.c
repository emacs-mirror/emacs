/* testlib.c: TEST LIBRARY
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (C) 2002 Global Graphics Software.
 *
 * .purpose: A library of functions that may be of use to unit tests.
 */

#include "testlib.h"
#include "clock.h" /* for EVENT_CLOCK */
#include "mps.h"
#include "misc.h" /* for NOOP */

#include <math.h> /* fmod, log, HUGE_VAL */
#include <stdio.h> /* fflush, printf, stderr, sscanf, vfprintf */
#include <stdlib.h> /* abort, exit, getenv */
#include <time.h> /* time */


/* fail -- like assert, but (notionally) returns a value, so usable in an expression */

int fail(void)
{
  Insist(FALSE);
  return 1111UL;
}


/* rnd -- a random number generator
 *
 * We use the (Multiplicative) Linear Congruential Generator
 *   Xn = a * Xn-1 mod m
 * with: m = 2147483647 (2^31 - 1, a Mersenne prime), and a = 48271.  
 * This is a 'full-period' generator: all values in [1..(mod-1)] 
 * (ie. 0x00000001 to 0x7ffffffe inclusive) are returned once, and then 
 * the cycle begins again.  The value 0 is not part of the cycle and 
 * is never returned.  So the period = mod-1, ie. 2147483646.
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
  s = (double)seed_verify_float;
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
 * 2: slow (~ 1 minute) -- run the fast generator for a full cycle
 * 3: very slow (several minutes) -- verify a full cycle
 */
void rnd_verify(int depth)
{
  unsigned long orig_seed = seed;
  unsigned long i;
  unsigned long r = 0;
  
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
    seed_verify_schrage = seed;
    seed_verify_float = seed;
    for(i = 2; i <= 10001; i += 1) {
      r = rnd();
      Insist(rnd_verify_schrage() == r);
      Insist(rnd_verify_float() == r);
    }
    /* Insist(r == 1043618065UL); -- correct value for a = 16807 */
    Insist(r == 399268537UL);  /* correct for a = 48271 */
  }

  /* 1: observe wrap-around (note: 0-based indexing) */
  if(depth >= 1) {
    /* set-up seed value for i = 2147483645 */
    /* seed = 1407677000UL; -- correct value for a = 16807 */
    seed = 1899818559UL;  /* correct for a = 48271 */
    seed_verify_schrage = seed;
    seed_verify_float = seed;
    r = rnd();
    Insist(rnd_verify_schrage() == r);
    Insist(rnd_verify_float() == r);
    Insist(r == 1);  /* wrap-around */
  }

  /* 2 & 3: Full cycle (3 => verifying each value) */
  if(depth >= 2) {
    int verify = (depth >= 3);
    unsigned long r1 = 1;
    
    i = 0;
    seed = 1;
    seed_verify_schrage = seed;
    seed_verify_float = seed;
    while(1) {
      i += 1;
      r = rnd();
      if(verify) {
        Insist(rnd_verify_schrage() == r);
        Insist(rnd_verify_float() == r);
      }
      if(r == 1) {
        printf("Full cycle complete%s:\n",
               verify ? " (verifying every value)"
                      : " (fast implementation only)" );
        printf("Wrapped at i=%lu, r=%lu, r(i-1)=%lu.\n",
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

double rnd_double(void)
{
  return rnd() / R_m_float;
}

static unsigned sizelog2(size_t size)
{
  return (unsigned)(log((double)size) / log(2.0));
}

size_t rnd_grain(size_t arena_size)
{
  /* The grain size must be small enough to allow for a complete set
     of zones in the initial chunk, but bigger than one word. */
  Insist(arena_size >> MPS_WORD_SHIFT >= sizeof(void *));
  return rnd_align(sizeof(void *), (size_t)1 << sizelog2(arena_size >> MPS_WORD_SHIFT));
}

size_t rnd_align(size_t min, size_t max)
{
  unsigned log2min = sizelog2(min);
  unsigned log2max = sizelog2(max);
  Insist(min <= max);
  Insist((size_t)1 << log2min == min);
  Insist((size_t)1 << log2max == max);
  if (log2min < log2max)
    return min << (rnd() % (log2max - log2min + 1));
  else
    return min;
}

double rnd_pause_time(void)
{
  double t = rnd_double();
  if (t == 0.0)
    return HUGE_VAL; /* Would prefer to use INFINITY but it's not in C89. */
  else
    return 1 / t - 1;
}

rnd_state_t rnd_seed(void)
{
  /* Initialize seed based on seconds since epoch and on processor
   * cycle count. */
  EventClock t2;
  EVENT_CLOCK(t2);
  return 1 + ((unsigned long)time(NULL) + (unsigned long)t2) % (R_m - 1);
}


/* randomize -- randomize the generator, or initialize to replay
 *
 * There have been 3 versions of the rnd-states reported by this 
 * function:
 *
 * 1. before RHSK got his hands on rnd(), ie. pre-2008.  These seed 
 *    values are not currently supported, but it might be easy to 
 *    add support.
 *
 * 2. v2 states: the published "seed" (state) value was the seed 
 *    *before* the 10 rnds to churn up and separate nearby values 
 *    from time().  This was unfortunate: you can't write a rnd_state 
 *    getter, because it would have to go 10 steps backwards, and 
 *    that's impossible.
 *    (2008..2010-03-22)
 *
 * 3. v3 states: the published state is the state *after* all
 *    initialization is complete. Therefore you can easily store and
 *    re-use the published state. (From 2010-03-22, changelist
 *    170093).
 */

void randomize(int argc, char *argv[])
{
  int n;
  unsigned long seed0;

  if (argc > 1) {
    n = sscanf(argv[1], "%lu", &seed0);
    Insist(n == 1);
    printf("%s: randomize(): resetting initial state (v3) to: %lu.\n", 
           argv[0], seed0);
    rnd_state_set(seed0);
  } else {
    seed0 = rnd_seed();
    printf("%s: randomize(): choosing initial state (v3): %lu.\n",
           argv[0], seed0);
    rnd_state_set(seed0);
  }
  (void)fflush(stdout); /* ensure seed is not lost in case of failure */
}

unsigned long rnd_state(void)
{
  return seed;
}

void rnd_state_set(unsigned long seed0)
{
  Insist(seed0 < R_m);
  Insist(seed0 != 0);
  seed = seed0;

  rnd_verify(0);
  Insist(seed == seed0);
}

/* rnd_state_set_2 -- legacy support for v2 rnd states
 *
 * In v2, the published "seed" (state) value was the seed *before* 
 * the 10 rnds to churn up and separate nearby values from time().
 *
 * Set the seed, then convert it to a v3 state by doing those 10 rnds.
 */
void rnd_state_set_v2(unsigned long seed0_v2)
{
  int i;
  unsigned long seed0;

  rnd_state_set(seed0_v2);
  for(i = 0; i < 10; i += 1) {
    (void)rnd();
  }

  seed0 = rnd_state();
  printf("rnd_state_set_v2(): seed0_v2 = %lu, converted to state_v3 = %lu.\n", seed0_v2, seed0);
  rnd_state_set(seed0);
}


/* res_strings -- human readable MPS result codes */

static struct {
  const char *ident;
  const char *doc;
} const res_strings[] = {
#define RES_STRINGS_ROW(X, ident, doc) {#ident, doc},
  _mps_RES_ENUM(RES_STRINGS_ROW, X)
};


/* verror -- die with message */

ATTRIBUTE_FORMAT((printf, 1, 0))
void verror(const char *format, va_list args)
{
  (void)fflush(stdout); /* synchronize */
  (void)vfprintf(stderr, format, args);
  (void)fprintf(stderr, "\n");
  (void)fflush(stderr); /* make sure the message is output */
  mps_telemetry_flush();
  /* On Windows, the abort signal pops up a dialog box. This suspends
   * the test suite until a button is pressed, which is not acceptable
   * for offline testing, so if the MPS_TESTLIB_NOABORT environment
   * variable is set, then the test case exits instead of aborting.
   */
  if (getenv("MPS_TESTLIB_NOABORT")) {
    exit(EXIT_FAILURE);
  } else {
    abort();
  }
}


/* error -- die with message */

ATTRIBUTE_FORMAT((printf, 1, 2))
void error(const char *format, ...)
{
 va_list args;

 va_start(args, format);
 verror(format, args);
 /* va_end(args); */ /* provokes "unreachable code" error from MSVC */
}


/* die_expect -- Test a return code, and exit on unexpected result */

void die_expect(mps_res_t res, mps_res_t expected, const char *s)
{
  if (res != expected) {
    if (0 <= res && (unsigned)res < NELEMS(res_strings))
      error("\n%s: %s: %s\n", s, res_strings[res].ident, res_strings[res].doc);
    else
      error("\n%s: %d: unknown result code\n", s, res);
  }
}


/* die -- Test a return code, and exit on error */

void die(mps_res_t res, const char *s)
{
  die_expect(res, MPS_RES_OK, s);
}


/* cdie -- Test a C boolean, and exit on error */

void cdie(int res, const char *s)
{
  if (!res) {
    error("\n%s: %d\n", s, res);
  }
}


/* assert_die -- always die on assertion */

void assert_die(const char *file, unsigned line, const char *condition)
{
  error("%s:%u: MPS ASSERTION FAILED: %s\n", file, line, condition);
}


/* testlib_init -- install assertion handler and seed RNG */

void testlib_init(int argc, char *argv[])
{
  (void)mps_lib_assert_fail_install(assert_die);
  randomize(argc, argv);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

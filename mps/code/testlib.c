/* testlib.c: TEST LIBRARY
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
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


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

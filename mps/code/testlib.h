/* testlib.h: TEST LIBRARY INTERFACE
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: A library of functions that may be of use to unit tests.
 */

#ifndef testlib_h
#define testlib_h

#include "mps.h"
#include "misc.h" /* for STR */

/* Include system header hackery. */
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif
#ifdef MPS_OS_XC
#include "osxc.h"
#endif

#include <stdio.h>


/* Suppress Visual C warnings at warning level 4, */
/* see mail.richard.1997-09-25.13-26. */
/* Essentially the same settings are done in config.h. */

#ifdef MPS_BUILD_MV

/* "unreferenced inline function has been removed" (windows.h) */
#pragma warning(disable: 4514)

/* "constant conditional" (MPS_END) */
#pragma warning(disable: 4127)

/* MSVC 2.0 generates a warning when using NOCHECK or UNUSED */
#ifdef _MSC_VER
#if _MSC_VER < 1000
#pragma warning(disable: 4705)
#endif
#else /* _MSC_VER */
#error "Expected _MSC_VER to be defined for builder.mv"
#endif /* _MSC_VER */


/* MSVC 10.00 on PowerPC generates erroneous warnings about */
/* uninitialized local variables, if you take their address. */
#ifdef MPS_ARCH_PP
#pragma warning(disable: 4701)
#endif

/* In white-hot versions, absolutely no checking is done.  This leads to
 * many spurious warnings because parameters are suddenly unused, etc.
 * We aren't interested in these.
 */

#if defined(CONFIG_VAR_WI)

/* "unreferenced formal parameter" */
#pragma warning(disable: 4100)

/* "unreferenced local function has been removed" */
#pragma warning(disable: 4505)

#endif


#endif /* MPS_BUILD_MV */


/* testlib_unused -- declares that a variable is unused
 *
 * It should be used to prevent compiler warnings about unused
 * variables.  Care should be exercised; the fact that a variable
 * is unused may need justification.
 */

#define testlib_unused(v) ((void)(v))


/* die -- succeed or die
 *
 * If the first argument is not ResOK then prints the second
 * argument on stderr and exits the program.  Otherwise does nothing.
 *
 * Typical use:
 *   die(mps_ap_create(&ap, pool, MPS_RANK_EXACT), "APCreate");
 */

extern void die(mps_res_t res, const char *s);


/* die_expect -- get expected result or die
 *
 * If the first argument is not  thename as the second argument,
 * prints the third argument on stderr and exits the program. 
 * Otherwise does nothing.
 *
 * Typical use:
 *   die_expect(res, MPS_RES_COMMIT_LIMIT, "Commit limit allocation");
 */

extern void die_expect(mps_res_t res, mps_res_t expected, const char *s);


/* cdie -- succeed or die
 *
 * If the first argument is not true (non-zero) then prints the second
 * argument on stderr and exits the program.  Otherwise does nothing.
 *
 * Typical use:
 *   cdie(foo != NULL, "No foo");
 */

extern void cdie(int res, const char *s);


/* error, verror -- die with message */

extern void error(const char *format, ...);
extern void verror(const char *format, va_list args);


/* Insist -- like assert, but even in release varieties */

#define Insist(cond) insist1(cond, #cond)

#define insist1(cond, condstring) \
  cdie(cond, condstring "\n" __FILE__ "\n" STR(__LINE__))


/* rnd -- random number generator
 *
 * rnd() generates a sequence of integers in the range [0, 2^31-2].
 */

extern unsigned long rnd(void);


/* randomize -- randomize the generator, or initialize to replay
 *
 * randomize(argc, argv) randomizes the rnd generator (using time(3))
 * and prints out the randomization seed, or takes a seed (as a command-
 * line argument) and initializes the generator to the same state.
 */

extern void randomize(int argc, char **argv);


#endif /* testlib_h */


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

/* impl.h.testlib: TEST LIBRARY INTERFACE
 *
 * $HopeName$
 * Copyright (C) 1995, 1998, 1999 Harlequin Group plc.  All rights reserved.
 *
 * .purpose: A library of functions that may be of use to unit tests.
 */

#ifndef testlib_h
#define testlib_h

#include "mps.h"

#include <stdio.h>
/* On SunOS, need ossu.h as well */
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


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
 *   die(mps_space_create(&space), "SpaceCreate");
 */

extern void die(mps_res_t res, const char *s);


/* die_expect -- get expected result or die
 *
 * If the first argument is not  thename as the second argument,
 * prints the third argument on stderr and exits the program.  
 * Otherwise does nothing.
 *
 * Typical use:
 *   die_expect(mps_space_create(&space), MPS_RES_OK, "SpaceCreate");
 */

extern void die_expect(mps_res_t res, mps_res_t expected, const char *s);


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


/* adjust_collection_freq -- multiply all collection frequencies by
 *                           a given factor
 */

extern void adjust_collection_freq(double multiplier);


#endif /* testlib_h */

/*  ==== TEST LIBRARY ====
 *
 *  $HopeName: MMsrc!testlib.h(trunk.6) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a library of functions that unit test developers might find
 *  useful.  We hope they enhance your programming pleasure.
 *
 *  Notes
 *   1. There is no way to set the seed for rnd.
 *    1995-03-14 drj
 */

#ifndef testlib_h
#define testlib_h

#include "mps.h"

/*  == MISC ==  */

/*  == UNUSED ==
 *
 *  The testlib_unused macro declares that a variable is unused.
 *  It should be used to prevent compiler warnings about unused
 *  variables.  Care should be exercised; the fact that a variable
 *  is unused may need justification.
 */

#define testlib_unused(v) ((void)(v))

/* MSVC 2.0 generates a warning when using testlib_unused */
#ifdef MPS_BUILD_MV
#ifdef _MSC_VER
#if _MSC_VER < 1000
#pragma warning(disable: 4705)
#endif /* _MSC_VER < 1000 */
#else /* _MSC_VER */
#error "Expected _MSC_VER to be defined for builder.mv"
#endif /* _MSC_VER */
#endif /* MPS_BUILD_MV */


/*  == SUCCEED OR DIE ==
 *
 *  If the first argument is not ResOK then prints the second
 *  argument on stderr and exits the program.  Otherwise does nothing.
 *
 *  Typical use:
 *   die(mps_space_create(&space), "SpaceCreate");
 */

extern void die(mps_res_t res, const char *s);

/*  == RANDOM NUMBER GENERATOR ==
 *
 *  rnd() generates a sequence of integers in the range [0, 2^31-2]
 */

extern unsigned long rnd(void);

#endif /* testlib_h */

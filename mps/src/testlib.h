/*  ==== TEST LIBRARY ====
 *
 *  $Id: testlib.h,v 1.1.5.2 1996/03/12 13:04:10 richard Exp $
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

#include "std.h"

/*  == SUCCEED OR DIE ==
 *
 *  If the first argument is not ResOK then prints the second
 *  argument on stderr and exits the program.  Otherwise does nothing.
 *
 *  Typical use:
 *   die(SpaceInit(space), "SpaceInit");
 */

extern void die(Res res, const char *s);

/*  == RANDOM NUMBER GENERATOR ==
 *
 *  rnd() generates a sequence of integers in the range [0, 2^31-2]
 */

extern unsigned long rnd(void);

#endif /* testlib_h */

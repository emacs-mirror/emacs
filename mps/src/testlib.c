/*  ==== TEST LIBRARY ====
 *
 *  $HopeName$
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  This is a library of functions that may be of use to unit tests.
 *
 *  Notes
 */

#include "std.h"
#include "testlib.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>


/* I nabbed it from "ML for the Working Programmer"
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

void die(Res res, const char *s)
{
  if(res != ResOK)
  {
    fprintf(stderr, "%s: %d\n", s, res);
    exit(1);
  }
}


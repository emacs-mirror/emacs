/* impl.c.version: VERSION INSPECTION
 *
 * $HopeName$
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * PURPOSE
 *
 * The purpose of this module is to provide a means by which the
 * version of the MM library being used can be determined.
 *
 * DESIGN
 *
 * .design: See design.mps.version-library, but to let you in on a
 * secret it works by declaring a string with all the necessary info
 * in.
 */

#include "mpm.h"


SRCID(version, "$HopeName$");


/* MPS_RELEASE -- the release name
 *
 * .release: When making a new release, change the expansion of
 * MPS_RELEASE to be a string of the form "release.dylan.crow.2" or
 * whatever.
 */

#define MPS_RELEASE "$HopeName$ *** DEVELOPMENT ONLY ***"


/* MPSCopyrightNotice -- copyright notice for the binary
 *
 * .copyright.year: This one should have the current year in it
 * (assuming we've made any substantial changes to the library this year).
 */

char MPSCopyrightNotice[] =
  "Copyright (C) 2000 Harlequin Limited.  All rights reserved.";


/* MPSVersion -- return version string
 *
 * The value of MPSVersion is a declared object comprising the
 * concatenation of all the version info.
 */

char MPSVersionString[] =
  "@(#)HQNMPS, "
  "product." MPS_PROD_STRING ", " MPS_RELEASE ", platform." MPS_PF_STRING
  ", variety." MPS_VARIETY_STRING ", compiled on " __DATE__ " " __TIME__;

char *MPSVersion(void)
{
  return MPSVersionString;
}

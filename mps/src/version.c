/* impl.c.version: Version Inspection
 *
 * $HopeName: !version.c(trunk.2) $
 * Copyright (C) 1998.  Harlequin Group plc.  All rights reserved.
 *
 * PURPOSE
 *
 * The purpose of this module is to provide a means by which the
 * version of the MM library being used can be determined.
 *
 * READERSHIP
 *
 * .readership: Any MPS developers.
 *
 * DESIGN
 *
 * .design: See design.mps.version-library, but to let you in on a
 * secret it works by declaring a string with all the necessary info
 * in.
 */

#include "mpm.h"

/* .release: When making a new release, change the expansion of
 * MPS_RELEASE to be a string of the form "release.dylan.crow.2"
 * or whatever. */
#define MPS_RELEASE "$HopeName: !version.c(trunk.2) $ *** DEVELOPMENT ONLY ***"

/* Version String
 *
 * MPSVersion is a declared object comprising the concatenation of
 * various other strings. */
char MPSVersionString[] =
  "@(#)HQNMPS, "
  "product." MPS_PROD_STRING ", " MPS_RELEASE ", platform." MPS_PF_STRING
  ", variety." MPS_VARIETY_STRING ", compiled on " __DATE__ " " __TIME__;

char *MPSVersion(void)
{
  return MPSVersionString;
}

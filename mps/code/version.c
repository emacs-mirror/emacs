/* version.c: VERSION INSPECTION
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  
 * See end of file for license.
 *
 * PURPOSE
 *
 * The purpose of this module is to provide a means by which the
 * version of the MM library being used can be determined.
 *
 * DESIGN
 *
 * .design: See <design/version-library>, but -- to let you in on a
 * secret -- it works by declaring a string with all the necessary info
 * in.  */

#include "mpm.h"

SRCID(version, "$Id$");


/* MPS_RELEASE -- the release name
 *
 * .release.use: This macro is used (i) to prepare MPSVersionString
 * (see below) and so identify any binary built using this source
 * file; (ii) by the Sphinx documentation (see manual/source/conf.py)
 * to identify the documentation; (iii) by the Autoconf script (see
 * configure.ac) to identify the configure script.
 *
 * .release.meaning: This names the next release that is expected to
 * be built from these sources.
 *
 * .release.procedure: After making a version branch, update this
 * string in the master sources to name the next version. After making
 * a point release, update this string to name the next point release.
 *
 * .release.old: before 2006-02-01 the style was "release.epcore.chub".
 */

#define MPS_RELEASE "release/1.115.0"


/* MPSCopyrightNotice -- copyright notice for the binary
 *
 * .copyright.year: This one should have the current year in it
 * (assuming we've made any substantial changes to the library this year).
 */

extern char MPSCopyrightNotice[];
char MPSCopyrightNotice[] =
  "Portions copyright (c) 2010-2016 Ravenbrook Limited and Global Graphics Software.";


/* MPSVersion -- return version string
 *
 * The value of MPSVersion is a declared object comprising the
 * concatenation of all the version info.  The "@(#)" prefix 
 * is the convention used by the BSD Unix command what(1); 
 * see also guide.mps.version.
 */

extern char MPSVersionString[];
char MPSVersionString[] =
  "@(#)Ravenbrook MPS, "
  "product." MPS_PROD_STRING ", " MPS_RELEASE ", platform." MPS_PF_STRING
  ", variety." MPS_VARIETY_STRING ", compiled on " __DATE__ " " __TIME__;

char *MPSVersion(void)
{
  return MPSVersionString;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited 
 * <http://www.ravenbrook.com/>.
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

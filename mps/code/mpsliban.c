/* impl.c.mpsliban: RAVENBROOK MEMORY POOL SYSTEM LIBRARY INTERFACE (ANSI)
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: The purpose of this code is
 *   1. to connect the MPS Library Interface to the ANSI C libraries,
 *      where they exist, and
 *   2. to provide an example of how to implement the MPS Library
 *      Interface.
 *
 * .readership: For MPS client application developers and MPS developers.
 * .sources: <design/lib/>
 *
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * .trans.file: The ANSI standard says (in section 7.9.1) that FILE is an
 * object type, and hence the casts between FILE and mps_lib_FILE (an
 * incomplete type) are not necessarily valid.  We assume that this trick
 * works, however, in all current environments.
 */

#include "mpslib.h"

#include "mpstd.h"

#ifdef MPS_OS_SU
#include "ossu.h"
#endif
#ifdef MPS_OS_XC
#include "osxc.h"
#endif

#ifdef MPS_OS_IA
struct itimerspec; /* stop complaints from time.h */
#endif
#include <time.h>

#ifdef MPS_OS_SU
extern int fputc (int c, FILE *stream);
extern int fputs (const char *s, FILE *stream);
extern clock_t clock(void);
extern long strtol(const char *, char **, int);
/* @@@@ This doesn't do quite the right thing, but will get by. */
#define strtoul(a,b,c) (unsigned long)strtol(a, b, c)
extern void *memset(void *, int, size_t);
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>


int mps_lib_get_EOF(void)
{
  return EOF;
}

mps_lib_FILE *mps_lib_get_stderr(void)
{
  return (mps_lib_FILE *)stderr; /* see .trans.file */
}

mps_lib_FILE *mps_lib_get_stdout(void)
{
  return (mps_lib_FILE *)stdout; /* see .trans.file */
}

int mps_lib_fputc(int c, mps_lib_FILE *stream)
{
  return fputc(c, (FILE *)stream); /* see .trans.file */
}

int mps_lib_fputs(const char *s, mps_lib_FILE *stream)
{
  return fputs(s, (FILE *)stream); /* see .trans.file */
}


void mps_lib_abort(void)
{
  abort();
}


void *mps_lib_memset(void *s, int c, size_t n)
{
  return memset(s, c, n);
}

void *mps_lib_memcpy(void *s1, const void *s2, size_t n)
{
  return memcpy(s1, s2, n);
}

int mps_lib_memcmp(const void *s1, const void *s2, size_t n)
{
  return memcmp(s1, s2, n);
}


/* @@@@ Platform specific conversion? */
/* See http://devworld.apple.com/dev/techsupport/insidemac/OSUtilities/OSUtilities-94.html#MARKER-9-32 */
mps_clock_t mps_clock(void)
{
  return (unsigned long)clock();
}


unsigned long mps_lib_telemetry_control(void)
{
  char *s;
  char **null = NULL;

  s = getenv("MPS_TELEMETRY_CONTROL");
  if(s != NULL)
    return strtoul(s, null, 0);
  else
    return 0;
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

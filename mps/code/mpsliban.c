/* mpsliban.c: RAVENBROOK MEMORY POOL SYSTEM LIBRARY INTERFACE (ANSI)
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
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
#include "event.h"

#ifdef MPS_OS_XC
#include "osxc.h"
#endif

#include <time.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>


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


void mps_lib_assert_fail(const char *message)
{
  fflush(stdout); /* synchronize */
  fprintf(stderr, "\nMPS ASSERTION FAILURE: %s\n\nRECENT EVENTS:\n", message);
  EventDump((mps_lib_FILE *)stderr);
  fflush(stderr); /* make sure the message is output */
  abort();
}


void *(mps_lib_memset)(void *s, int c, size_t n)
{
  return memset(s, c, n);
}

void *(mps_lib_memcpy)(void *s1, const void *s2, size_t n)
{
  return memcpy(s1, s2, n);
}

int (mps_lib_memcmp)(const void *s1, const void *s2, size_t n)
{
  return memcmp(s1, s2, n);
}


/* @@@@ Platform specific conversion? */
/* See http://devworld.apple.com/dev/techsupport/insidemac/OSUtilities/OSUtilities-94.html#MARKER-9-32 */
mps_clock_t mps_clock(void)
{
  return (unsigned long)clock();
}


mps_clock_t mps_clocks_per_sec(void)
{
  return (unsigned long)CLOCKS_PER_SEC;
}


/* mps_lib_telemetry_control -- get and interpret MPS_TELEMETRY_CONTROL */

#ifdef MPS_BUILD_MV
/* MSVC warning 4996 = stdio / C runtime 'unsafe' */
/* Objects to: getenv.  See job001934. */
#pragma warning( disable : 4996 )
#endif

unsigned long mps_lib_telemetry_control(void)
{
  char *s;
  char **null = NULL;
  unsigned long mask;
  char buf[256];
  char *word;
  char *p;
  char *sep = " ";
  char rowName[256];

  s = getenv("MPS_TELEMETRY_CONTROL");
  if (s == NULL)
    return 0;

  /* If the value can be read as a number, use it. */
  mask = strtoul(s, null, 0);
  if (mask != 0)
    return mask;

  /* copy the envar to a buffer so we can mess with it. */
  strncpy(buf, s, sizeof(buf) - 1);
  buf[sizeof(buf) - 1] = '\0';
  /* downcase it */
  for (p = buf; *p != '\0'; ++p)
          *p = (char)tolower(*p);
  
  /* Split the value at spaces and try to match the words against the names
     of event kinds, enabling them if there's a match. */
  for (word = strtok(buf, sep); word != NULL; word = strtok(NULL, sep)) {
          if (strcmp(word, "all") == 0) {
                  mask = (unsigned long)-1;
                  printf("All events.");
                  return mask;
          }
#define TELEMATCH(X, name, rowDoc) \
    strncpy(rowName, #name, sizeof(rowName) - 1); \
    rowName[sizeof(rowName) - 1] = '\0'; \
    for (p = rowName; *p != '\0'; ++p) \
            *p = (char)tolower(*p); \
    if (strcmp(word, rowName) == 0) {          \
      mask |= (1ul << EventKind##name); \
      printf("Events to include " rowDoc "\n"); }
    EventKindENUM(TELEMATCH, X)
  }
  
  return mask;
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

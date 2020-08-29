/* mpsliban.c: RAVENBROOK MEMORY POOL SYSTEM LIBRARY INTERFACE (ANSI)
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * Portions copyright (c) 2002 Global Graphics Software.
 *
 * .purpose: The purpose of this code is
 *   1. to connect the MPS Library Interface to the ANSI C libraries,
 *      where they exist, and
 *   2. to provide an example of how to implement the MPS Library
 *      Interface.
 *
 * .readership: For MPS client application developers and MPS developers.
 * .sources: <design/lib>
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

#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>


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


static void mps_lib_assert_fail_default(const char *file, unsigned line,
                                        const char *condition)
{
  /* Synchronize with stdout. */
  (void)fflush(stdout);
  (void)fprintf(stderr,
                "The MPS detected a problem!\n"
                "%s:%u: MPS ASSERTION FAILED: %s\n"
                "See the \"Assertions\" section in the reference manual:\n"
                "https://www.ravenbrook.com/project/mps/master/manual/html/topic/error.html#assertions\n",
                file, line, condition);
  /* Ensure the message is output even if stderr is buffered. */
  (void)fflush(stderr);
  mps_telemetry_flush();
  ASSERT_ABORT(); /* see config.h */
}

static mps_lib_assert_fail_t mps_lib_assert_handler = mps_lib_assert_fail_default;

void mps_lib_assert_fail(const char *file,
                          unsigned line,
                          const char *condition)
{
  mps_lib_assert_handler(file, line, condition);
}

mps_lib_assert_fail_t mps_lib_assert_fail_install(mps_lib_assert_fail_t handler)
{
  mps_lib_assert_fail_t old_handler = mps_lib_assert_handler;
  mps_lib_assert_handler = handler;
  return old_handler;
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


/* If your platform has a low-resolution clock(), and there are
 * higher-resolution clocks readily available, then using one of those
 * will improve MPS scheduling decisions and the quality of telemetry
 * output.  For instance, with getrusage():
 *
 *   #include <sys/resource.h>
 *   struct rusage s;
 *   int res = getrusage(RUSAGE_SELF, &s);
 *   if (res != 0) {
 *     ...
 *   }
 *   return ((mps_clock_t)s.ru_utime.tv_sec) * 1000000 + s.ru_utime.tv_usec;
 */

mps_clock_t mps_clock(void)
{
  /* The clock values need to fit in mps_clock_t.  If your platform
     has a very wide clock type, trim or truncate it. */
  assert(sizeof(mps_clock_t) >= sizeof(clock_t));

  return (mps_clock_t)clock();
}


mps_clock_t mps_clocks_per_sec(void)
{
  /* must correspond to whatever mps_clock() does */
  return (mps_clock_t)CLOCKS_PER_SEC;
}


/* mps_lib_telemetry_control -- get and interpret MPS_TELEMETRY_CONTROL */

#ifdef MPS_BUILD_MV
/* MSVC warning 4996 = stdio / C runtime 'unsafe' */
/* Objects to: getenv.  See job001934. */
#pragma warning( disable : 4996 )
#endif

/* Simple case-insensitive string comparison */
static int striequal(const char *s0, const char *s1)
{
  int c;
  do {
    c = *s0;
    if (tolower(c) != tolower(*s1)) /* note: works for '\0' */
      return 0;
    ++s0;
    ++s1;
  } while (c != '\0');
  return 1;
}

unsigned long mps_lib_telemetry_control(void)
{
  char *s;
  char **null = NULL;
  unsigned long mask;
  char buf[256];
  char *word;
  const char *sep = " ";

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

  /* Split the value at spaces and try to match the words against the names
     of event kinds, enabling them if there's a match. */
  for (word = strtok(buf, sep); word != NULL; word = strtok(NULL, sep)) {
    if (striequal(word, "all")) {
      mask = (unsigned long)-1;
      return mask;
    }
#define TELEMATCH(X, name, rowDoc) \
    if (striequal(word, #name)) \
      mask |= (1ul << EventKind##name);
    EventKindENUM(TELEMATCH, X)
#undef TELEMATCH
  }

  return mask;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* $HopeName: MMQA_harness!testlib:testlib.c(trunk.23) $
plinth for testing the MPS */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <time.h>
#include "testlib.h"

/* This comes from mpsliban.c, except for the
   assertion handler. */

#include "mpslib.h"

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

mps_clock_t mps_clocks_per_sec(void)
{
  return (unsigned long)CLOCKS_PER_SEC;
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

#ifndef MMQA_DEFINED_mps_assert_install

void mps_lib_assert_fail(const char *message)
{
  mmqa_assert_handler(message, NULL, NULL, 0);
}

#endif

/* diag.c: MEMORY POOL MANAGER DIAGNOSTICS
 *
 * $Id$
 * Copyright (c) 2007 Ravenbrook Limited.  See end of file for license.
 *
 */

#include <stdarg.h>

#include "mpm.h"
#include "mpslib.h" /* for mps_lib_stdout */


static mps_lib_FILE *FilterStream(void);
static int FilterStream_fputc(int c, mps_lib_FILE *stream);
static int FilterStream_fputs(const char *s, mps_lib_FILE *stream);


/* Stream -- output to FilterStream or to a real mps_lib_FILE stream
 *
 * There are only two functions and two destinations; a full class 
 * hierarchy would be overkill!  RHSK 2007-08-08.
 */

int stream_fputc(int c, mps_lib_FILE *stream)
{
  if(stream == FilterStream())
    return FilterStream_fputc(c, stream);
  else
    return mps_lib_fputc(c, stream);
}

int stream_fputs(const char *s, mps_lib_FILE *stream)
{
  if(stream == FilterStream())
    return FilterStream_fputs(s, stream);
  else
    return mps_lib_fputs(s, stream);
}


/* Diag -- buffer to store a diagnostic
 *
 */

enum {DIAG_BUF_SIZE = 10000};

typedef struct DiagStruct {
  /* @@ needs sig */
  const char  *tag;
  Count n;
  char buf[DIAG_BUF_SIZE];
} *Diag;


/* FilterStream -- capable of filtering diagnostics
 *
 * This is not really an mps_lib_FILE*; it is a single global instance 
 * of a DiagStruct.
 *
 * Output is stored in a buffer in a DiagStruct, to be filtered and 
 * output when complete.
 */

struct DiagStruct FilterDiagGlobal;

static mps_lib_FILE *FilterStream(void)
{
  return (mps_lib_FILE*)&FilterDiagGlobal;
}

static mps_lib_FILE *FilterUnderlyingStream(void)
{
  return mps_lib_stdout;
}

typedef struct RuleStruct {
  const char *action;
  const char *tag;
  const char *para;
  const char *line;
  int state;
} *Rule;

struct RuleStruct RulesGlobal[] = {
  { "+", "TraceStart", "*", "*" }
};

static void FilterOutput(Diag diag, Rule rules)
{
  Res res;
  Index i;
  Bool newline = TRUE;
  
  if(diag->tag == NULL)
    diag->tag = "(...no tag...)";

  res = WriteF(FilterUnderlyingStream(), "\nMPS.$S {\n", diag->tag, NULL);
  AVER(res == ResOK);

  for(i = 0; i < diag->n; i++) {
    char c;
    int r;

    if(newline) {
      r = stream_fputc(' ', FilterUnderlyingStream());
      AVER(r != mps_lib_EOF);
      newline = FALSE;
    }
    
    c = diag->buf[i];
    r = stream_fputc(c, FilterUnderlyingStream());
    AVER(r != mps_lib_EOF);
    
    if(c == '\n') {
      newline = TRUE;
    }
  }

  res = WriteF(FilterUnderlyingStream(), "} MPS.$S\n", diag->tag, NULL);
  AVER(res == ResOK);
}

static void FilterStream_TagBegin(mps_lib_FILE *stream, const char *tag)
{
  Diag diag;
  diag = (Diag)stream;
  /* AVERT(Diag, diag) */
  
  AVER(diag->tag == NULL);

  /* @@ when all diags are tagged, the buffer must be empty */
  /* @@ but for now, as a courtesy... */
  if(diag->n > 0) {
    FilterOutput(diag, NULL);
    diag->n = 0;
  }

  AVER(diag->n == 0);
  diag->tag = tag;
}

static void FilterStream_TagEnd(mps_lib_FILE *stream, const char *tag)
{
  Diag diag;
  diag = (Diag)stream;
  /* AVERT(Diag, diag) */

  AVER(diag->tag != NULL);
  /* AVER(strequal(diag->tag, tag)); */

  /* Output the diag */
  FilterOutput(diag, &RulesGlobal[0]);

  diag->tag = NULL;
  diag->n = 0;
}

static int FilterStream_fputc(int c, mps_lib_FILE *stream)
{
  Diag diag;
  
  AVER(c != mps_lib_EOF);
  AVER(stream == FilterStream());

  diag = (Diag)stream;
  /* AVERT(Diag, diag) */
  /* @@ when all diags are tagged: AVER(diag->tag != NULL); */

  AVER(diag->n + 1 <= sizeof(diag->buf));
  if (diag->n + 1 > sizeof(diag->buf))
    return mps_lib_EOF;
  
  /* add c to buffer */
  diag->buf[diag->n++] = c;
  return c;
}

static int FilterStream_fputs(const char *s, mps_lib_FILE *stream)
{
  Diag diag;
  Count l;
  Index i;
  
  AVER(s);
  AVER(stream == FilterStream());

  diag = (Diag)stream;
  /* AVERT(Diag, diag) */
  /* @@ when all diags are tagged: AVER(diag->tag != NULL); */

  l = StringLength(s);
  
  AVER(diag->n + l <= sizeof(diag->buf));
  if (diag->n + l > sizeof(diag->buf))
    return mps_lib_EOF;

  /* add s to buffer */
  for (i = 0; i < l; i++) {
    diag->buf[diag->n++] = s[i];
  }
  return 1;
}


/* DIAG_WITH_STREAM_AND_WRITEF -- Diagnostic output channel
 *
 * Only used for DIAG_WITH_STREAM_AND_WRITEF; see config.h.
 */

Bool DiagEnabledGlobal = TRUE;

Bool DiagIsOn(void)
{
  return DiagEnabledGlobal;
}

mps_lib_FILE *DiagStream(void)
{
  if(0) {
    return FilterStream();
  } else {
    return mps_lib_stdout;
  }
}

static void DiagTagBegin(mps_lib_FILE *stream, const char *tag)
{
  AVER(stream);
  AVER(tag);

  if(stream == FilterStream()) {
    FilterStream_TagBegin(stream, tag);
  } else {
    Res res;
    res = WriteF(stream, "MPS.$S {\n", tag, NULL);
    AVER(res == ResOK);
  }
}

static void DiagTagEnd(mps_lib_FILE *stream, const char *tag)
{
  AVER(stream);
  AVER(tag);

  if(stream == FilterStream()) {
    FilterStream_TagEnd(stream, tag);
  } else {
    Res res;
    res = WriteF(stream, "} MPS.$S\n", tag, NULL);
    AVER(res == ResOK);
  }
}


/* Diag*F functions -- interface for general MPS code (via macros)
 *
 * These function manage TagBegin/End, and WriteF the text to 
 * DiagStream().
 *
 * Direct writing to DiagStream() is also permitted (eg. from a
 * Describe method).
 */

void DiagSingleF(const char *tag, ...)
{
  va_list args;
  Res res;

  DiagTagBegin(DiagStream(), tag);

  va_start(args, tag);
  res = WriteF_v(DiagStream(), args);
  AVER(res == ResOK);
  va_end(args);

  DiagTagEnd(DiagStream(), tag);
}

void DiagFirstF(const char *tag, ...)
{
  va_list args;
  Res res;

  DiagTagBegin(DiagStream(), tag);

  va_start(args, tag);
  res = WriteF_v(DiagStream(), args);
  AVER(res == ResOK);
  va_end(args);
}

void DiagMoreF(const char *firstformat, ...)
{
  va_list args;
  Res res;

  /* ISO C says there must be at least one named parameter: hence */
  /* the named firstformat.  It only looks different: there is no */
  /* change from the expected WriteF protocol.  (In particular, */
  /* firstformat may legally be NULL, with the variable part empty). */

  va_start(args, firstformat);
  res = WriteF_firstformat_v(DiagStream(), firstformat, args);
  AVER(res == ResOK);
  va_end(args);
}

void DiagEnd(const char *tag)
{
  DiagTagEnd(DiagStream(), tag);
}

extern void diag_test(void);

void diag_test(void)
{
  DIAG_SINGLEF(( "TestTag1", "text $U.\n", 42, NULL ));

  DIAG_FIRSTF(( "TestTag2", "text $U.\n", 42, NULL ));
  DIAG_MOREF(( NULL ));
  DIAG_MOREF(( "string $S.\n", "fooey!", NULL ));
  DIAG_MOREF(( NULL ));
  DIAG_MOREF(( "Another string $S.\n", "baloney!", NULL ));
  DIAG_END( "TestTag2" );
}

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2007 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

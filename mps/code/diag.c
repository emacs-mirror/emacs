/* diag.c: MEMORY POOL MANAGER DIAGNOSTICS
 *
 * $Id$
 * Copyright (c) 2007 Ravenbrook Limited.  See end of file for license.
 *
 * To Do: [RHSK 2007-08-11]
 *  @@ unit test for StringMatch
 *  @@ handle diag->buf overflow (currently asserts)
 *  @@ sigs and AVERTs for Diag and Rule
 *  @@ .improve.empty-diag
 */

#include <stdarg.h>

#include "mpm.h"
#include "mpslib.h" /* for mps_lib_stdout */


static mps_lib_FILE *FilterStream(void);
static int FilterStream_fputc(int c, mps_lib_FILE *stream);
static int FilterStream_fputs(const char *s, mps_lib_FILE *stream);
static void diag_test(void);


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
 * Output is stored in a DiagStruct, to be filtered and output
 * (or not) when complete.
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

enum {
  TPMatch_Unknown = 0,  /* initial value = 0 */
  TPMatch_Yes,
  TPMatch_No
};

typedef struct RuleStruct {
  const char *action;
  const char *tag;
  const char *para;
  const char *line;
  int tpMatch;  /* does this rule match diag on tag and para? */
} *Rule;

struct RuleStruct RulesGlobalX[] = {
  { "+", "*", "*", "*" },
  { "+", "TraceStart", "*", "*" },
  { "+", "TraceStart", "*", "freeSet" },
  { NULL, "", "", "" }
};

struct RuleStruct RulesGlobal[] = {
  { "+", "*", "*", "*" },
  { "+", "ChainCondemnAuto", "gens [0..0]", "*" },
  { "+", "TraceStart", "*", "*" },
  { "+", "TraceStart", "because code 1:", "*" },
  { "-", "TraceStart", "*", "controlPool" },
  { "-", "TraceStart", "*", "ommit" },
  { "-", "TraceStart", "*", "zoneShift" },
  { "-", "TraceStart", "*", "alignment" },
  { "-", "amcScanNailed-loop", "*", "*" },
  { NULL, "", "", "" }
};

static void Rules_diag(Rule rules)
{
  Index ir;
  
  AVER(rules);
  DIAG_FIRSTF(( "Rules_diag", 
    "Only showing diags permitted by these tag/paragraph/line"
    " rules:\n", NULL ));
  for(ir = 0; rules[ir].action != NULL; ir++) {
    Rule rule = &rules[ir];
    DIAG_MOREF(( "$S$S/$S/$S\n", rule->action, rule->tag, 
                 rule->para, rule->line,
                 NULL ));
  }
  DIAG_END("Rules_diag");
}

static Bool StringEqual(const char *s1, const char *s2)
{
  Index i;

  AVER(s1);
  AVER(s2);

  for(i = 0; ; i++) {
    if(s1[i] != s2[i])
      return FALSE;
    if(s1[i] == '\0')    
      break;
  }
  return TRUE;
}

static Bool StringMatch(const char *patt, Count pattLen, 
                        const char *buf, Index i, Index j)
{
  Index im; /* start of tentative match */
  Index ip; /* index into patt */

  AVER(patt);
  AVER(buf);
  AVER(i <= j);

  /* Search (naively) for patt anywhere inside buf[i..j) */
  for(im = i; im + pattLen <= j; im++) {
    /* Consider upto pattLen chars starting at patt[0] and buf[im] */
    for(ip = 0; ip < pattLen; ip++) {
      if(patt[ip] != buf[im + ip])
        break;
    }
    if(ip == pattLen) {
      return TRUE;
    }
  }

  return FALSE;
}

static Bool MatchLine(Rule rule, Diag diag, Index i, Index j)
{
  Count pattLen;

  AVER(rule);
  AVER(diag);
  AVER(i <= j);
  AVER(j <= diag->n);

  if(rule->line[0] == '*')
    return TRUE;
  
  pattLen = StringLength(rule->line);

  return StringMatch(rule->line, pattLen, diag->buf, i, j);
}

static Bool MatchPara(Rule rule, Diag diag)
{
  Count pattLen;

  AVER(rule);
  AVER(diag);

  if(rule->para[0] == '*')
    return TRUE;
  
  pattLen = StringLength(rule->para);

  return StringMatch(rule->para, pattLen, diag->buf, 0, diag->n);
}

static Bool MatchTag(Rule rule, const char *tag)
{
  AVER(rule);
  AVER(rule->tag);
  AVER(tag);

  if(rule->tag[0] == '*')
    return TRUE;

  return StringEqual(rule->tag, tag);
}

static void LineOutput(Diag diag, Index i, Index j)
{
  int r;

  AVER(diag);
  AVER(i <= j);
  AVER(j <= diag->n);
  
  r = stream_fputc(' ', FilterUnderlyingStream());
  AVER(r != mps_lib_EOF);
  
  for(; i < j; i++) {
    char c;
    c = diag->buf[i];
    r = stream_fputc(c, FilterUnderlyingStream());
    AVER(r != mps_lib_EOF);
  }
}

static void FilterOutput(Diag diag, Rule rules)
{
  static Bool inside = FALSE;
  Res res;
  Count nr;
  Index ir;
  Index i, j;
  Bool nolinesyet = TRUE;
  /* .improve.empty-diag: @@ We only output if nolinesyet becomes 
   * FALSE.  So if diag has no output, entire diag will be skipped.
   * That means an intentionally empty diag such as:
   *   DIAG_SINGLEF(( "Tag", NULL ))
   * will never appear, but gives no warning.  This is probably 
   * a bug: we should distinguish between no-output because 
   * it was all filtered out, and just no output.  RHSK.
   */
  
  AVER(!inside);
  inside = TRUE;
  AVER(diag);
  AVER(rules);
  
  if(diag->tag == NULL)
    diag->tag = "(no tag)";

  /* Count the rules */
  for(ir = 0; rules[ir].action != NULL; ir++) {
    rules[ir].tpMatch = TPMatch_Unknown;
  }
  nr = ir;
  
  /* Filter */
  /* emptyonce = (diag->n == 0); */
  /* for(i = 0; emptyonce || i < diag->n; i = j) { */
  /*   emptyonce = FALSE; */
  for(i = 0; i < diag->n; i = j) {
    
    /* Get the next line [i..j) */
    for(j = i; j < diag->n; j++) {
      if(diag->buf[j] == '\n') {
        j++;
        break;
      }
    }

    /* Find the lowest rule that matches it. */
    ir = nr - 1;
    for(;;) {
      Rule rule = &rules[ir];
      if(rule->tpMatch == TPMatch_Unknown) {
        /* memoize tpMatch */
        if(MatchTag(rule, diag->tag) && MatchPara(rule, diag)) {
          rule->tpMatch = TPMatch_Yes;
        } else {
          rule->tpMatch = TPMatch_No;
        }
      }
      if(rule->tpMatch == TPMatch_Yes && MatchLine(rule, diag, i, j))
        break;
      AVER(ir != 0); /* there must ALWAYS be a matching rule */
      ir--;
    }
    
    /* Do the rule's action. */
    if(0)
      (void) WriteF(FilterUnderlyingStream(),
                    "[RULE: $U (of $U);", ir, nr, 
                    " ACTION: $C]\n", rules[ir].action[0],
                    NULL);
    if(rules[ir].action[0] == '+') {
      if(nolinesyet) {
        res = WriteF(FilterUnderlyingStream(), "MPS.$S {", diag->tag, NULL);
        AVER(res == ResOK);
        nolinesyet = FALSE;
      }
      LineOutput(diag, i, j);
    }
  }

  if(!nolinesyet) {
    res = WriteF(FilterUnderlyingStream(), "}\n", NULL);
    AVER(res == ResOK);
  }
  inside = FALSE;
}

static void FilterStream_TagBegin(mps_lib_FILE *stream, const char *tag)
{
  static Bool first = TRUE;
  Diag diag;

  AVER(stream);
  AVER(tag);

  diag = (Diag)stream;
  /* AVERT(Diag, diag) */

  if(first) {
    first = FALSE;
    Rules_diag(&RulesGlobal[0]);
    diag_test();
  }

  if(diag->tag != NULL) {
    /* Be helpful to the poor programmer! */
    (void) WriteF(FilterUnderlyingStream(),
                  "\nWARNING: diag tag \"$S\" is still current"
                  " (missing DIAG_END()).", 
                  diag->tag, NULL);  
  }
  AVER(diag->tag == NULL);

  /* @@ when all diags are tagged, the buffer must be empty */
  /* @@ but for now, as a courtesy... */
  if(diag->n > 0) {
    FilterOutput(diag, &RulesGlobal[0]);
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
  AVER(StringEqual(diag->tag, tag));

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
  diag->buf[diag->n++] = (char)c;
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
  if(1) {
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

static void diag_test(void)
{
  DIAG_SINGLEF(( "DIAGTEST-Tag1", "text $U.\n", 42, NULL ));

  DIAG_SINGLEF(( "DIAGTEST-NoLines", NULL ));

  DIAG_FIRSTF((
    "DIAGTEST-StringEqual",
    "Fred = Fred: $U.\n",
    StringEqual("Fred", "Fred"),
    NULL
  ));
  DIAG_MOREF(("Fred = Tom: $U.\n", StringEqual("Fred", "Tom"), NULL));
  DIAG_MOREF(("Tom = Fred: $U.\n", StringEqual("Tom", "Fred"), NULL));
  DIAG_MOREF(("0 = Fred: $U.\n", StringEqual("", "Fred"), NULL));
  DIAG_MOREF(("Fred = 0: $U.\n", StringEqual("Fred", ""), NULL));
  DIAG_MOREF(("0 = 0: $U.\n", StringEqual("", ""), NULL));
  DIAG_MOREF(("0 = 000: $U.\n", StringEqual("", "\0\0"), NULL));
  DIAG_END("DIAGTEST-StringEqual");

#if 0
  DIAG_FIRSTF(( "TestTag2", "text $U.\n", 42, NULL ));
  DIAG_MOREF(( NULL ));
  DIAG_MOREF(( "string $S.\n", "fooey!", NULL ));
  DIAG_MOREF(( NULL ));
  DIAG_MOREF(( "Another string $S.\n", "baloney!", NULL ));
  DIAG_END( "TestTag2" );
#endif
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

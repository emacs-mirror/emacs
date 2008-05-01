/* diag.c: MEMORY POOL MANAGER DIAGNOSTICS
 *
 * $Id$
 * Copyright (c) 2007 Ravenbrook Limited.  See end of file for license.
 *
 * To Do: [RHSK 2007-08-13]
 *  @@ sigs and AVERTs for Rule, and macro for Rules initializer
 *  @@ deprecate un-tagged diags, remove old macros
 *  @@ every diag should end with \n: warn if this is missing.
 */

#include <stdarg.h>

#include "mpm.h"
#include "mpslib.h" /* for mps_lib_stdout */


typedef struct RuleStruct {
  const char *action;
  const char *tag;
  const char *para;
  const char *line;
  int tpMatch;  /* .tpmatch */
  /* @@ needs sig; (at end, to make initializer expression easy?) */
} *Rule;


/* RulesGlobal -- throw away some diags (see INSTRUCTIONS below) */

struct RuleStruct RulesGlobal[] = {
  { "+", "*", "*", "*" },
  { "-", "DIAGTEST_", "*", "*" },
  { "-", "SegPrefZonesNext", "*", "*" },
  { "-", "SegPrefZonesClose_newzone", "*", "*" },
  { NULL, "", "", "" }
};

struct RuleStruct RulesGlobalExample[] = {
  { "+", "*", "*", "*" },
  { "-", "DIAGTEST_", "*", "*" },
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

/* RulesGlobal -- INSTRUCTIONS
 *
 * In your local copy of diag.c, you can modify RulesGlobal as you 
 * wish, to control what diags you see.
 *
 * Each rule consists of: action, tag, para, and line.  A rule that 
 * matches on TAG, PARA and LINE determines what ACTION is taken 
 * for that line of that diag.  Later rules override earlier rules, 
 * ie. the lowest matching rule wins.  (And at least one rule must 
 * match, so the first rule should be a catch-all).
 *
 * ACTION = "+" (output this line of diag), or "-" (skip this line).
 *
 * TAG: does pattern (text or *) appear in diag's tag?
 *
 * PARA: does pattern (text or *) appear anywhere in diag's text output
 * (does not match the tag)?
 *
 * LINE: does pattern (text or *) appear on this line of the diag
 * text?
 *
 * Note: a diag that deliberately has no output, eg.
 *     DIAG_SINGLEF(( "MyTag", NULL )),
 * is treated as having a single empty 'line'.  See .empty-diag.
 *
 * Note: for help debugging your ruleset, see .rules.debug below.
 *
 * Note: the entire filtering mechanism can be turned off, so that 
 * diagnostics go immediately to mps_lib_stdout: see .filter-disable.
 */


/* Forward declarations */

static mps_lib_FILE *filterStream(void);
static int filterStream_fputc(int c, mps_lib_FILE *stream);
static int filterStream_fputs(const char *s, mps_lib_FILE *stream);
static void diag_test(void);


/* Stream -- output to filterStream or to a real mps_lib_FILE stream
 *
 * There are only two functions and two destinations; a full class 
 * hierarchy would be overkill!  RHSK 2007-08-08.
 */

int Stream_fputc(int c, mps_lib_FILE *stream)
{
  if(stream == filterStream())
    return filterStream_fputc(c, stream);
  else
    return mps_lib_fputc(c, stream);
}

int Stream_fputs(const char *s, mps_lib_FILE *stream)
{
  if(stream == filterStream())
    return filterStream_fputs(s, stream);
  else
    return mps_lib_fputs(s, stream);
}


/* Diag -- a buffer to store a diagnostic
 *
 */

#define DiagSig       ((Sig)0x519D1A99)  /* SIGnature DIAG */

typedef struct DiagStruct {
  Sig sig;
  const char  *tag;
  Count n;
  char buf[DIAG_BUFFER_SIZE];
} *Diag;

static Bool DiagCheck(Diag diag)
{
  CHECKS(Diag, diag);
  CHECKL(diag->n <= sizeof(diag->buf));
  return TRUE;
}



/* filterStream -- capable of filtering diagnostics
 *
 * This is not really an mps_lib_FILE*; it is a single global instance 
 * of a DiagStruct.
 *
 * Output is stored in a DiagStruct, to be filtered and output
 * (or not) when complete.
 */

static struct DiagStruct filterDiagGlobal = { DiagSig, NULL, 0 };

static mps_lib_FILE *filterStream(void)
{
  return (mps_lib_FILE*)&filterDiagGlobal;
}

/* filterStream_under: the underlying stream used to output diags */
/* that pass the filter. */
static mps_lib_FILE *filterStream_under(void)
{
  return mps_lib_stdout;
}

/* .tpmatch: does this rule match current diag's tag and para? */
enum {
  TPMatch_Unknown = 0,  /* initial value = 0 */
  TPMatch_Yes,
  TPMatch_No
};

static void rules_diag(Rule rules)
{
  Index ir;
  
  AVER(rules);
  DIAG_FIRSTF(( "DiagFilter_Rules", 
    "Only showing diags permitted by these tag/paragraph/line"
    " rules:\n", NULL ));
  for(ir = 0; rules[ir].action != NULL; ir++) {
    DIAG_DECL( Rule rule = &rules[ir]; )
    DIAG_MOREF(( "$S$S/$S/$S\n", rule->action, rule->tag, 
                 rule->para, rule->line,
                 NULL ));
  }
  DIAG_END("DiagFilter_Rules");
}


/* patternOccurs -- does patt occur in buf[i..j)?
 *
 * Returns true iff patt[0..pattLen) literally occurs in buf[i..j).
 */

static Bool patternOccurs(const char *patt, Count pattLen, 
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

static Bool matchLine(Rule rule, Diag diag, Index i, Index j)
{
  AVER(rule);
  AVER(diag);
  AVER(i <= j);
  AVER(j <= diag->n);

  if(rule->line[0] == '*')
    return TRUE;

  return patternOccurs(rule->line, StringLength(rule->line),
                       diag->buf, i, j);
}

static Bool matchPara(Rule rule, Diag diag)
{
  AVER(rule);
  AVER(diag);

  if(rule->para[0] == '*')
    return TRUE;
  
  return patternOccurs(rule->para, StringLength(rule->para),
                       diag->buf, 0, diag->n);
}

static Bool matchTag(Rule rule, const char *tag)
{
  AVER(rule);
  AVER(rule->tag);
  AVER(tag);

  if(rule->tag[0] == '*')
    return TRUE;

  return patternOccurs(rule->tag, StringLength(rule->tag),
                       tag, 0, StringLength(tag));
}

static void filterStream_LineOut(Diag diag, Index i, Index j)
{
  int r;

  AVER(diag);
  AVER(i <= j);
  AVER(j <= diag->n);
  
  r = Stream_fputs(DIAG_PREFIX_LINE, filterStream_under());
  AVER(r != mps_lib_EOF);
  
  for(; i < j; i++) {
    char c;
    c = diag->buf[i];
    r = Stream_fputc(c, filterStream_under());
    AVER(r != mps_lib_EOF);
  }
}


/* filterStream_Output -- output this diag, if the rules select it
 *
 */

static void filterStream_Output(Diag diag, Rule rules)
{
  static Bool inside = FALSE;
  Res res;
  Count nr;
  Index ir;
  Index i, j;
  Bool nolinesyet = TRUE;
  Bool emptyonce;
  
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
  /* .empty-diag: Treat a diag that deliberately has no output, */
  /* eg: DIAG_SINGLEF(( "Tag", NULL )), as having a single empty */
  /* 'line'.  This is the only time a line may be empty. */
  emptyonce = (diag->n == 0);
  for(i = 0; emptyonce || i < diag->n; i = j) {
    
    /* Get the next line [i..j) */
    for(j = i; j < diag->n; j++) {
      if(diag->buf[j] == '\n') {
        j++;
        break;
      }
    }
    AVER(emptyonce || i < j);  /* .empty-diag */
    emptyonce = FALSE;

    /* Find the lowest rule that matches it. */
    ir = nr - 1;
    for(;;) {
      Rule rule = &rules[ir];
      if(rule->tpMatch == TPMatch_Unknown) {
        /* memoize .tpMatch */
        if(matchTag(rule, diag->tag) && matchPara(rule, diag)) {
          rule->tpMatch = TPMatch_Yes;
        } else {
          rule->tpMatch = TPMatch_No;
        }
      }
      if(rule->tpMatch == TPMatch_Yes && matchLine(rule, diag, i, j))
        break;
      AVER(ir != 0); /* there must ALWAYS be a matching rule */
      ir--;
    }
    
    /* Do the rule's action. */
    if(0) {
      /* .rules.debug: Turn this on to show which rule applied. */
      Rule rule = &rules[ir];
      (void) WriteF(filterStream_under(), "[$U/$U:", ir, nr, 
                    " $S$S/$S/$S] ", rule->action, rule->tag, 
                    rule->para, rule->line, NULL);
    }
    if(rules[ir].action[0] == '+') {
      if(nolinesyet) {
        res = WriteF(filterStream_under(),
                     DIAG_PREFIX_TAGSTART "$S {\n", diag->tag, NULL);
        AVER(res == ResOK);
        nolinesyet = FALSE;
      }
      filterStream_LineOut(diag, i, j);
    }
  }

  if(!nolinesyet) {
    res = WriteF(filterStream_under(), DIAG_PREFIX_TAGEND "}\n", NULL);
    AVER(res == ResOK);
  }
  inside = FALSE;
}

static void filterStream_TagBegin(mps_lib_FILE *stream, const char *tag)
{
  static Bool first = TRUE;
  Diag diag;

  AVER(stream);
  AVER(tag);

  diag = (Diag)stream;
  AVERT(Diag, diag);

  if(first) {
    first = FALSE;
    rules_diag(&RulesGlobal[0]);
    diag_test();
  }

  if(diag->tag != NULL) {
    /* Be helpful to the poor programmer! */
    (void) WriteF(filterStream_under(),
                  "\nWARNING: diag tag \"$S\" is still current"
                  " (missing DIAG_END()).", 
                  diag->tag, NULL);  
  }
  AVER(diag->tag == NULL);

  /* @@ when all diags are tagged, the buffer must be empty */
  /* @@ but for now, as a courtesy... */
  if(diag->n > 0) {
    filterStream_Output(diag, &RulesGlobal[0]);
    diag->n = 0;
  }

  AVER(diag->n == 0);
  diag->tag = tag;
}

static void filterStream_TagEnd(mps_lib_FILE *stream, const char *tag)
{
  Diag diag;
  diag = (Diag)stream;
  AVERT(Diag, diag);

  AVER(diag->tag != NULL);

  if(!StringEqual(diag->tag, tag)) {
    /* Be helpful to the poor programmer! */
    (void) WriteF(filterStream_under(),
                  "\nWARNING: diag tag \"$S\" is current, "
                  "but got DIAG_END(\"$S\").  (They must match).", 
                  diag->tag, tag, NULL);
  }
  AVER(StringEqual(diag->tag, tag));

  /* Output the diag */
  filterStream_Output(diag, &RulesGlobal[0]);

  diag->tag = NULL;
  diag->n = 0;
}

static int filterStream_fputc(int c, mps_lib_FILE *stream)
{
  Diag diag;
  
  AVER(c != mps_lib_EOF);
  AVER(stream == filterStream());

  diag = (Diag)stream;
  AVERT(Diag, diag);
  /* @@ when all diags are tagged: AVER(diag->tag != NULL); */

  AVER(diag->n + 1 <= sizeof(diag->buf));
  if(!(diag->n + 1 <= sizeof(diag->buf)))
    return mps_lib_EOF;
  
  /* add c to buffer */
  diag->buf[diag->n++] = (char)c;
  return c;
}

static int filterStream_fputs(const char *s, mps_lib_FILE *stream)
{
  Diag diag;
  Count l;
  Index i;
  
  AVER(s);
  AVER(stream == filterStream());

  diag = (Diag)stream;
  AVERT(Diag, diag);
  /* @@ when all diags are tagged: AVER(diag->tag != NULL); */

  l = StringLength(s);
  
  AVER(diag->n + l <= sizeof(diag->buf));
  if(!(diag->n + l <= sizeof(diag->buf)))
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
  /* .filter-disable: the entire filtering mechanism can be turned */
  /* off, so that diagnostics go immediately to mps_lib_stdout, */
  /* with no buffering or filtering. */
  Bool filter = TRUE;

  if(filter) {
    return filterStream();
  } else {
    return mps_lib_stdout;
  }
}

static void diagTagBegin(mps_lib_FILE *stream, const char *tag)
{
  AVER(stream);
  AVER(tag);

  if(stream == filterStream()) {
    filterStream_TagBegin(stream, tag);
  } else {
    Res res;
    res = WriteF(stream, DIAG_PREFIX_TAGSTART "$S {\n", tag, NULL);
    AVER(res == ResOK);
  }
}

static void diagTagEnd(mps_lib_FILE *stream, const char *tag)
{
  AVER(stream);
  AVER(tag);

  if(stream == filterStream()) {
    filterStream_TagEnd(stream, tag);
  } else {
    Res res;
    res = WriteF(stream, DIAG_PREFIX_TAGEND "}\n", tag, NULL);
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

  diagTagBegin(DiagStream(), tag);

  va_start(args, tag);
  res = WriteF_v(DiagStream(), args);
  AVER(res == ResOK);
  va_end(args);

  diagTagEnd(DiagStream(), tag);
}

void DiagFirstF(const char *tag, ...)
{
  va_list args;
  Res res;

  diagTagBegin(DiagStream(), tag);

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
  diagTagEnd(DiagStream(), tag);
}


/* Test Code -- unit tests for this source file
 *
 * These are for developers to run if they modify this source file.
 * There's no point running them otherwise.  RHSK.
 */

static void patternOccurs_test(Bool expect, const char *patt,
                               const char *text)
{
  Count pattLen = StringLength(patt);
  Count textLen = StringLength(text);
  enum {bufLen = 100};
  char buf[bufLen];
  Index start, i;
  Count padLen;
  Bool occurs;

  /* Call patternOccurs with this patt and text 3 times: each time */
  /* putting the text in the buffer at a different offset, to */
  /* verify that patternOccurs is not accepting matches outside the */
  /* [i..j) portion of the buffer. */
  
  for(start = 0; start < 21; start += 7) {
    AVER(bufLen > (start + textLen));
    /* put text into buf at start */
    for(i = 0; i < start; i++) {
      buf[i] = 'X';
    }
    for(i = 0; i < textLen; i++) {
      (buf+start)[i] = text[i];
    }
    padLen = bufLen - (start + textLen);
    for(i = 0; i < padLen; i++) {
      (buf+start+textLen)[i] = 'X';
    }
    occurs = patternOccurs(patt, pattLen, buf, start, start+textLen);
    AVER(occurs == expect);
  }
}

static void diag_test(void)
{
  DIAG_SINGLEF(( "DIAGTEST_Tag1", "text $U.\n", 42, NULL ));

  DIAG_SINGLEF(( "DIAGTEST_EmptyDiag", NULL ));

  DIAG_FIRSTF((
    "DIAGTEST_StringEqual",
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
  DIAG_END("DIAGTEST_StringEqual");

  DIAG_FIRSTF(( "DIAGTEST_patternOccurs", NULL ));
  patternOccurs_test(TRUE, "Fred", "Fred");
  patternOccurs_test(TRUE, "Fred", "XFredX");
  patternOccurs_test(TRUE, "Fred", "FFred");
  patternOccurs_test(TRUE, "Fred", "FrFred");
  patternOccurs_test(TRUE, "Fred", "FreFred");
  patternOccurs_test(TRUE, "Fred", "FreFreFFred");
  patternOccurs_test(TRUE, "Fred", "FredFred");
  patternOccurs_test(TRUE, "Fred", "FFredFre");
  patternOccurs_test(TRUE, "Fred", "FrFredFr");
  patternOccurs_test(TRUE, "Fred", "FreFredF");
  patternOccurs_test(TRUE, "Fred", "FreFreFFredFre");
  patternOccurs_test(TRUE, "Fred", "FredFredF");
  patternOccurs_test(TRUE, "X", "X");
  patternOccurs_test(TRUE, "", "X");
  patternOccurs_test(TRUE, "", "Whatever");
  patternOccurs_test(FALSE, "Fred", "Tom");
  patternOccurs_test(FALSE, "X", "Tom");
  patternOccurs_test(FALSE, "X", "x");
  patternOccurs_test(FALSE, "X", "");
  patternOccurs_test(FALSE, "Whatever", "");
  patternOccurs_test(FALSE, "Fred", "Fre");
  patternOccurs_test(FALSE, "Fred", "red");
  patternOccurs_test(FALSE, "Fred", "Fxred");
  patternOccurs_test(FALSE, "Fred", "Frexd");
  DIAG_END("DIAGTEST_patternOccurs");

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

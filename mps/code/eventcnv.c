/* eventcnv.c: Simple event log converter
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * This is a command-line tool that converts a binary format telemetry output
 * stream from the MPS into several textual formats.
 *
 * The default MPS library will write a telemetry stream to a file called
 * "mpsio.log" when the environment variable MPS_TELEMETRY_CONTROL is set
 * to an integer whose bits select event kinds.  For example:
 *
 *   MPS_TELEMETRY_CONTROL=7 amcss
 *
 * will run the amcss test program and emit a file with event kinds 0, 1, 2.
 * The file can then be converted into text format with a command like:
 *
 *   eventcnv -v | sort
 *
 * Note that the eventcnv program can only read streams that come from an
 * MPS compiled on the same platform.
 *
 * $Id$
 */

#include "config.h"

#include "eventdef.h"
#include "eventcom.h"
#include "eventpro.h"
#include "mpmtypes.h"
#include "testlib.h" /* for ulongest_t and associated print formats */

#include <stddef.h> /* for size_t */
#include <stdio.h> /* for printf */
#include <stdarg.h> /* for va_list */
#include <stdlib.h> /* for EXIT_FAILURE */
#include <assert.h> /* for assert */
#include <string.h> /* for strcmp */
#include <math.h> /* for sqrt */
#include "mpstd.h"

#ifdef MPS_BUILD_MV
/* MSVC warning 4996 = stdio / C runtime 'unsafe' */
/* Objects to: strncpy, sscanf, fopen.  See job001934. */
#pragma warning( disable : 4996 )
#endif



typedef unsigned int uint;
typedef unsigned long ulong;


static EventClock eventTime; /* current event time */


/* event counters */

typedef unsigned long eventCountArray[EventCodeMAX+1];
static unsigned long bucketEventCount[EventCodeMAX+1];
static unsigned long totalEventCount[EventCodeMAX+1];


static char *prog; /* program name */


/* command-line arguments */

static Bool verbose = FALSE;
/* style: '\0' for human-readable, 'L' for Lisp, 'C' for CDF. */
static char style = '\0';
static Bool reportStats = FALSE;
static Bool eventEnabled[EventCodeMAX+1];
static Word bucketSize = 0;


/* everror -- error signalling */

static void everror(const char *format, ...)
{
  va_list args;

  fflush(stdout); /* sync */
  fprintf(stderr, "%s: @", prog);
  EVENT_CLOCK_PRINT(stderr, eventTime);
  va_start(args, format);
  vfprintf(stderr, format, args);
  fprintf(stderr, "\n");
  va_end(args);
  exit(EXIT_FAILURE);
}


/* usage -- usage message */

static void usage(void)
{
  fprintf(stderr,
          "Usage: %s [-f logfile] [-p] [-v] [-e events] [-b size]"
          " [-S[LC]] [-?]\nSee guide.mps.telemetry for instructions.\n",
          prog);
}


/* usageError -- explain usage and error */

static void usageError(void)
{
  usage();
  everror("Bad usage");
}


/* parseEventSpec -- parses an event spec
 *
 * The spec is of the form: <name>[(+|-)<name>]...
 * The first name can be 'all'.
 */

static void parseEventSpec(const char *arg)
{
  size_t arglen;
  EventCode i;
  const char *end;
  char name[EventNameMAX+1];
  Bool enabled = TRUE;

  end = arg + strlen(arg);
  for(i = 0; i <= EventCodeMAX; ++i)
    eventEnabled[i] = FALSE;
  do {
    arglen = strcspn(arg, "+-");
    strncpy(name, arg, arglen); name[arglen] = '\0';
    if (strcmp(name, "all") == 0) {
      for(i = 0; i <= EventCodeMAX; ++i)
        eventEnabled[i] = EventCodeIsValid(i);
    } else
      eventEnabled[EventName2Code(name)] = enabled;
    enabled = (arg[arglen] == '+'); arg += arglen + 1;
  } while (arg < end);
}


/* parseArgs -- parse command line arguments, return log file name */

static char *parseArgs(int argc, char *argv[])
{
  char *name = "mpsio.log";
  int i = 1;

  if (argc >= 1)
    prog = argv[0];
  else
    prog = "unknown";

  while (i < argc) { /* consider argument i */
    if (argv[i][0] == '-') { /* it's an option argument */
      switch (argv[i][1]) {
      case 'f': /* file name */
        ++ i;
        if (i == argc)
          usageError();
        else
          name = argv[i];
        break;
      case 'v': /* verbosity */
        verbose = TRUE;
        break;
      case 'e': { /* event statistics */
        reportStats = TRUE;
        ++ i;
        if (i == argc)
          usageError();
        else
          parseEventSpec(argv[i]);
      } break;
      case 'b': { /* bucket size */
        ++ i;
        if (i == argc)
          usageError();
        else {
          int n;

          n = sscanf(argv[i], "%lu", &bucketSize);
          if (n != 1) usageError();
        }
      } break;
      case 'S': /* style */
        style = argv[i][2]; /* '\0' for human-readable, 'L' for Lisp, */
        break;              /* 'C' for CDF. */
      case '?': case 'h': /* help */
        usage();
        break;
      default:
        usageError();
      }
    } /* if option */
    ++ i;
  }
  return name;
}


/* recordEvent -- record event
 *
 * This is the beginning of a system to model MPS state as events are read,
 * but for the moment it just records which strings have been interned
 * and which addresses have been labelled with them.
 *
 * NOTE: Since branch/2012-08-21/diagnostic-telemetry events are no longer
 * in order in the event stream, so eventcnv would need some serious
 * rethinking to model state.  It's questionable that it should attempt it
 * or event try to label addresses, but instead leave that to later stages of
 * processing.  RB 2012-09-07
 */

static void recordEvent(EventProc proc, Event event, EventClock etime)
{
  Res res;

  res = EventRecord(proc, event, etime);
  if (res != ResOK)
    everror("Can't record event: error %d.", res);
  switch(event->any.code) {
  default:
    break;
  }
}


/* Printing routines */


/* printStr -- print an EventString */

static void printStr(const char *str, Bool quotes)
{
  size_t i;

  if (quotes) putchar('"');
  for (i = 0; str[i] != '\0'; ++i) {
    char c = str[i];
    if (quotes && (c == '"' || c == '\\')) putchar('\\');
    putchar(c);
  }
  if (quotes) putchar('"');
}


/* printAddr -- print an Addr or its label */

static void printAddr(EventProc proc, Addr addr)
{
  Word label;

  label = AddrLabel(proc, addr);
  if (label != 0 && addr != 0) {
    /* We assume labelling zero is meant to record a point in time */
    const char *sym = LabelText(proc, label);
    if (sym != NULL) {
      putchar(' ');
      printStr(sym, (style == 'C'));
    } else {
      printf((style == '\0') ?
             " sym%05"PRIXLONGEST :
             " \"sym %"PRIXLONGEST"\"",
             (ulongest_t)label);
    }
  } else
    printf(style != 'C' ?
           " %0"PRIwWORD PRIXLONGEST :
           " %"PRIuLONGEST,
           (ulongest_t)addr);
}


/* reportEventResults -- report event counts from a count array */

static void reportEventResults(eventCountArray eventCounts)
{
  EventCode i;
  unsigned long total = 0;

  for(i = 0; i <= EventCodeMAX; ++i) {
    total += eventCounts[i];
    if (eventEnabled[i])
      switch (style) {
      case '\0':
        printf(" %5lu", eventCounts[i]);
        break;
      case 'L':
        printf(" %lX", eventCounts[i]);
        break;
      case 'C':
        printf(", %lu", eventCounts[i]);
        break;
      }
  }
  switch (style) {
  case '\0':
    printf(" %5lu\n", total);
    break;
  case 'L':
    printf(" %lX)\n", total);
    break;
  case 'C':
    printf(", %lu\n", total);
    break;
  }
}


/* reportBucketResults -- report results of the current bucket */

static void reportBucketResults(EventClock bucketLimit)
{
  switch (style) {
  case '\0':
    EVENT_CLOCK_PRINT(stdout, bucketLimit);
    putchar(':');
    break;
  case 'L':
    putchar('(');
    EVENT_CLOCK_PRINT(stdout, bucketLimit);
    break;
  case 'C':
    EVENT_CLOCK_PRINT(stdout, bucketLimit);
    break;
  }
  if (reportStats) {
    reportEventResults(bucketEventCount);
  }
}


/* clearBucket -- clear bucket */

static void clearBucket(void)
{
  EventCode i;

  for(i = 0; i <= EventCodeMAX; ++i)
    bucketEventCount[i] = 0;
}


/* printParam* -- printing functions for event parameter types */

static void printParamA(EventProc proc, char *styleConv, Addr addr)
{
  if (style != 'L') {
    if (style == 'C') putchar(',');
    printAddr(proc, addr);
  } else
    printf(styleConv, (ulongest_t)addr);
}

static void printParamP(EventProc proc, char *styleConv, void *p)
{
  UNUSED(proc);
  printf(styleConv, (ulongest_t)p);
}

static void printParamU(EventProc proc, char *styleConv, unsigned u)
{
  UNUSED(proc);
  printf(styleConv, (ulongest_t)u);
}

static void printParamW(EventProc proc, char *styleConv, Word w)
{
  UNUSED(proc);
  printf(styleConv, (ulongest_t)w);
}

static void printParamD(EventProc proc, char *styleConv, double d)
{
  UNUSED(proc);
  UNUSED(styleConv);
  switch (style) {
  case '\0':
    printf(" %#8.3g", d); break;
  case 'C':
    printf(", %.10G", d); break;
  case 'L':
    printf(" %#.10G", d); break;
  }
}

static void printParamS(EventProc proc, char *styleConv, const char *s)
{
  UNUSED(proc);
  UNUSED(styleConv);
  if (style == 'C') putchar(',');
  putchar(' ');
  printStr(s, (style == 'C' || style == 'L'));
}

static void printParamB(EventProc proc, char *styleConv, Bool b)
{
  UNUSED(proc);
  UNUSED(proc);
  printf(styleConv, (ulongest_t)b);
}


/* readLog -- read and parse log
 *
 * This is the heart of eventcnv: It reads an event log using EventRead.
 * It updates the counters.  If verbose is true, it looks up the format,
 * parses the arguments, and prints a representation of the event.  Each
 * argument is printed using printArg (see RELATION, below), except for
 * some event types that are handled specially.
 */

static void readLog(EventProc proc)
{
  EventCode c;
  Word bucketLimit = bucketSize;
  char *styleConv = NULL; /* suppress uninit warning */

  /* Print event count header. */
  if (reportStats) {
    if (style == '\0') {
      printf("  bucket:");
      for(c = 0; c <= EventCodeMAX; ++c)
        if (eventEnabled[c])
          printf("  %04X", (unsigned)c);
      printf("   all\n");
    }
  }

  /* Init event counts. */
  for(c = 0; c <= EventCodeMAX; ++c)
    totalEventCount[c] = 0;
  clearBucket();

  /* Init style. */
  switch (style) {
  case '\0':
    styleConv = " %8"PRIXLONGEST; break;
  case 'C':
    styleConv = ", %"PRIuLONGEST; break;
  case 'L':
    styleConv = " %"PRIXLONGEST; break;
  default:
    everror("Unknown style code '%c'", style);
  }

  while (TRUE) { /* loop for each event */
    Event event;
    EventCode code;
    Res res;

    /* Read and parse event. */
    res = EventRead(&event, proc);
    if (res == ResFAIL) break; /* eof */
    if (res != ResOK) everror("Truncated log");
    eventTime = event->any.clock;
    code = event->any.code;

    /* Output bucket, if necessary, and update counters */
    if (bucketSize != 0 && eventTime >= bucketLimit) {
      reportBucketResults(bucketLimit-1);
      clearBucket();
      do {
        bucketLimit += bucketSize;
      } while (eventTime >= bucketLimit);
    }
    if (reportStats) {
      ++bucketEventCount[code];
      ++totalEventCount[code];
    }

    /* Output event. */
    if (verbose) {
      if (style == 'L') putchar('(');

      switch (style) {
      case '\0': case 'L':
        EVENT_CLOCK_PRINT(stdout, eventTime);
        putchar(' ');
        break;
      case 'C':
        EVENT_CLOCK_PRINT(stdout, eventTime);
        fputs(", ", stdout);
        break;
      }

      switch (style) {
      case '\0': case 'L': {
        printf("%-19s ", EventCode2Name(code));
      } break;
      case 'C':
        printf("%u", (unsigned)code);
        break;
      }

     switch (code) {

     case EventLabelCode:
       switch (style) {
       case '\0':
         {
           const char *sym = LabelText(proc, event->Label.f1);
           printf(style == '\0' ?
                  " %08"PRIXLONGEST" " :
                  ", %"PRIuLONGEST", ",
                  (ulongest_t)event->Label.f0);
           if (sym != NULL) {
                   printStr(sym, 0);
           } else {
                   printf("sym %05"PRIXLONGEST ,
                          (ulongest_t)event->Label.f1);
           }
         }
         break;
       case 'L':
         printf(" %"PRIXLONGEST" %"PRIXLONGEST,
                (ulongest_t)event->Label.f0,
                (ulongest_t)event->Label.f1);
         break;
       case 'C':
         printf(", %"PRIuLONGEST", %"PRIuLONGEST,
                (ulongest_t)event->Label.f0,
                (ulongest_t)event->Label.f1);
         break;
       }
       break;

     case EventMeterValuesCode:
       switch (style) {
       case '\0':
         if (event->MeterValues.f3 == 0) {
           printf(" %08"PRIXLONGEST"        0      N/A      N/A      N/A      N/A",
                  (ulongest_t)event->MeterValues.f0);
         } else {
           double mean = event->MeterValues.f1 / (double)event->MeterValues.f3;
           /* .stddev: stddev = sqrt(meanSquared - mean^2), but see */
           /* <code/meter.c#limitation.variance>. */
           double stddev = sqrt(fabs(event->MeterValues.f2
                                     - (mean * mean)));
           printf(" %08"PRIXLONGEST" %8u %8u %8u %#8.3g %#8.3g",
                  (ulongest_t)event->MeterValues.f0, (uint)event->MeterValues.f3,
                  (uint)event->MeterValues.f4, (uint)event->MeterValues.f5,
                  mean, stddev);
         }
         printAddr(proc, (Addr)event->MeterValues.f0);
         break;

       case 'C':
         putchar(',');
         printAddr(proc, (Addr)event->MeterValues.f0);
         printf(", %.10G, %.10G, %u, %u, %u",
                event->MeterValues.f1, event->MeterValues.f2,
                (uint)event->MeterValues.f3, (uint)event->MeterValues.f4,
                (uint)event->MeterValues.f5);
         break;

       case 'L':
         printf(" %"PRIXLONGEST" %#.10G %#.10G %X %X %X",
                (ulongest_t)event->MeterValues.f0,
                event->MeterValues.f1, event->MeterValues.f2,
                (uint)event->MeterValues.f3, (uint)event->MeterValues.f4,
                (uint)event->MeterValues.f5);
         break;
       }
       break;

     case EventPoolInitCode: /* pool, arena, class */
       printf(styleConv, (ulongest_t)event->PoolInit.f0);
       printf(styleConv, (ulongest_t)event->PoolInit.f1);
       /* class is a Pointer, but we label them, so call printAddr */
       if (style != 'L') {
         if (style == 'C') putchar(',');
         printAddr(proc, (Addr)event->PoolInit.f2);
       } else
         printf(styleConv, (ulongest_t)event->PoolInit.f2);
       break;

     default:
#define EVENT_PARAM_PRINT(name, index, sort, ident) \
         printParam##sort(proc, styleConv, event->name.f##index);
#define EVENT_PRINT(X, name, code, always, kind) \
       case code: \
         EVENT_##name##_PARAMS(EVENT_PARAM_PRINT, name) \
         break;
       switch (code) { EVENT_LIST(EVENT_PRINT, X) }
     }

      if (style == 'L') putchar(')');
      putchar('\n');
      fflush(stdout);
    }
    recordEvent(proc, event, eventTime);
    EventDestroy(proc, event);
  } /* while(!feof(input)) */

  /* report last bucket (partial) */
  if (bucketSize != 0) {
    reportBucketResults(eventTime);
  }
  if (reportStats) {
    /* report totals */
    switch (style) {
    case '\0':
      printf("\n     run:");
      break;
    case 'L':
      printf("(t");
      break;
    case 'C':
      {
        /* FIXME: This attempted to print the event stats on a row that
           resembled a kind of final event, but the event clock no longer runs
           monotonically upwards. */
        EventClock last = eventTime + 1;
        EVENT_CLOCK_PRINT(stdout, last);
      }
      break;
    }
    reportEventResults(totalEventCount);

    /* explain event codes */
    if (style == '\0') {
      printf("\n");
      for(c = 0; c <= EventCodeMAX; ++c)
        if (eventEnabled[c])
          printf(" %04X %s\n", (unsigned)c, EventCode2Name(c));
      if (bucketSize == 0)
        printf("\nevent clock stopped at ");
        EVENT_CLOCK_PRINT(stdout, eventTime);
        printf("\n");
    }
  }
}


/* logReader -- reader function for a file log */

static FILE *input;

static Res logReader(void *file, void *p, size_t len)
{
  size_t n;

  n = fread(p, 1, len, (FILE *)file);
  return (n < len) ? (feof((FILE *)file) ? ResFAIL : ResIO) : ResOK;
}


/* CHECKCONV -- check t2 can be cast to t1 without loss */

#define CHECKCONV(t1, t2) \
  (sizeof(t1) >= sizeof(t2))


/* main */

int main(int argc, char *argv[])
{
  char *filename;
  EventProc proc;
  Res res;

  assert(CHECKCONV(ulongest_t, Word));
  assert(CHECKCONV(ulongest_t, Addr));
  assert(CHECKCONV(ulongest_t, void *));
  assert(CHECKCONV(ulongest_t, EventCode));
  assert(CHECKCONV(Addr, void *)); /* for labelled pointers */

  filename = parseArgs(argc, argv);

  if (strcmp(filename, "-") == 0)
    input = stdin;
  else {
    input = fopen(filename, "rb");
    if (input == NULL)
      everror("unable to open \"%s\"\n", filename);
  }

  res = EventProcCreate(&proc, logReader, (void *)input);
  if (res != ResOK)
    everror("Can't init EventProc module: error %d.", res);

  readLog(proc);

  EventProcDestroy(proc);
  return EXIT_SUCCESS;
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

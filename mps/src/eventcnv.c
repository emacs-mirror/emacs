/* impl.c.eventcnv: Simple event log converter
 * Copyright (C) 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName: MMsrc!eventcnv.c(trunk.1) $
 */

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "eventcom.h"
#include "eventpro.h"
#include "misc.h"
#include "mpmtypes.h"

#include <stddef.h> /* for size_t */
#include <stdio.h> /* for printf */
#include <stdarg.h> /* for va_list */
#include <stdlib.h> /* for EXIT_FAILURE */
#include <assert.h> /* for assert */
#include <string.h> /* for strcmp */
#include <math.h> /* for sqrt */
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


typedef unsigned int uint;
typedef unsigned long ulong;


static Word eventTime; /* current event time */


/* event counters */

typedef unsigned long eventCountArray[EventCodeMAX];
static unsigned long bucketEventCount[EventCodeMAX];
static unsigned long totalEventCount[EventCodeMAX];


static char *prog; /* program name */


/* command-line arguments */

static Bool verbose = FALSE;
/* style: '\0' for human-readable, 'L' for Lisp, 'C' for CDF. */
static char style = '\0';
static Bool reportEvents = FALSE;
static Bool eventEnabled[EventCodeMAX];
static Bool partialLog = FALSE;
static Word bucketSize = 0;


/* error -- error signalling */

static void error(const char *format, ...)
{
  va_list args;

  fflush(stdout); /* sync */
  fprintf(stderr, "%s: ", prog);
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
          "Usage: %s [-f logfile] [-^] [-p] [-v] [-e events] [-b size]"
          " [-S[LC]] [-?]\nSee guide.mps.telemetry for instructions.",
          prog);
}


/* usageError -- explain usage and error */

static void usageError(void)
{
  usage();
  error("Bad usage");
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
  for(i = 0; i < EventCodeMAX; ++i)
    eventEnabled[i] = FALSE;
  do {
    arglen = strcspn(arg, "+-");
    strncpy(name, arg, arglen); name[arglen] = '\0';
    if (strcmp(name, "all") == 0) {
      for(i = 0; i < EventCodeMAX; ++i)
        eventEnabled[i] = TRUE;
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
      case 'p': /* partial log */
        partialLog = TRUE;
        break;
      case 'v': /* verbosity */
        verbose = TRUE;
        break;
      case 'e': { /* event statistics */
        reportEvents = TRUE;
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


/* processEvent -- process event */

static void processEvent(Event event, Word etime)
{
  EventRecord(event, etime);
  switch(event->any.code) {
  case EventArenaCreate:	/* arena */
    break;
  case EventArenaDestroy:	/* arena */
    break;
  default: 
    break;
  }
}


/* Printing routines */


/* printStr -- print an EventString */

static void printStr(EventString str, Bool quotes)
{
  size_t i, len;

  if (quotes) putchar('"');
  len = str->len;
  for (i = 0; i < len; ++ i) {
    char c = str->str[i];
    if (quotes && (c == '"' || c == '\\')) putchar('\\');
    putchar(c);
  }
  if (quotes) putchar('"');
}


/* printAddr -- print an Addr or its label */

static void printAddr(Addr addr)
{
  Word label;

  label = AddrLabel(addr);
  if (label != 0 && addr != 0) {
    /* We assume labelling zero is meant to record a point in time */
    EventString sym = LabelText(label);
    if (sym != NULL) {
      putchar(' ');
      printStr(sym, (style == 'C'));
    } else {
      printf((style == '\0') ? " sym%05lX" : " \"sym %lX\"",
             (ulong)label);
    }
  } else
    printf((style != 'C') ? " %08lX" : " %lu", (ulong)addr);
}


/* reportEventResults -- report event counts from a count array */

static void reportEventResults(eventCountArray eventCounts)
{
  EventCode i;
  unsigned long total = 0;
  
  for(i = 0; i < EventCodeMAX; ++i) {
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

static void reportBucketResults(Word bucketLimit)
{
  switch (style) {
  case '\0':
    printf("%8lu:", (ulong)bucketLimit);
    break;
  case 'L':
    printf("(%lX", (ulong)bucketLimit);
    break;
  case 'C':
    printf("%lu", (ulong)bucketLimit);
    break;
  }
  if (reportEvents) {
    reportEventResults(bucketEventCount);
  }
}


/* clearBucket -- clear bucket */

static void clearBucket(void)
{
  EventCode i;

  for(i = 0; i < EventCodeMAX; ++i)
    bucketEventCount[i] = 0;
}


/* readLog -- read and parse log
 *
 * This is the heart of eventcnv: It reads an event log using EventRead.
 * It updates the counters.  If verbose is true, it looks up the format,
 * parses the arguments, and prints a representation of the event.  Each
 * argument is printed using printArg (see RELATION, below), except for
 * some event types that are handled specially.
 */

static void printArg(void *arg, char argType, char *styleConv)
{
  switch (argType) {
  case 'A': {
    if (style != 'L') {
      if (style == 'C') putchar(',');
      printAddr(*(Addr *)arg);
    } else
      printf(styleConv, (ulong)*(Addr *)arg);
  } break;
  case 'P': {
    printf(styleConv, (ulong)*(void **)arg);
  } break;
  case 'U': {
    printf(styleConv, (ulong)*(unsigned *)arg);
  } break;
  case 'W': {
    printf(styleConv, (ulong)*(Word *)arg);
  } break;
  case 'D': {
    switch (style) {
    case '\0':
      printf(" %#8.3g", *(double *)arg); break;
    case 'C':
      printf(", %.10G", *(double *)arg); break;
    case 'L':
      printf(" %#.10G", *(double *)arg); break;
    }
  } break;
  case 'S': {
    if (style == 'C') putchar(',');
    putchar(' ');
    printStr((EventStringStruct *)arg, (style == 'C' || style == 'L'));
  } break;
  default: error("Can't print format >%c<", argType);
  }
}


#define RELATION(name, code, always, kind, format) \
  case code: { \
    printArg(EVENT_##format##_FIELD_PTR(event, i), \
             eventFormat[i], styleConv); \
  } break;


static Res readLog(char *filename)
{
  FILE *input;
  Word bucketLimit = bucketSize;
  char *styleConv = NULL; /* suppress uninit warning */

  if (strcmp(filename, "-") == 0)
    input = stdin;
  else {
    input = fopen(filename, "rb");
    if (input == NULL)
      error("unable to open \"%s\"\n", filename);
  }

  clearBucket();

  switch (style) {
  case '\0':
    styleConv = " %8lX"; break;
  case 'C':
    styleConv = ", %lu"; break;
  case 'L':
    styleConv = " %lX"; break;
  default:
    error("Unknown style code '%c'", style);
  }

  while (TRUE) {
    char *eventFormat;
    int argCount, i;
    Event event;
    EventCode code;
    Res res;

    res = EventRead(&event, input);
    if (res != ResOK) error("Truncated file");
    if (event == NULL) break;
    eventTime = event->any.clock;
    code = EventGetCode(event);

    if (bucketSize != 0 && eventTime >= bucketLimit) {
      reportBucketResults(bucketLimit-1);
      clearBucket();
      do {
        bucketLimit += bucketSize;
      } while (eventTime >= bucketLimit);
    }
    if (reportEvents) {
      ++bucketEventCount[code];
      ++totalEventCount[code];
    }

    if (verbose) {
      eventFormat = EventCode2Format(code);
      argCount = strlen(eventFormat);
      if (eventFormat[0] == '0') argCount = 0;
      assert(argCount < eventArgsMAX);

      if (style == 'L') putchar('(');

      switch (style) {
      case '\0': case 'L': {
        printf("%-19s", EventCode2Name(code));
      } break;
      case 'C':
        printf("%lu", (ulong)code);
        break;
      }
 
     switch (style) {
     case '\0':
       printf(" %8lu", (ulong)eventTime); break;
     case 'C':
       printf(", %lu", (ulong)eventTime); break;
     case 'L':
       printf(" %lX", (ulong)eventTime); break;
     }

     switch (event->any.code) {
     case EventLabel: {
       switch (style) {
       case '\0': case 'C': {
         EventString sym = LabelText(event->aw.w1);
         printf((style == '\0') ? " %08lX " : ", %lu, ",
                (ulong)event->aw.a0);
         if (sym != NULL) {
           printStr(sym, (style == 'C'));
         } else {
           printf((style == '\0') ? "sym %05lX" : "sym %lX\"",
                  (ulong)event->aw.w1);
         }
       } break;
       case 'L': {
         printf(" %lX %lX", (ulong)event->aw.a0, (ulong)event->aw.w1);
       } break;
       }
     } break;
     case EventMeterValues: {
       switch (style) {
       case '\0': {
         if (event->pddwww.w3 == 0) {
           printf(" %08lX        0      N/A      N/A      N/A      N/A",
                  (ulong)event->pddwww.p0);
         } else {
           double mean = event->pddwww.d1 / (double)event->pddwww.w3;
           /* .stddev: stddev = sqrt(meanSquared - mean^2), but see */
           /* impl.c.meter.limitation.variance. */
           double stddev = sqrt(fabs(event->pddwww.d2
                                     - (mean * mean)));
           printf(" %08lX %8u %8u %8u %#8.3g %#8.3g",
                  (ulong)event->pddwww.p0, (uint)event->pddwww.w3,
                  (uint)event->pddwww.w4, (uint)event->pddwww.w5,
                  mean, stddev);
         }
         printAddr((Addr)event->pddwww.p0);
       } break;
       case 'C': {
         putchar(',');
         printAddr((Addr)event->pddwww.p0);
         printf(", %.10G, %.10G, %u, %u, %u",
                event->pddwww.d1, event->pddwww.d2,
                (uint)event->pddwww.w3, (uint)event->pddwww.w4,
                (uint)event->pddwww.w5);
       } break;
       case 'L': {
         printf(" %lX %#.10G %#.10G %X %X %X", (ulong)event->pddwww.p0,
                event->pddwww.d1, event->pddwww.d2,
                (uint)event->pddwww.w3, (uint)event->pddwww.w4,
                (uint)event->pddwww.w5);
       } break;
       }
     } break;
     case EventPoolInit: { /* pool, arena, class */
       printf(styleConv, (ulong)event->ppp.p0);
       printf(styleConv, (ulong)event->ppp.p1);
       /* class is a Pointer, but we label them, so call printAddr */
       if (style != 'L') {
         if (style == 'C') putchar(',');
         printAddr((Addr)event->ppp.p2);
       } else
         printf(styleConv, (ulong)event->ppp.p2);
     } break;
     default:
       for (i = 0; i < argCount; ++i) {
         switch(code) {
#include "eventdef.h"
#undef RELATION
         }
       }
     }

      if (style == 'L') putchar(')');
      putchar('\n');
      fflush(stdout);
    }
    processEvent(event, eventTime);
    EventDestroy(event);
  } /* while(!feof(input)) */

  if (bucketSize != 0) {
    reportBucketResults(eventTime);
  }
  return ResOK;
}


/* main */

int main(int argc, char *argv[])
{
  char *name;
  Res result;
  EventCode i;

  name = parseArgs(argc,argv);

  EventProcInit(partialLog);

  for(i = 0; i < EventCodeMAX; ++i)
    totalEventCount[i] = 0;
  if (reportEvents) {
    if (style == '\0') {
      printf("  bucket:");
      for(i = 0; i < EventCodeMAX; ++i)
        if (eventEnabled[i])
          printf("  %04X", (unsigned)i);
      printf("   all\n");
    }
  }

  result = readLog(name);
  switch(result) {
  case ResOK:
    break;   
  case ResIO:
    printf("Log file prematurely terminated.\n");
    break;
  default:
    printf("Mystery error.\n");
  }

  if (reportEvents) {
    switch (style) {
    case '\0': {
      printf("\n     run:");
      reportEventResults(totalEventCount);
      printf("\n");
      for(i = 0; i < EventCodeMAX; ++i)
        if (eventEnabled[i])
          printf(" %04X %s\n", (unsigned)i, EventCode2Name(i));
      if (bucketSize == 0)
        printf("\nevent clock stopped at %lu\n", (ulong)eventTime);
    } break;
    case 'L': {
      printf("(t");
      reportEventResults(totalEventCount);
    } break;
    case 'C': {
      printf("%lu", eventTime+1);
      reportEventResults(totalEventCount);
    } break;
    }
  }

  EventProcFinish();
  return EXIT_SUCCESS;
}

/* eventcnv.c: Simple event log converter
 * Copyright (c) 2001-2018 Ravenbrook Limited.  See end of file for license.
 *
 * This is a command-line tool that converts a binary format telemetry output
 * stream from the MPS into a more-portable textual format.
 *
 * eventcnv can only read binary-format files that come from an MPS
 * compiled on the same platform, whereas the text-format files it
 * produces can be processed on any platform.
 *
 * The default MPS library will write a telemetry stream to a file
 * when the environment variable MPS_TELEMETRY_CONTROL is set to an
 * integer whose bits select event kinds.  For example:
 *
 *   MPS_TELEMETRY_CONTROL=7 amcss
 *
 * will run the amcss test program and emit a telemetry file with
 * event kinds 0, 1, 2.  The file can then be converted into a sorted
 * text format log with a command like:
 *
 *   eventcnv | sort > mps-events.txt
 *
 * These text-format files have one line per event, and can be
 * manipulated by various programs systems in the usual Unix way.
 *
 * The binary telemetry filename can be specified with a -f
 * command-line argument (use -f - to specify standard input).  If no
 * filename is specified on the command line, the environment variable
 * MPS_TELEMETRY_FILENAME is consulted (this is the same environment
 * variable used to specify the telemetry file to the MPS library).
 * If the environment variable does not exist, the default filename of
 * "mpsio.log" is used.
 *
 * $Id$
 */

#include "config.h"
#include "eventdef.h"
#include "eventcom.h"
#include "testlib.h" /* for ulongest_t and associated print formats */

#include <stddef.h> /* for size_t */
#include <stdio.h> /* for printf */
#include <stdlib.h> /* for EXIT_FAILURE */
#include <assert.h> /* for assert */
#include <string.h> /* for strcmp */
#include "mpstd.h"

#define DEFAULT_TELEMETRY_FILENAME "mpsio.log"
#define TELEMETRY_FILENAME_ENVAR   "MPS_TELEMETRY_FILENAME"

static EventClock eventTime; /* current event time */
static const char *prog; /* program name */

/* Errors and Warnings */

/* fevwarn -- flush stdout, write message to stderr */

ATTRIBUTE_FORMAT((printf, 2, 0))
static void fevwarn(const char *prefix, const char *format, va_list args)
{
  (void)fflush(stdout); /* sync */
  (void)fprintf(stderr, "%s: %s @", prog, prefix);
  (void)EVENT_CLOCK_PRINT(stderr, eventTime);
  (void)fprintf(stderr, " ");
  (void)vfprintf(stderr, format, args);
  (void)fprintf(stderr, "\n");
}

/* evwarn -- flush stdout, warn to stderr */

ATTRIBUTE_FORMAT((printf, 1, 2))
static void evwarn(const char *format, ...)
{
  va_list args;

  va_start(args, format);
  fevwarn("Warning", format, args);
  va_end(args);
}

/* everror -- flush stdout, message to stderr, exit */

ATTRIBUTE_FORMAT((printf, 1, 2))
static void everror(const char *format, ...)
{
  va_list args;

  va_start(args, format);
  fevwarn("Error", format, args);
  va_end(args);
  exit(EXIT_FAILURE);
}


/* usage -- usage message */

static void usage(void)
{
  (void)fprintf(stderr, "Usage: %s [-f logfile] [-h]\n"
                "See \"Telemetry\" in the reference manual for instructions.\n",
                prog);
}


/* usageError -- explain usage and error */

static void usageError(void)
{
  usage();
  everror("Bad usage");
}


/* parseArgs -- parse command line arguments, return log file name */

static char *parseArgs(int argc, char *argv[])
{
  char *name = NULL;
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
      case '?': case 'h': /* help */
        usage();
        exit(EXIT_SUCCESS);
      default:
        usageError();
      }
    } /* if option */
    ++ i;
  }
  return name;
}


/* Printing routines */

static void printHex(ulongest_t val)
{
  printf(" %"PRIXLONGEST, (ulongest_t)val);
}

#define printParamP(p) printHex((ulongest_t)p)
#define printParamA(a) printHex((ulongest_t)a)
#define printParamU(u) printHex((ulongest_t)u)
#define printParamW(w) printHex((ulongest_t)w)
#define printParamB(b) printHex((ulongest_t)b)

static void printParamD(double d)
{
  printf(" %.10G", d);
}

static void printParamS(const char *str)
{
  size_t i;
  putchar(' ');
  putchar('"');
  for (i = 0; str[i] != '\0'; ++i) {
    char c = str[i];
    if (c == '"' || c == '\\')
      putchar('\\');
    putchar(c);
  }
  putchar('"');
}


/* EventRead -- read one event from the file */

static Res eventRead(Bool *eofOut, EventUnion *event, FILE *stream)
{
  size_t n;
  size_t rest;

  /* Read the prefix common to all event structures, in order to decode the
     event size. */
  n = fread(&event->any, sizeof(event->any), 1, stream);
  if (n < 1) {
    if (feof(stream)) {
      *eofOut = TRUE;
      return ResOK;
    }
    return ResIO;
  }

  if (event->any.size < sizeof(event->any))
    return ResFAIL; /* invalid size: too small */

  if (event->any.size > sizeof(*event))
    return ResFAIL; /* invalid size: too large */

  /* Read the rest of the event. */
  rest = event->any.size - sizeof(event->any);
  if (rest > 0) {
    n = fread((char *)event + sizeof(event->any), rest, 1, stream);
    if (n < 1) {
      if (feof(stream))
        return ResFAIL; /* truncated event */
      else
        return ResIO;
    }
  }

  *eofOut = FALSE;
  return ResOK;
}

/* readLog -- read and parse log */

static void readLog(FILE *stream)
{
  for(;;) { /* loop for each event */
    EventUnion eventUnion;
    Event event = &eventUnion;
    EventCode code;
    Res res;
    Bool eof = FALSE; /* suppress warnings about uninitialized use */

    /* Read and parse event. */
    res = eventRead(&eof, event, stream);
    if (res == ResFAIL)
      everror("Truncated log");
    else if (res == ResIO)
      everror("I/O error reading log");
    else if (res != ResOK)
      everror("Unknown error reading log");
    if (eof)
      break;

    eventTime = event->any.clock;
    code = event->any.code;

    /* Special handling for some events, prior to text output */

    switch(code) {
    case EventEventInitCode:
      if ((event->EventInit.f0 != EVENT_VERSION_MAJOR) ||
          (event->EventInit.f1 != EVENT_VERSION_MEDIAN) ||
          (event->EventInit.f2 != EVENT_VERSION_MINOR))
        evwarn("Event log version does not match: %d.%d.%d vs %d.%d.%d",
               event->EventInit.f0,
               event->EventInit.f1,
               event->EventInit.f2,
               EVENT_VERSION_MAJOR,
               EVENT_VERSION_MEDIAN,
               EVENT_VERSION_MINOR);

      if (event->EventInit.f3 > EventCodeMAX)
        evwarn("Event log may contain unknown events with codes from %d to %d",
               EventCodeMAX+1, event->EventInit.f3);

      if (event->EventInit.f5 != MPS_WORD_WIDTH)
        /* This probably can't happen; other things will break
         * before we get here */
        evwarn("Event log has incompatible word width: %d instead of %d",
               event->EventInit.f5,
               MPS_WORD_WIDTH);
      break;
    default:
      /* No special treatment needed. */
      break;
    }

    (void)EVENT_CLOCK_PRINT(stdout, eventTime);
    printf(" %4X", (unsigned)code);

    switch (code) {
#define EVENT_PARAM_PRINT(name, index, sort, ident, doc) \
      printParam##sort(event->name.f##index);
#define EVENT_PRINT(X, name, code, used, kind)          \
      case code:                                        \
        EVENT_##name##_PARAMS(EVENT_PARAM_PRINT, name)  \
        break;
      EVENT_LIST(EVENT_PRINT, X)
    default:
      evwarn("Unknown event code %d", code);
    }

    putchar('\n');
    (void)fflush(stdout);
  } /* while(!feof(input)) */
}


/* CHECKCONV -- check t2 can be cast to t1 without loss */

#define CHECKCONV(t1, t2) \
  (sizeof(t1) >= sizeof(t2))


/* main */

int main(int argc, char *argv[])
{
  const char *filename;
  FILE *input;

  assert(CHECKCONV(ulongest_t, Word));
  assert(CHECKCONV(ulongest_t, Addr));
  assert(CHECKCONV(ulongest_t, void *));
  assert(CHECKCONV(ulongest_t, EventCode));

  filename = parseArgs(argc, argv);
  if (!filename) {
    filename = getenv(TELEMETRY_FILENAME_ENVAR);
    if(!filename)
      filename = DEFAULT_TELEMETRY_FILENAME;
  }

  if (strcmp(filename, "-") == 0)
    input = stdin;
  else {
    input = fopen(filename, "rb");
    if (input == NULL)
      everror("unable to open \"%s\"\n", filename);
  }

  readLog(input);

  return EXIT_SUCCESS;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

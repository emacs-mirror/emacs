/* impl.c.replay: Allocation replayer
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 *
 * $HopeName: MMsrc!replay.c(MMdevel_alloc_replay.3) $
 */

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "eventcom.h"
#include "eventpro.h"
#include "eventrep.h"
#include "mpmtypes.h"

#include <stddef.h> /* for size_t */
#include <stdio.h> /* for printf */
#include <stdarg.h> /* for va_list */
#include <stdlib.h> /* for EXIT_FAILURE */
#include <string.h> /* for strcmp */
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


typedef unsigned long ulong;


/* command-line arguments */

static Bool partialLog = FALSE;

static char *prog; /* program name */


/* Globals */

static Word eventTime = 0; /* current event time */


/* error -- error signalling */

static void error(const char *format, ...)
{
  va_list args;

  fflush(stdout); /* sync */
  fprintf(stderr, "%s: @%lu ", prog, (ulong)eventTime);
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
          "Usage: %s [-f logfile] [-p] [-?]\n"
          "See guide.mps.telemetry for instructions.\n",
          prog);
}


/* usageError -- explain usage and error */

static void usageError(void)
{
  usage();
  error("Bad usage");
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


/* readLog -- read and parse log */


static void readLog(EventProc proc)
{
  while (TRUE) {
    Event event;
    Res res;

    res = EventRead(&event, proc);
    if (res == ResFAIL) break; /* eof */
    if (res != ResOK) error("Truncated log");
    eventTime = event->any.clock;
    EventRecord(proc, event, eventTime);
    EventReplay(event, eventTime);
    EventDestroy(proc, event);
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


/* main */

int main(int argc, char *argv[])
{
  char *filename;
  EventProc proc;
  Res res;

  filename = parseArgs(argc,argv);

  if (strcmp(filename, "-") == 0)
    input = stdin;
  else {
    input = fopen(filename, "rb");
    if (input == NULL)
      error("unable to open \"%s\"\n", filename);
  }

  res = EventProcCreate(&proc, partialLog, logReader, (void *)input);
  if (res != ResOK)
    error("Can't init EventProc module: error %d.", res);

  res = EventRepInit(partialLog);
  if (res != ResOK)
    error("Can't init EventRep module: error %d.", res);

  readLog(proc);

  EventRepFinish();
  EventProcDestroy(proc);
  return EXIT_SUCCESS;
}

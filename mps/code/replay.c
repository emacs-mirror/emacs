/* replay.c: Allocation replayer
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * $Id$
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

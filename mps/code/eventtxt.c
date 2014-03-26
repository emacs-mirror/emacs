/* eventtxt.c: event text log to human-friendly format.
 * 
 * $Id$
 * 
 * Copyright (c) 2012-2013 Ravenbrook Limited.  See end of file for license.
 *
 * This is a command-line tool that converts events from a text-format
 * MPS telemetry file into a more human-readable format.
 *
 * The default MPS library will write a binary-format telemetry file
 * which can be converted into a text-format file using the eventcnv
 * program (q.v.).
 *
 * For efficiency, eventcnv writes all event times, codes, and
 * parameters (apart from EventFS - strings - and EventFD -
 * floating-point) as hexadecimal strings, separated by single spaces.
 * For human-readable purposes, we'd prefer a format in which
 * parameters are named; event codes are converted to event type
 * names; integers are in decimal; booleans are 'True ' or 'False';
 * pointers, addresses, and words are in hex; and labelled addresses
 * are shown with their label strings.  This program performs that
 * conversion.
 *
 * Options:
 * 
 * -l <logfile>: Import events from the named logfile.  Defaults to
 * stdin.
 *
 * $Id$
 */

#include "config.h"
#include "eventdef.h"
#include "eventcom.h"
#include "table.h"
#include "testlib.h" /* for ulongest_t and associated print formats */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef MPS_BUILD_MV
/* MSVC warning 4996 = stdio / C runtime 'unsafe' */
/* Objects to: strncpy, sscanf, fopen.  See job001934. */
#pragma warning( disable : 4996 )
#endif

static const char *prog; /* program name */
static const char *logFileName = NULL;

/* everror -- error signalling */

ATTRIBUTE_FORMAT((printf, 1, 2))
static void everror(const char *format, ...)
{
  va_list args;

  (void)fflush(stdout); /* sync */
  (void)fprintf(stderr, "%s: ", prog);
  va_start(args, format);
  (void)vfprintf(stderr, format, args);
  (void)fprintf(stderr, "\n");
  va_end(args);
  exit(EXIT_FAILURE);
}

static void usage(void)
{
  (void)fprintf(stderr, "Usage: %s [-l <logfile>]\n", prog);
}

static void usageError(void)
{
  usage();
  everror("Bad usage");
}

/* parseArgs -- parse command line arguments */

static void parseArgs(int argc, char *argv[])
{
  int i = 1;

  if (argc >= 1)
    prog = argv[0];
  else
    prog = "unknown";

  while (i < argc) { /* consider argument i */
    if (argv[i][0] == '-') { /* it's an option argument */
      switch (argv[i][1]) {
      case 'l': /* log file name */
        ++ i;
        if (i == argc)
          usageError();
        else
          logFileName = argv[i];
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
}

/* table methods for a table of interned strings, and another of
 * labelled addresses. */

static void *tableAlloc(void *closure, size_t size)
{
  UNUSED(closure);
  return malloc(size);
}

static void tableFree(void *closure, void *p, size_t size)
{
  UNUSED(closure);
  UNUSED(size);
  free(p);
}

/* Printing routines */

/* printStr -- print an EventString */

static void printStr(const char *str)
{
  size_t i;

  putchar('"');
  for (i = 0; str[i] != '\0'; ++i) {
    char c = str[i];
    if (c == '"' || c == '\\') putchar('\\');
    putchar(c);
  }
  putchar('"');
}


/* Reading hex numbers, and doubles, and quoted-and-escaped
 * strings. */

static ulongest_t parseHex(char **pInOut)
{
  ulongest_t val;
  int i, l;
  char *p = *pInOut;

  i = sscanf(p, "%" SCNXLONGEST "%n", &val, &l);
  if (i != 1)
    everror("Couldn't read a hex number from '%s'", p);
  *pInOut = p + l;
  return val;
}

static double parseDouble(char **pInOut)
{
  double val;
  int i, l;
  char *p = *pInOut;

  i = sscanf(p, "%lg%n", &val, &l);
  if (i != 1)
    everror("Couldn't read a float from '%s'", p);
  *pInOut = p + l;
  return val;
}

/* parseString checks string syntax (opening and closing quotation
 * marks) and takes a copy (stripping escaping backslashes) into a
 * static buffer (callers must "use it or lose it"; the next
 * invocation will over-write it).  Probably not bullet-proof. */

#define MAX_STRING_LENGTH 1024

char strBuf[MAX_STRING_LENGTH];

static char *parseString(char **pInOut)
{
  char *p = *pInOut;
  char *q = strBuf;
  while(*p == ' ')
    ++p;
        
  if (*p != '"')
    everror("String has no opening quotation mark: '%s'", p);
  ++p;

  while(1) {
    if (q - strBuf >= MAX_STRING_LENGTH) {
      everror("String length exceeds %d", MAX_STRING_LENGTH);
    }
    if (*p == '\\') { /* escaped character */
      ++p;
      if (*p == '\0')
        everror("Closing NUL byte escaped by backslash.");
      *q++ = *p++;
    } else if (*p == '"') { /* end of string */
      *q = '\0';
      ++p;
      *pInOut = p;
      return strBuf;
    } else if (*p == '\0')
      everror("Unexpected closing NUL byte.");
    else
      *q++ = *p++;
  }
}

/* Event logs have interned strings (i.e. they construct a partial
 * function from non-negatie integer IDs to strings), and can label
 * addresses with intern string IDs (i.e. they construct a partial
 * function from address to string ID). We need two tables to keep
 * track of these. */

static Table internTable;      /* dictionary of intern ids to strings */

static Table labelTable;       /* dictionary of addrs to intern ids */

static void createTables(void)
{
  Res res;
  /* MPS intern IDs are serials from zero up, so we can use -1
   * and -2 as specials. */
  res = TableCreate(&internTable,
                    (size_t)1<<4,
                    tableAlloc, tableFree, NULL,
                    (Word)-1, (Word)-2); 
  if (res != ResOK)
    everror("Couldn't make intern table.");

  /* We assume that 0 and 1 are invalid as Addrs. */
  res = TableCreate(&labelTable, (size_t)1<<7,
                    tableAlloc, tableFree, NULL,
                    0, 1);
  if (res != ResOK)
    everror("Couldn't make label table.");
}

/* recordIntern -- record an interned string in the table.  a copy of
* the string from the parsed buffer into a newly-allocated block. */

static void recordIntern(char *p)
{
  ulongest_t stringId;
  char *string;
  char *copy;
  size_t len;
  Res res;
        
  stringId = parseHex(&p);
  string = parseString(&p);
  len = strlen(string);
  copy = malloc(len+1);
  if (copy == NULL)
    everror("Couldn't allocate space for a string.");
  (void)strcpy(copy, string);
  res = TableDefine(internTable, (Word)stringId, (void *)copy);
  if (res != ResOK)
    everror("Couldn't create an intern mapping.");
}

/* recordLabel records a label (an association between an address and
 * a string ID).  Note that the event log may have been generated on a
 * platform with addresses larger than Word on the current platform.
 * If that happens then we are scuppered because our Table code uses
 * Word as the key type: there's nothing we can do except detect this
 * bad case (see also the EventInit handling and warning code).
 *
 * We can and do handle the case where string IDs (which are Words on
 * the MPS platform) are larger than void* on the current platform.
 * This is probably in fact the same case, because Word should be the
 * same size as void*.  In practice, trying to analyse a log from a
 * wide platform on a narrow one (e.g. - the only case which is likely
 * to occur this decade - from a 64-bit platform on a 32-bit one) is
 * probably a bad idea and maybe doomed to failure.
 */

static void recordLabel(char *p)
{
  ulongest_t address;
  ulongest_t *stringIdP;
  Res res;
        
  address = parseHex(&p);
  if (address > (Word)-1) {
    (void)printf("label address too large!");
    return;
  }
                
  stringIdP = malloc(sizeof(ulongest_t));
  if (stringIdP == NULL)
    everror("Can't allocate space for a string's ID");
  *stringIdP = parseHex(&p);
  res = TableDefine(labelTable, (Word)address, (void *)stringIdP);
  if (res != ResOK)
    everror("Couldn't create an intern mapping.");
}

/* output code */

/* hexWordWidth is the number of characters used to output a Word
 * value in hexadecimal.  Note that what we really care about is the
 * width of a Word on the source platform, not here.  So when we see
 * an EventInit event, we update this variable to the necessary
 * width. */

static int hexWordWidth = (MPS_WORD_WIDTH+3)/4;

/* printAddr -- output a ulongest_t in hex, with the interned string
 * if the value is in the label table */

static void printAddr(ulongest_t addr, const char *ident)
{
  ulongest_t label;
  void *alias;
        
  printf("%s:%0*" PRIXLONGEST, ident, hexWordWidth, addr);
  if (TableLookup(&alias, labelTable, addr)) {
    label = *(ulongest_t*)alias;
    putchar('[');
    if (TableLookup(&alias, internTable, label))
      printStr((char *)alias);
    else
      printf("unknown label %" PRIuLONGEST, label);
    putchar(']');
  }
  putchar(' ');
}

/* parameter processing.  For each parameter we parse it and then
 * print it, preceded by its name and a colon and followed by a
 * space. */

#define processParamA(ident)          \
        val_hex = parseHex(&p);       \
        printAddr(val_hex, #ident);

#define processParamP processParamA
#define processParamW processParamA

#define processParamU(ident)          \
        val_hex = parseHex(&p);       \
        printf(#ident ":%" PRIuLONGEST " ", val_hex);

#define processParamD(ident)          \
        val_float = parseDouble(&p);  \
        printf(#ident ":%#8.3g ", val_float);

#define processParamS(ident)          \
        val_string = parseString(&p); \
        printf(#ident ":");           \
        printStr(val_string);         \
        putchar(' ');

#define processParamB(ident)          \
        val_hex = parseHex(&p);       \
        printf(#ident ":%s ", val_hex ? "True" : "False");
        
#define EVENT_PROCESS_PARAM(X, index, sort, ident) processParam##sort(ident);

#define EVENT_PROCESS(X, name, code, always, kind) \
        case code: \
                EVENT_##name##_PARAMS(EVENT_PROCESS_PARAM, X) \
        break;

/* a table of the event names */

static const char *eventName[EventCodeMAX+EventCodeMAX];

#define EVENT_SET_NAME(X, name, code, always, kind) \
        eventName[code] = #name;

/* this is overkill, at present. */

#define MAX_LOG_LINE_LENGTH 1024

/* readLog -- read and parse log.  Returns the number of events written.  */

static void readLog(FILE *input)
{
  int i;

  for (i=0; i <= EventCodeMAX; ++i)
    eventName[i] = NULL;

  EVENT_LIST(EVENT_SET_NAME, X);

  while (TRUE) { /* loop for each event */
    char line[MAX_LOG_LINE_LENGTH];
    char *p, *q;
    ulongest_t clock;
    int code;
    ulongest_t val_hex;
    double val_float;
    const char *val_string;

    p = fgets(line, MAX_LOG_LINE_LENGTH, input);
    if (!p) {
      if (feof(input))
        break;
      else
        everror("Couldn't read line from input.");
    }

    clock = parseHex(&p);
    code = (int)parseHex(&p);

    if (eventName[code])
      printf("%0*" PRIXLONGEST " %04X %-19s ", hexWordWidth, clock, code,
             eventName[code]);
    else 
      printf("%0*" PRIXLONGEST " %04X %-19s ", hexWordWidth, clock, code,
             "[Unknown]");

    q = p;

    /* for a few particular codes, we do local processing. */
    if (code == EventInternCode) {
      recordIntern(q);
    } else if (code == EventLabelCode) {
      recordLabel(q);
    } else if (code == EventEventInitCode) {
      ulongest_t major, median, minor, maxCode, maxNameLen, wordWidth, clocksPerSec;
      major = parseHex(&q);  /* EVENT_VERSION_MAJOR */
      median = parseHex(&q); /* EVENT_VERSION_MEDIAN */
      minor = parseHex(&q);  /* EVENT_VERSION_MINOR */
      maxCode = parseHex(&q); /* EventCodeMAX */
      maxNameLen = parseHex(&q); /* EventNameMAX */
      wordWidth = parseHex(&q); /* MPS_WORD_WIDTH */
      clocksPerSec = parseHex(&q); /* mps_clocks_per_sec() */
      UNUSED(clocksPerSec);
      UNUSED(maxNameLen);

      if ((major != EVENT_VERSION_MAJOR) ||
          (median != EVENT_VERSION_MEDIAN) ||
          (minor != EVENT_VERSION_MINOR)) {
        (void)fprintf(stderr, "Event log version does not match: "
                      "%d.%d.%d vs %d.%d.%d\n",
                      (int)major, (int)median, (int)minor,
                      EVENT_VERSION_MAJOR,
                      EVENT_VERSION_MEDIAN,
                      EVENT_VERSION_MINOR);
      }

      if (maxCode > EventCodeMAX) {
        (void)fprintf(stderr, "Event log may contain unknown events "
                      "with codes from %d to %d\n",
                      EventCodeMAX+1, (int)maxCode);
      }

      if (wordWidth > MPS_WORD_WIDTH) {
        int newHexWordWidth = (int)((wordWidth + 3) / 4);
        if (newHexWordWidth > hexWordWidth) {
          (void)fprintf(stderr,
                        "Event log word width is greater than on current "
                        "platform; previous values may be printed too "
                        "narrowly.\n");
        }
        hexWordWidth = newHexWordWidth;
      }
      
      if (wordWidth > sizeof(ulongest_t) * CHAR_BIT) {
        everror("Event log word width %d is too wide for the current platform.",
                (int)wordWidth);
      }
    }

    switch(code) {
      EVENT_LIST(EVENT_PROCESS, X);
    default:
      printf("Unknown event.");
    }
    putchar('\n');

  }
}

int main(int argc, char *argv[])
{
  FILE *input;

  parseArgs(argc, argv);
  if (!logFileName) {
    input = stdin;
    logFileName = "<stdin>";
  } else {
    input = fopen(logFileName, "r");
    if (input == NULL)
      everror("unable to open %s", logFileName);
  }

  createTables();
  readLog(input);
  (void)fclose(input);
  return 0;
}

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2012-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

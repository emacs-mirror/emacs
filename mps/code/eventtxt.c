/* eventtxt.c: event text log to human-friendly format.
 *
 * $Id$
 *
 * Copyright (c) 2012-2018 Ravenbrook Limited.  See end of file for license.
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

#include "check.h"
#include "config.h"
#include "eventcom.h"
#include "eventdef.h"
#include "mps.h"
#include "mpsavm.h"
#include "mpscmvff.h"
#include "table.h"
#include "testlib.h" /* for ulongest_t and associated print formats */

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h> /* exit, EXIT_FAILURE, EXIT_SUCCESS */
#include <string.h> /* strcpy, strerror, strlen */

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
  mps_pool_t pool = closure;
  mps_addr_t p;
  mps_res_t res;
  res = mps_alloc(&p, pool, size);
  if (res != MPS_RES_OK)
    everror("allocation failed: %d", res);
  return p;
}

static void tableFree(void *closure, void *p, size_t size)
{
  mps_pool_t pool = closure;
  mps_free(pool, p, size);
}

/* Printing routines */

/* printStr -- print an EventString */

static void printStr(const char *str)
{
  size_t i;

  putchar('"');
  for (i = 0; str[i] != '\0'; ++i) {
    char c = str[i];
    if (c == '"' || c == '\\')
      putchar('\\');
    putchar(c);
  }
  putchar('"');
}


/* Reading clocks, hex numbers, and doubles, and quoted-and-escaped
 * strings. */

static EventClock parseClock(char **pInOut)
{
  EventClock val;
  int i, l;
  unsigned long low, high;
  char *p = *pInOut;

  i = sscanf(p, "%08lX%08lX%n", &high, &low, &l);
  if (i != 2)
    everror("Couldn't read a clock from '%s'", p);
  EVENT_CLOCK_MAKE(val, low, high);

  *pInOut = p + l;
  return val;
}

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

static char strBuf[MAX_STRING_LENGTH];

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
 * function from non-negative integer IDs to strings), and can label
 * addresses and pointers with intern string IDs (i.e. they construct
 * a partial function from address or pointer to string ID). We need
 * three tables to keep track of these. */

static Table internTable;      /* dictionary of intern ids to strings */

static Table labelAddrTable;   /* dictionary of addrs to intern ids */

static Table labelPointerTable; /* dictionary of pointers to intern ids */

static void createTables(mps_pool_t pool)
{
  Res res;
  /* MPS intern IDs are serials from zero up, so we can use -1
   * and -2 as specials. */
  res = TableCreate(&internTable,
                    (size_t)1<<4,
                    tableAlloc, tableFree, pool,
                    (TableKey)-1, (TableKey)-2);
  if (res != ResOK)
    everror("Couldn't make intern table.");

  /* We assume that 0 and 1 are invalid as Addrs. */
  res = TableCreate(&labelAddrTable, (size_t)1<<7,
                    tableAlloc, tableFree, pool,
                    0, 1);
  if (res != ResOK)
    everror("Couldn't make address label table.");

  /* We assume that 0 and 1 are invalid as Pointers. */
  res = TableCreate(&labelPointerTable, (size_t)1<<7,
                    tableAlloc, tableFree, pool,
                    0, 1);
  if (res != ResOK)
    everror("Couldn't make pointer label table.");
}

/* recordIntern -- record an interned string in the table.  a copy of
* the string from the parsed buffer into a newly-allocated block. */

static void recordIntern(mps_pool_t pool, char *p)
{
  ulongest_t stringId;
  char *string;
  mps_addr_t copy;
  size_t len;
  Res res;

  stringId = parseHex(&p);
  string = parseString(&p);
  len = strlen(string);
  res = mps_alloc(&copy, pool, len + 1);
  if (res != MPS_RES_OK)
    everror("Couldn't allocate space for a string.");
  (void)strcpy(copy, string);
  res = TableDefine(internTable, (TableKey)stringId, (void *)copy);
  if (res != ResOK)
    everror("Couldn't create an intern mapping.");
}

/* Over time there may be multiple labels associated with an address,
 * so we keep a list, recording for each label the clock when the
 * association was made. This means that printAddr can select the
 * label that was in force at the time of the event.
 */

typedef struct LabelStruct *Label;
typedef struct LabelStruct {
  EventClock clock;             /* clock of this label */
  ulongest_t id;                /* string id of this label */
} LabelStruct;

typedef struct LabelListStruct *LabelList;
typedef struct LabelListStruct {
  size_t n;                     /* number of labels in array */
  Label labels;                 /* labels, sorted in order by clock */
} LabelListStruct;

/* labelFind returns the index of the first entry in list with a clock
 * value that's greater than 'clock', or list->n if there is no such
 * label. The list is assumed to be sorted.
 */

static size_t labelFind(LabelList list, EventClock clock)
{
  size_t low = 0, high = list->n;
  while (low < high) {
    size_t mid = (low + high) / 2;
    assert(NONNEGATIVE(mid) && mid < list->n);
    if (list->labels[mid].clock > clock) {
      high = mid;
    } else {
      low = mid + 1;
    }
  }
  assert(NONNEGATIVE(low) && low <= list->n);
  assert(low == list->n || list->labels[low].clock > clock);
  return low;
}

/* recordLabel records a label: an association (made at the time given
 * by 'clock') between a client address or an internal pointer and a
 * string ID. These are encoded as two hexadecimal numbers in the
 * string pointed to by 'p'.
 *
 * Note that the event log may have been generated on a platform with
 * addresses larger than Word on the current platform. If that happens
 * then we are scuppered because our Table code uses Word as the key
 * type: there's nothing we can do except detect this bad case (see
 * also the EventInit handling and warning code).
 *
 * We can and do handle the case where string IDs (which are Words on
 * the MPS platform) are larger than void* on the current platform.
 * This is probably in fact the same case, because Word should be the
 * same size as void*.  In practice, trying to analyse a log from a
 * wide platform on a narrow one (e.g. - the only case which is likely
 * to occur this decade - from a 64-bit platform on a 32-bit one) is
 * probably a bad idea and maybe doomed to failure.
 */

static void recordLabel(mps_pool_t pool, Table table, EventClock clock, char *p)
{
  ulongest_t address;
  LabelList list;
  Label newlabels;
  mps_addr_t tmp;
  size_t pos;
  Res res;

  address = parseHex(&p);
  if (address > (Word)-1) {
    (void)printf("label address too large!");
    return;
  }

  if (TableLookup(&tmp, table, (TableKey)address)) {
    list = tmp;
  } else {
    /* First label for this address */
    res = mps_alloc(&tmp, pool, sizeof(LabelListStruct));
    if (res != MPS_RES_OK)
      everror("Can't allocate space for a label list");
    list = tmp;
    list->n = 0;
    res = TableDefine(table, (TableKey)address, list);
    if (res != ResOK)
      everror("Couldn't create a label mapping.");
  }

  res = mps_alloc(&tmp, pool, sizeof(LabelStruct) * (list->n + 1));
  if (res != ResOK)
    everror("Couldn't allocate space for list of labels.");
  newlabels = tmp;

  pos = labelFind(list, clock);
  memcpy(newlabels, list->labels, sizeof(LabelStruct) * pos);
  newlabels[pos].clock = clock;
  newlabels[pos].id = parseHex(&p);
  memcpy(newlabels + pos + 1, list->labels + pos,
         sizeof(LabelStruct) * (list->n - pos));
  if (list->n > 0)
    mps_free(pool, list->labels, sizeof(LabelStruct) * list->n);
  list->labels = newlabels;
  ++ list->n;
}

/* output code */

/* hexWordWidth is the number of characters used to output a Word
 * value in hexadecimal.  Note that what we really care about is the
 * width of a Word on the source platform, not here.  So when we see
 * an EventInit event, we update this variable to the necessary
 * width. */

static int hexWordWidth = (MPS_WORD_WIDTH+3)/4;

/* printLabelled -- output a ulongest_t in hex, with the interned
 * string if the value is in the table */

static void printLabelled(EventClock clock, ulongest_t value,
                          const char *ident, Table table)
{
  void *tmp;

  printf("%s:%0*" PRIXLONGEST, ident, hexWordWidth, value);
  if (table != NULL && TableLookup(&tmp, table, (TableKey)value)) {
    LabelList list = tmp;
    size_t pos = labelFind(list, clock);
    if (pos > 0) {
      ulongest_t id = list->labels[pos - 1].id;
      putchar('[');
      if (TableLookup(&tmp, internTable, (TableKey)id))
        printStr((char *)tmp);
      else
        printf("unknown label %" PRIXLONGEST, id);
      putchar(']');
    }
  }
  putchar(' ');
}

/* parameter processing.  For each parameter we parse it and then
 * print it, preceded by its name and a colon and followed by a
 * space. */

#define processParamA(ident)                                    \
  val_hex = parseHex(&p);                                       \
  printLabelled(clock, val_hex, #ident, labelAddrTable);

#define processParamP(ident)                                    \
  val_hex = parseHex(&p);                                       \
  printLabelled(clock, val_hex, #ident, labelPointerTable);

#define processParamW(ident)                    \
  val_hex = parseHex(&p);                       \
  printLabelled(clock, val_hex, #ident, NULL);

#define processParamU(ident)                    \
  val_hex = parseHex(&p);                       \
  printf(#ident ":%" PRIuLONGEST " ", val_hex);

#define processParamD(ident)                    \
  val_float = parseDouble(&p);                  \
  printf(#ident ":%#8.3g ", val_float);

#define processParamS(ident)                    \
  val_string = parseString(&p);                 \
  printf(#ident ":");                           \
  printStr(val_string);                         \
  putchar(' ');

#define processParamB(ident)                            \
  val_hex = parseHex(&p);                               \
  printf(#ident ":%s ", val_hex ? "True" : "False");

#define EVENT_PROCESS_PARAM(X, index, sort, ident, doc) \
  processParam##sort(ident);

#define EVENT_PROCESS(X, name, code, used, kind)        \
  case code:                                            \
    EVENT_##name##_PARAMS(EVENT_PROCESS_PARAM, X)       \
    break;

/* a table of the event names */

static const char *eventName[EventCodeMAX+EventCodeMAX];

#define EVENT_SET_NAME(X, name, code, used, kind) \
        eventName[code] = #name;

/* this is overkill, at present. */

#define MAX_LOG_LINE_LENGTH 1024

/* readLog -- read and parse log.  Returns the number of events written.  */

static void readLog(mps_pool_t pool, FILE *input)
{
  int i;

  for (i=0; i <= EventCodeMAX; ++i)
    eventName[i] = NULL;

  EVENT_LIST(EVENT_SET_NAME, X);

  while (TRUE) { /* loop for each event */
    char line[MAX_LOG_LINE_LENGTH];
    char *p, *q;
    EventClock clock;
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

    clock = parseClock(&p);
    EVENT_CLOCK_PRINT(stdout, clock);

    code = (int)parseHex(&p);
    printf(" %04X ", code);
    if (eventName[code])
      printf("%-19s ", eventName[code]);
    else
      printf("%-19s ", "[Unknown]");

    q = p;

    /* for a few particular codes, we do local processing. */
    if (code == EventInternCode) {
      recordIntern(pool, q);
    } else if (code == EventLabelCode) {
      recordLabel(pool, labelAddrTable, clock, q);
    } else if (code == EventLabelPointerCode) {
      recordLabel(pool, labelPointerTable, clock, q);
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
  mps_arena_t arena;
  mps_pool_t pool;
  mps_res_t res;
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

  /* Ensure no telemetry output. */
  res = setenv("MPS_TELEMETRY_CONTROL", "0", 1);
  if (res != 0)
    everror("failed to set MPS_TELEMETRY_CONTROL: %s", strerror(errno));

  res = mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none);
  if (res != MPS_RES_OK)
    everror("failed to create arena: %d", res);

  res = mps_pool_create_k(&pool, arena, mps_class_mvff(), mps_args_none);
  if (res != MPS_RES_OK)
    everror("failed to create pool: %d", res);

  createTables(pool);
  readLog(pool, input);

  mps_pool_destroy(pool);
  mps_arena_destroy(arena);

  (void)fclose(input);
  return 0;
}

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2012-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

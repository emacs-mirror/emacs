/* eventpy.c: GENERATE PYTHON INTERFACE TO EVENTS
 *
 * $Id$
 * Copyright (c) 2016-2020 Ravenbrook Limited.  See end of file for license.
 *
 * This command-line program emits Python data structures that can be
 * used to parse a telemetry stream in the binary format for the
 * platform it was compiled for.
 */

#include <assert.h> /* assert */
#include <stddef.h> /* offsetof */
#include <stdio.h> /* printf, puts */

#include "event.h"

/* See <https://docs.python.org/3/library/struct.html#byte-order-size-and-alignment> */
#if defined(MPS_ARCH_A6) || defined(MPS_ARCH_I3) || defined(MPS_ARCH_I6)
#define BYTE_ORDER "<"
#else
#error "Can't determine byte order for platform architecture."
#endif


/* format -- output struct format code corresponding to event field
 *
 * size is the size of the field in bytes
 * sort is a one-character string corresponding to the EventF* typedef
 * for the field, thus "P" for a field of type EventFP.
 *
 * See <https://docs.python.org/3/library/struct.html>
 */

static void format(size_t size, const char *sort)
{
  switch (sort[0]) {
  case 'B':
    printf("?");
    break;
  case 'D':
    printf("d");
    break;
  case 'S':
    /* Strings can't be handled through the struct format mechanism
       because we don't know their length until the header has been
       read. */
    break;
  default:
    switch (size) {
    case 1:
      printf("B");
      break;
    case 2:
      printf("H");
      break;
    case 4:
      printf("L");
      break;
    case 8:
      printf("Q");
      break;
    default:
      assert(FALSE);
      break;
    }
  }
}

int main(int argc, char *argv[])
{
  size_t size, prev_offset;
  char prev_sort;
  UNUSED(argc);
  UNUSED(argv);

  puts("from collections import namedtuple");

  printf("\n__version__ = %d, %d, %d\n", EVENT_VERSION_MAJOR,
         EVENT_VERSION_MEDIAN, EVENT_VERSION_MINOR);

  puts("\n# Description of an event kind.");
  puts("KindDesc = namedtuple('KindDesc', 'name code doc')");

  puts("\n# Namespace containing a KindDesc for every kind.");
  puts("class Kind:");
#define ENUM(X, NAME, DOC)                                      \
  printf("    " #NAME " = KindDesc('" #NAME "', %d, '%s')\n",   \
         EventKind ## NAME, DOC);
  EventKindENUM(ENUM, X);
#undef ENUM

  puts("\n# Mapping from kind number to KindDesc.");
  puts("KIND = {");
#define ENUM(X, NAME, DOC) \
  printf("    %d: Kind." #NAME ",\n", EventKind ## NAME);
  EventKindENUM(ENUM, X);
#undef ENUM
  puts("}");

  puts("\n# Description of a parameter of an event.");
  puts("EventParam = namedtuple('EventParam', 'sort name doc')");

  puts("\n# Description of the parameters of an event.");
  puts("EventDesc = namedtuple('EventDesc', "
       "'name code used kind params maxsize format')");

  puts("\n# Namespace containing an EventDesc for every event.");
  puts("class Event:");
#define PAD_TO(OFFSET)                                  \
  BEGIN {                                               \
    size_t offset = (OFFSET);                           \
    if (prev_sort != 'S' && prev_offset < offset)       \
      printf("%ux", (unsigned)(offset - prev_offset));  \
    prev_offset = offset;                               \
  } END
#define EVENT_PARAM(X, INDEX, SORT, IDENT, DOC)                         \
  puts("        EventParam('" #SORT "', '" #IDENT "', \"" DOC "\"),");  \
  prev_sort = #SORT[0];
#define EVENT_FORMAT(NAME, INDEX, SORT, IDENT, DOC)     \
  PAD_TO(offsetof(Event##NAME##Struct, f##INDEX));      \
  format(sizeof(EventF##SORT), #SORT);                  \
  prev_offset += sizeof(EventF##SORT);
#define EVENT_DEFINE(X, NAME, CODE, USED, KIND)                         \
  printf("    " #NAME " = EventDesc('" #NAME "', %d, %s, Kind." #KIND ", [\n", \
         CODE, USED ? "True" : "False");                                \
  EVENT_ ## NAME ## _PARAMS(EVENT_PARAM, X);                            \
  size = sizeof(Event##NAME##Struct) - sizeof(EventAnyStruct);          \
  printf("    ], %u, '%s", (unsigned)size, BYTE_ORDER);                 \
  prev_offset = sizeof(EventAnyStruct);                                 \
  EVENT_ ## NAME ## _PARAMS(EVENT_FORMAT, NAME);                        \
  PAD_TO(sizeof(Event##NAME##Struct));                                  \
  puts("')");
  EVENT_LIST(EVENT_DEFINE, 0);
#undef EVENT_DEFINE
#undef EVENT_PARAM
#undef EVENT_FORMAT

  puts("\n# Mapping from event number to EventDesc.");
  puts("EVENT = {");
#define EVENT_ITEM(X, NAME, CODE, USED, KIND) \
  printf("    %d: Event." #NAME ",\n", CODE);
  EVENT_LIST(EVENT_ITEM, 0);
#undef EVENT_ITEM
  puts("}");

  puts("\n# Description of an event header.");
  printf("HeaderDesc = namedtuple('HeaderDesc', '");
#define EVENT_FIELD(TYPE, NAME, DOC) printf("%s ", #NAME);
  EVENT_ANY_FIELDS(EVENT_FIELD)
#undef EVENT_FIELD
  puts("')\nHeaderDesc.__doc__ = '''");
#define EVENT_FIELD(TYPE, NAME, DOC) printf("%s -- %s\n", #NAME, DOC);
  EVENT_ANY_FIELDS(EVENT_FIELD)
#undef EVENT_FIELD
  puts("'''");

  puts("\n# Size of event header in bytes.");
  printf("HEADER_SIZE = %u\n", (unsigned)sizeof(EventAnyStruct));

  puts("\n# Struct format for event header.");
  printf("HEADER_FORMAT = '%s", BYTE_ORDER);
  prev_offset = 0;
#define EVENT_FIELD(TYPE, NAME, DOC)            \
  PAD_TO(offsetof(EventAnyStruct, NAME));       \
  format(sizeof(TYPE), "?");                    \
  prev_offset += sizeof(TYPE);
  EVENT_ANY_FIELDS(EVENT_FIELD)
#undef EVENT_FIELD
  PAD_TO(sizeof(EventAnyStruct));
  puts("'");

  puts("\n# Mapping from access mode to its name.");
  puts("ACCESS_MODE = {");
  printf("    %u: \"READ\",\n", (unsigned)AccessREAD);
  printf("    %u: \"WRITE\",\n", (unsigned)AccessWRITE);
  printf("    %u: \"READ/WRITE\",\n",
         (unsigned)BS_UNION(AccessREAD, AccessWRITE));
  puts("}");

  puts("\n# Mapping from rank to its name.");
  puts("RANK = {");
#define X(RANK) printf("    %u: \"%s\",\n", (unsigned)Rank ## RANK, #RANK);
  RANK_LIST(X)
#undef X
  puts("}");

  puts("\n# Mapping from trace start reason to its short description.");
  puts("TRACE_START_WHY = {");
#define X(WHY, SHORT, LONG) \
  printf("    %u: \"%s\",\n", (unsigned)TraceStartWhy ## WHY, SHORT);
  TRACE_START_WHY_LIST(X)
#undef X
  puts("}");

  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2016-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

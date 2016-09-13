/* eventpy.c: GENERATE PYTHON INTERFACE TO EVENTS
 *
 * $Id$
 * Copyright (c) 2016 Ravenbrook Limited.  See end of file for license.
 *
 * This command-line program emits Python data structures that can be
 * used to parse an event stream in text format (as output by the
 * mpseventcnv program).
 */

#include <stdio.h> /* printf, puts */

#include "event.h"

int main(int argc, char *argv[])
{
  UNUSED(argc);
  UNUSED(argv);

  puts("from collections import namedtuple");

  printf("__version__ = %d, %d, %d\n", EVENT_VERSION_MAJOR,
         EVENT_VERSION_MEDIAN, EVENT_VERSION_MINOR);

  puts("EventKind = namedtuple('EventKind', 'name code doc')");
  puts("class kind:");
#define ENUM(_, NAME, DOC)                                              \
  printf("    " #NAME " = EventKind('" #NAME "', %d, \"%s\")\n",        \
         EventKind ## NAME, DOC);
  EventKindENUM(ENUM, _);
#undef ENUM

  puts("kinds = {");
#define ENUM(_, NAME, _1) \
  printf("    %d: kind." #NAME ",\n", EventKind ## NAME);
  EventKindENUM(ENUM, _);
#undef ENUM
  puts("}");

  puts("EventParam = namedtuple('EventParam', 'sort, name')");
  puts("Event = namedtuple('Event', 'name code always kind params')");
  puts("class event:");
#define EVENT_PARAM(X, INDEX, SORT, NAME)               \
  puts("        EventParam('" #SORT "', '" #NAME "'),");
#define EVENT_DEFINE(X, NAME, CODE, ALWAYS, KIND)                       \
  printf("    " #NAME " = Event('" #NAME "', %d, %s, kind." #KIND ", [\n", \
         CODE, ALWAYS ? "True" : "False");                              \
  EVENT_ ## NAME ## _PARAMS(EVENT_PARAM, X);                            \
  puts("    ]);");
  EVENT_LIST(EVENT_DEFINE, 0);
#undef EVENT

  puts("events = {");
#define EVENT_ITEM(X, NAME, CODE, ALWAYS, KIND) \
  printf("    %d: event." #NAME ",\n", CODE);
  EVENT_LIST(EVENT_ITEM, 0);
#undef EVENT
  puts("}");

  return 0;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

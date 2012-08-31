/* eventpro.c: Event processing routines
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * $Id$
 */

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "table.h"

#include "eventdef.h"
#include "eventcom.h"
#include "eventpro.h"
#include "misc.h"
#include "mpmtypes.h"
#include "testlib.h" /* for ulongest_t and associated print formats */

#include <assert.h> /* assert */
#include <stdlib.h> /* size_t */
#include <string.h> /* strcmp */

struct EventProcStruct {
  Bool partialLog;        /* Is this a partial log? */
  EventProcReader reader; /* reader fn */
  void *readerP;          /* closure pointer for reader fn */
  Table internTable;      /* dictionary of intern ids to symbols */
  Table labelTable;       /* dictionary of addrs to intern ids */
  void *cachedEvent;
};


/* error -- error signalling
 *
 * Should integrate with client exceptions, but that'll do for now.
 */

#define error(fmt, arg) assert(((void)fmt, FALSE));


/* PointerAdd -- add offset to pointer
 *
 * Copy of the def in mpm.h which we can't include
 */

#define PointerAdd(p, s) ((void *)((char *)(p) + (s)))


/* sizeAlignUp -- align size_t values up */

#define sizeAlignUp(w, a) (((w) + (a) - 1) & ~((size_t)(a) - 1))


/* EventSizeAlign -- Calculate actual size of event in the output
 *
 * Calculates the actual size of an event in the output, given the size
 * of the structure.  This has to agree with the writing (EVENT_END).
 */

/* FIXME: MPS_PF_ALIGN should be read from event file header? */

#define EventSizeAlign(size) sizeAlignUp(size, MPS_PF_ALIGN)



/* Event types */


/* eventTypes -- an array containing info about the event types */

typedef struct {
  char *name;           /* Event name e.g. "TraceStart" */
  EventCode code;
  size_t size;          /* event record size, rounded up from structure */
  Count count;          /* Parameter count */
  char *format;         /* string format, e.g. "PPW" */
} eventRecord;

#define EVENT_COUNT_PARAM(X, index, sort, ident) + 1

#define EVENT_FORMAT_PARAM(X, index, sort, ident) #sort

#define EVENT_OFFSETS_PARAM(name, index, sort, ident) \
  offsetof(Event##name##Struct, f##index),

#define EVENT_INIT(X, name, code, always, kind) \
  {#name, \
   code, \
   EventSizeAlign(sizeof(Event##name##Struct)), \
   0 EVENT_##name##_PARAMS(EVENT_COUNT_PARAM, X), \
   "" EVENT_##name##_PARAMS(EVENT_FORMAT_PARAM, X)},

static eventRecord eventTypes[] = {
  {"(unused)", 0, 0, 0, ""},
  EVENT_LIST(EVENT_INIT, X)
};

#define eventTypeCount (sizeof(eventTypes) / sizeof(eventRecord))


/* eventcode2Index -- find index in eventTypes for the given code */

static size_t eventCode2Index(EventCode code, Bool errorp)
{
  size_t i;

  for(i = 0; i < eventTypeCount; ++i)
    if (eventTypes[i].code == code)
      return i;
  if (errorp)
    error("Unknown event code %0"PRIwWORD PRIXLONGEST, (ulongest_t)code);
  return 0;
}


/* EventName2Code -- find event code for the given event name */

EventCode EventName2Code(char *name)
{
  size_t i;

  for(i = 0; i < eventTypeCount; ++i)
    if (strcmp(eventTypes[i].name, name) == 0) {
      assert(eventTypes[i].code <= EventCodeMAX);
      return eventTypes[i].code;
    }
  error("Unknown event name %s", name);
  return 0;
}


/* EventCode2Name -- find event name for the given event code */

char *EventCode2Name(EventCode code)
{
  return eventTypes[eventCode2Index(code, TRUE)].name;
}


/* EventCode2Format -- find format for the given event code */

char *EventCode2Format(EventCode code)
{
  return eventTypes[eventCode2Index(code, TRUE)].format;
}


Bool EventCodeIsValid(EventCode code)
{
  return (eventCode2Index(code, FALSE) != 0);
}


/* EventStrings */


/* eventStringCopy -- copy an event string */

static Res stringCopy(char **str_o, char *str)
{
  char *newStr;
  size_t len;

  len = strlen(str);
  newStr = (char *)malloc(len + sizeof('\0'));
  if (newStr == NULL) return ResMEMORY;
  memcpy(newStr, str, len + sizeof('\0'));
  *str_o = newStr;
  return ResOK;
}


static void eventStringDestroy(char *str)
{
  free(str);
}


/* Labels */


/* Symbol -- representation of an interned string */

typedef struct symbolStruct {
  Word id;
  char *name;
} symbolStruct;
typedef struct symbolStruct *Symbol;


/* Label -- representation of a labelled address */

typedef struct labelStruct {
  Word id;
  EventClock time;
  Addr addr;
} labelStruct;
typedef struct labelStruct *Label;


/* AddrLabel -- return intern id for given addr (or 0 if none) */

Word AddrLabel(EventProc proc, Addr addr)
{
  void *entry;

  if (TableLookup(&entry, proc->labelTable, (Word)addr))
    return ((Label)entry)->id;
  else
    return (Word)0;
}


/* LabelText -- return text for given intern id (or NULL if none) */

char *LabelText(EventProc proc, Word id)
{
  void *entry;

  if (TableLookup(&entry, proc->internTable, id))
    return ((Symbol)entry)->name;
  else
    return NULL;
}


/* Processing */


/* EventRead -- read one event from the file and allocate descriptor */

#define internStrOffset (offsetof(EventInternStruct, f1.str))

Res EventRead(Event *eventReturn, EventProc proc)
{
  size_t eventIndex;
  Res res;
  EventAnyStruct anyStruct;
  Event event;
  void *restOfEvent;
  
  res = proc->reader(proc->readerP, &anyStruct, sizeof(anyStruct));
  if (res != ResOK)
    return res;

  eventIndex = eventCode2Index(anyStruct.code, TRUE);
  assert(anyStruct.code == EventInternCode ||
         anyStruct.size == eventTypes[eventIndex].size);

  if (proc->cachedEvent != NULL) {
    event = proc->cachedEvent;
    proc->cachedEvent = NULL;
  } else {
    /* This is too long for most events, but never mind. */
    event = (Event)malloc(sizeof(EventUnion));
    if (event == NULL)
      return ResMEMORY;
  }

  event->any = anyStruct;
  restOfEvent = PointerAdd(event, sizeof(anyStruct));
  res = proc->reader(proc->readerP,
                     PointerAdd(event, sizeof(anyStruct)),
                     anyStruct.size - sizeof(anyStruct));
  if (res != ResOK)
    return res;

  *eventReturn = event;
  return ResOK;
}


/* EventRecord -- record event in databases
 *
 * Currently only labels are tracked, but perhaps there will be other
 * stuff in the future.
 */

Res EventRecord(EventProc proc, Event event, EventClock etime)
{
  Res res;

  switch(event->any.code) {
  case EventInternCode: {           /* id, label */
    Symbol sym = malloc(sizeof(symbolStruct));

    if (sym == NULL) return ResMEMORY;
    sym->id = event->Intern.f0;
    res = stringCopy(&sym->name, event->Intern.f1);
    if (res != ResOK) {
      free(sym);
      return res;
    }
    res = TableDefine(proc->internTable, sym->id, sym);
  } break;
  case EventLabelCode: {            /* addr, id */
    Label label = malloc(sizeof(labelStruct));
    void *entry;

    if (label == NULL) return ResMEMORY;
    label->id = event->Label.f1;
    if (!proc->partialLog) {
      assert(TableLookup(&entry, proc->internTable, label->id));
    }
    label->time = etime;
    label->addr = event->Label.f0;
    if (TableLookup(&entry, proc->labelTable, (Word)label->addr))
      res = TableRedefine(proc->labelTable, (Word)label->addr, label);
    else
      res = TableDefine(proc->labelTable, (Word)label->addr, label);
  } break;
  default:
    res = ResOK;
    break;
  }
  return res;
}


/* EventDestroy -- destroy an event */

void EventDestroy(EventProc proc, Event event)
{
  if (proc->cachedEvent == NULL)
    proc->cachedEvent = event;
  else
    free(event);
}


/* initialization and finishing */


/* Checking macros, copied from check.h */

#define CHECKLVALUE(lv1, lv2) \
  ((void)sizeof((lv1) = (lv2)), (void)sizeof((lv2) = (lv1)), TRUE)

#define CHECKTYPE(t1, t2) \
  (sizeof(t1) == sizeof(t2) && \
   CHECKLVALUE(*((t1 *)0), *((t2 *)0)))

#define CHECKFIELDAPPROX(s1, f1, s2, f2) \
  (sizeof(((s1 *)0)->f1) == sizeof(((s2 *)0)->f2) && \
   offsetof(s1, f1) == offsetof(s2, f2))

#define CHECKFIELD(s1, f1, s2, f2) \
  (CHECKFIELDAPPROX(s1, f1, s2, f2) && \
   CHECKLVALUE(((s1 *)0)->f1, ((s2 *)0)->f2))


/* EventProcCreate -- initialize the module */

Res EventProcCreate(EventProc *procReturn, Bool partial,
                    EventProcReader reader, void *readerP)
{
  Res res;
  EventProc proc = malloc(sizeof(struct EventProcStruct));

  if (proc == NULL) return ResMEMORY;

  /* check event struct access */
  assert(CHECKFIELD(EventUnion, any.code, EventInternStruct, code));
  assert(CHECKFIELD(EventUnion, any.clock, EventInternStruct, clock));
  /* check use of labelTable */
  assert(sizeof(Word) >= sizeof(Addr));

  proc->partialLog = partial;
  proc->reader = reader; proc->readerP = readerP;
  res = TableCreate(&proc->internTable, (size_t)1<<4);
  if (res != ResOK) goto failIntern;
  res = TableCreate(&proc->labelTable, (size_t)1<<7);
  if (res != ResOK) goto failLabel;
  proc->cachedEvent = NULL;
  *procReturn = proc;
  return ResOK;

failLabel:
  TableDestroy(proc->internTable);
failIntern:
  free(proc);
  return res;
}


/* EventProcDestroy -- finish the module */

static void deallocItem(Word key, void *value)
{
  UNUSED(key);
  free(value);
}

static void deallocSym(Word key, void *value)
{
  UNUSED(key);
  eventStringDestroy(((Symbol)value)->name);
  free(value);
}

void EventProcDestroy(EventProc proc)
{
  TableMap(proc->labelTable, deallocItem);
  TableMap(proc->internTable, deallocSym);
  TableDestroy(proc->labelTable);
  TableDestroy(proc->internTable);
  if (proc->cachedEvent != NULL)
    free(proc->cachedEvent);
  free(proc);
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

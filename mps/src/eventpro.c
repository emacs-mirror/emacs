/* impl.c.eventpro: Event processing routines
 * Copyright (C) 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName: MMsrc!eventpro.c(trunk.2) $
 */

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "table.h"

#include "eventcom.h"
#include "eventpro.h"
#include "misc.h"
#include "mpmtypes.h"


struct EventProcStruct {
  Bool partialLog;        /* Is this a partial log? */
  EventProcReader reader; /* reader fn */
  void *readerP;          /* closure pointer for reader fn */
  Table internTable;      /* dictionary of intern ids to symbols */
  Table labelTable;       /* dictionary of addrs to intern ids */
};


/* error -- error signalling
 *
 * Should integrate with client exceptions, but that'll do for now.
 */

#define error(fmt, arg) assert(FALSE);


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

#define EventSizeAlign(size) sizeAlignUp(size, sizeof(Word))



/* Event types */


/* eventTypes -- an array containing info about the event types */

typedef struct {
  EventType type;
  char *name;
  size_t code;
  size_t length;
  char *format;
} eventRecord;

static eventRecord eventTypes[] = {
  {0, "(unused)", 0, 0, "0"},
#define RELATION(name, code, always, kind, format) \
  {Event##name, #name, code, \
   EventSizeAlign(sizeof(Event##format##Struct)), #format},
#include "eventdef.h"
#undef RELATION
};

#define eventTypeCount (sizeof(eventTypes) / sizeof(eventRecord))


/* eventType2Index -- find index in eventTypes for the given type */

static size_t eventType2Index(EventType type)
{
  size_t i;
  
  for(i = 0; i < eventTypeCount; ++i)
    if (eventTypes[i].type == type)
      return i;
  error("Unknown event type %08lX", type);
  return 0;
}


/* eventcode2Index -- find index in eventTypes for the given code */

static size_t eventCode2Index(EventCode code, Bool errorp)
{
  size_t i;
  
  for(i = 0; i < eventTypeCount; ++i)
    if (eventTypes[i].code == code)
      return i;
  if (errorp)
    error("Unknown event code %08lX", code);
  return 0;
}


/* EventName2Code -- find event code for the given event name */

EventCode EventName2Code(char *name)
{
  size_t i;
  
  for(i = 0; i < eventTypeCount; ++i)
    if (strcmp(eventTypes[i].name, name) == 0)
      return eventTypes[i].code;
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


/* EventGetCode -- get event code of the given event */

EventCode EventGetCode(Event event)
{
  size_t i = eventType2Index(event->any.code);
  return eventTypes[i].code;
}


Bool EventCodeIsValid(EventCode code)
{
  return (eventCode2Index(code, FALSE) != 0);
}


/* EventStrings */


/* EventStringEmpty -- an empty event string */

EventStringStruct EventStringEmpty = {0, ""};


/* eventStringCopy -- copy an event string */

static Res eventStringCopy(EventString *str_o, EventString str)
{
  EventString newStr;

  newStr = (EventString)malloc(offsetof(EventStringStruct, str)
                               + str->len);
  if (newStr == NULL) return ResMEMORY;
  newStr->len = str->len;
  memcpy(&(newStr->str), &(str->str), str->len);
  *str_o = newStr;
  return ResOK;
}


/* Labels */


/* Symbol -- representation of an interned string */

typedef struct symbolStruct {
  Word id;
  EventString name;
} symbolStruct;
typedef struct symbolStruct *Symbol;


/* Label -- representation of a labelled address */

typedef struct labelStruct {
  Word id;
  Word time;
  Addr addr;
} labelStruct;
typedef struct labelStruct *Label;


/* AddrLabel -- return intern id for given addr (or 0 if none) */

Word AddrLabel(EventProc proc, Addr addr)
{
  Label label;
  if (TableLookup((void **)&label, proc->labelTable, (Word)addr))
    return label->id;
  else
    return (Word)0;
}


/* LabelText -- return text for given intern id (or NULL if none) */

EventString LabelText(EventProc proc, Word id)
{
  Symbol sym;

  if (TableLookup((void **)&sym, proc->internTable, id))
    return sym->name;
  else
    return NULL;
}


/* Processing */


/* EventRead -- read one event from the file and allocate descriptor */

#define internStrOffset (offsetof(EventWSStruct, s1.str))

Res EventRead(Event *eventReturn, EventProc proc)
{
  size_t index, length;
  Res res;
  EventType type;
  Event event;
  void *restOfEvent;

  res = proc->reader(proc->readerP, &type, sizeof(EventType));
  if (res != ResOK) return res;

  index = eventType2Index(type);
  length = eventTypes[index].length;
  /* This is too long for string events, but nevermind. */
  event = (Event)malloc(length);
  if (event == NULL) return ResMEMORY;

  event->any.code = type;
  restOfEvent = PointerAdd(event, sizeof(EventType));
  if (type == EventIntern) { /* the only string event */
    /* read enough to get the length */
    res = proc->reader(proc->readerP, restOfEvent,
                       internStrOffset - sizeof(EventType));
    if (res != ResOK) return res;
    /* read the rest */
    res = proc->reader(proc->readerP, &(event->ws.s1.str),
                       /* Length must agree with EVENT_WS. */
                       EventSizeAlign(internStrOffset + event->ws.s1.len)
                       - internStrOffset);
    if (res != ResOK) return res;
  } else {
    res = proc->reader(proc->readerP, restOfEvent,
                       length - sizeof(EventType));
    if (res != ResOK) return res;
  }
  *eventReturn = event;
  return ResOK;
}


/* EventRecord -- record event in databases
 *
 * Currently only labels are tracked, but perhaps there will be other
 * stuff in the future.
 */

Res EventRecord(EventProc proc, Event event, Word etime)
{
  Res res;

  switch(event->any.code) {
  case EventIntern: {   	/* id, label */
    Symbol sym = malloc(sizeof(symbolStruct));
      
    if (sym == NULL) return ResMEMORY;
    sym->id = event->ws.w0;
    res = eventStringCopy(&(sym->name), &(event->ws.s1));
    if (res != ResOK) {
      free(sym);
      return res;
    }
    res = TableDefine(proc->internTable, sym->id, sym);
  } break;
  case EventLabel: {		/* addr, id */
    Label label = malloc(sizeof(labelStruct)), oldLabel;

    if (label == NULL) return ResMEMORY;
    label->id = event->aw.w1;
    if (!proc->partialLog) {
      Symbol sym;
      assert(TableLookup((void **)&sym, proc->internTable, label->id));
      UNUSED(sym);
    }
    label->time = etime;
    label->addr = event->aw.a0;
    if (TableLookup((void **)&oldLabel, proc->labelTable,
                    (Word)label->addr))
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
  UNUSED(proc);
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


/* EventProcInit -- initialize the module */

Res EventProcInit(EventProc *procReturn, Bool partial,
                   EventProcReader reader, void *readerP)
{
  Res res;
  EventProc proc = malloc(sizeof(struct EventProcStruct));

  if (proc == NULL) return ResMEMORY;

  /* check event struct access */
  assert(CHECKFIELD(EventUnion, any.code, EventWSStruct, code));
  assert(CHECKFIELD(EventUnion, any.clock, EventWSStruct, clock));
  /* check use of labelTable */
  assert(sizeof(Word) >= sizeof(Addr));

  proc->partialLog = partial;
  proc->reader = reader; proc->readerP = readerP;
  res = TableCreate(&proc->internTable, (size_t)1<<4);
  if (res != ResOK) goto failIntern;
  res = TableCreate(&proc->labelTable, (size_t)1<<7);
  if (res != ResOK) goto failLabel;
  *procReturn = proc;
  return ResOK;

failLabel:
  TableDestroy(proc->internTable);
failIntern:
  free(proc);
  return res;
}


/* EventProcFinish -- finish the module */

static void deallocItem(Word key, void *value)
{
  UNUSED(key);
  free(value);
}

void EventProcFinish(EventProc proc)
{
  TableMap(proc->labelTable, deallocItem);
  TableMap(proc->internTable, deallocItem);
  TableDestroy(proc->labelTable);
  TableDestroy(proc->internTable);
  free(proc);
}

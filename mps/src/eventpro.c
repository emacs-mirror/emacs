/* impl.c.eventpro: Event processing routines
 * Copyright (C) 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName: MMsrc!eventpro.c(trunk.1) $
 */

#include <assert.h>
#include <stdlib.h>

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "table.h"

#include "eventcom.h"
#include "eventpro.h"
#include "misc.h"
#include "mpmtypes.h"


static Bool partialLog; /* Is this a partial log? */


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

static size_t eventCode2Index(EventCode code)
{
  size_t i;
  
  for(i = 0; i < eventTypeCount; ++i)
    if (eventTypes[i].code == code)
      return i;
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
  return eventTypes[eventCode2Index(code)].name;
}


/* EventCode2Format -- find format for the given event code */

char *EventCode2Format(EventCode code)
{
  return eventTypes[eventCode2Index(code)].format;
}


/* EventGetCode -- get event code of the given event */

EventCode EventGetCode(Event event)
{
  size_t i = eventType2Index(event->any.code);
  return eventTypes[i].code;
}


/* EventStrings */


/* EventStringEmpty -- an empty event string */

EventStringStruct EventStringEmpty = {0, ""};


/* eventStringCopy -- copy an event string */

static void eventStringCopy(EventString *str_o, EventString str)
{
  EventString newStr;

  newStr = (EventString)malloc(offsetof(EventStringStruct, str)
                               + str->len);
  assert(newStr != NULL);
  newStr->len = str->len;
  memcpy((void *)&(newStr->str), (void *)&(str->str), str->len);
  *str_o = newStr;
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


static Table internTable; /* dictionary of intern ids to symbols */
static Table labelTable; /* dictionary of addrs to intern ids */


/* AddrLabel -- return intern id for given addr (or 0 if none) */

Word AddrLabel(Addr addr)
{
  Label label = (Label)TableLookup(labelTable, (Word)addr);
  return (label == NULL) ? (Word)0 : label->id;
}


/* LabelText -- return text for given intern id (or NULL if none) */

EventString LabelText(Word id)
{
  Symbol sym = (Symbol)TableLookup(internTable, id);
  return (sym == NULL) ? NULL : sym->name;
}


/* Processing */


/* EventRead -- read one event from the file and allocate descriptor */

#define internStrOffset (offsetof(EventWSStruct, s1.str))

Res EventRead(Event *eventOut, FILE *input)
{
  size_t n, index, length;
  EventType type;
  Event event;
  void *restOfEvent;

  n = fread((void *)&type, sizeof(EventType), 1, input);
  if (n < 1) {
    if (feof(input)) {
      *eventOut = NULL;
      return ResOK;
    } else {
      return ResIO;
    }
  }

  index = eventType2Index(type);
  length = eventTypes[index].length;
  /* This is too long for string events, but nevermind. */
  event = (Event)malloc(length);
  assert(event != NULL);

  event->any.code = type;
  restOfEvent = PointerAdd((void *)event, sizeof(EventType));
  if (type == EventIntern) { /* the only string event */
    /* read enough to get the length */
    n = fread(restOfEvent, internStrOffset - sizeof(EventType),
              1, input);
    if (n < 1) return ResIO;
    /* read the rest */
    n = fread((void *)&(event->ws.s1.str),
              /* Length calculation must agree with EVENT_WS. */
              EventSizeAlign(internStrOffset + event->ws.s1.len)
              - internStrOffset,
              1, input);
    if (n < 1) return ResIO;
  } else {
    n = fread(restOfEvent, length - sizeof(EventType), 1, input);
    if (n < 1) return ResIO;
  }
  *eventOut = event;
  return ResOK;
}


/* EventRecord -- record event in databases
 *
 * Currently only labels are tracked, but perhaps there will be other
 * stuff in the future.
 */

void EventRecord(Event event, Word etime)
{
  Res res;
  switch(event->any.code) {
  case EventIntern: {   	/* id, label */
    Symbol sym = malloc(sizeof(symbolStruct));
      
    assert(sym != NULL);
    sym->id = event->ws.w0;
    eventStringCopy(&(sym->name), &(event->ws.s1));
    res = TableDefine(internTable, sym->id, sym);
    assert(res == ResOK);
  } break;
  case EventLabel: {		/* addr, id */
    Label label = malloc(sizeof(labelStruct));

    assert(label != NULL);
    label->id = event->aw.w1;
    if (!partialLog)
      assert(TableLookup(internTable, label->id) != NULL);
    label->time = etime;
    label->addr = event->aw.a0;
    if (TableLookup(labelTable, (Word)label->addr) != NULL)
      res = TableRedefine(labelTable, (Word)label->addr, label);
    else
      res = TableDefine(labelTable, (Word)label->addr, label);
    assert(res == ResOK);
  } break;
  default: 
    break;
  }
}


/* EventDestroy -- destroy an event */

void EventDestroy(Event event)
{
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

void EventProcInit(Bool partial)
{
  Res res;

  assert(CHECKFIELD(EventUnion, any.code, EventWSStruct, code));
  assert(CHECKFIELD(EventUnion, any.clock, EventWSStruct, clock));

  partialLog = partial;
  res = TableCreate(&internTable, (size_t)1<<4); /* Magic number */
  if (res != ResOK)
    error("unable to create internTable", 0);

  res = TableCreate(&labelTable, (size_t)1<<4); /* Magic number */
  if (res != ResOK)
    error("unable to create labelTable", 0);
}


/* EventProcFinish -- finish the module */

void EventProcFinish(void)
{
  TableDestroy(labelTable);
  TableDestroy(internTable);
}

/* impl.c.eventpro: Event processing routines
 * Copyright (C) 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName$
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


static Bool partialLog;


/* Event types */


typedef struct {
  EventType type;
  char *name;
  size_t code;
  size_t length;
  char *format;
} eventRecord;

static eventRecord eventTypes[] = {
#define RELATION(name, code, always, kind, format) \
  {Event##name, #name, code, sizeof(Event##format##Struct), #format},
#include "eventdef.h"
#undef RELATION
};

#define eventTypeCount (sizeof(eventTypes) / sizeof(eventRecord))


#define error(fmt, arg) assert(FALSE);
/* Should integrate with client exceptions, but that'll do for now. */


static size_t eventType2Index(EventType type)
{
  size_t i;
  
  for(i = 0; i < eventTypeCount; ++i)
    if (eventTypes[i].type == type)
      return i;
  error("Unknown event type %08lX", type);
  return 0;
}


static size_t eventCode2Index(EventCode code)
{
  size_t i;
  
  for(i = 0; i < eventTypeCount; ++i)
    if (eventTypes[i].code == code)
      return i;
  error("Unknown event code %08lX", code);
  return 0;
}


EventCode EventName2Code(char *name)
{
  size_t i;
  
  for(i = 0; i < eventTypeCount; ++i)
    if (strcmp(eventTypes[i].name, name) == 0)
      return eventTypes[i].code;
  error("Unknown event name %s", name);
  return 0;
}

char *EventCode2Name(EventCode code)
{
  return eventTypes[eventCode2Index(code)].name;
}

char *EventCode2Format(EventCode code)
{
  return eventTypes[eventCode2Index(code)].format;
}


EventCode EventGetCode(Event event)
{
  size_t i = eventType2Index(event->any.code);
  return eventTypes[i].code;
}


/* Labels */


typedef struct symbolStruct {
  Word id;
  char *name;
} symbolStruct;
typedef struct symbolStruct *Symbol;

typedef struct labelStruct {
  Word id;
  Word time;
  Addr addr;
} labelStruct;
typedef struct labelStruct *Label;

static Table internTable;
static Table labelTable;


Word AddrLabel(Addr addr)
{
  Label label = (Label)TableLookup(labelTable, (Word)addr);
  return (label == NULL) ? (Word)0 : label->id;
}

char *LabelText(Word id)
{
  Symbol sym = (Symbol)TableLookup(internTable, id);
  return (sym == NULL) ? "" : sym->name;
}


/* Processing */


Res EventRead(Event *eventOut, FILE *input)
{
  size_t n, index, length;
  EventType type;
  Event event;

  n = fread((void *)&type, sizeof(EventType), 1, input);
  if (n < 1) {
    if (feof(input))
      return ResFAIL;
    else
      return ResIO;
  }
  index = eventType2Index(type);
  length = eventTypes[index].length;
  event = (Event)malloc(length);
  assert(event != NULL);
  event->any.code = type;
  n = fread((void *)&(event->any.length), length - sizeof(EventType), 1,
            input);
  if (n < 1) return ResIO;
  *eventOut = event;
  return ResOK;
}


void EventRecord(Event event, Word etime)
{
  Res res;
  switch(event->any.code) {
  case EventIntern: {   	/* id, label */
    Symbol sym = malloc(sizeof(symbolStruct));
      
    assert(sym != NULL);
    sym->id = event->ws.w0;
    sym->name = event->ws.s1;
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


void EventDestroy(Event event)
{
  free(event);
}


/* initialization and finishing */


void EventProcInit(Bool partial)
{
  Res res;

  partialLog = partial;
  res = TableCreate(&internTable, (size_t)1<<4); /* Magic number */
  if (res != ResOK)
    error("unable to create internTable", 0);

  res = TableCreate(&labelTable, (size_t)1<<4); /* Magic number */
  if (res != ResOK)
    error("unable to create labelTable", 0);
}


void EventProcFinish(void)
{
}

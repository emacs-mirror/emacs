/* impl.h.eventpro: Interface for event processing routines
 * Copyright (C) 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName: MMsrc!eventpro.h(trunk.3) $
 */

#ifndef eventpro_h
#define eventpro_h

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "eventcom.h"
#include "mpmtypes.h"


typedef struct EventProcStruct *EventProc;
typedef Res (*EventProcReader)(void *, void *, size_t);


extern EventCode EventName2Code(char *name);
extern char *EventCode2Name(EventCode code);
extern EventCode EventGetCode(Event event);
extern char *EventCode2Format(EventCode code);
extern Bool EventCodeIsValid(EventCode code);

extern Word AddrLabel(EventProc proc, Addr addr);
extern EventString LabelText(EventProc proc, Word label);

extern Res EventRead(Event *eventReturn, EventProc proc);
extern void EventDestroy(EventProc proc, Event event);

extern Res EventRecord(EventProc proc, Event event, Word etime);

extern Res EventProcCreate(EventProc *procReturn, Bool partial,
                           EventProcReader reader, void *readerP);
extern void EventProcDestroy(EventProc proc);


#endif /* eventpro_h */

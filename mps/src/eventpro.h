/* impl.h.eventpro: Interface for event processing routines
 * Copyright (C) 1999 Harlequin Group plc.  All rights reserved.
 *
 * $HopeName$
 */

#ifndef eventpro_h
#define eventpro_h

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "eventcom.h"
#include "mpmtypes.h"
#include <stdio.h>
#include "mpstd.h"
#ifdef MPS_OS_SU
#include "ossu.h"
#endif


typedef EventUnion *Event;


extern EventCode EventName2Code(char *name);
extern char *EventCode2Name(EventCode code);
extern EventCode EventGetCode(Event event);
extern char *EventCode2Format(EventCode code);

extern Word AddrLabel(Addr addr);
extern char *LabelText(Word label);

extern Res EventRead(Event *eventOut, FILE *input);
extern void EventDestroy(Event event);

extern void EventRecord(Event event, Word etime);

extern void EventProcInit(Bool partial);
extern void EventProcFinish(void);


#endif /* eventpro_h */

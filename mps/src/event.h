/* impl.h.event -- Event Logging Interface
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!event.h(trunk.7) $
 *
 * READERSHIP
 *
 * .readership: MPS developers.
 *
 * DESIGN
 *
 * .design: design.mps.telemetry.
 */

#ifndef event_h
#define event_h

#include "mpm.h"
#include "eventcom.h"
#include "eventgen.h"

extern Res EventFlush(void);
extern Res EventInit(void);
extern void EventFinish(void);
extern Word EventControl(Word, Word);
extern Word EventInternString(const char *);
extern void EventLabelAddr(Addr, Word);

typedef Index EventKind;


/* Event Kinds --- see design.mps.telemetry
 *
 * All events are classified as being of one event type.
 * They are small enough to be able to be used as shifts within a word.
 */

#define EventKindArena      ((EventType)0) /* Per space or arena */
#define EventKindPool       ((EventType)1) /* Per pool */
#define EventKindTrace      ((EventType)2) /* Per trace or scan */
#define EventKindSeg        ((EventType)3) /* Per seg */
#define EventKindRef        ((EventType)4) /* Per ref or fix */
#define EventKindObject     ((EventType)5) /* Per alloc or object */
#define EventKindUser       ((EventType)6) /* User-invoked */

#define EventKindNumber     ((Count)7) /* Number of event kinds */


#ifdef EVENT

/* Note that enum values can be up to fifteen bits long portably. */

#define RELATION(type, code, always, kind, format) \
  enum { \
    Event ## type ## High = ((code >> 8) & 0xFF), \
    Event ## type ## Low = (code & 0xFF), \
    Event ## type ## Always = always,\
    Event ## type ## Kind = EventKind ## kind, \
    Event ## type ## Format = EventFormat ## format \
  }; 
  
#include "eventdef.h"

#undef RELATION


extern EventUnion Event;

#define EVENT_BEGIN(type, format, _length) \
  BEGIN \
    AVER(EventFormat ## format == Event ## type ## Format); \
    /* @@@@ As an interim measure, send the old event codes */ \
    Event.any.code = Event ## type; \
    /* @@@@ Length is in words, excluding header; this will change */ \
    /* We know that _length is aligned to word size */ \
    Event.any.length = ((_length / sizeof(Word)) - 3); \
    Event.any.clock = mps_clock(); 

#define EVENT_END(type, length) \
  if(BS_IS_MEMBER(EventKindControl, ((Index)Event ## type ## Kind))) { \
    AVER(EventNext <= EventLimit); \
    if((length) > (size_t)(EventLimit - EventNext)) \
      EventFlush(); /* @@@ should pass length */ \
    AVER((length) <= (size_t)(EventLimit - EventNext)); \
    MPS_MEMCPY(EventNext, &Event, (length)); \
    EventNext += (length); \
  } \
  END

extern char *EventNext, *EventLimit;
extern Word EventKindControl;

#else /* EVENT not */

#define EventInit()                        NOOP
#define EventFinish()                      NOOP

#endif /* EVENT */

#endif /* event_h */

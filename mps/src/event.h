/* impl.h.event -- Event Logging Interface
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!event.h(trunk.2) $
 *
 * .readership: MPS developers.
 * .sources: mps.design.event
 */

#ifndef event_h
#define event_h

#include "mpm.h"

extern Res EventFlush(void);
extern Res EventInit(void);
extern void EventFinish(void);

#include "eventgen.h"

#ifdef EVENT

extern EventUnion Event;

#define EVENT_BEGIN(type, _length) \
  BEGIN \
    unsigned _i; \
    Event.any.code = Event ## type; \
    /* @@@ Length is in words, excluding header; this will change */ \
    /* We know that _length is aligned to word size */ \
    Event.any.length = ((_length / sizeof(Word)) - 3); \
    Event.any.clock = mps_clock(); 

/* @@@@ We'd like to be able to use memcpy here, for performance.
 * We can't use structure copy because EventNext isn't guaranteed
 * to be aligned; we can't force it to be aligned without both
 * changing the log format and bloating its size.
 */

#define EVENT_END(type, length) \
  if(length > EventLimit - EventNext) \
    EventFlush(); /* @@@ should pass length */ \
  AVER((length) <= EventLimit - EventNext); \
  /* memcpy(EventNext, (char *)&Event, length); */ \
  for(_i = 0; _i < length; _i++) \
    EventNext[_i] = ((char *)&Event)[_i]; \
  EventNext += length; \
  END

extern char *EventNext, *EventLimit;

#define EVENT_0(type) \
  EVENT_BEGIN(type, sizeof(EventStruct)) \
  EVENT_END(type, sizeof(EventStruct))

#else /* EVENT not */

#define EventInit()                        NOOP
#define EventFinish()                      NOOP

#endif /* EVENT */

#endif /* event_h */

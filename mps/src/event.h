/* impl.h.event -- Event Logging Interface
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName: MMsrc!event.h(trunk.5) $
 *
 * READERSHIP
 *
 * .readership: MPS developers.
 *
 * DESIGN
 *
 * .design: design.mps.telemetry.
 *
 * TRANSGRESSIONS
 *
 * .trans.macro.memcpy: We define _memcpy (similar to ISO memcpy) as
 * a macro in this file.  This is done so that it gets optimised.
 * This is perhaps not a terribly good reason.
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

#define EventKindNumber     ((Count)6) /* Number of event kinds */


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


/* @@@@ We can't use memcpy, because it's a dependence on the ANSI C
 * library, despite the fact that many compilers will inline it.
 * Also, because we're always dealing with aligned words, we could 
 * copy more efficiently.
 */
/* see .trans.macro.memcpy */

#define _memcpy(to, from, length) \
  BEGIN \
    Index _i; \
    char *_to = (char *)(to); \
    char *_from = (char *)(from); \
    Count _length2 = (length); \
    for(_i = 0; _i < _length2; _i++) \
      _to[_i] = _from[_i]; \
  END

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
    _memcpy(EventNext, &Event, (length)); \
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

/* impl.h.event -- Event Logging Interface
 *
 * Copyright (C) 1997. Harlequin Group plc. All rights reserved.
 * $HopeName: MMsrc!event.h(trunk.14) $
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

#include "eventcom.h"
#include "mpm.h"


extern Res EventSync(void);
extern Res EventInit(void);
extern void EventFinish(void);
extern Word EventControl(Word, Word);
extern Word EventInternString(const char *);
extern Word EventInternGenString(size_t, const char *);
extern void EventLabelAddr(Addr, Word);


#ifdef EVENT


extern Res EventFlush(void);


/* Event Kinds --- see design.mps.telemetry
 *
 * All events are classified as being of one event type.
 * They are small enough to be able to be used as shifts within a word.
 */

#define EventKindArena      ((EventKind)0) /* Per space or arena */
#define EventKindPool       ((EventKind)1) /* Per pool */
#define EventKindTrace      ((EventKind)2) /* Per trace or scan */
#define EventKindSeg        ((EventKind)3) /* Per seg */
#define EventKindRef        ((EventKind)4) /* Per ref or fix */
#define EventKindObject     ((EventKind)5) /* Per alloc or object */
#define EventKindUser       ((EventKind)6) /* User-invoked */

#define EventKindNumber     ((Count)7) /* Number of event kinds */


/* Event type definitions
 *
 * Define various constants for each event type to describe them.
 */

/* Note that enum values can be up to fifteen bits long portably. */
#define RELATION(type, code, always, kind, format) \
  enum { \
    Event##type##High = ((code >> 8) & 0xFF), \
    Event##type##Low = (code & 0xFF), \
    Event##type##Always = always, \
    Event##type##Kind = EventKind##kind, \
    Event##type##Format = EventFormat##format \
  }; 
  
#include "eventdef.h"

#undef RELATION


/* Event writing support */

extern EventUnion EventMould;
extern char *EventNext, *EventLimit;
extern Word EventKindControl;

#define EVENT_BEGIN(type) \
  BEGIN \
    if(BS_IS_MEMBER(EventKindControl, ((Index)Event##type##Kind))) { \
      size_t _length;

#define EVENT_END(type, format, length) \
      AVER(EventFormat##format == Event##type##Format); \
      /* @@@@ As an interim measure, send the old event codes */ \
      EventMould.any.code = Event##type; \
      EventMould.any.clock = mps_clock(); \
      AVER(EventNext <= EventLimit); \
      _length = size_tAlignUp(length, sizeof(Word)); \
      if(_length > (size_t)(EventLimit - EventNext)) \
        EventFlush(); /* @@@@ should pass length */ \
      AVER(_length <= (size_t)(EventLimit - EventNext)); \
      mps_lib_memcpy(EventNext, &EventMould, _length); \
      EventNext += _length; \
    } \
  END


#else /* EVENT not */


#define EventInit()            NOOP
#define EventFinish()          NOOP
#define EventControl(r, f)     (UNUSED(r), UNUSED(f), (Word)0)
#define EventInternString(s)   (UNUSED(s), (Word)0)
#define EventInternGenString(l, s) (UNUSED(l), UNUSED(s), (Word)0)
#define EventLabelAddr(a, i)   BEGIN UNUSED(a); UNUSED(i); END


#endif /* EVENT */


#endif /* event_h */

/* impl.c.event: EVENT LOGGING
 *
 * $HopeName: MMsrc!event.c(trunk.13) $
 * Copyright (C) 1999 Harlequin Limited.  All rights reserved.
 *
 * .sources: mps.design.event
 *
 * TRANSGRESSIONS (rule.impl.trans)
 *
 * .trans.ref: The reference counting used to destroy the mps_io object 
 * isn't right.
 *
 * .trans.log: The log file will be re-created if the lifetimes of 
 * arenas don't overlap, but shared if they do.  mps_io_create cannot
 * be called twice, but EventInit avoids this anyway.
 *
 * .trans.ifdef: This file should logically be split into two, event.c
 * (which contains NOOP definitions, for general use) and eventdl.c, which
 * is specific to the logging variety and actually does logging (maybe).
 * Unfortunately, the build system doesn't really cope, and so this file
 * consists of two versions which are conditional on the EVENT symbol.
 */

#include "mpm.h"
#include "event.h"
#include "mpsio.h"

SRCID(event, "$HopeName: MMsrc!event.c(trunk.13) $");


#ifdef EVENT /* .trans.ifdef */


static Bool eventInited = FALSE;
static mps_io_t eventIO;
static char eventBuffer[EventBufferSIZE];
static Count eventUserCount;
static Serial EventInternSerial;

EventUnion EventMould; /* Used by macros in impl.h.event */
char *EventNext, *EventLimit; /* Used by macros in impl.h.event */
Word EventKindControl; /* Bit set used to control output. */


/* EventFlush -- flush event buffer to the event stream */

Res EventFlush(void)
{
  Res res;
  
  AVER(eventInited);

  res = (Res)mps_io_write(eventIO, (void *)eventBuffer,
                          EventNext - eventBuffer);
  EventNext = eventBuffer;
  if (res != ResOK) return res;

  return ResOK;
}


/* EventSync -- synchronize the event stream with the buffers */

Res EventSync(void)
{
  Res resEv, resIO;

  resEv = EventFlush();
  resIO = mps_io_flush(eventIO);
  return (resEv != ResOK) ? resEv : resIO;
}


/* EventInit -- start using the event system, initialize if necessary */

Res EventInit(void)
{
  Res res;

  /* Only if this is the first call. */
  if(!eventInited) { /* See .trans.log */
    AVER(EventNext == 0);
    AVER(EventLimit == 0);
    res = (Res)mps_io_create(&eventIO);
    if(res != ResOK) return res;
    EventNext = eventBuffer;
    EventLimit = &eventBuffer[EventBufferSIZE];
    eventUserCount = (Count)1;
    eventInited = TRUE;
    EventKindControl = (Word)mps_lib_telemetry_control();
    EventInternSerial = (Serial)1; /* 0 is reserved */
    (void)EventInternString(MPSVersion()); /* emit version */
  } else {
    ++eventUserCount;
  }

  return ResOK;
}


/* EventFinish -- stop using the event system */

void EventFinish(void)
{
  AVER(eventInited);
  AVER(eventUserCount > 0);

  (void)EventSync();

  --eventUserCount;
}


/* EventControl -- Change or read control word
 *
 * Resets the bits specified in resetMask, and flips those in
 * flipMask.  Returns old value.
 *
 * Operations can be implemented as follows:
 *   Set(M)   EventControl(M,M)
 *   Reset(M) EventControl(M,0)
 *   Flip(M)  EventControl(0,M)
 *   Read()   EventControl(0,0)
 */
   
Word EventControl(Word resetMask, Word flipMask)
{
  Word oldValue = EventKindControl;
      
  /* EventKindControl = (EventKindControl & ~resetMask) ^ flipMask */
  EventKindControl =
    BS_SYM_DIFF(BS_DIFF(EventKindControl, resetMask), flipMask);
       
  return oldValue;
}


/* EventInternString -- emit an Intern event on the (null-term) string given */

Word EventInternString(const char *label)
{
  Word id;

  AVER(label != NULL);

  id = (Word)EventInternSerial;
  ++EventInternSerial;
  EVENT_WS(Intern, id, StringLength(label), label);
  return id;
}


/* EventInternGenString -- emit an Intern event on the string given */

Word EventInternGenString(size_t len, const char *label)
{
  Word id;

  AVER(label != NULL);

  id = (Word)EventInternSerial;
  ++EventInternSerial;
  EVENT_WS(Intern, id, len, label);
  return id;
}


/* EventLabelAddr -- emit event to label address with the given id */

void EventLabelAddr(Addr addr, Word id)
{
  AVER((Serial)id < EventInternSerial);

  EVENT_AW(Label, addr, id);
}


#else /* EVENT, not */


Res (EventSync)(void)
{
  return(ResOK);  
}


Res (EventInit)(void)
{
  return(ResOK);  
}


void (EventFinish)(void)
{
  NOOP;
}


Word (EventControl)(Word resetMask, Word flipMask)
{
  UNUSED(resetMask);
  UNUSED(flipMask);

  return (Word)0;
}


Word (EventInternString)(const char *label)
{
  UNUSED(label);

  return (Word)0;
}


Word (EventInternGenString)(size_t len, const char *label)
{
  UNUSED(len); UNUSED(label);

  return (Word)0;
}


void (EventLabelAddr)(Addr addr, Word id)
{
  UNUSED(addr);
  UNUSED(id);
}


#endif /* EVENT */

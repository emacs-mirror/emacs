/* impl.c.event: EVENT LOGGING
 *
 * $HopeName$
 * Copyright (C) 1997, 1998 Harlequin Group plc.  All rights reserved.
 *
 * .readership: MPS developers.
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

SRCID(event, "$HopeName$");


#ifdef EVENT /* .trans.ifdef */


static Bool eventInited = FALSE;
static mps_io_t eventIO;
static char eventBuffer[EVENT_BUFFER_SIZE];
static Count eventUserCount;
static Serial EventInternSerial;

EventUnion Event; /* Used by macros in impl.h.event */
char *EventNext, *EventLimit; /* Used by macros in impl.h.event */
Word EventKindControl; /* Bit set used to control output. */


Res EventFlush(void)
{
  Res res;
  
  AVER(eventInited);

  res = (Res)mps_io_write(eventIO,
                          (void *)eventBuffer,
                          EventNext - eventBuffer);
  EventNext = eventBuffer;
  if(res != ResOK) return res;

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


Res (EventInit)(void)
{
  Res res;

  /* Initialize the event system if this is the first call. */
  if(!eventInited) { /* See .trans.log */
    AVER(EventNext == 0);
    AVER(EventLimit == 0);
    res = (Res)mps_io_create(&eventIO);
    if(res != ResOK) return res;
    EventNext = eventBuffer;
    EventLimit = &eventBuffer[EVENT_BUFFER_SIZE];
    eventUserCount = (Count)0;
    eventInited = TRUE;
    EventKindControl = (Word)mps_lib_telemetry_control();
  }

  ++eventUserCount;
  EventInternSerial = (Serial)0;

  return ResOK;
}


void (EventFinish)(void)
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


Word EventInternString(const char *label)
{
  Word id;

  AVER(label != NULL);

  id = (Word)EventInternSerial;
  ++EventInternSerial;
  EVENT_WS(Intern, id, label);
  return id;
}


void EventLabelAddr(Addr addr, Word id)
{
  AVER((Serial)id < EventInternSerial);

  EVENT_AW(Label, addr, id);
}


#else /* EVENT, not */


Res EventSync(void)
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


void (EventLabelAddr)(Addr addr, Word id)
{
  UNUSED(addr);
  UNUSED(id);
}


#endif /* EVENT */

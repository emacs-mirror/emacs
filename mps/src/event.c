/* impl.c.event: EVENT LOGGING
 *
 * $HopeName: MMsrc!event.c(trunk.2) $
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
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
 * spaces don't overlap, but shared if they do.  mps_io_create cannot
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

SRCID(event, "$HopeName: MMsrc!event.c(trunk.2) $");

#ifdef EVENT /* .trans.ifdef */

static Bool eventInited = FALSE;
static mps_io_t eventIO;
static char eventBuffer[EVENT_BUFFER_SIZE];
static Count eventUserCount;

EventUnion Event; /* Used by macros in impl.h.event */
char *EventNext, *EventLimit; /* Used by macros in impl.h.event */

Res EventFlush(void)
{
  Res res;
  
  AVER(eventInited);

  res = (Res)mps_io_write(eventIO,
                          (void *)eventBuffer,
                          EventNext - eventBuffer);
  if(res != ResOK) return res;

  EventNext = eventBuffer;

  return ResOK;
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
    eventUserCount = 0;
    eventInited = TRUE;
  }

  ++eventUserCount;

  return ResOK;
}

void (EventFinish)(void)
{
  AVER(eventInited);
  AVER(eventUserCount > 0);
  
  (void)EventFlush();
  (void)mps_io_flush(eventIO);

  --eventUserCount;
}

#else /* EVENT, not */

Res (EventInit)(void)
{
  return(ResOK);  
}

void (EventFinish)(void)
{
  NOOP;
}

#endif /* EVENT */

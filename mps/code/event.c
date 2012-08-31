/* event.c: EVENT LOGGING
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
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

SRCID(event, "$Id$");


#ifdef EVENT /* .trans.ifdef */


static Bool eventInited = FALSE;
static mps_io_t eventIO;
static char eventBuffer[EventBufferSIZE];
static Count eventUserCount;
static Serial EventInternSerial;

char *EventNext, *EventLimit; /* Used by macros in <code/event.h> */
EventControlSet EventKindControl; /* Bit set used to control output. */


/* EventFlush -- flush event buffer to the event stream */

Res EventFlush(void)
{
  Res res;
  size_t size;
 
  AVER(eventInited);

  AVER(eventBuffer <= EventNext);
  AVER(EventNext <= eventBuffer + EventBufferSIZE);
  size = (size_t)(EventNext - eventBuffer);

  res = (Res)mps_io_write(eventIO, (void *)eventBuffer, size);
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

  /* Make local enums for all event params in order to check that the indexes
     in the parameter definition macros are in order, and that parameter
     idents are unique. */

#define EVENT_CHECK_ENUM_PARAM(name, index, sort, ident) \
  Event##name##Param##ident,

#define EVENT_CHECK_ENUM(X, name, code, always, kind) \
  enum Event##name##ParamEnum { \
    EVENT_##name##_PARAMS(EVENT_CHECK_ENUM_PARAM, name) \
    Event##name##ParamLIMIT \
  };

  EVENT_LIST(EVENT_CHECK_ENUM, X)

  /* Check consistency of the event definitions.  These are all compile-time
     checks and should get optimised away. */

#define EVENT_PARAM_CHECK(name, index, sort, ident) \
  AVER(index == Event##name##Param##ident); \
  AVER(sizeof(EventF##sort) >= 0); /* check existence of type */

#define EVENT_CHECK(X, name, code, always, kind) \
  AVER(size_tAlignUp(sizeof(Event##name##Struct), MPS_PF_ALIGN) \
       <= EventSizeMAX); \
  AVER(Event##name##Code == code); \
  AVER(0 <= code && code <= EventCodeMAX); \
  AVER(sizeof(#name) - 1 <= EventNameMAX); \
  AVER(Event##name##Always == always); \
  AVERT(Bool, always); \
  AVER(0 <= Event##name##Kind && Event##name##Kind < EventKindLIMIT); \
  EVENT_##name##_PARAMS(EVENT_PARAM_CHECK, name)

  EVENT_LIST(EVENT_CHECK, X)

  /* Ensure that no event can be larger than the maximum event size. */
  AVER(EventBufferSIZE <= EventSizeMAX);

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
 *
 * FIXME: Candy-machine interface is a transgression.
 */
  
EventControlSet EventControl(EventControlSet resetMask,
                             EventControlSet flipMask)
{
  EventControlSet oldValue = EventKindControl;
     
  /* EventKindControl = (EventKindControl & ~resetMask) ^ flipMask */
  EventKindControl =
    BS_SYM_DIFF(BS_DIFF(EventKindControl, resetMask), flipMask);
      
  return oldValue;
}


/* EventInternString -- emit an Intern event on the (null-term) string given */

EventStringId EventInternString(const char *label)
{
  AVER(label != NULL);
  return EventInternGenString(StringLength(label), label);
}


/* EventInternGenString -- emit an Intern event on the string given */

EventStringId EventInternGenString(size_t len, const char *label)
{
  EventStringId id;

  AVER(label != NULL);

  id = EventInternSerial;
  ++EventInternSerial;

  EVENT2S(Intern, id, len, label);

  return id;
}


/* EventLabelAddr -- emit event to label address with the given id */

void EventLabelAddr(Addr addr, EventStringId id)
{
  AVER((Serial)id < EventInternSerial);

  EVENT2(Label, addr, id);
}


#else /* EVENT, not */


Res (EventSync)(void)
{
  return ResOK;
}


Res (EventInit)(void)
{
  return ResOK;
}


void (EventFinish)(void)
{
  NOOP;
}


EventControlSet (EventControl)(EventControlSet resetMask,
                               EventControlSet flipMask)
{
  UNUSED(resetMask);
  UNUSED(flipMask);
  return BS_EMPTY(EventControlSet);
}


EventStringId (EventInternString)(const char *label)
{
  UNUSED(label);
  NOTREACHED;
  return (EventInternString)0x9024EACH;
}


Word (EventInternGenString)(size_t len, const char *label)
{
  UNUSED(len); UNUSED(label);
  NOTREACHED;
  return (EventInternString)0x9024EACH;
}


void (EventLabelAddr)(Addr addr, Word id)
{
  UNUSED(addr);
  UNUSED(id);
  NOTREACHED;
}


#endif /* EVENT */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

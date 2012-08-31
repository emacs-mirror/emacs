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
static Count eventUserCount;
static Serial EventInternSerial;

char EventBuffer[EventBufferSIZE];      /* in which events are recorded */
char *EventLast;                        /* points to last written event */
EventControlSet EventKindControl;       /* Bit set used to control output. */


/* EventFlush -- flush event buffer to the event stream */

Res EventFlush(void)
{
  Res res;
  size_t size;

  AVER(eventInited);

  AVER(EventBuffer <= EventLast);
  AVER(EventLast <= EventBuffer + EventBufferSIZE);
  size = (size_t)(EventBuffer + EventBufferSIZE - EventLast);

  /* Writing might be faster if the size is aligned to a multiple of the
     C library or kernel's buffer size.  We could pad out the buffer with
     a marker for this purpose. */

  res = (Res)mps_io_write(eventIO, (void *)EventLast, size);

  /* Flush the in-memory buffer whether or not we succeeded, so that we can
     record recent events there. */
  EventLast = EventBuffer + EventBufferSIZE;

  return res;
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

#define EVENT_PARAM_CHECK_P(name, index, ident)
#define EVENT_PARAM_CHECK_A(name, index, ident)
#define EVENT_PARAM_CHECK_W(name, index, ident)
#define EVENT_PARAM_CHECK_U(name, index, ident)
#define EVENT_PARAM_CHECK_D(name, index, ident)
#define EVENT_PARAM_CHECK_B(name, index, ident)
#define EVENT_PARAM_CHECK_S(name, index, ident) \
  AVER(index + 1 == Event##name##ParamLIMIT); /* strings must come last */ \
  AVER(offsetof(Event##name##Struct, f##index.str) \
       + EventStringLengthMAX \
       + sizeof('\0') \
       <= EventSizeMAX);

#define EVENT_PARAM_CHECK(name, index, sort, ident) \
  AVER(index == Event##name##Param##ident); \
  AVER(sizeof(EventF##sort) >= 0); /* check existence of type */ \
  EVENT_PARAM_CHECK_##sort(name, index, ident)

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
    AVER(EventLast == NULL);
    res = (Res)mps_io_create(&eventIO);
    if(res != ResOK) return res;
    EventLast = EventBuffer + EventBufferSIZE;
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


/* Convert event parameter sort to WriteF arguments */

#define EVENT_WRITE_PARAM_MOST(name, index, sort, ident) \
  " $"#sort, (WriteF##sort)event->name.f##index,
#define EVENT_WRITE_PARAM_A EVENT_WRITE_PARAM_MOST
#define EVENT_WRITE_PARAM_P EVENT_WRITE_PARAM_MOST
#define EVENT_WRITE_PARAM_U EVENT_WRITE_PARAM_MOST
#define EVENT_WRITE_PARAM_W EVENT_WRITE_PARAM_MOST
#define EVENT_WRITE_PARAM_D EVENT_WRITE_PARAM_MOST
#define EVENT_WRITE_PARAM_B(name, index, sort, ident) \
  " $U", (WriteFU)event->name.f##index,
#define EVENT_WRITE_PARAM_S(name, index, sort, ident) \
  " $S", (WriteFS)event->name.f##index.str, /* FIXME: relies on NUL? */

Res EventDescribe(Event event, mps_lib_FILE *stream)
{
  Res res;

  /* FIXME: Some sort of EventCheck would be good */
  if (event == NULL)
    return ResFAIL;
  if (stream == NULL)
    return ResFAIL;

  res = WriteF(stream,
               "Event $P {\n", (WriteFP)event,
               NULL);
  if (res != ResOK) return res;

  switch (event->any.code) {

#define EVENT_DESC_PARAM(name, index, sort, ident) \
  "\n  $S", (WriteFS)#ident, \
  EVENT_WRITE_PARAM_##sort(name, index, sort, ident)

#define EVENT_DESC(X, name, _code, always, kind) \
  case _code: \
    res = WriteF(stream, \
                 "  code $U ($S)\n", (WriteFU)event->any.code, (WriteFS)#name, \
                 "  clock ", NULL); \
    if (res != ResOK) return res; \
    EVENT_CLOCK_WRITE(stream, event->any.clock); /* FIXME: return code */ \
    res = WriteF(stream, "\n  size $U", (WriteFU)event->any.size, \
                 EVENT_##name##_PARAMS(EVENT_DESC_PARAM, name) \
                 NULL); \
    if (res != ResOK) return res; \
    break;

  EVENT_LIST(EVENT_DESC, X)

  default:
    NOTREACHED; /* FIXME: should print .any info */
  }
  
  res = WriteF(stream,
               "\n} Event $P\n", (WriteFP)event,
               NULL);
  return res;
}


Res EventWrite(Event event, mps_lib_FILE *stream)
{
  Res res;
  
  if (event == NULL) return ResFAIL;
  if (stream == NULL) return ResFAIL;

  EVENT_CLOCK_WRITE(stream, event->any.clock); /* FIXME: return code */

  switch (event->any.code) {

#define EVENT_WRITE_PARAM(name, index, sort, ident) \
  EVENT_WRITE_PARAM_##sort(name, index, sort, ident)

#define EVENT_WRITE(X, name, code, always, kind) \
  case code: \
    res = WriteF(stream, " "#name, \
                 EVENT_##name##_PARAMS(EVENT_WRITE_PARAM, name) \
                 NULL); \
    if (res != ResOK) return res; \
    break;
  EVENT_LIST(EVENT_WRITE, X)

  default:
    res = WriteF(stream, " <unknown code $U>", event->any.code, NULL);
    if (res != ResOK) return res;
    /* FIXME: Should dump contents in hex. */
    break;
  }
  
  return ResOK;
}


void EventDump(mps_lib_FILE *stream)
{
  Event event;

  AVER(stream != NULL);

  for (event = (Event)EventLast;
       event < (Event)(EventBuffer + EventBufferSIZE);
       event = (Event)((char *)event + event->any.size)) {
    /* Try to keep going even if there's an error, because this is used as a
       backtrace and we'll take what we can get. */
    (void)EventWrite(event, stream);
    (void)WriteF(stream, "\n", NULL);
  }
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


Res EventDescribe(Event event, mps_lib_FILE *stream)
{
  UNUSED(event);
  UNUSED(stream);
  return ResUNIMPL;
}


Res EventWrite(Event event, mps_lib_FILE *stream)
{
  UNUSED(event);
  UNUSED(stream);
  return ResUNIMPL;
}


extern void EventDump(mps_lib_FILE *stream)
{
  UNUSED(stream);
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

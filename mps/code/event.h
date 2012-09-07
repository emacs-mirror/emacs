/* <code/event.h> -- Event Logging Interface
 *
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 * $Id$
 *
 * READERSHIP
 *
 * .readership: MPS developers.
 *
 * DESIGN
 *
 * .design: <design/telemetry/>.
 */

#ifndef event_h
#define event_h

#include "eventcom.h"
#include "mpm.h"
#include "eventdef.h"
#include "mpslib.h"


typedef Word EventStringId;
typedef Word EventControlSet;

extern void EventSync(void);
extern void EventInit(void);
extern void EventFinish(void);
extern EventControlSet EventControl(EventControlSet resetMask,
                                    EventControlSet flipMask);
extern EventStringId EventInternString(const char *label);
extern EventStringId EventInternGenString(size_t, const char *label);
extern void EventLabelAddr(Addr addr, Word id);
extern Res EventFlush(EventKind kind);
extern Res EventDescribe(Event event, mps_lib_FILE *stream);
extern Res EventWrite(Event event, mps_lib_FILE *stream);
extern void EventDump(mps_lib_FILE *stream);


#ifdef EVENT

/* Event writing support */

extern char EventBuffer[EventKindLIMIT][EventBufferSIZE];
extern char *EventLast[EventKindLIMIT];
extern Word EventKindControl;


/* Events are written into the buffer from the top down, so that a backtrace
   can find them all starting at EventLast. */

#define EVENT_BEGIN(name, structSize) \
  BEGIN \
    if(EVENT_ALL || Event##name##Always) { /* see config.h */ \
      Event##name##Struct *_event; \
      size_t _size = size_tAlignUp(structSize, MPS_PF_ALIGN); \
      if (_size > (size_t)(EventLast[Event##name##Kind] \
                           - EventBuffer[Event##name##Kind])) \
        EventFlush(Event##name##Kind); \
      AVER(_size <= (size_t)(EventLast[Event##name##Kind] \
                             - EventBuffer[Event##name##Kind])); \
      _event = (void *)(EventLast[Event##name##Kind] - _size); \
      _event->code = Event##name##Code; \
      _event->size = (EventSize)_size; \
      EVENT_CLOCK(_event->clock);

#define EVENT_END(name, size) \
      EventLast[Event##name##Kind] -= _size; \
    } \
  END


/* EVENTn -- event emitting macros
 *
 * The macros EVENT0, EVENT1, etc. are used throughout the MPS to emit an
 * event with parameters.  They work by appending the event parameters to
 * an event buffer, which is flushed to the telemetry output stream when
 * full.  EVENT2S is a special case that takes a variable length string.
 */

#define EVENT2S(name, p0, length, string) \
  BEGIN \
    size_t _string_len = (length); \
    size_t size; \
    AVER(_string_len <= EventStringLengthMAX); \
    size = offsetof(Event##name##Struct, f1) + _string_len + sizeof('\0'); \
    EVENT_BEGIN(name, size) \
      _event->f0 = (p0); \
      mps_lib_memcpy(_event->f1, (string), _string_len); \
      _event->f1[_string_len] = '\0'; \
    EVENT_END(name, size); \
  END


#define EVENT0(name) EVENT_BEGIN(name, sizeof(EventAnyStruct)) EVENT_END(name, sizeof(EventAnyStruct))
/* The following lines were generated with
   python -c 'for i in range(1,15): print "#define EVENT%d(name, %s) EVENT_BEGIN(name, sizeof(Event##name##Struct)) %s EVENT_END(name, sizeof(Event##name##Struct))" % (i, ", ".join(["p%d" % j for j in range(0, i)]), " ".join(["_event->f%d = (p%d);" % (j, j) for j in range(0, i)]))'
 */
#define EVENT1(name, p0) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT2(name, p0, p1) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT3(name, p0, p1, p2) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT4(name, p0, p1, p2, p3) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT5(name, p0, p1, p2, p3, p4) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT6(name, p0, p1, p2, p3, p4, p5) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT7(name, p0, p1, p2, p3, p4, p5, p6) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT8(name, p0, p1, p2, p3, p4, p5, p6, p7) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT9(name, p0, p1, p2, p3, p4, p5, p6, p7, p8) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT10(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT11(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); _event->f10 = (p10); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT12(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); _event->f10 = (p10); _event->f11 = (p11); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT13(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); _event->f10 = (p10); _event->f11 = (p11); _event->f12 = (p12); EVENT_END(name, sizeof(Event##name##Struct))
#define EVENT14(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); _event->f10 = (p10); _event->f11 = (p11); _event->f12 = (p12); _event->f13 = (p13); EVENT_END(name, sizeof(Event##name##Struct))


#else /* EVENT not */


#define EVENT0(name) NOOP
/* The following lines were generated with
  python -c 'for i in range(1,15): print "#define EVENT%d(name, %s) NOOP" % (i, ", ".join(["p%d" % j for j in range(0, i)]))'
 */
#define EVENT1(name, p0) NOOP
#define EVENT2(name, p0, p1) NOOP
#define EVENT3(name, p0, p1, p2) NOOP
#define EVENT4(name, p0, p1, p2, p3) NOOP
#define EVENT5(name, p0, p1, p2, p3, p4) NOOP
#define EVENT6(name, p0, p1, p2, p3, p4, p5) NOOP
#define EVENT7(name, p0, p1, p2, p3, p4, p5, p6) NOOP
#define EVENT8(name, p0, p1, p2, p3, p4, p5, p6, p7) NOOP
#define EVENT9(name, p0, p1, p2, p3, p4, p5, p6, p7, p8) NOOP
#define EVENT10(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9) NOOP
#define EVENT11(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) NOOP
#define EVENT12(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) NOOP
#define EVENT13(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) NOOP
#define EVENT14(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) NOOP


#endif /* EVENT */


#endif /* event_h */


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

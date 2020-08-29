/* <code/event.h> -- Event Logging Interface
 *
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 * $Id$
 *
 * READERSHIP
 *
 * .readership: MPS developers.
 *
 * DESIGN
 *
 * .design: <design/telemetry>.
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
extern void EventLabelPointer(Pointer pointer, Word id);
extern void EventFlush(EventKind kind);
extern Res EventDescribe(Event event, mps_lib_FILE *stream, Count depth);
extern Res EventWrite(Event event, mps_lib_FILE *stream);
extern void EventDump(mps_lib_FILE *stream);


#ifdef EVENT

/* Event writing support */

extern char EventBuffer[EventKindLIMIT][EventBufferSIZE];
extern char *EventLast[EventKindLIMIT];
extern Word EventKindControl;


/* EVENT_BEGIN -- flush buffer if necessary and write event header */

#define EVENT_BEGIN(name, structSize)                           \
  BEGIN                                                         \
    Event##name##Struct *_event;                                \
    EventKind _kind = Event##name##Kind;                        \
    size_t _size = size_tAlignUp(structSize, EVENT_ALIGN);      \
    AVER(Event##name##Used);                                    \
    if (_size > (size_t)(EventLast[Event##name##Kind]           \
                         - EventBuffer[Event##name##Kind]))     \
      EventFlush(Event##name##Kind);                            \
    AVER(_size <= (size_t)(EventLast[Event##name##Kind]         \
                           - EventBuffer[Event##name##Kind]));  \
    _event = (void *)(EventLast[Event##name##Kind] - _size);    \
    _event->code = Event##name##Code;                           \
    _event->size = (EventSize)_size;                            \
    EVENT_CLOCK(_event->clock);

#define EVENT_END                   \
    EventLast[_kind] -= _size;      \
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
      (void)mps_lib_memcpy(_event->f1, (string), _string_len); \
      _event->f1[_string_len] = '\0'; \
    EVENT_END; \
  END

#define EVENT0 EVENT_RECORD0
#define EVENT1 EVENT_RECORD1
#define EVENT2 EVENT_RECORD2
#define EVENT3 EVENT_RECORD3
#define EVENT4 EVENT_RECORD4
#define EVENT5 EVENT_RECORD5
#define EVENT6 EVENT_RECORD6
#define EVENT7 EVENT_RECORD7
#define EVENT8 EVENT_RECORD8
#define EVENT9 EVENT_RECORD9
#define EVENT10 EVENT_RECORD10
#define EVENT11 EVENT_RECORD11
#define EVENT12 EVENT_RECORD12
#define EVENT13 EVENT_RECORD13
#define EVENT14 EVENT_RECORD14

#else /* !EVENT */

#define EVENT0 EVENT_IGNORE0
#define EVENT1 EVENT_IGNORE1
#define EVENT2 EVENT_IGNORE2
#define EVENT3 EVENT_IGNORE3
#define EVENT4 EVENT_IGNORE4
#define EVENT5 EVENT_IGNORE5
#define EVENT6 EVENT_IGNORE6
#define EVENT7 EVENT_IGNORE7
#define EVENT8 EVENT_IGNORE8
#define EVENT9 EVENT_IGNORE9
#define EVENT10 EVENT_IGNORE10
#define EVENT11 EVENT_IGNORE11
#define EVENT12 EVENT_IGNORE12
#define EVENT13 EVENT_IGNORE13
#define EVENT14 EVENT_IGNORE14

#endif /* !EVENT */

#if EVENT_ALL

#define EVENT_CRITICAL0 EVENT0
#define EVENT_CRITICAL1 EVENT1
#define EVENT_CRITICAL2 EVENT2
#define EVENT_CRITICAL3 EVENT3
#define EVENT_CRITICAL4 EVENT4
#define EVENT_CRITICAL5 EVENT5
#define EVENT_CRITICAL6 EVENT6
#define EVENT_CRITICAL7 EVENT7
#define EVENT_CRITICAL8 EVENT8
#define EVENT_CRITICAL9 EVENT9
#define EVENT_CRITICAL10 EVENT10
#define EVENT_CRITICAL11 EVENT11
#define EVENT_CRITICAL12 EVENT12
#define EVENT_CRITICAL13 EVENT13
#define EVENT_CRITICAL14 EVENT14

#else /* !EVENT_ALL */

#define EVENT_CRITICAL0 EVENT_IGNORE0
#define EVENT_CRITICAL1 EVENT_IGNORE1
#define EVENT_CRITICAL2 EVENT_IGNORE2
#define EVENT_CRITICAL3 EVENT_IGNORE3
#define EVENT_CRITICAL4 EVENT_IGNORE4
#define EVENT_CRITICAL5 EVENT_IGNORE5
#define EVENT_CRITICAL6 EVENT_IGNORE6
#define EVENT_CRITICAL7 EVENT_IGNORE7
#define EVENT_CRITICAL8 EVENT_IGNORE8
#define EVENT_CRITICAL9 EVENT_IGNORE9
#define EVENT_CRITICAL10 EVENT_IGNORE10
#define EVENT_CRITICAL11 EVENT_IGNORE11
#define EVENT_CRITICAL12 EVENT_IGNORE12
#define EVENT_CRITICAL13 EVENT_IGNORE13
#define EVENT_CRITICAL14 EVENT_IGNORE14

#endif /* !EVENT_ALL */


/* The following lines were generated with
   python -c 'for i in range(15): print("#define EVENT_RECORD{i}(name{args}) EVENT_BEGIN(name, sizeof(Event##name##Struct)) {assign} EVENT_END\n#define EVENT_IGNORE{i}(name{args}) BEGIN {unused} END".format(i=i, args="".join(map(", p{}".format, range(i))), assign=" ".join(map("_event->f{0} = (p{0});".format, range(i))), unused=" ".join(map("UNUSED(p{});".format, range(i)))))'
 */
#define EVENT_RECORD0(name) EVENT_BEGIN(name, sizeof(Event##name##Struct))  EVENT_END
#define EVENT_IGNORE0(name) BEGIN  END
#define EVENT_RECORD1(name, p0) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); EVENT_END
#define EVENT_IGNORE1(name, p0) BEGIN UNUSED(p0); END
#define EVENT_RECORD2(name, p0, p1) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); EVENT_END
#define EVENT_IGNORE2(name, p0, p1) BEGIN UNUSED(p0); UNUSED(p1); END
#define EVENT_RECORD3(name, p0, p1, p2) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); EVENT_END
#define EVENT_IGNORE3(name, p0, p1, p2) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); END
#define EVENT_RECORD4(name, p0, p1, p2, p3) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); EVENT_END
#define EVENT_IGNORE4(name, p0, p1, p2, p3) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); END
#define EVENT_RECORD5(name, p0, p1, p2, p3, p4) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); EVENT_END
#define EVENT_IGNORE5(name, p0, p1, p2, p3, p4) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); END
#define EVENT_RECORD6(name, p0, p1, p2, p3, p4, p5) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); EVENT_END
#define EVENT_IGNORE6(name, p0, p1, p2, p3, p4, p5) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); UNUSED(p5); END
#define EVENT_RECORD7(name, p0, p1, p2, p3, p4, p5, p6) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); EVENT_END
#define EVENT_IGNORE7(name, p0, p1, p2, p3, p4, p5, p6) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); UNUSED(p5); UNUSED(p6); END
#define EVENT_RECORD8(name, p0, p1, p2, p3, p4, p5, p6, p7) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); EVENT_END
#define EVENT_IGNORE8(name, p0, p1, p2, p3, p4, p5, p6, p7) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); UNUSED(p5); UNUSED(p6); UNUSED(p7); END
#define EVENT_RECORD9(name, p0, p1, p2, p3, p4, p5, p6, p7, p8) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); EVENT_END
#define EVENT_IGNORE9(name, p0, p1, p2, p3, p4, p5, p6, p7, p8) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); UNUSED(p5); UNUSED(p6); UNUSED(p7); UNUSED(p8); END
#define EVENT_RECORD10(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); EVENT_END
#define EVENT_IGNORE10(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); UNUSED(p5); UNUSED(p6); UNUSED(p7); UNUSED(p8); UNUSED(p9); END
#define EVENT_RECORD11(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); _event->f10 = (p10); EVENT_END
#define EVENT_IGNORE11(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); UNUSED(p5); UNUSED(p6); UNUSED(p7); UNUSED(p8); UNUSED(p9); UNUSED(p10); END
#define EVENT_RECORD12(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); _event->f10 = (p10); _event->f11 = (p11); EVENT_END
#define EVENT_IGNORE12(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); UNUSED(p5); UNUSED(p6); UNUSED(p7); UNUSED(p8); UNUSED(p9); UNUSED(p10); UNUSED(p11); END
#define EVENT_RECORD13(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); _event->f10 = (p10); _event->f11 = (p11); _event->f12 = (p12); EVENT_END
#define EVENT_IGNORE13(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); UNUSED(p5); UNUSED(p6); UNUSED(p7); UNUSED(p8); UNUSED(p9); UNUSED(p10); UNUSED(p11); UNUSED(p12); END
#define EVENT_RECORD14(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) EVENT_BEGIN(name, sizeof(Event##name##Struct)) _event->f0 = (p0); _event->f1 = (p1); _event->f2 = (p2); _event->f3 = (p3); _event->f4 = (p4); _event->f5 = (p5); _event->f6 = (p6); _event->f7 = (p7); _event->f8 = (p8); _event->f9 = (p9); _event->f10 = (p10); _event->f11 = (p11); _event->f12 = (p12); _event->f13 = (p13); EVENT_END
#define EVENT_IGNORE14(name, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) BEGIN UNUSED(p0); UNUSED(p1); UNUSED(p2); UNUSED(p3); UNUSED(p4); UNUSED(p5); UNUSED(p6); UNUSED(p7); UNUSED(p8); UNUSED(p9); UNUSED(p10); UNUSED(p11); UNUSED(p12); UNUSED(p13); END

#endif /* event_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

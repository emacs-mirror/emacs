/* <code/eventcom.h> -- Event Logging Common Definitions
 *
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 * $Id$
 *
 * .sources: mps.design.telemetry
 */

#ifndef eventcom_h
#define eventcom_h

#include <limits.h>
#include "mpmtypes.h" /* for Word */
#include "eventdef.h"


/* Types for event fields */

typedef unsigned long long EventClock;
typedef unsigned short EventCode;
typedef unsigned EventKind;
typedef unsigned short EventSize;
#define EventSizeMAX USHRT_MAX

typedef Byte EventStringLen;

typedef struct {
  EventStringLen len;
  char str[EventStringLengthMAX];
} EventStringStruct;

typedef EventStringStruct *EventString;


#define EventNameMAX ((size_t)19)
#define EventCodeMAX ((EventCode)0x0069)


/* Event Kinds --- see <design/telemetry/>
 *
 * All events are classified as being of one event type.
 * They are small enough to be able to be used as shifts within a word.
 */

enum {
  EventKindArena,       /* Per space or arena */
  EventKindPool,        /* Per pool */
  EventKindTrace,       /* Per trace or scan */
  EventKindSeg,         /* Per seg */
  EventKindRef,         /* Per ref or fix */
  EventKindObject,      /* Per alloc or object */
  EventKindUser,        /* User-invoked */
  EventKindLIMIT
};


/* Event type definitions
 *
 * Define various constants for each event type to describe them.
 */

/* Note that enum values can be up to fifteen bits long portably. */
#define EVENT_ENUM(X, name, code, always, kind, count, format) \
  enum { \
    Event##name##Code = code, \
    Event##name##Always = always, \
    Event##name##Kind = EventKind##kind \
  };

EVENT_LIST(EVENT_ENUM, X)


/* Event*Struct -- Event Structures
 *
 * Declare the structures that are used to encode events in the internal event
 * buffers and on the binary telemetry output stream.
 */

/* Event field types -- similar to WriteF* */
typedef void *EventFP;
typedef Addr EventFA;
typedef Word EventFW;
typedef unsigned EventFU;
typedef EventStringStruct EventFS;
typedef double EventFD;

/*
for i in range(0, 15):
  print "#define EVENT%d_STRUCT(%s) struct { EventCode code; EventSize size; EventClock clock; %s }" % (
    i,
    ", ".join(["p%s" % j for j in range(0, i)]),
    " ".join("EventF##p%d f%d;" % (j, j) for j in range(0,i))
  )
 */
#define EVENT0_STRUCT() struct { EventCode code; EventSize size; EventClock clock;  }
#define EVENT1_STRUCT(p0) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; }
#define EVENT2_STRUCT(p0, p1) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; }
#define EVENT3_STRUCT(p0, p1, p2) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; }
#define EVENT4_STRUCT(p0, p1, p2, p3) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; }
#define EVENT5_STRUCT(p0, p1, p2, p3, p4) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; }
#define EVENT6_STRUCT(p0, p1, p2, p3, p4, p5) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; EventF##p5 f5; }
#define EVENT7_STRUCT(p0, p1, p2, p3, p4, p5, p6) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; EventF##p5 f5; EventF##p6 f6; }
#define EVENT8_STRUCT(p0, p1, p2, p3, p4, p5, p6, p7) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; EventF##p5 f5; EventF##p6 f6; EventF##p7 f7; }
#define EVENT9_STRUCT(p0, p1, p2, p3, p4, p5, p6, p7, p8) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; EventF##p5 f5; EventF##p6 f6; EventF##p7 f7; EventF##p8 f8; }
#define EVENT10_STRUCT(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; EventF##p5 f5; EventF##p6 f6; EventF##p7 f7; EventF##p8 f8; EventF##p9 f9; }
#define EVENT11_STRUCT(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; EventF##p5 f5; EventF##p6 f6; EventF##p7 f7; EventF##p8 f8; EventF##p9 f9; EventF##p10 f10; }
#define EVENT12_STRUCT(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; EventF##p5 f5; EventF##p6 f6; EventF##p7 f7; EventF##p8 f8; EventF##p9 f9; EventF##p10 f10; EventF##p11 f11; }
#define EVENT13_STRUCT(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; EventF##p5 f5; EventF##p6 f6; EventF##p7 f7; EventF##p8 f8; EventF##p9 f9; EventF##p10 f10; EventF##p11 f11; EventF##p12 f12; }
#define EVENT14_STRUCT(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) struct { EventCode code; EventSize size; EventClock clock; EventF##p0 f0; EventF##p1 f1; EventF##p2 f2; EventF##p3 f3; EventF##p4 f4; EventF##p5 f5; EventF##p6 f6; EventF##p7 f7; EventF##p8 f8; EventF##p9 f9; EventF##p10 f10; EventF##p11 f11; EventF##p12 f12; EventF##p13 f13; }

/* Common prefix for all event structures.  The size field allows an event
   reader to skip over events whose codes it does not recognise. */
typedef EVENT0_STRUCT() EventAnyStruct;

#define EVENT_STRUCT(X, name, _code, always, kind, count, format) \
  typedef EVENT##count##_STRUCT format Event##name##Struct;

EVENT_LIST(EVENT_STRUCT, X)


/* Event -- event union type
 *
 * Event is the type of a pointer to EventUnion, which is a union of all
 * event structures.  This can be used as the type of any event, decoded
 * by examining event->any.code.
 */

#define EVENT_UNION_MEMBER(X, name, code, always, kind, count, format) \
  Event##name##Struct name;

typedef union EventUnion {
  EventAnyStruct any;
  EVENT_LIST(EVENT_UNION_MEMBER, X)
} EventUnion, *Event;


#endif /* eventcom_h */


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

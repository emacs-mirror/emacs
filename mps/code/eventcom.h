/* <code/eventcom.h> -- Event Logging Common Definitions
 *
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * $Id$
 *
 * .sources: mps.design.telemetry
 */

#ifndef eventcom_h
#define eventcom_h

#include <limits.h>
#include "mpmtypes.h" /* for Word */
#include "eventdef.h"
#include "clock.h"


/* Event Kinds --- see <design/telemetry/>
 *
 * All events are classified as being of one event type.
 * They are small enough to be able to be used as members of a bit set.
 */

#define EventKindENUM(ENUM, X) \
  ENUM(X, Arena,  "Per space or arena") \
  ENUM(X, Pool,   "Per pool") \
  ENUM(X, Trace,  "Per trace or scan") \
  ENUM(X, Seg,    "Per seg") \
  ENUM(X, Ref,    "Per ref or fix") \
  ENUM(X, Object, "Per alloc or object") \
  ENUM(X, User,   "User-invoked")

#define ENUM_DECLARE(name) \
  enum name##Enum { \
    name##ENUM(ENUM_DECLARE_ROW, name) \
    name##LIMIT \
  };

#define ENUM_DECLARE_ROW(enumName, rowName, rowDoc) \
  enumName##rowName,

ENUM_DECLARE(EventKind)


/* Event type definitions
 *
 * Various constants for each event type to describe them, so that they
 * can easily be looked up from macros by name.
 */

/* Note that enum values can be up to fifteen bits long portably. */
#define EVENT_ENUM(X, name, code, always, kind) \
    Event##name##Code = code, \
    Event##name##Always = always, \
    Event##name##Kind = EventKind##kind,

enum EventDefinitionsEnum {
  EVENT_LIST(EVENT_ENUM, X)
  /* suppress comma-at-end-of-enum warning */
  EventEnumWarningSuppressor = USHRT_MAX
};


/* Event*Struct -- Event Structures
 *
 * Declare the structures that are used to encode events in the internal event
 * buffers and on the binary telemetry output stream.
 */

/* Types for common event fields */
typedef unsigned short EventCode;
typedef unsigned EventKind;
typedef unsigned short EventSize;
#define EventSizeMAX USHRT_MAX

/* Common prefix for all event structures.  The size field allows an event
   reader to skip over events whose codes it does not recognise. */
#define EVENT_ANY_FIELDS(X) \
  X(EventCode, code, "encoding of the event type") \
  X(EventSize, size, "allows reader to skip events of unknown code") \
  X(EventClock, clock, "when the event occurred")

#define EVENT_ANY_STRUCT_FIELD(TYPE, NAME, DOC) TYPE NAME;

typedef struct EventAnyStruct {
  EVENT_ANY_FIELDS(EVENT_ANY_STRUCT_FIELD)
} EventAnyStruct;

/* Event field types, for indexing by macro on the event parameter sort */
typedef void *EventFP;                  /* pointer to C object */
typedef Addr EventFA;                   /* address on the heap */
typedef Word EventFW;                   /* word */
typedef unsigned EventFU;               /* unsigned integer */
typedef char EventFS[EventStringLengthMAX + sizeof('\0')]; /* string */
typedef double EventFD;                 /* double */
typedef unsigned char EventFB;          /* Boolean */

/* Event packing bitfield specifiers */
#define EventFP_BITFIELD
#define EventFA_BITFIELD
#define EventFW_BITFIELD
#define EventFU_BITFIELD
#define EventFS_BITFIELD
#define EventFD_BITFIELD
#define EventFB_BITFIELD

#define EVENT_STRUCT_FIELD(X, index, sort, ident) \
  EventF##sort f##index EventF##sort##_BITFIELD;

#define EVENT_STRUCT(X, name, _code, always, kind) \
  typedef struct Event##name##Struct { \
    EVENT_ANY_FIELDS(EVENT_ANY_STRUCT_FIELD) \
    EVENT_##name##_PARAMS(EVENT_STRUCT_FIELD, X) \
  } Event##name##Struct;

EVENT_LIST(EVENT_STRUCT, X)

/* Maximum alignment requirement of any event type. */
#define EVENT_ALIGN (sizeof(EventFP))


/* Event -- event union type
 *
 * Event is the type of a pointer to EventUnion, which is a union of all
 * event structures.  This can be used as the type of any event, decoded
 * by examining event->any.code.
 */

#define EVENT_UNION_MEMBER(X, name, code, always, kind) \
  Event##name##Struct name;

typedef union EventUnion {
  EventAnyStruct any;
  EVENT_LIST(EVENT_UNION_MEMBER, X)
} EventUnion, *Event;


#endif /* eventcom_h */


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

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


/* EVENT_CLOCK -- fast event timestamp clock
 *
 * On platforms that support it, we want to stamp events with a very cheap
 * and fast high-resolution timer.
 */

/* http://msdn.microsoft.com/en-US/library/twchhe95%28v=vs.100%29.aspx */
#if (defined(MPS_ARCH_I3) || defined(MPS_ARCH_I6)) && defined(MPS_BUILD_MV)

#pragma intrinsic(__rdtsc)

typedef unsigned __int64 EventClock;

#define EVENT_CLOCK(lvalue) \
  BEGIN \
    (lvalue) = __rdtsc(); \
  END

#define EVENT_CLOCK_PRINT(stream, clock) fprintf(stream, "%llu", clock)
#if defined(MPS_ARCH_I3)
#define EVENT_CLOCK_WRITE(stream, clock) \
  WriteF(stream, "$W$W", (WriteFW)((clock) >> 32), (WriteFW)clock, NULL)
#else /* I6 */
  WriteF(stream, "$W", (WriteFW)(clock), NULL)
#endif

/* http://clang.llvm.org/docs/LanguageExtensions.html#builtins */
#elif defined(MPS_BUILD_LL)

#if __has_builtin(__builtin_readcyclecounter)

typedef unsigned long long EventClock;

#define EVENT_CLOCK(lvalue) \
  BEGIN \
    (lvalue) = __builtin_readcyclecounter(); \
  END

#define EVENT_CLOCK_PRINT(stream, clock) fprintf(stream, "%llu", clock)
/* FIXME: No EVENT_CLOCK_WRITE */

#endif /* __has_builtin(__builtin_readcyclecounter) */

#endif

/* Assemble the rdtsc instruction */
#if !defined(EVENT_CLOCK) && \
    (defined(MPS_ARCH_I3) || defined(MPS_ARCH_I6)) && \
      (defined(MPS_BUILD_GC) || defined(MPS_BUILD_LL))

/* Use __extension__ to enable use of a 64-bit type on 32-bit pedantic GCC */
__extension__ typedef unsigned long long EventClock;

#define EVENT_CLOCK(lvalue) \
  BEGIN \
    unsigned _l, _h; \
    __asm__ __volatile__("rdtsc" : "=a"(_l), "=d"(_h)); \
    (lvalue) = ((EventClock)_h << 32) | _l; \
  END

#define EVENT_CLOCK_PRINT(stream, clock) \
  fprintf(stream, "%08lX%08lX",  /* FIXME: Should be %llu */ \
          (unsigned long)((clock) >> 32), \
          (unsigned long)(clock))

#define EVENT_CLOCK_WRITE(stream, clock) \
  WriteF(stream, "$W$W", (WriteFW)((clock) >> 32), (WriteFW)clock, NULL)

#endif /* Intel, GCC or Clang */

/* no fast clock, use plinth, probably from the C library */
#ifndef EVENT_CLOCK

typedef Word EventClock;

#define EVENT_CLOCK(lvalue) \
  BEGIN \
    (lvalue) = mps_clock(); \
  END

#define EVENT_CLOCK_PRINT(stream, clock) \
  fprintf(stream, "%llu", (unsigned long long)clock)
/* FIXME: No EVENT_CLOCK_WRITE */

#endif


/* Event Kinds --- see <design/telemetry/>
 *
 * All events are classified as being of one event type.
 * They are small enough to be able to be used as members of a bit set.
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
 * Various constants for each event type to describe them, so that they
 * can easily be looked up from macros by name.
 */

/* Note that enum values can be up to fifteen bits long portably. */
#define EVENT_ENUM(X, name, code, always, kind) \
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

/* Types for common event fields */
typedef unsigned short EventCode;
typedef unsigned EventKind;
typedef unsigned short EventSize;
#define EventSizeMAX USHRT_MAX

/* FIXME: Is the length byte really necessary? Why not use the overall event
   size and NUL terminate? */
typedef Byte EventStringLen;
typedef struct EventStringStruct {
  EventStringLen len;
  char str[EventStringLengthMAX];
} EventStringStruct, *EventString;

/* Common prefix for all event structures.  The size field allows an event
   reader to skip over events whose codes it does not recognise. */
typedef struct EventAnyStruct {
  EventCode code;       /* encoding of the event type */
  EventSize size;       /* allows reader to skip events of unknown code */
  EventClock clock;     /* when the event occurred */
} EventAnyStruct;

/* Event field types, for indexing by macro on the event parameter sort */
typedef void *EventFP;                  /* pointer to C object */
typedef Addr EventFA;                   /* address on the heap */
typedef Word EventFW;                   /* word */
typedef unsigned EventFU;               /* unsigned integer */
typedef EventStringStruct EventFS;      /* string */
typedef double EventFD;                 /* double */
typedef int EventFB;                    /* boolean */

/* Event packing bitfield specifiers */
#define EventFP_BITFIELD
#define EventFA_BITFIELD
#define EventFW_BITFIELD
#define EventFU_BITFIELD
#define EventFS_BITFIELD
#define EventFD_BITFIELD
#define EventFB_BITFIELD : 1

#define EVENT_STRUCT_FIELD(X, index, sort, ident) \
  EventF##sort f##index EventF##sort##_BITFIELD;

#define EVENT_STRUCT(X, name, _code, always, kind) \
  typedef struct Event##name##Struct { \
    EventCode code;     /* Must match EventAnyStruct */ \
    EventSize size;     /* ditto */ \
    EventClock clock;   /* ditto */ \
    EVENT_##name##_PARAMS(EVENT_STRUCT_FIELD, X) \
  } Event##name##Struct;

EVENT_LIST(EVENT_STRUCT, X)


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

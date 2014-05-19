/* clock.h -- Fast clocks and timers
 *
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 * $Id$
 */

#ifndef clock_h
#define clock_h

#include "mpmtypes.h" /* for Word */


/* EVENT_CLOCK -- fast event timestamp clock
 *
 * On platforms that support it, we want to stamp events with a very cheap
 * and fast high-resolution timer.
 */

/* Microsoft C provides an intrinsic for the Intel rdtsc instruction.
   <http://msdn.microsoft.com/en-US/library/twchhe95%28v=vs.100%29.aspx> */
#if (defined(MPS_ARCH_I3) || defined(MPS_ARCH_I6)) && defined(MPS_BUILD_MV)

typedef unsigned __int64 EventClock;

typedef union EventClockUnion {
  struct {
    unsigned low, high;
  } half;
  unsigned __int64 whole;
} EventClockUnion;

#if _MSC_VER >= 1400

#pragma intrinsic(__rdtsc)

#define EVENT_CLOCK(lvalue) \
  BEGIN \
    (lvalue) = __rdtsc(); \
  END

#else /* _MSC_VER < 1400 */

/* This is mostly a patch for Open Dylan's bootstrap on Windows, which is
   using Microsoft Visual Studio 6 because of support for CodeView debugging
   information. */

#include "mpswin.h" /* KILL IT WITH FIRE! */

#define EVENT_CLOCK(lvalue) \
  BEGIN \
    LARGE_INTEGER _count; \
    QueryPerformanceCounter(&_count); \
    (lvalue) = _count.QuadPart; \
  END
  
#endif /* _MSC_VER < 1400 */

#if defined(MPS_ARCH_I3)

/* We can't use a shift to get the top half of the 64-bit event clock,
   because that introduces a dependency on `__aullshr` in the C run-time. */

#define EVENT_CLOCK_PRINT(stream, clock) \
  fprintf(stream, "%08lX%08lX", \
          (*(EventClockUnion *)&(clock)).half.high, \
          (*(EventClockUnion *)&(clock)).half.low)

#define EVENT_CLOCK_WRITE(stream, clock) \
  WriteF(stream, "$W$W", \
         (*(EventClockUnion *)&(clock)).half.high, \
         (*(EventClockUnion *)&(clock)).half.low, \
         NULL)

#elif defined(MPS_ARCH_I6)

#if defined(MPS_BUILD_MV)

#define EVENT_CLOCK_PRINT(stream, clock) \
  fprintf(stream, "%016llX", (clock));

#else

#define EVENT_CLOCK_PRINT(stream, clock) \
  fprintf(stream, "%016lX", (clock));

#endif

#define EVENT_CLOCK_WRITE(stream, clock) \
  WriteF(stream, "$W", (WriteFW)(clock), NULL)

#endif

#endif /* Microsoft C on Intel */

/* If we have GCC or Clang, assemble the rdtsc instruction */
#if !defined(EVENT_CLOCK) && \
    (defined(MPS_ARCH_I3) || defined(MPS_ARCH_I6)) && \
      (defined(MPS_BUILD_GC) || defined(MPS_BUILD_LL))

/* Use __extension__ to enable use of a 64-bit type on 32-bit pedantic
   GCC or Clang. */
__extension__ typedef unsigned long long EventClock;

/* Clang provides a cross-platform builtin for a fast timer, but it
   was not available on Mac OS X 10.8 until the release of XCode 4.6.
   <http://clang.llvm.org/docs/LanguageExtensions.html#builtins> */
#if defined(MPS_BUILD_LL)

#if __has_builtin(__builtin_readcyclecounter)

#define EVENT_CLOCK(lvalue) \
  BEGIN \
    (lvalue) = __builtin_readcyclecounter(); \
  END

#endif /* __has_builtin(__builtin_readcyclecounter) */

#endif /* Clang */

#ifndef EVENT_CLOCK

#define EVENT_CLOCK(lvalue) \
  BEGIN \
    unsigned _l, _h; \
    __asm__ __volatile__("rdtsc" : "=a"(_l), "=d"(_h)); \
    (lvalue) = ((EventClock)_h << 32) | _l; \
  END

#endif

/* The __extension__ keyword doesn't work on printf formats, so we
   concatenate two 32-bit hex numbers to print the 64-bit value. */
#define EVENT_CLOCK_PRINT(stream, clock) \
  fprintf(stream, "%08lX%08lX", \
          (unsigned long)((clock) >> 32), \
          (unsigned long)((clock) & 0xffffffff))

#define EVENT_CLOCK_WRITE(stream, clock) \
  WriteF(stream, "$W$W", (WriteFW)((clock) >> 32), (WriteFW)clock, NULL)

#endif /* Intel, GCC or Clang */

/* no fast clock, use plinth, probably from the C library */
#ifndef EVENT_CLOCK

typedef mps_clock_t EventClock;

#define EVENT_CLOCK(lvalue) \
  BEGIN \
    (lvalue) = mps_clock(); \
  END

#define EVENT_CLOCK_PRINT(stream, clock) \
  fprintf(stream, "%lu", (unsigned long)clock)

#define EVENT_CLOCK_WRITE(stream, clock) \
  WriteF(stream, "$W", (WriteFW)clock, NULL)

#endif


#endif /* clock_h */


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

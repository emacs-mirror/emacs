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


extern Res EventSync(void);
extern Res EventInit(void);
extern void EventFinish(void);
extern Word EventControl(Word, Word);
extern Word EventInternString(const char *);
extern Word EventInternGenString(size_t, const char *);
extern void EventLabelAddr(Addr, Word);


#ifdef EVENT


extern Res EventFlush(void);


/* Event writing support */

extern char *EventNext, *EventLimit;
extern Word EventKindControl;

#define EVENT_BEGIN(name, size) \
  BEGIN \
    if(BS_IS_MEMBER(EventKindControl, ((Index)Event##name##Kind))) { \
      Event##name##Struct *_event; \
      size_t _size = size_tAlignUp(size, MPS_PF_ALIGN); \
      if (_size > (size_t)(EventLimit - EventNext)) \
        EventFlush(); \
      AVER(_size <= (size_t)(EventLimit - EventNext)); \
      _event = (void *)EventNext; \
      _event->code = Event##name; \
      _event->clock = mps_clock();

#define EVENT_END(name, size) \
      EventNext += _size; \
    } \
  END


#else /* EVENT not */


#define EventInit()            NOOP
#define EventFinish()          NOOP
#define EventControl(r, f)     (UNUSED(r), UNUSED(f), (Word)0)
#define EventInternString(s)   (UNUSED(s), (Word)0)
#define EventInternGenString(l, s) (UNUSED(l), UNUSED(s), (Word)0)
#define EventLabelAddr(a, i)   BEGIN UNUSED(a); UNUSED(i); END


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

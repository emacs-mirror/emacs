/* eventpro.h: Interface for event processing routines
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * $Id$
 */

#ifndef eventpro_h
#define eventpro_h

#include "config.h"
/* override variety setting for EVENT */
#define EVENT

#include "eventcom.h"
#include "mpmtypes.h"


typedef struct EventProcStruct *EventProc;
typedef Res (*EventProcReader)(void *, void *, size_t);


extern EventCode EventName2Code(char *name);
extern char *EventCode2Name(EventCode code);
extern EventCode EventGetCode(Event event);
extern char *EventCode2Format(EventCode code);
extern Bool EventCodeIsValid(EventCode code);

extern Word AddrLabel(EventProc proc, Addr addr);
extern EventString LabelText(EventProc proc, Word label);

extern Res EventRead(Event *eventReturn, EventProc proc);
extern void EventDestroy(EventProc proc, Event event);

extern Res EventRecord(EventProc proc, Event event, Word etime);

extern Res EventProcCreate(EventProc *procReturn, Bool partial,
                           EventProcReader reader, void *readerP);
extern void EventProcDestroy(EventProc proc);


#endif /* eventpro_h */


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

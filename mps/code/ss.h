/* ss.h: STACK SCANNING
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * This module saves the mutator context on entry to the MPS, and
 * provides functions for decoding the context and scanning the root
 * registers. See <design/ss/>.
 */

#ifndef ss_h
#define ss_h

#include "mpm.h"

     
/* StackContext -- structure containing the callee-save registers and
 * the stack pointer */

#include <setjmp.h>

typedef struct StackContextStruct {
  jmp_buf jumpBuffer;
} StackContextStruct;


/* STACK_CONTEXT_BEGIN -- enter arena and save context */

#define STACK_CONTEXT_BEGIN(arena) BEGIN \
  StackContextStruct _sc; \
  ArenaEnter(arena); \
  STACK_CONTEXT_SAVE(&_sc); \
  AVER(arena->scAtArenaEnter == NULL); \
  arena->scAtArenaEnter = &_sc; \
  BEGIN


/* STACK_CONTEXT_END -- clear context and leave arena */

#define STACK_CONTEXT_END(arena) END; \
  arena->scAtArenaEnter = NULL; \
  ArenaLeave(arena); \
  END


/* StackContextStackTop -- return the "top" of the mutator's stack at
 * the point when the context was saved by STACK_CONTEXT_SAVE. */

extern Addr *StackContextStackTop(StackContext sc);


/* StackContextScan -- scan references in the stack context */

extern Res StackContextScan(ScanState ss, StackContext sc);


/* STACK_CONTEXT_SAVE -- save the callee-saves and stack pointer */

#if defined(MPS_OS_XC)

/* We call _setjmp rather than setjmp because _setjmp saves only the
 * register set and the stack while setjmp also saves the signal mask.
 * See _setjmp(2). */

#define STACK_CONTEXT_SAVE(sc) ((void)_setjmp((sc)->jumpBuffer))

#else  /* other platforms */

#define STACK_CONTEXT_SAVE(sc) ((void)setjmp((sc)->jumpBuffer))

#endif /* platform defines */


/* StackScan -- scan the mutator's stack and registers */

extern Res StackScan(ScanState ss, Addr *stackBot);


#endif /* ss_h */


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

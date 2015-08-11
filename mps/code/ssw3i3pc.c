/* ssw3i3pc.c: STACK SCANNING FOR WINDOWS ON IA-32 WITH PELLES C
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * This decodes the stack context and scans the registers which may
 * contain roots. See <design/ss/>.
 *
 * .assume.ms-compat: We rely on the fact that Pelles C's setjmp stores
 * the callee-save registers in the jmp_buf and is compatible with Microsoft
 * C.  The Pelles C 7.00 setjmp.h header has a comment "MS compatible".  See
 * also "Is Pelles C's jmp_buf compatible with Microsoft C's?"
 * <http://forum.pellesc.de/index.php?topic=5464>
 *
 * REFERENCES
 *
 * "Argument Passing and Naming Conventions"; MSDN; Microsoft Corporation;
 * <http://msdn.microsoft.com/en-us/library/984x0h58%28v=vs.100%29.aspx>.
 *
 * "Calling conventions for different C++ compilers and operating systems";
 * Agner Fog; Copenhagen University College of Engineering; 2014-08-07;
 * <http://agner.org/optimize/calling_conventions.pdf>.
 */

#include "mpm.h"
#include <setjmp.h>

SRCID(ssw3i3pc, "$Id$");

#if !defined(MPS_PF_W3I3PC)
#error "ssw3i3pc.c is specific to MPS_PF_W3I3PC."
#endif


/* This definition isn't in the Pelles C headers, so we reproduce it here.
 * See .assume.ms-compat. */

typedef struct __JUMP_BUFFER {
    unsigned long Ebp;
    unsigned long Ebx;
    unsigned long Edi;
    unsigned long Esi;
    unsigned long Esp;
    unsigned long Eip;
    unsigned long Registration;
    unsigned long TryLevel;
    unsigned long Cookie;
    unsigned long UnwindFunc;
    unsigned long UnwindData[6];
} _JUMP_BUFFER;


/* StackContextStackTop -- return the "top" of the mutator's stack at
 * the point when the context was saved by STACK_CONTEXT_SAVE. */

Addr *StackContextStackTop(StackContext sc)
{
  _JUMP_BUFFER *jb = (_JUMP_BUFFER *)&sc->jumpBuffer;
  Addr **p_esp = (void *)&jb->Esp;
  return *p_esp;
}


/* StackContextScan -- scan references in the stack context */

Res StackContextScan(ScanState ss, StackContext sc)
{
  /* .assume.ms-compat */
  _JUMP_BUFFER *jb = (_JUMP_BUFFER *)&sc->jumpBuffer;
  Addr *p_ebx = (void *)&jb->Ebx;

  /* Ensure that the callee-save registers will be found by
     TraceScanAreaTagged when it's passed the address of the Ebx
     field. */
  AVER(offsetof(_JUMP_BUFFER, Edi) == offsetof(_JUMP_BUFFER, Ebx) + 4);
  AVER(offsetof(_JUMP_BUFFER, Esi) == offsetof(_JUMP_BUFFER, Ebx) + 8);

  return TraceScanAreaTagged(ss, p_ebx, p_ebx + 3);
}


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

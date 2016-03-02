/* ssw3i3mv.c: STACK SCANNING FOR WINDOWS ON IA-32 WITH MICROSOFT VISUAL C
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * This decodes the stack context and scans the registers which may
 * contain roots. See <design/ss/>.
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

SRCID(ssw3i3mv, "$Id$");

#if !defined(MPS_PF_W3I3MV)
#error "ssw3i3mv.c is specific to MPS_PF_W3I3MV."
#endif


/* StackContextStackHot -- hot end of the mutator's stack
 *
 * Retrieves the stack pointer at the point when the context was saved
 * by STACK_CONTEXT_SAVE.
 */

Word *StackContextStackHot(StackContext sc)
{
  _JUMP_BUFFER *jb = (_JUMP_BUFFER *)&sc->jumpBuffer;
  Word **p_esp = (void *)&jb->Esp;
  return *p_esp;
}


/* StackContextScan -- scan references in the stack context */

Res StackContextScan(ScanState ss, StackContext sc,
                     mps_area_scan_t scan_area, void *closure)
{
  _JUMP_BUFFER *jb = (_JUMP_BUFFER *)&sc->jumpBuffer;
  Word *p_ebx = (void *)&jb->Ebx;

  /* These checks will just serve to warn us at compile-time if the
     setjmp.h header changes to indicate that the registers we want aren't
     saved any more. */
  AVER(sizeof jb->Edi == sizeof(Word));
  AVER(sizeof jb->Esi == sizeof(Word));
  AVER(sizeof jb->Ebx == sizeof(Word));

  /* Ensure that the callee-save registers will be found by
     TraceScanArea when it's passed the address of the Ebx
     field. */
  AVER(offsetof(_JUMP_BUFFER, Edi) == offsetof(_JUMP_BUFFER, Ebx) + 4);
  AVER(offsetof(_JUMP_BUFFER, Esi) == offsetof(_JUMP_BUFFER, Ebx) + 8);

  return TraceScanArea(ss, p_ebx, p_ebx + 3, scan_area, closure);
}


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

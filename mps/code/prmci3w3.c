/* prmci3w3.c: PROTECTION MUTATOR CONTEXT INTEL 386 (Win32)
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * PURPOSE
 *
 * .purpose: This module implements the part of the protection module
 * that decodes the MutatorFaultContext. 
 *
 * SOURCES
 *
 * .source.i486: Intel486 Microprocessor Family Programmer's
 * Reference Manual (book.intel92).
 *
 * ASSUMPTIONS
 *
 * .assume.regref: The resisters in the context can be modified by
 * storing into an MRef pointer.
 */

#include "prmcw3.h"
#include "prmci3.h"
#include "mpm.h"

SRCID(prmci3w3, "$Id$");


/* Prmci3AddressHoldingReg -- Return an address for a given machine register */

MRef Prmci3AddressHoldingReg(MutatorFaultContext context, unsigned int regnum)
{
  PCONTEXT wincont;

  AVER(regnum <= 7);
  AVER(regnum >= 0);

  wincont = context->ep->ContextRecord;

  switch (regnum) {
  case 0: return (MRef)&wincont->Eax;
  case 1: return (MRef)&wincont->Ecx;
  case 2: return (MRef)&wincont->Edx;
  case 3: return (MRef)&wincont->Ebx;
  case 4: return (MRef)&wincont->Esp;
  case 5: return (MRef)&wincont->Ebp;
  case 6: return (MRef)&wincont->Esi;
  case 7: return (MRef)&wincont->Edi;
  }
  NOTREACHED;
  return NULL; /* suppress warning */
}


/* Prmci3DecodeFaultContext -- decode fault context */

void Prmci3DecodeFaultContext(MRef *faultmemReturn, Byte **insvecReturn,
                              MutatorFaultContext context)
{
  LPEXCEPTION_RECORD er;

  er = context->ep->ExceptionRecord;

  /* Assert that this is an access violation.  The computation of */
  /* faultmem depends on this. */
  AVER(er->ExceptionCode == EXCEPTION_ACCESS_VIOLATION);

  *faultmemReturn = (MRef)er->ExceptionInformation[1];
  *insvecReturn = (Byte*)context->ep->ContextRecord->Eip;
}


/* Prmci3StepOverIns -- skip an instruction by changing the context */

void Prmci3StepOverIns(MutatorFaultContext context, Size inslen)
{
  context->ep->ContextRecord->Eip += (DWORD)inslen;
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

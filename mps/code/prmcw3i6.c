/* prmcw3i6.c: PROTECTION MUTATOR CONTEXT INTEL x64 (Windows)
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * PURPOSE
 *
 * .purpose: This module implements the part of the protection module
 * that decodes the MutatorContext. 
 *
 * SOURCES
 *
 *
 * ASSUMPTIONS
 *
 * .assume.regref: The registers in the context can be modified by
 * storing into an MRef pointer.
 */

#include "prmcw3.h"
#include "prmci6.h"
#include "mpm.h"

SRCID(prmcw3i6, "$Id$");

#if !defined(MPS_OS_W3) || !defined(MPS_ARCH_I6)
#error "prmcw3i6.c is specific to MPS_OS_W3 and MPS_ARCH_I6"
#endif


/* Prmci6AddressHoldingReg -- Return an address for a given machine register */

MRef Prmci6AddressHoldingReg(MutatorContext context, unsigned int regnum)
{
  PCONTEXT wincont;

  AVER(NONNEGATIVE(regnum));
  AVER(regnum <= 16);

  wincont = context->ep->ContextRecord;

  switch (regnum) {
  case  0: return (MRef)&wincont->Rax;
  case  1: return (MRef)&wincont->Rcx;
  case  2: return (MRef)&wincont->Rdx;
  case  3: return (MRef)&wincont->Rbx;
  case  4: return (MRef)&wincont->Rsp;
  case  5: return (MRef)&wincont->Rbp;
  case  6: return (MRef)&wincont->Rsi;
  case  7: return (MRef)&wincont->Rdi;
  case  8: return (MRef)&wincont->R8;
  case  9: return (MRef)&wincont->R9;
  case 10: return (MRef)&wincont->R10;
  case 11: return (MRef)&wincont->R11;
  case 12: return (MRef)&wincont->R12;
  case 13: return (MRef)&wincont->R13;
  case 14: return (MRef)&wincont->R14;
  case 15: return (MRef)&wincont->R15;
  default:
    NOTREACHED;
    return NULL; /* suppress warning */
  }
}


/* Prmci6DecodeFaultContext -- decode fault context */

void Prmci6DecodeFaultContext(MRef *faultmemReturn, Byte **insvecReturn,
                              MutatorContext context)
{
  LPEXCEPTION_RECORD er;

  er = context->ep->ExceptionRecord;

  /* Assert that this is an access violation.  The computation of */
  /* faultmem depends on this. */
  AVER(er->ExceptionCode == EXCEPTION_ACCESS_VIOLATION);

  *faultmemReturn = (MRef)er->ExceptionInformation[1];
  *insvecReturn = (Byte*)context->ep->ContextRecord->Rip;
}


/* Prmci6StepOverIns -- skip an instruction by changing the context */

void Prmci6StepOverIns(MutatorContext context, Size inslen)
{
  context->ep->ContextRecord->Rip += (DWORD64)inslen;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

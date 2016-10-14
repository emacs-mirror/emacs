/* prmcxci6.c: MUTATOR CONTEXT x64 (OS X)
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Implement the mutator context module. See <design/prmc/>.
 *
 *
 * SOURCES
 *
 *
 * ASSUMPTIONS
 *
 * .sp: The stack pointer in the context is RSP.
 *
 * .assume.regref: The registers in the context can be modified by
 * storing into an MRef pointer.
 */

#include "prmcxc.h"
#include "prmci6.h"

SRCID(prmcxci6, "$Id$");

#if !defined(MPS_OS_XC) || !defined(MPS_ARCH_I6)
#error "prmcxci6.c is specific to MPS_OS_XC and MPS_ARCH_I6"
#endif


/* Prmci6AddressHoldingReg -- return an address of a register in a context */

MRef Prmci6AddressHoldingReg(MutatorContext context, unsigned int regnum)
{
  THREAD_STATE_S *threadState;

  AVERT(MutatorContext, context);
  AVER(NONNEGATIVE(regnum));
  AVER(regnum <= 15);

  threadState = context->threadState;

  /* .assume.regref */
  /* The register numbers (REG_RAX etc.) are defined in <ucontext.h>
     but only if _XOPEN_SOURCE is defined: see .feature.xc in
     config.h. */
  /* MRef (a Word *) is not compatible with pointers to the register
     types (actually a __uint64_t). To avoid aliasing optimization
     problems, the registers are cast through (void *). */
  switch (regnum) {
    case  0: return (void *)&threadState->__rax;
    case  1: return (void *)&threadState->__rcx;
    case  2: return (void *)&threadState->__rdx;
    case  3: return (void *)&threadState->__rbx;
    case  4: return (void *)&threadState->__rsp;
    case  5: return (void *)&threadState->__rbp;
    case  6: return (void *)&threadState->__rsi;
    case  7: return (void *)&threadState->__rdi;
    case  8: return (void *)&threadState->__r8;
    case  9: return (void *)&threadState->__r9;
    case 10: return (void *)&threadState->__r10;
    case 11: return (void *)&threadState->__r11;
    case 12: return (void *)&threadState->__r12;
    case 13: return (void *)&threadState->__r13;
    case 14: return (void *)&threadState->__r14;
    case 15: return (void *)&threadState->__r15;
    default:
      NOTREACHED;
      return NULL;  /* Avoids compiler warning. */
  }
}


/* Prmci6DecodeFaultContext -- decode fault to find faulting address and IP */

void Prmci6DecodeFaultContext(MRef *faultmemReturn,
                              Byte **insvecReturn,
                              MutatorContext context)
{
  AVER(faultmemReturn != NULL);
  AVER(insvecReturn != NULL);
  AVERT(MutatorContext, context);
  AVER(context->var == MutatorContextFAULT);

  *faultmemReturn = (MRef)context->address;
  *insvecReturn = (Byte*)context->threadState->__rip;
}


/* Prmci6StepOverIns -- modify context to step over instruction */

void Prmci6StepOverIns(MutatorContext context, Size inslen)
{
  AVERT(MutatorContext, context);
  AVER(0 < inslen);

  context->threadState->__rip += (Word)inslen;
}


Addr MutatorContextSP(MutatorContext context)
{
  AVERT(MutatorContext, context);

  return (Addr)context->threadState->__rsp;
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

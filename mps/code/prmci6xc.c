/* prmci6xc.c: PROTECTION MUTATOR CONTEXT x64 (MAC OS X)
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This module implements the part of the protection module
 * that decodes the MutatorFaultContext. 
 *
 *
 * SOURCES
 *
 * .source.linux.kernel: Linux kernel source files.
 *
 *
 * ASSUMPTIONS
 *
 * .context.regroots: The root regs are assumed to be recorded in the context
 * at pointer-aligned boundaries.
 *
 * .assume.regref: The resisters in the context can be modified by
 * storing into an MRef pointer.
 */

#include "prmcxc.h"
#include "prmci6.h"

SRCID(prmci6li, "$Id$");


/* Prmci6AddressHoldingReg -- return an address of a register in a context */

MRef Prmci6AddressHoldingReg(MutatorFaultContext mfc, unsigned int regnum)
{
  AVER(NONNEGATIVE(regnum));
  AVER(regnum <= 15);

  /* .assume.regref */
  /* The register numbers (REG_RAX etc.) are defined in <ucontext.h>
     but only if _XOPEN_SOURCE is defined: see .feature.xc in
     config.h. */
  /* MRef (a Word *) is not compatible with pointers to the register
     types (actually a __uint64_t).  To avoid aliasing optimization
     problems, The registers are cast through (char *) */
  switch (regnum) {
    case  0: return (MRef)((char *)&mfc->threadState->__rax);
    case  1: return (MRef)((char *)&mfc->threadState->__rcx);
    case  2: return (MRef)((char *)&mfc->threadState->__rdx);
    case  3: return (MRef)((char *)&mfc->threadState->__rbx);
    case  4: return (MRef)((char *)&mfc->threadState->__rsp);
    case  5: return (MRef)((char *)&mfc->threadState->__rbp);
    case  6: return (MRef)((char *)&mfc->threadState->__rsi);
    case  7: return (MRef)((char *)&mfc->threadState->__rdi);
    case  8: return (MRef)((char *)&mfc->threadState->__r8);
    case  9: return (MRef)((char *)&mfc->threadState->__r9);
    case 10: return (MRef)((char *)&mfc->threadState->__r10);
    case 11: return (MRef)((char *)&mfc->threadState->__r11);
    case 12: return (MRef)((char *)&mfc->threadState->__r12);
    case 13: return (MRef)((char *)&mfc->threadState->__r13);
    case 14: return (MRef)((char *)&mfc->threadState->__r14);
    case 15: return (MRef)((char *)&mfc->threadState->__r15);
    default:
      NOTREACHED;
      return NULL;  /* Avoids compiler warning. */
  }
}


/* Prmci3DecodeFaultContext -- decode fault to find faulting address and IP */

void Prmci6DecodeFaultContext(MRef *faultmemReturn,
                              Byte **insvecReturn,
                              MutatorFaultContext mfc)
{
  *faultmemReturn = (MRef)mfc->address;
  *insvecReturn = (Byte*)mfc->threadState->__rip;
}


/* Prmci3StepOverIns -- modify context to step over instruction */

void Prmci6StepOverIns(MutatorFaultContext mfc, Size inslen)
{
  mfc->threadState->__rip += (Word)inslen;
}


Addr MutatorFaultContextSP(MutatorFaultContext mfc)
{
  return (Addr)mfc->threadState->__rsp;
}


Res MutatorFaultContextScan(ScanState ss, MutatorFaultContext mfc)
{
  x86_thread_state64_t *mc;
  Res res;

  /* This scans the root registers (.context.regroots).  It also
     unnecessarily scans the rest of the context.  The optimisation
     to scan only relevant parts would be machine dependent. */
  mc = mfc->threadState;
  res = TraceScanAreaTagged(ss,
                            (Addr *)mc,
                            (Addr *)((char *)mc + sizeof(*mc)));
  return res;
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

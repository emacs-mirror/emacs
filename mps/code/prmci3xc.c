/* prmci3xc.c: PROTECTION MUTATOR CONTEXT INTEL 386 (MAC OS X)
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This module implements the part of the protection module
 * that decodes the MutatorFaultContext. 
 *
 *
 * SOURCES
 *
 * .source.i486: Intel486 Microprocessor Family Programmer's
 * Reference Manual
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

#include "prmcix.h"
#include "prmci3.h"

SRCID(prmci3li, "$Id$");


/* Prmci3AddressHoldingReg -- return an address of a register in a context */

MRef Prmci3AddressHoldingReg(MutatorFaultContext mfc, unsigned int regnum)
{
  AVER(regnum <= 7);
  AVER(regnum >= 0);

  /* .source.i486 */
  /* .assume.regref */
  /* The register numbers (REG_EAX etc.) are defined in <ucontext.h>
     but only if _GNU_SOURCE is defined: see .feature.li in
     config.h. */
  /* TODO: The current arrangement of the fix operation (taking a Ref *)
     forces us to pun these registers (actually `int` on LII3GC).  We can
     suppress the warning my casting through `char *` and this might make
     it safe, but does it really?  RB 2012-09-10 */
  switch (regnum) {
    case 0: return (MRef)((char *)&mfc->ucontext->uc_mcontext.gregs[REG_EAX]);
    case 1: return (MRef)((char *)&mfc->ucontext->uc_mcontext.gregs[REG_ECX]);
    case 2: return (MRef)((char *)&mfc->ucontext->uc_mcontext.gregs[REG_EDX]);
    case 3: return (MRef)((char *)&mfc->ucontext->uc_mcontext.gregs[REG_EBX]);
    case 4: return (MRef)((char *)&mfc->ucontext->uc_mcontext.gregs[REG_ESP]);
    case 5: return (MRef)((char *)&mfc->ucontext->uc_mcontext.gregs[REG_EBP]);
    case 6: return (MRef)((char *)&mfc->ucontext->uc_mcontext.gregs[REG_ESI]);
    case 7: return (MRef)((char *)&mfc->ucontext->uc_mcontext.gregs[REG_EDI]);
  }
  NOTREACHED;
  return (MRef)NULL;  /* Avoids compiler warning. */
}


/* Prmci3DecodeFaultContext -- decode fault to find faulting address and IP */

void Prmci3DecodeFaultContext(MRef *faultmemReturn,
                              Byte **insvecReturn,
                              MutatorFaultContext mfc)
{
  /* .source.linux.kernel (linux/arch/i386/mm/fault.c). */
  *faultmemReturn = (MRef)mfc->info->si_addr;
  *insvecReturn = (Byte*)mfc->ucontext->uc_mcontext.gregs[REG_EIP];
}


/* Prmci3StepOverIns -- modify context to step over instruction */

void Prmci3StepOverIns(MutatorFaultContext mfc, Size inslen)
{
  mfc->ucontext->uc_mcontext.gregs[REG_EIP] += (unsigned long)inslen;
}


Addr MutatorFaultContextSP(MutatorFaultContext mfc)
{
  return (Addr)mfc->ucontext->uc_mcontext.gregs[REG_ESP];
}


Res MutatorFaultContextScan(ScanState ss, MutatorFaultContext mfc)
{
  mcontext_t *mc;
  Res res;

  /* This scans the root registers (.context.regroots).  It also
     unnecessarily scans the rest of the context.  The optimisation
     to scan only relevant parts would be machine dependent. */
  mc = &mfc->ucontext->uc_mcontext;
  res = TraceScanAreaTagged(ss,
                            (Addr *)mc,
                            (Addr *)((char *)mc + sizeof(*mc)));
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

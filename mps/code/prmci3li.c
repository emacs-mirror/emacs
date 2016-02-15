/* prmci3li.c: PROTECTION MUTATOR CONTEXT INTEL 386 (LINUX)
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
 * .source.i486: Intel486 Microprocessor Family Programmer's
 * Reference Manual
 *
 * .source.linux.kernel: Linux kernel source files.
 *
 *
 * ASSUMPTIONS
 *
 * .sp: The stack pointer in the context is ESP.
 *
 * .context.regroots: The root regs are assumed to be recorded in the context
 * at pointer-aligned boundaries.
 *
 * .assume.regref: The registers in the context can be modified by
 * storing into an MRef pointer.
 */

#include "prmcix.h"
#include "prmci3.h"

SRCID(prmci3li, "$Id$");

#if !defined(MPS_OS_LI) || !defined(MPS_ARCH_I3)
#error "prmci3li.c is specific to MPS_OS_LI and MPS_ARCH_I3"
#endif


/* Prmci3AddressHoldingReg -- return an address of a register in a context */

MRef Prmci3AddressHoldingReg(MutatorFaultContext mfc, unsigned int regnum)
{
  MRef gregs;

  AVER(mfc != NULL);
  AVER(NONNEGATIVE(regnum));
  AVER(regnum <= 7);
  AVER(mfc->ucontext != NULL);

  /* TODO: The current arrangement of the fix operation (taking a Ref *)
     forces us to pun these registers (actually `int` on LII3GC).  We can
     suppress the warning by casting through `void *` and this might make
     it safe, but does it really?  RB 2012-09-10 */
  AVER(sizeof(void *) == sizeof(*mfc->ucontext->uc_mcontext.gregs));
  gregs = (void *)mfc->ucontext->uc_mcontext.gregs;

  /* .source.i486 */
  /* .assume.regref */
  /* The register numbers (REG_EAX etc.) are defined in <ucontext.h>
     but only if _GNU_SOURCE is defined: see .feature.li in
     config.h. */
  switch (regnum) {
    case 0: return &gregs[REG_EAX];
    case 1: return &gregs[REG_ECX];
    case 2: return &gregs[REG_EDX];
    case 3: return &gregs[REG_EBX];
    case 4: return &gregs[REG_ESP];
    case 5: return &gregs[REG_EBP];
    case 6: return &gregs[REG_ESI];
    case 7: return &gregs[REG_EDI];
    default:
      NOTREACHED;
      return NULL;  /* Avoids compiler warning. */
  }
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


Res MutatorFaultContextScan(ScanState ss, MutatorFaultContext mfc,
			    mps_area_scan_t scan_area,
			    void *closure, size_t closure_size)
{
  mcontext_t *mc;
  Res res;

  /* This scans the root registers (.context.regroots).  It also
     unnecessarily scans the rest of the context.  The optimisation
     to scan only relevant parts would be machine dependent. */
  mc = &mfc->ucontext->uc_mcontext;
  res = TraceScanArea(ss,
		      (Word *)mc,
		      (Word *)((char *)mc + sizeof(*mc)),
		      scan_area, closure, closure_size);
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

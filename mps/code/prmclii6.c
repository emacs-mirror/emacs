/* prmclii6.c: MUTATOR CONTEXT x64 (LINUX)
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Implement the mutator context module. See <design/prmc/>.
 *
 *
 * SOURCES
 *
 * .source.linux.kernel: Linux kernel source files.
 *
 *
 * ASSUMPTIONS
 *
 * .sp: The stack pointer in the context is RSP.
 *
 * .assume.regref: The registers in the context can be modified by
 * storing into an MRef pointer.
 */

#include "prmcix.h"
#include "prmci6.h"

SRCID(prmclii6, "$Id$");

#if !defined(MPS_OS_LI) || !defined(MPS_ARCH_I6)
#error "prmclii6.c is specific to MPS_OS_LI and MPS_ARCH_I6"
#endif


/* Prmci6AddressHoldingReg -- return an address of a register in a context */

MRef Prmci6AddressHoldingReg(MutatorContext context, unsigned int regnum)
{
  MRef gregs;

  AVERT(MutatorContext, context);
  AVER(NONNEGATIVE(regnum));
  AVER(regnum <= 15);

  /* TODO: The current arrangement of the fix operation (taking a Ref *)
     forces us to pun these registers (actually `int` on LII6GC).  We can
     suppress the warning by casting through `void *` and this might make
     it safe, but does it really?  RB 2012-09-10 */
  AVER(sizeof(void *) == sizeof(*context->ucontext->uc_mcontext.gregs));
  gregs = (void *)context->ucontext->uc_mcontext.gregs;

  /* .assume.regref */
  /* The register numbers (REG_RAX etc.) are defined in <ucontext.h>
     but only if _GNU_SOURCE is defined: see .feature.li in
     config.h. */
  switch (regnum) {
    case  0: return &gregs[REG_RAX];
    case  1: return &gregs[REG_RCX];
    case  2: return &gregs[REG_RDX];
    case  3: return &gregs[REG_RBX];
    case  4: return &gregs[REG_RSP];
    case  5: return &gregs[REG_RBP];
    case  6: return &gregs[REG_RSI];
    case  7: return &gregs[REG_RDI];
    case  8: return &gregs[REG_R8];
    case  9: return &gregs[REG_R9];
    case 10: return &gregs[REG_R10];
    case 11: return &gregs[REG_R11];
    case 12: return &gregs[REG_R12];
    case 13: return &gregs[REG_R13];
    case 14: return &gregs[REG_R14];
    case 15: return &gregs[REG_R15];
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

  /* .source.linux.kernel (linux/arch/x86/mm/fault.c). */
  *faultmemReturn = (MRef)context->info->si_addr;
  *insvecReturn = (Byte*)context->ucontext->uc_mcontext.gregs[REG_RIP];
}


/* Prmci6StepOverIns -- modify context to step over instruction */

void Prmci6StepOverIns(MutatorContext context, Size inslen)
{
  AVERT(MutatorContext, context);

  context->ucontext->uc_mcontext.gregs[REG_RIP] += (Word)inslen;
}


Addr MutatorContextSP(MutatorContext context)
{
  AVERT(MutatorContext, context);

  return (Addr)context->ucontext->uc_mcontext.gregs[REG_RSP];
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

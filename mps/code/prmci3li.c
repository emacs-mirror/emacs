/* impl.c.prmci3li: PROTECTION MUTATOR CONTEXT INTEL 386 (LINUX)
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
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
 * .assume.regref: The resisters in the context can be modified by
 * storing into an MRef pointer.
 */

/* prmcli.h will include mpm.h after defining open sesame magic */
#include "prmcli.h"
#include "prmci3.h"

SRCID(prmci3li, "$Id$");


/* Prmci3AddressHoldingReg -- return an address of a register in a context */

MRef Prmci3AddressHoldingReg(MutatorFaultContext context, unsigned int regnum)
{
  struct sigcontext *scp;

  AVER(regnum <= 7);
  AVER(regnum >= 0);

  scp = context->scp;

  /* .source.i486 */
  /* .assume.regref */
  switch (regnum) {
  case 0: return (MRef)&scp->eax;
  case 1: return (MRef)&scp->ecx;
  case 2: return (MRef)&scp->edx;
  case 3: return (MRef)&scp->ebx;
  case 4: return (MRef)&scp->esp;
  case 5: return (MRef)&scp->ebp;
  case 6: return (MRef)&scp->esi;
  case 7: return (MRef)&scp->edi;
  }
  NOTREACHED;
  return (MRef)NULL;  /* Keep compiler happy. */
}


/* Prmci3DecodeFaultContext -- decode fault to find faulting address and IP */

void Prmci3DecodeFaultContext(MRef *faultmemReturn,
                              Byte **insvecReturn,
                              MutatorFaultContext context)
{
  struct sigcontext *scp;

  scp = context->scp;

  /* Assert that this is a page fault exception. The computation of */
  /* faultmem depends on this.  See .source.i486 (9.9.14). */
  AVER(scp->trapno == 14);

  /* cr2 contains the address which caused the fault. */
  /* See .source.i486 (9.9.14) and */
  /* .source.linux.kernel (linux/arch/i386/mm/fault.c). */
  *faultmemReturn = (MRef)scp->cr2;
  *insvecReturn = (Byte*)scp->eip;
}


/* Prmci3StepOverIns -- modify context to step over instruction */

void Prmci3StepOverIns(MutatorFaultContext context, Size inslen)
{
  context->scp->eip += (unsigned long)inslen;
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

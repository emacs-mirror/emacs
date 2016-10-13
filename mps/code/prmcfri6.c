/* prmcfri6.c: PROTECTION MUTATOR CONTEXT x64 (FREEBSD)
 *
 * $Id$
 * Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This module implements the part of the protection module
 * that decodes the MutatorFaultContext.
 *
 *
 * ASSUMPTIONS
 *
 * .sp: The stack pointer in the context is RSP.
 *
 * .context.regroots: The root regs are RDI, RSI, RBX, RDX, RCX, RAX,
 * and they are assumed to be recorded in the context at
 * pointer-aligned boundaries.
 */

#include "prmcix.h"
#include "prmci6.h"

SRCID(prmcfri6, "$Id$");

#if !defined(MPS_OS_FR) || !defined(MPS_ARCH_I6)
#error "prmcfri6.c is specific to MPS_OS_FR and MPS_ARCH_I6"
#endif


Addr MutatorFaultContextSP(MutatorFaultContext mfc)
{
  return (Addr)mfc->ucontext->uc_mcontext.mc_rsp;   /* .sp */
}


Res MutatorFaultContextScan(ScanState ss, MutatorFaultContext mfc,
                            mps_area_scan_t scan_area,
                            void *closure)
{
  Res res;

  /* This scans the root registers (.context.regroots).  It also unnecessarily
     scans the rest of the context.  The optimisation to scan only relevant
     parts would be machine dependent. */
  res = TraceScanArea(
    ss,
    (Word *)mfc->ucontext,
    (Word *)((char *)mfc->ucontext + sizeof(*(mfc->ucontext))),
    scan_area, closure
  );

  return res;
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

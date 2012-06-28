/* proti6.c: PROTECTION MUTATOR CONTEXT (x64)
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * .design: See <design/prot/> for the generic design of the interface
 * which is implemented in this module, including the contracts for the
 * functions.
 *
 * .purpose: This module implements the part of the protection module
 * that implements the MutatorFaultContext type. 
 *
 *
 * SOURCES
 *
 * .source.amd64: AMD64 Architecture Programmer’s Manual Volume 3: 
 * General-Purpose and System Instructions
 * <http://support.amd.com/us/Processor_TechDocs/24594_APM_v3.pdf>
 *
 *
 * ASSUMPTIONS
 *
 * .assume.null: It's always safe for Prot*StepInstruction to return
 * ResUNIMPL.  A null implementation of this module would be overly
 * conservative but otherwise correct.
 *
 */

#include "mpm.h"
#include "prmci6.h"

SRCID(proti6, "$Id$");


static Bool IsSimpleMov(Size *inslenReturn,
                        MRef *srcReturn,
                        MRef *destReturn,
                        MutatorFaultContext context)
{
  Byte *insvec;
  MRef faultmem;

  Prmci6DecodeFaultContext(&faultmem, &insvec, context);
  /* Unimplemented */
  UNUSED(inslenReturn);
  UNUSED(srcReturn);
  UNUSED(destReturn);
  
  return FALSE;
}


Bool ProtCanStepInstruction(MutatorFaultContext context)
{
  Size inslen;
  MRef src;
  MRef dest;

  /* .assume.null */
  if(IsSimpleMov(&inslen, &src, &dest, context)) {
    return TRUE;
  }

  return FALSE;
}


Res ProtStepInstruction(MutatorFaultContext context)
{
  Size inslen;
  MRef src;
  MRef dest;

  /* .assume.null */
  if(IsSimpleMov(&inslen, &src, &dest, context)) {
    *dest = *src;
    Prmci6StepOverIns(context, inslen);
    return ResOK;
  }

  return ResUNIMPL;
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

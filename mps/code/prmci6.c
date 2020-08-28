/* prmci6.c: MUTATOR CONTEXT (x64)
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .design: See <design/prmc> for the generic design of the interface
 * which is implemented in this module, including the contracts for the
 * functions.
 *
 * .purpose: Implement the mutator context module. <design/prmc>.
 *
 *
 * SOURCES
 *
 * .source.amd64: AMD64 Architecture Programmer's Manual Volume 3:
 * General-Purpose and System Instructions
 * <https://www.amd.com/system/files/TechDocs/24594.pdf>
 *
 *
 * ASSUMPTIONS
 *
 * .assume.null: It's always safe for MutatorContextCanStepInstruction
 * to return FALSE. A null implementation of this module would be
 * overly conservative but otherwise correct.
 *
 */

#include "mpm.h"
#include "prmci6.h"

SRCID(prmci6, "$Id$");

#if !defined(MPS_ARCH_I6)
#error "prmci6.c is specific to MPS_ARCH_I6"
#endif


static Bool IsSimpleMov(Size *inslenReturn,
                        MRef *srcReturn,
                        MRef *destReturn,
                        MutatorContext context)
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


Bool MutatorContextCanStepInstruction(MutatorContext context)
{
  Size inslen;
  MRef src;
  MRef dest;

  AVERT(MutatorContext, context);

  /* .assume.null */
  if(IsSimpleMov(&inslen, &src, &dest, context)) {
    return TRUE;
  }

  return FALSE;
}


Res MutatorContextStepInstruction(MutatorContext context)
{
  Size inslen;
  MRef src;
  MRef dest;

  AVERT(MutatorContext, context);

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
 * Copyright (C) 2001-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

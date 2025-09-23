/* prmcw3.c: MUTATOR CONTEXT FOR WIN32
 *
 * $Id$
 * Copyright (c) 2016-2020 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Implement the mutator context module. <design/prmc>.
 *
 *
 * ASSUMPTIONS
 *
 * .context.regroots: The root registers are assumed to be recorded in
 * the context at word-aligned boundaries.
 *
 * .context.flags: The ContextFlags field in the CONTEXT structure
 * determines what is recorded by GetThreadContext. This must include:
 *
 * .context.sp: CONTEXT_CONTROL, so that the stack pointer (Esp on
 * IA-32; Rsp on x86-64) is recorded.
 *
 * .context.regroots: CONTEXT_INTEGER, so that the root registers
 * (Edi, Esi, Ebx, Edx, Ecx, Eax on IA-32; Rdi, Rsi, Rbx, Rbp, Rdx,
 * Rcx, Rax, R8, ..., R15 on x86-64) are recorded.
 *
 * See the header WinNT.h for documentation of CONTEXT and
 * ContextFlags.
 */

#include "prmcw3.h"

SRCID(prmcw3, "$Id$");

#if !defined(MPS_OS_W3)
#error "prmcw3.c is specific to MPS_OS_W3"
#endif


Bool MutatorContextCheck(MutatorContext context)
{
  CHECKS(MutatorContext, context);
  CHECKL(NONNEGATIVE(context->var));
  CHECKL(context->var < MutatorContextLIMIT);
  return TRUE;
}


Res MutatorContextInitThread(MutatorContext context, HANDLE thread)
{
  BOOL success;

  AVER(context != NULL);

  context->var = MutatorContextTHREAD;
  /* This dumps the relevant registers into the context */
  /* .context.flags */
  context->the.context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
  success = GetThreadContext(thread, &context->the.context);
  if (!success)
    return ResFAIL;
  context->sig = MutatorContextSig;

  AVERT(MutatorContext, context);
  return ResOK;
}


void MutatorContextInitFault(MutatorContext context,
                             LPEXCEPTION_POINTERS ep)
{
  AVER(context != NULL);
  AVER(ep != NULL);

  context->var = MutatorContextFAULT;
  context->the.ep = ep;
  context->sig = MutatorContextSig;

  AVERT(MutatorContext, context);
}


Res MutatorContextScan(ScanState ss, MutatorContext context,
                       mps_area_scan_t scan_area, void *closure)
{
  CONTEXT *cx;
  Res res;

  AVERT(ScanState, ss);
  AVERT(MutatorContext, context);
  AVER(context->var == MutatorContextTHREAD);

  cx = &context->the.context;
  res = TraceScanArea(ss, (Word *)cx, (Word *)((char *)cx + sizeof *cx),
                      scan_area, closure); /* .context.regroots */

  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2016-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
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

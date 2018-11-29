/* prmcxc.c: MUTATOR CONTEXT (macOS)
 *
 * $Id$
 * Copyright (c) 2016-2018 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: Implement the mutator context module. <design/prmc>.
 *
 *
 * ASSUMPTIONS
 *
 * .context.regroots: The root registers are assumed to be recorded in
 * the context at pointer-aligned boundaries.
 */

#include "prmcxc.h"

SRCID(prmcxc, "$Id$");

#if !defined(MPS_OS_XC)
#error "prmcxc.c is specific to MPS_OS_XC"
#endif


Bool MutatorContextCheck(MutatorContext context)
{
  CHECKS(MutatorContext, context);
  CHECKL(sizeof *context->threadState == sizeof(THREAD_STATE_S));
  CHECKL(NONNEGATIVE(context->var));
  CHECKL(context->var < MutatorContextLIMIT);
  CHECKL((context->var == MutatorContextTHREAD) == (context->address == NULL));
  CHECKL(context->threadState != NULL);
  return TRUE;
}


void MutatorContextInitFault(MutatorContext context, Addr address,
                             THREAD_STATE_S *threadState)
{
  AVER(context != NULL);
  AVER(address != NULL);
  AVER(threadState != NULL);

  context->var = MutatorContextFAULT;
  context->address = address;
  context->threadState = threadState;
  context->sig = MutatorContextSig;

  AVERT(MutatorContext, context);
}


void MutatorContextInitThread(MutatorContext context,
                              THREAD_STATE_S *threadState)
{
  AVER(context != NULL);
  AVER(threadState != NULL);

  context->var = MutatorContextTHREAD;
  context->address = NULL;
  context->threadState = threadState;
  context->sig = MutatorContextSig;

  AVERT(MutatorContext, context);
}


Res MutatorContextScan(ScanState ss, MutatorContext context,
                       mps_area_scan_t scan_area, void *closure)
{
  THREAD_STATE_S *mc;
  Res res;

  AVERT(ScanState, ss);
  AVERT(MutatorContext, context);
  AVER(context->var == MutatorContextTHREAD);

  /* This scans the root registers (.context.regroots).  It also
     unnecessarily scans the rest of the context.  The optimisation
     to scan only relevant parts would be architecture dependent. */
  mc = context->threadState;
  res = TraceScanArea(ss,
                      (Word *)mc,
                      (Word *)((char *)mc + sizeof(*mc)),
                      scan_area, closure);
  return res;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2016-2018 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

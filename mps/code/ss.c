/* ss.c: STACK SCANNING
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 *  This is part of the code that scans the stack and fixes the registers
 *  that may contain roots.  See <design/thread-manager/>
 *
 *  Each platform ABI has a set of callee-save registers that may still
 *  contain roots.  The StackScan function is defined for each ABI in source
 *  files like ss*.c and ss*.asm.  That function saves the callee save
 *  registers in its frame, then calls StackScanInner to do the scanning.
 */

#include "mpm.h"

SRCID(ss, "$Id$");


/* StackScanInner -- carry out stack scanning
 *
 * This function should be called by StackScan once it has saved the
 * callee-save registers for the platform ABI in order to do the actual
 * scanning.
 */

Res StackScanInner(ScanState ss,
                   Addr *stackBot,
                   Addr *stackTop,
                   Count nSavedRegs)
{
  Arena arena;
  Res res;

  AVERT(ScanState, ss);
  AVER(stackTop < stackBot);
  AVER(AddrIsAligned((Addr)stackTop, sizeof(Addr)));  /* .assume.align */
  AVER(0 < nSavedRegs && nSavedRegs < 128);     /* sanity check */

  arena = ss->arena;

  /* If a stack pointer was stored when we entered the arena (through the
     MPS interface in mpsi*.c) then we scan just the saved registers and
     the stack starting there, in order to avoid false ambiguous references
     in the MPS stack.  This is particularly important for transforms
     (trans.c).  Otherwise, scan the whole stack. */

  if (arena->stackAtArenaEnter != NULL) {
    AVER(stackTop < arena->stackAtArenaEnter);
    AVER(arena->stackAtArenaEnter < stackBot);
    res = TraceScanAreaTagged(ss, stackTop, stackTop + nSavedRegs);
    if (res != ResOK)
      return res;
    res = TraceScanAreaTagged(ss, arena->stackAtArenaEnter, stackBot);
    if (res != ResOK)
      return res;
  } else {
    res = TraceScanAreaTagged(ss, stackTop, stackBot);
    if (res != ResOK)
      return res;
  }

  return ResOK;
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

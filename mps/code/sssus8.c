/* sssus8.c: SPARC STACK SCANNING
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 * Copyright (c) 2002 Global Graphics Software.
 *
 * This scans the stack and fixes the registers which may contain
 * roots.  See design.mps.thread-manager.
 *
 * .roots: The non-global registers are preserved into the stackframe
 * by the "ta 3" instruction.  This leaves the global registers.
 * According to the Sparc Architecture Manual:
 * %g1 is assumed to be volatile across procedure calls
 * %g2...%g4 are "reserved for use by application programmer"
 * %g5...%g7 are "nonvolatile and reserved for (as-yet-undefined)
 *    use by the execution environment"
 * To be safe %g2 to %g7 are pushed onto the stack before scanning
 * it just in case.
 *
 * ASSUMPTIONS
 *
 * .assume.align: The stack pointer is assumed to be aligned on a word
 * boundary.
 *
 * .assume.asm.stack: The compiler must not do wacky things with the
 * stack pointer around a call since we need to ensure that the
 * callee-save regs are visible during TraceScanArea.
 *
 * .assume.asm.order: The volatile modifier should prevent movement
 * of code, which might break .assume.asm.stack.
 */

#include "mpm.h"
#include <alloca.h>

SRCID(sssus8, "$Id$");


/* .assume.asm.order */
#define ASMV(x) __asm__ volatile (x)


Res StackScan(ScanState ss, Addr *stackBot)
{
  Addr *stackTop;
  Res res;
  void *globals;

  /* We expect C will save the caller's window, but we don't really care, */
  /* because it's bound to be an MPS window. */
  globals = alloca(24); /* for 6 globals */
  ASMV("std %%g2, [%0]" : : "r" (globals)); /* double stores */
  ASMV("std %%g4, [%0 + 8]" : : "r" (globals));
  ASMV("std %%g6, [%0 + 16]" : : "r" (globals));
  ASMV("ta 3"); /* flushes register windows onto stack */
  ASMV("mov %%sp, %0" : "=r" (stackTop));    /* stackTop = sp */

  AVER(AddrIsAligned((Addr)stackTop, sizeof(Addr)));  /* .assume.align */
  res = TraceScanArea(ss, stackTop, stackBot);

  return res;
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

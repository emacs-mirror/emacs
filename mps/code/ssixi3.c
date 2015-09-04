/* ssixi3.c: UNIX/INTEL STACK SCANNING
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 *  This scans the stack and fixes the registers which may contain
 *  roots.  See <design/thread-manager/>
 *
 *  This code was originally developed and tested on Linux, and then
 *  copied to the FreeBSD and Darwin (OS X) operating systems where it
 *  also seems to work.  Note that on FreeBSD and Darwin it has not
 *  been indepently verified with respect to any ABI documentation.
 *
 *  This code is common to more than one Unix implementation on
 *  Intel hardware (but is not portable Unix code).
 *
 *  The registers edi, esi, ebx are the registers defined to be preserved
 *  across function calls and therefore may contain roots.
 *  These are pushed on the stack for scanning.
 *
 * SOURCES
 *
 * .source.callees.saves: Set of callee-saved registers taken from
 * CALL_USED_REGISTERS in <gcc-sources>/config/i386/i386.h.
 * ebp added to the list because gcc now doesn't always use it as
 * a frame pointer so it could contain a root.
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
 *
 */


#include "mpm.h"

SRCID(ssixi3, "$Id$");


/* .assume.asm.order */
#define ASMV(x) __asm__ volatile (x)


Res StackScan(ScanState ss, Word *stackBot, Word mask, Word pattern)
{
  Word calleeSaveRegs[4];

  /* .assume.asm.stack */
  /* Store the callee save registers on the stack so they get scanned 
   * as they may contain roots.
   */
  ASMV("mov %%ebx, %0" : "=m" (calleeSaveRegs[0]));
  ASMV("mov %%esi, %0" : "=m" (calleeSaveRegs[1]));
  ASMV("mov %%edi, %0" : "=m" (calleeSaveRegs[2]));
  ASMV("mov %%ebp, %0" : "=m" (calleeSaveRegs[3]));
  
  return StackScanInner(ss, stackBot, calleeSaveRegs, NELEMS(calleeSaveRegs),
                        mask, pattern);
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

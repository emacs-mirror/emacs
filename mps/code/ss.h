/* ss.h: STACK SCANNING
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * Provides a function for scanning the stack and registers
 */

#ifndef ss_h
#define ss_h

#include "mpm.h"


/* StackScan -- scan the current thread's stack
 *
 * StackScan scans the stack of the current thread, Between stackBot and the
 * current top of stack. It also fixes any roots which may be in callee-save
 * registers.
 *
 * See the specific implementation for the exact registers which are scanned.
 *
 * If a stack pointer has been stashed at arena entry (through the MPS
 * interface in mpsi*.c) then only the registers and the stack between
 * stackAtArenaEnter and stackBot is scanned, to avoid scanning false
 * ambiguous references on the MPS's own stack.  This is particularly
 * important for transforms (trans.c).
 *
 * The word pointed to by stackBot is fixed if the stack is by convention
 * empty, and not fixed if it is full.  Where empty means sp points to first
 * free word beyond the top of stack.  Full means sp points to the top of
 * stack itself.
 */

extern Res StackScan(ScanState ss, Word *stackBot, Word mask, Word pattern);
extern Res StackScanInner(ScanState ss, Word *stackBot, Word *stackTop,
                          Count nSavedRegs, Word mask, Word pattern);

#endif /* ss_h */


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

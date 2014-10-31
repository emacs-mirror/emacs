/* ssxci6.c: STACK SCANNING FOR OS X ON x86-64
 *
 * $Id$
 * Copyright (c) 2012-2014 Ravenbrook Limited.  See end of file for license.
 *
 * This decodes the stack context and scans the registers which may
 * contain roots. See <design/ss/>.
 */

#include "mpm.h"

SRCID(ssxci6, "$Id$");

#if !defined(MPS_OS_XC) || !defined(MPS_ARCH_I6)
#error "ssxci6.c is specific to MPS_OS_XC and MPS_ARCH_I6."
#endif


/* Offset of RSP in the jmp_buf in bytes, as defined in _setjmp.s.
 * See the implementation of _setjmp in
 * <http://www.opensource.apple.com/source/Libc/Libc-825.24/x86_64/sys/_setjmp.s> */

#define JB_RSP 16


/* StackContextStackTop -- return the "top" of the mutator's stack at
 * the point when the context was saved by STACK_CONTEXT_SAVE. */

Addr *StackContextStackTop(StackContext sc)
{
  Addr **p_rsp = PointerAdd(&(sc)->jumpBuffer, JB_RSP);
  return *p_rsp;
}


/* StackContextScan -- scan references in the stack context
 *
 * We conservatively scan the whole of the jump buffer, assuming that
 * all references are stored at aligned positions.
 *
 * The PointerAlignDown is necessary because the size of the jump
 * buffer is not a multiple of sizeof(Addr) on this platform: see
 * setjmp.h, where _JBLEN is 37.
 * <http://www.opensource.apple.com/source/Libc/Libc-825.24/include/setjmp.h>
 */

Res StackContextScan(ScanState ss, StackContext sc)
{
  return TraceScanAreaTagged(ss, (void *)sc,
                             PointerAlignDown(sc + 1, sizeof(Addr)));
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2012-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

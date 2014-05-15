/* ssan.c: ANSI STACK SCANNER
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * This module makes a best effort to scan the stack and fix the
 * registers which may contain roots, using only the features of the
 * Standard C library.
 * 
 * .assume.setjmp: The implementation assumes that setjmp stores all
 * the registers that need to be scanned in the jmp_buf.
 */

#include <setjmp.h>

#include "mpmtypes.h"
#include "misc.h"
#include "ss.h"


SRCID(ssan, "$Id$");


Res StackScan(ScanState ss, Addr *stackBot)
{
  jmp_buf jb;
  void *stackTop = &jb;

  /* .assume.stack: This implementation assumes that the stack grows
   * downwards, so that the address of the jmp_buf is the limit of the
   * part of the stack that needs to be scanned. (StackScanInner makes
   * the same assumption.)
   */
  AVER(stackTop < (void *)stackBot);

  (void)setjmp(jb);

  return StackScanInner(ss, stackBot, stackTop, sizeof jb / sizeof(Addr*));
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

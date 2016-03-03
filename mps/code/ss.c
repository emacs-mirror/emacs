/* ss.c: STACK SCANNING
 *
 * $Id$
 * Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 * This scans the mutator's stack and fixes the registers that may
 * contain roots. See <design/ss/>.
 *
 * This is a generic implementation, but it makes assumptions that,
 * while true on all the platforms we currently (version 1.115)
 * support, may not be true on all platforms. See
 * <design/ss/#sol.platform>.
 * 
 * .assume.desc: The stack is descending (and so stackHot is a lower
 * address than stackCold).
 *
 * .assume.full: The stack convention is "full" (and so we must scan
 * the word pointed to by stackHot but not the word pointed to by
 * stackCold).
 *
 * .assume.align: Addresses on the stack are word-aligned.  TODO: It's
 * not clear why we assume this, since it depends on the area scanner.
 */

#include "mpm.h"

SRCID(ss, "$Id$");


/* StackScan -- scan the mutator's stack and registers */

Res StackScan(ScanState ss, Word *stackCold,
              mps_area_scan_t scan_area, void *closure)
{
  StackContextStruct scStruct;
  StackContext sc;
  Arena arena;
  Word *stackHot;

  AVERT(ScanState, ss);

  arena = ss->arena;

  AVER(arena->scAtArenaEnter != NULL);
  if (arena->scAtArenaEnter) {
    sc = arena->scAtArenaEnter;
  } else {
    /* Somehow missed saving the context at the entry point (see
       <design/ss/#sol.entry-points.fragile>): do it now. */
    sc = &scStruct;
    STACK_CONTEXT_SAVE(sc);
  }

  stackHot = (void *)sc;
  AVER(stackHot < stackCold);                         /* .assume.desc */
  AVER(AddrIsAligned((Addr)stackHot, sizeof(Word)));  /* .assume.align */

  return TraceScanArea(ss, stackHot, stackCold, scan_area, closure);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

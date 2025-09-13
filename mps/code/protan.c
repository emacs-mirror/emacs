/* protan.c: ANSI MEMORY PROTECTION
 *
 * $Id$
 * Copyright (c) 2001-2020 Ravenbrook Limited.  See end of file for license.
 *
 *
 * DESIGN
 *
 * <design/protan>
 */

#include "mpm.h"

SRCID(protan, "$Id$");


/* ProtSetup -- global protection setup */

void ProtSetup(void)
{
  NOOP;
}


/* ProtGranularity -- return the granularity of protection */

Size ProtGranularity(void)
{
  /* Any range of addresses can be "protected" since ProtSet does nothing. */
  return (Size)MPS_PF_ALIGN;
}


/* ProtSet -- set the protection for a page */

void ProtSet(Addr base, Addr limit, AccessSet pm)
{
  AVER(base < limit);
  AVERT(AccessSet, pm);
  UNUSED(pm);
  NOOP;
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * <design/protan#.fun.sync>.
 */

void ProtSync(Arena arena)
{
  Bool synced;

  AVERT(Arena, arena);

  do {
    Seg seg;

    synced = TRUE;
    if (SegFirst(&seg, arena)) {
      do {
        if (SegPM(seg) != AccessSetEMPTY) { /* <design/protan#.fun.sync.seg> */
          ShieldEnter(arena);
          TraceSegAccess(arena, seg, SegPM(seg));
          ShieldLeave(arena);
          synced = FALSE;
        }
      } while(SegNext(&seg, arena, seg));
    }
  } while(!synced);
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

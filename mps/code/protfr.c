/*  impl.c.protfr: PROTECTION FOR FreeBSD
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 */

#include "mpm.h"

#ifndef MPS_OS_FR
#error "protfr.c is FreeBSD specific, but MPS_OS_FR is not set"
#endif
#ifndef PROTECTION
#error "protfr.c implements protection, but PROTECTION is not set"
#endif

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/mman.h>

SRCID(protfr, "$Id$");


/* ProtSet -- set protection
 *
 * This is just a thin veneer on top of mprotect(2).
 */

void ProtSet(Addr base, Addr limit, AccessSet mode)
{
  int flags;
  int res;

  AVER(sizeof(int) == sizeof(Addr));   /* should be redundant; will fail on Alpha */
  AVER(base < limit);
  AVER(base != 0);
  AVER(AddrOffset(base, limit) <= INT_MAX);     /* should be redundant */

#if 0
  /* .flags.trouble: This less strict version of flags (which allows write
   * access unless explicitly told not to) caused mmqa test 37 to fail.
   * This might be a bug in MPS, so for now we go with the stricter
   * version that matches the Win32 implementation. */
  /* .flags.trouble.freebsd: the above comment was in the Linux version
   * of this code; I haven't verified it for FreeBSD.  */
  flags = 0;
  if((mode & AccessREAD) == 0)
    flags |= PROT_READ | PROT_EXEC;
  if((mode & AccessWRITE) == 0)
    flags |= PROT_WRITE;
#endif
  flags = PROT_READ | PROT_WRITE | PROT_EXEC;
  if((mode & AccessWRITE) != 0)
    flags = PROT_READ | PROT_EXEC;
  if((mode & AccessREAD) != 0)
    flags = 0;

  res = mprotect((void *)base, (size_t)AddrOffset(base, limit), flags);
  AVER(res == 0);
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * This does nothing under FreeBSD.
 */

void ProtSync(Arena arena)
{
  NOOP;
}



/* ProtTramp -- protection trampoline
 *
 * The protection trampoline is trivial under FreeBSD, as there is
 * nothing that needs to be done in the dynamic context of the mutator
 * in order to catch faults.  (Contrast this with Win32 Structured
 * Exception Handling.)
 */

void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
               void *p, size_t s)
{
  AVER(resultReturn != NULL);
  AVER(FUNCHECK(f));
  /* Can't check p and s as they are interpreted by the client */

  *resultReturn = (*f)(p, s);
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

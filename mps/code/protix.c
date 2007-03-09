/* protix.c: PROTECTION FOR UNIX
 *
 *  $Id$
 *  Copyright (c) 2001,2007 Ravenbrook Limited.  See end of file for license.
 *
 *  Somewhat generic across different Unix systems.  Shared between
 *  Darwin (OS X), OSF/1 (DIGITAL UNIX), FreeBSD, and Linux.
 *
 *  May not actually work on OSF/1 due to lack of available machines.
 *
 *  This file does not contain a signal handler.  That's in protsgix.c
 *  (for FreeBSD and Darwin on Intel); in protxcpp.c (for Darwin on
 *  PowerPC).
 *
 *  ASSUMPTIONS
 *
 *  .assume.mprotect.base: We assume that the first argument to mprotect can
 *    be safely passed as a void *.  Single UNIX Specification Version 2
 *    (aka X/OPEN XSH5) says that the parameter is a void *.  Some
 *    Unix-likes may declare this parameter as a caddr_t.  FreeBSD used to
 *    do this (on the now very obsolete FreeBSD 2.2.x series).  The
 *    Darwin man page documents it as caddr_t but it appears to be
 *    implemented correctly as void *.  caddr_t is usually char *.
 */


/* open sesame magic, see standards(5) */
#define _POSIX_C_SOURCE 199309L
#define _XOPEN_SOURCE_EXTENDED 1

#include "mpm.h"

#if !defined(MPS_OS_LI) && !defined(MPS_OS_FR) && !defined(MPS_OS_XC) && !defined(MPS_OS_O1)
#error "protix.c is Unix-specific, currently for MPS_OS_LI FR XC O1"
#endif
#ifndef PROTECTION
#error "protix.c implements protection, but PROTECTION is not set"
#endif

#include <limits.h>
#include <stddef.h>

#include <sys/mman.h>
#include <sys/types.h>

SRCID(protix, "$Id$");

/* ProtSet -- set protection
 *
 * This is just a thin veneer on top of mprotect(2).
 */

void ProtSet(Addr base, Addr limit, AccessSet mode)
{
  int flags;

  AVER(sizeof(size_t) == sizeof(Addr));
  AVER(base < limit);
  AVER(base != 0);
  AVER(AddrOffset(base, limit) <= INT_MAX);     /* should be redundant */

  /* Convert between MPS AccessSet and UNIX PROT thingies.
     In this function, AccessREAD means protect against read accesses
     (disallow them).  PROT_READ means allow read accesses.
   */
  switch(mode) {
  case AccessWRITE | AccessREAD:
  case AccessREAD:      /* forbids writes as well */
    flags = PROT_NONE;
    break;
  case AccessWRITE:
    flags = PROT_READ | PROT_EXEC;
    break;
  case AccessSetEMPTY:
    flags = PROT_READ | PROT_WRITE | PROT_EXEC;
    break;
  default:
    NOTREACHED;
    flags = PROT_NONE;
  }

  /* .assume.mprotect.base */
  if(mprotect((void *)base, (size_t)AddrOffset(base, limit), flags) != 0)
    NOTREACHED;
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * This does nothing under Solaris.
 */

void ProtSync(Arena arena)
{
  UNUSED(arena);
  NOOP;
}


/* ProtTramp -- protection trampoline
 *
 * The protection trampoline is trivial under Unix, as there is
 * nothing that needs to be done in the dynamic context of the mutator in
 * order to catch faults.  (Contrast this with Win32 Structured Exception
 * Handling.)
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
 * Copyright (C) 2001-2007 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

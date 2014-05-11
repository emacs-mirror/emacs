/* protix.c: PROTECTION FOR UNIX
 *
 *  $Id$
 *  Copyright (c) 2001-2014 Ravenbrook Limited.  See end of file for license.
 *
 *  Somewhat generic across different Unix systems.  Shared between
 *  Darwin (OS X), FreeBSD, and Linux.
 *
 *  This file does not contain a signal handler.  That's in protsgix.c
 *  (for FreeBSD and Darwin on Intel); in protxcpp.c (for Darwin on
 *  PowerPC); in protlii3.c (for Intel Linux).
 *
 *
 *  SOURCES
 *
 *  [SUSV2MPROTECT] Single UNIX Specification, Version 2, mprotect man
 *  page:
 *  http://opengroup.org/onlinepubs/007908799/xsh/mprotect.html
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
 *
 *  .assume.write-only:  More of an anti-assumption really.  We
 *    assume that asking the OS for a write-only page (that is, flags =
 *    PROT_WRITE) does not work.  What actually happens on all the
 *    Unix-like OSes that we've seen is that asking for write-permission
 *    (flags = PROT_WRITE) grants read-permission as well.  That is why
 *    when the MPS requires that a page be read-protected (mode ==
 *    AccessREAD) we must ensure that writes are also not allowed.
 *    The portable guarantees of mprotect (see [SUSV2MPROTECT]) are that
 *    writes are not permitted where PROT_WRITE is not used and no access
 *    is permitted when PROT_NONE alone is used.
 */

#include "mpm.h"

#if !defined(MPS_OS_LI) && !defined(MPS_OS_FR) && !defined(MPS_OS_XC)
#error "protix.c is Unix-specific, currently for MPS_OS_LI FR XC"
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
     (disallow them).  PROT_READ means allow read accesses.  Notice that
     this follows a difference in contract as well as style.  AccessREAD
     means that no reads should be permitted (all reads should go via
     the signal handler), possibly other operations (write) also go via
     the signal handler; PROT_WRITE means that all writes should be
     allowed, possibly that means other operations (read) are also
     allowed.
   */
  switch(mode) {
  case AccessWRITE | AccessREAD:
  case AccessREAD:      /* forbids writes as well, see .assume.write-only */
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
 * This does nothing under Posix.  See protan.c.
 */

void ProtSync(Arena arena)
{
  UNUSED(arena);
  NOOP;
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

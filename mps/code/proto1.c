/* proto1.c: PROTECTION FOR DIGITAL UNIX
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 */


/* open sesame magic, see standards(5) */
#define _POSIX_C_SOURCE 199309L
#define _XOPEN_SOURCE_EXTENDED 1

#include "mpm.h"

#ifndef MPS_OS_O1
#error "proto1.c is OSF/1-specific, but MPS_OS_O1 is not set"
#endif
#ifndef PROTECTION
#error "proto1.c implements protection, but PROTECTION is not set"
#endif

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <signal.h>
#include <siginfo.h>
#include <sys/mman.h>
/* for getpid() */
#include <unistd.h>

SRCID(proto1, "$Id$");


/* The previously-installed signal action, as returned by */
/* sigaction(3).  See ProtSetup. */

static struct sigaction sigNext;


/* == Protection Signal Handler ==
 *
 * This is the signal handler installed by ProtSetup to deal with
 * protection faults.  It is installed on the SIGSEGV signal.
 * It decodes the protection fault details from the signal context
 * and passes them to ArenaAccess, which attempts to handle the
 * fault and remove its cause.  If the fault is handled, then
 * the handler returns and execution resumes.  If it isn't handled,
 * then sigHandle does its best to pass the signal on to the
 * previously installed signal handler (sigNext).
 *
 * .sigh.addr: We assume that the OS decodes the address to something
 * sensible
 * .sigh.limit: We throw away the limit information.
 */

static void sigHandle(int sig, siginfo_t *info, void *context)
{
  int e;
  sigset_t sigset, oldset;
  struct sigaction sa;

  AVER(sig == SIGSEGV);
  AVER(info != NULL);

  if(info->si_code == SEGV_ACCERR) {
    AccessSet mode;
    Addr base, limit;

    /* We can't determine the access mode (read, write, etc.) */
    /* under Solaris without decoding the faulting instruction. */
    /* Don't bother, yet.  We can do this if necessary. */

    mode = AccessREAD | AccessWRITE;

    /* We assume that the access is for one word at the address. */

    base = (Addr)info->si_addr;
    limit = AddrAdd(base, (Size)sizeof(Addr));

    /* Offer each protection structure the opportunity to handle the */
    /* exception.  If it succeeds, then allow the mutator to continue. */

    /* MutatorFaultContext parameter is a dummy parameter in this */
    /* implementation */
    if(ArenaAccess(base, mode, NULL))
      return;
  }

  /* The exception was not handled by any known protection structure, */
  /* so throw it to the previously installed handler. */

  /* @@ This is really weak.
   * Need to implement rest of the contract of sigaction */
 
  e = sigaction(SIGSEGV, &sigNext, &sa);
  AVER(e == 0);
  sigemptyset(&sigset);
  sigaddset(&sigset, SIGSEGV);
  e = sigprocmask(SIG_UNBLOCK, &sigset, &oldset);
  AVER(e == 0);
  kill(getpid(), SIGSEGV);
  e = sigprocmask(SIG_SETMASK, &oldset, NULL);
  AVER(e == 0);
  e = sigaction(SIGSEGV, &sa, NULL);
  AVER(e == 0);
}


/* ProtSetup -- global protection setup
 *
 * Under DIGITAL UNIX, the global setup involves installing a signal handler
 * on SIGSEGV to catch and handle protection faults (see sigHandle).
 * The previous handler is recorded so that it can be reached from
 * sigHandle if it fails to handle the fault.
 *
 * NOTE: There are problems with this approach:
 *   1. we can't honor the wishes of the sigaction(2) entry for the
 *      previous handler,
 *   2. what if this thread is suspended just after calling signal(3)?
 *      The sigNext variable will never be initialized!
 */

void ProtSetup(void)
{
  struct sigaction sa;
  int result;

  sa.sa_sigaction = sigHandle;
  sa.sa_flags = SA_SIGINFO;

  result = sigaction(SIGSEGV, &sa, &sigNext);
  AVER(result == 0);
}


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

  /* convert between MPS AccessSet and UNIX PROT thingies. */
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
 * The protection trampoline is trivial under DIGITAL UNIX, as there is
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

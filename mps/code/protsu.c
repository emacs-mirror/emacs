/* impl.c.protsu: PROTECTION FOR SUNOS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * DESIGN
 *
 * see design.mps.prot for the generic design (including the interface),
 * and design.mps.protsu for the design specific to this implementation.
 *
 * TRANSGRESSIONS
 *
 * .hack.sigdfl: GCC 2.5.8 produces a warning when we use SIG_DFL with
 * -Wstrict-prototypes, which we want.  SIG_DFL is just zero, so we
 * have our own definition.  We don't expect SIG_DFL to change, because
 * that would break SunOS binaries.  *sigh*
 */

#include "mpm.h"

#ifndef MPS_OS_SU
#error "protsu.c is SunOS 4 specific, but MPS_OS_SU is not set"
#endif
#ifndef PROTECTION
#error "protsu.c implements protection, but PROTECTION is not set"
#endif

#include <sys/mman.h>
#include <signal.h>
#include <limits.h>

SRCID(protsu, "$Id$");


/* Fix up unprototyped system calls. */

extern int getpagesize(void);
extern int getpid(void);
/* .depend.caddrt.self-promote: The following prototype for mprotect
 * assumes that the type caddr_t is compatible with type that is produced
 * when the default argument promotions are applied to caddr_t.  See
 * ISO C clause 6.3.2.2.  caddr_t is defined is defined in
 * /usr/include/sys/types.h to be char *, so this assumption is valid.
 */
extern int mprotect(caddr_t, int, int);
extern int sigblock(int);
extern int sigsetmask(int);
typedef void (*handler_t)(int, int, struct sigcontext *, char *);


/* .hack.sigdfl */
#ifndef SIG_DFL
#error "protsu.c expected SIG_DFL to be declared by signal.h"
#else
#undef SIG_DFL
#define SIG_DFL         ((handler_t)0)
#endif


/* Pointer to the previously-installed signal handler, as returned by */
/* signal(3).  See ProtSetup. */

static handler_t sigNext = NULL;


/* sigHandle -- protection signal handler
 *
 * This is the signal handler installed by ProtSetup to deal with
 * protection faults.  It is installed on the SIGSEGV signal.
 * It decodes the protection fault details from the signal context
 * and passes them to ArenaAccess, which attempts to handle the
 * fault and remove its cause.  If the fault is handled, then
 * the handler returns and execution resumes.
 *
 * If it isn't handled, then sigHandle does its best to pass the signal
 * on to the previously installed signal handler (sigNext).  sigHandle
 * cannot emulate a signal precisely.  The problems are that the signal
 * mask for that signal (set by sigvec) will not be set properly, also
 * the handler will be executed on the current stack and not on its own
 * stack (if it requested it).
 *
 * .assume.addr: This code assumes that the system will decode the
 * address of the protection violation.  This is documented in the
 * "ADDR" section of the sigvec(2) man page.
 *
 * .sigh.decode: We can't determine the access mode (read, write, etc.)
 * without decoding the faulting instruction.  We don't bother to do
 * this yet.  It can be done later, if necessary.
 *
 * .sigh.size: We also assume that the access only affects the page
 * of the faulting address, i.e. is a single word access or a double-
 * aligned double-word access.
 */

static void sigHandle(int sig, int code,
                      struct sigcontext *scp, char *addr)
{
  AVER(sig == SIGSEGV);
  AVER(scp != NULL);

  if(code == SEGV_PROT) {
    AccessSet mode;
    AVER(addr != SIG_NOADDR);           /* .assume.addr */
    mode = AccessREAD | AccessWRITE;    /* .sigh.decode */
    /* MutatorFaultContext parameter is a dummy parameter in */
    /* this implementation */
    if(ArenaAccess((Addr)addr, mode, NULL))   /* .sigh.size */
      return;
  }

  /* The exception was not handled by any known protection structure, */
  /* so throw it to the previously installed handler. */
  AVER(sigNext != NULL);
  (*sigNext)(sig, code, scp, addr);
}


/* sigDefault -- default signal handler
 *
 * This is a signal handler used as sigNext if the previous handler
 * returned by signal(3) was SIG_DFL.  It does its best to get to
 * the default handler, which will probably dump core.
 */

static void sigDefault(int sig, int code,
                       struct sigcontext *scp, char *addr)
{
  UNUSED(sig);
  UNUSED(code);
  UNUSED(scp);
  UNUSED(addr);

  AVER(sig == SIGSEGV);

  (void)sigsetmask(sigblock(0) & ~sigmask(SIGSEGV));
  (void)signal(SIGSEGV, SIG_DFL);
  (void)kill(getpid(), SIGSEGV);
  NOTREACHED;
  abort();
}


/* ProtSetup -- global protection setup
 *
 * NOTE: There are problems with this approach:
 *   1. If the thread is suspended just after calling signal(3)
 *      then the sigNext variable will not be set and sigHandle will
 *      be installed as the signal handler.  sigHandle will fall over
 *      if it tries to call the next handler in the chain.
 */

void ProtSetup(void)
{
  handler_t next;

  /* ProtSetup is called exactly once, see design.mps.prot.if.setup */
  AVER(sigNext == NULL);

  next = signal(SIGSEGV, sigHandle);
  /* should always succeed as our parameters are valid */
  AVER(next != (handler_t)-1);

  if(next == SIG_DFL)           /* use the suicide function */
    sigNext = sigDefault;
  else
    sigNext = next;
}


/* ProtSet -- set the protection for a page
 *
 * This is just a thin veneer on top of mprotect(2).
 *
 * .assume.size: We asssume the type int and the type Size are the
 * same size.  This assumption is made in the call to mprotect.
 */

void ProtSet(Addr base, Addr limit, AccessSet mode)
{
  int flags;

  AVER(sizeof(int) == sizeof(Size));    /* See .assume.size */
  AVER(base < limit);
  AVER(base != (Addr)0);
  /* we assume that the difference between limit and base (which is */
  /* positive) will fit in an int */
  AVER(AddrOffset(base, limit) <= INT_MAX); /* should be redundant */
  /* There is no AccessSetCheck, so we don't */

  /* convert between MPS AccessSet and SunOS PROT thingies. */
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

  /* 2nd arg to mprotect, .assume.size */
  if(mprotect((caddr_t)base, (int)AddrOffset(base, limit), flags) != 0) {
    /* design.mps.protsu.fun.set.assume.mprotect */
    NOTREACHED;
  }
}


/* ProtSync -- synchronize protection settings with hardware */

void ProtSync(Arena arena)
{
  AVERT(Arena, arena);
  UNUSED(arena);
  NOOP;
}


/* ProtTramp -- protection trampoline */

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

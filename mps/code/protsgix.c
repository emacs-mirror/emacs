/* protsgix.c: PROTECTION (SIGNAL HANDLER) FOR POSIX
 *
 *  $Id$
 *  Copyright (c) 2001-2016 Ravenbrook Limited.  See end of file for license.
 *
 * This implements protection exception handling using POSIX signals.
 * It is designed to run on any POSIX-compliant Unix.
 *
 *
 * SOURCES
 *
 * .source.posix: POSIX specifications for signal.h and sigaction
 * <http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/signal.h.html>
 * <http://pubs.opengroup.org/onlinepubs/9699919799/functions/sigaction.html>
 *
 * .source.man: sigaction(2): FreeBSD System Calls Manual.
 *
 * .source.merge: A blend from primarily the FreeBSD version (protfri3.c)
 * and the OSF/1 (DIGITAL UNIX) version (proto1.c); influenced by other
 * Unix versions.
 */

#include "mpm.h"

#if !defined(MPS_OS_FR) && !defined(MPS_OS_LI)
#error "protsgix.c is Unix-specific, currently for MPS_OS_FR and MPS_OS_LI"
#endif

#include "prmcix.h"

#include <signal.h>    /* for many functions */
#include <ucontext.h>  /* for ucontext_t */
#include <unistd.h>    /* for getpid */

SRCID(protsgix, "$Id$");


/* The previously-installed signal action, as returned by */
/* sigaction(3).  See ProtSetup. */

static struct sigaction sigNext;


/* sigHandle -- protection signal handler
 *
 * This is the signal handler installed by ProtSetup to deal with
 * protection faults. It is installed on the signal given by the
 * PROT_SIGNAL macro (that is, SIGSEGV). It constructs a mutator
 * context based on the signal context, and passes it to ArenaAccess,
 * which attempts to handle the fault and remove its cause. If the
 * fault is handled, then the handler returns and execution resumes.
 * If it isn't handled, then sigHandle does its best to pass the
 * signal on to the previously installed signal handler (sigNext);
 * which it does by signalling itself using kill(2).
 *
 * .sigh.args: We set the SA_SIGINFO flag in the sa_flags field of the
 * sigaction structure, and so the signal handler in the sa_sigaction
 * field receives three arguments: signal number, pointer to signal
 * info structure, pointer to signal context structure.
 *
 * .sigh.check: We check that info->si_code is SEGV_ACCERR (meaning
 * "Invalid permissions for mapped object").
 *
 * .sign.addr: If so, we assume info->si_addr is the fault address.
 *
 * .sigh.mode: The fault type (read/write) does not appear to be
 * available to the signal handler (see mail archive).
 */

#define PROT_SIGNAL SIGSEGV

static void sigHandle(int sig, siginfo_t *info, void *uap)  /* .sigh.args */
{
  int e;
  /* sigset renamed to asigset due to clash with global on Darwin. */
  sigset_t asigset, oldset;
  struct sigaction sa;

  AVER(sig == PROT_SIGNAL);

  if(info->si_code == SEGV_ACCERR) {  /* .sigh.check */
    AccessSet mode;
    Addr base;
    ucontext_t *ucontext;
    MutatorContextStruct context;

    ucontext = (ucontext_t *)uap;
    context.ucontext = ucontext;
    context.info = info;

    mode = AccessREAD | AccessWRITE; /* .sigh.mode */

    /* We assume that the access is for one word at the address. */
    base = (Addr)info->si_addr;   /* .sigh.addr */

    /* Offer each protection structure the opportunity to handle the */
    /* exception.  If it succeeds, then allow the mutator to continue. */
    if(ArenaAccess(base, mode, &context))
      return;
  }

  /* The exception was not handled by any known protection structure, */
  /* so throw it to the previously installed handler.  That handler won't */
  /* get an accurate context (the MPS would fail if it were the second in */
  /* line) but it's the best we can do. */

  e = sigaction(PROT_SIGNAL, &sigNext, &sa);
  AVER(e == 0);
  sigemptyset(&asigset);
  sigaddset(&asigset, PROT_SIGNAL);
  e = sigprocmask(SIG_UNBLOCK, &asigset, &oldset);
  AVER(e == 0);
  kill(getpid(), PROT_SIGNAL);
  e = sigprocmask(SIG_SETMASK, &oldset, NULL);
  AVER(e == 0);
  e = sigaction(PROT_SIGNAL, &sa, NULL);
  AVER(e == 0);
}


/*  ProtSetup -- global protection setup
 *
 *  Under Unix, the global setup involves installing a signal handler
 *  on PROT_SIGNAL to catch and handle page faults (see sigHandle).
 *  The previous handler is recorded so that it can be reached from
 *  sigHandle if it fails to handle the fault.
 *
 *  NOTE: There are problems with this approach:
 *    1. we can't honor the sa_flags for the previous handler,
 *    2. what if this thread is suspended just after calling signal(3)?
 *       The sigNext variable will never be initialized!
 */

void ProtSetup(void)
{
  struct sigaction sa;
  int result;

  sa.sa_sigaction = sigHandle;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

  result = sigaction(PROT_SIGNAL, &sa, &sigNext);
  AVER(result == 0);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2016 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

/*  impl.c.protfri3: PROTECTION FOR FREEBSD (INTEL 386)
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.
 *
 * SOURCES
 *
 * .source.i486: Intel486 Microprocessor Family Programmer's
 * Reference Manual
 *
 * .source.man: sigaction(2): FreeBSD System Calls Manual.
 */

#include "prmcfr.h"

#ifndef MPS_OS_FR
#error "protfri3.c is FreeBSD-specific, but MPS_OS_FR is not set"
#endif
#if !defined(MPS_ARCH_I3) && !defined(MPS_ARCH_I4)
#error "protfri3.c is Intel-specific, but MPS_ARCH_I3 or MPS_ARCH_I4 is not set"
#endif
#ifndef PROTECTION
#error "protfri3.c implements protection, but PROTECTION is not set"
#endif

#include <signal.h>
#include <machine/trap.h>

SRCID(protfri3, "$Id$");


/* The previously-installed signal action, as returned by */
/* sigaction(3).  See ProtSetup. */

static struct sigaction sigNext;

/* sigHandle -- protection signal handler
 *
 *  This is the signal handler installed by ProtSetup to deal with
 *  protection faults.  It is installed on the SIGBUS signal.  It
 *  decodes the protection fault details from the signal context and
 *  passes them to ArenaAccess, which attempts to handle the fault and
 *  remove its cause.  If the fault is handled, then the handler
 *  returns and execution resumes.  If it isn't handled, then
 *  sigHandle does its best to pass the signal on to the previously
 *  installed signal handler (sigNext).
 *
 *  .sigh.args: The sigaction manual page .source.man documents three
 *  different handler prototypes: ANSI C sa_handler, traditional BSD
 *  sa_handler, and POSIX SA_SIGINFO sa_sigaction.  The ANSI C
 *  prototype isn't powerful enough for us (can't get addresses), and
 *  the manual page deprecates the BSD sa_handler in favour of the
 *  POSIX SA_SIGINFO sa_sigaction.  In that prototype, the arguments
 *  are: signal number, pointer to signal info structure, pointer to
 *  signal context structure.
 *
 *  .sigh.context: We only know how to handle signals with code
 *  BUS_PAGE_FAULT, where info->si_addr gives the fault address.
 *
 *  .sigh.mode: The fault type (read/write) does not appear to be
 *  available to the signal handler (see mail archive).
 */

static void sigHandle(int sig, siginfo_t *info, void *context)  /* .sigh.args */
{
  AVER(sig == SIGBUS);

  if(info->si_code == BUS_PAGE_FAULT) {  /* .sigh.context */
    AccessSet mode;
    Addr base, limit;

    mode = AccessREAD | AccessWRITE; /* .sigh.mode */

    /* We assume that the access is for one word at the address. */
    base = (Addr)info->si_addr;   /* .sigh.context */
    limit = AddrAdd(base, (Size)sizeof(Addr));

    /* Offer each protection structure the opportunity to handle the */
    /* exception.  If it succeeds, then allow the mutator to continue. */
    if(ArenaAccess(base, mode, NULL))
      return;
  }

  /* The exception was not handled by any known protection structure, */
  /* so throw it to the previously installed handler. */

  /* @@@@ This is really weak. */
  /* Need to implement rest of the contract of sigaction */
  /* We might also want to set SA_RESETHAND in the flags and explicitly */
  /* reinstall the handler from withint itself so the SIG_DFL/SIG_IGN */
  /* case can work properly by just returning. */
  switch ((int)sigNext.sa_handler) {
  case (int)SIG_DFL:
  case (int)SIG_IGN:
    abort();
    NOTREACHED;
    break;
  default:
    if ((int)sigNext.sa_handler & SA_SIGINFO) {
      (*sigNext.sa_sigaction)(sig, info, context);
    } else {
      /* @@@@ what if the previous handler is BSD-style? */
        /* We don't have a struct sigcontext to pass to it.
           The second argument (the code) is just info->si_code
           but the third argument (the sigcontext) we would have to
           fake from the ucontext.  We could do that. */
      (*sigNext.sa_handler)(sig);
    }
    break;
  }
}


/*  ProtSetup -- global protection setup
 *
 *  Under FreeBSD, the global setup involves installing a signal
 *  handler on SIGBUS to catch and handle page faults (see
 *  sigHandle).  The previous handler is recorded so that it can be
 *  reached from sigHandle if it fails to handle the fault.
 *
 *  NOTE: There are problems with this approach:
 *    1. we can't honor the sa_flags for the previous handler,
 *    2. what if this thread is suspended just after calling signal(3)?
 *       The sigNext variable will never be initialized!  */

void ProtSetup(void)
{
  struct sigaction sa;
  int result;

  sa.sa_sigaction = sigHandle;
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

  result = sigaction(SIGBUS, &sa, &sigNext);
  AVER(result == 0);
}

/*  impl.c.protsu
 *
 *                  PROTECTION FOR SUNOS
 *
 *  $HopeName: MMsrc!protsu.c(trunk.2) $
 *
 *  Copyright (C) 1995 Harlequin Group, all rights reserved
 *
 *  .hack.sigdfl: GCC 2.5.8 produces a warning when we use SIG_DFL with
 *  -Wstrict-prototypes, which we want.  SIG_DFL is just zero, so we
 *  have our own definition.  We don't expect SIG_DFL to change, because
 *  that would break SunOS binaries.  *sigh*
 */

#include "std.h"
#include "prot.h"
#include "fault.h"
#include "lock.h"
#include "lockst.h"
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <signal.h>

#ifndef OS_SUNOS
#error "protsu.c is SunOS 4 specific, but OS_SUNOS is not set"
#endif

SRCID("$HopeName");


/* .hack.sigdfl */
#ifndef SIG_DFL
#error "protsu.c expected SIG_DFL to be declared by signal.h"
#else
#undef SIG_DFL
#define SIG_DFL		((handler_t)0)
#endif


/* Fix up unprototyped system calls.  */
/* Note that these are not fixed up by std.h because that only fixes */
/* up discrepancies with ANSI. */

extern int mprotect(caddr_t addr, int len, int prot);
extern int getpagesize(void);
extern int getpid(void);
typedef void (*handler_t)(int sig, int code,
                          struct sigcontext *scp, char *addr);


/*  == Protection Granularity ==
 *
 *  The granularity of protection is one page under SunOS.
 */

Addr ProtGrain(void)
{
  Addr grain;

  grain = (Addr)getpagesize();
  AVER(IsPoT(grain));

  return(grain);
}


/* Pointer to the previously-installed signal handler, as returned by */
/* signal(3).  See ProtSetup. */

static handler_t sigNext = NULL;


/*  == Protection Signal Handler ==
 *
 *  This is the signal handler installed by ProtSetup to deal with
 *  protection faults.  It is installed on the SIGSEGV signal.
 *  It decodes the protection fault details from the signal context
 *  and passes them to FaultDispatch, which attempts to handle the
 *  fault and remove its cause.  If the fault is handled, then
 *  the handler returns and execution resumes.  If it isn't handled,
 *  then sigHandle does its best to pass the signal on to the
 *  previously installed signal handler (sigNext).
 */

static void sigHandle(int sig, int code,
                      struct sigcontext *scp, char *addr)
{
  AVER(sig == SIGSEGV);
  AVER(scp != NULL);

  if(code == SEGV_PROT) {
    ProtMode mode;
    Addr base, limit;

    AVER(addr != SIG_NOADDR);  /* Assume protection address was decoded. */

    /* We can't determine the access mode (read, write, etc.) */
    /* under SunOS without decoding the faulting instruction. */
    /* Don't bother, yet.  We can do this if necessary. */

    mode = ProtREAD | ProtWRITE;

    /* We assume that the access is for one word at the address. */
    /* @@@@ What happens to ldd across a page boundary? */
    /* ldd has to be dword aligned */

    base = (Addr)addr;
    limit = (Addr)addr + sizeof(Addr);

    /* Offer each protection structure the opportunity to handle the */
    /* exception.  If it succeeds, then allow the mutator to continue. */

    if(FaultDispatch(base, limit, mode))
      return;
  }

  /* The exception was not handled by any known protection structure, */
  /* so throw it to the previously installed handler. */

  (*sigNext)(sig, code, scp, addr);
}


/*  == Default Signal Handler ==
 *
 *  This is a signal handler used as sigNext if the previous handler
 *  returned by signal(3) was SIG_DFL.  It does its best to get to
 *  the default handler, which will probably dump core.
 */

static void sigDefault(int sig, int code, struct sigcontext *scp, char *addr)
{
  UNUSED(sig);
  UNUSED(code);
  UNUSED(scp);
  UNUSED(addr);

  (void)signal(SIGSEGV, SIG_DFL);
  kill(getpid(), SIGSEGV);
  NOTREACHED;
  abort();
}


/*  == Global Protection Setup ==
 *
 *  Under SunOS, the global setup involves installing a signal handler
 *  on SIGSEGV to catch and handle protection faults (see sigHandle).
 *  The previous handler is recorded so that it can be reached from
 *  sigHandle if it fails to handle the fault.
 *
 *  NOTE: There are problems with this approach:
 *    1. we can't honor the wishes of the sigvec(2) entry for the
 *       previous handler,
 *    2. what if this thread is suspended just after calling signal(3)?
 *       The sigNext variable will never be initialized!
 */

void ProtSetup(void)
{
  handler_t next;

  next = signal(SIGSEGV, sigHandle);

  if(next == SIG_DFL)           /* suicide function */
    sigNext = sigDefault;
  else if(sigNext != sigHandle) /* already installed? */
    sigNext = next;
}


/*  == Set Protection ==
 *
 *  This is just a thin veneer on top of mprotect(2).
 */

void ProtSet(Addr base, Addr limit, ProtMode mode)
{
#ifdef DEBUG
  Addr grain = ProtGrain();
#endif
  int flags;

  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base != 0);
  AVER((limit - base) <= INT_MAX);	/* should be redundant */
  AVER(IsAligned(grain, base));
  AVER(IsAligned(grain, limit));

  flags = PROT_READ | PROT_WRITE | PROT_EXEC;
  if((mode & ProtREAD) != 0)
    flags &= ~PROT_READ;
  if((mode & ProtWRITE) != 0)
    flags &= ~PROT_WRITE;

  if(mprotect((caddr_t)base, (int)(limit - base), flags) != 0)
    NOTREACHED;
}


/*  == Protection Trampoline ==
 *
 *  The protection trampoline is trivial under SunOS, as there is nothing
 *  that needs to be done in the dynamic context of the mutator in order
 *  to catch faults.  (Contrast this with Win32 Structured Exception
 *  Handling.)
 */

void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
               void *p, size_t s)
{
  AVER(f != NULL);
  AVER(resultReturn != NULL);

  *resultReturn = (*f)(p, s);
}

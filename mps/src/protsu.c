/* impl.c.protsu: PROTECTION FOR SUNOS
 *
 * $HopeName: MMsrc!protsu.c(trunk.6) $
 *
 * Copyright (C) 1995,1997 Harlequin Group, all rights reserved
 *
 * .hack.sigdfl: GCC 2.5.8 produces a warning when we use SIG_DFL with
 * -Wstrict-prototypes, which we want.  SIG_DFL is just zero, so we
 * have our own definition.  We don't expect SIG_DFL to change, because
 * that would break SunOS binaries.  *sigh*
 */

#include "mpm.h"
#include <stdlib.h>
#include <sys/mman.h>
#include <signal.h>

#ifndef MPS_OS_SU
#error "protsu.c is SunOS 4 specific, but MPS_OS_SU is not set"
#endif

SRCID(protsu, "$HopeName: MMsrc!protsu.c(trunk.6) $");


/* .hack.sigdfl */
#ifndef SIG_DFL
#error "protsu.c expected SIG_DFL to be declared by signal.h"
#else
#undef SIG_DFL
#define SIG_DFL         ((handler_t)0)
#endif


/* Fix up unprototyped system calls.  */
/* Note that these are not fixed up by std.h because that only fixes */
/* up discrepancies with ANSI. */

extern int getpagesize(void);
extern int getpid(void);
extern int mprotect(caddr_t addr, int len, int prot);
extern int sigblock(int);
extern int sigsetmask(int);
typedef void (*handler_t)(int sig, int code,
                          struct sigcontext *scp, char *addr);


/* Pointer to the previously-installed signal handler, as returned by */
/* signal(3).  See ProtSetup. */

static handler_t sigNext = NULL;


/* sigHandle -- protection signal handler
 *
 * This is the signal handler installed by ProtSetup to deal with
 * protection faults.  It is installed on the SIGSEGV signal.
 * It decodes the protection fault details from the signal context
 * and passes them to FaultDispatch, which attempts to handle the
 * fault and remove its cause.  If the fault is handled, then
 * the handler returns and execution resumes.  If it isn't handled,
 * then sigHandle does its best to pass the signal on to the
 * previously installed signal handler (sigNext).
 *
 * .sigh.addr: This code assumes that the system will decode the
 * address of the protection violation.  SunOS doesn't document
 * when this will or will not happen.
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
    AVER(addr != SIG_NOADDR);           /* .sigh.addr */
    mode = AccessREAD | AccessWRITE;    /* .sigh.decode */
    if(SpaceAccess((Addr)addr, mode))   /* .sigh.size */
      return;
  }

  /* The exception was not handled by any known protection structure, */
  /* so throw it to the previously installed handler. */
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
 * Under SunOS, the global setup involves installing a signal handler
 * on SIGSEGV to catch and handle protection faults (see sigHandle).
 * The previous handler is recorded so that it can be reached from
 * sigHandle if it fails to handle the fault.
 *
 * NOTE: There are problems with this approach:
 *   1. we can't honor the wishes of the sigvec(2) entry for the
 *      previous handler,
 *   2. what if this thread is suspended just after calling signal(3)?
 *      The sigNext variable will never be initialized!
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


/* ProtSet -- set the protection for a page
 *
 * This is just a thin veneer on top of mprotect(2).
 */

void ProtSet(Addr base, Addr limit, AccessSet mode)
{
  int flags;

  AVER(sizeof(int) == sizeof(Addr));
  AVER(base < limit);
  AVER(base != 0);
  AVER(AddrOffset(base, limit) <= INT_MAX); /* should be redundant */

  flags = PROT_READ | PROT_WRITE | PROT_EXEC;
  if((mode & AccessWRITE) != 0)
    flags = PROT_READ | PROT_EXEC;
  if((mode & AccessREAD) != 0)
    flags = PROT_NONE;

  if(mprotect((caddr_t)base, (int)AddrOffset(base, limit), flags) != 0)
    NOTREACHED;
}


/* ProtSync -- synchronize protection settings with hardware
 *
 * This does nothing under SunOS.
 */

void ProtSync(Space space)
{
  NOOP;
}


/* ProtTramp -- protection trampoline
 *
 * The protection trampoline is trivial under SunOS, as there is nothing
 * that needs to be done in the dynamic context of the mutator in order
 * to catch faults.  (Contrast this with Win32 Structured Exception
 * Handling.)
 */

void ProtTramp(void **resultReturn, void *(*f)(void *, size_t),
               void *p, size_t s)
{
  AVER(f != NULL);
  AVER(resultReturn != NULL);

  *resultReturn = (*f)(p, s);
}

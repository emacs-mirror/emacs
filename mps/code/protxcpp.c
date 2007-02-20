/* protxcpp.c: PROTECTION FOR MAC OS X ON POWERPC
 *
 *  $Id$
 *  Copyright (c) 2001,2005 Ravenbrook Limited.  See end of file for license.
 *
 *  Most of this was copied from protso.c and modified.
 *
 *  REFERENCES
 *
 * [PEM] "PowerPC Microprocessor Family: The Programming Environments for
 * 32-bit Microprocessors", Motorola, 1997-01.  MPCFPE32B/AD REV.1
 *
 * [SIGNALH] /usr/include/sys/signal.h on drj's iBook (Mac OS X 10.3.8)
 *
 * [PPCUCH] /usr/include/ppc/ucontext.h
 *
 */

#include "mpm.h"

#ifndef MPS_OS_XC
#error "protxcpp.c is Mac OS X specific, but MPS_OS_XC is not set"
#endif
#ifndef MPS_ARCH_PP
#error "protxcpp.c is PowerPC specific, but MPS_ARCH_PP is not set"
#endif
#ifndef PROTECTION
#error "protxcpp.c implements protection, but PROTECTION is not set"
#endif

#include <signal.h>
#include <sys/ucontext.h>

SRCID(protxcpp, "$Id$");

/* The previously-installed signal action, as returned by */
/* sigaction(3).  See ProtSetup. */

static struct sigaction sigNext;

/* sigHandle -- protection signal handler
 *
 *  This is the signal handler installed by ProtSetup to deal with
 *  protection faults.  It is installed on the SIGBUS signal.
 *  It decodes the protection fault details from the signal context
 *  and passes them to ArenaAccess, which attempts to handle the
 *  fault and remove its cause.  If the fault is handled, then
 *  the handler returns and execution resumes.  If it isn't handled,
 *  then sigHandle does its best to pass the signal on to the
 *  previously installed signal handler (sigNext).
 *
 *  .sigh.addr: si_addr, on Darwin 7.8.0, is the address of the faulting
 *  instruction (by observation). [SIGNALH] says, in the declaration of
 *  siginfo_t, that this field should be the faulting instruction, but
 *  below that says that for SIGBUS it should be the faulting address).
 *  We grub around in the ucontext to find the PowerPC DAR register (See
 *  [PEM] 6-25, Table 6-9) which contains the faulting address.
 *
 *  .sigh.limit: We throw away the limit information.
 */

static void sigHandle(int sig, siginfo_t *info, void *contextArg)
{
  ucontext_t *ucontext;

  AVER(sig == SIGBUS);
  AVER(info != NULL);

  ucontext = contextArg;

  /* On OS X the si_code field does't appear to be useful.  Protection
   * faults appear as SIGBUS signals, and the only documented code for
   * SIGBUS is BUS_ADRALN (invalid address alignment) which is what
   * si_code is set to (it has value 1), even though the address is
   * in fact aligned.
   * On other platforms a test like info->si_code == SEGV_ACCERR appears
   * here. */
  if(1) {
    AccessSet mode;
    Addr base, limit;

    /* We dont't bother to determine the access mode (read, write, etc.)
     * under OS X.  It's possible that this information is available in
     * the context structures.  Needs more investigation.
     */

    mode = AccessREAD | AccessWRITE;

    /* We assume that the access is for one word at the address. */

    /* See [PPCUCH] */
    base = (Addr)ucontext->uc_mcontext->es.dar;
    limit = AddrAdd(base, (Size)sizeof(Addr));

    /* Offer each protection structure the opportunity to handle the */
    /* exception.  If it succeeds, then allow the mutator to continue. */

    /* MutatorFaultContext parameter is a dummy parameter for this */
    /* implementation */
    if(ArenaAccess(base, mode, NULL)) {
      return;
    }
  }

  /* The exception was not handled by any known protection structure, */
  /* so throw it to the previously installed handler. */

  /* @@ This is really weak.
   * Need to implement rest of the contract of sigaction */
  (*sigNext.sa_sigaction)(sig, info, contextArg);
}


/*  ProtSetup -- global protection setup
 *
 *  Under OS X, the global setup involves installing a signal handler
 *  on SIGBUS to catch and handle protection faults (see sigHandle).
 *  The previous handler is recorded so that it can be reached from
 *  sigHandle if it fails to handle the fault.
 *
 *  NOTE: There are problems with this approach:
 *    1. we can't honor the wishes of the sigvec(2) entry for the
 *       previous handler,
 */

/* This function itself probably isn't architecture specific, but it
 * references the sigHandle which is currently static and which is
 * architecture specific. */
void ProtSetup(void)
{
  struct sigaction sa;
  int result;

  sa.sa_sigaction = sigHandle;
  /* No idea if sigemptyset is necessary, copied from protfri3.c,
   * 2005-03-02 DRJ */
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO;

  result = sigaction(SIGBUS, &sa, &sigNext);
  AVER(result == 0);
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2002,2005 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

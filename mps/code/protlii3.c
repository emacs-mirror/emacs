/*  impl.c.protlii3: PROTECTION FOR LINUX (INTEL 386)
 *
 *  $Id$
 *  Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * SOURCES
 *
 * .source.i486: Intel486 Microprocessor Family Programmer's
 * Reference Manual
 *
 * .source.linux.kernel: Linux kernel source files.
 */

#include "prmcli.h"

#ifndef MPS_OS_LI
#error "protlii3.c is Linux-specific, but MPS_OS_LI is not set"
#endif
#if !defined(MPS_ARCH_I3) && !defined(MPS_ARCH_I4)
#error "protlii3.c is Intel-specific, but MPS_ARCH_I3 or MPS_ARCH_I4 is not set"
#endif
#ifndef PROTECTION
#error "protlii3.c implements protection, but PROTECTION is not set"
#endif

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <signal.h>

SRCID(protlii3, "$Id$");


/* Useful stuff that doesn't appear to be in any header files. */

/* Interrupt number 14 is Page Fault. */
#define TRAPNO_PAGE_FAULT 14

/* Bits in err field of sigcontext for interrupt 14 (page fault) */
#define PAGE_FAULT_ERR_PAGEPROT   0x1
#define PAGE_FAULT_ERR_WRITE      0x2
#define PAGE_FAULT_ERR_USERMODE   0x4


/* The previously-installed signal action, as returned by */
/* sigaction(3).  See ProtSetup. */

static struct sigaction sigNext;


typedef void (*__real_lii3_sighandler_t)(int, struct sigcontext);


/* sigHandle -- protection signal handler
 *
 *  This is the signal handler installed by ProtSetup to deal with
 *  protection faults.  It is installed on the SIGSEGV signal.
 *  It decodes the protection fault details from the signal context
 *  and passes them to ArenaAccess, which attempts to handle the
 *  fault and remove its cause.  If the fault is handled, then
 *  the handler returns and execution resumes.  If it isn't handled,
 *  then sigHandle does its best to pass the signal on to the
 *  previously installed signal handler (sigNext).
 *
 *  .sigh.args: There is no officially documented way of getting the
 *  sigcontext, but on x86 Linux at least it is passed BY VALUE as a
 *  second argument to the signal handler.  The prototype doesn't
 *  include this arg.
 *  See .source.linux.kernel (linux/arch/i386/kernel/signal.c).
 *
 *  .sigh.context: We only know how to handle interrupt 14, where
 *  context.err gives the page fault error code and context.cr2 gives
 *  the fault address.  See .source.i486 (9.9.14) and
 *  .source.linux.kernel (linux/arch/i386/mm/fault.c).
 *
 *  .sigh.addr: We assume that the OS decodes the address to something
 *  sensible
 */

static void sigHandle(int sig, struct sigcontext context)  /* .sigh.args */
{
  AVER(sig == SIGSEGV);

  if(context.trapno == TRAPNO_PAGE_FAULT) {  /* .sigh.context */
    AccessSet mode;
    Addr base, limit;
    MutatorFaultContextStruct mfContext;

    mfContext.scp = &context;

    mode = ((context.err & PAGE_FAULT_ERR_WRITE) != 0)  /* .sigh.context */
             ? (AccessREAD | AccessWRITE)
             : AccessREAD;

    /* We assume that the access is for one word at the address. */
    base = (Addr)context.cr2;   /* .sigh.addr */
    limit = AddrAdd(base, (Size)sizeof(Addr));

    /* Offer each protection structure the opportunity to handle the */
    /* exception.  If it succeeds, then allow the mutator to continue. */

    if(ArenaAccess(base, mode, &mfContext))
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
    (*(__real_lii3_sighandler_t)sigNext.sa_handler)(sig, context);
    break;
  }
}


/*  ProtSetup -- global protection setup
 *
 *  Under Linux, the global setup involves installing a signal handler
 *  on SIGSEGV to catch and handle page faults (see sigHandle).
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

  sa.sa_handler = (__sighandler_t)sigHandle;  /* .sigh.args */
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = 0;

  result = sigaction(SIGSEGV, &sa, &sigNext);
  AVER(result == 0);
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

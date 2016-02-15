/* thw3i3.c: WIN32 THREAD MANAGER x86
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.  See end of file for license.
 *
 * Implements thread stack scanning.  See <design/thread-manager/>.
 *
 * This supports the <code/th.h> together with <code/thw3.c>
 *
 * .thread.id: The thread id is used to identify the current thread.
 *
 *
 * ASSUMPTIONS
 *
 * .error: some major errors are assumed not to happen.
 *
 * Other errors are assumed to only happen in certain circumstances.
 * .error.get-context: GetThreadContext is assumed to succeed unless the
 * thread has been destroyed.
 *
 * .stack.full-descend:  assumes full descending stack.
 * i.e. stack pointer points to the last allocated location;
 * stack grows downwards.
 *
 * .stack.below-bottom: it's legal for the stack pointer to be at a
 * higher address than the registered bottom of stack. This might
 * happen if the stack of another thread doesn't contain any frames
 * belonging to the client language. In this case, the stack should
 * not be scanned.
 *
 * .stack.align: assume roots on the stack are always word-aligned,
 * but don't assume that the stack pointer is necessarily
 * word-aligned at the time of reading the context of another thread.
 *
 * .i6: assumes MPS_ARCH_I6
 * .i6.sp: The sp in the context is Rsp
 * .i6.context: Rsp is in control context so .context.sp holds
 * The root registers are Rdi, Rsi, Rbx, Rbp, Rdx, Rcx, Rax, R8 - R15
 * these are given by CONTEXT_INTEGER, so .context.regroots holds.
 *
 * .nt: uses Win32 specific stuff
 * HANDLE
 * DWORD
 * GetCurrentThreadId
 * CONTEXT
 * CONTEXT_CONTROL | CONTEXT_INTEGER
 * GetThreadContext
 *
 * .context: ContextFlags determine what is recorded by
 * GetThreadContext.  This should be set to whichever bits of the
 * context that need to be recorded.  This should include:
 * .context.sp: sp assumed to be recorded by CONTEXT_CONTROL.
 * .context.regroots: assumed to be recorded by CONTEXT_INTEGER.
 * see winnt.h for description of CONTEXT and ContextFlags.
 */

#include "mpm.h"

#if !defined(MPS_OS_W3) || !defined(MPS_ARCH_I6) /* .i6 .nt */
#error "Compiling thw3i6 when MPS_OS_W3 or MPS_ARCH_I6 not defined."
#endif

#include "thw3.h"

#include "mpswin.h"

SRCID(thw3i6, "$Id$");


Res ThreadScan(ScanState ss, Thread thread, Word *stackBot,
               mps_area_scan_t scan_area,
	       void *closure, size_t closure_size)
{
  DWORD id;
  Res res;

  id = GetCurrentThreadId();

  if (id != thread->id) { /* .thread.id */
    CONTEXT context;
    BOOL success;
    Word *stackBase, *stackLimit;
    Addr stackPtr;

    /* scan stack and register roots in other threads */

    /* This dumps the relevant registers into the context */
    /* .context.flags */
    context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
    /* .thread.handle.get-context */
    success = GetThreadContext(thread->handle, &context);
    if(!success) {
      /* .error.get-context */
      /* We assume that the thread must have been destroyed. */
      /* We ignore the situation by returning immediately. */
      return ResOK;
    }

    stackPtr  = (Addr)context.Rsp;   /* .i6.sp */
    /* .stack.align */
    stackBase  = (Word *)AddrAlignUp(stackPtr, sizeof(Addr));
    stackLimit = stackBot;
    if (stackBase >= stackLimit)
      return ResOK;    /* .stack.below-bottom */

    /* scan stack inclusive of current sp and exclusive of
     * stackBot (.stack.full-descend)
     */
    res = TraceScanArea(ss, stackBase, stackLimit,
			scan_area, closure, closure_size);
    if(res != ResOK)
      return res;

    /* (.context.regroots)
     * This scans the root registers (.context.regroots).  It also
     * unnecessarily scans the rest of the context.  The optimisation
     * to scan only relevant parts would be machine dependent.
     */
    res = TraceScanArea(ss, (Word *)&context,
			(Word *)((char *)&context + sizeof(CONTEXT)),
			scan_area, closure, closure_size);
    if(res != ResOK)
      return res;

  } else { /* scan this thread's stack */
    res = StackScan(ss, stackBot, scan_area, closure, closure_size);
    if(res != ResOK)
      return res;
  }

  return ResOK;
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

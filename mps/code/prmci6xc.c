/* prmci6xc.c: PROTECTION MUTATOR CONTEXT x64 (MAC OS X)
 *
 * $Id$
 * Copyright (c) 2001-2013 Ravenbrook Limited.  See end of file for license.
 *
 * .purpose: This module implements the part of the protection module
 * that decodes the MutatorFaultContext. 
 *
 *
 * SOURCES
 *
 * .source.linux.kernel: Linux kernel source files.
 *
 *
 * ASSUMPTIONS
 *
 * .sp: The stack pointer in the context is uc_stack.ss_sp.
 *
 * .context.regroots: The root regs are assumed to be recorded in the context
 * at pointer-aligned boundaries.
 *
 * .assume.regref: The resisters in the context can be modified by
 * storing into an MRef pointer.
 */

#include "prmci6xc.h"
#include "prmci6.h"

SRCID(prmci6li, "$Id$");


/* Prmci6AddressHoldingReg -- return an address of a register in a context */

MRef Prmci6AddressHoldingReg(MutatorFaultContext mfc, unsigned int regnum)
{
  AVER(regnum <= 15);
  AVER(regnum >= 0);

  /* .assume.regref */
  /* The register numbers (REG_RAX etc.) are defined in <ucontext.h>
     but only if _XOPEN_SOURCE is defined: see .feature.xc in
     config.h. */
  /* MRef (a Word *) is not compatible with pointers to the register
     types (actually a __uint64_t).  To avoid aliasing optimization
     problems, The registers are cast through (char *) */
  switch (regnum) {
    case  0: return (MRef)((char *)&mfc->thread_state.__rax);
    case  1: return (MRef)((char *)&mfc->thread_state.__rcx);
    case  2: return (MRef)((char *)&mfc->thread_state.__rdx);
    case  3: return (MRef)((char *)&mfc->thread_state.__rbx);
    case  4: return (MRef)((char *)&mfc->thread_state.__rsp);
    case  5: return (MRef)((char *)&mfc->thread_state.__rbp);
    case  6: return (MRef)((char *)&mfc->thread_state.__rsi);
    case  7: return (MRef)((char *)&mfc->thread_state.__rdi);
    case  8: return (MRef)((char *)&mfc->thread_state.__r8);
    case  9: return (MRef)((char *)&mfc->thread_state.__r9);
    case 10: return (MRef)((char *)&mfc->thread_state.__r10);
    case 11: return (MRef)((char *)&mfc->thread_state.__r11);
    case 12: return (MRef)((char *)&mfc->thread_state.__r12);
    case 13: return (MRef)((char *)&mfc->thread_state.__r13);
    case 14: return (MRef)((char *)&mfc->thread_state.__r14);
    case 15: return (MRef)((char *)&mfc->thread_state.__r15);
  }
  NOTREACHED;
  return (MRef)NULL;  /* Avoids compiler warning. */
}


/* Prmci3DecodeFaultContext -- decode fault to find faulting address and IP */

void Prmci6DecodeFaultContext(MRef *faultmemReturn,
                              Byte **insvecReturn,
                              MutatorFaultContext mfc)
{
  *faultmemReturn = (MRef)mfc->exception_state.__faultvaddr;
  *insvecReturn = (Byte*)mfc->thread_state.__rip;
}


/* Prmci3StepOverIns -- modify context to step over instruction */

void Prmci6StepOverIns(MutatorFaultContext mfc, Size inslen)
{
  mfc->thread_state.__rip += (Word)inslen;
}


Addr MutatorFaultContextSP(MutatorFaultContext mfc)
{
  return (Addr)mfc->thread_state.__rsp;
}


Res MutatorFaultContextScan(ScanState ss, MutatorFaultContext mfc)
{
  x86_thread_state64_t *mc;
  Res res;

  /* This scans the root registers (.context.regroots).  It also
     unnecessarily scans the rest of the context.  The optimisation
     to scan only relevant parts would be machine dependent. */
  mc = &mfc->thread_state;
  res = TraceScanAreaTagged(ss,
                            (Addr *)mc,
                            (Addr *)((char *)mc + sizeof(*mc)));
  return res;
}


/* ThreadScan -- scan the state of a thread (stack and regs) */

Res ThreadScan(ScanState ss, Thread thread, void *stackBot)
{
  mach_port_t self;
  Res res;

  AVERT(Thread, thread);
  self = mach_thread_self();
  if (thread->port == self) {
    /* scan this thread's stack */
    res = StackScan(ss, stackBot);
    if(res != ResOK)
      return res;
  } else {
    struct MutatorFaultContextStruct mfcStruct;
    Addr *stackBase, *stackLimit, stackPtr;
    mach_msg_type_number_t count;
    kern_return_t kern_return;

#if 0
    count = x86_EXCEPTION_STATE64_COUNT;
    kern_return = thread_get_state(thread->port,
                                   x86_EXCEPTION_STATE64,
                                   (thread_state_t)&mfcStruct.exception_state,
                                   &count);
    /* FIXME: error checking */
#endif

    count = x86_THREAD_STATE64_COUNT;
    kern_return = thread_get_state(thread->port,
                                   x86_THREAD_STATE64,
                                   (thread_state_t)&mfcStruct.thread_state,
                                   &count);
    /* FIXME: error checking */
    AVER(kern_return == KERN_SUCCESS);
    
    stackPtr = MutatorFaultContextSP(&mfcStruct);
    /* .stack.align */
    stackBase  = (Addr *)AddrAlignUp(stackPtr, sizeof(Addr));
    stackLimit = (Addr *)stackBot;
    if (stackBase >= stackLimit)
      return ResOK;    /* .stack.below-bottom */

    /* scan stack inclusive of current sp and exclusive of
     * stackBot (.stack.full-descend)
     */
    res = TraceScanAreaTagged(ss, stackBase, stackLimit);
    if(res != ResOK)
      return res;

    /* scan the registers in the mutator fault context */
    res = MutatorFaultContextScan(ss, &mfcStruct);
    if(res != ResOK)
      return res;
  }

  return ResOK;
}

extern kern_return_t catch_exception_raise(mach_port_t exception_port,
                                           mach_port_t thread,
                                           mach_port_t task,
                                           exception_type_t exception,
                                           exception_data_t code,
                                           mach_msg_type_number_t code_count);

kern_return_t catch_exception_raise(mach_port_t exception_port,
                                    mach_port_t thread,
                                    mach_port_t task,
                                    exception_type_t exception,
                                    exception_data_t code,
                                    mach_msg_type_number_t code_count)
{
  MutatorFaultContextStruct mfcStruct;
  mach_msg_type_number_t count;
  kern_return_t kern_return;
  AccessSet mode;
  Addr base;
  
  UNUSED(exception_port);
  UNUSED(task);
  UNUSED(code_count);

  if (exception != EXC_BAD_ACCESS || code[0] != KERN_PROTECTION_FAILURE) {
    /* FIXME: forward this somehow */
    NOTREACHED;
  }
  
  count = x86_EXCEPTION_STATE64_COUNT;
  kern_return = thread_get_state(thread,
                                 x86_EXCEPTION_STATE64,
                                 (thread_state_t)&mfcStruct.exception_state,
                                 &count);
  /* FIXME: error checking */
  AVER(kern_return == KERN_SUCCESS);

  count = x86_THREAD_STATE64_COUNT;
  kern_return = thread_get_state(thread,
                                 x86_THREAD_STATE64,
                                 (thread_state_t)&mfcStruct.thread_state,
                                 &count);
  /* FIXME: error checking */
  AVER(kern_return == KERN_SUCCESS);

  mode = AccessREAD | AccessWRITE; /* FIXME: Can refine? */
  base = (Addr)mfcStruct.exception_state.__faultvaddr;
  
  if (ArenaAccess(base, mode, &mfcStruct))
    return KERN_SUCCESS;

  /* FIXME: Forward the exception somehow. */
  NOTREACHED;
  return KERN_SUCCESS;
}


/*  ProtSetup -- global protection setup */

void ProtSetup(void)
{
  NOOP;
}


/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2001-2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
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

/* impl.c.ssfri3: FREEBSD/INTEL STACK SCANNING
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 *  This scans the stack and fixes the registers which may contain
 *  roots.  See design.mps.thread-manager
 *
 *  The registers edi, esi, ebx are the registers defined to be preserved
 *  across function calls and therefore may contain roots.
 *  These are pushed on the stack for scanning.
 *
 * SOURCES
 *
 * .source.callees.saves: Set of callee-saved registers taken from
 * CALL_USED_REGISTERS in <gcc-sources>/config/i386/i386.h.
 *
 * ASSUMPTIONS
 *
 * .assume.align: The stack pointer is assumed to be aligned on a word
 * boundary.
 *
 * .assume.asm.stack: The compiler must not do wacky things with the
 * stack pointer around a call since we need to ensure that the
 * callee-save regs are visible during TraceScanArea.
 *
 * .assume.asm.order: The volatile modifier should prevent movement
 * of code, which might break .assume.asm.stack.
 *
 */


#include "mpm.h"

SRCID(ssfri3, "$Id$");

/* .assume.asm.order */
#define ASMV(x) __asm__ volatile (x)


Res StackScan(ScanState ss, Addr *stackBot)
{
  Addr *stackTop;
  Res res;

  /* .assume.asm.stack */
  ASMV("push %ebx");    /* These registers are callee-saved */
  ASMV("push %esi");    /* and so may contain roots.  They are pushed */
  ASMV("push %edi");    /* for scanning.  See .source.callees.saves. */
  ASMV("mov %%esp, %0" : "=r" (stackTop) :);    /* stackTop = esp */

  AVER(AddrIsAligned((Addr)stackTop, sizeof(Addr)));  /* .assume.align */
  res = TraceScanArea(ss, stackTop, stackBot);

  ASMV("add $12, %esp");  /* pop 3 regs to restore the stack pointer */

  return res;
}

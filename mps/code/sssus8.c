/* impl.c.sssus8: SPARC STACK SCANNING
 *
 * $HopeName: MMsrc!sssus8.c(trunk.1) $
 * $Id: sssus8.c,v 1.1 2002/02/15 19:12:02 pekka Exp $
 * Copyright (c) 2001 Ravenbrook Limited.
 * Copyright (c) 2002 Global Graphics Software.
 *
 * This scans the stack and fixes the registers which may contain
 * roots.  See design.mps.thread-manager.
 *
 * .roots: The non-global registers are preserved into the stackframe
 * by the "ta 3" instruction.  This leaves the global registers.
 * According to the Sparc Architecture Manual:
 * %g1 is assumed to be volatile across procedure calls
 * %g2...%g4 are "reserved for use by application programmer"
 * %g5...%g7 are "nonvolatile and reserved for (as-yet-undefined)
 *    use by the execution environment"
 * To be safe %g2 to %g7 are pushed onto the stack before scanning
 * it just in case.
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
 */

#include "mpm.h"
#include <alloca.h>

SRCID(sssus8, "$Id: sssus8.c,v 1.1 2002/02/15 19:12:02 pekka Exp $");


/* .assume.asm.order */
#define ASMV(x) __asm__ volatile (x)


Res StackScan(ScanState ss, Addr *stackBot)
{
  Addr *stackTop;
  Res res;
  void *globals;

  /* We expect C will save the caller's window, but we don't really care, */
  /* because it's bound to be an MPS window. */
  globals = alloca(24); /* for 6 globals */
  ASMV("std %%g2, [%0]" : : "r" (globals)); /* double stores */
  ASMV("std %%g4, [%0 + 8]" : : "r" (globals));
  ASMV("std %%g6, [%0 + 16]" : : "r" (globals));
  ASMV("ta 3"); /* flushes register windows onto stack */
  ASMV("mov %%sp, %0" : "=r" (stackTop));    /* stackTop = sp */

  AVER(AddrIsAligned((Addr)stackTop, sizeof(Addr)));  /* .assume.align */
  res = TraceScanArea(ss, stackTop, stackBot);

  return res;
}
